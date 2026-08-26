# Package-specific environment for storing the database location, driver, and connection
.ddb_env <- new.env(parent = emptyenv())

# Basic usage idea: lrh_con() pulls the correct connection. con_write()
# pulls a writeable connection, which is closed/replaced with read-only at the next
# lrh_con()


#' Check if a ddb file is encrypted
#'
#' @param db_file File to check
#'
#' @returns TRUE or FALSE
#' @export
ddb_is_encrypted <- function(db_file) {
  tryCatch({
    if (db_file == ":memory:") {
      FALSE
    } else {
      d <- duckdb::duckdb(dbdir = db_file, read_only = TRUE)
      duckdb::duckdb_shutdown(d)
    }
    FALSE
  }, error = \(e) {
      if (grepl("without a key", e$message)) {
        TRUE  # Encrypted
      } else {
        stop(e)  # Other error
      }
    }
  )
}

#' Database Connection Helper
#'
#' Returns a connection to the current duckdb database. Stores the driver and
#' connection in a package-level environment for use between calls. If calling for
#' a different type of connection, closes the original connection before opening
#' the new one and returning it.
#'
#' @param db_file Location of the DuckDB. Defaults to [lrh_db()]
#' @param timezone_out Timezone to use for pulling date-time columns
#' @param tz_out_convert "with" or "force"
#' @param ... Optional additional arguments
#' @param type Either "read_only",  "read_write" or "any". Defaults to "any", which
#' will use the current connection if it exists, or create a read-only connection.
#'
#' @returns a database connection
#' @md
#' @export
lrh_con <- function(db_file = lrh_db(), timezone_out = "America/New_York",
                    tz_out_convert = "with", type = "any", ...) {

  # Process type
  if (type == "read_only" || type == "any") {
    ro <- TRUE
  } else if (type == "read_write") {
    ro <- FALSE
  } else {
    cli::cli_abort("{.arg type} must be one of {.val read_only}, {.val read_write}, or {.val any}")
  }
  con_type <- if (ro) "read_only" else "read_write"

  # If existing matching connection, return it
  if (!is.null(.ddb_env$con) && (.ddb_env$con_type == con_type || type == "any")) {
    return (.ddb_env$con)
  }

  # Track previous connection type for restore-on-failure when switching
  prev_type <- .ddb_env$con_type

  # If existing non-matching connection, close it
  if (!is.null(.ddb_env$con)) {
    lrh_disconnect()
    cli::cli_alert_info("Switching connection to {.val {con_type}}")
  }

  # Create temp directory if it doesn't exist
  temp_dir <- "C:/temp/duckdb_temp"
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)

  # Set up driver and connection
  .ddb_env$drv <- duckdb::duckdb(
    config = list(memory_limit = "10GB", threads = "4", temp_directory = temp_dir)
  )
  .ddb_env$con <- DBI::dbConnect(.ddb_env$drv,
    timezone_out = timezone_out, tz_out_convert = tz_out_convert, ...
  )
  .ddb_env$con_type <- con_type

  # If anything below fails, clean up the in-memory connection so the next
  # call to lrh_con() doesn't return a connection with no database attached
  tryCatch({

    # General connection setup
    # Set autoload (for using things like 'today' in patient days view)
    DBI::dbExecute(.ddb_env$con, "SET autoinstall_known_extensions = 1;")
    DBI::dbExecute(.ddb_env$con, "SET autoload_known_extensions = 1;")

    # Set timezone (for importing) to UTC
    DBI::dbExecute(.ddb_env$con, "SET TimeZone = 'UTC';")

    # Attach database (required to be this way for encrypted databases)
    attach_extras <- NULL
    # Encryption details
    if (ddb_is_encrypted(db_file)) {
      key <- tntpr::tntp_cred("DUCKDB_KEY") # Has prompting built in
      DBI::dbExecute(.ddb_env$con, "INSTALL httpfs; LOAD httpfs;") # To speed up encryption
      attach_extras <- paste0("ENCRYPTION_KEY '", key, "'")
    }
    # Set read_only
    if (ro) attach_extras <- c(attach_extras, "READ_ONLY")
    attach_str <- paste0("ATTACH '", db_file, "' AS db")
    if (length(attach_extras) > 0) {
      attach_str <- paste0(attach_str, " (", paste0(attach_extras, collapse = ", "), ")")
    }
    DBI::dbExecute(.ddb_env$con, attach_str)
    DBI::dbExecute(.ddb_env$con, "USE db")

  }, error = \(e) {
    lrh_disconnect()
    # If we were switching from an existing connection, try to restore it
    if (!is.null(prev_type) && prev_type != con_type) {
      tryCatch(
        lrh_con(type = prev_type),
        error = \(re) cli::cli_warn("Could not restore previous {.val {prev_type}} connection")
      )
    }
    cli::cli_abort(c(
      "x" = "Unable to connect to DuckDB",
      "i" = "Error Message: {conditionMessage(e)}"
    ), call = rlang::caller_env())
  })

  # return the connection
  .ddb_env$con
}

#' Disconnect from DuckDB
#'
#' @returns nothing
#' @export
lrh_disconnect <- function() {
  if (!is.null(.ddb_env$con)) {
    duckdb::duckdb_shutdown(.ddb_env$drv)
    DBI::dbDisconnect(.ddb_env$con)
    .ddb_env$con <- NULL
    .ddb_env$drv <- NULL
    .ddb_env$con_type <- NULL
  }
}

#' Append only unique rows to a duckdb table
#'
#' @param x Object (data frame) to be appended
#' @param table Table to append to
#' @param remove_duplicates Should duplicate rows be removed? Defaults to TRUE.
#'
#' @md
#' @returns nothing
#' @export
append_duckdb <- function(x, table, remove_duplicates = TRUE) {
  # Restore previous connection type on exit if it was read_only
  prev_type <- .ddb_env$con_type
  on.exit(if (identical(prev_type, "read_only")) lrh_con(type = "read_only"), add = TRUE)

  # Connect to the DuckDB database in read_write
  con <- lrh_con(type = "read_write")

  # Check if the table already exists in the DuckDB database
  existing_tables <- DBI::dbListTables(con)
  if (!(table %in% existing_tables)) {
    # If the table doesn't exist, just write the new data and return
    write_tz_duckdb(table, x)
    return(invisible(NULL))
  }

  # Append the new data to the table
  DBI::dbAppendTable(con, table, x)

  # Remove duplicates if specified
  if (remove_duplicates) {
    dedup_query <- paste0(
      "CREATE TABLE tmp_dedup AS ",
      "SELECT DISTINCT * FROM ", table, "; ",
      "DROP TABLE ", table, "; ",
      "ALTER TABLE tmp_dedup RENAME TO ", table, ";"
    )
    DBI::dbExecute(con, dedup_query)
  }
}

#' Replace rows in a table
#'
#' This convenience function first deletes rows according to the where statement
#' and then appends the rows from x
#'
#' @param x Object (data frame) to be appended
#' @param table Table name
#' @param delete_where String of a SQL Where selection for rows to delete
#'
#' @returns Nothing
#' @export
replace_duckdb <- function(x, table, delete_where) {
  # Restore previous connection type on exit if it was read_only
  prev_type <- .ddb_env$con_type
  on.exit(if (identical(prev_type, "read_only")) lrh_con(type = "read_only"), add = TRUE)

  # Connect to the DuckDB database in read_write
  con <- lrh_con(type = "read_write")

  del_query <- paste("DELETE FROM", table, "WHERE", delete_where)
  n_del <- DBI::dbExecute(con, del_query)
  cli::cli_inform(c("i" = "Removed {.val {n_del}} row{?s}"))
  DBI::dbAppendTable(con, table, x)
  cli::cli_inform(c("i" = "Appended {.val {nrow(x)}} row{?s}"))
}

#' Quickly pull a table from Duck DB
#'
#' Convenience function for pulling a single table. Handles connecting and
#' disconnecting. For more advanced queries, create the connection once and
#' use existing `DBI` functions.
#'
#' @param table Name of table
#' @param max_rows Maximum number of rows to pull (by default, pulls all)
#'
#' @returns a tibble
#' @export
pull_duckdb <- function(table, max_rows = Inf) {

  # Connect to the DuckDB database in read_only
  con <- lrh_con(type = "any")

  q <- paste0("Select * FROM ", table)
  if (max_rows != Inf) q <- paste0(q, " LIMIT ", max_rows)

  DBI::dbGetQuery(con, q) |>
    tibble::as_tibble()
}

#' Write a dataframe to duckdb using TIMESTAMPTZ for date-time fields
#'
#' @param table Name of table to write to
#' @param x dataframe
#' @param overwrite Overwrite a current table (if it exists)?
#'
#' @returns x (invisibly)
#' @export
#' @md
write_tz_duckdb <- function(table, x, overwrite = FALSE) {
  # Restore previous connection type on exit if it was read_only
  prev_type <- .ddb_env$con_type
  on.exit(if (identical(prev_type, "read_only")) lrh_con(type = "read_only"), add = TRUE)

  # Connect to the DuckDB database in read_write
  con <- lrh_con(type = "read_write")

  # Determine which fields should be written as TIMESTAMPTZ
  dt_fields <- names(purrr::keep(x, lubridate::is.POSIXct))
  ft <- rlang::set_names(rep("TIMESTAMPTZ", length(dt_fields)), dt_fields)

  # Write table
  DBI::dbWriteTable(con, table, x, field.types = ft, overwrite = overwrite)

  # Check that times are still correct after pulling
  y <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table, " LIMIT 10"))
  if (nrow(suppressMessages(dplyr::anti_join(y, x))) > 0) {
    cli::cli_abort(c("x" = "Date/Times don't seem to match after getting pulled",
                     "i" = "Check date-time settings in the file"))
  } else {
    cli::cli_alert_success("Wrote table {.val {table}} with TZs")
  }

  invisible(x)
}


#' Upsert in DuckDB
#'
#' Performs an upsert on a duckdb table. If the table doesn't yet exist, this
#' function will prompt the user if they would like to create it.
#'
#' @param table Table name
#' @param x data frame to update
#' @param id_col the id column of the table
#' @param dt_col the date column of the table
#'
#' @returns number of rows affected
#' @export
#' @md
upsert_duckdb <- function(table, x, id_col, dt_col) {

  # Check that x is unique by primary key (crashes R otherwise)
  if (anyDuplicated(x[[id_col]]) > 0) {
    cli::cli_abort(c("x" = "{.var x} contains duplicate values of {.var {id_col}}"))
  }
  if (any(is.na(x[[id_col]]))) {
    cli::cli_abort(c("x" = "{.var x} contains NAs in {.var {id_col}}"))
  }

  # Restore previous connection type on exit if it was read_only
  prev_type <- .ddb_env$con_type
  on.exit(if (identical(prev_type, "read_only")) lrh_con(type = "read_only"), add = TRUE)

  # Connect to the DuckDB database in read_write
  con <- lrh_con(type = "read_write")

  # Check if the table exists
  if (!(table %in% DBI::dbListTables(con))) {
    create <- utils::askYesNo(
      cli::format_inline("Table \"{table}\" does not exist. Create it?")
    )
    if (isTRUE(create)) {
      write_tz_duckdb(table, x)
      cli::cli_alert_success("Created table {.val {table}} with {.val {nrow(x)}} row{?s}")
      return(invisible(nrow(x)))
    } else {
      cli::cli_abort("Table {.val {table}} does not exist. Upsert cancelled.")
    }
  }

  # Check / alter the primary key column
  sch <- DBI::dbGetQuery(con, stringr::str_glue("PRAGMA table_info({table})"))
  pk <- sch[sch$pk, "name"]

  if (length(pk) == 0) { # Add if it doesn't exist
    pk_q <- stringr::str_glue("ALTER TABLE {table} ADD PRIMARY KEY ({id_col});")
    DBI::dbExecute(con, pk_q)
  } else if (pk != id_col) { # Error if it doesn't match
    cli::cli_abort(c("Provided id column {.val {id_col}} does not match table's primary key column {.val {pk}}"))
  }

  # Read x into a temporary table with properties matching the current table
  ct <- stringr::str_glue(
    "CREATE OR REPLACE TEMP TABLE stg as
    SELECT * FROM {table} WHERE 1 = 2"
  )
  DBI::dbExecute(con, ct)
  on.exit(try(DBI::dbRemoveTable(con, "stg"), silent = TRUE), add = TRUE)
  DBI::dbAppendTable(con, "stg", x)

  # Create UPSERT query
  all_fields <- DBI::dbListFields(con, table) |> setdiff(id_col)
  update_fields <- stringr::str_c("  ", all_fields, " = EXCLUDED.", all_fields, collapse = ",\n")

  iq <- stringr::str_glue(
    "INSERT INTO {table}
    SELECT * FROM stg
    ON CONFLICT ({id_col})
    DO UPDATE SET
    {update_fields}
    WHERE EXCLUDED.{dt_col} > {table}.{dt_col}"
  )

  # Perform UPSERT and return rows affected
  DBI::dbExecute(con, iq)
}

#' LRH Database File Location
#'
#' This function will pull the database file location for your computer.
#'
#' @returns file path to the quality.duckdb file
#' @export
#' @md
lrh_db <- function() {
  # If no value is stored in the package environment
  if (is.null(.ddb_env$lrh_db)) {
    tryCatch({
        # Use windows credential value (if it exists)
        db_file <- keyring::key_get("LRH_DB")

        # Test that the file it connects to exists
        stopifnot(file.exists(db_file))

        # Save this value to the package environment and alert the user
        cli::cli_inform(c(
          "i" = "Using database file {.val {db_file}}",
          "i" = "To choose a different file, run {.run LRHTools::choose_lrh_db()}"))
        .ddb_env$lrh_db <- db_file
      },
      error = \(e) { choose_lrh_db() }
    )
  }
  .ddb_env$lrh_db
}

#' Select and set the LRH Database File
#'
#' Saves selection in the package environment AND in the Windows Credential
#' Store for use later
#'
#' @returns Nothing
#' @export
#' @md
choose_lrh_db <- function() {
  # Have the user select a value and save it to windows credential
  cli::cli_alert_info("Please select the location of the database file (quality.duckdb)")
  db_raw <- rstudioapi::selectFile(caption = "Select the Quality Database File (quality.duckdb)",
                                   filter = "Quality Database (*.duckdb)")
  if (is.null(db_raw)) cli::cli_abort("Database File selection cancelled")
  db_file <- normalizePath(db_raw, winslash = "/")
  cli::cli_alert_success("Saving database file location")

  # Save to Windows Credential
  keyring::key_set_with_value(service = "LRH_DB", password = db_file)

  # Save to package environment
  .ddb_env$lrh_db <- db_file
}

#' Path helper for the Sharepoint Tableau Data Folder
#'
#' Helper function to return the local location of the LRH Tableau Data Folder.
#' Can be run with arguments like [file.path()]
#'
#' @param ... additional arguments passed to [file.path()]
#'
#' @returns a file path
#' @export
#' @md
tableau_data_path <- function(...) {

  # Hard-coded parent folder
  f <- "C:\\Users\\stalcott\\Littleton Regional Healthcare\\Sam Tableau - General\\Quality\\Tableau Data"

  if (!dir.exists(f)) cli::cli_abort("Could not find Tableau Data folder at {.path {f}}")

  normalizePath(file.path(f, ...))
}


#' Helper function to connect to the LRH DuckDB (now wraps lrh_con())
#'
#' @param db_file Location of the DuckDB. Defaults to [lrh_db()]
#' @param timezone_out Timezone to use for pulling date-time columns
#' @param tz_out_convert "with" or "force"
#' @param ... Optional additional arguments
#'
#' @returns a database connection
#' @export
connect_duckdb <- function(db_file = lrh_db(), timezone_out = "America/New_York",
                           tz_out_convert = "with", ...) {

  lrh_con(db_file = db_file, timezone_out = timezone_out,
          tz_out_convert = tz_out_convert, type = "read_write", ...)
}

#' Get The time a table as last updated
#'
#' `lastupdate_duckdb()` returns the raw value of the last update (for use in
#' scripts). `lastupdate_alert_duckdb()` is a convenience function that wraps
#' the last updated time in a cli-style message.
#'
#' @param table Table name
#' @param dt_col the date column of the table. If not provided, will search for a column that includes "update_dt_tm".
#' @param min_dt Generates an error if the last update time is less than this value.
#'
#' @md
#' @returns Prints a cli info statement with the last updated time
#' @export
lastupdate_duckdb <- function(table, dt_col = NULL) {

  # Connect to the DuckDB database in read_only
  con <- lrh_con()

  # Get the update column if it isn't provided
  if (is.null(dt_col)) {
    dt_col <- DBI::dbListFields(con, table) |>
      grep("update_dt_tm", x = _, value = TRUE)

    if (length(dt_col) > 1) {
      cli::cli_abort(c(
        "x" = "No update column specified, and multiple possible columns found in {.val {table}}",
        "i" = "{.val {dt_col}}"
      ))
    } else if (length(dt_col) == 0) {
      cli::cli_abort(c(
        "x" = "No update column specified, and no columns containing {.val update_dt_tm} found in {.val {table}}"
      ))
    }
  }

  q <- stringr::str_glue("SELECT MAX({dt_col}) FROM {table}")

  # Return last update time
  DBI::dbGetQuery(con, q)[[1]]
}

#' @export
#' @rdname lastupdate_duckdb
lastupdate_alert_duckdb <- function(table, dt_col = NULL) {

  m <- lastupdate_duckdb(table, dt_col)

  cli::cli_inform(c(
    "i" = "The {.val {table}} table was last updated on {.val {m}}"
  ))

}

#' @rdname lastupdate_duckdb
#' @export
lastupdate_min_duckdb <- function(table, min_dt, dt_col = NULL) {
  m <- lastupdate_duckdb(table, dt_col)

  if (m < min_dt) cli::cli_abort(c(
    "x" = "The {.val {table}} table was last updated on {.val {m}}",
    "i" = "Specified minimum update date: {.val {min_dt}}"
  ))
}

#' Materialize a view
#'
#' Runs `CREATE OR REPLACE TABLE {table} AS SELECT * FROM {view}`.
#'
#' Unless `check = FALSE`, a set of pre-flight checks runs first:
#' * `view` exists in the database, and is a view (not a table)
#' * `table`, if it already exists, is a table (not a view)
#' * the existing table's columns match the view's columns (plus
#'   `materialized` when `materialized_col = TRUE`) in name, order and type
#'
#' A `table` that doesn't exist yet is reported but is not a violation.
#'
#' @param view name of the view to materialize
#' @param table name of the table to create
#' @param quiet If TRUE doesn't display the success message
#' @param materialized_col Add a column `materialized` with the current date-time?
#' @param check Run the pre-flight checks? Defaults to TRUE.
#' @param on_violation What to do when a pre-flight check fails. `"prompt"`
#'   (the default) asks whether to continue in an interactive session and
#'   warns-and-continues otherwise; `"abort"` always errors; `"warn"` always
#'   continues.
#'
#' @returns tbl(con, table), invisibly
#' @export
#' @md
materialize <- function(view, table, quiet = FALSE, materialized_col = FALSE,
                        check = TRUE, on_violation = c("prompt", "abort", "warn")) {

  on_violation <- match.arg(on_violation)

  # Restore previous connection type on exit if it was read_only
  prev_type <- .ddb_env$con_type
  on.exit(if (identical(prev_type, "read_only")) lrh_con(type = "read_only"), add = TRUE)

  # Connect to the DuckDB database in read_write
  con <- lrh_con(type = "read_write")

  if (check) {
    materialize_preflight(view, table, materialized_col, on_violation, con)
  }

  if (materialized_col) {
    s <- "*, NOW() AS materialized"
  } else {
    s <- "*"
  }
  DBI::dbExecute(con, stringr::str_glue("CREATE OR REPLACE TABLE {table} AS SELECT {s} FROM {view}"))
  DBI::dbExecute(con, "CHECKPOINT")
  gc()
  if (!quiet) cli::cli_alert_success("Materialized {.var {table}}")

  # Clear exit handler and run now
  on.exit()
  if (identical(prev_type, "read_only")) con <- lrh_con(type = "read_only")

  invisible(dplyr::tbl(con, table))
}

#' Look up a database object in the catalog
#'
#' @param x object name, optionally schema-qualified
#' @param con connection
#'
#' @returns The object's `table_type` -- usually `"VIEW"` or `"BASE TABLE"` --
#'   or `NA` if no such object exists
#' @export
#' @md
#' @examples
#' \dontrun{
#' ddb_object_type("orders_view")   # "VIEW"
#' ddb_object_type("orders_mview")  # "BASE TABLE"
#' }
ddb_object_type <- function(x, con = lrh_con()) {

  parts <- strsplit(gsub('"', "", x, fixed = TRUE), ".", fixed = TRUE)[[1]]
  nm <- utils::tail(parts, 1)
  schema <- if (length(parts) > 1) parts[length(parts) - 1] else NULL

  q <- paste0(
    "SELECT table_type FROM information_schema.tables WHERE table_name = ",
    DBI::dbQuoteString(con, nm),
    if (!is.null(schema)) paste0(" AND table_schema = ", DBI::dbQuoteString(con, schema))
  )

  res <- DBI::dbGetQuery(con, q)[[1]]

  if (length(res) == 0) NA_character_ else res[[1]]
}

#' Pre-flight checks for materialize()
#'
#' @inheritParams materialize
#' @param con connection
#'
#' @returns invisible(TRUE) if clear, otherwise messages / prompts / aborts
#' @noRd
materialize_preflight <- function(view, table, materialized_col = FALSE,
                                  on_violation = "prompt", con = lrh_con()) {

  bullets <- character()

  # -- Source must exist, and must be a view ---------------------------------
  view_type <- ddb_object_type(view, con)
  view_ok <- identical(view_type, "VIEW")

  if (is.na(view_type)) {
    bullets <- c(bullets, "x" = "Source {.val {view}} does not exist in the database")
  } else if (!view_ok) {
    bullets <- c(bullets, "x" = "Source {.val {view}} is a {.val {view_type}}, not a {.val VIEW}")
  }

  # -- Destination, if it exists, must be a table ----------------------------
  table_type <- ddb_object_type(table, con)
  table_ok <- identical(table_type, "BASE TABLE")

  if (is.na(table_type)) {
    cli::cli_inform(c("i" = "Table {.val {table}} does not exist yet and will be created"))
  } else if (!table_ok) {
    bullets <- c(bullets, "x" = "Destination {.val {table}} is a {.val {table_type}}, not a table")
  }

  # -- Columns must match ----------------------------------------------------
  if (!is.na(view_type) && table_ok) {

    v_cols <- ddb_columns(view, con)
    if (materialized_col) {
      v_cols <- dplyr::bind_rows(
        v_cols,
        data.frame(column_name = "materialized", column_type = "TIMESTAMP WITH TIME ZONE")
      )
    }
    t_cols <- ddb_columns(table, con)

    missing <- setdiff(v_cols$column_name, t_cols$column_name)
    added   <- setdiff(t_cols$column_name, v_cols$column_name)

    if (length(missing) > 0) {
      bullets <- c(bullets, "x" = "New in {.val {view}}, not in {.val {table}}: {.val {missing}}")
    }
    if (length(added) > 0) {
      bullets <- c(bullets, "x" = "In {.val {table}} but no longer in {.val {view}}: {.val {added}}")
    }

    # Order and type, for the columns present in both
    both <- intersect(v_cols$column_name, t_cols$column_name)

    if (length(missing) == 0 && length(added) == 0 &&
        !identical(v_cols$column_name, t_cols$column_name)) {
      bullets <- c(bullets, "!" = "Column {.emph order} differs between {.val {view}} and {.val {table}}")
    }

    v_type <- v_cols$column_type[match(both, v_cols$column_name)]
    t_type <- t_cols$column_type[match(both, t_cols$column_name)]
    i <- which(v_type != t_type)

    if (length(i) > 0) {
      changed <- paste0(both[i], " (", t_type[i], " -> ", v_type[i], ")")
      bullets <- c(bullets, "!" = "Column type{?s} changed: {.val {changed}}")
    }
  }

  if (length(bullets) == 0) return(invisible(TRUE))

  bullets <- c(
    "!" = "Pre-flight checks failed for {.code materialize(\"{view}\", \"{table}\")}",
    bullets
  )

  if (on_violation == "abort") {
    cli::cli_abort(bullets)
  }

  cli::cli_inform(bullets)

  if (on_violation == "warn" || !interactive()) {
    cli::cli_warn("Continuing anyway")
    return(invisible(FALSE))
  }

  if (!isTRUE(utils::askYesNo("Materialize anyway?", default = FALSE))) {
    cli::cli_abort("Materialization of {.val {table}} cancelled")
  }

  invisible(FALSE)
}

#' Column names and types of a table or view
#'
#' @param x name of the table or view
#' @param con connection
#'
#' @returns A data.frame with `column_name` and `column_type`, in position order
#' @export
#' @md
#' @examples
#' \dontrun{
#' ddb_columns("orders_view")
#' }
ddb_columns <- function(x, con = lrh_con()) {
  DBI::dbGetQuery(con, stringr::str_glue("DESCRIBE {x}"))[c("column_name", "column_type")]
}

#' Check materialization date
#'
#' @param table table to check
#' @param dt date it should be after
#' @param dt_col defaults to "materialized"
#' @param con connection
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @md
materialized_after <- function(table, dt, dt_col = "materialized", con = lrh_con()) {

  # Coerce to date-time
  if (inherits(dt, "Date")) dt <- as.POSIXct(format(dt), tz = Sys.timezone())

  t <- dplyr::tbl(con, table)

  if (!dt_col %in% colnames(t)) {
    cli::cli_abort("Table {.val {table}} doesn't contain column {.val {dt_col}}")
  }

  m <- dplyr::select(t, dplyr::all_of(dt_col)) |> dplyr::distinct() |> dplyr::pull()

  if (length(m) != 1 || !inherits(m, "POSIXt")) {
    cli::cli_abort(c("x" = "{.val {dt_col}} Needs to contain a single date-time",
                     "i" = "Current value(s): {.val {m}}"))
  }
  m >= dt
}
