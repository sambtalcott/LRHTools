# Package-specific environment for storing the database location
.ddb_env <- new.env(parent = emptyenv())

#' Append only unique rows to a duckdb table
#'
#' @param x Object (data frame) to be appended
#' @param table Table to append to
#' @param db_file Location of the duckdb file (defaults to [lrh_db()])
#' @param remove_duplicates Should duplicate rows be removed? Defaults to TRUE.
#' @param con (Optional) connection object if you are already connected to the DB.
#'
#' @md
#' @returns nothing
#' @export
append_duckdb <- function(x, table, db_file = lrh_db(), remove_duplicates = TRUE, con = NULL) {
  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- connect_duckdb(db_file = db_file)
    # In this case, set up a disconnect when the function ends
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  # Check if the table already exists in the DuckDB database
  existing_tables <- DBI::dbListTables(con)
  if (!(table %in% existing_tables)) {
    # If the table doesn't exist, just write the new data and return
    DBI::dbWriteTable(con, table, x, append = FALSE)
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
#' @param db_file Location of the duckdb file (defaults to [lrh_db()])
#' @param con (Optional) connection object if you are already connected to the DB.
#'
#' @returns Nothing
#' @export
replace_duckdb <- function(x, table, delete_where, db_file = lrh_db(), con = NULL) {
  if (is.null(con)) {
    con <- connect_duckdb(db_file = db_file)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
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
#' @param db_file Location of the duckdb file (defaults to [lrh_db()])
#' @param con (Optional) connection object if you are already connected to the DB.
#' @param max_rows Maximum number of rows to pull (by default, pulls all)
#'
#' @returns a tibble
#' @export
pull_duckdb <- function(table, db_file = lrh_db(), con = NULL, max_rows = Inf) {
  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- connect_duckdb(db_file = db_file)
    # In this case, set up a disconnect when the function ends
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  q <- paste0("Select * FROM ", table)
  if (max_rows != Inf) q <- paste0(q, " LIMIT ", max_rows)

  DBI::dbGetQuery(con, q) |>
    tibble::as_tibble()
}

#' Write a dataframe to duckdb using TIMESTAMPTZ for date-time fields
#'
#' @param table Name of table to write to
#' @param x dataframe
#' @param con (Optional) connection object if you are already connected to the DB.
#' @param db_file Location of the duckdb file (defaults to [lrh_db()])
#'
#' @returns x (invisibly)
#' @export
#' @md
write_tz_duckdb <- function(table, x, con = NULL, db_file = lrh_db()) {
  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- connect_duckdb(db_file = db_file)
    # In this case, set up a disconnect when the function ends
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  # Determine which fields should be written as TIMESTAMPTZ
  dt_fields <- names(purrr::keep(x, lubridate::is.POSIXct))
  ft <- rlang::set_names(rep("TIMESTAMPTZ", length(dt_fields)), dt_fields)

  # Write table
  DBI::dbWriteTable(con, table, x, field.types = ft)

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
#' Performs an upsert on a duckdb table.
#'
#' @param table Table name
#' @param x data frame to update
#' @param id_col the id column of the table
#' @param dt_col the date column of the table
#' @param db_file Location of the duckdb file (defaults to [lrh_db()])
#' @param con (Optional) connection object if you are already connected to the DB.
#' @param od_folder The folder to save the .csv file in for `upsert_duckdb_od()`
#' @param loc_folder The folder to save the .cvs file in for `upsert_duckdb_local()`
#'
#' @returns number of rows affected
#' @export
#' @md
upsert_duckdb <- function(table, x, id_col, dt_col, db_file = lrh_db(), con = NULL) {

  # Check that x is unique by primary key (crashes R otherwise)
  if (!identical(x[[id_col]], unique(x[[id_col]]))) {
    cli::cli_abort(c("x" = "{.var x} contains duplicate values of {.var {id_col}}"))
  }
  if (any(is.na(x[[id_col]]))) {
    cli::cli_abort(c("x" = "{.var x} contains NAs in {.var {id_col}}"))
  }

  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- connect_duckdb(db_file = db_file)
    # In this case, set up a disconnect when the function ends
    on.exit(DBI::dbDisconnect(con), add = TRUE)
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

  # Perform UPSERT
  rows <- DBI::dbExecute(con, iq)

  # Remove staging table
  DBI::dbRemoveTable(con, "stg")

  # Return rows
  rows
}

#' Upsert into DuckDB AND write .csv to OneDrive for Tableau
#' @export
#' @rdname upsert_duckdb
upsert_duckdb_od <- function(table, x, id_col, dt_col, db_file = lrh_db(), con = NULL,
                             od_folder = "Documents/.database/Tableau Data") {
  cli::cli_status("{.info Updating in DuckDB}")
  upsert_duckdb(table, x, id_col, dt_col, db_file, con)
  cli::cli_status("{.info Pulling final data from DuckDB}")
  x <- pull_duckdb(table, db_file, con)
  cli::cli_status("{.info Writing .csv to OneDrive}")
  od_write(x, file.path(od_folder, paste0(table, ".csv")))
  cli::cli_status_clear()
  cli::cli_alert_success("Successfully upserted {.val {table}}!")
}

#' Upsert into DuckDB AND write .csv locally for Tableau
#' @export
#' @rdname upsert_duckdb
upsert_duckdb_local <- function(table, x, id_col, dt_col, db_file = lrh_db(), con = NULL,
                                loc_folder = lrh_tableau_folder()) {
  cli::cli_status("{.info Updating in DuckDB}")
  upsert_duckdb(table, x, id_col, dt_col, db_file, con)
  cli::cli_status(cli::col_yellow("{.info Pulling final data from DuckDB}"))
  x <- pull_duckdb(table, db_file, con)
  cli::cli_status(cli::col_yellow("{.info Writing .csv locally}"))
  readr::write_csv(x, file.path(loc_folder, paste0(table, ".csv")))
  cli::cli_status_clear()
  cli::cli_alert_success(cli::col_yellow("Successfully upserted {.val {table}}!"))
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

#' LRH Tableau Folder
#'
#' Helper function to return the local location of the LRH Tableau Folder. Uses
#' the location of the lrh database (`lrh_db()`) to get the location of the
#' folder
#'
#' @returns a file path
#' @export
#' @md
lrh_tableau_folder <- function() {
  # Work backwards from the db file
  parent <- dirname(lrh_db())
  lrh_tab <- file.path(parent, "Tableau Data")
  if (!dir.exists(lrh_tab)) cli::cli_abort("Attempted to use folder {.val {lrh_tab}}, which doesn't exist")
  lrh_tab
}

#' @export
lrh_test <- "Test"


#' Helper function to connect to the LRH DuckDB
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

  con <- DBI::dbConnect(duckdb::duckdb(), db_file, , timezone_out = timezone_out,
                        tz_out_convert = tz_out_convert, ...)

  # Set autoload (for using things like 'today' in patient days view)
  DBI::dbExecute(con, "SET autoinstall_known_extensions=1;")
  DBI::dbExecute(con, "SET autoload_known_extensions=1;")

  # Set timezone (for importing) to UTC
  DBI::dbExecute(con, "SET TimeZone = 'UTC'")

  # return the connection
  con
}

#' Get The time a table as last updated
#'
#' `lastupdate_duckdb()` returns the raw value of the last update (for use in
#' scripts). `lastupdate_alert_duckdb()` is a convenience function that wraps
#' the last updated time in a cli-style message.
#'
#' @param table Table name
#' @param dt_col the date column of the table. If not provided, will search for a column that includes "update_dt_tm".
#' @param db_file Location of the duckdb file (defaults to [lrh_db()])
#' @param con (Optional) connection object if you are already connected to the DB.
#' @param min_dt Generates an error if the last update time is less than this value.
#'
#' @md
#' @returns Prints a cli info statement with the last updated time
#' @export
lastupdate_duckdb <- function(table, dt_col = NULL, db_file = lrh_db(), con = NULL) {

  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- connect_duckdb(db_file = db_file)
    # In this case, set up a disconnect when the function ends
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

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
lastupdate_alert_duckdb <- function(table, dt_col = NULL, db_file = lrh_db(), con = NULL) {

  m <- lastupdate_duckdb(table, dt_col, db_file, con)

  cli::cli_inform(c(
    "i" = "The {.val {table}} table was last updated on {.val {m}}"
  ))

}

#' @rdname lastupdate_duckdb
#' @export
lastupdate_min_duckdb <- function(table, min_dt, dt_col = NULL, db_file = lrh_db(), con = NULL) {
  m <- lastupdate_duckdb(table, dt_col, db_file, con)

  if (m < min_dt) cli::cli_abort(c(
    "x" = "The {.val {table}} table was last updated on {.val {m}}",
    "i" = "Specified minimum update date: {.val {min_dt}}"
  ))
}

