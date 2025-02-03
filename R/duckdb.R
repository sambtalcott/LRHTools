#' Append only unique rows to a duckdb table
#'
#' @param x Object (data frame) to be appended
#' @param table Table to append to
#' @param db_file Location of the duckdb file (defaults to [lrh_db])
#' @param remove_duplicates Should duplicate rows be removed? Defaults to TRUE.
#' @param con (Optional) connection object if you are already connected to the DB.
#'
#' @md
#' @returns nothing
#' @export
append_duckdb <- function(x, table, db_file = lrh_db, remove_duplicates = TRUE, con = NULL) {
  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_file)
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
#' @param db_file Location of the duckdb file (defaults to [lrh_db])
#' @param con (Optional) connection object if you are already connected to the DB.
#'
#' @returns Nothing
#' @export
replace_duckdb <- function(x, table, delete_where, db_file = lrh_db, con = NULL) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_file)
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
#' @param db_file Location of the duckdb file (defaults to [lrh_db])
#' @param con (Optional) connection object if you are already connected to the DB.
#'
#' @returns a tibble
#' @export
pull_duckdb <- function(table, db_file = lrh_db, con = NULL) {
  # Connect to the DuckDB database if connection isn't provided
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_file)
    # In this case, set up a disconnect when the function ends
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  DBI::dbGetQuery(con, paste0("SELECT * FROM ", table)) |>
    tibble::as_tibble()
}

#' Quality Database Location
#'
#' Hard-coded location of the quality database on the network.
#'
#' @export
lrh_db <- "//lrhfs1/Departments/Quality Services/4. Data/database/quality.duckdb"

#' Helper function to connect to the LRH DuckDB
#'
#' @param db_file Location of the DuckDB. Defaults to [lrh_db]
#' @param ... Optional additional arguments
#'
#' @returns a database connection
#' @export
connect_duckdb <- function(db_file = lrh_db, ...) {
  DBI::dbConnect(duckdb::duckdb(), db_file, ...)
}

