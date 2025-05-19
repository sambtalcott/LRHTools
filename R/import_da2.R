#' Import from an DA2 Query CSV
#'
#' Convenience function. Imports the csv (assuming infinite guessing), cleans
#' column names and converts any timestamp columns
#'
#' @param file file location
#' @param guess_max from `readr::read_csv()`. Defaults to infinite (slightly
#' slower, but no errors)
#' @param ... Other arguments passed on to `readr::read_csv()`
#' @param ddb_reference Optional: A duckdb table name to use as a reference for
#' the input. This ensures columns are read with the same column types as the
#' duckdb table. Uses `pull_duckdb()`
#' @param tz Timezone to use when converting date/time columns
#'
#' @md
#' @returns a tibble
#' @export
import_da2 <- function(file, guess_max = Inf, ddb_reference = NULL,
                       tz = "America/New_York", ...) {

  if (!grepl("\\.csv$", file)) {
    cli::cli_abort(c("x" = "{.code import_da2()} expects a {.val .csv} file",
                     "i" = "Provided file is {.val {basename(file)}}"))
  }

  if (!is.null(ddb_reference)) {
    # Pull the reference table and map it to column types
    ref <- pull_duckdb(ddb_reference, max_rows = 0)
    c_list <- purrr::map(ref, \(x) {
      if (inherits(x, "POSIXct") || inherits(x, "POSIXt") || is.character(x)) {
        readr::col_character()
      } else if (is.double(x)) {
        readr::col_double()
      } else if (is.integer(x)) {
        readr::col_integer()
      } else if (is.logical(x)) {
        readr::col_logical()
      } else {
        cli::cli_abort("Unknown column type in duckdb table {.val {ddb_reference}}")
      }
    })

    # Create a lookup for clean-unclean names
    nm <- readr::read_csv(file, n_max = 1, show_col_types = FALSE) |> names()
    names(nm) <- janitor::make_clean_names(nm)

    # Deal with mismatch errors in column names
    # Table is missing a column from file: error
    nm_missing_table <- setdiff(names(nm), names(c_list))
    if (length(nm_missing_table) > 0) {
      cli::cli_abort(c("x" = "File contains {cli::qty(nm_missing_table)}column{?s} {.val {nm_missing_table}} which {?is/are} not in table {.val {ddb_reference}}."))
    }
    # File is missing a column from table: warn
    nm_missing_file <- setdiff(names(c_list), names(nm))
    if (length(nm_missing_file) > 0) {
      cli::cli_warn(c("!" = "Table {.val {ddb_reference}} {cli::qty(nm_missing_file)}contains column{?s} {.val {nm_missing_file}} which {?is/are} not in the .csv file"))
      c_list <- c_list[names(nm)]
    }

    # Unclean the names in c_list
    names(c_list) <- nm[names(c_list)]


    # Pull with col_types specification
    x <- readr::read_csv(file, col_types = readr::cols(!!!c_list), ...)
  } else {
    # Pull without col_types spec
    x <- readr::read_csv(file, guess_max = guess_max, ...)
  }

  # Clean names and fix dates
  y <- x |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(dplyr::contains("dt_tm"),
                                \(x) lubridate::mdy_hm(x, tz = tz)))

  # If there were issues importing x, attach them to y
  attr(y, "problems") <- attr(x, "problems")

  return (y)
}
