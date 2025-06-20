

#' LRH CSV writer
#'
#' Wraps `write.csv()` with some standard defaults. Unlike `readr::write_csv()`,
#' this function will write date-times in their current timezone (rather than
#' translating to UTC).
#'
#' @param x the object to be written, preferably a matrix or data frame. If not,
#'   it is attempted to coerce x to a data frame.
#' @param file either a character string naming a file or a connection open for
#'   writing. "" indicates output to the console.
#' @param na the string to use for missing values in the data.
#' @param row.names either a logical value indicating whether the row names of x
#'   are to be written along with x, or a character vector of row names to be written.
#' @param ... Additional arguments passed on to `write.csv()`
#'
#' @returns `x`, invisibly
#' @export
#' @md
lrh_csv <- function(x, file, na = "", row.names = FALSE, ...) {
  utils::write.csv(x = x, file = file, na = na, row.names = row.names, ...)
  invisible(x)
}

#' LRH Excel Writer
#'
#' Uses `openxlsx2` to write to an excel file. Accepts a single data frame or a
#' list of data frames for multiple tabs. Data is written as a table, and column
#' widths are set to "auto" by default.
#'
#' By default, writes to a tempfile and opens the file, but can also be used to
#' write to specified location with the `file` parameter
#'
#' @param x a dataframe or list of dataframes
#' @param widths column widths (default: auto)
#' @param table_style Excel table style (default: gray/striped)
#' @param file path to write to (defaults to tempfile)
#' @param open open the file after writing? By default, opens tempfiles but not
#' other files
#' @param na what NA values are written as
#'
#' @returns the path, invisibly
#' @export
#'
#' @md
lrh_excel <- function(x, widths = "auto", table_style = "TableStyleMedium1",
                      file = NULL, open = is.null(file), na = "") {

  force(open)
  # Get x to be a list of dataframes (if only a single dataframe was provided)
  if (!is.data.frame(x[[1]])) x <- list(x)

  if (!is.null(names(x))) x <- janitor::clean_names(x)

  wb <- openxlsx2::wb_workbook()

  purrr::iwalk(x, \(df, nm) {
    wb$
      add_worksheet(nm)$
      add_data_table(x = df, table_style = table_style, na.strings = na)$
      set_col_widths(cols = 1:ncol(df), widths = "auto")
  })

  if (is.null(file)) file <- tempfile(fileext = ".xlsx")

  wb$save(file = file)

  if (open) shell.exec(file)

  invisible(file)
}
