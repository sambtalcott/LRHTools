

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
