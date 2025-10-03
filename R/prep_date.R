#' Prepend a formatted date on the file name
#'
#' Prepends a date to the file part of a file location string. So the location
#' `foo/bar.csv` becomes `foo/2025-01-01.bar.csv`. `prep_ymd()` uses a default
#' of YYYY-MM-DD. `prep_ym()` uses a default of YYYY-MM, and `prep_y()` uses YYYY
#'
#' @param file File name
#' @param date Date reference
#' @param fmt Formatting string. See [strptime] for details
#' @param sep Separator between date and filename
#'
#' @md
#' @returns a formatted file name with the date prepended followed by "."
#' @export
prep_ymd <- function(file, date, fmt = "%Y-%m-%d", sep = ".") {
  base <- basename(file)
  folder <- stringr::str_remove(file, stringr::fixed(base))
  paste0(folder, format(date, fmt), sep, base)
}

#' @rdname prep_ymd
#' @export
prep_ym <- function(file, date, fmt = "%Y-%m", sep = ".") {
  prep_ymd(file, date, fmt, sep)
}

#' @rdname prep_ymd
#' @export
prep_y <- function(file, date, fmt = "%Y", sep = ".") {
  prep_ymd(file, date, fmt, sep)
}

#' Format display months for plots
#'
#' Designed to be used in `scale_x_date()` or `scale_x_datetime()`. Formats the
#' months of the year so that the first displayed month of each year gets a year
#' (e.g. "Jan 2024") and following months get only the month ("Feb")
#'
#' `lab_yq()` does the same, but for quarters.
#'
#' @param x A vector of dates or date/times
#'
#' @returns A character vector for displays
#' @export
#' @md
lab_ym <- function(x) {

  if (!identical(sort(x[!is.na(x)]), x[!is.na(x)])) {
    cli::cli_abort("`lab_ym()` expects a sorted vector of dates")
  }

  first_dates <- split(x, lubridate::year(x)) |>
    purrr::map(1) |>
    unlist(use.names = FALSE) |>
    as.Date()

  dplyr::if_else(x %in% first_dates, format(x, "%b\n%Y"), format(x, "%b"))

}

#' @export
#' @rdname lab_ym
lab_yq <- function(x) {

  if (!identical(sort(x[!is.na(x)]), x[!is.na(x)])) {
    cli::cli_abort("`lab_yq()` expects a sorted vector of dates")
  }

  first_dates <- split(x, lubridate::year(x)) |>
    purrr::map(1) |>
    unlist(use.names = FALSE) |>
    as.Date()

  x_q <- stringr::str_c("Q", lubridate::quarter(x))

  dplyr::if_else(x %in% first_dates, paste0(x_q, "\n", lubridate::year(x)), x_q)

}
