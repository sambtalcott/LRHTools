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
#' `lab_yq()` does the same, but for quarters. `lab_md()` works at the
#' month/day level — the first displayed day of each month gets a month label
#' (e.g. "Jan 01") and following days get only the day ("02").
#'
#' @param x A vector of dates or date/times
#' @param fmt Format string for the month label in `lab_md()`. Defaults to
#'   `"%b"` (3-letter abbreviation). See [strptime] for details.
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
lab_md <- function(x, fmt = "%b") {

  if (!identical(sort(x[!is.na(x)]), x[!is.na(x)])) {
    cli::cli_abort("`lab_md()` expects a sorted vector of dates")
  }

  first_dates <- split(x, format(x, "%Y-%m")) |>
    purrr::map(1) |>
    unlist(use.names = FALSE) |>
    as.Date()

  dplyr::if_else(x %in% first_dates, format(x, paste0(fmt, "\n%d")), format(x, "%d"))

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

#' Most recent complete month
#'
#' Returns the most recent complete time period, given a certain amount of
#' flex. For example, a `flex` value of `days(5)` would allow June to count
#' as the most recent month for June 26th, but not June 25th.
#'
#' @param flex How flexible to be on the time period? Use `days()`, `weeks()`, etc.
#' @param unit What time unit to use. Passed to [lubridate::floor_date]
#'
#' @returns a date (no time zone)
#' @export
#'
#' @md
last_x <- function(unit, flex = lubridate::days(0)) {
  if (!lubridate::is.period(flex)) {
    cli::cli_abort(c(
      "x" = "Value of {.var flex} needs to be a period (e.g. `lubridate::days(5)`)",
      "i" = "Given value was {.val {flex}}"
    ))
  }
  lubridate::`%m-%`(Sys.time() + flex, months(1)) |>
    lubridate::floor_date(unit) |>
    lubridate::as_date()
}

#' @rdname last_x
#' @export
last_mo <- function(flex = lubridate::days(0)) {
  last_x("month", flex)
}

#' @rdname last_x
#' @export
last_q <- function(flex = lubridate::days(0)) {
  last_x("quarter", flex)
}
