#' Prepend a formatted date on the file name
#'
#' @param file File name
#' @param date Date reference
#' @param fmt Formatting string. See [strptime] for details
#' @param sep Separator
#'
#' @md
#' @returns a formatted file name with the date prepended followed by "."
#' @export
prep_date <- function(file, date, fmt = "%Y-%m-%d", sep = ".") {
  base <- basename(file)
  folder <- stringr::str_remove(file, stringr::fixed(base))
  paste0(folder, format(date, fmt), sep, base)
}
