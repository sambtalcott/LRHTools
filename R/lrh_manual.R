
#' Load manual data from an Excel sheet
#'
#' Each tab will be turned into a tibble and loaded into the Global Environment
#' with names coming from the sheet names. Sheet names that start with "." will
#' be skipped (e.g. ".validation" or ".directions")
#'
#' @param xl_loc Location of the excel file
#'
#' @returns invisibly, the global environment
#' @export
lrh_manual_load <- function(xl_loc) {
  wb <- openxlsx2::wb_load(xl_loc)

  dat <- lrh_manual_names(wb) |>
    purrr::set_names() |>
    purrr::map(\(sheet) openxlsx2::wb_to_df(wb, sheet) |> tibble::as_tibble())

  invisible(list2env(dat, envir = .GlobalEnv))
}

#' List manual data names from an Excel sheet
#'
#' Gives the list of variables that would be imported for a given manual excel
#' sheet by [lrh_manual_load].
#'
#' @param xl_loc Location of the excel file OR an openxlsx2 workbook object
#'
#' @returns a character vector of sheet names / variable names
#' @export
lrh_manual_names <- function(xl_loc) {
  if ("wbWorkbook" %in% class(xl_loc)) wb <- xl_loc else wb <- openxlsx2::wb_load(xl_loc)
  openxlsx2::wb_get_sheet_names(wb) |>
    stringr::str_subset("^[^\\.]") # Remove sheets starting with "."
}
