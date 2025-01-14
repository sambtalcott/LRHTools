#' Import from an `XLSX` DA2 Export File
#'
#' @param loc File location
#' @param first_header The first header title. Used to identify the starting row
#' @param filter_out A character vector of A values to filter out.
#' @param clean_names Should column names be cleaned with `janitor::clean_names()`?
#'
#' @md
#' @returns a tibble
#' @export
import_da2 <- function(loc, first_header, filter_out = c(NA, "Total Cases:"), clean_names = TRUE) {
  # Determine header row
  a_vals <- readxl::read_excel(loc, range = "A1:A10", col_names = "A") |> dplyr::pull("A")
  header_row <- which(a_vals == first_header)

  if (length(header_row) == 0) cli::cli_abort("{.val {first_header}} not found in column A")
  if (length(header_row) > 1) cli::cli_abort("{lval {first_header}} found multiple times in column A")

  # Import from that header row
  raw <- readxl::read_excel(loc, skip = header_row - 1)
  if (clean_names) raw <- janitor::clean_names(raw)

  # Filter out blank/total/etc.
  dplyr::filter(raw, !raw[[1]] %in% filter_out)
}
