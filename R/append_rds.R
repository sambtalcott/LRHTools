

#' Add data to an existing .rds file
#'
#' @param x Object (data frame) to be appended
#' @param file Location of the .rds file
#' @param remove_duplicates Should duplicate rows be removed? Defaults to TRUE.
#' @param add_cols Should new columns in x be added to the file? If FALSE (default),
#'                 will throw an error if new columns are found.
#'
#' @md
#' @returns the full data file (invisibly)
#' @export
#'
append_rds <- function(x, file, remove_duplicates = TRUE, add_cols = FALSE) {
  df <- readr::read_rds(file)

  # Check on column inclusion
  new_cols <- setdiff(names(x), names(df))
  if (length(new_cols) > 0 && !add_cols) {
    cli::cli_abort(c("x" = "Data contains new column{?s} {.var {new_cols}} which {?isn't/aren't} in the current file",
                     "i" = "Run with argument `add_cols = TRUE` to add these columns"))
  }

  unused_cols <- setdiff(names(df), names(x))
  if (length(unused_cols) > 0) {
    cli::cli_warn(c("!" = "Current file includes column{?s} {.var {unused_cols}}, which {?isn't/aren't} in the data",
                    "i" = "Missing column(s) will be filled with {.val NA}"))
  }

  # Combine and remove duplicates if needed
  out <- dplyr::bind_rows(df, x)
  if (remove_duplicates) out <- dplyr::distinct(out)

  readr::write_rds(out, file)
}
