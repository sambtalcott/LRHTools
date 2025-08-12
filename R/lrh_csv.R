

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
#' @param x a dataframe or list of dataframes. Lazy tibbles are accepted and will
#' be fetched using `collect()`
#' @param widths column widths (default: auto)
#' @param table_style Excel table style (default: gray/striped)
#' @param wrap Should columns be word-wrapped? (default: TRUE)
#' @param file path to write to (defaults to tempfile)
#' @param open open the file after writing? By default, opens tempfiles but not
#' other files
#' @param na what NA values are written as
#' @param return_wb Should this return the openxlsx2 workbook object? If FALSE
#' returns the path instead.
#'
#' @returns the path, invisibly
#' @export
#'
#' @md
lrh_excel <- function(x, widths = "auto", table_style = "TableStyleMedium1",
                      wrap = TRUE, file = NULL, open = is.null(file), na = "",
                      return_wb = FALSE) {

  # Evaluate now (before temp file is created)
  force(open)

  # Get x to be a list of dataframes (if only a single dataframe was provided)
  if (!inherits(x, "list")) x <- list(x)

  # Clean names for sheet names
  if (!is.null(names(x))) x <- janitor::clean_names(x)

  # Collect any lazy tables
  x <- purrr::map(x, dplyr::collect)

  wb <- openxlsx2::wb_workbook()

  purrr::iwalk(x, \(df, nm) {
    wb$add_worksheet(nm)$
      add_data_table(x = df, table_style = table_style, na.strings = na)$
      set_col_widths(cols = 1:ncol(df), widths = widths)

    if (wrap) {
      if (isTRUE(widths == "auto")) {
        # Estimate new col widths
        col_widths <- df |>
          purrr::keep(~any(grepl("\n", .x))) |> # Only columns with a line break
          purrr::imap(\(vec, name) { # Get maximum width
            stringr::str_split(vec, "\n") |> unlist() |>
              c(name) |> sapply(stringr::str_length) |> max(na.rm = TRUE)
          })

        # Apply new col widths
        purrr::iwalk(col_widths, \(w, col) {
          wb$set_col_widths(cols = which(names(df) == col), widths = w)
        })
      }

      # Wrap cells
      wb$add_cell_style(dims = openxlsx2::wb_dims(x = df), wrap_text = TRUE)

    }
  })

  if (is.null(file)) file <- tempfile(fileext = ".xlsx")

  wb$save(file = file)

  if (open) shell.exec(file)

  if (return_wb) invisible(wb) else invisible(file)
}
