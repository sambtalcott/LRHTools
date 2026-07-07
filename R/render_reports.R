#' Render a parameterized Quarto report to PDF
#'
#' Renders a `.qmd` file with `quarto::quarto_render()` and then prints the
#' resulting sibling `.html` file to PDF with `pagedown::chrome_print()`.
#' This is the single-report unit used by the individual/department report
#' runner scripts (BCMA, OPPE, PG Unit, Trauma).
#'
#' Because each render overwrites the html (and its figure files) from the
#' previous one, render + print must happen as a single step per report.
#'
#' @param qmd_file Path to the `.qmd` file
#' @param pdf_file Output path for the PDF
#' @param params Named list of parameters passed to `execute_params`
#' @param quiet Suppress quarto rendering output? Defaults to `TRUE`
#' @param ... Additional arguments passed to [pagedown::chrome_print()], e.g.
#'   `options = list(pageRanges = "1")` or `timeout = 60`
#'
#' @returns `pdf_file`, invisibly
#' @export
#' @md
render_qmd_pdf <- function(qmd_file, pdf_file, params = list(),
                           quiet = TRUE, ...) {
  rlang::check_installed(c("quarto", "pagedown"))

  # Uses the qmd file name but with html
  html_file <- stringr::str_replace(qmd_file, "\\.qmd$", ".html")

  quarto::quarto_render(input = qmd_file, execute_params = params,
                        quiet = quiet)

  pagedown::chrome_print(input = html_file, output = pdf_file, ...)

  invisible(pdf_file)
}

#' Render a batch of parameterized Quarto reports to PDF
#'
#' Runs [render_qmd_pdf()] for each row of `run_df` with the standard
#' progress bar, skipping reports whose PDF already exists unless
#' `overwrite = TRUE`.
#'
#' NOTE: chrome_print occasionally drops reports with handle_read_frame
#' errors. Run twice with `overwrite = FALSE` to ensure none are missed.
#'
#' @param run_df A data frame with one row per report. Must contain `file_col`
#'   plus the columns named in `param_cols`.
#' @param qmd_file Path to the `.qmd` file
#' @param param_cols Character vector of `run_df` column names to pass as
#'   quarto parameters. Optionally named to map a column to a differently
#'   named quarto param, e.g. `c(id = "rn_id", "month")` passes the `rn_id`
#'   column as param `id`
#' @param file_col Name of the column holding output PDF paths. Defaults to
#'   `"pdf_file"`
#' @param overwrite Re-render reports whose PDF already exists? Defaults to
#'   `FALSE`
#' @param post_render Optional function called with the row (a one-row tibble)
#'   after each successful render, e.g. to copy figure files
#' @param quiet Suppress quarto rendering output? Defaults to `TRUE`
#' @param ... Additional arguments passed to [pagedown::chrome_print()]
#'
#' @returns The rendered subset of `run_df`, invisibly
#' @importFrom rlang .data
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' render_reports(run_df, "Prod/OPPE 03 Ind Report.qmd",
#'                param_cols = c("person", "quarter"))
#' }
render_reports <- function(run_df, qmd_file, param_cols,
                           file_col = "pdf_file", overwrite = FALSE,
                           post_render = NULL, quiet = TRUE, ...) {

  missing_cols <- setdiff(c(file_col, param_cols), names(run_df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.var run_df} is missing column{?s} {.val {missing_cols}}")
  }

  # Unnamed entries pass under the column name; named entries under the name
  param_names <- names(param_cols)
  if (is.null(param_names)) {
    param_names <- param_cols
  } else {
    param_names[param_names == ""] <- param_cols[param_names == ""]
  }

  todo <- run_df
  if (!overwrite) {
    todo <- dplyr::filter(todo, !file.exists(.data[[file_col]]))
  }

  cli::cli_inform(c("i" = "Creating {.val {nrow(todo)}} PDF{?s}"))

  purrr::walk(seq_len(nrow(todo)), \(i) {
    row <- todo[i, ]
    render_qmd_pdf(qmd_file, row[[file_col]],
                   params = rlang::set_names(as.list(row[unname(param_cols)]),
                                             param_names),
                   quiet = quiet, ...)
    if (!is.null(post_render)) post_render(row)
  }, .progress = list(
    type = "iterator",
    format = "Creating PDFs {cli::pb_bar} {cli::pb_current} / {cli::pb_total}. ETA:{cli::pb_eta}"
  ))

  invisible(todo)
}
