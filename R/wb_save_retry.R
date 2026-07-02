#' Save an openxlsx2 workbook with retries
#'
#' openxlsx2's `wb$save()` ends with a bare `file.copy()` of the finished
#' workbook out of its temp build directory; endpoint security (e.g.
#' CrowdStrike) can briefly hold a lock on the freshly-created zip, making the
#' copy fail intermittently with a generic "Failed to save workbook" error
#' while the OS-level warning explaining why is swallowed. This wrapper retries
#' with backoff and surfaces any warnings raised during failed attempts, so the
#' real cause lands in run logs.
#'
#' @param wb an openxlsx2 `wbWorkbook`
#' @param file path to save to
#' @param ... additional arguments passed to `wb$save()`
#' @param waits seconds to wait after each failed attempt; one final attempt
#'   is made after the last wait, with errors propagated
#'
#' @returns the path, invisibly
#' @noRd
wb_save_retry <- function(wb, file, ..., waits = c(1, 3, 9)) {

  save_once <- function() {
    withCallingHandlers(
      wb$save(file = file, ...),
      warning = \(w) {
        cli::cli_alert_warning("Workbook save warning: {conditionMessage(w)}")
        invokeRestart("muffleWarning")
      }
    )
  }

  for (wait in waits) {
    ok <- tryCatch({save_once(); TRUE}, error = \(e) {
      cli::cli_alert_warning(
        "Workbook save failed ({conditionMessage(e)}). Retrying in {wait}s"
      )
      FALSE
    })
    if (ok) return(invisible(file))
    Sys.sleep(wait)
  }

  # One last attempt to surface real errors
  save_once()
  invisible(file)
}
