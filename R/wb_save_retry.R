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

  # openxlsx2's "Failed to save workbook" is a bare stop() after file.copy()
  # returns FALSE, and file.copy() fails *silently* (file.create with
  # showWarnings = FALSE), so no OS reason ever reaches the handlers above.
  # Probe the destination side ourselves: if the destination opens fine for
  # write, the copy must have failed reading the freshly-zipped source file
  # (e.g. an endpoint-security scan lock).
  diagnose <- function() {
    dir <- dirname(file)
    existed <- file.exists(file) # before the probe, which creates it
    probe <- tryCatch(
      { close(base::file(file, open = "wb")); "writable" },
      error = \(e) conditionMessage(e),
      warning = \(w) conditionMessage(w)
    )
    cli::cli_alert_warning(paste0(
      "Save diagnostics: dir exists: {dir.exists(dir)} | ",
      "dir writable: {file.access(dir, 2) == 0} | ",
      "dest existed: {existed} | ",
      "dest open-for-write: {probe}"
    ))
  }

  for (wait in waits) {
    ok <- tryCatch({save_once(); TRUE}, error = \(e) {
      cli::cli_alert_warning(
        "Workbook save failed ({conditionMessage(e)}). Retrying in {wait}s"
      )
      diagnose()
      FALSE
    })
    if (ok) return(invisible(file))
    Sys.sleep(wait)
  }

  # One last attempt to surface real errors (diagnostics logged before the
  # error propagates)
  withCallingHandlers(save_once(), error = \(e) diagnose())
  invisible(file)
}
