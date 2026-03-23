# Internal environment to store flags (survives rm(list = ls()) in user scripts)
.lrh_flags <- new.env(parent = emptyenv())
.lrh_flags$items <- character(0)

#' Register a flag for end-of-script summary
#'
#' Stores the message for later display via [lrh_show_flags()] and
#' prints it inline as a `cli_alert_info()`. Use this in place of
#' `cli_alert_info()` for messages you want surfaced in the summary.
#'
#' @param msg Character string. Supports cli inline markup (e.g. `{.val x}`).
#' @param .envir Environment for cli glue interpolation (default: caller).
#' @return The rendered message string, invisibly.
#' @export
#' @md
lrh_flag <- function(msg, .envir = parent.frame()) {
  rendered <- cli::cli_fmt(cli::cli_text(msg, .envir = .envir))
  .lrh_flags$items <- c(.lrh_flags$items, rendered)
  cli::cli_alert_info(msg, .envir = .envir)
  invisible(rendered)
}

#' Display all accumulated flags
#'
#' Call at the end of a script to show a summary of all flags registered
#' via [lrh_flag()]. Shows a success message if no flags were registered.
#'
#' @param clear Logical. Clear flags after displaying? Default `TRUE`.
#' @return The flag messages, invisibly.
#' @export
lrh_show_flags <- function(clear = TRUE) {
  flags <- .lrh_flags$items
  if (length(flags) == 0) {
    cli::cli_alert_success("No warnings or flags to report.")
  } else {
    cli::cli_h2("Daily Script Flags ({length(flags)})")
    for (flag in flags) cli::cli_li(flag)
  }
  if (clear) lrh_clear_flags()
  invisible(flags)
}

#' Clear all accumulated flags
#' @export
lrh_clear_flags <- function() {
  .lrh_flags$items <- character(0)
  invisible(NULL)
}
