#' Check if a local folder exists, and if not prompt for it.
#'
#' @param path the path to check
#'
#' @returns the current OR correct path
#' @export
#'
#' @examples
check_folder <- function(path) {
  if (!dir.exists(path)) {
    cli::cli_alert_info("Could not find folder {.val {path}}. Choose the correct folder to continue.")
    path <- rstudioapi::selectDirectory(caption = "Select folder")
    cli::cli_alert_info("Using folder {.val {path}}")
  }
  return(path)
}
