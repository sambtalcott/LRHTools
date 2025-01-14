#' Check if a file is locked for editing
#'
#' @param file Path to the file to check
#'
#' @md
#' @returns TRUE or FALSE
#' @export
is_file_locked <- function(file) {
  con <- try(file(file, open = "r+"), silent = TRUE)

  if (inherits(con, "try-error")) {
    # If there's an error, the file might be locked
    return(TRUE)
  } else {
    close(con)  # Close connection if no error
    return(FALSE)
  }
}
