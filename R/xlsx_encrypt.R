#' Password-encrypt an Office file
#'
#' Applies true file-level encryption (Excel's "Encrypt with Password", the
#' ECMA-376 agile encryption container) to an .xlsx/.docx/.pptx file, so the
#' file cannot be opened without the password. This is different from
#' `openxlsx2::wb_protect()`, which only locks workbook structure.
#'
#' Encryption is done by the Python tool `msoffcrypto-tool`, run through
#' `uvx` so nothing needs to be installed permanently. The password is passed
#' on the command line.
#'
#' @param path path to the unencrypted file
#' @param password password required to open the file
#' @param out path to write the encrypted file to; defaults to overwriting
#'   `path`
#'
#' @returns `out`, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' lrh_excel(df, file = "tmp/report.xlsx")
#' xlsx_encrypt("tmp/report.xlsx", "s3cret")
#' }
xlsx_encrypt <- function(path, password, out = path) {
  if (!file.exists(path)) {
    cli::cli_abort("File {.file {path}} does not exist.")
  }
  if (Sys.which("uvx") == "") {
    cli::cli_abort(c(
      "{.code uvx} not found on the PATH.",
      "i" = "Install uv, or {.code pip install msoffcrypto-tool} and put it on the PATH."
    ))
  }

  # Encrypt to a temp file first so a failure never clobbers the original
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  on.exit(unlink(tmp), add = TRUE)

  res <- suppressWarnings(system2(
    "uvx",
    c("msoffcrypto-tool", "-e", "-p", shQuote(password), shQuote(path), shQuote(tmp)),
    stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(res, "status")) || !file.exists(tmp)) {
    cli::cli_abort(c("Encryption failed:", paste(res, collapse = "\n")))
  }

  if (!file.copy(tmp, out, overwrite = TRUE)) {
    cli::cli_abort("Could not write encrypted file to {.file {out}}.")
  }
  invisible(out)
}
