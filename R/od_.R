
# Package-specific environment for storing the OneDrive object
.od_env <- new.env(parent = emptyenv())

#' Set default onedrive for od_* functions
#'
#' @param shared Name of shared folder (if setting default to a shared folder)
#' @param od OneDrive object to set as default or use for shared folder
#' @param folder A folder within the provided OneDrive or shared folder to set as the default.
#'
#' @returns the OneDrive object (invisibly)
#' @export
#' @md
od_default <- function(folder = NULL, shared = NULL, od = NULL) {
  if (!is.null(shared)) {
    .od_env$od <- od_get_shared(name = shared, od = od)
  } else if (!is.null(od)) {
    .od_env$od <- od
  } else {
    .od_env$od <- Microsoft365R::get_business_onedrive()
  }

  if (!is.null(folder)) {
    f <- .od_env$od$get_item(folder)
    if (is.null(f$properties$folder)) {
      cli::cli_abort("Item {.val {folder}} is not a folder")
    }
    .od_env$od <- f
  }

  invisible(.od_env$od)
}

#' Get stored OneDrive object. If none is stored, calls od_default() to create
#' it.
#'
#' @returns OneDrive object (or NULL)
#' @export
od <- function() {
  if (is.null(.od_env$od)) {
    od_default()
  }
  .od_env$od
}

#' Get an ms_drive object for a shared folder
#'
#' @param name Name of the shared folder
#' @param od OneDrive object (optional)
#'
#' @returns an ms_drive object for the shared folder
#' @export
#' @md
od_get_shared <- function(name, od = NULL) {
  if (is.null(od)) {
    od <- Microsoft365R::get_business_onedrive()
  }

  sh <- od$do_operation("sharedWithMe", options=list(allowexternal="true"))

  if (!is.null(name)) {
    sh_list <- purrr::keep(sh$value, ~.x$name == name)
  } else {
    sh_list <- purrr::keep(sh$value, ~.x$webUrl == webUrl)
  }

  if (length(sh_list) == 0) {
    cli::cli_abort("Shared folder {.val {name}} not found")
  }

  if (!"folder" %in% names(sh_list[[1]])) {
    cli::cli_abort("Shared item {.val {name}} is not a folder")
  }
  sh_i <- sh_list[[1]]$remoteItem
  Microsoft365R::ms_drive_item$new(od$token, od$tenant, sh_i)
}

#' List OneDrive Items
#'
#' @param folder Path specification
#' @param od OneDrive. If NULL, will use the stored or default onedrive
#' @param pattern Optional pattern to filter on.
#' @param full_names Should full names be shown?
#'
#' @returns tibble of folder contents
#'
#' @export
#' @md
od_list <- function(folder = "", od = NULL, pattern = NULL, full_names = FALSE) {

  if (is.null(od)) od <- od()

  items <- od$list_items(path = folder, full_names = full_names) |>
    tibble::as_tibble()
  if (!is.null(pattern)) {
    items[grepl(pattern, items$name), ]
  } else {
    items
  }

}

#' Get or open a share link to an item on OneDrive
#'
#' Should work for files or folders.
#'
#' @param path The path to get a link to
#' @param od OneDrive. If NULL, will use the stored or default onedrive
#'
#' @returns a character of the link
#' @export
#' @md
od_get_link <- function(path = "", od = NULL) {

  if (is.null(od)) od <- od()

  od$get_item(path)$create_share_link()
}


#' @export
#' @rdname od_get_link
od_open <- function(path = "", od = NULL) {
  shell.exec(od_get_link(path, od))
}

#' Pull extension from a path
#'
#' @param path file path
#' @return The extension as a character vector
get_ext <- function(path) {
  ext <- regmatches(path, regexpr("\\..{1,5}$", path))
  tolower(ext)
}

#' Determine function type by extension and provided type
#' Handles type validation
#'
#' @param ext File extension (from get_ext)
#' @param type User-specified type
#'
#' @return a type ("dataframe" "rds" or "rdata")
process_type <- function(ext, type) {

  # Extensions and associated types
  file_types <- list(
    ".csv" = "dataframe", ".csv2" = "dataframe", ".tsv" = "dataframe",
    ".xlsx" = "xlsx",".xls" = "xlsx", ".rds" = "rds",
    ".txt" = "lines", ".html" = "lines"
  )

  possible_types <- unique(unlist(file_types))

  # Determine function by file type
  if (is.null(type)) {

    type <- do.call(switch, c(ext, file_types, "error"))

    if (type == "error") {
      cli::cli_abort(c(
        "x" = "Cannot determine file type",
        "i" = "Extension {.val {ext}} is not associated with a known read/write function",
        "i" = "Fix the extension, or manually provide a {.var type}. Known types are {.val {possible_types}}",
        "i" = "Known extensions are {.val {names(file_types)}}"
      ))
    }
  } else if (!type %in% file_types) {
    cli::cli_abort(c(
      "x" = "Invalid {.var type} {.val {type}}",
      "i" = "Valid read types are {.val {as.character(unique(file_types))}}"
    ))
  }
  type
}

#' Read/Write from OneDrive
#'
#' @description
#' Based off of [tntpr::sp_read()] and [tntpr::sp_write()].
#'
#' Read or write data to/from a OneDrive Folder. Can be used with default
#' folder/drive set by [od_default()] or with a specified folder/drive.
#'
#' Currently supported file types include: `.csv`, `.csv2`, `.tsv`, `.xls`,
#' `.xlsx`, `.rds`, `.txt`, `.html`
#'
#' These functions will attempt to use the appropriate read/write function based
#' on the file extension, however this can be overridden by specifying type.
#'
#' The `...` parameter is passed on to the appropriate reading or writing
#' function. See the details section for more information on these functions
#' by type.
#'
#' If the folder in `path` does not yet exist, the user will be prompted if they
#' would like to create it.
#'
#' @details
#' # Details
#' For more information on methods (shown as `$__()` below) see documentation
#' on [Microsoft365R::ms_drive].
#' ## Reading Functions
#' *  ".csv", ".csv2", ".tsv" are read using the `$load_dataframe()` method,
#' which uses [`readr::read_delim()`].
#' *  ".rds" is read using the `$load_rds()` method which accepts no additional
#' arguments.
#' *  ".xls" and ".xlsx" are read using [`readxl::read_excel()`] (if installed).
#' The function will download the excel file temporarily, then import it and
#' delete the temporary copy.
#' *  ".txt" and ".html" are read using [`readr::read_lines()`] (if installed).
#' The function will download the file temporarily, then import it and delete
#' the temporary copy.
#'
#' ## Writing Functions
#' *  ".csv", ".csv2", ".tsv" are written using the `$save_dataframe()` method
#' and uses [`readr::write_delim()`]. Delimiter will be assumed by the extension
#' unless provided in a `delim` argument
#' *  ".rds" is written using the `$save_rds()` method, which accepts no
#' additional arguments
#' *  ".xlsx" is written using [`writexl::write_xlsx()`] (if
#' installed) and then uploaded using the `$upload_file()` method.
#'
#' @param x file to save
#' @param path The location in the Sharepoint drive
#' @param od OneDrive (if null, will use the stored OneDrive)
#' @param type Optional. One of "dataframe" (for delimited files), "xlsx", or "rds". Uses the file extension to determine type if not provided.
#' @param ... Additional arguments passed on to the reading/writing function.
#'
#' @seealso [od_upload()], [od_download()]; `$upload_file()`, `$download_file()`, `$save_rdata()`, `$load_rdata()` from [Microsoft365R::ms_drive]
#'
#' @return `od_read()` returns an R object as specified by type. `od_write()`
#' returns x, invisibly
#'
#' @export
#' @md
od_read <- function(path, od = NULL, type = NULL, ...) {

  if (is.null(od)) od <- od()

  ext <- get_ext(path)
  type <- process_type(ext, type)

  # General error handling.
  if (!od_exists(path, od = od)) {
    cli::cli_abort(c(
      "x" = "File {.val {path}} does not exist in the current OneDrive"
    ))
  }

  if (type == "rds") {
    od$load_rds(path = path)
  } else if (type == "dataframe") {
    od$load_dataframe(path = path, ...)
  } else if (type == "xlsx") {
    od_read_xlsx(path, od, ...) # Error catching in this function
  } else if (type == "lines") {
    od_read_lines(path, od, ...)
  }

}

#' Internal function for reading excel files from OneDrive
#'
#' @param path path
#' @param od OneDrive object or folder object
#' @param ... additional arguments from od_read()
#'
#' @return data read by readxl::read_excel()
od_read_xlsx <- function(path, od, ...) {
  if (rlang::is_installed("readxl")) {
    ext <- get_ext(path)
    tf <- tempfile(fileext = ext)

    od_download(src = path, dest = tf, od = od)
    on.exit(file.remove(tf)) # Error-safe cleanup

    readxl::read_excel(tf, ...)
  } else {
    cli::cli_abort(c(
      "x" = "Package `readxl` required to read .xls/.xlsx files",
      "i" = "Run {.code install.packages('readxl')} to install"
    ))
  }
}

#' Internal function for reading text files from OneDrive
#'
#' @param path path
#' @param od OneDrive object or folder object
#' @param ... additional arguments from od_read()
#'
#' @return data read by readr::read_lines()
od_read_lines <- function(path, od, ...) {
  if (rlang::is_installed("readr")) {
    ext <- get_ext(path)
    tf <- tempfile(fileext = ext)

    od_download(src = path, dest = tf, od = od)
    on.exit(file.remove(tf)) # Error-safe cleanup

    readr::read_lines(tf, ...)
  } else {
    cli::cli_abort(c(
      "x" = "Package `readr` required to read text files",
      "i" = "Run {.code install.packages('readr')} to install"
    ))
  }
}

#' @export
#' @rdname od_read
od_write <- function(x, path, od = NULL, ...) {

  if (is.null(od)) od <- od()

  ext <- get_ext(path)
  type <- process_type(ext, type = NULL)

  args <- rlang::list2(...)

  if (type == "rds") {
    od$save_rds(object = x, file = path)
  } else if (type == "dataframe") {
    if (!"delim" %in% names(args)) {
      args$delim <- switch(ext, ".csv" = ",", .csv2 = ";", "\t")
    }
    args <- c(list(df = x, file = path), args)
    rlang::exec(od$save_dataframe, !!!args)
  } else if (type == "xlsx") {
    if (ext != ".xlsx") cli::cli_abort(c(
      "{.code od_write()} can only write Excel documents to {.val .xlsx} files"
    ))
    od_write_xlsx(x, path, od, ...) # Creates folders -- no error catching needed
  }

  invisible(x)
}

#' Internal funtion for writing xlsx files
#'
#' @param x item
#' @param path path
#' @param od onedrive
#' @param ... additional parameters
#'
#' @returns nothing
od_write_xlsx <- function(x, path, od, ...) {
  if (rlang::is_installed("writexl")) {
    tf <- tempfile(fileext = "xlsx")
    writexl::write_xlsx(x = x, path = tf, ...)
    on.exit(file.remove(tf)) # Error-safe cleanup
    suppressMessages( # To block the cli_inform() with the tempfile
      od_upload(src = tf, dest = path, od = od)
    )
  } else {
    cli::cli_abort(c(
      "x" = "Package `writexl` required to write .xlsx files",
      "i" = "Run {.code install.packages('writexl')} to install"
    ))
  }
}

#' OneDrive Upload / Download
#'
#' @param src Location of source file. Either a local path (for `od_upload`), or a Sharepoint path (for `od_download`)
#' @param dest Location of destination file. If not provided, uses the same file name as `src`
#' @param od OneDrive (if null, will use the stored OneDrive)
#' @param overwrite Should the destination file be overwritten if it exists?
#'
#' @returns Returns `dest` invisibly
#' @export
#'
#' @md
od_upload <- function(src, dest = basename(src), od = NULL) {

  if (is.null(od)) od <- od()

  # Check for existing file
  if (!file.exists(src)) {
    cli::cli_abort(c(
      "x" = "Could not find source file {.val {src}}"
    ))
  }

  # Check for existing folder in Sharepoint
  od_check_folder(od, folder_path = dirname(dest))

  od$upload(src = src, dest = dest)

  invisible(dest)
}

#' @export
#' @rdname od_upload
od_download <- function(src, dest = basename(src), od = NULL, overwrite = FALSE) {

  if (is.null(od)) od <- od()

  # Check for existing destination file
  if (file.exists(dest) && !overwrite) {
    cli::cli_abort(c(
      "x" = "Destination path {.val {dest}} exists, and {.var overwrite} is set to {.val FALSE}"
    ))
  }

  # Check for existing source file
  if (!od_exists(src, od = od)) {
    cli::cli_abort(c(
      "x" = "File {.val {path}} does not exist in the current OneDrive"
    ))
  }

  od$get_item(src)$download(dest = dest, overwrite = overwrite)

  invisible(dest)
}

#' Check if a path / item exists in OneDrive.
#'
#' @param path Path to check
#' @param od OneDrive (if null, will use the stored OneDrive)
#'
#' @returns TRUE or FALSE
#' @export
#' @md
od_exists <- function(path, od = NULL) {

  if (is.null(od)) od <- od()

  tryCatch({
    od$get_item(path)
    TRUE
  }, error = \(e) {
    FALSE
  })
}

#' Internal function for checking if a OD folder exists
#'
#' @param od OneDrive (if null, will use the stored OneDrive)
#' @param folder_path Folder to check
#'
#' @returns nothing. Throws an error if it doesn't exist.
od_check_folder <- function(od, folder_path) {

  # If no path is provided, return (dirname() returns "." for root)
  if (folder_path %in% c("", ".", NA)) return()

  tryCatch(
    od$get_item(folder_path),

    # If it doesn't exist, ask if it should be created
    error = \(cnd) {
      cli::cli_inform(c(
        "i" = "Destination folder {.val {folder_path}} does not exist.",
        "Would you like to create this folder?"
      ))
      if (utils::select.list(c("No", "Yes")) == "Yes") {
        od_create_folder(folder_path, od = od)
      } else {
        cli::cli_abort(c("x" = "Folder not created. Script halted"),
                       parent = NA)
      }
    }
  )
}

#' Create OneDrive Folder
#'
#' @param folder_path Path to the new folder
#' @param od OneDrive (if null, will use the stored OneDrive)
#'
#' @return returns `folder_path` invisibly
#' @export
#'
#' @md
od_create_folder <- function(folder_path, od = NULL) {

  if (is.null(od)) od <- od()

  od$create_folder(folder_path)

  invisible(folder_path)
}
