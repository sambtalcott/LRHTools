#' Preview Selected SQL
#'
#' RStudio addin that runs [rstudioapi::previewSql()] on the highlighted SQL in
#' the active editor, so multi-statement .sql files can be previewed one query
#' at a time. With no selection, previews the statement under the cursor, where
#' statements are assumed to be separated by lines ending in ";".
#'
#' The connection is taken from the file's `-- !preview conn=...` header if one
#' exists, otherwise [lrh_con()]. Note that previewing runs the SQL via
#' `DBI::dbGetQuery()`, so select the SELECT body of a CREATE VIEW statement
#' rather than the whole DDL.
#'
#' Bind to a keyboard shortcut via Tools > Modify Keyboard Shortcuts and
#' searching for "Preview SQL Selection".
#'
#' RStudio's preview pane silently shows nothing when a statement fails, so the
#' statement is first validated with `EXPLAIN` (which parses and binds the query
#' without running it) and any database error is printed to the console.
#'
#' @param validate Whether to `EXPLAIN` the statement first to surface database
#'   errors. Set `FALSE` for a backend where `EXPLAIN` is unsupported.
#'
#' @returns Invisibly, the SQL string sent to the preview pane
#' @export
preview_sql_selection <- function(validate = TRUE) {
  ctx <- rstudioapi::getSourceEditorContext()
  if (is.null(ctx)) cli::cli_abort("No active source editor.")

  sel <- rstudioapi::primary_selection(ctx)
  sql <- sel$text

  # No selection: take the statement under the cursor. Statement boundaries
  # are lines whose last non-whitespace character is a semicolon.
  if (!nzchar(trimws(sql))) {
    row <- sel$range$start[["row"]]
    ends <- grep(";\\s*$", ctx$contents)
    prev_ends <- ends[ends < row]
    start_row <- if (length(prev_ends) == 0) 1L else max(prev_ends) + 1L
    next_ends <- ends[ends >= row]
    end_row <- if (length(next_ends) == 0) length(ctx$contents) else min(next_ends)
    sql <- paste(ctx$contents[start_row:end_row], collapse = "\n")
  }

  # Drop any !preview header lines that got swept into the statement
  sql <- paste(
    grep("^\\s*--\\s*!preview", strsplit(sql, "\n")[[1]], value = TRUE, invert = TRUE),
    collapse = "\n"
  )
  if (!nzchar(trimws(sql))) cli::cli_abort("Nothing to preview.")

  # Connection: the file's !preview header if present, otherwise lrh_con()
  hdr <- grep("^\\s*--\\s*!preview\\s+conn\\s*=", ctx$contents, value = TRUE)
  if (length(hdr) > 0) {
    conn_expr <- sub("^\\s*--\\s*!preview\\s+conn\\s*=\\s*", "", hdr[1])
    con <- eval(parse(text = conn_expr), envir = globalenv())
  } else {
    con <- lrh_con()
  }

  # previewSql() shows an empty pane and no message when the statement fails,
  # so bind-check it here and report the database error ourselves. Reported as
  # a message rather than an error: RStudio throws a modal dialog on top of the
  # console output whenever an addin signals an error condition.
  if (isTRUE(validate)) {
    err <- tryCatch(
      {
        DBI::dbGetQuery(con, paste0("EXPLAIN ", sub(";\\s*$", "", trimws(sql))))
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(err)) {
      cli::cli_alert_danger("SQL error - statement was not previewed.")
      cli::cli_verbatim(err)
      return(invisible(sql))
    }
  }

  rstudioapi::previewSql(conn = con, statement = sql)
  invisible(sql)
}
