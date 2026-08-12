#' Preview Selected SQL
#'
#' RStudio addin that runs [rstudioapi::previewSql()] on the highlighted SQL in
#' the active editor, so multi-statement .sql files can be previewed one query
#' at a time. With no selection, previews the statement under the cursor, where
#' statements are assumed to be separated by lines ending in ";".
#'
#' The connection is taken from the file's `-- !preview conn=...` header if one
#' exists, otherwise [lrh_con()]. Previewing executes the SQL via
#' `DBI::dbGetQuery()`, so previewing a `CREATE ... VIEW` statement on a
#' read-write connection does write the view. That is supported. Preview the
#' SELECT body instead if you want no side effect.
#'
#' Statements that return no rows (`CREATE`, `INSERT`, ...) would render as an
#' empty preview tab, so they are run directly and the tab shows a one-row
#' summary of the statement, its affected-row count where non-zero, and the
#' time it completed.
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

  # A statement that returns no rows (CREATE, INSERT, ...) renders as an empty
  # preview tab. Run it here instead and preview a one-row summary, so the tab
  # confirms what happened. dbExecute() hands back the affected-row count, so
  # this costs no extra round trip.
  if (!sql_returns_rows(sql)) {
    n <- DBI::dbExecute(con, sql)
    rstudioapi::previewSql(conn = con, statement = sql_status_select(sql, n))
    return(invisible(sql))
  }

  # previewSql() executes the statement via dbGetQuery(). Anything routed here
  # returns rows, but if the classifier above is ever wrong duckdb warns that a
  # non-SELECT result has nothing to fetch. The warning is deferred to the next
  # top-level call, so it surfaces detached from its cause; muffle just that one.
  withCallingHandlers(
    rstudioapi::previewSql(conn = con, statement = sql),
    warning = function(w) {
      if (grepl("do not come from SELECT", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  invisible(sql)
}

# Leading keyword of a statement, ignoring comment lines, blank lines and any
# opening parenthesis.
sql_first_keyword <- function(sql) {
  lines <- strsplit(sql, "\n")[[1]]
  lines <- lines[!grepl("^\\s*(--.*)?$", lines)]
  if (length(lines) == 0) return("")
  tolower(sub("^[^[:alnum:]]*([[:alnum:]_]+).*$", "\\1", lines[1]))
}

# Whether a statement produces a result set worth rendering. Deliberately a
# keyword check rather than a parse: the statement has already been bound by
# EXPLAIN, and a wrong guess only costs an empty tab.
sql_returns_rows <- function(sql) {
  sql_first_keyword(sql) %in% c(
    "select", "with", "from", "table", "values", "describe", "desc",
    "summarize", "show", "pragma", "explain", "call", "pivot", "unpivot"
  )
}

# A one-row SELECT describing a statement that returned no rows, for display in
# the preview tab.
sql_status_select <- function(sql, n = 0L) {
  lines <- strsplit(sql, "\n")[[1]]
  lines <- lines[!grepl("^\\s*(--.*)?$", lines)]
  label <- if (length(lines) == 0) "statement" else trimws(lines[1])
  if (nchar(label) > 80) label <- paste0(substr(label, 1, 77), "...")
  quote_lit <- function(x) paste0("'", gsub("'", "''", x), "'")

  parts <- c(
    paste(quote_lit(label), "as statement"),
    if (isTRUE(n > 0)) paste(format(n, scientific = FALSE), "as rows_affected"),
    paste(quote_lit(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "as completed")
  )
  paste("select", paste(parts, collapse = ", "))
}
