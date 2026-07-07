#' HTML fragments for notification emails
#'
#' Helpers for building the per-record HTML tables used in tracker
#' notification emails (Falls, Restraints, Pressure Ulcers, ...).
#'
#' `html_highlight()` bolds and colors values that need attention (by default
#' `"--NOT DOCUMENTED--"`). `html_highlight_change()` displays `"old -> new"`
#' when a value changed, highlighting either side as needed.
#' `html_table_row()` builds a single `<tr>` key-value row, and
#' `html_kv_table()` builds a full bordered key-value table from named
#' arguments. All functions are vectorized, so they can be used directly
#' inside `mutate()`/`transmute()` to build one block per record.
#'
#' @param x,old,new Character vectors of display values
#' @param values Values to highlight. Defaults to `"--NOT DOCUMENTED--"`
#' @param color Highlight color. Defaults to `"red"`
#' @param name Row label (bolded in the left cell)
#' @param value Row value (right cell)
#' @param ... For `html_kv_table()`: named character vectors, one per row of
#'   the table, e.g. `html_kv_table("Unit" = unit, "DOB" = dob)`
#'
#' @returns A character vector of HTML fragments
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' new_rows |>
#'   mutate(across(where(is.character), ~html_highlight(replace_na(.x, "--NOT DOCUMENTED--")))) |>
#'   transmute(text = str_c(
#'     "### ", FIN, "  (", person_mrn, ")\n",
#'     html_kv_table("Unit" = enc_loc_nurse_unit, "DOB" = dob, "Injury" = injury)
#'   ))
#' }
html_highlight <- function(x, values = "--NOT DOCUMENTED--", color = "red") {
  dplyr::if_else(
    x %in% values,
    stringr::str_c("<span style='color:", color, ";'><b>", x, "</b></span>"),
    x
  )
}

#' @export
#' @rdname html_highlight
html_highlight_change <- function(old, new, values = "--NOT DOCUMENTED--",
                                  color = "red") {
  dplyr::if_else(
    old == new,
    html_highlight(new, values, color),
    stringr::str_c(html_highlight(old, values, color), "&emsp;\u27f6 &emsp;",
                   html_highlight(new, values, color))
  )
}

#' @export
#' @rdname html_highlight
html_table_row <- function(name, value) {
  stringr::str_c("<tr><td><b>", name, "</b>:</td><td>", value, "</td></tr>\n")
}

#' @export
#' @rdname html_highlight
html_kv_table <- function(...) {
  cols <- rlang::list2(...)

  if (length(cols) == 0 || is.null(names(cols)) || any(names(cols) == "")) {
    cli::cli_abort("All arguments to {.fn html_kv_table} must be named")
  }

  rows <- purrr::imap(cols, \(v, nm) html_table_row(nm, v))
  body <- purrr::reduce(rows, stringr::str_c)

  stringr::str_c(
    "<table border = '1', cellpadding = '5', ",
    "style='border-collapse: collapse; margin-left: 20px;'>\n",
    body,
    "</table>"
  )
}
