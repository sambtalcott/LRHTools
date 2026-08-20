#' Check if a local folder exists, and if not prompt for it.
#'
#' @param path the path to check
#'
#' @returns the current OR correct path
#' @export
check_folder <- function(path) {
  if (!dir.exists(path)) {
    cli::cli_alert_info("Could not find folder {.val {path}}. Choose the correct folder to continue.")
    path <- rstudioapi::selectDirectory(caption = "Select folder")
    cli::cli_alert_info("Using folder {.val {path}}")
  }
  return(path)
}

#' Normalize names for similarity comparison
#'
#' Strips common credential abbreviations / punctuation, lowercases, then
#' splits on spaces and re-joins the parts in sorted order so first/last
#' name order doesn't affect comparisons.
#'
#' @param x character vector
#' @returns character vector of normalized names
#' @noRd
normalize_names <- function(x) {
  # Remove credentials before lowercasing — regex is case-sensitive and includes
  # mixed-case tokens like "EdD"
  x <- stringr::str_remove_all(x, "(\\b(MD|DO|APRN|PA|LICSW|EdD|MA|CCMA|RN|LPN|CRNA|LNA)\\b)|,|-|'")
  x <- stringr::str_to_lower(x)
  parts <- strsplit(x, " ", fixed = TRUE)
  vapply(parts, \(p) paste0(sort(p), collapse = ""), character(1))
}

#' Name similarity
#'
#' Get the similarity of two vectors of names after removing common abbreviations
#' and auto-sorting first vs last names
#'
#' @param a vector 1
#' @param b vector 2
#'
#' @returns a vector of similarities
#' @export
name_sim <- function(a, b) {
  stringdist::stringsim(normalize_names(a), normalize_names(b))
}

#' Check names against current aliases
#'
#' @param names character vector of names to check
#' @param table table to check names against
#' @param sensitivity How similar do names need to be to trigger an audit? Set
#' to 0 to always audit.
#' @param print How many rows of the table should be printed? Set to 0 or FALSE to hide
#'
#' @returns the name check data frame, invisibly
#' @export
alias_check <- function(names = character(0), table = "PG_PROVIDER_ALIAS", sensitivity = 0.7,
                        print = 10) {
  alias <- pull_duckdb(table)

  # Validate
  old_match <- intersect(names, alias$name_old)
  if (length(old_match) > 0) {
    cli::cli_abort(c(
      "x" = "{.var names} contains values in the {.var name_old} column of {.val {table}}",
      "i" = "Check that names are being properly recategorized before this step."
    ))
  }

  if (any(alias$name_old %in% alias$name_new)) {
    print(dplyr::filter(alias, name_old %in% name_new | name_new %in% name_old))
    cli::cli_abort(c(
      "x" = "Table {.val {table}} contains the same value(s) in {.var name_old} and {.var name_new}"
    ))
  }

  # Generate similarity df
  new_names <- setdiff(names, alias$name_new)

  # Normalize each unique name once, then look up per pair instead of
  # re-normalizing for every grid row
  all_names <- unique(c(alias$name_new, new_names))
  norm_lookup <- normalize_names(all_names)
  names(norm_lookup) <- all_names
  pair_sim <- function(a, b) stringdist::stringsim(norm_lookup[a], norm_lookup[b])

  a_a <- name_grid(alias$name_new) |>
    dplyr::mutate(sim = pair_sim(a, b),
                  type = "Alias - Alias Check (Fix Manually)")

  n_a <- name_grid(new_names, alias$name_new) |>
    dplyr::mutate(sim = pair_sim(a, b),
                  type = "Name - Alias Check (ALWAYS choose b)")

  n_n <- name_grid(new_names) |>
    dplyr::mutate(sim = pair_sim(a, b),
                  type = "Name - Name Check (Choose a or b)")

  final <- dplyr::bind_rows(a_a, n_a, n_n) |>
    dplyr::arrange(dplyr::desc(sim))

  if (dplyr::filter(final, sim >= sensitivity) |> nrow() > 0) {
    # Open in excel. Type "a" or "b" to process into keeping a name and save
    file <- final |> dplyr::mutate(keep = NA) |> lrh_excel()

    cli::cli_inform(c("Name Audit Triggered. Check and update if needed",
                      i = "Update the file with {.val a} or {.val b} to decide which to use.",
                      i = "Save, close and then press enter to update the {.val PG_PROVIDER_ALIAS} table",
                      i = "Re-run the script when finished to re-sync aliases"))
    readline()

    y <- openxlsx2::read_xlsx(file)
    y2 <- y |>
      dplyr::filter(!is.na(keep)) |>
      dplyr::transmute(name_old = dplyr::recode_values(keep, "a" ~ b, "b" ~ a),
                       name_new = dplyr::recode_values(keep, "a" ~ a, "b" ~ b))

    append_duckdb(y2, table)
    cli::cli_abort(c("v" = "{.val {table}} updated. Rerun the script that triggered this"))
  }

  if (print > 0) {
    print(final, n = print)
  }

  invisible(final)
}

#' Create a name grid
#'
#' @param a x names
#' @param b y names
#'
#' @returns a tibble
#' @export
name_grid <- function(a, b = a) {
  expand.grid(a = sort(unique(a)), b = sort(unique(b)), stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    # Remove A-B B-A duplicates
    dplyr::mutate(key = stringr::str_c(pmin(a, b), pmax(a, b))) |>
    dplyr::distinct(key, .keep_all = TRUE) |>
    dplyr::select(-key) |>
    # Remove a == b
    dplyr::filter(a != b)
}



#' Compare two data-frames and highlight mismatches
#'
#' Compares two data-frames that ought to be identical and reports, in order:
#' column differences (present in only one frame, or differing class),
#' duplicate `id_cols`, rows present in only one frame, and value mismatches
#' for the rows and columns the two frames share.
#'
#' Lazy tables (e.g. dbplyr) are `collect()`ed up front, both so the row and
#' value checks run locally and to avoid the database default of
#' `na_matches = "never"`, which would flag every row holding a `NULL` in an
#' id column.
#'
#' @details
#' Values are compared as text (via [as.character()]), so there is no numeric
#' `tolerance` as in [all.equal()]: doubles agreeing to ~15 significant digits
#' compare equal, and date-times are compared as printed, which makes the
#' comparison sensitive to each column's `tzone` attribute.
#'
#' @param a data frame 1
#' @param b data frame 2
#' @param id_cols uniquely identifying columns
#' @param n number of rows to print for each section of the report
#'
#' @returns invisibly, a list of tibbles: `columns` (column-level differences),
#'   `rows` (rows found in only one frame), `summary` (mismatch count per
#'   column) and `values` (the value mismatches themselves)
#' @export
lrh_compare <- function(a, b, id_cols, n = 20) {

  cli::cli_progress_message("Checking columns")

  # Lazy tables must be local: the row/value checks are local operations, and
  # database joins default to na_matches = "never"
  a2 <- dplyr::collect(a)
  b2 <- dplyr::collect(b)

  # Check id_cols are usable before any join can fail cryptically
  missing_a <- setdiff(id_cols, names(a2))
  missing_b <- setdiff(id_cols, names(b2))
  if (length(missing_a) > 0 || length(missing_b) > 0) {
    cli::cli_abort(c(
      "{.var id_cols} must be present in both data frames.",
      x = if (length(missing_a) > 0) "Missing from {.var a}: {.val {missing_a}}",
      x = if (length(missing_b) > 0) "Missing from {.var b}: {.val {missing_b}}"
    ))
  }

  # Values are reshaped with a "^" separator, so it can't appear in a name
  bad_sep <- grep("\\^", union(names(a2), names(b2)), value = TRUE)
  if (length(bad_sep) > 0) {
    cli::cli_abort(c(
      "Column names cannot contain {.val ^}.",
      x = "Found in {.val {bad_sep}}"
    ))
  }

  # Standardize column order
  a2 <- dplyr::relocate(a2, sort(colnames(a2)))
  b2 <- dplyr::relocate(b2, sort(colnames(b2)))

  # Check columns match
  col_class <- function(x) {
    tibble::tibble(column = names(x),
                   class = purrr::map_chr(x, ~stringr::str_flatten_comma(class(.x))))
  }

  col_mismatches <- dplyr::full_join(col_class(a2), col_class(b2),
                                     by = "column", suffix = c(".a", ".b")) |>
    dplyr::mutate(status = dplyr::case_when(is.na(.data$class.b) ~ "only in a",
                                            is.na(.data$class.a) ~ "only in b",
                                            .data$class.a != .data$class.b ~ "class differs",
                                            .default = "match")) |>
    dplyr::filter(.data$status != "match") |>
    dplyr::relocate("status", .after = "column")

  if (nrow(col_mismatches) > 0) {
    cli::cli_alert_danger("Columns in {.var a} and {.var b} don't match: {nrow(col_mismatches)} column{?s}")
    print(col_mismatches, n = n)
  } else {
    cli::cli_alert_success("Columns match")
  }

  cli::cli_progress_message("Checking uniqueness of {.var id_cols}")

  # Check uniqueness by id_cols
  dups <- function(x) {
    x |>
      dplyr::add_count(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::filter(.data$n > 1) |>
      dplyr::relocate(dplyr::all_of(id_cols))
  }
  a_dup <- dups(a2)
  b_dup <- dups(b2)

  if (nrow(a_dup) > 0) {
    cli::cli_alert_danger("{.var a} is not unique by {.var {id_cols}}: {nrow(a_dup)} row{?s}")
    print(a_dup, n = n)
  }
  if (nrow(b_dup) > 0) {
    cli::cli_alert_danger("{.var b} is not unique by {.var {id_cols}}: {nrow(b_dup)} row{?s}")
    print(b_dup, n = n)
  }
  unique_ok <- nrow(a_dup) == 0 && nrow(b_dup) == 0
  if (unique_ok) {
    cli::cli_alert_success("Both dataframes are unique by {.var {id_cols}}")
  }

  cli::cli_progress_message("Checking rows")

  # Rows only in one frame - reported here so they don't masquerade as a value
  # mismatch in every single column below
  only_ids <- function(x, y) {
    dplyr::anti_join(x, y, by = id_cols) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(id_cols)))
  }
  only_a <- only_ids(a2, b2)
  only_b <- only_ids(b2, a2)

  row_mismatch <- dplyr::bind_rows(
    dplyr::mutate(only_a, df = "a"),
    dplyr::mutate(only_b, df = "b")
  ) |>
    dplyr::relocate("df")

  if (nrow(row_mismatch) > 0) {
    cli::cli_alert_danger(paste("Rows don't match: {nrow(a2)} row{?s} in {.var a},",
                                "{nrow(b2)} in {.var b};",
                                "{nrow(only_a)} only in {.var a}, {nrow(only_b)} only in {.var b}"))
    print(row_mismatch, n = n)
  } else {
    cli::cli_alert_success("All {nrow(a2)} row{?s} match by {.var {id_cols}}")
  }

  cli::cli_progress_message("Checking values")

  # Check that values match, for the rows and columns both frames share
  shared_cols <- setdiff(intersect(names(a2), names(b2)), id_cols)

  if (!unique_ok) {
    # Values can't be lined up row-for-row without a unique key
    cli::cli_alert_warning("Skipping value check: not unique by {.var {id_cols}}")
    value_mismatch <- tibble::tibble()
    value_summary <- tibble::tibble()
  } else {
    value_mismatch <- dplyr::inner_join(
      dplyr::select(a2, dplyr::all_of(c(id_cols, shared_cols))),
      dplyr::select(b2, dplyr::all_of(c(id_cols, shared_cols))),
      by = id_cols, suffix = c("^a", "^b")
    ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(-dplyr::all_of(id_cols), names_to = c("column", "df"), names_sep = "\\^") |>
      tidyr::pivot_wider(names_from = "df") |>
      dplyr::filter(.data$a != .data$b | is.na(.data$a) != is.na(.data$b))

    value_summary <- value_mismatch |>
      dplyr::count(.data$column, name = "mismatches") |>
      dplyr::arrange(dplyr::desc(.data$mismatches))

    if (nrow(value_mismatch) > 0) {
      cli::cli_alert_danger("Values in {.var a} and {.var b} don't match: {nrow(value_mismatch)} value{?s} in {nrow(value_summary)} column{?s}")
      print(value_summary, n = n)
      print(value_mismatch, n = n)
    } else if (length(shared_cols) == 0) {
      cli::cli_alert_info("No shared columns to compare values in")
    } else {
      cli::cli_alert_success("Values match")
    }
  }

  cli::cli_progress_done()

  invisible(list(columns = col_mismatches,
                 rows = row_mismatch,
                 summary = value_summary,
                 values = value_mismatch))
}
