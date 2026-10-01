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
#' @param exclude_table table of name pairs (`name_a`, `name_b`) confirmed to be
#'   different people. These pairs are never audited. Pairs marked `x` in the
#'   audit file are added to it (the table is created on first use).
#'
#' @returns the name check data frame (excluded pairs removed), invisibly
#' @export
alias_check <- function(names = character(0), table = "PG_PROVIDER_ALIAS", sensitivity = 0.7,
                        print = 10, exclude_table = paste0(table, "_EXCLUDE")) {
  alias <- pull_duckdb(table)

  exclude <- if (DBI::dbExistsTable(lrh_con(type = "any"), exclude_table)) {
    pull_duckdb(exclude_table)
  } else {
    tibble::tibble(name_a = character(0), name_b = character(0))
  }

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

  final <- dplyr::bind_rows(
    name_sim_pairs(alias$name_new) |>
      dplyr::mutate(type = "Alias - Alias Check (Fix Manually)"),
    name_sim_pairs(new_names, alias$name_new) |>
      dplyr::mutate(type = "Name - Alias Check (ALWAYS choose b)"),
    name_sim_pairs(new_names) |>
      dplyr::mutate(type = "Name - Name Check (Choose a or b)")
  ) |>
    # Drop known different-people pairs, in either orientation
    dplyr::anti_join(exclude, by = c(a = "name_a", b = "name_b")) |>
    dplyr::anti_join(exclude, by = c(a = "name_b", b = "name_a")) |>
    dplyr::arrange(dplyr::desc(sim))

  audit <- dplyr::filter(final, sim >= sensitivity)

  if (nrow(audit) > 0) {
    # Open in excel. Type "a", "b" or "x" and save.
    # Only pairs at/above sensitivity are written -- the full grid can run to
    # hundreds of thousands of rows, which is slow to write and to review
    file <- audit |> dplyr::mutate(keep = NA) |> lrh_excel()

    cli::cli_inform(c("Name Audit Triggered. Check and update if needed",
                      i = "Update {.var keep} with {.val a} or {.val b} to decide which to use,
                           or {.val x} if they are different people.",
                      i = "Save, close and then press enter to update {.val {table}} / {.val {exclude_table}}",
                      i = "Re-run the script when finished to re-sync aliases"))
    readline()

    y <- openxlsx2::read_xlsx(file) |>
      dplyr::mutate(keep = stringr::str_to_lower(stringr::str_trim(as.character(keep)))) |>
      dplyr::filter(!is.na(keep), keep != "")

    bad_keep <- setdiff(y$keep, c("a", "b", "x"))
    if (length(bad_keep) > 0) {
      cli::cli_abort(c("x" = "{.var keep} must be {.val a}, {.val b} or {.val x}",
                       "i" = "Found {.val {bad_keep}}. Nothing was updated."))
    }

    y_alias <- y |>
      dplyr::filter(keep %in% c("a", "b")) |>
      dplyr::transmute(name_old = dplyr::recode_values(keep, "a" ~ b, "b" ~ a),
                       name_new = dplyr::recode_values(keep, "a" ~ a, "b" ~ b))

    y_exclude <- y |>
      dplyr::filter(keep == "x") |>
      dplyr::transmute(name_a = pmin(a, b), name_b = pmax(a, b))

    if (nrow(y_alias) > 0) append_duckdb(y_alias, table)
    if (nrow(y_exclude) > 0) append_duckdb(y_exclude, exclude_table)
    cli::cli_abort(c(
      "v" = "Added {nrow(y_alias)} alias{?es} to {.val {table}} and
             {nrow(y_exclude)} exclusion{?s} to {.val {exclude_table}}.",
      "i" = "Rerun the script that triggered this"
    ))
  }

  if (print > 0) {
    print(final, n = print)
  }

  invisible(final)
}

#' Pairwise name similarities
#'
#' Same pairs as `name_grid()` -- every a/b combination, or each unordered pair
#' once when `b` is omitted -- but scored with one vectorized
#' `stringsimmatrix()` call over normalized names instead of building and
#' de-duplicating the full string grid.
#'
#' @param a names
#' @param b names to compare against; `NULL` compares `a` with itself
#' @returns a tibble of `a`, `b`, `sim`
#' @noRd
name_sim_pairs <- function(a, b = NULL) {
  a <- sort(unique(a[!is.na(a)]))
  self <- is.null(b)
  b <- if (self) a else sort(unique(b[!is.na(b)]))

  m <- stringdist::stringsimmatrix(normalize_names(a), normalize_names(b))
  # Self comparison: lower triangle only (a > b), matching name_grid()'s kept order
  idx <- if (self) which(lower.tri(m), arr.ind = TRUE) else arrayInd(seq_along(m), dim(m))

  tibble::tibble(a = a[idx[, 1]], b = b[idx[, 2]], sim = m[idx]) |>
    dplyr::filter(a != b)
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
