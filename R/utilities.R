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
