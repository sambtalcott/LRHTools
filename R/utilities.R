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
  tibble::tibble(a = a, b = b) |>
    dplyr::mutate_all(~stringr::str_to_lower(stringr::str_remove_all(.x, "MD|DO|APRN|PA|LICSW|EdD|,|-|'"))) |>
    dplyr::mutate_all(~purrr::map_chr(.x, \(v) stringr::str_flatten(sort(str_split_1(v, " "))))) |>
    dplyr::mutate(sim = stringdist::stringsim(a, b)) |>
    dplyr::pull(sim)
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

  a_a <- name_grid(alias$name_new) |>
    dplyr::mutate(sim = name_sim(a, b),
                  type = "Alias - Alias Check (Fix Manually)")

  n_a <- name_grid(new_names, alias$name_new) |>
    dplyr::mutate(sim = name_sim(a, b),
                  type = "Name - Alias Check (ALWAYS choose b)")

  n_n <- name_grid(new_names) |>
    dplyr::mutate(sim = name_sim(a, b),
                  type = "Name - Name Check (Choose a or b)")

  final <- dplyr::bind_rows(a_a, n_a, n_n) |>
    dplyr::arrange(desc(sim))

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
    cli::cli_abort("{.val {table}} updated. Rerun the script that triggered this")
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
