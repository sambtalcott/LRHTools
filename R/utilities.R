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

#' Check names for similarities
#'
#' @param names vector of names to check
#'
#' @returns prints the top similarities and silently returns the grid
#' @export
name_check <- function(names) {
  expand.grid(a = sort(unique(names)), b = sort(unique(names)), stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    # Remove A-B B-A duplicates
    dplyr::mutate(key = stringr::str_c(pmin(a, b), pmax(a, b))) |>
    dplyr::distinct(key, .keep_all = TRUE) |>
    dplyr::select(-key) |>
    # Remove a == b
    dplyr::filter(a != b) |>
    # Sort by similarity of split names
    dplyr::mutate(sim = name_sim(a, b)) |>
    dplyr::arrange(desc(sim)) |>
    print()
}
