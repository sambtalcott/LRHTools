# Package-specific environment for tableau credentials
.tab_env <- new.env(parent = emptyenv())

#' Authenticate in Tableau
#'
#' Authenticates in tableau and stores the credentials token for use with other
#' `tableau_*` functions. By default, credentials tokens are valid for 120 minutes
#' on Tableau Cloud -- if using for longer you will need to reauthorize.
#'
#' Defaults are currently set for LRH, but can be adjusted
#' for any site. For more details see [Tableau's REST Documentation](https://help.tableau.com/current/api/rest_api/en-us/REST/rest_api_ref_authentication.htm#sign_in)
#'
#' @param base_url Tableau Base URL
#' @param site_content_url Site name
#' @param pat_name PAT Name
#' @param pat_secret PAT
#'
#' @md
#' @returns nothing
#' @export
tableau_auth <- function(base_url = "https://us-east-1.online.tableau.com/api/3.15",
                         site_content_url = "lrh_nh",
                         pat_name = "RStudio",
                         pat_secret = tntpr::tntp_cred("TABLEAU_PAT")) {

  base_req <- httr2::request(base_url) |>
    httr2::req_headers("Accept" = "application/json")

  withCallingHandlers(
    auth_content <- base_req |>
      httr2::req_url_path_append("auth", "signin") |>
      httr2::req_body_json(list(credentials = list(
        personalAccessTokenName = pat_name,
        personalAccessTokenSecret = pat_secret,
        site = list(contentUrl = site_content_url)
      ))) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    httr2_http_401 = function(cnd) {
      cli::cli_abort(c(
        "x" = "Tableau Authentication Error:",
        "Attempted to authenticate with PAT name {.val {pat_name}} and value {.val {paste0(substr(pat_secret, 1, 5), '...')}}",
        "Check that the provided PAT is correct and still active."
      ))
    }
  )

  .tab_env$token <- auth_content$credentials$token
  .tab_env$site_id <- auth_content$credentials$site$id
  .tab_env$base_req <- base_req |>
    httr2::req_url_path_append("sites", .tab_env$site_id) |>
    httr2::req_auth_bearer_token(.tab_env$token)
}

#' Check for a stored authentication (internal)
#'
#' @returns nothing
tableau_has_auth <- function() {
  if (is.null(.tab_env$base_req)) {
    cli::cli_abort(c(
      "x" = "No stored Tableau authentication",
      "Authenticate first using `tableau_auth()`"
    ))
  }
}

#' List Tableau Data sources
#'
#' @returns a tibble with details on all current Tableau data sources
#' @export
tableau_list_data <- function() {
  tableau_has_auth()
  a <- .tab_env$base_req |>
    httr2::req_url_path_append("datasources") |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  a$datasources$datasource |>
    tibble::as_tibble()
}

# Could potentially add additional functions if wanted
