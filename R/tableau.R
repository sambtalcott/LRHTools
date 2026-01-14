
#' Sign in to Tableau Cloud using Personal Access Token
#'
#' @param server Tableau Cloud server URL (e.g., "https://us-east-1.online.tableau.com")
#' @param site_name The site name (content URL), e.g., "lrh_nh"
#' @param pat_name Personal Access Token name (or set TABLEAU_PAT_NAME env var)
#' @param pat_secret Personal Access Token secret (or set TABLEAU_PAT_SECRET env var)
#' @param api_version Tableau REST API version (default "3.21")
#' @export
#' @return List with site_id, auth_token, and api_base_url
tableau_sign_in <- function(server = "https://us-east-1.online.tableau.com",
                            site_name = "lrh_nh",
                            pat_name = "RStudio",
                            pat_secret = keyring::key_get("TABLEAU_PAT"),
                            api_version = "3.21") {

  api_base <- paste0(server, "/api/", api_version)

  # Build sign-in request body
  body <- list(
    credentials = list(
      personalAccessTokenName = pat_name,
      personalAccessTokenSecret = pat_secret,
      site = list(contentUrl = site_name)
    )
  )

  resp <- httr2::request(paste0(api_base, "/auth/signin")) |>
    httr2::req_body_json(body) |>
    httr2::req_headers("Accept" = "application/json", "Content-Type" = "application/json") |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  list(
    site_id = result$credentials$site$id,
    auth_token = result$credentials$token,
    api_base_url = api_base,
    user_id = result$credentials$user$id
  )
}


#' Get project ID by name
#'
#' @param auth Authentication list from tableau_sign_in()
#' @param project_name Name of the project/folder
#' @export
#' @return Project ID string
tableau_get_project_id <- function(auth, project_name) {
  # Query projects with filter
  url <- paste0(
    auth$api_base_url, "/sites/", auth$site_id, "/projects",
    "?filter=name:eq:", utils::URLencode(project_name, reserved = TRUE)
  )

  resp <- httr2::request(url) |>
    httr2::req_headers(
      "X-Tableau-Auth" = auth$auth_token,
      "Accept" = "application/json"
    ) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  if (length(result$projects$project) == 0) {
    stop(sprintf("Project '%s' not found", project_name))
  }

  result$projects$project[[1]]$id
}



#' Get data source ID by name
#'
#' @param auth Authentication list from tableau_sign_in()
#' @param datasource_name Name of the data source
#' @param project_name Optional project name to filter by (recommended for disambiguation)
#' @export
#' @return Data source ID string
tableau_get_datasource_id <- function(auth, datasource_name, project_name = NULL) {
  # Query datasources with filter by name
  url <- paste0(
    auth$api_base_url, "/sites/", auth$site_id, "/datasources",
    "?filter=name:eq:", utils::URLencode(datasource_name, reserved = TRUE)
  )

  resp <- httr2::request(url) |>
    httr2::req_headers(
      "X-Tableau-Auth" = auth$auth_token,
      "Accept" = "application/json"
    ) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  if (length(result$datasources$datasource) == 0) {
    stop(sprintf("Data source '%s' not found", datasource_name))
  }

  datasources <- result$datasources$datasource

  # If project_name specified, filter to that project

  if (!is.null(project_name)) {
    project_id <- tableau_get_project_id(auth, project_name)
    datasources <- Filter(function(ds) ds$project$id == project_id, datasources)

    if (length(datasources) == 0) {
      stop(sprintf("Data source '%s' not found in project '%s'", datasource_name, project_name))
    }
  }

  if (length(datasources) > 1) {
    warning(sprintf(
      "Multiple data sources named '%s' found. Using the first one. Consider specifying project_name.",
      datasource_name
    ))
  }

  datasources[[1]]$id
}


#' Refresh a data source extract
#'
#' Initiates an extract refresh job for the specified data source on Tableau Cloud.
#'
#' @param auth Authentication list from tableau_sign_in()
#' @param datasource_name Name of the data source to refresh
#' @param project_name Optional project name to filter by (recommended for disambiguation)
#' @param datasource_id Optional data source ID (if known, skips lookup by name)
#' @export
#' @return List with job information (id, mode, type, createdAt)
tableau_refresh_datasource <- function(auth,
                                       datasource_name = NULL,
                                       project_name = NULL,
                                       datasource_id = NULL) {

  # Get datasource ID if not provided

  if (is.null(datasource_id)) {
    if (is.null(datasource_name)) {
      stop("Either datasource_name or datasource_id must be provided")
    }
    datasource_id <- tableau_get_datasource_id(auth, datasource_name, project_name)
  }

  # Build the refresh URL
  url <- paste0(
    auth$api_base_url, "/sites/", auth$site_id,
    "/datasources/", datasource_id, "/refresh"
  )

  # Tableau API requires empty tsRequest XML body
  body_xml <- "<tsRequest />"

  resp <- httr2::request(url) |>
    httr2::req_headers(
      "X-Tableau-Auth" = auth$auth_token,
      "Accept" = "application/json",
      "Content-Type" = "application/xml"
    ) |>
    httr2::req_body_raw(body_xml, type = "application/xml") |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  job_info <- list(
    id = result$job$id,
    mode = result$job$mode,
    type = result$job$type,
    created_at = result$job$createdAt
  )

  cli::cli_alert_success("Extract refresh initiated for data source (Job ID: {job_info$id})")

  invisible(job_info)
}
