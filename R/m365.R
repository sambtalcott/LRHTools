#' Warm up a Microsoft365R client using device-code auth
#'
#' Pre-authenticates a Microsoft365R resource (Outlook, OneDrive, etc.) using
#' the device-code flow, and patches the underlying Microsoft365R function so
#' every later call in this R session returns the warmed-up client without
#' re-authenticating. Designed for unattended runs (e.g. the daily script)
#' where a browser pop-up cannot complete.
#'
#' When device-code approval is required, the function emits a structured
#' marker line to the log:
#'
#' `! DEVICE_CODE_AUTH_REQUIRED url=https://login.microsoft.com/device code=ABC123`
#'
#' The `/dailyscript` slash command watches for this marker and pushes the URL
#' and code to the user's phone via PushNotification.
#'
#' If a valid token is already cached on disk, no prompt fires.
#'
#' @param fn_name Character. Name of the Microsoft365R function to warm up,
#'   e.g. `"get_business_outlook"` or `"get_business_onedrive"`.
#' @param ... Additional arguments forwarded to the resource function.
#' @returns The Microsoft365R client object, invisibly.
#' @export
#' @md
lrh_m365_login <- function(fn_name, ...) {
  azure_ns <- asNamespace("AzureAuth")
  m365_ns  <- asNamespace("Microsoft365R")

  # Patch AzureAuth's get_device_creds so that when a device-code prompt is
  # needed, we emit a structured marker line *before* AzureTokenDeviceCode's
  # internal cat(creds$message, "\n") runs. We also blank creds$message so the
  # subsequent cat is a no-op and the raw "To sign in..." sentence never hits
  # the log (where it would be confusing alongside our marker).
  orig_get_creds <- get("get_device_creds", envir = azure_ns)
  patched_get_creds <- function(...) {
    creds <- orig_get_creds(...)
    if (!is.null(creds$verification_uri) && !is.null(creds$user_code)) {
      cli::cli_alert_warning(sprintf(
        "DEVICE_CODE_AUTH_REQUIRED url=%s code=%s",
        creds$verification_uri, creds$user_code
      ))
      creds$message <- ""
    }
    creds
  }
  utils::assignInNamespace("get_device_creds", patched_get_creds, "AzureAuth")
  on.exit(
    utils::assignInNamespace("get_device_creds", orig_get_creds, "AzureAuth"),
    add = TRUE
  )

  # Run the actual auth. If a valid cached token exists, this returns instantly
  # with no prompt; otherwise AzureAuth fires the device-code flow above.
  client <- get(fn_name, envir = m365_ns)(auth_type = "device_code", ...)

  # Replace the Microsoft365R function in its namespace so every subsequent
  # call — including ones from LRHTools internals like od_get_shortcut() that
  # we don't directly control here — returns this cached client without
  # re-authenticating. Intentionally NOT reverted on exit; we want this to
  # persist for the rest of the R session.
  utils::assignInNamespace(fn_name, function(...) client, "Microsoft365R")

  invisible(client)
}
