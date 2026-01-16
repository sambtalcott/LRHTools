# Package startup hooks

.onAttach <- function(libname, pkgname) {
  # Silently maintain Azure tokens on package load
  # This removes stale tokens (>6 days old) and refreshes valid ones
  # to prevent authentication errors from expired refresh tokens
  tryCatch(
    maintain_azure_tokens(max_age_days = 6, verbose = FALSE),
    error = function(e) {
      # Silently ignore errors during startup
      # (e.g., if AzureAuth is not fully available)
    }
  )

  register_lrh_fonts()
  cli::cli_alert_success("Registered theme_lrh() fonts")
}
