#' Send an email from the Quality Services mailbox
#'
#' Wraps the Microsoft365R + blastula boilerplate used across the production
#' scripts: composes a markdown email, attaches any files, switches the sender
#' to the shared Quality Services mailbox, and sends.
#'
#' @param body A markdown string for the email body
#' @param subject Subject line
#' @param to Character vector of recipient addresses
#' @param cc Optional character vector of cc addresses
#' @param attachments Optional character vector of file paths to attach
#' @param from Sending address. Defaults to the shared Quality Services
#'   mailbox. Use `NULL` to send from your own account.
#' @param reply_to Optional reply-to address (the report emailers set this to
#'   the shared mailbox so replies skip personal inboxes)
#' @param send Should the email be sent? If `FALSE`, the email is left as a
#'   draft (useful for reviewing before a first live run).
#'
#' @returns The `ms_outlook_email` object, invisibly
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' lrh_email(
#'   body = str_glue("# Falls Tracking Updates\n[Falls Tracker]({link})"),
#'   subject = str_c(format(today(), "%B %e"), " Falls Tracking Updates"),
#'   to = fa_notify_emails
#' )
#' }
lrh_email <- function(body, subject, to, cc = NULL, attachments = NULL,
                      from = "QualityServiceDept@lrhcares.org",
                      reply_to = NULL, send = TRUE) {

  ol <- suppressMessages(Microsoft365R::get_business_outlook())

  em_bl <- blastula::compose_email(body = blastula::md(body))

  for (a in attachments) {
    if (!file.exists(a)) {
      cli::cli_abort("Attachment {.file {a}} does not exist")
    }
    em_bl <- blastula::add_attachment(em_bl, a)
  }

  args <- list(em_bl, subject = subject, to = to)
  if (!is.null(cc)) args$cc <- cc
  if (!is.null(reply_to)) args$reply_to <- reply_to
  em <- do.call(ol$create_email, args)

  if (!is.null(from)) {
    em$update(from = list(emailAddress = list(address = from)))
  }

  if (send) em$send()

  invisible(em)
}
