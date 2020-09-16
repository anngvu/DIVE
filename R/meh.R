#' Modal Error Handling
#'
#' Bring up a modal dialog for application errors
#'
#' Internally, errors in module code summon a dialog when execution fails or
#' when something is not allowed, etc.
#' Indeed, this defaults to a generic "something went wrong" message,
#' but the caller can provide more specific verbiage.
#' When the \code{dev_mode} option is set, the original error will also be displayed.
#' The footer can optionally contain a link to where the user can file issues,
#' set globally for all error modals with \code{base::options(DIVE.error.modal.footer = ui)},
#' or the caller can pass in custom ui.
#'
#' @param msg User-facing message that can contain HTML for formatting.
#' @param error Forwarded error messsage to be shown depending on \code{dev_mode} option.
#' @param footer Bottom part of the modal dialog, which by default contains nothing unless
#' set with \code{base::options(DIVE.error.modal.footer = ui)}. See details for intended uses.
meh <- function(msg = "Sorry, something went wrong.<br><br>Try again later or let us know about the issue.<br>",
                error = NULL,
                footer = getOption("DIVE.error.modal.footer")) {
  showModal(modalDialog(
    shiny::HTML(msg),
    if(dev_mode()) paste("Error:", error) else "",
    footer = footer,
    easyClose = TRUE,
    fade = FALSE
  ))
}

#' Internal function for getting and setting dev mode
#'
#' Control the display of error messages in \code{\link{meh}} and of certain other UI
#'
#' Aside from \code{\link{meh}}, some modules use this setting:
#' \itemize{
#'   \item \code{\link{matchResultServer}}: intermediate results are only downloadable when ON
#'}
#'
#' @param setting TRUE or FALSE, passed to \code{base::options} \code{DIVE.dev.mode}.
dev_mode <- function(setting = NULL) {
  if(is.null(setting)) getOption("DIVE.dev.mode", default = FALSE) else { options(DIVE.dev.mode = setting) }
}
