#' Modal Error Handling
#'
#' Brings up a generic modal for application errors
#' Shows error if dev_mode
meh <- function(msg = NULL, error = NULL) {
  showModal(modalDialog(
    HTML("Sorry, something went wrong.<br><br>Try again later or let us know about the issue.<br>"),
    if(dev_mode()) paste("Error:", error) else ""
    )
  )
}

# Internal function for turning on dev mode
# Modules that currently use the dev_mode setting:
# matchResult - intermediate results shown only for dev_mode
#
dev_mode <- function(setting = NULL) {
  if(is.null(setting)) getOption("dev_mode", default = FALSE) else { options(dev_mode = setting) }
}
