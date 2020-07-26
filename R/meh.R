#' Modal Error Handling
#'
#' Brings up a generic modal for application errors
meh <- function(msg = NULL, error = NULL) {
  showModal(modalDialog(
    HTML("Sorry, something went wrong.<br>Try again later or let us know what you were trying to do.<br>"),
    if(dev_mode()) paste("Error:", error) else ""
    )
  )
}
