#' Modal Error Handling
#' 
#' Brings up a generic modal for application errors
meh <- function(msg = NULL) {
  showModal(modalDialog(
    "Sorry, something went wrong. Try again later or let us know what you were trying to do."
  ))
}