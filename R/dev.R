# interntal function for dev mode development
dev_mode <- function(setting = NULL) {
  if(is.null(setting)) getOption("dev_mode", default = FALSE) else { options(dev_mode = setting) }
}