# Internal function for turning on dev mode
# Modules that use the dev_mode setting:
# matchResultOutput - intermediate results shown only for dev_mode
#
dev_mode <- function(setting = NULL) {
  if(is.null(setting)) getOption("dev_mode", default = FALSE) else { options(dev_mode = setting) }
}
