#' Connected Sort UI
#'
#' Create a CONnected SORT element whose items are sortable and transferable between other elements initiated by this method.
#'
#' This is based on the package function \code{shinyjqui::orderInput}, where the R input builder is simpler
#' and the custom Shiny input bindings are implemented differently.
#'
#' @param inputId
#' @param label Element label.
#' @param items Optional, character vector of items.
#' @param item_class Optional, additional class for items in the sortable ui element.
#' @param classes Optional, additional classes for the sortable ui element.
#' Note: including the "one-to-one" class will create a "monogamous" consort element that will only take one item.
#' @param placeholder Placeholder text.
#' @param width
#' @result A sortable ui element.
#' @export
consortUI <- function(inputId, label, items = NULL,
                      item_class = NULL, classes = NULL, placeholder = "n/a", width = "300px", ...) {

  dep <- list(
    htmltools::htmlDependency(
    name       = "jqueryui",
    version    = "1.12.1",
    package    = "shiny",
    src        = "www/shared/jqueryui",
    script     = "jquery-ui.min.js",
    stylesheet = "jquery-ui.css"
    ),
    htmltools::htmlDependency(
      name       = "consortui-bindings",
      version    = "0.1.0",
      package    = "DIVE",
      src        = "www",
      script     = "consortUIBinding.js",
      stylesheet = "consortUI.css"
    )
  )
  placeholder <- sprintf('#%s:empty:before{content: "%s"; font-size: 14px; opacity: 0.5;}',
                         inputId, placeholder)
  placeholder <- shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(placeholder))))
  label <- shiny::tags$label(label, `for` = inputId)
  tagsBuilder <- function(value, label) {
    shiny::tags$div(
      `data-value` = value,
      class = item_class,
      label)
  }
  item_tags <- Map(tagsBuilder, items, items)
  style <- sprintf("width: %s; font-size: 14px; min-height: 25px;", shiny::validateCssUnit(width))
  container <- shiny::tags$div(id = inputId, style = style, class = paste(classes, "consort"), ...)
  container <- shiny::tagSetChildren(container, list = item_tags)
  return(shiny::tagList(dep, placeholder, label, container))
}




