#' Shiny module UI for retrieving internal data with action buttons/links
#'
#' Puts any number of action buttons/links on page that can be used to 
#' get data from a specified source.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param labels One or more labels.
#' @param link Logical flag to indicate whether element is action link or button. Defaults to link.
#' @return A div were UI is inserted.
#' @export
getLinkInput <- function(id, labels, link = T) {
  ns <- NS(id)
  lapply(seq_along(labels), 
         function(i) {
           if(link) actionLink(ns(paste0("get", i)), labels[i]) else actionButton(ns(paste0("get", i)), labels[i])
          }
  )
}

#' Shiny module server for retrieving internal data with action buttons/links
#'
#' Inserts action buttons/links that that can be used to 
#' get data from a specified source.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param sources A vector of source paths matching the order of labels given in \code{\link{getLinkInput}}.
#' @param readfun A function to handle reading source files. Defaults to \code{\link[base]{readLines}}.
#' @param ... Additional arguments for \preformatted{readfun}.
#' @return Data object.
getLink <- function(input, output, session,
                    link = T, sources, readfun = readLines, ...) {
  
  linkdata <- reactiveVal(NULL)
  state <- rep(0, length(sources))
  
  readfun <- match.fun(readfun)
  
  whichget <- reactive({
    current <- unlist(lapply(seq_along(sources), function(i) input[[paste0("get", i)]]))
    changed <- which(state != current)
    if(!length(changed)) { 
      return(NULL)
    } else {
      state <<- current
      return(changed)
    }
  })
  
  observeEvent(whichget(), {
    data <- readfun(sources[[whichget()]], ...)
    linkdata(data)
  })
  
  return(linkdata)
}