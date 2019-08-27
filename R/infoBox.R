#' A UI box element for summary values in dashboard-like presentation 
#'
#' This is similar to the UI element in shinydashboard.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
infoBox <- function(title, value, subtitle = NULL, icon = shiny::icon("info"), width = 4) {
  boxStyle <- "display: block;
               min-height: 100px;
               background: #fff;
               width: 100%;
               box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
               border-radius: 2px;
               margin-bottom: 15px;"
  iconStyle <- "border-top-left-radius: 2px;
                border-top-right-radius: 0;
                border-bottom-right-radius: 0;
                border-bottom-left-radius: 2px;
                display: block;
                float: left;
                height: 100px;
                width: 100px;
                text-align: center;
                font-size: 45px;
                line-height: 100px;
                background: rgba(0, 0, 0, 0.2);"
  contentStyle <- "padding: 5px 10px;
                   margin-left: 100px;"
  titleStyle <- "text-transform: uppercase;"
  valueStyle <- "display: block;
                font-weight: bold;
                font-size: 24px;"
  box <- div(style = boxStyle,
                    span(style = iconStyle, icon),
                    div(style = contentStyle, 
                        span(style = titleStyle, title), 
                        span(style = valueStyle, value), 
                        if (!is.null(subtitle)) p(subtitle)
                      )
                    )
  div(class = if (!is.null(width)) paste0("col-sm-", width), box)
}

#' A UI box element for summary values in dashboard-like presentation 
#'
#' This is similar to the UI element in shinydashboard.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
valueBox <- function(value, subtitle, icon = NULL, textcolor = "black", bgcolor = "white", width = 4, href = NULL) {
  boxCSS <- "
    border-radius: 2px;
    position: relative;
    display: block;
    margin-bottom: 20px;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);"
  iconCSS <- "
    position: absolute;
    top: auto;
    bottom: 5px;
    right: 5px;
    font-size: 70px;
    color: rgba(0, 0, 0, 0.15);"
  subtitleCSS <- "
    font-weight: bold;
    font-size: 48px;"
  boxContent <- div(style = paste0(boxCSS, paste("color:", textcolor), ";", paste("background-color:", bgcolor)), 
                    div(style = "padding: 10px;", div(style = subtitleCSS, value), p(subtitle)), if (!is.null(icon)) div(style = iconCSS, icon))
  if (!is.null(href)) boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) paste0("col-sm-", width), boxContent)
}
  