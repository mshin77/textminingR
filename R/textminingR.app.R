# Launch textminingR app ----

#' @title Launch and browser the textminingR app
#'
#' @name textminingR.app
#'
#' @description
#' Launch and browser the textminingR app.
#'
#' @examples
#' \dontrun{
#' library(textminingR)
#' textminingR.app()
#' }
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import ggraph
#' @import shiny
#' @import stm
#' @import tidyr
#' @import tidytext
#' @import widyr
#' @importFrom magrittr %>%

textminingR.app <- function() {

    uiDir <- system.file("textminingR.app", "ui.R", package = "textminingR")
    serveDir <- system.file("textminingR.app", "server.R", package = "textminingR")

    source(uiDir, local = TRUE, chdir = TRUE)
    source(serveDir, local = TRUE, chdir = TRUE)


    # Running a Shiny app object
    app <- shinyApp(ui, server)
    runApp(app, display.mode = "normal", launch.browser = TRUE)

}

