#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom magrittr "%>%"
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel))
#' @rawNamespace import(shinyjs, except = c(alert, runExample))
#' @rawNamespace import(lme4, except = show)
#' @rawNamespace import(Matrix, except = show)
#' @import shinyWidgets readxl ggplot2 summarytools SpATS waiter echarts4r data.table ggrepel
run_app <- function(...) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = TRUE)
    ),
    golem_opts = list(...)
  )
}
