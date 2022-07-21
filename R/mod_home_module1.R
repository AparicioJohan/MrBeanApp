#' home_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_module1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 9,
        bs4Card(
          HTML("<img src='www/beans3.png' width='52' vspace='10' hspace='10' height='48' align='left'>
                  <font size='7'>Mr.Bean</font>"),
          br(),
          includeHTML(
            system.file("app/www/encabezado.html", package = "MrBean")
          ),
          width = 12,
          status = "success",
          solidHeader = FALSE,
          title = tagList(shiny::icon("house", verify_fa = FALSE), "Home")
        )
      ),
      column(
        width = 3,
        bs4Dash::valueBox(
          value = "SpATS",
          subtitle = "Spatial P-Splines",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://cran.r-project.org/web/packages/SpATS/SpATS.pdf",
          icon = shiny::icon("braille")
        ),
        bs4Dash::valueBox(
          value = "ASReml-R",
          subtitle = "AR1xAR1 Correlation",
          width = 12,
          color = "danger",
          elevation = 3,
          href = "https://asreml.kb.vsni.co.uk/wp-content/uploads/sites/3/2018/02/ASReml-R-Reference-Manual-4.pdf",
          icon = shiny::icon("braille")
        ),
        bs4Dash::valueBox(
          value = "Lme4",
          subtitle = "lmer",
          width = 12,
          icon = shiny::icon("chart-line"),
          color = "warning",
          elevation = 3,
          href = "https://cran.r-project.org/web/packages/lme4/lme4.pdf"
        ),
        bs4Dash::valueBox(
          value = "Predictions",
          subtitle = "BLUPs/BLUEs",
          width = 12,
          icon = shiny::icon("sort-numeric-up", verify_fa = FALSE),
          color = "info",
          elevation = 3,
          href = "https://www.frontiersin.org/articles/10.3389/fpls.2018.01511/full"
        ),
        bs4Card(
          title = "Jump",
          status = "danger",
          width = 12,
          solidHeader = TRUE,
          actionLink(
            inputId = ns("toAwesome1"),
            label = "Data",
            icon = icon("database"),
            style = "color: #d9534f"
          ),
          br(),
          actionLink(
            inputId = ns("toAwesome2"),
            label = "Spatial",
            icon = icon("braille"),
            style = "color: #d9534f"
          ),
          br(),
          actionLink(
            inputId = ns("toAwesome3"),
            label = "Lme4",
            icon = icon("bar-chart-o", verify_fa = FALSE),
            style = "color: #d9534f"
          ),
          br()
        )
      )
    )
  )
}

#' home_module1 Server Function
#'
#' @noRd
mod_home_module1_server <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$toAwesome1, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "Data")
  })

  observeEvent(input$toAwesome2, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "modelo")
  })

  observeEvent(input$toAwesome3, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "mixed")
  })
}

## To be copied in the UI
# mod_home_module1_ui("home_module1_ui_1")

## To be copied in the server
# callModule(mod_home_module1_server, "home_module1_ui_1")
