#' residuals_lme4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_residuals_lme4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("plot_effects")),
          type = 6, color = "#28a745"
        ),
        width = 12, title = tagList(shiny::icon("sort-numeric-up", verify_fa = FALSE), "Genotype Effects"), status = "success", solidHeader = FALSE
      )
    ),
    fluidRow(
      column(
        width = 6,
        fluidRow(
          bs4TabCard(
            width = 12, id = "Res_lme4", title = tagList(shiny::icon("bar-chart-o", verify_fa = FALSE), "Residuals"),
            status = "success", collapsible = T, maximizable = T, solidHeader = FALSE, side = "left", type = "tabs",
            tabPanel(
              title = "Residual", active = T, helpText("First run the Mixed Model"),
              prettySwitch(
                inputId = ns("swicht"),
                label = "Fitted Values",
                status = "success",
                slim = TRUE
              ),
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("plotati3")),
                type = 5, color = "#28a745"
              ), icon = icon("circle-arrow-right", verify_fa = FALSE)
            ),
            tabPanel(
              title = "QQplot",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("Normality")),
                type = 5, color = "#28a745"
              ), icon = icon("ellipsis-h", verify_fa = FALSE)
            ),
            tabPanel(
              title = "Hist",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("Hgram")),
                type = 5, color = "#28a745"
              ), icon = icon("ellipsis-h", verify_fa = FALSE)
            )
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          bs4Card(
            width = 12, style = "overflow-x: scroll;",
            status = "success", title = "Outliers Residuals", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("OUT3")), type = 5, color = "#28a745")
          )
        )
      )
    )
  )
}

#' residuals_lme4 Server Function
#'
#' @noRd
mod_residuals_lme4_server <- function(input, output, session, model) {
  ns <- session$ns


  output$plot_effects <- plotly::renderPlotly({
    model$run()
    req(model$model())
    if (model$run() == 0) {
      return()
    }
    isolate({
      if (isFALSE(model$res_ran2())) {
        req(model$effects())
        BLUPS <- model$effects()
        v <- as.character(BLUPS[order(BLUPS$Estimation, decreasing = TRUE), 1])
        g1 <- ggplot(BLUPS, aes(x = Genotype, Estimation)) +
          geom_point(size = 1) +
          geom_errorbar(aes(ymax = upper, ymin = lower)) +
          theme_bw() +
          theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          scale_x_discrete(limits = v)
        plotly::ggplotly(g1)
      } else {
        BLUPS <- model$effects()
        BLUPS$ub <- BLUPS[, 2] - 1.645 * BLUPS[, 3]
        BLUPS$lb <- BLUPS[, 2] + 1.645 * BLUPS[, 3]
        gen <- names(BLUPS)[1]
        effect <- names(BLUPS)[2]
        v <- as.character(BLUPS[order(BLUPS[, 2], decreasing = TRUE), 1])

        p <- ggplot(BLUPS, aes(x = .data[[gen]], y = .data[[effect]])) +
          geom_errorbar(aes(ymax = ub, ymin = lb)) +
          theme_bw() +
          theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          geom_point(size = 1) +
          scale_x_discrete(limits = v)
        plotly::ggplotly(p)
      }
    })
  })

  data_out <- reactive({
    req(model$model())
    res_data_lme4(model$model())
  })

  output$plotati3 <- plotly::renderPlotly({
    model$run()
    input$swicht
    isolate({
      if (input$swicht) {
        res_fitted(data_out())
      } else {
        res_index(data_out())
      }
    })
  })

  output$Normality <- plotly::renderPlotly({
    model$run()
    isolate({
      res_qqplot(data_out())
    })
  })

  output$Hgram <- plotly::renderPlotly({
    model$run()
    isolate({
      res_hist(data_out())
    })
  })

  output$OUT3 <- DT::renderDataTable({
    model$run()
    isolate({
      raw_dt <- data_out()
      raw_dt <- raw_dt[raw_dt$Classify == "Outlier" & !is.na(raw_dt$Classify), ]

      DT::datatable(raw_dt,
        extensions = "Buttons", filter = "top", selection = "multiple",
        options = list(
          scrollX = TRUE, pageLength = 2, lengthMenu = c(1:4, nrow(raw_dt)),
          columnnDefs = list(list(className = "dt-center", targets = 0:ncol(raw_dt)))
        )
      )
    })
  })
}

## To be copied in the UI
# mod_residuals_lme4_ui("residuals_lme4_ui_1")

## To be copied in the server
# callModule(mod_residuals_lme4_server, "residuals_lme4_ui_1")
