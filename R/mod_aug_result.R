#' aug_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aug_result_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("notfound"),
      rep_br(2),
      HTML("<center><img src='www/code.svg' width='60%' height='60%'></center>")
    ),
    shinyjs::hidden(
      div(
        id = ns("second"),
        fluidRow(
          column(
            12,
            fluidRow(
              bs4Card(
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("plotblup2")),
                  type = 5, color = "#28a745"
                ),
                title = tagList(icon = icon("sort-numeric-up", verify_fa = FALSE), "Predictions Plot"),
                solidHeader = FALSE, status = "success", width = 12, collapsed = F
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            fluidRow(
              column(
                fluidRow(
                  bs4Card(
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput(ns("blups")),
                      type = 5, color = "#28a745"
                    ),
                    width = 12,
                    title = tagList(icon = icon("table"), "Predictions Table"), status = "success", solidHeader = FALSE,
                    downloadButton(ns("downloadData2"), label = "Download")
                  )
                ),
                width = 6
              ),
              column(
                fluidRow(
                  bs4Card(
                    shinycssloaders::withSpinner(
                      plotly::plotlyOutput(ns("hist")),
                      type = 5, color = "#28a745"
                    ),
                    width = 12,
                    title = tagList(icon = icon("chart-bar"), "Histogram"),
                    status = "success", solidHeader = FALSE, collapsed = F
                  )
                ),
                width = 6
              )
            )
          )
        )
      )
    )
  )
}

#' aug_result Server Function
#'
#' @noRd
mod_aug_result_server <- function(input, output, session, model) {
  ns <- session$ns

  observe({
    toggle(id = "notfound", condition = is.null(model$model()))
    toggle(id = "second", condition = !is.null(model$model()))
  })

  BLUPS <- reactive({
    req(model$run())
    req(model$model()$mod)
    model$model()$predictions %>%
      dplyr::mutate(
        lower_95 = predicted.value - 1.645 * std.error,
        upper_95 = predicted.value + 1.645 * std.error
      ) %>%
      dplyr::select(-status)
  })

  output$plotblup2 <- plotly::renderPlotly({
    model$run()
    isolate({
      BLUPS <- BLUPS()

      if ("check" %in% names(BLUPS)) {
        q <- ggplot(BLUPS, aes(x = gen, y = predicted.value, color = check))
        v <- as.character(BLUPS[order(BLUPS[, "predicted.value"], decreasing = TRUE), 2])
      } else {
        q <- ggplot(BLUPS, aes(x = gen, y = predicted.value))
        v <- as.character(BLUPS[order(BLUPS[, "predicted.value"], decreasing = TRUE), 1])
      }

      p <- q +
        geom_point(size = 1) +
        geom_errorbar(aes(ymax = upper_95, ymin = lower_95)) +
        theme_bw() +
        geom_hline(yintercept = mean(BLUPS[, "predicted.value"]), linetype = 2, color = "grey") +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        scale_x_discrete(limits = v) + xlab("Genotypes")
      isolate(plotly::ggplotly(p))
    })
  })

  output$blups <- DT::renderDataTable({
    DT::datatable(
      {
        dplyr::mutate_if(BLUPS(), is.numeric, round, 2)
      },
      option = list(pageLength = 6, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:ncol(BLUPS())))),
      filter = "top",
      selection = "multiple"
    )
  })

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("preds_spatial_auM_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(BLUPS(), file, row.names = FALSE)
    }
  )

  output$hist <- plotly::renderPlotly({
    BLUPS <- BLUPS()
    hi <- hist(BLUPS[, "predicted.value"], plot = FALSE)
    br <- hi$breaks
    label <- names(BLUPS)[2]
    k <- ggplot(BLUPS, aes_string("predicted.value")) +
      geom_histogram(breaks = c(br)) +
      theme_bw() +
      ggtitle(paste0("Histogram of Genotype Predictions")) +
      xlab("")
    isolate(plotly::ggplotly(k))
  })
}

## To be copied in the UI
# mod_aug_result_ui("aug_result_ui_1")

## To be copied in the server
# callModule(mod_aug_result_server, "aug_result_ui_1")
