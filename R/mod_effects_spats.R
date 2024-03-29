#' effects_spats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_effects_spats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">BLUPs/BLUEs Predictions</h1>'),
    fluidRow(
      column(
        12,
        bs4Card(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plotblup2")),
            type = 5, color = "#28a745"
          ),
          title = tagList(icon = icon("sort-numeric-up", verify_fa = FALSE), "Predictions Plot"),
          solidHeader = FALSE, status = "success", width = 12, collapsed = T
        )
      )
    ),
    fluidRow(
      column(
        12,
        fluidRow(
          column(
            bs4Card(
              shinycssloaders::withSpinner(
                DT::dataTableOutput(ns("blups")),
                type = 5, color = "#28a745"
              ),
              width = 12,
              title = tagList(icon = icon("table"), "Predictions Table"),
              status = "danger", solidHeader = FALSE,
              downloadButton(ns("downloadData2"), label = "Download")
            ),
            width = 6
          ),
          column(
            bs4Card(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("hist")),
                type = 5, color = "#28a745"
              ),
              width = 12,
              title = tagList(icon = icon("chart-bar"), "Histogram"),
              status = "success", solidHeader = FALSE, collapsed = T
            ),
            width = 6
          )
        )
      )
    )
  )
}

#' effects_spats Server Function
#'
#' @noRd
mod_effects_spats_server <- function(input, output, session, Model) {
  ns <- session$ns


  output$plotblup2 <- plotly::renderPlotly({
    BLUPS <- Model$Effects()
    BLUPS$Lu <- BLUPS[, 2] - 1.645 * BLUPS[, 3]
    BLUPS$Ls <- BLUPS[, 2] + 1.645 * BLUPS[, 3]
    v <- as.character(BLUPS[order(BLUPS[, 2], decreasing = TRUE), 1])
    names(BLUPS)[1] <- "Line"
    names(BLUPS)[2] <- "predicted.value"


    if ("type" %in% names(BLUPS)) {
      q <- ggplot2::ggplot(BLUPS, ggplot2::aes(x = Line, y = predicted.value, color = type))
    } else {
      q <- ggplot2::ggplot(BLUPS, ggplot2::aes(x = Line, y = predicted.value))
    }

    p <- q +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = Ls, ymin = Lu)) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = mean(BLUPS[, 2]), linetype = 2, color = "grey") +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(), 
        axis.text.x = ggplot2::element_blank(), 
        axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::ylab(names(BLUPS)[2]) + ggplot2::scale_x_discrete(limits = v)
    isolate(plotly::ggplotly(p))
  })


  output$blups <- DT::renderDataTable({
    DT::datatable(
      {
        Model$Effects()
      },
      option = list(pageLength = 6, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:ncol(Model$Effects())))),
      filter = "top",
      selection = "multiple"
    )
  })

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("effects_SpATS_Model_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(Model$Effects(), file, row.names = FALSE)
    }
  )

  output$hist <- plotly::renderPlotly({
    BLUPS <- Model$Effects()
    hi <- graphics::hist(BLUPS[, 2], plot = FALSE)
    br <- hi$breaks
    label <- names(BLUPS)[2]
    k <- ggplot2::ggplot(BLUPS, ggplot2::aes_string(label)) +
      ggplot2::geom_histogram(breaks = c(br)) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0("Histogram of ", label)) +
      ggplot2::xlab("")
    isolate(plotly::ggplotly(k))
  })
}

## To be copied in the UI
# mod_effects_spats_ui("effects_spats_ui_1")

## To be copied in the server
# callModule(mod_effects_spats_server, "effects_spats_ui_1")
