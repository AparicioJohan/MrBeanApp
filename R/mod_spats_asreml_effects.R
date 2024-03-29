#' spats_asreml_effects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spats_asreml_effects_ui <- function(id) {
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
                    title = tagList(icon = icon("table"), "Predictions Table"),
                    status = "success", solidHeader = FALSE,
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

#' spats_asreml_effects Server Function
#'
#' @noRd
mod_spats_asreml_effects_server <- function(input, output, session, model) {
  ns <- session$ns

  # observe({
  #   if(is.null(model$model())){
  #     show(id = "notfound", anim = TRUE, animType = "slide" )
  #   } else{
  #     hide(id = "notfound", anim = TRUE, animType = "slide" )
  #   }
  # })

  # observeEvent(model$run(),{
  #   if(!is.null(model$model())){
  #     show(id = "second", anim = TRUE, animType = "slide" )
  #   } else{
  #     hide(id = "second", anim = TRUE, animType = "slide" )
  #   }
  # }, ignoreInit = T, ignoreNULL = T)

  observe({
    toggle(id = "notfound", condition = is.null(model$model()))
    toggle(id = "second", condition = !is.null(model$model()))
  })


  BLUPS <- reactive({
    model$run()
    isolate({
      req(model$model()$mod)
      model$model()$predictions %>%
        dplyr::mutate(
          lower = predicted.value - 1.645 * std.error,
          upper = predicted.value + 1.645 * std.error
        ) %>%
        dplyr::select(-status)
    })
  })

  output$plotblup2 <- plotly::renderPlotly({
    model$run()
    isolate({
      BLUPS <- BLUPS()

      q <- ggplot2::ggplot(BLUPS, ggplot2::aes(x = gen, y = predicted.value))
      v <- as.character(BLUPS[order(BLUPS[, "predicted.value"], decreasing = TRUE), 1])

      p <- q +
        ggplot2::geom_point(size = 1) +
        ggplot2::geom_errorbar(ggplot2::aes(ymax = upper, ymin = lower)) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(yintercept = mean(BLUPS[, "predicted.value"]), linetype = 2, color = "red") +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(), 
          axis.text.x = ggplot2::element_blank(), 
          axis.ticks.x = ggplot2::element_blank()) +
        ggplot2::scale_x_discrete(limits = v)
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
      paste("effects_ASReml_Model_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(BLUPS(), file, row.names = FALSE)
    }
  )

  output$hist <- plotly::renderPlotly({
    BLUPS <- BLUPS()
    hi <- graphics::hist(BLUPS[, "predicted.value"], plot = FALSE)
    br <- hi$breaks
    label <- names(BLUPS)[2]
    k <- ggplot2::ggplot(BLUPS, ggplot2::aes_string("predicted.value")) +
      ggplot2::geom_histogram(breaks = c(br)) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0("Histogram of Genotype Predictions")) +
      ggplot2::xlab("")
    isolate(plotly::ggplotly(k))
  })
}

## To be copied in the UI
# mod_spats_asreml_effects_ui("spats_asreml_effects_ui_1")

## To be copied in the server
# callModule(mod_spats_asreml_effects_server, "spats_asreml_effects_ui_1")
