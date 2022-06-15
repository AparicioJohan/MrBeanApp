#' descrip_scatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_descrip_scatter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        dropdown(
          tags$h3("Input List"),
          uiOutput(ns("vary")),
          uiOutput(ns("varx")),
          awesomeCheckbox(
            inputId = ns("factor_scat"),
            label = "Grouping variable?",
            value = F,
            status = "danger"
          ),
          uiOutput(ns("factor2")),
          actionButton(
            ns("actionplot"),
            label = "Plot",
            class = "btn-success",
            style = "display:rigth ;color: white  ; background-color: #28a745"
          ),
          animate = shinyWidgets::animateOptions(
            enter = shinyWidgets::animations$fading_entrances[["fadeInLeftBig"]],
            exit = shinyWidgets::animations$fading_exits[["fadeOutLeftBig"]]
          ),
          style = "unite",
          icon = icon("gear", verify_fa = FALSE),
          status = "warning",
          width = "300px"
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("plot")),
          type = 5,
          color = "#28a745"
        ),
        uiOutput(ns("correlation")),
        width = 12,
        title = tagList(shiny::icon("braille"), "Scatterplot"),
        status = "success",
        solidHeader = FALSE,
        maximizable = T
      )
    )
  )
}

#' descrip_scatter Server Functions
#'
#' @noRd
mod_descrip_scatter_server <- function(id, data, plot = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$varx <- renderUI({
      selectInput(
        ns("variablex"),
        "Select variable for X-axis",
        choices = names(data$data()),
        selected = "col"
      )
    })

    output$vary <- renderUI({
      selectInput(
        ns("variabley"),
        "Select variable for Y-axis ",
        choices = names(data$data()),
        selected = "row"
      )
    })

    output$factor2 <- renderUI({
      selectInput(
        ns("factor2"),
        "Select a grouping variable",
        choices = names(data$data()),
        selected = "yield"
      )
    })

    observe({
      toggle(
        id = "factor2",
        condition = input$factor_scat,
        animType = "fade",
        anim = TRUE
      )
    })


    output$plot <- plotly::renderPlotly({
      req(input$actionplot)
      if (isTRUE(input$factor_scat)) {
        isolate({
          req(input$factor2)
          dt <- data$data()
          dt[, input$factor2] <- as.factor(dt[, input$factor2])
          if (plot == 1) {
            gra <- ggplot(
              dt,
              aes_string(
                x = input$variablex,
                y = input$variabley,
                color = input$factor2
              )
            ) +
              geom_point() +
              theme_bw()
          } else {}
          plotly::ggplotly(gra)
        })
      } else {
        isolate({
          dt <- data$data()
          dt[, input$factor2] <- as.factor(dt[, input$factor2])
          if (plot == 1) {
            gra <- ggplot(
              dt,
              aes_string(
                x = input$variablex,
                y = input$variabley
              )
            ) +
              geom_point() +
              theme_bw()
          } else {}
          plotly::ggplotly(gra)
        })
      }
    })



    # Correlation -------------------------------------------------------------

    output$correlation <- renderUI({
      req(input$actionplot)
      isolate({
        if (plot == 1) {
          req(input$variablex)
          req(input$variabley)
          x <- data$data()[, input$variablex]
          y <- data$data()[, input$variabley]
          req(is.numeric(x))
          req(is.numeric(y))
          C <- round(cor(x, y, use = "pairwise.complete.obs"), 3)
          P <- round(cor.test(x, y)$p.value, 3)
          Corr <- paste("Correlation =", C, "/ p.value =", P)
          return(
            tagList(
              br(),
              div(
                id = ns("corr_show"),
                h5(Corr)
              )
            )
          )
        } else {
          return()
        }
      })
    })
  })
}

## To be copied in the UI
# mod_descrip_scatter_ui("descrip_scatter_1")

## To be copied in the server
# mod_descrip_scatter_server("descrip_scatter_1")
