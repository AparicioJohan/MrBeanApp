#' residuals_spats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_residuals_spats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("notfound"),
      rep_br(2),
      HTML("<center><img src='www/code.svg' width='60%' height='60%'></center>")
    ),
    div(
      id = ns("block2"),
      fluidRow(
        column(
          width = 6,
          bs4Dash::box(
            status = "success", width = 12, collapsible = TRUE, collapsed = F,
            title = tagList(icon = icon("braille"), "Field Residuals"), solidHeader = FALSE, maximizable = T,
            prettySwitch(
              inputId = ns("swicht2"),
              label = "Histogram",
              status = "success",
              slim = TRUE
            ),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("field")),
              type = 5, color = "#28a745"
            ),
            prettySwitch(
              inputId = ns("swicht"),
              label = "Fitted Values",
              status = "success",
              slim = TRUE
            ),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("plotati2")),
              type = 5, color = "#28a745"
            )
          )
        ),
        column(
          width = 6,
          bs4TabCard(
            width = 12,
            maximizable = T, solidHeader = FALSE, closable = F,
            status = "success", side = "left", type = "tabs",
            tabPanel(
              title = "QQplot",
              icon = icon("circle-arrow-right", verify_fa = FALSE),
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("qqplot")), type = 5, color = "#28a745")
            ),
            tabPanel(
              title = "Residual against Other",
              icon = icon("exchange-alt", verify_fa = FALSE),
              fluidRow(
                column(
                  width = 6,
                  selectInput(ns("variable"), label = "Variable", choices = "", width = "100%")
                ),
                column(
                  width = 6,
                  strong("Is it a factor?"),
                  switchInput(
                    inputId = ns("factor"),
                    size = "large",
                    label = "factor",
                    onLabel = "Yes",
                    offLabel = "No",
                    onStatus = "success",
                    offStatus = "danger"
                  )
                )
              ),
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("compare")), type = 5, color = "#28a745")
            ),
            tabPanel(
              title = "Residuals Table",
              icon = icon("table"),
              shinycssloaders::withSpinner(DT::dataTableOutput(ns("Info")), type = 5, color = "#28a745")
            )
          ),
          bs4Card(shinycssloaders::withSpinner(DT::dataTableOutput(ns("OUT")), type = 5, color = "#28a745"),
            verbatimTextOutput(ns("selectOut")), # sugerencia Elizabeth
            disabled(downloadButton(ns("downByhand"), "Download Data Cleaned",
              class = "btn-success",
              style = "color: white ; background-color: #28a745"
            )),
            width = 12, style = "overflow-x: scroll;",
            status = "success", title = tagList(icon = icon("exclamation-triangle", verify_fa = FALSE), "Potential Outliers"), solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE
          )
        )
      )
    )
  )
}

#' residuals_spats Server Function
#'
#' @noRd
mod_residuals_spats_server <- function(input, output, session, Model) {
  ns <- session$ns


  observe({
    a <- try(Model$Modelo()$call, silent = T)
    if (class(a)[1] != "call") {
      shinyjs::hide("block2", animType = "fade", anim = TRUE)
      shinyjs::show("notfound", animType = "fade", anim = TRUE)
    } else {
      shinyjs::hide("notfound", animType = "fade", anim = TRUE)
      shinyjs::show("block2", animType = "fade", anim = TRUE)
    }
  })


  data_out <- reactive({
    req(Model$Modelo())
    res_data(Model$Modelo(), k = Model$rLimit())
  })

  observeEvent(Model$Modelo()$data, {
    updateSelectInput(
      session, 
      inputId = "variable", 
      choices = names(Model$Modelo()$data),
      selected = "YdHa_clean"
    )
  })

  output$field <- plotly::renderPlotly({
    Model$action()
    if (input$swicht2) {
      res_hist(data_out())
    } else {
      res_map(data_out())
    }
  })

  output$plotati2 <- plotly::renderPlotly({
    Model$action()
    if (input$swicht) {
      res_fitted(data_out())
    } else {
      res_index(data_out())
    }
  })

  output$qqplot <- plotly::renderPlotly({
    Model$action()
    isolate({
      res_qqplot(data_out())
    })
  })


  output$compare <- plotly::renderPlotly({
    Model$action()

    req(input$variable)
    req(Model$Modelo())
    res_compare(
      Model = Model$Modelo(), 
      variable = input$variable,
      factor = input$factor
    )
  })

  output$Info <- DT::renderDataTable({
    req(Model$action())
    req(Model$Modelo())
    dt <- dplyr::mutate_if(data_out(), is.numeric, round, 1)[, -1]
    DT::datatable(
      dt,
      extensions = "Buttons", 
      filter = "top", 
      selection = "multiple",
      options = list(
        dom = "lfrtipB",
        scrollX = TRUE,
        pageLength = 2,
        lengthMenu = c(1:4, nrow(dt)),
        columnnDefs = list(
          list(className = "dt-center", targets = 0:ncol(dt))
        )
      )
    )
  })

  data_res <- reactive({
    req(Model$action())
    req(Model$Modelo())
    dt <- dplyr::mutate_if(data_out(), is.numeric, round, 1)[, -1]
    raw_dt <- Model$Modelo()$data
    raw_dt$Classify <- dt$Classify
    raw_dt <- raw_dt[raw_dt$Classify == "Outlier" & !is.na(raw_dt$Classify), ]
    raw_dt
  })

  output$OUT <- DT::renderDataTable({
    raw_dt <- data_res()
    DT::datatable(
      raw_dt,
      extensions = "Buttons", 
      filter = "top", 
      selection = "multiple",
      options = list(
        scrollX = TRUE, 
        pageLength = 2,
        lengthMenu = c(1:4, nrow(raw_dt)),
        columnnDefs = list(
          list(className = "dt-center", targets = 0:ncol(raw_dt))
        )
      )
    )
  })

  output$selectOut <- renderPrint({
    s <- input$OUT_rows_selected
    if (length(s)) {
      cat("These outliers will be selected to be removed:\n\n")
      cat(data_res()[s, Model$Modelo()$model$response], sep = ", ")
    }
  })

  selecByhand <- reactive({
    req(Model$Modelo())
    OUT2 <- data_res()
    s <- input$OUT_rows_selected
    OUT2 <- OUT2[s, ]
    dt <- Model$Modelo()$data
    dt$Classify <- data_out()$Classify
    dt <- setdiff(dt, OUT2)
    OUT2[, Model$Modelo()$model$response] <- NA
    dt <- rbind(OUT2, dt)
    dt
  })


  output$downByhand <- downloadHandler(
    filename = function() {
      paste("Data_Clean_", input$variable, ".csv", sep = "")
    },
    content = function(file) {
      req(input$OUT_rows_selected)
      datos <- data.frame(selecByhand())
      utils::write.csv(datos, file, row.names = FALSE)
    }
  )

  observeEvent(length(input$OUT2_rows_selected), {
    enable("downByhand")
  })
}

## To be copied in the UI
# mod_residuals_spats_ui("residuals_spats_ui_1")

## To be copied in the server
# callModule(mod_residuals_spats_server, "residuals_spats_ui_1")
