#' distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distribution_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        fluidRow(
          bs4Dash::box(
            width = 12,
            status = "success",
            title = "Descriptive",
            solidHeader = TRUE,
            selectInput(
              inputId = ns("Id088"),
              label = "Select variable",
              choices = "",
              width = "100%"
            ),
            sliderInput(
              ns("alphagg"),
              label = "Alpha Bar",
              min = 0,
              max =  1,
              value = 0.9,
              width = "100%"
            ),
            sliderInput(
              ns("binwidth"),
              label = "Binwidth",
              min = 3,
              max =  150,
              value =  30,
              width = "100%",
              step = 2
            ),
            awesomeCheckbox(
              inputId = ns("factor_hist"),
              label = "Grouping variable?",
              value = F,
              status = "danger"
            ),
            selectInput(
              inputId = ns("Id088f"),
              label = "Factor",
              choices = "",
              width = "100%"
            ),
            sliderInput(
              ns("alphagD"),
              label = "Alpha Density",
              min = 0,
              max =  1,
              value =  0.2,
              width = "100%"
            ),
            awesomeCheckbox(
              inputId = ns("Facewrap"),
              label = "Individual Plots",
              value = F,
              status = "danger"
            )
          )
        )
      ),
      column(
        width = 9,
        fluidRow(
          bs4TabCard(
            width = 12,
            maximizable = T,
            solidHeader = FALSE,
            closable = F,
            status = "success",
            side = "left", 
            type = "tabs",
            tabPanel(
              title = "Histogram",
              icon = icon("chart-bar"),
              active = T,
              dropdown(
                prettyRadioButtons(
                  inputId = ns("typefileDes"),
                  label = "Download Plot File Type",
                  outline = TRUE,
                  fill = FALSE,
                  shape = "square",
                  inline = TRUE,
                  choices = list(PNG = "png", PDF = "pdf"),
                  icon = icon("check"),
                  animation = "tada"
                ),
                conditionalPanel(
                  condition = "input.typefileDes=='png'",
                  ns = ns,
                  sliderInput(
                    inputId = ns("png.wid.d"),
                    min = 200,
                    max = 2000,
                    value = 900,
                    label = "Width pixels"
                  ),
                  sliderInput(
                    inputId = ns("png.hei.d"),
                    min = 200,
                    max = 2000,
                    value = 600,
                    label = "Height pixels"
                  )
                ),
                conditionalPanel(
                  condition = "input.typefileDes=='pdf'",
                  ns = ns,
                  sliderInput(
                    inputId = ns("pdf.wid.d"),
                    min = 2,
                    max = 20,
                    value = 10,
                    label = "Width"
                  ),
                  sliderInput(
                    inputId = ns("pdf.hei.d"),
                    min = 2,
                    max = 20,
                    value = 8,
                    label = "Height"
                  )
                ),
                downloadButton(
                  ns("descargar.d"),
                  "Download plot",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745"
                ),
                br(),
                animate = shinyWidgets::animateOptions(
                  enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                  exit = shinyWidgets::animations$fading_exits$fadeOutLeftBig
                ),
                style = "unite",
                icon = icon("gear", verify_fa = FALSE),
                status = "warning",
                width = "300px"
              ),
              plotOutput(
                ns("plothistogram"),
                click = ns("plot1_click"),
                brush = brushOpts(
                  id = ns("plot1_brush"),
                  direction = "x",
                  fill = "#ccc"
                )
              ),
              shinyjs::hidden(
                div(
                  id = ns("sel"),
                  hr(),
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput(ns("dataSele")),
                    type = 5,
                    color = "#28a745"
                  )
                )
              )
            ),
            tabPanel(
              title = "Summary Statistics",
              active = F,
              icon = icon("sort-numeric-up", verify_fa = FALSE),
              DT::dataTableOutput(ns("statisticSumm2"))
            )
          )
        )
      )
    )
  )
}

#' distribution Server Function
#'
#' @noRd
mod_distribution_server <- function(input, output, session, data) {
  ns <- session$ns


  #------------  HISTOGRAM ------------

  observeEvent(data$data(), {
    updateSelectInput(
      session, "Id088",
      choices = names(data$data()),
      selected = "YdHa_clean"
    )
    updateSelectInput(
      session,
      "Id088f",
      choices = names(data$data()),
      selected = "NNNNN"
    )
  })

  observe({
    toggle(
      id = "Id088f",
      condition = input$factor_hist,
      animType = "fade",
      anim = TRUE
    )
  })

  observe({
    toggle(
      id = "Facewrap",
      condition = input$factor_hist,
      animType = "fade",
      anim = TRUE
    )
  })

  observe({
    toggle(
      id = "alphagD",
      condition = input$factor_hist,
      animType = "fade",
      anim = TRUE
    )
  })

  output$plothistogram <- renderPlot({
    validate(
      need(input$Id088 != "", "Select a variable")
    )
    req(data$data())
    dt <- data$data()
    if (is.numeric(dt[, input$Id088])) {
      if ((input$Id088f) == "" | !isTRUE(input$factor_hist)) {
        ggplot2::ggplot(dt, ggplot2::aes_string(x = input$Id088)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            alpha = input$alphagg,
            na.rm = TRUE,
            bins = input$binwidth
          ) +
          ggplot2::theme_bw() +
          ggplot2::geom_density(alpha = input$alphagD, na.rm = TRUE)
      } else {
        dt[, input$Id088f] <- as.factor(dt[, input$Id088f])
        p <- ggplot2::ggplot(
          dt,
          ggplot2::aes_string(
            x = input$Id088,
            fill = input$Id088f,
            color = input$Id088f
          )
        ) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            alpha = input$alphagg,
            position = "identity",
            na.rm = TRUE,
            bins = input$binwidth
          ) +
          ggplot2::theme_bw() +
          ggplot2::geom_density(alpha = input$alphagD, na.rm = TRUE)
        if (isTRUE(input$Facewrap)) {
          p + ggplot2::facet_wrap(~ .data[[input$Id088f]])
        } else {
          p
        }
      }
    } else {
      ggplot2::ggplot(dt, ggplot2::aes_string(x = input$Id088)) +
        ggplot2::geom_bar(na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::coord_flip()
    }
  })

  # Download PLOT
  output$descargar.d <- downloadHandler(
    filename = function() {
      paste("Descriptive", input$typefileDes, sep = ".")
    },
    content = function(file) {
      validate(
        need(input$Id088 != "", "Select a variable")
      )
      req(data$data())
      dt <- data$data()
      if (is.numeric(dt[, input$Id088])) {
        if ((input$Id088f) == "" | !isTRUE(input$factor_hist)) {
          p <- ggplot2::ggplot(dt, ggplot2::aes_string(x = input$Id088)) +
            ggplot2::geom_histogram(
              ggplot2::aes(y = ggplot2::after_stat(density)),
              alpha = input$alphagg,
              na.rm = TRUE,
              bins = input$binwidth
            ) +
            ggplot2::theme_bw() +
            ggplot2::geom_density(alpha = input$alphagD, na.rm = TRUE)
        } else {
          dt[, input$Id088f] <- as.factor(dt[, input$Id088f])
          p <- ggplot2::ggplot(
            dt,
            ggplot2::aes_string(
              x = input$Id088,
              fill = input$Id088f,
              color = input$Id088f
            )
          ) +
            ggplot2::geom_histogram(
              ggplot2::aes(y = ggplot2::after_stat(density)),
              alpha = input$alphagg,
              position = "identity",
              na.rm = TRUE,
              bins = input$binwidth
            ) +
            ggplot2::theme_bw() +
            ggplot2::geom_density(alpha = input$alphagD, na.rm = TRUE)
          if (isTRUE(input$Facewrap)) {
            p <- p + ggplot2::facet_wrap(~ .data[[input$Id088f]])
          } else {
            p
          }
        }
      } else {
        p <- ggplot2::ggplot(
          dt,
          ggplot2::aes_string(
            x = input$Id088
          )
        ) +
          ggplot2::geom_bar(na.rm = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::coord_flip()
      }
      if (input$typefileDes == "png") {
        grDevices::png(file, width = input$png.wid.d, height = input$png.hei.d)
        print(p)
        grDevices::dev.off()
      } else {
        grDevices::pdf(file, width = input$pdf.wid.d, height = input$pdf.hei.d)
        print(p)
        grDevices::dev.off()
      }
    }
  )


  # statistic summary

  statistics <- reactive({
    validate(
      need(input$Id088 != "", "Select a variable")
    )
    req(data$data())
    dt <- data$data()
    if (is.numeric(dt[, input$Id088])) {
      if ((input$Id088f) == "" | !isTRUE(input$factor_hist)) {
        p <- summ_complete(dt, grp = "", var = input$Id088)
      } else {
        p <- summ_complete(dt, grp = input$Id088f, var = input$Id088)
      }
    } else {
      p <- summ_complete(dt, grp = input$Id088f, var = input$Id088)
    }
    p
  })

  output$statisticSumm2 <- DT::renderDataTable({
    DT::datatable(
      {
        statistics() %>% dplyr::mutate_if(is.numeric, round, 2)
      },
      option = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = 0:ncol(statistics())
          )
        )
      ),
      filter = "top",
      selection = "multiple"
    )
  })


  # BRUSH

  infoselect <- reactive({
    req(input$plot1_brush)
    res <- input$plot1_brush
    req(res$mapping$x %in% names(data$data()))
    data$data() %>%
      dplyr::filter(
        .data[[res$mapping$x]] >= res$xmin & .data[[res$mapping$x]] <= res$xmax
      )
  })

  output$dataSele <- DT::renderDataTable({
    req(input$plot1_brush)
    req(infoselect())
    DT::datatable(
      infoselect(),
      extensions = "Buttons",
      filter = "top",
      selection = "multiple",
      options = list(
        dom = "lfrtipB",
        scrollX = TRUE,
        pageLength = 2,
        lengthMenu = c(1:4, nrow(infoselect())),
        columnnDefs = list(
          list(
            className = "dt-center",
            targets = 0:ncol(infoselect())
          )
        )
      )
    )
  })

  observe({
    toggle(
      id = "sel",
      condition = !is.null(input$plot1_brush),
      animType = "fade",
      anim = TRUE
    )
  })
}

## To be copied in the UI
# mod_distribution_ui("distribution_ui_1")

## To be copied in the server
# callModule(mod_distribution_server, "distribution_ui_1")
