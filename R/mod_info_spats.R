#' info_spats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_spats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("heritability"), width = 3),
      valueBoxOutput(ns("maxline"), width = 3),
      valueBoxOutput(ns("minline"), width = 3),
      valueBoxOutput(ns("cv"), width = 3)
    ),
    fluidRow(
      bs4TabCard(
        width = 6, 
        maximizable = T, closable = F,
        status = "success", side = "left", type = "tabs",
        tabPanel(
          title = "Summary",
          helpText("First upload your data and fill the required fields."),
          shinycssloaders::withSpinner(
            verbatimTextOutput(ns("summary2")),
            type = 5, color = "#28a745"
          ), icon = icon("circle-arrow-right", verify_fa = FALSE)
        ),
        tabPanel(
          title = "Model Plot",
          dropdown(
            prettyRadioButtons(
              inputId = ns("typefile"), label = "Download Plot File Type", outline = TRUE, fill = FALSE, shape = "square", inline = TRUE,
              choices = list(PNG = "png", PDF = "pdf"),
              icon = icon("check"), animation = "tada"
            ),
            conditionalPanel(
              condition = "input.typefile=='png'", ns = ns,
              sliderInput(inputId = ns("png.wid"), min = 200, max = 2000, value = 900, label = "Width pixels"),
              sliderInput(inputId = ns("png.hei"), min = 200, max = 2000, value = 600, label = "Height pixels")
            ),
            conditionalPanel(
              condition = "input.typefile=='pdf'", ns = ns,
              sliderInput(inputId = ns("pdf.wid"), min = 2, max = 20, value = 10, label = "Width"),
              sliderInput(inputId = ns("pdf.hei"), min = 2, max = 20, value = 8, label = "Height")
            ),
            downloadButton(ns("descargar"), "Download Plot",
              class = "btn-success",
              style = " color: white ; background-color: #28a745"
            ), br(),
            animate = shinyWidgets::animateOptions(
              enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
              exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
            ),
            style = "unite", icon = icon("gear", verify_fa = FALSE),
            status = "warning", width = "300px"
          ),
          shinycssloaders::withSpinner(plotOutput(ns("plot_spats")), type = 5, color = "#28a745"),
          materialSwitch(ns("tog_plot"), label = "Percentage", status = "success", value = FALSE),
          icon = icon("table-cells", verify_fa = FALSE)
        ),
        tabPanel(
          title = "Variance-Plot", icon = icon("signal"),
          shinycssloaders::withSpinner(plotly::plotlyOutput(ns("varcomp"), height = "500px"), type = 5, color = "#28a745")
        ),
        tabPanel(
          title = "Variance-Table", icon = icon("table"),
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("vartable")), type = 6, color = "#28a745"),
          downloadButton(ns("downloadTable"),
            "Download Table",
            class = "btn-success",
            style = " color: white ; background-color: #28a745; float:left"
          )
        )
      ),
      bs4Dash::box(
        status = "success", width = 6, collapsible = TRUE, collapsed = T,
        title = tagList(icon = icon("cloud-sun-rain"), "Spatial Trend"), solidHeader = FALSE, maximizable = T,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("trend")),
          type = 5, color = "#28a745"
        )
      )
    )
  )
}

#' info_spats Server Function
#'
#' @noRd
mod_info_spats_server <- function(input, output, session, Model) {
  ns <- session$ns

  modelo <- reactive({
    req(Model$Modelo())
    Model$Modelo()
  })

  output$plot_spats <- renderPlot({
    Model$action()
    input$tog_plot
    isolate({
      spaTrend <- ifelse(input$tog_plot == TRUE, "percentage", "raw")
      plot(modelo(), spaTrend = spaTrend)
    })
  })

  output$summary2 <- renderPrint({
    Model$action()
    req(modelo())
    isolate(summary(modelo()))
  })

  output$varcomp <- plotly::renderPlotly({
    Model$action()

    isolate({
      VarE <- round(sqrt(modelo()$psi[1]), 2)
      va <- round(sqrt(modelo()$var.comp), 2)
      Comp <- data.frame(Component = c(names(modelo()$var.comp), "Residual"), Standard_deviation = c(va, VarE))
      v <- as.character(Comp[order(Comp$Standard_deviation, decreasing = FALSE), 1])
      g1 <- ggplot2::ggplot(Comp, ggplot2::aes(x = Component, Standard_deviation)) +
        ggplot2::geom_bar(position = "dodge", stat = "identity") +
        ggplot2::xlab("") +
        ggplot2::ggtitle("Components") +
        ggplot2::theme_bw(base_size = 15) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        ggplot2::scale_x_discrete(limits = v) +
        ggplot2::ylab("Standard Deviation")
      plotly::ggplotly(g1)
    })
  })

  output$trend <- plotly::renderPlotly({
    Model$spatial()

    isolate({
      COL <- obtain.spatialtrend(modelo())[[1]]
      ROW <- obtain.spatialtrend(modelo())[[2]]
      SPATIAL <- obtain.spatialtrend(modelo())$fit
      p <- plotly::plot_ly(x = COL, y = ROW, z = SPATIAL) %>%
        plotly::add_surface() %>%
        plotly::layout(
          title = "Spatial Trend",
          scene = list(
            xaxis = list(title = "Column"),
            yaxis = list(title = "Row"),
            zaxis = list(title = "Z"),
            camera = list(
              eye = list(x = -1.5, y = -1.5, z = 1.2)
            )
          )
        )
      p
    })
  })

  ## to output varcomponents
  variances <- reactive({
    Model$action()
    req(modelo())
    varComp(modelo())
  })

  output$vartable <- DT::renderDataTable(
    if (Model$action() == 0) {
      return()
    } else {
      DT::datatable(
        {
          variances() %>% dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(
          pageLength = 5, lengthMenu = c(1:nrow(variances())),
          columnDefs = list(list(className = "dt-center", targets = 0:ncol(variances())))
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("varcomp_SpATS", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(variances(), file, row.names = FALSE)
    }
  )


  # BOXES

  observeEvent(!Model$inf(), shinyjs::toggle("heritability", anim = TRUE, time = 1, animType = "fade"))
  observeEvent(!Model$inf(), shinyjs::toggle("maxline", anim = TRUE, time = 1, animType = "fade"))
  observeEvent(!Model$inf(), shinyjs::toggle("minline", anim = TRUE, time = 1, animType = "fade"))
  observeEvent(!Model$inf(), shinyjs::toggle("cv", anim = TRUE, time = 1, animType = "fade"))

  output$heritability <- renderbs4ValueBox({
    ran <- modelo()$model$geno$as.random
    validate(need(ran, "Heritability can only be calculated when genotype is random"))
    H <- getHeritability(modelo())
    bs4ValueBox(
      value = H, subtitle = "Heritability",
      icon = shiny::icon("pagelines"),
      color = "info", elevation = 3,
      footer = HTML("<center> 0 = Bad / 1 = Good <center> ")
    )
  })

  observeEvent(Model$res_ran(), { # warning Message heritability
    tryCatch(
      {
        aleatorio <- Model$res_ran()
        if (!isTRUE(aleatorio)) stop("Remember: Heritability can only be calculated when genotype is random")
      },
      error = function(e) {
        shinytoastr::toastr_warning(title = "INFO:", conditionMessage(e), position = "bottom-right")
      }
    )
  })

  output$maxline <- renderbs4ValueBox({
    a <- round(sqrt(modelo()$psi[1]), 2)
    bs4ValueBox(
      value = a, subtitle = "Residual SD",
      icon = shiny::icon("arrow-circle-down", verify_fa = FALSE),
      color = "success", elevation = 3,
      footer = HTML("<center> Looking for low <center>")
    )
  })

  output$minline <- renderbs4ValueBox({
    a <- R.square(modelo())
    bs4ValueBox(
      value = a, subtitle = "R-Square",
      icon = shiny::icon("arrow-circle-up", verify_fa = FALSE),
      color = "danger", elevation = 3,
      footer = HTML("<center> 0 = Bad / 1 = Good  <center>")
    )
  })

  output$cv <- renderbs4ValueBox({
    Model$action()
    isolate({
      m0 <- modelo()
      cv <- CV.spats(m0)
      bs4ValueBox(
        value = paste0(cv, "%"), subtitle = "Coefficient of Variation",
        icon = shiny::icon("arrow-circle-up", verify_fa = FALSE),
        color = "warning", elevation = 3,
        footer = HTML("<center> Looking for low <center>")
      )
    })
  })


  # Download PLOT SPATIAL
  output$descargar <- downloadHandler(
    filename = function() {
      paste("plotSpATS", input$typefile, sep = ".")
    },
    content = function(file) {
      if (input$typefile == "png") {
        grDevices::png(file, width = input$png.wid, height = input$png.hei)
        spaTrend <- ifelse(input$tog_plot == TRUE, "percentage", "raw")
        plot(modelo(), spaTrend = spaTrend, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, axis.args = list(cex.axis = 1.2))
        grDevices::dev.off()
      } else {
        grDevices::pdf(file, width = input$pdf.wid, height = input$pdf.hei)
        spaTrend <- ifelse(input$tog_plot == TRUE, "percentage", "raw")
        plot(modelo(), spaTrend = spaTrend, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, axis.args = list(cex.axis = 1.2))
        grDevices::dev.off()
      }
    }
  )
}

## To be copied in the UI
# mod_info_spats_ui("info_spats_ui_1")

## To be copied in the server
# callModule(mod_info_spats_server, "info_spats_ui_1")
