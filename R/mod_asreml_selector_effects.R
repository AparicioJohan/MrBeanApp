#' asreml_selector_effects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_asreml_selector_effects_ui <- function(id) {
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
        dropdown(
          tags$h4("Select Model."),
          pickerInput(
            inputId = ns("selected"),
            label = "Model",
            choices = 1:32,
            selected = 1,
            options = list(
              title = "Select a model...", size = 5
            )
          ),
          animate = shinyWidgets::animateOptions(
            enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
            exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
          ),
          style = "unite", icon = icon("gear", verify_fa = FALSE),
          status = "warning", width = "300px", label = "Model",
          tooltip = tooltipOptions(title = "Click to select model!")
        ),
        br(),
        shinyjs::hidden(
          div(
            id = ns("third"),
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
                    width = 5
                  ),
                  column(
                    width = 7,
                    fluidRow(
                      bs4TabCard(
                        width = 12, id = "selected_model", maximizable = T, solidHeader = FALSE, closable = F,
                        status = "success", side = "left", type = "tabs",
                        tabPanel(
                          title = "Spatial-Plot", active = T,
                          dropdown(
                            prettyRadioButtons(
                              inputId = ns("typefile"), label = "Filetype for download plot", outline = TRUE, fill = FALSE, shape = "square", inline = TRUE,
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
                            downloadButton(ns("descargar"), "Download the plot",
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
                          shinycssloaders::withSpinner(plotOutput(ns("plot_spats")), type = 5, color = "#28a745"), icon = icon("table-cells", verify_fa = FALSE)
                        ),
                        tabPanel(
                          title = "Diagnostics",
                          prettySwitch(
                            inputId = ns("swicht1"),
                            label = "Residuals",
                            status = "success",
                            slim = FALSE
                          ),
                          shinycssloaders::withSpinner(plotOutput(ns("residuals_aug")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE),
                          strong("Goodness-of-fit Statistics:"),
                          rep_br(2),
                          HTML(
                            paste0("<center>", tableOutput(ns("INFO")), "</center>")
                          ),
                          style = "overflow-x: scroll;"
                        ),
                        tabPanel(
                          title = "Info", icon = icon("signal"),
                          strong("Formula:"),
                          shinycssloaders::withSpinner(verbatimTextOutput(ns("callModel")), type = 6, color = "#28a745"),
                          hr(),
                          strong("Variance Components:"),
                          shinycssloaders::withSpinner(verbatimTextOutput(ns("summ")), type = 6, color = "#28a745"),
                          hr(),
                          strong("ANOVA wald-test:"),
                          shinycssloaders::withSpinner(verbatimTextOutput(ns("aov")), type = 6, color = "#28a745")
                        ),
                        tabPanel(
                          title = "Semi-Variogram",
                          prettySwitch(
                            inputId = ns("swicht2"),
                            label = "Residuals",
                            status = "success",
                            slim = FALSE
                          ),
                          shinycssloaders::withSpinner(plotOutput(ns("semivariogram")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' asreml_selector_effects Server Function
#'
#' @noRd
mod_asreml_selector_effects_server <- function(input, output, session, model) {
  ns <- session$ns

  observe({
    toggle(id = "notfound", condition = is.null(model$modelo()))
    toggle(id = "second", condition = !is.null(model$modelo()))
  })

  observe({
    req(model$modelo())
    # print(model$modelo())
    tmp <- model$modelo()$parms[1:32, ]
    min <- which.max(tmp$A.opt)
    # print(min)
    # print(class(min))
    updatePickerInput(session, inputId = "selected", selected = min)
  })

  observe({
    req(fit_mod())
    toggle(id = "third", condition = !is.null(fit_mod()))
  })


  test <- Waiter$new(
    html = HTML(
      "<center>",
      '<div class="dots-loader"></div>',
      "<br>", "<br>", "<br>", "<br>",
      '<h5 style="font-weight: bold; color: grey;">Fitting Model...</h5>',
      "</center>"
    ),
    color = transparent(0.3)
  )

  fit_mod <- reactive({
    input$selected
    isolate({
      req(model$modelo())
      dt <- model$newData()$data
      comp <- model$newData()$components
      test$show()
      k <- as.numeric(input$selected)

      tryCatch(
        {
          m0 <- spatial.selector(
            data = dt,
            gen = conv_null(comp$genotype),
            block = conv_null(comp$block),
            ibk = conv_null(comp$incomplete),
            row = conv_null(comp$row),
            col = conv_null(comp$column),
            cov1 = conv_null(comp$cov1),
            cov2 = conv_null(comp$cov2),
            resp = conv_null(comp$response),
            type.gen = ifelse(comp$res_ran, "random", "fixed"),
            type.block = comp$type.block,
            nugget = comp$nugget,
            model = k
          )
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:", conditionMessage(e), position = "bottom-full-width",
            showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
          )
          test$hide()
        }
      )

      test$hide()
      if (!exists("m0")) m0 <- NULL

      return(m0)
    })
  })


  # observeEvent(input$selected,{
  #   print(names(fit_mod()))
  #   print(names(fit_mod()$gt))
  #
  # }, ignoreInit = T, ignoreNULL = T)

  observe({
    req(fit_mod())
    toggle(id = "swicht1", condition = length(grep("units", fit_mod()$mod$call)) != 0)
    toggle(id = "swicht2", condition = length(grep("units", fit_mod()$mod$call)) != 0)
  })


  # BLUPS

  BLUPS <- reactive({
    input$selected
    isolate({
      req(fit_mod()$mod)
      fit_mod()$predictions %>%
        dplyr::mutate(
          lower = predicted.value - 1.645 * std.error,
          upper = predicted.value + 1.645 * std.error
        ) %>%
        dplyr::select(-status)
    })
  })

  output$plotblup2 <- plotly::renderPlotly({
    input$selected

    isolate({
      BLUPS <- BLUPS()
      q <- ggplot(BLUPS, aes(x = gen, y = predicted.value))
      v <- as.character(BLUPS[order(BLUPS[, "predicted.value"], decreasing = TRUE), 1])

      p <- q +
        geom_point(size = 1) +
        geom_errorbar(aes(ymax = upper, ymin = lower)) +
        theme_bw() +
        geom_hline(yintercept = mean(BLUPS[, "predicted.value"]), linetype = 2, color = "red") +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        scale_x_discrete(limits = v)
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
      write.csv(BLUPS(), file, row.names = FALSE)
    }
  )

  # Spatial PLOT

  output$plot_spats <- renderPlot({
    input$selected
    isolate({
      req(fit_mod())
      model <- fit_mod()$mod
      spatial.ASReml(model, col = "col", row = "row", response = "resp", genotype = "gen")
    })
  })

  # Diagnostic
  output$residuals_aug <- renderPlot({
    input$selected
    input$swicht1
    isolate({
      req(fit_mod())
      model <- fit_mod()$mod
      plot(model, spatial = ifelse(input$swicht1, "plot", "trend"))
    })
  })

  # Formula

  output$callModel <- renderPrint({
    input$selected
    isolate({
      req(fit_mod())
      model <- fit_mod()$mod
      model$call
    })
  })

  # summary
  output$summ <- renderPrint({
    input$selected
    isolate({
      req(fit_mod())
      model <- fit_mod()$mod
      summary(model)$varcomp %>%
        tibble::rownames_to_column("name") %>%
        dplyr::mutate_if(is.numeric, round, 3)
    })
  })

  # AOV
  output$aov <- renderPrint({
    input$selected
    isolate({
      req(fit_mod())
      model <- fit_mod()$aov
      model %>%
        tibble::rownames_to_column("name") %>%
        dplyr::mutate_if(is.numeric, round, 3)
    })
  })

  # Semi-variogram
  output$semivariogram <- renderPlot({
    input$selected
    input$swicht2
    isolate({
      req(fit_mod())
      model <- fit_mod()$mod
      # DATA <- data.frame(model$mf)
      # DATA$residuals <- residuals(model, spatial =  ifelse(input$swicht2, "plot", "trend"))
      # DATA$col <- as.numeric(DATA$col)
      # DATA$row <- as.numeric(DATA$row)
      # ic <- which(names(DATA)=="col")
      # ir <- which(names(DATA)=="row")
      # ix <- which(names(DATA)=="residuals")
      # DATA <- geoR::as.geodata(DATA, coords.col = c(ic,ir),  data.col = ix)
      # par(mfrow=c(1,2))
      # var1 = geoR::variog(DATA, max.dist=1000)
      # plot(var1)
      # env.var = geoR::variog.mc.env(DATA, obj.v=var1, nsim=100)
      # plot(var1, env=env.var)
      plot(asreml::varioGram.asreml(model,
        spatial = ifelse(input$swicht2, "plot", "trend")
      ),
      main = paste("Empirical Variogram ( Residuals:", ifelse(input$swicht2, "plot", "trend"), ")")
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
        png(file, width = input$png.wid, height = input$png.hei)
        spatial.ASReml(fit_mod()$mod, col = "col", row = "row", response = "resp", genotype = "gen")
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid, height = input$pdf.hei)
        spatial.ASReml(fit_mod()$mod, col = "col", row = "row", response = "resp", genotype = "gen")
        dev.off()
      }
    }
  )


  # Info Criteria
  output$INFO <- function() {
    input$selected
    isolate({
      req(fit_mod())
      gt <- fit_mod()$gt
      gfit <- matrix(NA, ncol = 9, nrow = 1)
      gfit[1, 1] <- c("Model")
      gfit[1, 2] <- round(gt$n.vc, 3)
      gfit[1, 3] <- round(gt$logL, 3)
      gfit[1, 4] <- round(gt$aic, 3)
      gfit[1, 5] <- round(gt$bic, 3)
      gfit[1, 6] <- round(gt$herit.PEV, 3)
      gfit[1, 7] <- round(gt$herit.VC, 3)
      gfit[1, 8] <- round(gt$Aopt, 3)
      gfit[1, 9] <- round(gt$Dopt, 3)

      colnames(gfit) <- c("MODEL", "n.VC", "logL", "AIC", "BIC", "herit-PEV", "herit-VC", "A-opt", "D-opt")
      gfit <- data.frame(gfit)
      gfit %>%
        dplyr::select(MODEL, everything()) %>%
        kableExtra::kable(escape = F, align = "c") %>%
        kableExtra::kable_styling(c("hover", "responsive", "condensed"), full_width = T, position = "center")
    })
  }
}

## To be copied in the UI
# mod_asreml_selector_effects_ui("asreml_selector_effects_ui_1")

## To be copied in the server
# callModule(mod_asreml_selector_effects_server, "asreml_selector_effects_ui_1")
