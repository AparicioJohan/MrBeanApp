#' MET_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MET_results_ui <- function(id) {
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
            6,
            fluidRow(
              bs4TabCard(
                width = 12, id = "MET_as", maximizable = T, solidHeader = FALSE, closable = F,
                status = "success", side = "left", type = "tabs",
                tabPanel(
                  title = "Correlation Matrix", active = T,
                  fluidRow(
                    col_3(
                      dropdown(
                        prettyRadioButtons(
                          inputId = ns("filetype"), label = "Download Plot File Type", outline = TRUE, fill = FALSE, shape = "square", inline = TRUE,
                          choices = list(PNG = "png", PDF = "pdf"),
                          icon = icon("check"), animation = "tada"
                        ),
                        conditionalPanel(
                          condition = "input.filetype=='png'", ns = ns,
                          sliderInput(inputId = ns("png.wid.corr"), min = 200, max = 2000, value = 900, label = "Width pixels"),
                          sliderInput(inputId = ns("png.hei.corr"), min = 200, max = 2000, value = 600, label = "Height pixels")
                        ),
                        conditionalPanel(
                          condition = "input.filetype=='pdf'", ns = ns,
                          sliderInput(inputId = ns("pdf.wid.corr"), min = 2, max = 20, value = 10, label = "Width"),
                          sliderInput(inputId = ns("pdf.hei.corr"), min = 2, max = 20, value = 8, label = "Height")
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
                        tooltip = tooltipOptions(title = "Click to Download!"),
                        status = "warning", width = "300px"
                      )
                    ),
                    col_6(
                      # sliderTextInput(
                      #   inputId = ns("size_corr"), label = "Choose a size:",
                      #   choices = c(1,1.5, 2.0 , 2.5, 3, 3.5, 4 , 4.5, 5, 5.5),
                      #   grid = TRUE, selected = 4, width = "100%"
                      # )
                      sliderInput(
                        inputId = ns("size_corr"),
                        label = "Choose a size:",
                        min = 1, max = 5.5, value = 4, step = 0.5,
                        width = "100%"
                      )
                    ),
                    col_3()
                  ),
                  shinycssloaders::withSpinner(plotOutput(ns("corr_MAT")), type = 5, color = "#28a745"), icon = icon("table-cells", verify_fa = FALSE),
                  downloadButton(ns("downloadCorr"),
                    label = "Download Matrix",
                    class = "btn-success",
                    style = " color: white ; background-color: #28a745"
                  ),
                  fluidRow(
                    col_3(),
                    col_6(
                      actionBttn(inputId = ns("dendo"), label = "Dendrogram", style = "jelly", color = "warning", block = T, icon = icon("check"))
                    ),
                    col_3()
                  )
                ),
                tabPanel(
                  title = "Covariance Matrix", icon = icon("signal"),
                  fluidRow(
                    col_3(
                      dropdown(
                        prettyRadioButtons(
                          inputId = ns("filetype2"), label = "Download Plot File Type", outline = TRUE, fill = FALSE, shape = "square", inline = TRUE,
                          choices = list(PNG = "png", PDF = "pdf"),
                          icon = icon("check"), animation = "tada"
                        ),
                        conditionalPanel(
                          condition = "input.filetype2=='png'", ns = ns,
                          sliderInput(inputId = ns("png.wid.cov"), min = 200, max = 2000, value = 900, label = "Width pixels"),
                          sliderInput(inputId = ns("png.hei.cov"), min = 200, max = 2000, value = 600, label = "Height pixels")
                        ),
                        conditionalPanel(
                          condition = "input.filetype2=='pdf'", ns = ns,
                          sliderInput(inputId = ns("pdf.wid.cov"), min = 2, max = 20, value = 10, label = "Width"),
                          sliderInput(inputId = ns("pdf.hei.cov"), min = 2, max = 20, value = 8, label = "Height")
                        ),
                        downloadButton(ns("descargar2"), "Download Plot",
                          class = "btn-success",
                          style = " color: white ; background-color: #28a745"
                        ), br(),
                        animate = shinyWidgets::animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                          exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
                        ),
                        style = "unite", icon = icon("gear", verify_fa = FALSE),
                        tooltip = tooltipOptions(title = "Click to Download!"),
                        status = "warning", width = "300px"
                      )
                    ),
                    col_6(
                      # sliderTextInput(
                      #   inputId = ns("size_gmat"),label = "Choose a size:",
                      #   choices = c(1,1.5, 2.0 , 2.5, 3, 3.5, 4 , 4.5, 5, 5.5),
                      #   grid = TRUE, selected = 2.5, width = "100%"
                      #   )
                      sliderInput(
                        inputId = ns("size_gmat"),
                        label = "Choose a size:",
                        min = 1, max = 5.5, value = 2.5, step = 0.5,
                        width = "100%"
                      )
                    ),
                    col_3()
                  ),
                  shinycssloaders::withSpinner(plotOutput(ns("g_MAT")), type = 5, color = "#28a745"),
                  downloadButton(ns("downloadCov"),
                    label = "Download Matrix",
                    class = "btn-success",
                    style = " color: white ; background-color: #28a745"
                  )
                )
              )
            ),
            fluidRow(
              bs4TabCard(
                width = 12, id = "MET_summ", maximizable = T, solidHeader = FALSE, closable = F,
                status = "success", side = "left", type = "tabs",
                tabPanel(
                  title = "Residuals", active = T,
                  shinycssloaders::withSpinner(plotOutput(ns("residuals_aug")), type = 6, color = "#28a745"),
                  icon = icon("table-cells", verify_fa = FALSE)
                ),
                tabPanel(
                  title = "Information", icon = icon("signal"),
                  strong("Formula:"),
                  shinycssloaders::withSpinner(verbatimTextOutput(ns("formula")), type = 6, color = "#28a745"),
                  hr(),
                  strong("Variance Components:"),
                  shinycssloaders::withSpinner(verbatimTextOutput(ns("summ")), type = 6, color = "#28a745")
                )
              )
            )
          ),
          column(
            6,
            fluidRow(
              bs4Card(
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("blups")),
                  type = 5, color = "#28a745"
                ),
                width = 12, title = tagList(icon = icon("chart-line"), "Predictions"),
                status = "success", solidHeader = FALSE, maximizable = T,
                downloadButton(ns("downloadData2"),
                  label = "Download Table",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745"
                ),
                downloadButton(ns("downloadData3"),
                  "Overall",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745"
                ),
                actionBttn(
                  inputId = ns("pca"),
                  label = "PCA",
                  style = "minimal",
                  color = "success",
                  icon = icon("chart-pie")
                ),
                actionBttn(
                  inputId = ns("preds"),
                  label = "GxE",
                  style = "minimal",
                  color = "success",
                  icon = icon("check")
                ),
                actionBttn(
                  inputId = ns("FA"),
                  label = "Factor Analytic",
                  style = "minimal",
                  color = "success",
                  icon = icon("chart-bar")
                )
              )
            ),
            fluidRow(
              bs4Card(
                fluidRow(
                  col_6(
                    pickerInput(
                      inputId = ns("mod1"),
                      label = "Model 1",
                      choices = list(
                        diag = "diag", corv = "corv",
                        corh = "corh", fa1 = "fa1", fa2 = "fa2", fa3 = "fa3", fa4 = "fa4", corgh = "corgh"
                      ),
                      options = list(
                        title = "Please select a model ...", size = 5
                      ), width = "100%"
                    )
                  ),
                  col_6(
                    pickerInput(
                      inputId = ns("mod2"),
                      label = "Model 2",
                      choices = list(
                        diag = "diag", corv = "corv",
                        corh = "corh", fa1 = "fa1", fa2 = "fa2", fa3 = "fa3", fa4 = "fa4", corgh = "corgh"
                      ),
                      options = list(
                        title = "Please select a model ...", size = 5
                      ), width = "100%"
                    )
                  )
                ),
                fluidRow(
                  col_3(),
                  col_6(
                    actionBttn(inputId = ns("comparison"), label = "Compare!", style = "jelly", color = "success", block = T, icon = icon("check"))
                  ),
                  col_3()
                ),
                DT::dataTableOutput(ns("anovamix")),
                width = 12, title = tagList(icon = icon("exchange-alt", verify_fa = FALSE), "Comparison"), status = "success", solidHeader = FALSE
              )
            )
          )
        )
        # fluidRow(
        #   column(12,
        #          fluidRow(
        #            bs4Card(
        #              shinycssloaders::withSpinner(
        #                plotly::plotlyOutput(ns("plotblup2")),
        #                type = 5,color = "#28a745"
        #              ),
        #              title = "Genotype Effects", solidHeader =  TRUE,status = "success",width = 12,collapsed = F)
        #          )
        #   )
        # )
      )
    )
  )
}

#' MET_results Server Function
#'
#' @noRd
mod_MET_results_server <- function(input, output, session, model) {
  ns <- session$ns

  observeEvent(model$run(), {
    toggle(id = "notfound", condition = is.null(model$model()))
    toggle(id = "second", condition = !is.null(model$model()))
  })

  output$corr_MAT <- renderPlot({
    model$run()
    input$size_corr
    isolate({
      req(model$model())
      m <- model$model()
      covariance_asreml(round(m$corrM, 2), corr = T, size = input$size_corr)
    })
  })

  # Download corrPLOT
  output$descargar <- downloadHandler(
    filename = function() {
      paste("corrPlot", input$filetype, sep = ".")
    },
    content = function(file) {
      if (input$filetype == "png") {
        png(file, width = input$png.wid.corr, height = input$png.hei.corr)
        req(model$model())
        m <- model$model()
        print(covariance_asreml(round(m$corrM, 2), corr = T, size = input$size_corr))
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid.corr, height = input$pdf.hei.corr)
        req(model$model())
        m <- model$model()
        print(covariance_asreml(round(m$corrM, 2), corr = T, size = input$size_corr))
        dev.off()
      }
    }
  )

  # Download CorrMat
  output$downloadCorr <- downloadHandler(
    filename = function() {
      paste("CORR", ".csv", sep = "")
    },
    content = function(file) {
      req(model$model())
      m <- model$model()
      write.csv(m$corrM, file, row.names = TRUE)
    }
  )


  output$g_MAT <- renderPlot({
    model$run()
    input$size_gmat
    isolate({
      req(model$model())
      m <- model$model()
      covariance_asreml(round(m$vcovM, 2), corr = F, size = input$size_gmat)
    })
  })

  # Download covPLOT
  output$descargar2 <- downloadHandler(
    filename = function() {
      paste("covPlot", input$filetype2, sep = ".")
    },
    content = function(file) {
      if (input$filetype2 == "png") {
        png(file, width = input$png.wid.cov, height = input$png.hei.cov)
        req(model$model())
        m <- model$model()
        print(covariance_asreml(round(m$vcovM, 2), corr = F, size = input$size_gmat))
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid.cov, height = input$pdf.hei.cov)
        req(model$model())
        m <- model$model()
        print(covariance_asreml(round(m$vcovM, 2), corr = F, size = input$size_gmat))
        dev.off()
      }
    }
  )

  # Download COVmat
  output$downloadCov <- downloadHandler(
    filename = function() {
      paste("COV", ".csv", sep = "")
    },
    content = function(file) {
      req(model$model())
      m <- model$model()
      write.csv(m$vcovM, file, row.names = TRUE)
    }
  )


  output$residuals_aug <- renderPlot({
    model$run()
    isolate({
      req(model$model())
      model <- model$model()$mod
      plot(model)
    })
  })

  # Formula

  output$formula <- renderPrint({
    model$run()
    isolate({
      req(model$model())
      model <- model$model()$mod
      model$call
    })
  })

  output$summ <- renderPrint({
    model$run()
    isolate({
      req(model$model())
      model <- model$model()$mod
      summary(model)$varcomp %>%
        tibble::rownames_to_column("name") %>%
        dplyr::mutate_if(is.numeric, round, 3) %>%
        dplyr::select(1:4)
    })
  })


  output$blups <- DT::renderDataTable({
    model$run()
    isolate({
      req(model$model())
      bl <- model$model()$predictions %>% dplyr::select(-status)
      DT::datatable(
        {
          bl %>% dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(pageLength = 10, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:ncol(bl)))),
        filter = "top",
        selection = "multiple"
      )
    })
  })

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("predictions_2stage_ASReml_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model$model()$predictions, file, row.names = FALSE)
    }
  )

  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("Overall_predictions_2stage", ".csv", sep = "")
    },
    content = function(file) {
      req(model$model())
      # bl <- model$model()$predictions %>% dplyr::select(-status) %>% group_by(gen) %>% dplyr::summarise(Overall = mean(predicted.value, na.rm = T))
      bl <- model$model()$overall %>% dplyr::select(-status)
      write.csv(bl, file, row.names = FALSE)
    }
  )

  # Model comparison

  test <- Waiter$new(
    html = HTML(
      "<center>",
      '<div class="dots-loader"></div>',
      "<br>", "<br>", "<br>", "<br>",
      '<h5 style="font-weight: bold; color: grey;">Comparing  Models...</h5>',
      "</center>"
    ),
    color = transparent(0.3)
  )

  loglike <- reactive({
    input$comparison
    isolate({
      req(model$datafilter())
      dt <- model$datafilter()$data
      comp <- model$datafilter()
      req(input$mod1)
      req(input$mod2)
      k <- input$mod1
      j <- input$mod2
      validate(need(k != j, "You are comparing the same model"))
      test$show()

      tryCatch(
        {
          m0 <- stageMET(
            data = dt,
            gen = conv_null(comp$gen),
            trial = conv_null(comp$trial),
            resp = conv_null(comp$resp),
            type.gen = ifelse(comp$type.gen, "random", "fixed"),
            type.trial = ifelse(comp$type.trial, "random", "fixed"),
            weight = conv_null(comp$weight),
            vc.model = k
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

      tryCatch(
        {
          m1 <- stageMET(
            data = dt,
            gen = conv_null(comp$gen),
            trial = conv_null(comp$trial),
            resp = conv_null(comp$resp),
            type.gen = ifelse(comp$type.gen, "random", "fixed"),
            type.trial = ifelse(comp$type.trial, "random", "fixed"),
            weight = conv_null(comp$weight),
            vc.model = j
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
      if (!exists("m1")) m1 <- NULL

      result <- test_lrt_2stage(m0 = m0, m1 = m1)

      return(result)
    })
  })

  output$anovamix <- DT::renderDataTable({
    input$comparison
    isolate({
      req(loglike())
      DT::datatable(
        {
          loglike()
        },
        options = list(paging = FALSE, searching = FALSE, scrollX = TRUE)
      )
    })
  })


  # dendo ---------

  observeEvent(model$run(),
    {
      toggle(id = "dendo", condition = length(grep("fa", model$model()$mod$call)) != 0 | length(grep("corgh", model$model()$mod$call)) != 0)
    },
    ignoreInit = T,
    ignoreNULL = T
  )

  output$plot_dend <- renderPlot({
    input$dendo
    input$box
    input$horiz
    input$size_dendo
    input$size_line
    input$num_k
    isolate({
      req(model$model())
      m <- model$model()
      corr <- m$corrM
      validate(need(input$num_k <= ncol(corr), "The number of clusters should be less"))
      res <- factoextra::hcut(corr, k = input$num_k, stand = FALSE)
      dend <- factoextra::fviz_dend(res,
        rect = input$box,
        cex = input$size_dendo,
        lwd = input$size_line, main = "Cluster Dendrogram",
        horiz = input$horiz
      )
      dend
    })
  })

  observeEvent(input$dendo,
    {
      showModal(modalDialog(
        title = "Dendrogram", size = "l", easyClose = T,
        dropdown(
          prettyRadioButtons(
            inputId = ns("filetype3"), label = "Download Plot File Type", outline = TRUE, fill = FALSE, shape = "square", inline = TRUE,
            choices = list(PNG = "png", PDF = "pdf"),
            icon = icon("check"), animation = "tada"
          ),
          conditionalPanel(
            condition = "input.filetype3=='png'", ns = ns,
            sliderInput(inputId = ns("png.wid.den"), min = 200, max = 2000, value = 900, label = "Width pixels"),
            sliderInput(inputId = ns("png.hei.den"), min = 200, max = 2000, value = 600, label = "Height pixels")
          ),
          conditionalPanel(
            condition = "input.filetype3=='pdf'", ns = ns,
            sliderInput(inputId = ns("pdf.wid.den"), min = 2, max = 20, value = 10, label = "Width"),
            sliderInput(inputId = ns("pdf.hei.den"), min = 2, max = 20, value = 8, label = "Height")
          ),
          downloadButton(ns("descargar3"), "Download Plot",
            class = "btn-success",
            style = " color: white ; background-color: #28a745"
          ), br(),
          animate = shinyWidgets::animateOptions(
            enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
            exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
          ),
          style = "unite", icon = icon("gear", verify_fa = FALSE),
          tooltip = tooltipOptions(title = "Click to Download!"),
          status = "warning", width = "300px"
        ),
        shinycssloaders::withSpinner(plotOutput(ns("plot_dend")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE),
        br(),
        strong("Configuration plot:"),
        fluidRow(
          col_2(),
          col_4(
            switchInput(
              inputId = ns("box"),
              label = "Boxes?",
              labelWidth = "100%",
              onStatus = "success",
              offStatus = "danger",
              width = "100%", value = TRUE
            )
          ),
          col_4(
            switchInput(
              inputId = ns("horiz"),
              label = "Horizontal?",
              labelWidth = "100%",
              onStatus = "success",
              offStatus = "danger",
              width = "100%", value = TRUE
            )
          ),
          col_2(),
        ),
        fluidRow(
          col_4(
            sliderTextInput(
              inputId = ns("num_k"), label = "Clusters:",
              choices = c(2, 3, 4, 5),
              grid = TRUE, selected = 2, width = "100%"
            )
          ),
          col_4(
            sliderTextInput(
              inputId = ns("size_dendo"), label = "Letter size:",
              choices = c(0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 2.5),
              grid = TRUE, selected = 1, width = "100%"
            )
          ),
          col_4(
            sliderTextInput(
              inputId = ns("size_line"), label = "Line size:",
              choices = c(0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 2.5),
              grid = TRUE, selected = 0.8, width = "100%"
            )
          )
        )
      ))
    },
    ignoreInit = T,
    ignoreNULL = T
  )


  # Download Dendrogram
  output$descargar3 <- downloadHandler(
    filename = function() {
      paste("dendrogram", input$filetype3, sep = ".")
    },
    content = function(file) {
      if (input$filetype3 == "png") {
        png(file, width = input$png.wid.den, height = input$png.hei.den)
        req(model$model())
        m <- model$model()
        corr <- m$corrM
        validate(need(input$num_k <= ncol(corr), "The number of clusters should be less"))
        res <- factoextra::hcut(corr, k = input$num_k, stand = FALSE)
        dend <- factoextra::fviz_dend(res,
          rect = input$box,
          cex = input$size_dendo,
          lwd = input$size_line, main = "Cluster Dendrogram",
          horiz = input$horiz
        )
        print(dend)
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid.den, height = input$pdf.hei.den)
        req(model$model())
        m <- model$model()
        corr <- m$corrM
        validate(need(input$num_k <= ncol(corr), "The number of clusters should be less"))
        res <- factoextra::hcut(corr, k = input$num_k, stand = FALSE)
        dend <- factoextra::fviz_dend(res,
          rect = input$box,
          cex = input$size_dendo,
          lwd = input$size_line, main = "Cluster Dendrogram",
          horiz = input$horiz
        )
        print(dend)
        dev.off()
      }
    }
  )


  # PCA


  output$plot_pca <- renderPlot({
    input$pca
    input$type
    input$number
    input$ind_pca
    input$var_pca
    input$scale
    input$invisible_pca
    isolate({
      req(model$model())
      m <- model$model()
      data <- model$model()$predictions[, 1:3] %>%
        tidyr::spread(., "trial", "predicted.value") %>%
        tibble::column_to_rownames("gen")

      res.pca <- prcomp(data, scale. = T)

      if (input$type == "var") {
        res.pca.non <- prcomp(data, scale. = input$scale)
        factoextra::fviz_pca_var(res.pca.non, col.var = "steelblue", repel = T, alpha.var = 0.2)
      } else if (input$type == "ind") {
        top <- as.numeric(input$number)
        req(top <= nrow(data))
        fa12_scores <- res.pca$x[, 1:2] %>%
          data.frame() %>%
          tibble::rownames_to_column("Genotypes")
        fa12_scores$Score <- sqrt(fa12_scores$PC1^2 + fa12_scores$PC2^2)
        gen <- fa12_scores %>%
          dplyr::arrange(desc(Score)) %>%
          dplyr::top_n(top) %>%
          dplyr::pull(Genotypes)
        factoextra::fviz_pca_ind(res.pca, repel = T, alpha.ind = 0.5, select.ind = list(name = gen), labelsize = 4)
      } else {
        geom.ind <- c("point", "text")
        geom.var <- c("arrow", "text")
        invisible <- "none"

        if (!is.null(input$ind_pca)) geom.ind <- input$ind_pca
        if (!is.null(input$var_pca)) geom.var <- input$var_pca
        if (!is.null(input$invisible_pca)) invisible <- input$invisible_pca

        factoextra::fviz_pca_biplot(res.pca, repel = F, alpha.ind = 0.5, geom.ind = geom.ind, geom.var = geom.var, invisible = invisible)
      }
    })
  })




  observeEvent(input$pca,
    {
      showModal(modalDialog(
        title = tagList(icon = icon("chart-pie"), "PCA"), size = "l", easyClose = T,
        prettyRadioButtons(
          inputId = ns("type"),
          label = "Choose:",
          choices = c("Biplot" = "bip", "Variables" = "var", "Individuals" = "ind"),
          selected = "bip",
          inline = TRUE,
          status = "danger",
          fill = TRUE,
          icon = icon("check"), animation = "jelly"
        ),
        shinycssloaders::withSpinner(plotOutput(ns("plot_pca")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE),
        conditionalPanel(
          condition = "input.type=='ind'", ns = ns,
          fluidRow(
            col_4(),
            col_4(
              pickerInput(
                inputId = ns("number"),
                label = "Top (n)",
                choices = 4:100, selected = 20,
                options = list(
                  size = 5
                )
              )
            ),
            col_4()
          )
        ),
        conditionalPanel(
          condition = "input.type=='var'", ns = ns,
          fluidRow(
            col_4(),
            col_4(
              switchInput(
                inputId = ns("scale"),
                label = "Scale?",
                labelWidth = "100%",
                onStatus = "success",
                offStatus = "danger",
                width = "100%", value = TRUE
              )
            ),
            col_4()
          )
        ),
        conditionalPanel(
          condition = "input.type=='bip'", ns = ns,
          fluidRow(
            col_1(),
            col_3(
              checkboxGroupButtons(
                inputId = ns("ind_pca"),
                label = "Individuals",
                choices = c("point", "text"), justified = T,
                status = "success",
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"),
                  no = icon("remove", lib = "glyphicon")
                )
              )
            ),
            col_3(
              checkboxGroupButtons(
                inputId = ns("var_pca"),
                label = "Variables",
                choices = c("arrow", "text"), justified = T,
                status = "success",
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"),
                  no = icon("remove", lib = "glyphicon")
                )
              )
            ),
            col_3(
              checkboxGroupButtons(
                inputId = ns("invisible_pca"),
                label = "Invisible",
                choices = c("ind", "var"), justified = T,
                status = "success",
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"),
                  no = icon("remove", lib = "glyphicon")
                )
              )
            )
          )
        )
        # br()
      ))
    },
    ignoreInit = T,
    ignoreNULL = T
  )


  output$gxe <- echarts4r::renderEcharts4r({
    input$preds
    isolate({
      req(model$model())
      m <- model$model()
      bm <- model$model()$predictions %>%
        dplyr::group_by(trial) %>%
        dplyr::mutate(predicted.value = predicted.value - mean(predicted.value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          lower = predicted.value - 1.645 * std.error,
          upper = predicted.value + 1.645 * std.error
        )

      ll <- lapply(unique(bm$gen), function(x) list(text = paste0("Genotype: ", unique(x)), subtext = "Interaction GxE"))

      p1 <- bm %>%
        dplyr::group_by(gen) %>%
        echarts4r::e_charts(trial, timeline = TRUE) %>%
        echarts4r::e_scatter(predicted.value, bind = gen, symbol_size = 15) %>%
        echarts4r::e_tooltip() %>%
        echarts4r::e_error_bar(lower = lower, upper = upper) %>%
        echarts4r::e_toolbox_feature(feature = "dataView") %>%
        echarts4r::e_toolbox_feature(feature = "dataZoom") %>%
        echarts4r::e_legend(show = T, type = "scroll") %>%
        echarts4r::e_title(text = "BLUP genotype by environment") %>%
        echarts4r::e_timeline_serie(title = ll)
      p1
    })
  })

  observeEvent(input$preds, {
    req(model$model())
    bm <- model$model()$predictions
    lvl <- as.character(unique(bm$gen))
    updatePickerInput(session = session, inputId = "geno_filter", choices = lvl)
  })

  observeEvent(input$search, {
    req(model$model())
    req(input$geno_filter)
    bm <- model$model()$predictions
    a <- unique(bm$gen)
    b <- 0:length(a)
    names(b) <- a
    selected <- as.numeric(b[input$geno_filter])

    echarts4r::echarts4rProxy(ns("gxe")) %>%
      e_dispatch_action_p("timelineChange", currentIndex = selected)
  })

  observeEvent(input$preds,
    {
      showModal(modalDialog(
        title = tagList(icon = icon("chart-bar"), "GxE"), size = "l", easyClose = T,
        pickerInput(
          inputId = ns("geno_filter"),
          label = "Search Genotype:",
          choices = "",
          options = list(
            `live-search` = TRUE, size = 5
          ), width = "100%"
        ),
        actionBttn(
          inputId = ns("search"),
          label = "Search",
          style = "fill",
          color = "primary", size = "sm"
        ), rep_br(2),
        shinycssloaders::withSpinner(
          echarts4r::echarts4rOutput(ns("gxe")),
          type = 6, color = "#28a745"
        )
      ))
    },
    ignoreInit = T,
    ignoreNULL = T
  )



  observeEvent(model$run(),
    {
      toggle(id = "FA", condition = length(grep("fa", model$model()$mod$call)) != 0)
    },
    ignoreInit = T,
    ignoreNULL = T
  )


  return(reactive(input$FA))
}

## To be copied in the UI
# mod_MET_results_ui("MET_results_ui_1")

## To be copied in the server
# callModule(mod_MET_results_server, "MET_results_ui_1")
