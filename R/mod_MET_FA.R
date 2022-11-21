#' MET_FA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MET_FA_ui <- function(id) {
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
                width = 12, id = "MET_fa", maximizable = T, solidHeader = FALSE, closable = F,
                status = "success", side = "left", type = "tabs",
                tabPanel(
                  title = "Genotypic Variance", active = T,
                  shinycssloaders::withSpinner(
                    echarts4r::echarts4rOutput(ns("g1")),
                    type = 5, color = "#28a745"
                  )
                  # fluidRow(
                  #   col_3(),
                  #   col_6(
                  #     actionBttn(inputId = ns("dendo"),label = "Dendrogram",style = "jelly",color = "warning",block = T, icon = icon("check") )
                  #   ),
                  #   col_3()
                  # )
                ),
                tabPanel(
                  title = "Variance Explained", icon = icon("signal"),
                  shinycssloaders::withSpinner(
                    echarts4r::echarts4rOutput(ns("g2")),
                    type = 5, color = "#28a745"
                  )
                )
              )
            ),
            fluidRow(
              bs4Card(
                pickerInput(
                  inputId = ns("geno_filter2"),
                  label = "Search Genotype:",
                  choices = "",
                  options = list(
                    `live-search` = TRUE, size = 5
                  ), width = "100%"
                ),
                actionBttn(
                  inputId = ns("search2"),
                  label = "Search",
                  style = "fill",
                  color = "primary", size = "sm"
                ), rep_br(2),
                shinycssloaders::withSpinner(
                  echarts4r::echarts4rOutput(ns("g3")),
                  type = 5, color = "#28a745"
                ),
                br(),
                downloadButton(ns("downloadSC"),
                  "Download Scores",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745; float:left"
                ),
                width = 12, title = tagList(icon = icon("exchange-alt", verify_fa = FALSE), "Scores"),
                status = "success", solidHeader = FALSE, maximizable = T
              )
            )
          ),
          column(
            6,
            fluidRow(
              bs4Card(
                shinycssloaders::withSpinner(
                  echarts4r::echarts4rOutput(ns("g4")),
                  type = 5, color = "#28a745"
                ),
                br(),
                actionBttn(
                  inputId = ns("lat"),
                  label = "Latent Regression",
                  style = "minimal",
                  color = "success",
                  icon = icon("chart-pie")
                ),
                width = 12, title = tagList(icon = icon("chart-line"), "Total Variance"),
                status = "success", solidHeader = FALSE, maximizable = T
              )
            ),
            fluidRow(
              bs4Card(
                pickerInput(
                  inputId = ns("trial_filter"),
                  label = "Search Trial:",
                  choices = "",
                  options = list(
                    `live-search` = TRUE, size = 5
                  ), width = "100%"
                ),
                actionBttn(
                  inputId = ns("search3"),
                  label = "Search",
                  style = "fill",
                  color = "primary", size = "sm"
                ), rep_br(2),
                shinycssloaders::withSpinner(
                  echarts4r::echarts4rOutput(ns("g5")),
                  type = 5, color = "#28a745"
                ),
                br(),
                downloadButton(ns("downloadLO"),
                  "Download Loadings",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745; float:left"
                ),
                width = 12, title = tagList(icon = icon("exchange-alt", verify_fa = FALSE), "Loadings"),
                status = "success", solidHeader = FALSE, maximizable = T
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

#' MET_FA Server Function
#'
#' @noRd
mod_MET_FA_server <- function(input, output, session, model) {
  ns <- session$ns

  # observeEvent(model$run(),{
  #   toggle(id = "notfound", condition = is.null(model$model()) )
  #   toggle(id = "second", condition = !is.null(model$model()) )
  # })

  observeEvent(model$run(),
    {
      toggle(id = "notfound", condition = length(grep("fa", model$model()$mod$call)) == 0)
      toggle(id = "second", condition = length(grep("fa", model$model()$mod$call)) != 0)
    },
    ignoreInit = T,
    ignoreNULL = T
  )


  # Variance by Location
  output$g1 <- echarts4r::renderEcharts4r({
    req(model$model())
    mod <- model$model()$mod
    req(length(grep("fa", model$model()$mod$call)) != 0)
    ASM <- try(fa.asreml(mod, trunc.char = NULL), silent = T)
    if (class(ASM) == "try-error") {
      return()
    }
    genVarChart(mod, height = "65%")
  })

  # Variance Explained by Location
  output$g2 <- echarts4r::renderEcharts4r({
    req(model$model())
    req(length(grep("fa", model$model()$mod$call)) != 0)
    mod <- model$model()$mod
    varExpChart(mod, height = "65%")
  })


  # Score Plot
  observeEvent(model$run(),
    {
      req(model$model())
      req(length(grep("fa", model$model()$mod$call)) != 0)
      mod <- model$model()$mod
      sc <- scoreChart(mod, x = "Comp1", y = "Comp2", plot = F)
      inx <- as.character(unique(sc$gen))
      updatePickerInput(session = session, inputId = "geno_filter2", choices = inx)
    },
    ignoreInit = T,
    ignoreNULL = T
  )

  output$g3 <- echarts4r::renderEcharts4r({
    req(model$model())
    req(length(grep("fa", model$model()$mod$call)) != 0)
    input$search2
    isolate({
      req(input$geno_filter2)
      mod <- model$model()$mod
      scoreChart(mod, x = "Comp1", y = "Comp2", gen = input$geno_filter2)
    })
  })

  # Total Variance Explained
  output$g4 <- echarts4r::renderEcharts4r({
    req(model$model())
    req(length(grep("fa", model$model()$mod$call)) != 0)
    mod <- model$model()$mod
    waterChart(mod)
  })


  # Loading Plot

  observeEvent(model$run(),
    {
      req(model$model())
      req(length(grep("fa", model$model()$mod$call)) != 0)
      mod <- model$model()$mod
      ASM <- fa.asreml(mod, trunc.char = NULL)
      ASM <- ASM$gammas[[1]]$`rotated loads`
      inx <- rownames(ASM)
      updatePickerInput(session = session, inputId = "trial_filter", choices = inx)
    },
    ignoreInit = T,
    ignoreNULL = T
  )

  output$g5 <- echarts4r::renderEcharts4r({
    req(model$model())
    req(length(grep("fa", model$model()$mod$call)) != 0)
    input$search3
    isolate({
      req(input$trial_filter)
      mod <- model$model()$mod
      loadChart(mod, x = "fac_1", y = "fac_2", trial_selected = input$trial_filter)
    })
  })


  observeEvent(input$lat, {
    req(model$model())
    bm <- model$model()$predictions
    lvl <- as.character(unique(bm$gen))
    updatePickerInput(session = session, inputId = "geno_filter", choices = lvl, selected = lvl[1:2])
  })

  output$regress <- renderPlot({
    # input$lat
    input$search
    input$scale
    input$text_lab
    input$alpha_p
    input$size_text
    input$alpha_text
    isolate({
      req(model$model())
      req(length(grep("fa", model$model()$mod$call)) != 0)
      validate(need(input$geno_filter, message = "Please select at least one genotype."))
      mod <- model$model()$mod
      gen <- input$geno_filter
      latent_regress(mod, gen, input$scale, input$text_lab, input$size_text, input$alpha_text, input$alpha_p)
    })
  })


  observeEvent(input$lat,
    {
      showModal(modalDialog(
        title = tagList(icon = icon("chart-bar"), "Latent Regression"), size = "l", easyClose = T,
        pickerInput(
          inputId = ns("geno_filter"),
          label = "Search Genotype:",
          choices = "", multiple = T,
          options = list(
            `live-search` = TRUE, `actions-box` = TRUE, size = 5
          ), width = "100%"
        ),
        actionBttn(
          inputId = ns("search"),
          label = "Search",
          style = "fill",
          color = "primary", size = "sm"
        ), rep_br(2),
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
        ),
        shinycssloaders::withSpinner(
          plotOutput(ns("regress")),
          type = 6, color = "#28a745"
        ),
        fluidRow(
          col_3(),
          col_3(
            switchInput(
              inputId = ns("scale"),
              label = "Fixed Scale",
              labelWidth = "100%",
              onStatus = "success",
              offStatus = "danger",
              width = "100%", value = TRUE
            )
          ),
          col_3(
            switchInput(
              inputId = ns("text_lab"),
              label = "Labels",
              labelWidth = "100%",
              onStatus = "success",
              offStatus = "danger",
              width = "100%", value = F
            )
          )
        ),
        fluidRow(
          # col_3(),
          col_4(
            # sliderTextInput(
            #   inputId = ns("alpha_p"), label = "Opacity Point:",
            #   choices = seq(0.05,1, by = 0.1),
            #   grid = TRUE, selected = 0.55, width = "100%"
            # )
            sliderInput(
              inputId = ns("alpha_p"),
              label = "Opacity Point:",
              min = 0.05, max = 0.95, value = 0.55, step = 0.10,
              width = "100%"
            )
          ),
          col_4(
            # sliderTextInput(
            #   inputId = ns("size_text"), label = "Size Text:",
            #   choices = seq(1,8, by = 0.2),
            #   grid = TRUE, selected = 4, width = "100%"
            # )
            sliderInput(
              inputId = ns("size_text"),
              label = "Size Text:",
              min = 1, max = 8, value = 4, step = 0.2,
              width = "100%"
            )
          ),
          col_4(
            # sliderTextInput(
            #   inputId = ns("alpha_text"), label = "Opacity Text:",
            #   choices = seq(0.1,1, by = 0.1),
            #   grid = TRUE, selected = 0.7, width = "100%"
            # )
            sliderInput(
              inputId = ns("alpha_text"),
              label = "Opacity Text:",
              min = 0.1, max = 1, value = 0.7, step = 0.1,
              width = "100%"
            )
          )
          # col_3()
        ),
        footer = tagList(
          downloadButton(ns("downloadFA"),
            "Download Table",
            class = "btn-success",
            style = " color: white ; background-color: #28a745; float:left"
          ),
          modalButton("Cancel")
        )
      ))
    },
    ignoreInit = T,
    ignoreNULL = T
  )



  # Downloads ---------------------------------------------------------------


  output$descargar <- downloadHandler(
    filename = function() {
      paste("latent_regress", input$filetype, sep = ".")
    },
    content = function(file) {
      if (input$filetype == "png") {
        png(file, width = input$png.wid.corr, height = input$png.hei.corr)
        req(model$model())
        req(length(grep("fa", model$model()$mod$call)) != 0)
        validate(need(input$geno_filter, message = "Please select at least one genotype."))
        mod <- model$model()$mod
        gen <- input$geno_filter
        p <- latent_regress(mod, gen, input$scale, input$text_lab, input$size_text, input$alpha_text, input$alpha_p)
        print(p)
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid.corr, height = input$pdf.hei.corr)
        req(model$model())
        req(length(grep("fa", model$model()$mod$call)) != 0)
        validate(need(input$geno_filter, message = "Please select at least one genotype."))
        mod <- model$model()$mod
        gen <- input$geno_filter
        p <- latent_regress(mod, gen, input$scale, input$text_lab, input$size_text, input$alpha_text, input$alpha_p)
        print(p)
        dev.off()
      }
    }
  )



  fa_components <- reactive({
    req(model$model())
    req(length(grep("fa", model$model()$mod$call)) != 0)
    mod <- model$model()$mod
    datos <- latent_regress(mod, return_data = T)
    return(datos)
  })


  output$downloadFA <- downloadHandler(
    filename = function() {
      paste("FA_loadings_scores_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fa_components(), file, row.names = FALSE)
    }
  )

  output$downloadSC <- downloadHandler(
    filename = function() {
      paste("FA_scores_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      req(model$model())
      req(length(grep("fa", model$model()$mod$call)) != 0)
      model <- model$model()$mod
      ASM <- fa.asreml(model, trunc.char = NULL)
      ASM <- ASM$blups[[1]]$scores
      names(ASM)[c(1, 2, 4)] <- c("score", "component", "score_rotated")
      write.csv(ASM, file, row.names = FALSE)
    }
  )

  output$downloadLO <- downloadHandler(
    filename = function() {
      paste("FA_loadings_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      req(model$model())
      req(length(grep("fa", model$model()$mod$call)) != 0)
      model <- model$model()$mod
      ASM <- fa.asreml(model, trunc.char = NULL)
      psi <- ASM$gammas[[1]]$`specific var`
      ASM <- ASM$gammas[[1]]$`rotated loads` %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "site")
      ASM$psi <- psi
      write.csv(ASM, file, row.names = F)
    }
  )
}

## To be copied in the UI
# mod_MET_FA_ui("MET_FA_ui_1")

## To be copied in the server
# callModule(mod_MET_FA_server, "MET_FA_ui_1")
