#' spats_asreml UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spats_asreml_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Single-Site Spatial
         Analysis</h1>'),
    fluidRow(
      column(
        width = 4,
        fluidRow(
          bs4Dash::box(
            width = 12,
            status = "success",
            solidHeader = FALSE,
            title = tagList(icon = icon("cogs", verify_fa = FALSE), "ASReml"),
            selectInput(
              inputId = ns("variable"),
              label = tagList(
                "Response Variable",
                icon = tooltip(
                  icon("question-circle", verify_fa = FALSE),
                  title = "The column with the continous response variable.",
                  placement = "top"
                )
              ),
              choices = "",
              width = "100%"
            ),
            selectInput(
              inputId = ns("genotype"),
              label = tagList(
                "Genotype",
                icon = tooltip(
                  icon("question-circle", verify_fa = FALSE),
                  title = "The column with genotypes.",
                  placement = "top"
                )
              ),
              choices = "",
              width = "100%"
            ),
            awesomeCheckbox(
              inputId = ns("res_ran"),
              label = "Random Genotype",
              value = TRUE,
              status = "danger"
            ),
            hr(),
            fluidRow(
              column(
                6,
                selectInput(
                  inputId = ns("column"),
                  label = "Column",
                  choices = "",
                  width = "100%"
                )
              ),
              column(
                6,
                selectInput(
                  inputId = ns("row"),
                  label = "Row",
                  choices = "",
                  width = "100%"
                )
              )
            ),
            materialSwitch(
              ns("ar1ar1"),
              label = "Spatial Correlation",
              status = "success",
              right = T,
              width = "100%"
            ),
            div(
              id = ns("inclu_ar1"),
              fluidRow(
                col_12(
                  awesomeCheckbox(
                    inputId = ns("nugget"),
                    label = "Include Nugget",
                    value = FALSE, status = "danger"
                  )
                )
              )
            ),
            fluidRow(
              col_6(
                awesomeCheckboxGroup(
                  inputId = ns("splines"),
                  label = "Include Splines",
                  inline = F,
                  choices = c("Column", "Row"),
                  status = "success",
                  selected = c("Column", "Row")
                )
              ),
              col_6(
                awesomeCheckboxGroup(
                  inputId = ns("as_factors"),
                  label = "Include Factors",
                  inline = F,
                  choices = c("Column", "Row"),
                  status = "success",
                  selected = c("Column", "Row")
                )
              )
            ),
            hr(),
            materialSwitch(
              ns("able"),
              label = "Include Blocks",
              status = "success",
              right = T,
              width = "100%"
            ),
            div(
              id = ns("first"),
              fluidRow(
                column(
                  6,
                  selectInput(
                    inputId = ns("block"),
                    label = tagList(
                      "Block",
                      icon = tooltip(
                        icon("question-circle", verify_fa = FALSE),
                        title = "Select the replicate or complete block",
                        placement = "top"
                      )
                    ),
                    choices = "",
                    width = "100%"
                  )
                ),
                column(
                  6,
                  selectInput(
                    inputId = ns("incomplete"),
                    label = tagList(
                      "InBlock",
                      icon = tooltip(
                        icon("question-circle", verify_fa = FALSE),
                        title = "Select Incomplete Block",
                        placement = "top"
                      )
                    ),
                    choices = "",
                    width = "100%"
                  )
                ),
                awesomeCheckbox(
                  inputId = ns("block_ran"),
                  label = "Block as Random Effect",
                  value = FALSE, status = "success"
                )
              )
            ),
            materialSwitch(
              ns("able2"),
              label = "Include Covariate",
              status = "success",
              right = F,
              width = "100%"
            ),
            div(
              id = ns("second"),
              fluidRow(
                column(
                  6,
                  selectInput(
                    inputId = ns("cov1"),
                    label = tagList(
                      "Covariate 1",
                      icon = tooltip(
                        icon("question-circle", verify_fa = FALSE),
                        title = "Select a covariate",
                        placement = "top"
                      )
                    ),
                    choices = "",
                    width = "100%"
                  )
                ),
                column(
                  6,
                  selectInput(
                    inputId = ns("cov2"),
                    label = tagList(
                      "Covariate 2",
                      icon = tooltip(
                        icon("question-circle", verify_fa = FALSE),
                        title = "Select a covariate",
                        placement = "top"
                      )
                    ),
                    choices = "",
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              col_3(),
              col_6(
                actionBttn(
                  inputId = ns("ok"),
                  label = "Fit Model",
                  style = "jelly",
                  color = "success",
                  block = T,
                  icon = icon("check")
                )
              ),
              col_3()
            )
          )
        )
      ),
      column(
        8,
        shinyjs::hidden(
          div(
            id = ns("only"),
            fluidRow(
              column(
                12,
                fluidRow(
                  bs4TabCard(
                    width = 12,
                    id = "single_model",
                    maximizable = T,
                    solidHeader = FALSE,
                    closable = F,
                    status = "success",
                    side = "left",
                    type = "tabs",
                    tabPanel(
                      title = "Spatial-Plot",
                      active = T,
                      dropdown(
                        prettyRadioButtons(
                          inputId = ns("typefile"),
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
                          condition = "input.typefile=='png'",
                          ns = ns,
                          sliderInput(
                            inputId = ns("png.wid"),
                            min = 200,
                            max = 2000,
                            value = 900,
                            label = "Width pixels"
                          ),
                          sliderInput(
                            inputId = ns("png.hei"),
                            min = 200,
                            max = 2000,
                            value = 600,
                            label = "Height pixels"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.typefile=='pdf'",
                          ns = ns,
                          sliderInput(
                            inputId = ns("pdf.wid"),
                            min = 2,
                            max = 20,
                            value = 10,
                            label = "Width"
                          ),
                          sliderInput(
                            inputId = ns("pdf.hei"),
                            min = 2,
                            max = 20,
                            value = 8,
                            label = "Height"
                          )
                        ),
                        downloadButton(
                          ns("descargar"),
                          "Download Plot",
                          class = "btn-success",
                          style = "color: white ; background-color: #28a745"
                        ),
                        br(),
                        animate = shinyWidgets::animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                          exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
                        ),
                        style = "unite",
                        icon = icon("gear", verify_fa = FALSE),
                        status = "warning",
                        width = "300px"
                      ),
                      shinycssloaders::withSpinner(
                        plotOutput(ns("plot_spats")),
                        type = 5,
                        color = "#28a745"
                      ),
                      icon = icon("table-cells", verify_fa = FALSE)
                    ),
                    tabPanel(
                      title = "Diagnostics",
                      prettySwitch(
                        inputId = ns("swicht1"),
                        label = "Residuals",
                        status = "success",
                        slim = FALSE
                      ),
                      shinycssloaders::withSpinner(plotOutput(ns("residuals_aug")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE)
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
            ),
            fluidRow(
              column(
                12,
                fluidRow(
                  bs4Dash::box(
                    width = 12, title = tagList(icon = icon("wrench"), "Goodness-of-fit Statistics"), status = "success", solidHeader = FALSE, collapsible = TRUE,
                    HTML(
                      paste0("<center>", tableOutput(ns("INFO")), "</center>")
                    ),
                    style = "overflow-x: scroll;"
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

#' spats_asreml Server Function
#'
#' @noRd
mod_spats_asreml_server <- function(input, output, session, data) {
  ns <- session$ns

  observeEvent(!input$able, toggle("first", anim = TRUE, time = 1, animType = "fade"))
  observeEvent(!input$able2, toggle("second", anim = TRUE, time = 1, animType = "fade"))
  observeEvent(!input$ar1ar1, toggle("inclu_ar1", anim = TRUE, time = 1, animType = "fade"))

  observeEvent(data$data(), {
    dt <- data$data()
    req(dt)
    updateSelectInput(session, "variable", choices = names(dt), selected = "YdHa_clean")
    updateSelectInput(session, "genotype", choices = names(dt), selected = "line")
    updateSelectInput(session, "column", choices = names(dt), selected = "col")
    updateSelectInput(session, "row", choices = names(dt), selected = "row")
  })

  observeEvent(input$able, {
    dt <- data$data()
    req(dt)
    updateSelectInput(session, "block", choices = names(dt), selected = "")
    updateSelectInput(session, "incomplete", choices = names(dt), selected = "")
  })

  observeEvent(input$able2, {
    dt <- data$data()
    req(dt)
    updateSelectInput(session, "cov1", choices = names(dt), selected = "")
    updateSelectInput(session, "cov2", choices = names(dt), selected = "")
  })

  # preparing data for modelling
  newData <- reactive({
    input$ok
    isolate({
      req(data$data())
      dt <- data$data()
      req(input$variable)
      req(input$genotype)
      req(input$column)
      req(input$row)
      variables <- list(
        response = input$variable,
        genotype = input$genotype,
        res_ran = input$res_ran,
        column = input$column,
        row = input$row,
        nugget = input$nugget,
        block = input$block,
        incomplete = input$incomplete,
        block_ran = input$block_ran,
        cov1 = input$cov1,
        cov2 = input$cov2,
        add.block = ifelse(input$block != "", T, F),
        add.ibk = ifelse(input$incomplete != "", T, F),
        add.row = ifelse("Row" %in% input$as_factors, T, F),
        add.col = ifelse("Column" %in% input$as_factors, T, F),
        add.spl.row = ifelse("Row" %in% input$splines, T, F),
        add.spl.col = ifelse("Column" %in% input$splines, T, F),
        add.cov1 = ifelse(input$cov1 != "", T, F),
        add.cov2 = ifelse(input$cov2 != "", T, F),
        type.block = ifelse(input$block_ran, "random", "fixed")
      )

      dt <- try(fill.asreml(dt, rows = conv_null(input$row), ranges = conv_null(input$column)), silent = T)
      if (class(dt) == "try-error") dt <- data.frame()
    })
    return(list(components = variables, data = dt))
  })

  # Modelo

  w <- Waiter$new(
    html = HTML(
      "<center>",
      '<div class="dots-loader"></div>',
      "<br>", "<br>", "<br>",
      '<h5 style="font-weight: bold; color: grey;">Fitting ASreml Model...</h5>',
      "</center>"
    ),
    color = transparent(0.3)
  )

  # Single model
  modelo <- reactive({
    req(newData())
    if (input$row == "" | is.null(input$row)) {
      return()
    }
    if (input$column == "" | is.null(input$column)) {
      return()
    }
    isolate({
      dt <- newData()$data
      comp <- newData()$components
      w$show()
      tryCatch(
        {
          model <- spatial.single(
            data = dt,
            gen = conv_null(comp$genotype),
            block = conv_null(comp$block),
            ibk = conv_null(comp$incomplete),
            row = conv_null(comp$row),
            col = conv_null(comp$column),
            cov1 = conv_null(comp$cov1),
            cov2 = conv_null(comp$cov2),
            resp = conv_null(comp$response),
            add.block = comp$add.block,
            add.ibk = comp$add.ibk,
            add.row = comp$add.row,
            add.col = comp$add.col,
            add.spl.row = comp$add.spl.row,
            add.spl.col = comp$add.spl.col,
            add.cov1 = comp$add.cov1,
            add.cov2 = comp$add.cov2,
            add.nugget = comp$nugget,
            type.gen = ifelse(comp$res_ran, "random", "fixed"),
            type.block = comp$type.block,
            type.residual = ifelse(input$ar1ar1, "ar1", "indep")
          )
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:", conditionMessage(e), position = "bottom-full-width",
            showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
          )
        }
      )
      w$hide()
      if (!exists("model")) model <- NULL

      return(model)
    })
  })


  observeEvent(input$ok,
    {
      if (!is.null(modelo())) {
        show(id = "only", anim = TRUE, animType = "slide")
      } else {
        hide(id = "only", anim = TRUE, animType = "slide")
      }
    },
    ignoreInit = T,
    ignoreNULL = T
  )

  output$plot_spats <- renderPlot({
    input$ok
    isolate({
      req(modelo())
      model <- modelo()$mod
      spatial.ASReml(model, col = "col", row = "row", response = "resp", genotype = "gen")
    })
  })

  output$residuals_aug <- renderPlot({
    input$ok
    input$swicht1
    isolate({
      req(modelo())
      model <- modelo()$mod
      plot(model, spatial = ifelse(input$swicht1, "plot", "trend"))
    })
  })


  # Formula

  output$callModel <- renderPrint({
    input$ok
    isolate({
      req(modelo())
      model <- modelo()$mod
      model$call
    })
  })

  output$summ <- renderPrint({
    input$ok
    isolate({
      req(modelo())
      model <- modelo()$mod
      summary(model)$varcomp %>%
        tibble::rownames_to_column("name") %>%
        dplyr::mutate_if(is.numeric, round, 3)
    })
  })

  output$aov <- renderPrint({
    input$ok
    isolate({
      req(modelo())
      model <- modelo()$aov
      model %>%
        tibble::rownames_to_column("name") %>%
        dplyr::mutate_if(is.numeric, round, 3)
    })
  })


  output$INFO <- function() {
    input$ok
    isolate({
      req(modelo())
      gt <- modelo()$gt
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

  # observeEvent(newData()$components$nugget, toggle("swicht1",anim = TRUE,time = 1,animType = "fade"))
  # observeEvent(newData()$components$nugget, toggle("swicht2",anim = TRUE,time = 1,animType = "fade"))

  observe({
    toggle(id = "swicht1", condition = length(grep("units", modelo()$mod$call)) != 0)
    toggle(id = "swicht2", condition = length(grep("units", modelo()$mod$call)) != 0)
  })

  output$semivariogram <- renderPlot({
    input$ok
    input$swicht2
    isolate({
      req(modelo())
      model <- modelo()$mod
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

  # GUIA
  # observeEvent(input$guide,
  #              rintrojs::introjs(session,options = list("nextLabel"="Next",
  #                                                       "prevLabel"="Back",
  #                                                       "skipLabel"="Skip")))

  # observeEvent(input$ok,{
  #   print(newData()$components)
  #   # print(modelo()$string)
  #   # print(modelo()$gt)
  # })


  # Download PLOT SPATIAL
  output$descargar <- downloadHandler(
    filename = function() {
      paste("plotSpATS", input$typefile, sep = ".")
    },
    content = function(file) {
      if (input$typefile == "png") {
        png(file, width = input$png.wid, height = input$png.hei)
        spatial.ASReml(modelo()$mod, col = "col", row = "row", response = "resp", genotype = "gen")
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid, height = input$pdf.hei)
        spatial.ASReml(modelo()$mod, col = "col", row = "row", response = "resp", genotype = "gen")
        dev.off()
      }
    }
  )


  return(list(
    model = modelo,
    run = reactive(input$ok)
  ))
}

## To be copied in the UI
# mod_spats_asreml_ui("spats_asreml_ui_1")

## To be copied in the server
# callModule(mod_spats_asreml_server, "spats_asreml_ui_1")
