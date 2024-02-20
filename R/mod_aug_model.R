#' aug_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aug_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Single-Site Unreplicated/Augmented Designs</h1>'),
    fluidRow(
      column(
        width = 4,
        fluidRow(
          bs4Dash::box(
            width = 12, status = "success", solidHeader = FALSE, title = tagList(icon = icon("cogs", verify_fa = FALSE), "Components"), # background = "light-blue"
            selectInput(
              inputId = ns("variable"),
              label = tagList("Response Variable",
                icon = tooltip(icon("question-circle", verify_fa = FALSE),
                  title = "The column with the continous response variable.",
                  placement = "top"
                )
              ),
              choices = "", width = "100%"
            ),
            selectInput(
              inputId = ns("genotype"),
              label = tagList("Genotype",
                icon = tooltip(icon("question-circle", verify_fa = FALSE),
                  title = "The column with genotypes.",
                  placement = "top"
                )
              ),
              choices = "", width = "100%"
            ),
            awesomeCheckbox(
              inputId = ns("res_ran"),
              label = "Random Genotype",
              value = TRUE, status = "danger"
            ),
            hr(),
            pickerInput(
              inputId = ns("check"),
              label = "Select Checks/Controls",
              choices = NULL, width = "100%",
              options = list(
                size = 5, # style = "btn-danger",
                `actions-box` = TRUE
              ),
              multiple = TRUE
            ),
            hr(),
            # selectInput(inputId=ns("block"),
            #             label=tagList( "Block",
            #                            icon=tooltip(icon("question-circle", verify_fa = FALSE),
            #                                              title = "Select the variable with Experiment-ID",
            #                                              placement = "top")),
            #             choices="", width = "100%"),
            fluidRow(
              column(
                6,
                selectInput(
                  inputId = ns("block"),
                  label = tagList("Block",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "Select the variable with Experiment-ID",
                      placement = "top"
                    )
                  ),
                  choices = "", width = "100%"
                )
              ),
              column(
                6,
                selectInput(
                  inputId = ns("covariate"),
                  label = tagList("Covariate (Optional)",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "Select covariate",
                      placement = "top"
                    )
                  ),
                  choices = "", width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                6,
                selectInput(inputId = ns("column"), label = "Column", choices = "", width = "100%")
              ),
              column(
                6,
                selectInput(inputId = ns("row"), label = "Row", choices = "", width = "100%")
              )
            ),
            awesomeCheckbox(
              inputId = ns("nugget"),
              label = "Include Nugget",
              value = FALSE, status = "danger"
            ),
            fluidRow(
              col_3(),
              col_6(
                actionBttn(inputId = ns("ok"), label = "Fit Models!", style = "jelly", color = "success", block = T, icon = icon("check"))
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
              bs4Dash::box(
                width = 12, title = tagList(icon = icon("wrench"), "Comparison Statistics"), status = "success", solidHeader = FALSE, collapsible = TRUE,
                HTML(
                  paste0("<center>", tableOutput(ns("models_aug")), "</center>")
                ),
                style = "overflow-x: scroll;",
                fluidRow(
                  col_2(),
                  col_8(
                    radioGroupButtons(
                      inputId = ns("selected"),
                      label = "Select a Model",
                      choices = c("Independent" = 1, "id(Row):ar1(Col)" = 2, "ar1(Row):id(Col)" = 3, "ar1(Row):ar1(Col)" = 4),
                      status = "success",
                      justified = TRUE,
                      checkIcon = list(
                        yes = icon("ok", lib = "glyphicon"),
                        no = icon("remove", lib = "glyphicon")
                      )
                    )
                  ),
                  col_2()
                ),
                fluidRow(
                  col_4(),
                  col_4(
                    actionBttn(
                      inputId = ns("run"), icon = icon("sliders-h", verify_fa = FALSE), size = "md",
                      label = "Run Selected Model!", style = "unite", color = "warning", block = T
                    )
                  ),
                  col_4()
                )
              )
            ),
            fluidRow(
              column(
                12,
                shinyjs::hidden(
                  div(
                    id = ns("spats"),
                    fluidRow(
                      bs4TabCard(
                        width = 12, id = "augment", maximizable = T, solidHeader = FALSE, closable = F,
                        status = "success", side = "left", type = "tabs",
                        tabPanel(
                          title = "Spatial Plots", active = T,
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
                          shinycssloaders::withSpinner(plotOutput(ns("plot_spats")), type = 5, color = "#28a745"), icon = icon("table-cells", verify_fa = FALSE)
                        ),
                        tabPanel(
                          title = "Diagnostics",
                          shinycssloaders::withSpinner(plotOutput(ns("residuals_aug")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE)
                        ),
                        tabPanel(
                          title = "Var-Components", icon = icon("signal"),
                          strong("Model Formula:"),
                          shinycssloaders::withSpinner(verbatimTextOutput(ns("callModel")), type = 6, color = "#28a745"),
                          hr(),
                          strong("Variance Component Estimates:"),
                          shinycssloaders::withSpinner(verbatimTextOutput(ns("summ")), type = 6, color = "#28a745")
                        ),
                        tabPanel(
                          title = "Semi-Variogram",
                          shinycssloaders::withSpinner(plotOutput(ns("semivariogram")), type = 6, color = "#28a745"), icon = icon("circle-arrow-right", verify_fa = FALSE)
                        )
                      ) # ,
                      # actionLink(inputId = ns("gen_effects"), label = "Genotype Effects", icon = icon("arrow-right"), style = "color: #28a745"),
                      # br()
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

#' aug_model Server Function
#'
#' @noRd
mod_aug_model_server <- function(input, output, session, data) {
  ns <- session$ns


  # update inputs
  observeEvent(data$data(), {
    dt <- data$data()
    updateSelectInput(session, "variable", choices = names(dt), selected = "YdHa_clean")
    updateSelectInput(session, "genotype", choices = names(dt), selected = "line")
    updateSelectInput(session, "block", choices = names(dt), selected = "block")
    updateSelectInput(session, "covariate", choices = names(dt), selected = "cov")
    updateSelectInput(session, "column", choices = names(dt), selected = "col")
    updateSelectInput(session, "row", choices = names(dt), selected = "row")
  })
  observeEvent(input$genotype, {
    dt <- data$data()
    req(input$genotype)
    lvl <- as.character(unique(dt[, input$genotype]))
    updatePickerInput(session, "check", choices = lvl)
  })

  observeEvent(input$nugget, { # warning Message heritability
    tryCatch(
      {
        aleatorio <- input$nugget
        if (isTRUE(aleatorio)) stop("You are incorporing the nugget.")
      },
      error = function(e) {
        shinytoastr::toastr_info(title = "INFO:", conditionMessage(e), position = "bottom-full-width")
      }
    )
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
        check = input$check,
        block = input$block,
        covariate = input$covariate,
        column = input$column,
        row = input$row,
        nugget = input$nugget
      )

      if (!is.null(variables$check)) {
        dt$CHECK[dt[, input$genotype] %in% variables$check] <- "control"
        dt$CHECK[!dt[, input$genotype] %in% variables$check] <- "test"
      }
      dt <- try(fill.asreml(dt, rows = conv_null(input$row), ranges = conv_null(input$column)), silent = T)
      if (class(dt) == "try-error") dt <- data.frame()
    })
    return(list(components = variables, data = dt))
  })

  w <- Waiter$new(
    html = HTML("<center> <div class='ball-loader'></div> </center>"),
    color = transparent(0.3)
  )

  # Single augmented model
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
      check <- if (is.null(comp$check)) {
        NULL
      } else {
        "CHECK"
      }
      tryCatch(
        {
          output.1R <- spatial.aud(
            data = dt,
            resp = conv_null(comp$response),
            gen = conv_null(comp$genotype),
            type.gen = ifelse(comp$res_ran, "random", "fixed"),
            check = check,
            block = conv_null(comp$block),
            covariate = conv_null(comp$covariate),
            col = conv_null(comp$column),
            row = conv_null(comp$row),
            nugget = comp$nugget
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
      if (!exists("output.1R")) output.1R <- NULL

      return(output.1R)
    })
  })

  # observeEvent(input$ok,{
  #   print(modelo())
  # }, ignoreInit = T)


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


  output$models_aug <- function() {
    req(modelo())
    modelo() %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      .[-5, ] %>%
      formattable::formattable(
        list(
          df = formattable::formatter(
            "span",
            style = x ~ formattable::style(
              display = "block",
              "border-radius" = "2px",
              "padding" = "2px",
              "text-align" = "left"
            )
          ),
          logL = formattable::color_tile("white", "orange"),
          AIC = formattable::color_tile("orange", "white"),
          BIC = formattable::color_tile("orange", "white"),
          heritPEV = formattable::color_tile("white", "orange"),
          heritVC = formattable::color_tile("white", "orange"),
          A.opt = formattable::color_tile("orange", "white"),
          D.opt = formattable::color_tile("orange", "white"),
          pvalue = formattable::color_tile("white", "orange")
        )
      ) %>%
      dplyr::select(MODEL, dplyr::everything()) %>%
      kableExtra::kable(escape = F, align = "c") %>%
      kableExtra::kable_styling(c("hover", "responsive", "condensed"), full_width = T, position = "center")
  }


  model_selected <- reactive({
    input$run

    isolate({
      req(input$selected)
      req(newData()$data)
      dt <- newData()$data
      comp <- newData()$components
      w$show()
      check <- if (is.null(comp$check)) {
        NULL
      } else {
        "CHECK"
      }
      # output.1R <- try(spatial.aud(data = dt,
      #                              resp     = conv_null(comp$response),
      #                              gen      = conv_null(comp$genotype),
      #                              type.gen = ifelse(comp$res_ran, "random", 'fixed'),
      #                              check    = check,
      #                              block    = conv_null(comp$block),
      #                              col      = conv_null(comp$column),
      #                              row      = conv_null(comp$row),
      #                              model    = input$selected),
      #                  silent = T)
      tryCatch(
        {
          output.1R <- spatial.aud(
            data = dt,
            resp = conv_null(comp$response),
            gen = conv_null(comp$genotype),
            type.gen = ifelse(comp$res_ran, "random", "fixed"),
            check = check,
            block = conv_null(comp$block),
            covariate = conv_null(comp$covariate),
            col = conv_null(comp$column),
            row = conv_null(comp$row),
            nugget = comp$nugget,
            model = input$selected
          )
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:", conditionMessage(e), position = "bottom-full-width",
            showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
          )
        }
      )
      if (!exists("output.1R")) output.1R <- NULL
      w$hide()
      return(output.1R)
    })
  })


  output$plot_spats <- renderPlot({
    input$run
    isolate({
      req(model_selected())
      model <- model_selected()$mod
      augment.SpATS(model, col = "col", row = "row", response = "resp")
    })
  })


  observe({
    if (input$run != 0) {
      show(id = "spats", anim = TRUE, animType = "slide")
    } else {
      hide(id = "spats", anim = TRUE, animType = "slide")
    }
  })

  # Assumptions

  output$residuals_aug <- renderPlot({
    input$run
    isolate({
      req(model_selected())
      model <- model_selected()$mod
      plot(model)
    })
  })

  # # observeEvent(input$assumption,{
  # #   showModal(modalDialog(
  # #     title = "Assumptions", size = "l", easyClose = T,
  # #     shinycssloaders::withSpinner(plotOutput(ns("residuals_aug")),type = 6,color = "#28a745"),
  # #     footer = modalButton("Cancel")
  # #   ))
  # # })

  # Formula

  output$callModel <- renderPrint({
    input$run
    isolate({
      req(model_selected())
      model <- model_selected()$mod
      model$call
    })
  })

  output$summ <- renderPrint({
    input$run
    isolate({
      req(model_selected())
      model <- model_selected()$mod
      summary(model)$varcomp
    })
  })


  # # observeEvent(input$formula,{
  # #   showModal(modalDialog(
  # #     title = "Formula:", size = "l", easyClose = T,
  # #     shinycssloaders::withSpinner(verbatimTextOutput(ns("callModel")),type = 6,color = "#28a745" ),
  # #     hr(),
  # #     strong("Variance Components:"),
  # #     shinycssloaders::withSpinner(verbatimTextOutput(ns("summ")),type = 6,color = "#28a745" ),
  # #     footer = modalButton("Cancel")
  # #   ))
  # # })


  output$semivariogram <- renderPlot({
    input$run
    isolate({
      req(model_selected())
      model <- model_selected()$mod
      # DATA <- data.frame(model$mf)
      # DATA$residuals <-stats::residuals(model)
      # DATA$col <- as.numeric(DATA$col)
      # DATA$row <- as.numeric(DATA$row)
      # ic <- which(names(DATA)=="col")
      # ir <- which(names(DATA)=="row")
      # ix <- which(names(DATA)=="residuals")
      # DATA <- geoR::as.geodata(DATA, coords.col = c(ic,ir),  data.col = ix)
      # graphics::par(mfrow=c(1,2))
      # var1 = geoR::variog(DATA, max.dist=1000)
      # plot(var1)
      # env.var = geoR::variog.mc.env(DATA, obj.v=var1, nsim=100)
      # plot(var1, env=env.var)
      plot(asreml::varioGram.asreml(model), main = "Empirical Variogram")
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
        augment.SpATS(model_selected()$mod, col = "col", row = "row", response = "resp")
        grDevices::dev.off()
      } else {
        grDevices::pdf(file, width = input$pdf.wid, height = input$pdf.hei)
        augment.SpATS(model_selected()$mod, col = "col", row = "row", response = "resp")
        grDevices::dev.off()
      }
    }
  )


  return(list(
    model = model_selected,
    run = reactive(input$run)
  ))
}

## To be copied in the UI
# mod_aug_model_ui("aug_model_ui_1")

## To be copied in the server
# callModule(mod_aug_model_server, "aug_model_ui_1")
