#' asreml_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_asreml_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">ASReml Selector</h1>'),
    # HTML('<h4 style="font-weight: bold; color: #00a65a;">Spatial Model</h4>'),
    fluidRow(
      column(
        width = 4,
        fluidRow(
          bs4Dash::box(
            width = 12, status = "success", solidHeader = FALSE, title = tagList(icon = icon("cogs", verify_fa = FALSE), "Configuration"), # background = "light-blue"
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
              label = "Nugget",
              value = FALSE, status = "danger"
            ),
            # awesomeCheckbox(inputId = ns('nugget') ,
            #                 label='nugget?',
            #                 value = FALSE ,status = "danger"  ),
            # fluidRow(
            #   col_6(
            #     awesomeCheckboxGroup(
            #       inputId = ns("splines"),
            #       label = "Include Splines? ", inline = T,
            #       choices = c("Column", "Row"),
            #       status = "success",
            #       selected = c("Column", "Row")
            #     )
            #   ),
            #   col_6(
            #     awesomeCheckboxGroup(
            #       inputId = ns("as_factors"),
            #       label = "Include Factors? ", inline = T,
            #       choices = c("Column", "Row"),
            #       status = "success",
            #       selected = c("Column", "Row")
            #     )
            #   )
            # ),
            hr(),
            materialSwitch(ns("able"), label = "Blocks", status = "success", right = T, width = "100%"),
            div(
              id = ns("first"),
              fluidRow(
                column(
                  6,
                  selectInput(
                    inputId = ns("block"),
                    label = tagList("Block",
                      icon = tooltip(icon("question-circle", verify_fa = FALSE),
                        title = "Select the replicate or complete block",
                        placement = "top"
                      )
                    ),
                    choices = "", width = "100%"
                  )
                ),
                column(
                  6,
                  selectInput(
                    inputId = ns("incomplete"),
                    label = tagList("IncBlock",
                      icon = tooltip(icon("question-circle", verify_fa = FALSE),
                        title = "Select Incomplete Block",
                        placement = "top"
                      )
                    ),
                    choices = "", width = "100%"
                  )
                ),
                awesomeCheckbox(
                  inputId = ns("block_ran"),
                  label = "Random Block",
                  value = FALSE, status = "success"
                )
              )
            ),
            materialSwitch(ns("able2"), label = "Covariates", status = "success", right = F, width = "100%"),
            div(
              id = ns("second"),
              fluidRow(
                column(
                  6,
                  selectInput(
                    inputId = ns("cov1"),
                    label = tagList("Covariate 1",
                      icon = tooltip(icon("question-circle", verify_fa = FALSE),
                        title = "Select a covariate",
                        placement = "top"
                      )
                    ),
                    choices = "", width = "100%"
                  )
                ),
                column(
                  6,
                  selectInput(
                    inputId = ns("cov2"),
                    label = tagList("Covariate 2",
                      icon = tooltip(icon("question-circle", verify_fa = FALSE),
                        title = "Select a covariate",
                        placement = "top"
                      )
                    ),
                    choices = "", width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              col_3(),
              col_6(
                actionBttn(inputId = ns("ok"), label = "Check!", style = "jelly", color = "success", block = T, icon = icon("check"))
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
                    width = 12, id = "selector", maximizable = T, solidHeader = FALSE, closable = F,
                    status = "success", side = "left", type = "tabs",
                    tabPanel(
                      title = "Comparison", active = T,
                      fluidRow(
                        col_4(),
                        col_4(
                          selectInput(ns("criteria"),
                            label = HTML("<center> Criterion </center>"),
                            choices = c("logL", "AIC", "BIC", "heritPEV", "A.opt"),
                            selected = "heritPEV",
                            width = "100%"
                          )
                        ),
                        col_4()
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(ns("plot")),
                        type = 5, color = "#28a745"
                      ),
                      icon = icon("table-cells", verify_fa = FALSE)
                    ),
                    tabPanel(
                      title = "Summary",
                      shinycssloaders::withSpinner(
                        DT::dataTableOutput(ns("models")),
                        type = 5, color = "#28a745"
                      ),
                      icon = icon("circle-arrow-right", verify_fa = FALSE)
                    )
                  )
                ),
                fluidRow(
                  col_4(),
                  col_4(
                    actionBttn(
                      inputId = ns("tabBut"), icon = icon("sliders-h", verify_fa = FALSE), size = "md",
                      label = "Comparison", style = "jelly", color = "success", block = T
                    ),
                    br()
                  ),
                  col_4()
                )
              )
            )
          )
        )
      )
    )
  )
}

#' asreml_selector Server Function
#'
#' @noRd
mod_asreml_selector_server <- function(input, output, session, data) {
  ns <- session$ns


  observeEvent(!input$able, toggle("first", anim = TRUE, time = 1, animType = "fade"))
  observeEvent(!input$able2, toggle("second", anim = TRUE, time = 1, animType = "fade"))

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
        block = input$block,
        incomplete = input$incomplete,
        block_ran = input$block_ran,
        cov1 = input$cov1,
        cov2 = input$cov2,
        type.block = ifelse(input$block_ran, "random", "fixed"),
        nugget = input$nugget
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
      "<br>", "<br>", "<br>", "<br>",
      '<h5 style="font-weight: bold; color: grey;">Finding the Best Spatial Model...</h5>',
      "<br>",
      '<h6 style="font-weight: bold; color: grey;">This could take time...</h6>',
      "</center>"
    ),
    color = transparent(0.3)
  )

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

  # Single model
  modelo <- reactive({
    req(newData())
    if (input$row == "" | is.null(input$row)) {
      return()
    }
    if (input$column == "" | is.null(input$column)) {
      return()
    }
    dt <- newData()$data
    comp <- newData()$components
    isolate({
      w$show()
      tryCatch(
        {
          model <- spatial.selector(
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
            model = NULL
          )
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:", conditionMessage(e), position = "bottom-full-width",
            showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
          )
          w$hide()
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


  output$models <- DT::renderDataTable({
    input$ok
    isolate({
      req(modelo())
      dt <- modelo()$parms %>%
        dplyr::mutate_if(is.numeric, round, 3) %>%
        dplyr::mutate(MODEL = as.factor(MODEL))
      DT::datatable(dt,
        extensions = "Buttons", filter = "top", selection = "multiple",
        options = list(
          dom = "lfrtip", scrollX = TRUE, pageLength = 10, lengthMenu = c(5, 10, 15, 20, 33),
          columnnDefs = list(list(className = "dt-center", targets = 0:ncol(dt)))
        )
      )
    })
  })


  output$plot <- plotly::renderPlotly({
    input$criteria
    isolate({
      req(modelo())
      dt <- modelo()$parms
      g1 <- dt %>% ggplot(aes(x = MODEL, y = .data[[input$criteria]])) +
        geom_line() +
        geom_point() +
        theme_bw()
      plotly::ggplotly(g1)
    })
  })


  observeEvent(input$tabBut, {
    showModal(modalDialog(
      title = "Model Comparison:", size = "l", easyClose = T,
      fluidRow(
        col_6(
          pickerInput(
            inputId = ns("mod1"),
            label = "Model 1",
            choices = c(1:32),
            options = list(
              title = "Please select a model ...", size = 5
            ), width = "100%"
          )
        ),
        col_6(
          pickerInput(
            inputId = ns("mod2"),
            label = "Model 2",
            choices = c(1:32),
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
      shinyjs::hidden(
        div(
          id = ns("message"),
          HTML('<h6 style="font-weight: bold; color: #dc3545;">Note: Fixed Effect Differ</h6>')
        )
      ),
      DT::dataTableOutput(ns("anovamix")),
      footer = tagList(modalButton("Cancel"))
    ))
  })


  loglike <- reactive({
    input$comparison
    isolate({
      req(modelo())
      dt <- newData()$data
      comp <- newData()$components
      req(input$mod1)
      req(input$mod2)
      test$show()
      k <- as.numeric(input$mod1)
      j <- as.numeric(input$mod2)

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

      tryCatch(
        {
          m1 <- spatial.selector(
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
            model = j
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

      result <- test_lrt(m0 = m0, m1 = m1)

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

  observe({
    toggle(id = "message", condition = nrow(loglike()) == 2)
  })


  return(
    list(
      modelo = modelo,
      newData = newData
    )
  )
}

## To be copied in the UI
# mod_asreml_selector_ui("asreml_selector_ui_1")

## To be copied in the server
# callModule(mod_asreml_selector_server, "asreml_selector_ui_1")
