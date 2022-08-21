#' MSA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MSA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Spatial Analysis for Several Sites</h1>'),
    fluidRow(
      column(
        width = 3,
        fluidRow(
          bs4Dash::box(
            width = 12,
            status = "success", 
            solidHeader = FALSE, 
            title = tagList(
              icon = icon("cogs", verify_fa = FALSE),
              "Configuration"
            ), 
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
            shinyjs::hidden(
              pickerInput(
                inputId = ns("selected_checks"),
                label = tagList(
                  "Checks",
                  icon = tooltip(
                    icon("question-circle", verify_fa = FALSE),
                    title = "Select Checks",
                    placement = "top"
                  )
                ),
                choices = NULL,
                options = list(
                  `actions-box` = TRUE, size = 5, `live-search` = TRUE
                ),
                multiple = TRUE, 
                width = "100%"
              )
            ),
            hr(),
            selectInput(
              inputId = ns("experiment"),
              label = tagList(
                "Experiment",
                icon = tooltip(
                  icon("question-circle", verify_fa = FALSE),
                  title = "Select the variable with Experiment-ID",
                  placement = "top"
                )
              ),
              choices = "",
              width = "100%"
            ),
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
            actionBttn(
              inputId = ns("check"), 
              label = "Check!", 
              style = "jelly",
              color = "success", 
              block = T, 
              icon = icon("check")
            )
          )
        )
      ),
      column(
        3,
        shinyjs::hidden(
          div(
            id = ns("only"),
            fluidRow(
              bs4Dash::box(
                width = 12,
                title = tagList(
                  icon = icon("wrench"), "Additional Components"
                ), 
                status = "success", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                actionBttn(
                  inputId = ns("tabBut"),
                  icon = icon("sliders-h", verify_fa = FALSE),
                  size = "xs",
                  label = "Info About the Experiments", 
                  style = "unite", 
                  color = "warning", 
                  block = TRUE
                ),
                selectInput(
                  inputId = ns("replicate"), 
                  label = "Replicate", 
                  choices = "", 
                  width = "100%"
                ),
                selectizeInput(
                  ns("show_fixed"),
                  width = "100%",
                  label = tagList(
                    "Fixed",
                    icon = tooltip(
                      icon("question-circle", verify_fa = FALSE),
                      title = "Additional fixed factors.",
                      placement = "top"
                    )
                  ),
                  choices = "", 
                  multiple = TRUE
                ),
                shinyjs::hidden(
                  pickerInput(
                    inputId = ns("fix_traits"),
                    label = "Experiments",
                    choices = "",
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      `actions-box` = TRUE,
                      size = 5
                    )
                  )
                ),
                selectizeInput(
                  ns("show_random"),
                  width = "100%",
                  label = tagList(
                    "Random",
                    icon = tooltip(
                      icon("question-circle", verify_fa = FALSE),
                      title = "Additional random factors.",
                      placement = "top"
                    )
                  ),
                  choices = "", 
                  multiple = TRUE
                ),
                shinyjs::hidden(
                  pickerInput(
                    inputId = ns("ran_traits"),
                    label = "Experiments",
                    choices = "",
                    multiple = T,
                    width = "100%",
                    options = list(
                      `actions-box` = TRUE,
                      size = 5
                    )
                  )
                ),
                selectizeInput(
                  ns("covariate"),
                  width = "100%",
                  label = tagList(
                    "Covariate",
                    icon = tooltip(
                      icon("question-circle", verify_fa = FALSE),
                      title = "Additional covariate.",
                      placement = "top"
                    )
                  ),
                  choices = "",
                  multiple = TRUE,
                  selected = NULL
                ),
                hr(),
                sliderInput(
                  ns("k_clean_out"),
                  label = tagList(
                    "rLimit",
                    icon = tooltip(
                      icon("question-circle", verify_fa = FALSE),
                      title = paste0(
                        "A numerical value used for determining when a value is ",
                        "considered an outlier. All observations with standardized ",
                        "residuals exceeding rLimit will be marked as outliers. ",
                        "3 by default."
                      ),
                      placement = "top"
                    )
                  ),
                  min = 1,
                  max =  4,
                  value =  3,
                  step = 0.1,
                  width = "100%"
                ),
                awesomeCheckbox(
                  inputId = ns("outliers"),
                  label = "Remove Outliers",
                  value = FALSE, status = "danger"
                ),
                numericInput(
                  ns("times"), 
                  "Number of Times to Check", 
                  value = 1,
                  min = 1,
                  max = 3, 
                  step = 1,
                  width = "100%"
                ),
                actionBttn(
                  inputId = ns("continue"),
                  label = "Continue?",
                  style = "jelly", 
                  color = "success",
                  block = T, 
                  icon = icon("exclamation-circle", verify_fa = FALSE)
                )
              )
            )
          )
        )
      ),
      column(
        6,
        shinyjs::hidden(
          div(
            id = ns("plots"),
            fluidRow(
              bs4Dash::box(
                width = 12,
                title = tagList(
                  icon = icon("pagelines"), "Genotypes Statistics"
                ), 
                maximizable = T,
                status = "success", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                prettySwitch(
                  inputId = ns("swicht"),
                  label = "Shared Genotypes",
                  status = "success",
                  slim = F,
                  value = F
                ),
                echarts4r::echarts4rOutput(ns("nGen")),
                actionLink(
                  inputId = ns("conectivity"), 
                  label = "See Conectivity", 
                  icon = icon("arrow-right"),
                  style = "color: #28a745"
                ),
                fluidRow(
                  col_4(),
                  col_4(
                    actionBttn(
                      inputId = ns("run"),
                      label = "Run Models",
                      style = "unite",
                      size = "sm",
                      block = T,
                      color = "warning", 
                      icon = icon("spinner")
                    )
                  ),
                  col_4()
                ),
                shinyjs::hidden(
                  actionLink(
                    inputId = ns("Rlink"),
                    label = "Check models!",
                    icon = icon("arrow-right"),
                    style = "color: #28a745"
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

#' MSA Server Function
#'
#' @noRd
mod_MSA_server <- function(input, output, session, data) {
  ns <- session$ns

  observeEvent(
    !input$outliers,
    toggle("times", anim = TRUE, time = 1, animType = "fade")
  )

  observeEvent(data$data(), {
    dt <- data$data()
    updateSelectInput(
      session, "variable", choices = names(dt), selected = "YdHa_clean"
    )
    updateSelectInput(
      session, "genotype", choices = names(dt), selected = "line"
    )
    updateSelectInput(
      session, "experiment", choices = names(dt), selected = "dataset"
    )
    updateSelectInput(
      session, "column", choices = names(dt), selected = "col"
    )
    updateSelectInput(
      session, "row", choices = names(dt), selected = "row"
    )
    updateSelectInput(
      session, "replicate", choices = names(dt), selected = "rep"
    )
  })

  # fix and random factors for specific locations
  observe({
    toggle(id = "fix_traits", condition = !is.null(input$show_fixed))
    toggle(id = "ran_traits", condition = !is.null(input$show_random))
    req(input$experiment)
    req(data$data())
    req(input$experiment %in% names(data$data()))
    dt <- data$data()
    updatePickerInput(
      session, 
      "fix_traits",
      choices = unique(dt[, input$experiment]), 
      selected = "NNNN"
    )
    updatePickerInput(
      session, 
      "ran_traits",
      choices = unique(dt[, input$experiment]), 
      selected = "NNNN"
    )
  })

  observe({
    shinyjs::toggle(
      id = "selected_checks", 
      anim = TRUE, 
      time = 1,
      animType = "fade", 
      condition = input$genotype != "" & input$res_ran == TRUE
    )
    req(input$genotype)
    req(data$data())
    req(input$genotype %in% names(data$data()))
    lvl <- as.character(unique(data$data()[, input$genotype]))
    updatePickerInput(session, inputId = "selected_checks", choices = lvl)
  })


  w <- Waiter$new(
    html = HTML("<center> <div class='ball-loader'></div> </center>"),
    color = transparent(0.3)
  )

  info_check <- eventReactive(input$check,
    {
      req(data$data())
      w$show()
      inf <- check_spats(
        data$data(), 
        input$variable, 
        input$genotype, 
        input$experiment, 
        input$column, 
        input$row
      )
      w$hide()
      return(inf)
    },
    ignoreNULL = T
  )

  observeEvent(info_check(), {
    dt <- data$data()
    vars <- c(
      input$variable,
      input$genotype,
      input$experiment,
      input$column, 
      input$row
    )
    nNew <- names(dt)[!names(dt) %in% vars]
    updateSelectInput(session, "show_fixed", choices = nNew, selected = "NNNN")
    updateSelectInput(session, "show_random", choices = nNew, selected = "NNNN")
    updateSelectInput(session, "covariate", choices = nNew, selected = "NNNN")
  })

  observe({
    if (!is.null(info_check())) {
      show(id = "only", anim = TRUE, animType = "slide")
    } else {
      hide(id = "only", anim = TRUE, animType = "slide")
    }
  })

  output$table <- DT::renderDataTable(
    if (is.null(info_check())) {
      return()
    } else {
      DT::datatable(
        {
          info_check()
        },
        option = list(
          pageLength = 10, 
          scrollX = TRUE, 
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(info_check()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  observeEvent(input$tabBut, {
    showModal(modalDialog(
      title = "Info-Experiments",
      size = "l", 
      easyClose = TRUE,
      DT::dataTableOutput(ns("table"))
    ))
  })

  factors_check <- eventReactive(input$continue,
    {
      req(info_check())
      exp <- names(info_check())[1]
      w$show()
      inf <- info_check() %>%
        dplyr::filter(
          percentage <= 0.6 & length(ncol) >= 2 & length(nrow) >= 2) %>%
        dplyr::pull(.data[[exp]])
      w$hide()
      return(inf)
    },
    ignoreNULL = TRUE
  )

  shared <- eventReactive(input$continue,
    {
      req(data$data())
      w$show()
      inf <- gen_share(
        data$data(), 
        input$genotype, 
        input$experiment,
        input$variable
      )
      w$hide()
      return(inf)
    },
    ignoreNULL = T
  )

  observe({
    if (!is.null(factors_check()) & input$continue != "") {
      show(id = "plots", anim = TRUE, animType = "slide")
    } else {
      hide(id = "plots", anim = TRUE, animType = "slide")
    }
  })


  output$nGen <- echarts4r::renderEcharts4r({
    input$continue
    if (!input$swicht) {
      req(info_check())
      number_gen(info_check())
    } else {
      req(shared())
      plot_shared(shared())
    }
  })

  # check conectivity

  output$tbconection <- DT::renderDataTable(
    if (is.null(info_check())) {
      return()
    } else {
      DT::datatable(
        {
          checkConection(
            data = data$data(), 
            genotype = input$genotype,
            trial = input$experiment, 
            response = input$variable,
            all = TRUE
          )
        },
        option = list(
          pageLength = 10, 
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(info_check()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  observeEvent(input$conectivity, {
    showModal(modalDialog(
      title = "Conectivity", size = "l", easyClose = T,
      DT::dataTableOutput(ns("tbconection")),
      footer = tagList(
        downloadButton(ns("downloadData"),
          "Download",
          class = "btn-success",
          style = " color: white ; background-color: #28a745; float:left"
        ),
        modalButton("Cancel")
      )
    ))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Conectivity", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(checkConection(
        data = data$data(),
        genotype = input$genotype,
        trial = input$experiment,
        response = input$variable, all = T
      ),
      file,
      row.names = FALSE
      )
    }
  )


  # modelo

  prg <- Waiter$new(html = HTML("<center> <span>Initialising</span></center>"))

  Modelo <- eventReactive(input$run,
    {
      req(data$data())
      req(input$experiment)

      prg$show()
      dt <- data$data() %>%
        dplyr::filter(.data[[input$experiment]] %in% factors_check()) %>%
        droplevels()
      check <- dup_check(data = dt, input$experiment, input$column, input$row)
      ix <- names(check)[which(unlist(check) >= 1)]
      dt <- dt %>%
        dplyr::filter(!.data[[input$experiment]] %in% ix) %>%
        droplevels()
      dt[, input$experiment] <- as.factor(dt[, input$experiment])

      if (nrow(dt) == 0) {
        shinytoastr::toastr_error(
          title = "Warning:", 
          "Duplicated row and columns", 
          position = "bottom-right",
          progressBar = TRUE
        )
        prg$hide()
        return()
      }

      exp_valid <- levels(dt[, input$experiment])
      niter <- length(exp_valid)
      msgs <- paste("Fitting ", exp_valid, "... ", "(", 1:niter, "/", niter, ")")

      models_list <- list()
      i <- 1
      for (exp in exp_valid) {
        fixed <- input$show_fixed
        random <- input$show_random

        if (!is.null(fixed)) {
          if (!is.null(input$fix_traits) & exp %in% input$fix_traits) {
            fixed <- input$show_fixed
          } else {
            fixed <- NULL
          }
        }

        if (!is.null(random)) {
          if (!is.null(input$ran_traits) & exp %in% input$ran_traits) {
            random <- input$show_random
          } else {
            random <- NULL
          }
        }

        dt_tmp <- dt %>%
          dplyr::filter(.data[[input$experiment]] %in% exp) %>%
          droplevels()
        models_list[[exp]] <- try({
          SpATS_mrbean(
          data = dt_tmp, 
          response = input$variable,
          genotype = input$genotype,
          col = input$column, 
          row = input$row, 
          segm = FALSE,
          ncols = NULL, 
          nrows = NULL, 
          rep = input$replicate,
          fix_fact = fixed,
          ran_fact = random,
          gen_ran = input$res_ran, 
          covariate = input$covariate,
          clean_out = input$outliers,
          iterations = input$times, 
          checks = input$selected_checks,
          k_clean_out = input$k_clean_out
        )}, silent = TRUE
        )
        if (inherits(models_list[[exp]], "try-error")) {
          message_to_send <- models_list[[exp]]
          models_list[[exp]] <- NULL
          shinytoastr::toastr_error(
            message = message_to_send,
            title = paste0("Error in Exp '", exp, "':"),
            position =  "bottom-full-width",
            progressBar = TRUE
          )
        }
        prg$update(html = HTML(
          "<center>",
          '<div class="dots-loader"></div>',
          "<br>", "<br>", "<br>",
          msgs[i],
          "</center>"
        ))
        i <- i + 1
      }
      prg$hide()
      return(models_list)
    },
    ignoreNULL = FALSE
  )


  observeEvent(input$run, {
    if (!is.null(Modelo())) {
      show(id = "Rlink", anim = TRUE, animType = "slide")
    } else {
      hide(id = "Rlink", anim = TRUE, animType = "slide")
    }
  })

  return(list(
    modelo = Modelo,
    gen_ran = reactive(input$res_ran),
    run = reactive(input$run),
    check_mod = reactive(input$Rlink),
    rLimit = reactive(input$k_clean_out)
  ))
}

## To be copied in the UI
# mod_MSA_ui("MSA_ui_1")

## To be copied in the server
# callModule(mod_MSA_server, "MSA_ui_1")
