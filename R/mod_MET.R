#' MET UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MET_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # HTML("<center><img src='www/dashboard.svg' width='40%' height='40%'></center>"),
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Multi-Environmental Analysis with ASReml-R</h1>'),
    # HTML('<h4 style="font-weight: bold; color: #00a65a;">Two-Stage</h4>'),
    fluidRow(
      col_3(
        fluidRow(
          bs4Dash::box(
            title = tagList(shiny::icon("file-upload", verify_fa = FALSE), "Import Data"), solidHeader = FALSE, width = 12, status = "success",
            maximizable = T, closable = F,
            fileInput(
              inputId = ns("file1"), width = "100%",
              label = "Load your database",
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                "text/plain",
                ".csv",
                ".tsv", "xlsx"
              )
            ),
            helpText("Default max. file size is 5MB"),
            prettyCheckbox(
              inputId = ns("header"), label = "Header", icon = icon("check"), outline = TRUE, fill = FALSE, shape = "square",
              animation = "tada", value = TRUE, status = "success"
            ),
            shinyjs::hidden(
              div(
                id = ns("separation"),
                selectInput(
                  inputId = ns("sep"),
                  label = "Cell separation character:",
                  choices = list(
                    Tab = "\t", Comma = ",",
                    Semicolon = ";", "Space" = " "
                  ),
                  selected = ",", width = "100%"
                ),
                actionBttn(
                  inputId = ns("tabBut"), icon = icon("sliders-h", verify_fa = FALSE), size = "sm",
                  label = "View", style = "unite", color = "warning", block = T
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("info"),
            fluidRow(
              bs4InfoBoxOutput(ns("var"), width = 12),
              bs4InfoBoxOutput(ns("gen"), width = 12),
              bs4InfoBoxOutput(ns("exper"), width = 12)
            )
          )
        )
      ),
      col_3(
        shinyjs::hidden(
          div(
            id = ns("when_file1"),
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
                selectInput(
                  inputId = ns("experiment"),
                  label = tagList("Experiment",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "Select the variable with Experiment-ID",
                      placement = "top"
                    )
                  ),
                  choices = "", width = "100%"
                ),
                awesomeCheckbox(
                  inputId = ns("exp_ran"),
                  label = "Random Experiment",
                  value = FALSE, status = "danger"
                ),
                shinyjs::hidden(
                  pickerInput(
                    inputId = ns("selected"),
                    label = tagList("Subset",
                      icon = tooltip(icon("question-circle", verify_fa = FALSE),
                        title = "Select the experiments that you want to analyze.",
                        placement = "top"
                      )
                    ),
                    choices = NULL,
                    options = list(
                      `actions-box` = TRUE, size = 5
                    ),
                    multiple = TRUE, width = "100%"
                  )
                ),
                selectInput(
                  inputId = ns("weight"),
                  label = tagList("Weights",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "Select the variable with weights",
                      placement = "top"
                    )
                  ),
                  choices = "", width = "100%"
                ),
                selectInput(
                  inputId = ns("VC"),
                  label = tagList("Covariance Structure",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "Select the Variance-covariance Structure",
                      placement = "top"
                    )
                  ),
                  choices = list(
                    diag = "diag", corv = "corv",
                    corh = "corh", fa1 = "fa1", fa2 = "fa2", fa3 = "fa3", fa4 = "fa4", corgh = "corgh"
                  ),
                  width = "100%"
                ),
                textInput(ns("workspace"),
                  label = tagList("Workspace",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "128mb / 1gb / 2gb ",
                      placement = "top"
                    )
                  ),
                  value = "128mb"
                ),
                actionBttn(inputId = ns("check"), label = "Check!", style = "jelly", color = "success", block = T, icon = icon("check"))
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
                title = tagList(icon = icon("pagelines"), "Genotypes"), maximizable = T,
                status = "success", solidHeader = FALSE, collapsible = TRUE,
                radioGroupButtons(
                  inputId = ns("visual"),
                  label = "Plot",
                  choices = c("Number", "Shared", "Means"),
                  status = "success",
                  checkIcon = list(
                    yes = icon("ok",
                      lib = "glyphicon"
                    ),
                    no = icon("remove",
                      lib = "glyphicon"
                    )
                  )
                ),
                echarts4r::echarts4rOutput(ns("nGen")),
                fluidRow(
                  col_4(),
                  col_4(
                    actionBttn(
                      inputId = ns("run"),
                      label = "Run Models",
                      style = "unite", size = "sm", block = T,
                      color = "warning", icon = icon("spinner")
                    )
                  ),
                  col_4()
                ),
                shinyjs::hidden(
                  div(
                    id = ns("ok"),
                    actionLink(
                      inputId = ns("Rlink"),
                      label = "Check model!",
                      icon = icon("arrow-right"),
                      style = "color: #28a745"
                    ),
                    br(),
                    tableOutput(ns("INFO"))
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

#' MET Server Function
#'
#' @noRd
mod_MET_server <- function(input, output, session) {
  ns <- session$ns

  dataset <- reactive({
    tryCatch(
      {
        Ext <- tools::file_ext(input$file1$datapath)
        if (!Ext %in% c("csv", "CSV")) {
          return()
        } # if(Ext!="csv")
        read.csv(input$file1$datapath, header = input$header, sep = input$sep)
      },
      error = function(e) {
        shinytoastr::toastr_error(
          title = "Error:", conditionMessage(e), position = "bottom-full-width",
          showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
        )
      }
    )
  })

  observeEvent(input$file1, {
    if (!is.null(dataset())) {
      show(id = "separation", anim = TRUE, animType = "slide")
      show(id = "when_file1", animType = "fade", anim = TRUE)
    } else {
      hide(id = "separation", anim = TRUE, animType = "slide")
      hide(id = "when_file1", anim = TRUE, animType = "slide")
      hide(id = "plots", anim = TRUE, animType = "slide")
    }
  })

  observe({
    shinyjs::toggle(id = "selected", anim = T, time = 1, animType = "fade", condition = input$experiment != "")
    req(input$experiment)
    req(dataset())
    req(input$experiment %in% names(dataset()))
    lvl <- as.character(unique(dataset()[, input$experiment]))
    updatePickerInput(session, inputId = "selected", choices = lvl)
  })


  # Data -------------

  output$table <- DT::renderDataTable({
    req(input$tabBut)
    req(dataset())
    DT::datatable(
      {
        dataset() %>% dplyr::mutate_if(is.numeric, round, 3)
      },
      option = list(pageLength = 10, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:ncol(dataset())))),
      filter = "top",
      selection = "multiple"
    )
  })

  observeEvent(input$tabBut, {
    showModal(modalDialog(
      title = "Raw-Data", size = "l", easyClose = T,
      DT::dataTableOutput(ns("table"))
    ))
  })

  observeEvent(input$file1, {
    req(dataset())
    dt <- dataset()
    updateSelectInput(session, "variable", choices = names(dt), selected = "YdHa_clean")
    updateSelectInput(session, "genotype", choices = names(dt), selected = "line")
    updateSelectInput(session, "experiment", choices = names(dt), selected = "dataset")
    updateSelectInput(session, "weight", choices = names(dt), selected = "weight")
  })

  # Info Boxes

  datafilter <- reactive({
    input$check
    input$file1
    req(dataset())
    if (!input$genotype %in% names(dataset())) {
      return()
    }
    if (!input$experiment %in% names(dataset())) {
      return()
    }
    if (!input$variable %in% names(dataset())) {
      return()
    }
    if (!input$weight %in% names(dataset())) {
      return()
    }

    tmp_data <- dataset()
    if (!is.null(input$selected)) {
      tmp_data <- dataset() %>%
        dplyr::filter(.data[[input$experiment]] %in% input$selected) %>%
        droplevels()
    }

    data <- list(
      data = tmp_data,
      gen = input$genotype,
      trial = input$experiment,
      resp = input$variable,
      weight = input$weight,
      type.gen = input$res_ran,
      type.trial = input$exp_ran,
      vc.model = input$VC,
      workspace = input$workspace
    )
    return(data)
  })


  observeEvent(input$check, {
    if (!is.null(datafilter())) {
      show(id = "plots", anim = TRUE, animType = "slide")
    } else {
      hide(id = "plots", anim = TRUE, animType = "slide")
    }
  })



  observeEvent(input$check, {
    if (!is.null(datafilter())) {
      show(id = "info", anim = TRUE, animType = "slide")
    } else {
      hide(id = "info", anim = TRUE, animType = "slide")
    }
  })

  output$var <- renderbs4InfoBox({
    input$check
    input$file1
    isolate({
      req(datafilter())
      dt <- datafilter()$data
      req(datafilter()$resp %in% names(dt))
      na <- sum(is.na(dt[, datafilter()$resp]))
      co <- sum(!is.na(dt[, datafilter()$resp]))

      var <- paste0("(NA: ", na, " / ", co, ")")
      bs4InfoBox(
        title = paste0("Response: ", datafilter()$resp),
        color = "info", iconElevation = 2,
        value = var,
        icon = shiny::icon("ruler"), elevation = 1
      )
    })
  })

  output$gen <- renderbs4InfoBox({
    input$check
    input$file1
    isolate({
      req(datafilter())
      dt <- datafilter()$data
      req(datafilter()$gen %in% names(dt))
      gen <- length(unique(dt[, datafilter()$gen]))
      bs4InfoBox(
        title = "Number of Genotypes", color = "success", iconElevation = 2,
        value = gen,
        icon = shiny::icon("pagelines"), elevation = 1
      )
    })
  })

  output$exper <- renderbs4InfoBox({
    input$check
    input$file1
    isolate({
      req(datafilter())
      dt <- datafilter()$data
      req(datafilter()$trial %in% names(dt))
      num <- length(unique(dt[, datafilter()$trial]))
      bs4InfoBox(
        title = "Number of Trials", color = "info", iconElevation = 2,
        value = num,
        icon = shiny::icon("sort-amount-down", verify_fa = FALSE), elevation = 1
      )
    })
  })


  # check ----------------------

  w <- Waiter$new(
    html = HTML("<center> <div class='ball-loader'></div> </center>"),
    color = transparent(0.3)
  )

  # info_check <- reactive({
  #   input$check
  #   input$file1
  #   isolate({
  #     req(datafilter())
  #     req(datafilter()$data)
  #     print(datafilter()$data)
  #     w$show()
  #     inf <- check_spats(datafilter()$data, datafilter()$resp, datafilter()$gen, datafilter()$trial, NULL , NULL , two.stage = TRUE)
  #     w$hide()
  #     return(inf)
  #   })
  # })


  # shared <- reactive({
  #   input$check
  #   input$file1
  #   isolate({
  #     req(datafilter())
  #     req(datafilter()$data)
  #     w$show()
  #     inf <- gen_share(datafilter()$data, datafilter()$gen, datafilter()$trial, datafilter()$resp)
  #     w$hide()
  #     return(inf)
  #   })
  # })
  #
  # observe({
  #   print(info_check())
  # })
  #
  output$nGen <- echarts4r::renderEcharts4r({
    input$check
    input$file1
    input$visual
    isolate({
      req(datafilter())
      req(datafilter()$data)
      if (input$visual == "Number") {
        w$show()
        inf <- check_spats(datafilter()$data, datafilter()$resp, datafilter()$gen, datafilter()$trial, NULL, NULL, two.stage = TRUE)
        w$hide()
        req(inf)
        number_gen(inf)
      } else if (input$visual == "Shared") {
        w$show()
        shared <- gen_share(datafilter()$data, datafilter()$gen, datafilter()$trial, datafilter()$resp)
        w$hide()
        req(shared)
        plot_shared(shared)
      } else {
        df <- datafilter()$data %>%
          dplyr::group_by(trial = .data[[datafilter()$trial]]) %>%
          dplyr::summarise(
            mean = round(mean(.data[[datafilter()$resp]], na.rm = T), 2),
            lower = round(mean - sd(.data[[datafilter()$resp]], na.rm = T), 2),
            upper = round(mean + sd(.data[[datafilter()$resp]], na.rm = T), 2)
          )

        df %>%
          echarts4r::e_charts(trial) %>%
          echarts4r::e_bar(mean) %>%
          echarts4r::e_error_bar(lower, upper) %>%
          echarts4r::e_title("Means", subtext = "by experiment") %>%
          echarts4r::e_tooltip() %>%
          echarts4r::e_legend(show = FALSE) %>%
          echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
          echarts4r::e_toolbox_feature(feature = "dataView") %>%
          echarts4r::e_labels() %>%
          echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 65, fontSize = 12, margin = 8)) %>% # rotate
          echarts4r::e_grid(height = "65%", top = "15%")
      }
    })
  })

  # model

  # Single model
  modelo <- reactive({
    input$run
    isolate({
      req(datafilter())
      dt <- datafilter()$data
      comp <- datafilter()
      w$show()
      tryCatch(
        {
          model <- stageMET(
            data = dt,
            gen = conv_null(comp$gen),
            trial = conv_null(comp$trial),
            resp = conv_null(comp$resp),
            type.gen = ifelse(comp$type.gen, "random", "fixed"),
            type.trial = ifelse(comp$type.trial, "random", "fixed"),
            weight = conv_null(comp$weight),
            vc.model = comp$vc.model,
            workspace = comp$workspace
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

  observeEvent(input$run, {
    if (!is.null(modelo())) {
      show(id = "ok", anim = TRUE, animType = "slide")
    } else {
      hide(id = "ok", anim = TRUE, animType = "slide")
    }
  })

  # observeEvent(input$run,{
  #   print(modelo()$corrM)
  #   print(modelo()$vcovM)
  #   print(modelo()$gfit)
  #   print(head(modelo()$predictions))
  # })

  output$INFO <- function() {
    input$run
    isolate({
      req(modelo())
      gt <- data.frame(modelo()$gfit)
      gfit <- matrix(NA, ncol = 5, nrow = 1)
      gfit[1, 1] <- input$VC
      gfit[1, 2] <- round(gt$n.VC, 3)
      gfit[1, 3] <- round(gt$logL, 3)
      gfit[1, 4] <- round(gt$AIC, 3)
      gfit[1, 5] <- round(gt$BIC, 3)

      colnames(gfit) <- c("MODEL", "n.VC", "logL", "AIC", "BIC")
      gfit <- data.frame(gfit)
      gfit %>%
        dplyr::select(MODEL, everything()) %>%
        kableExtra::kable(escape = F, align = "c") %>%
        kableExtra::kable_styling(c("hover", "responsive", "condensed"), full_width = T, position = "center")
    })
  }


  observeEvent(input$VC,
    {
      txt <- input$VC
      url <- paste0("'www/", txt, ".png'")
      size <- ifelse(txt == "fa2", "l", "m")

      showModal(modalDialog(
        title = "Covariance-Matrix Selected:", size = size, easyClose = T,
        HTML(paste0("<img src=", url, "width='100%' align='center'>"))
      ))
    },
    ignoreInit = T
  )

  return(list(
    model = modelo,
    run = reactive(input$run),
    check_mod = reactive(input$Rlink),
    datafilter = datafilter
  ))
}

## To be copied in the UI
# mod_MET_ui("MET_ui_1")

## To be copied in the server
# callModule(mod_MET_server, "MET_ui_1")
