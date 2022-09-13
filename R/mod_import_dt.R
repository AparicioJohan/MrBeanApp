#' import_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_dt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        title = tagList(shiny::icon("upload"), "Source"),
        solidHeader = FALSE,
        status = "success",
        maximizable = F,
        closable = F,
        width = 3,
        radioGroupButtons(
          inputId = ns("Id004"),
          choices = c("Example Data" = 1, "Import Data" = 2, "BrAPI" = 3),
          status = "success",
          selected = 1
        ),
        conditionalPanel(
          condition = "input.Id004==1",
          h6("Use the example database to try the different modules of Mr.Bean"),
          ns = ns
        ),
        conditionalPanel(
          condition = "input.Id004==2",
          h6("Import external data preferably csv/txt files."),
          ns = ns
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.Id004==2",
          ns = ns,
          bs4Dash::box(
            title = tagList(
              shiny::icon("file-upload", verify_fa = FALSE), "Import Data"
            ),
            solidHeader = FALSE,
            width = 12,
            status = "success",
            maximizable = T,
            closable = F,
            fileInput(
              inputId = ns("file1"),
              width = "100%",
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
            helpText("Default max. file size is 100MB"),
            prettyCheckbox(
              inputId = ns("header"),
              label = "Include Header?",
              icon = icon("check"),
              outline = TRUE,
              fill = FALSE,
              shape = "square",
              animation = "tada",
              value = TRUE,
              status = "success"
            )
          )
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.Id004==2",
          ns = ns,
          shinyjs::hidden(
            div(
              id = ns("when_file1"),
              bs4Dash::box(
                title = tagList(shiny::icon("wrench"), "Attributes"),
                solidHeader = FALSE,
                maximizable = T,
                closable = F,
                radioButtons(
                  inputId = ns("miss"),
                  label = "Missing value character: ",
                  choices = list("NA", "Empty", "Other"),
                  inline = T
                ),
                conditionalPanel(
                  condition = "input.miss=='Other'",
                  ns = ns,
                  textInput(ns("datamiss"),
                    label = "String",
                    width = "100%"
                  )
                ),
                selectInput(
                  inputId = ns("sep"),
                  label = "Cell separation character:",
                  choices = list(
                    Tab = "\t",
                    Comma = ",",
                    Semicolon = ";",
                    "Space" = " "
                  ),
                  selected = ";",
                  width = "100%"
                ),
                uiOutput(ns("oshet")),
                width = 12,
                status = "success"
              )
            )
          )
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.Id004==2",
          ns = ns,
          shinyjs::hidden(
            div(
              id = ns("when_file2"),
              bs4Dash::box(
                title = tagList(shiny::icon("filter"), "Subset"),
                solidHeader = FALSE,
                maximizable = T,
                closable = F,
                prettyCheckbox(
                  inputId = ns("subset"),
                  label = "Select a data subset",
                  icon = icon("check"),
                  outline = TRUE,
                  fill = FALSE,
                  shape = "square",
                  animation = "tada",
                  value = FALSE,
                  status = "success"
                ),
                selectInput(
                  inputId = ns("varsubset"),
                  width = "100%",
                  label = tagList(
                    "Subset variable",
                    tags$a(icon("exclamation-circle", verify_fa = FALSE))
                  ),
                  choices = ""
                ),
                selectInput(
                  inputId = ns("levelessub"),
                  multiple = T,
                  width = "100%",
                  label = tagList(
                    "Which level?",
                    tags$a(icon("exclamation-circle", verify_fa = FALSE))
                  ),
                  choices = ""
                ),
                width = 12,
                status = "success"
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.Id004==3",
      ns = ns,
      fluidRow(
        column(
          width = 4,
          fluidRow(
            bs4Dash::box(
              title = tagList(
                shiny::icon("question-circle", verify_fa = FALSE), "Help"
              ),
              solidHeader = FALSE,
              width = 12,
              status = "success",
              h3("How to connect BrAPI in MrBean?"),
              hr(),
              includeHTML(
                system.file("app/www/icon.html",
                  package = "MrBean"
                )
              )
            )
          )
        ),
        column(
          width = 4,
          fluidRow(
            bs4Dash::box(
              title = tagList(shiny::icon("users"), "BMS"),
              solidHeader = FALSE,
              width = 12,
              status = "success",
              textInput(
                inputId = ns("urlbms"),
                label = tagList(
                  shiny::icon("server"),
                  "Server",
                  tooltip(
                    icon("question-circle", verify_fa = FALSE),
                    title = "For example:
                     https://cassavabase.org
                     https://sweetpotatobase.org
                     https://bms.ciat.cgiar.org/",
                    placement = "top"
                  )
                ),
                value = "https://bms.ciat.cgiar.org/ibpworkbench/controller/auth/login",
                width = "100%"
              ),
              awesomeCheckbox(
                inputId = ns("no_auth"),
                label = "No authentication required?",
                value = FALSE,
                status = "danger"
              ),
              prettyRadioButtons(
                inputId = ns("engine"),
                label = "Engine:",
                choices = c(
                  "BMS" = "bms",
                  "BreedBase" = "breedbase"
                ),
                icon = icon("check"),
                inline = TRUE,
                bigger = TRUE,
                status = "success",
                animation = "jelly"
              ),
              conditionalPanel(
                condition = "input.no_auth==false",
                ns = ns,
                textInput(
                  ns("user"),
                  label = tagList(shiny::icon("user"), "User:"),
                  placeholder = "username",
                  width = "100%"
                ),
                passwordInput(
                  ns("password"),
                  label = tagList(shiny::icon("key"), "Password:"),
                  width = "100%",
                  placeholder = "*****************"
                )
              ),
              actionButton(
                ns("mysql"),
                label = "Conect",
                icon = icon("sync", verify_fa = FALSE)
              ),
              strong(
                a(
                  "Can't Log In?",
                  href = "http://bms.ciat.cgiar.org:48080/ibpworkbench/controller/auth/login"
                )
              )
            )
          )
        ),
        column(
          width = 4,
          fluidRow(
            bs4Dash::box(
              title = tagList(shiny::icon("cogs", verify_fa = FALSE), "Information"),
              status = "success",
              width = 12,
              solidHeader = FALSE,
              selectInput(
                inputId = ns("Id008"),
                label = "Crops",
                choices = "",
                width = "100%"
              ),
              selectInput(
                inputId = ns("program"),
                label = tagList(
                  "Which program?",
                  tags$a(icon("exclamation-circle", verify_fa = FALSE))
                ),
                choices = "",
                width = "100%"
              ),
              pickerInput(
                inputId = ns("trial"),
                label = tagList(
                  "Which trial?",
                  icon = tooltip(
                    icon("question-circle", verify_fa = FALSE),
                    title = "Trials to select. Can be more than one.",
                    placement = "top"
                  )
                ),
                choices = NULL,
                options = list(
                  `actions-box` = TRUE, size = 5, `live-search` = TRUE
                ),
                multiple = TRUE, width = "100%"
              ),
              pickerInput(
                inputId = ns("study"),
                label = tagList(
                  "Which study?",
                  icon = tooltip(
                    icon("question-circle", verify_fa = FALSE),
                    title = "Studies to select. Can be more than one.",
                    placement = "top"
                  )
                ),
                choices = NULL,
                options = list(
                  `actions-box` = TRUE, size = 5, `live-search` = TRUE
                ),
                multiple = TRUE, width = "100%"
              ),
              fluidRow(
                col_3(),
                col_6(
                  actionBttn(
                    inputId = ns("ok2"),
                    label = "Search!",
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
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        collapsed = F,
        maximizable = T,
        closable = F,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("data")),
          type = 5,
          color = "#28a745"
        ),
        width = 12,
        title = tagList(shiny::icon("file-import"), "Data"),
        status = "success",
        solidHeader = FALSE,
        collapsible = TRUE
      )
    ),
    br()
  )
}

#' import_dt Server Function
#'
#' @noRd
mod_import_dt_server <- function(input, output, session) {
  ns <- session$ns

  observe({
    shinyjs::show(
      id = "when_file1",
      animType = "fade",
      anim = TRUE
    )
    shinyjs::show(
      "when_file2",
      animType = "fade",
      anim = TRUE
    )
  }) %>%
    bindEvent(input$file1)

  output$oshet <- renderUI({
    inFile <- input$file1
    Ext <- tools::file_ext(inFile$datapath)
    req(input$file1, Ext == "xlsx" | Ext == "xls")

    selectInput(
      inputId = ns("sheet"),
      label = "Sheet Excel",
      choices = readxl::excel_sheets(inFile$datapath),
      width = "100%"
    )
  })

  # BMS interface -----------------------------------------------------------

  w <- Waiter$new(
    html = HTML("<center> <div class='ball-loader'></div> </center>"),
    color = transparent(0.3)
  )

  bmscon <- reactive({
    input$mysql
    isolate({
      tryCatch(
        {
          tmpbms <- qbmsbrapi(
            url = input$urlbms,
            username = input$user,
            password = input$password,
            engine = input$engine,
            no_auth = input$no_auth
          )
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:",
            conditionMessage(e),
            position = "bottom-full-width",
            showMethod = "slideDown",
            hideMethod = "hide",
            hideEasing = "linear"
          )
        }
      )
      if (!exists("tmpbms")) tmpbms <- NULL
      return(tmpbms)
    })
  })

  observe({
    if (is.null(bmscon())) {
      shinyalert::shinyalert(
        title = "Incorrect username or password",
        type = "error",
        confirmButtonCol = "#28a745"
      )
    } else {
      shinyalert::shinyalert(
        title = paste0("Welcome to ", input$engine, "!"),
        type = "success",
        text = "",
        confirmButtonCol = "#28a745",
        imageUrl = ifelse(
          input$engine == "bms",
          "www/0.png",
          "www/brapi.png"
        ),
        animation = "slide-from-top"
      )
      updateSelectInput(
        session,
        inputId = "Id008",
        choices = bmscon()$crops,
        selected = "NNNNN"
      )
    }
  }) %>%
    bindEvent(input$mysql)

  programs <- reactive({
    crop <- input$Id008
    tryCatch(
      {
        list_programs <- qbmsprograms(crop = crop)
      },
      error = function(e) {
        shinytoastr::toastr_error(
          title = "Error:",
          conditionMessage(e),
          position = "bottom-full-width",
          showMethod = "slideDown",
          hideMethod = "hide",
          hideEasing = "linear"
        )
      }
    )
    if (!exists("list_programs")) list_programs <- NULL
    return(list_programs[[1]])
  })

  observe({
    if (is.null(programs())) {
      return()
    } else {
      updateSelectInput(
        session,
        inputId = "program",
        choices = programs(),
        selected = "NNNNN"
      )
    }
  }) %>%
    bindEvent(input$Id008, ignoreInit = TRUE)

  trials <- reactive({
    w$show()
    tryCatch(
      {
        list_trials <- qbmstrials(program = input$program)
      },
      error = function(e) {
        shinytoastr::toastr_error(
          title = "Error:",
          conditionMessage(e),
          position = "bottom-full-width",
          showMethod = "slideDown",
          hideMethod = "hide",
          hideEasing = "linear"
        )
        w$hide()
      }
    )
    w$hide()
    if (!exists("list_trials")) list_trials <- NULL
    return(list_trials$trialName)
  })

  observe({
    if (is.null(trials())) {
      options <- ""
    } else {
      options <- trials()
    }
    updatePickerInput(session, inputId = "trial", choices = options)
  }) %>%
    bindEvent(input$program, ignoreInit = TRUE)

  studies <- reactive({
    w$show()
    tryCatch(
      {
        list_studies <- lapply(input$trial, qbmsstudies)
        names(list_studies) <- input$trial
        dt_std <- data.frame(plyr::ldply(list_studies[],
          data.frame,
          .id = "trial"
        ))
      },
      error = function(e) {
        shinytoastr::toastr_error(
          title = "Error:",
          conditionMessage(e),
          position = "bottom-full-width",
          showMethod = "slideDown",
          hideMethod = "hide",
          hideEasing = "linear"
        )
        w$hide()
      }
    )
    w$hide()
    if (!exists("dt_std")) dt_std <- NULL
    return(dt_std)
  })

  observe({
    if (is.null(studies())) {
      options <- ""
    } else {
      options <- studies()[[2]]
    }
    updatePickerInput(session, inputId = "study", choices = options)
  }) %>%
    bindEvent(input$trial, ignoreInit = TRUE)

  # data --------------------------------------------------------------------

  DtReact <- reactive({
    input$ok2
    isolate({
      w$show()
      tryCatch(
        {
          datos <- dataqbms(studies = input$study, dt_studies = studies())
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:",
            conditionMessage(e),
            position = "bottom-full-width",
            showMethod = "slideDown",
            hideMethod = "hide",
            hideEasing = "linear"
          )
          w$hide()
        }
      )
      w$hide()
      if (!exists("datos")) datos <- NULL
      return(datos)
    })
  })


  dataset <- reactive({
    tryCatch(
      {
        data_react(
          file = input$file1,
          choice = input$Id004,
          header = input$header,
          sep = input$sep,
          miss = input$miss,
          string = input$datamiss,
          sheet = input$sheet,
          dataBMS = DtReact()
        )
      },
      error = function(e) {
        shinytoastr::toastr_error(
          title = "Error:",
          conditionMessage(e),
          position = "bottom-full-width",
          showMethod = "slideDown",
          hideMethod = "hide",
          hideEasing = "linear"
        )
      }
    )
  })

  # Subset data
  observe({
    updatePrettyCheckbox(
      session = session,
      inputId = "subset",
      value = F
    )
  }) %>%
    bindEvent(input$file1)

  observe({
    updateSelectInput(
      session,
      "varsubset",
      choices = names(dataset()),
      selected = "NNNNN"
    )
  })

  observe({
    toggle(
      "varsubset",
      anim = TRUE,
      time = 1,
      animType = "fade"
    )
    toggle("levelessub",
      anim = TRUE,
      time = 1,
      animType = "fade"
    )
  }) %>%
    bindEvent(input$subset)

  observe({
    if (input$varsubset != "") {
      lvl <- dataset()[, input$varsubset]
    } else {
      lvl <- ""
    }
    updateSelectInput(
      session,
      "levelessub",
      choices = lvl,
      selected = "NNNNN"
    )
  }) %>%
    bindEvent(input$varsubset, input$subset, ignoreInit = TRUE)

  dataset_sub <- reactive({
    data_subset(
      data = dataset(),
      subset = input$subset,
      variable = input$varsubset,
      level = input$levelessub
    )
  })

  output$data <- DT::renderDataTable({
    DT::datatable(
      {
        dataset_sub()
      },
      option = list(
        pageLength = 3,
        scrollX = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = 0:ncol(dataset_sub())
          )
        )
      ),
      filter = "top",
      selection = "multiple"
    )
  }) %>%
    bindEvent(dataset_sub())

  return(list(data = dataset_sub))
}

## To be copied in the UI
# mod_import_dt_ui("import_dt_ui_1")

## To be copied in the server
# callModule(mod_import_dt_server, "import_dt_ui_1")
