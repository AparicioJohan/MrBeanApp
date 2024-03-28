#' spats_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spats_single_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Single-Site Spatial
         Analysis</h1>'),
    fluidRow(
      bs4Dash::box(
        width = 3,
        status = "success",
        solidHeader = FALSE,
        title = tagList(
          icon = icon("braille"),
          "SpATS",
          actionButton(
            inputId = ns("btn"),
            label = tagList(
              icon = icon("question-circle", verify_fa = FALSE), "Guide"
            ),
            style = "color: white ; background-color: #dd4b39",
            class = "btn-danger"
          )
        ),
        rintrojs::introBox(
          selectInput(
            inputId = ns("variable"),
            label = with_red_star("Response Variable"),
            choices = "",
            width = "100%"
          ),
          data.step = 1,
          data.intro = "Select the column that contains
          the phenotypic response variable.",
          data.position = "right",
          color = "red"
        ),
        rintrojs::introBox(
          selectInput(
            inputId = ns("genotipo"),
            label = with_red_star("Genotype"),
            choices = "",
            width = "100%"
          ),
          awesomeCheckbox(
            inputId = ns("res_ran"),
            label = "Random Genotype",
            value = TRUE,
            status = "danger"
          ),
          shinyjs::hidden(
            pickerInput(
              inputId = ns("selected"),
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
          data.step = 2,
          data.intro = "Select the column that contains the genotype IDs.
          Check/Uncheck the box if you want to treat this as a random/fixed
          effect factor in the MLM.",
          data.position = "right",
          color = "red"
        )
      ),
      bs4Dash::box(
        width = 2,
        status = "success",
        solidHeader = FALSE,
        collapsible = TRUE,
        title = tagList(
          icon = icon("table-cells", verify_fa = FALSE), 
          "Coordinates"
        ),
        rintrojs::introBox(
          selectInput(
            inputId = ns("column"),
            label = with_red_star("Column"),
            choices = ""
          ),
          selectInput(
            inputId = ns("fila"),
            label = with_red_star("Row"),
            choices = ""
          ),
          selectInput(
            inputId = ns("replicate"),
            label = "Replicate",
            choices = ""
          ),
          data.step = 3,
          data.intro = "Select the columns in your dataset that contain
          the Row and Column coordinates for the plots in your trial.",
          data.position = "right"
        )
      ),
      bs4Dash::box(
        width = 2,
        title = tagList(icon = icon("tractor"), "Factors"),
        status = "success",
        solidHeader = FALSE,
        collapsible = TRUE,
        rintrojs::introBox(
          selectizeInput(ns("show_fixed"), "Fixed ",
            choices = "", multiple = TRUE
          ),
          selectizeInput(ns("show_random"), "Random",
            choices = "", multiple = TRUE
          ),
          data.step = 4,
          data.intro = "In case you want to include additional qualitative
          variables in the MLM, select them here as either fixed or
          random effect factors.",
          data.position = "bottom",
          color = "red"
        ),
        rintrojs::introBox(
          selectizeInput(
            ns("covariate"),
            "Covariate",
            choices = "",
            multiple = TRUE,
            selected = NULL
          ),
          data.step = 5,
          data.intro = "In case you want to include additional quantitative
         variables in the MLM.",
          data.position = "bottom",
          color = "red"
        )
      ),
      bs4Dash::box(
        width = 2,
        status = "success",
        solidHeader = FALSE,
        title = tagList(icon = icon("tasks", verify_fa = FALSE), "Model"),
        collapsible = TRUE,
        rintrojs::introBox(
          actionButton(
            ns("action"),
            label = "Run Model",
            class = "btn-success",
            style = "display:rigth; color: white ; background-color: #28a745"
          ),
          disabled(
            actionButton(
              ns("inf"),
              label = "Info-Box",
              style = "display:rigth"
            )
          ),
          br(), br(),
          disabled(
            actionButton(
              ns("spatial"),
              label = "Spatial Trend",
              style = "display:rigth"
            )
          ),
          disabled(
            actionButton(ns("LSD"), "LSD")
          ),
          br(), hr(),
          disabled(
            actionButton(ns("tabBut"), "View BLUPs/BLUEs")
          ),
          br(), br(),
          disabled(
            actionButton(ns("coeff"), "Coefficients")
          ),
          br(), br(),
          disabled(
            actionLink(
              inputId = ns("Rlink"),
              label = "Residuals",
              icon = icon("arrow-right"),
              style = "color: #28a745"
            )
          ),
          data.step = 6,
          data.intro = "Use this control panel to run the model and
          display the 3D spatial trend.",
          data.hint = "Good",
          data.position = "bottom-middle-aligned"
        )
      ),
      bs4Dash::box(
        width = 3,
        status = "success",
        solidHeader = FALSE,
        title = tagList(
          icon = icon("sliders-h", verify_fa = FALSE),
          "Segments and Residuals"
        ),
        collapsible = TRUE,
        rintrojs::introBox(
          materialSwitch(
            ns("able"),
            label = "Segments", status = "success"
          ),
          uiOutput(ns("segcol")),
          uiOutput(ns("segrow")),
          data.step = 7,
          data.intro = "Enable this box in case you have a large-scale trial
          (nColumns > 100 ; nRows > 100) to reduce the number of segments for
          the spatial components in the MLM.",
          data.position = "bottom"
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
          max = 4,
          value = 3,
          step = 0.1,
          width = "100%"
        ),
        awesomeCheckbox(
          inputId = ns("outliers"),
          label = "Remove Outliers",
          value = FALSE,
          status = "danger"
        ),
        div(
          id = ns("residuals"),
          numericInput(
            ns("times"),
            "Number of Times to Check",
            value = 1, min = 1, max = 3, step = 1,
            width = "100%"
          )
        )
      )
    )
  )
}

#' spats_single Server Function
#'
#' @noRd
mod_spats_single_server <- function(input, output, session, data) {
  ns <- session$ns


  observeEvent(data$data(), {
    dt <- data$data()
    updateSelectInput(session, "variable",
      choices = names(dt),
      selected = "YdHa_clean"
    )
    updateSelectInput(session,
      "column",
      choices = names(dt),
      selected = "col"
    )
    updateSelectInput(session,
      "fila",
      choices = names(dt),
      selected = "row"
    )
    updateSelectInput(session,
      "replicate",
      choices = names(dt),
      selected = "rep"
    )
    updateSelectInput(session,
      "factor",
      choices = names(dt),
      selected = "rep"
    )
    updateSelectInput(session,
      "genotipo",
      choices = names(dt),
      selected = "line"
    )
    updateSelectInput(session,
      "show_fixed",
      choices = names(dt),
      selected = NULL
    )
    updateSelectInput(session,
      "show_random",
      choices = names(dt),
      selected = NULL
    )
    updateSelectInput(session,
      "covariate",
      choices = names(dt),
      selected = NULL
    )
  })

  observe({
    shinyjs::toggle(
      id = "selected",
      anim = TRUE,
      time = 1,
      animType = "fade",
      condition = input$genotipo != "" & input$res_ran == TRUE
    )
    req(input$genotipo)
    req(data$data())
    req(input$genotipo %in% names(data$data()))
    lvl <- as.character(unique(data$data()[, input$genotipo]))
    updatePickerInput(session, inputId = "selected", choices = lvl)
  })

  output$segcol <- renderUI({
    validate(
      need(input$column != "", " Fill the area Column "),
      need(input$fila != "", " Fill the area Row ")
    )
    dt <- data$data()
    dt$col_f <- factor(dt[, input$column])
    sliderInput(
      ns("segcol"),
      label = "Num of col segments",
      min = 1,
      max = nlevels(dt$col_f) + 30,
      value = nlevels(dt$col_f),
      width = "100%"
    )
  })

  output$segrow <- renderUI({
    validate(
      need(input$column != "", " "),
      need(input$fila != "", " ")
    )
    dt <- data$data()
    dt$row_f <- factor(dt[, input$fila])
    sliderInput(
      ns("segrow"),
      label = "Num of row segments",
      min = 1,
      max = nlevels(dt$row_f) + 30,
      value = nlevels(dt$row_f),
      width = "100%"
    )
  })

  observe({
    variables <- c(input$variable, input$genotipo, input$column, input$fila)
    if (any(variables == "")) {
      sendSweetAlert(
        session = session,
        title = "Warning",
        text = HTML("<center> It's necessary that you fill the fields
                    </center>"),
        type = "warning",
        html = TRUE
      )
    }
  }) %>%
    bindEvent(input$action)

  observe({
    shinyjs::enable("inf")
    shinyjs::enable("spatial")
    shinyjs::enable("tabBut")
    shinyjs::enable("LSD")
    shinyjs::enable("coeff")
    shinyjs::enable("Rlink")
  }) %>%
    bindEvent(input$action)

  observe({
    toggle("segcol", anim = TRUE, time = 1, animType = "fade")
    toggle("segrow", anim = TRUE, time = 1, animType = "fade")
  }) %>%
    bindEvent(input$able)

  observe({
    toggle("residuals", anim = TRUE, time = 1, animType = "fade")
  }) %>%
    bindEvent(input$outliers)

  # GUIA
  observe({
    rintrojs::introjs(
      session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "Skip"
      )
    )
  }) %>%
    bindEvent(input$btn)

  # Modelo SPATS
  observe({
    variables <- c(input$variable, input$genotipo, input$column, input$fila)
    if (any(variables == "")) {
      return()
    } else {
      shinytoastr::toastr_info(
        title = "Fitting model...",
        message = HTML(
          "<div class='overlay'>
          <h2><i class='fa fa-refresh fa-spin'></i>
          </div>
          <h2>"
        ),
        position = "bottom-right",
        progressBar = TRUE,
        closeButton = TRUE,
        timeOut = 1000
      )
    }
  }) %>%
    bindEvent(input$action)

  Modelo <- eventReactive(input$action,
    {
      validate(
        need(input$variable != "", " "),
        need(input$genotipo != "", " "),
        need(input$column != "", " "),
        need(input$fila != "", " ")
      )

      dt <- data$data()
      dupl <- sum(duplicated(dt[, c(input$column, input$fila)]))
      dt$Response <- dt[, input$variable]

      tryCatch(
        {
          if (sum(is.na(dt$Response)) > 0.98 * length(dt$Response)) {
            stop("Missing data in the response")
          }
          if (dupl >= 1) {
            stop("Duplicated row & column coordinates")
          }
          Modelo_SpATS <- SpATS_mrbean(
            data = data$data(),
            response = input$variable,
            genotype = input$genotipo,
            col = input$column,
            row = input$fila,
            segm = input$able,
            ncols = input$segcol,
            nrows = input$segrow,
            rep = input$replicate,
            fix_fact = input$show_fixed,
            ran_fact = input$show_random,
            gen_ran = input$res_ran,
            covariate = input$covariate,
            clean_out = input$outliers,
            iterations = input$times,
            checks = input$selected,
            k_clean_out = input$k_clean_out
          )
          Modelo_SpATS
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Warning:",
            conditionMessage(e),
            position = "bottom-right",
            progressBar = TRUE
          )
        }
      )
      if (!exists("Modelo_SpATS")) Modelo_SpATS <- NULL
      return(Modelo_SpATS)
    },
    ignoreNULL = FALSE
  )

  # Coefficients
  co.spats <- reactive({
    req(Modelo())
    coef <- coef_SpATS(Modelo())
    coef
  })

  output$distTable2 <- DT::renderDataTable(
    if (input$action == 0) {
      return()
    } else {
      DT::datatable(
        {
          co.spats()
        },
        option = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(co.spats()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  observeEvent(input$coeff, {
    showModal(modalDialog(
      title = "Coefficients",
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        DT::dataTableOutput(ns("distTable2")),
        type = 6,
        color = "#28a745"
      ),
      footer = tagList(
        downloadButton(
          ns("downloadData2"),
          "Download Coefficients",
          class = "btn-success",
          style = " color: white ; background-color: #28a745; float:left"
        ),
        modalButton("Cancel")
      )
    ))
  })

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("coeff_SpATS_Model_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(co.spats(), file, row.names = FALSE)
    }
  )

  # BLUPs / BLUEs
  blup <- reactive({
    validate(
      need(input$variable != "", " "),
      need(input$genotipo != "", " "),
      need(input$column != "", " "),
      need(input$fila != "", " ")
    )
    req(Modelo())
    BLUPS <- msa_effects(Modelo())
    BLUPS
  })

  output$distTable <- DT::renderDataTable(
    if (input$action == 0) {
      return()
    } else {
      DT::datatable(
        {
          blup() %>%
            dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(blup()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  observeEvent(input$tabBut, {
    showModal(
      modalDialog(
        title = "BLUPs/BLUEs",
        size = "l",
        easyClose = TRUE,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("distTable")),
          type = 6,
          color = "#28a745"
        ),
        footer = tagList(
          downloadButton(
            ns("downloadData"),
            "Download Predictions",
            class = "btn-success",
            style = " color: white ; background-color: #28a745; float:left"
          ),
          modalButton("Cancel")
        )
      )
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("effects_SpATS_Model_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(blup(), file, row.names = FALSE)
    }
  )


  # LSD ---------------------------------------------------------------------


  lsd <- reactive({
    validate(
      need(input$variable != "", " "),
      need(input$genotipo != "", " "),
      need(input$column != "", " "),
      need(input$fila != "", " ")
    )
    req(Modelo())
    lsd <- LSD(model = Modelo(), data.frame = TRUE)
    lsd
  })

  output$lsd_table <- DT::renderDataTable(
    if (input$action == 0) {
      return()
    } else {
      req(lsd())
      DT::datatable(
        {
          lsd() %>%
            dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(
          scrollX = TRUE,
          dom = "t"
        )
      )
    }
  )

  observeEvent(input$LSD, {
    showModal(
      modalDialog(
        title = "LSD",
        size = "l",
        easyClose = TRUE,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("lsd_table")),
          type = 6,
          color = "#28a745"
        ),
        if (input$res_ran) {
          strong("Genotype should be fixed.")
        },
        footer = tagList(
          modalButton("Cancel")
        )
      )
    )
  })

  return(
    list(
      Modelo = Modelo,
      Effects = blup,
      action = reactive(input$action),
      spatial = reactive(input$spatial),
      res_ran = reactive(input$res_ran),
      inf = reactive(input$inf),
      Rlink = reactive(input$Rlink),
      rLimit = reactive(input$k_clean_out)
    )
  )
}

## To be copied in the UI
# mod_spats_single_ui("spats_single_ui_1")

## To be copied in the server
# callModule(mod_spats_single_server, "spats_single_ui_1")
