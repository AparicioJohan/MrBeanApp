#' GBLUP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_GBLUP_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">GBLUP</h1>'),
    fluidRow(
      col_3(
        bs4Dash::box(
          title = tagList(shiny::icon("file-upload", verify_fa = FALSE), "Import Data"),
          solidHeader = FALSE,
          width = 12,
          status = "success",
          maximizable = TRUE,
          closable = FALSE,
          fileInput(
            inputId = ns("phenotypic"),
            width = "100%",
            label = tagList(
              "Phenotypic Data",
              icon = tooltip(icon("question-circle", verify_fa = FALSE),
                title = "CSV file with one record per genotype",
                placement = "top"
              )
            ),
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              ".csv",
              ".tsv"
            )
          ),
          helpText("Only csv files."),
          prettyCheckbox(
            inputId = ns("header_phen"),
            label = "Header",
            icon = icon("check"),
            outline = TRUE,
            fill = FALSE,
            shape = "square",
            animation = "tada",
            value = TRUE,
            status = "success"
          ),
          shinyjs::hidden(
            div(
              id = ns("second_data"),
              hr(),
              fileInput(
                inputId = ns("genotypic"),
                width = "100%",
                label = tagList(
                  "Genotypic Data",
                  icon = tooltip(icon("question-circle", verify_fa = FALSE),
                    title = "CSV file with genotypic data in
                             numeric format (-1, 0, 1)",
                    placement = "top"
                  )
                ),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values",
                  "text/tab-separated-values",
                  ".csv",
                  ".tsv"
                )
              ),
              helpText("Numeric format (-1, 0, 1)"),
              prettyCheckbox(
                inputId = ns("header_gen"),
                label = "Header",
                icon = icon("check"),
                outline = TRUE,
                fill = FALSE,
                shape = "square",
                animation = "tada",
                value = TRUE,
                status = "success"
              ),
              actionBttn(
                inputId = ns("ok"),
                icon = icon("sliders-h", verify_fa = FALSE),
                size = "sm",
                label = "View",
                style = "unite",
                color = "warning",
                block = TRUE
              )
            )
          )
        )
      ),
      col_3(
        shinyjs::hidden(
          div(
            id = ns("when_files"),
            fluidRow(
              bs4Dash::box(
                width = 12,
                status = "success",
                solidHeader = FALSE,
                title = tagList(icon = icon("cogs", verify_fa = FALSE), "Components"),
                selectInput(
                  inputId = ns("variables"),
                  label = tagList(
                    "Response Variables",
                    icon = tooltip(
                      icon("question-circle", verify_fa = FALSE),
                      title = "The column with the continous response variable.",
                      placement = "top"
                    )
                  ),
                  choices = "",
                  multiple = TRUE,
                  width = "100%"
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
                selectInput(
                  inputId = ns("Method"),
                  label = tagList("Method",
                    icon = tooltip(icon("question-circle", verify_fa = FALSE),
                      title = "Only GBLUP available",
                      placement = "top"
                    )
                  ),
                  choices = list(
                    GBLUP = "GBLUP"
                  ),
                  width = "100%"
                ),
                actionBttn(
                  inputId = ns("run"),
                  label = "Run!",
                  style = "jelly",
                  color = "success",
                  block = T,
                  icon = icon("check")
                )
              )
            )
          )
        )
      ),
      col_6(
        shinyjs::hidden(
          div(
            id = ns("results"),
            bs4TabCard(
              width = 12,
              id = "tabcard",
              maximizable = T,
              closable = F,
              status = "success",
              side = "left",
              type = "tabs",
              tabPanel(
                title = "Summary",
                icon = icon("circle-arrow-right", verify_fa = FALSE),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("summary_model")),
                  type = 5,
                  color = "#28a745"
                ),
                downloadButton(
                  ns("download_summary"),
                  label = "Download Table",
                  class = "btn-success",
                  style = "color: white ; background-color: #28a745; float:left"
                )
              ),
              tabPanel(
                title = "Plot",
                icon = icon("table-cells", verify_fa = FALSE),
                shinycssloaders::withSpinner(
                  echarts4r::echarts4rOutput(ns("comparison")),
                  type = 5,
                  color = "#28a745"
                )
              ),
              tabPanel(
                title = "Correlation",
                icon = icon("circle-arrow-right", verify_fa = FALSE),
                dropdown(
                  prettyRadioButtons(
                    inputId = ns("type"),
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
                    condition = "input.type=='png'",
                    ns = ns,
                    sliderInput(
                      inputId = ns("png.wid.c"),
                      min = 200, max = 2000, value = 900,
                      label = "Width pixels"
                    ),
                    sliderInput(
                      inputId = ns("png.hei.c"),
                      min = 200, max = 2000, value = 600,
                      label = "Height pixels"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.type=='pdf'",
                    ns = ns,
                    sliderInput(
                      inputId = ns("pdf.wid.c"),
                      min = 2, max = 20, value = 10,
                      label = "Width"
                    ),
                    sliderInput(
                      inputId = ns("pdf.hei.c"),
                      min = 2, max = 20, value = 8,
                      label = "Height"
                    )
                  ),
                  downloadButton(
                    ns("descargar_corr"),
                    "Download Plot",
                    class = "btn-success",
                    style = " color: white ; background-color: #28a745"
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
                  plotOutput(ns("corr")),
                  type = 5,
                  color = "#28a745"
                ),
                materialSwitch(
                  ns("config"),
                  label = "Config Plot",
                  status = "success",
                  right = T,
                  width = "100%"
                ),
                div(
                  id = ns("configuration"),
                  fluidRow(
                    col_3(),
                    col_6(
                      sliderInput(
                        inputId = ns("size"),
                        label = "Text Size",
                        min = 1, max = 8, value = 4, step = 1
                      )
                    ),
                    col_3()
                  ),
                  fluidRow(
                    col_4(
                      colourpicker::colourInput(
                        ns("col1"), "Minimun", "#DB4437"
                      )
                    ),
                    col_4(
                      colourpicker::colourInput(
                        ns("col2"), "Medium", "white"
                      )
                    ),
                    col_4(
                      colourpicker::colourInput(
                        ns("col3"), "Maximun", "#FF9D00"
                      )
                    )
                  )
                )
              ),
              tabPanel(
                title = "GBLUPs", icon = icon("table"),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("gblups_table")),
                  type = 6,
                  color = "#28a745"
                ),
                downloadButton(
                  ns("download_gblups"),
                  "Download Table",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745; float:left"
                )
              ),
              tabPanel(
                title = "Markers",
                icon = icon("signal"),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("markers_table")),
                  type = 6,
                  color = "#28a745"
                ),
                downloadButton(
                  ns("download_markers"),
                  "Download Table",
                  class = "btn-success",
                  style = " color: white ; background-color: #28a745; float:left"
                )
              )
            )
          )
        )
      )
    )
  )
}

#' GBLUP Server Functions
#'
#' @noRd
mod_GBLUP_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    loading <- Waiter$new(
      html = HTML("<center> <div class='ball-loader'></div> </center>
      <br><br><br><br><br><H3><FONT COLOR='grey'>Loading Data...</FONT></H3>"),
      color = transparent(0.3)
    )

    data_read <- reactive({
      req(input$phenotypic)
      req(input$genotypic)
      loading$show()
      tryCatch(
        {
          file_phen <- input$phenotypic
          ext <- tools::file_ext(file_phen$datapath)
          if (!ext %in% c("csv", "CSV")) {
            stop("Only csv allowed in the phenotypic data")
          }
          dt_phenotypic <- read.csv(
            file = file_phen$datapath,
            header = input$header_phen,
            sep = ","
          )
          file_gen <- input$genotypic
          ext <- tools::file_ext(file_gen$datapath)
          if (!ext %in% c("csv", "CSV")) {
            stop("Only csv allowed in the genotypic data")
          }
          dt_genotypic <- read.csv(
            file = file_gen$datapath,
            header = input$header_gen,
            sep = ","
          )
          data_imported <- list(
            dt_phenotypic = dt_phenotypic,
            dt_genotypic = dt_genotypic
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
          loading$hide()
        }
      )
      loading$hide()
      if (!exists("data_imported")) data_imported <- NULL
      return(data_imported)
    })

    observe({
      req(data_read())
      dt <- data_read()[["dt_phenotypic"]]
      updateSelectInput(session, "variables",
        choices = names(dt)[-1], selected = "YdHa_clean"
      )
      updateSelectInput(session, "genotype",
        choices = names(dt)[1], selected = "line"
      )
    })

    output$table <- DT::renderDataTable({
      req(data_read())
      DT::datatable(
        {
          data_read()[["dt_phenotypic"]] %>%
            dplyr::mutate_if(is.numeric, round, 2) %>%
            head()
        },
        option = list(
          pageLength = 2,
          scrollX = TRUE,
          columnDefs = list(
            list(
              className = "dt-center",
              targets = 0:ncol(data_read()[["dt_phenotypic"]])
            )
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    })

    output$matrix <- DT::renderDataTable({
      req(data_read())
      dt <- data_read()[["dt_genotypic"]]
      DT::datatable(
        {
          dt[, 1:5]
        },
        option = list(
          pageLength = 2,
          scrollX = TRUE,
          columnDefs = list(
            list(
              className = "dt-center",
              targets = 0:ncol(dt[, 1:5])
            )
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    })

    observe({
      showModal(modalDialog(
        title = "Data Imported", size = "l", easyClose = T,
        h5("Phenotypic Data"),
        DT::dataTableOutput(ns("table")),
        br(),
        h5("Genotypic Data"),
        DT::dataTableOutput(ns("matrix"))
      ))
    }) %>%
      bindEvent(input$ok)

    observe({
      if (!is.null(data_read())) {
        show(id = "when_files", animType = "fade", anim = TRUE)
      } else {
        hide(id = "when_files", anim = TRUE, animType = "slide")
      }
    })

    observe({
      show(id = "second_data", animType = "fade", anim = TRUE)
    }) %>%
      bindEvent(input$phenotypic)

    w <- Waiter$new(
      html = HTML("<center> <div class='ball-loader'></div> </center>
      <br><br><br><br><br><H3><FONT COLOR='grey'>Please wait...</FONT></H3>"),
      color = transparent(0.3)
    )

    # GBLUP
    modelo <- reactive({
      req(data_read())
      req(input$genotype)
      req(input$variables)
      pheno <- data_read()[["dt_phenotypic"]]
      geno <- data_read()[["dt_genotypic"]]
      if (!input$genotype %in% names(pheno)) {
        return()
      }
      condition <- sum(
        input$variables %in% names(pheno)
      ) == length(input$variables)
      if (!condition) {
        return()
      }
      w$show()
      tryCatch(
        {
          model <- GBLUPs(
            pheno_data = pheno,
            geno_matrix = geno,
            genotype = input$genotype,
            traits = input$variables,
            method = c("GBLUP", "rrBLUP")
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
      w$hide()
      if (!exists("model")) model <- NULL
      return(model)
    }) %>%
      bindEvent(input$run)

    observe({
      if (!is.null(modelo())) {
        show(id = "results", animType = "fade", anim = TRUE)
      } else {
        hide(id = "results", anim = TRUE, animType = "slide")
      }
    }) %>%
      bindEvent(modelo())

    output$summary_model <- DT::renderDataTable({
      req(modelo())
      table <- modelo()$var_comp$GBLUP
      DT::datatable(
        {
          table %>%
            dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(
          pageLength = 5,
          scrollX = TRUE,
          columnDefs = list(
            list(
              className = "dt-center",
              targets = 0:ncol(table)
            )
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    })

    output$download_summary <- downloadHandler(
      filename = function() {
        paste("summary_GBLUP", ".csv", sep = "")
      },
      content = function(file) {
        req(modelo())
        datos <- data.frame(modelo()$var_comp$GBLUP)
        write.csv(datos, file, row.names = FALSE)
      }
    )

    output$comparison <- echarts4r::renderEcharts4r({
      req(modelo())
      table <- modelo()$var_comp$GBLUP
      comp <- table %>%
        e_charts(Trait) %>%
        e_bar(VarE, name = "Residual Variance") %>%
        e_bar(VarG, name = "Genotypic Variance") %>%
        e_legend(
          show = T,
          bottom = "bottom",
          left = "center",
          orient = "horizontal"
        ) %>%
        e_title("Variance Comparison", subtext = "By trait") %>%
        e_tooltip() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords() %>%
        e_grid(left = "20%")
      comp
    })

    output$gblups_table <- DT::renderDataTable({
      req(modelo())
      table <- modelo()$results$GBLUP
      DT::datatable(
        {
          table %>%
            dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(
          pageLength = 5,
          scrollX = TRUE,
          columnDefs = list(
            list(
              className = "dt-center",
              targets = 0:ncol(table)
            )
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    })

    output$download_gblups <- downloadHandler(
      filename = function() {
        paste("GBLUPs_table", ".csv", sep = "")
      },
      content = function(file) {
        req(modelo())
        datos <- data.frame(modelo()$results$GBLUP)
        write.csv(datos, file, row.names = TRUE)
      }
    )

    output$markers_table <- DT::renderDataTable({
      req(modelo())
      table <- modelo()$markers$rrBLUP
      DT::datatable(
        {
          table %>%
            dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(
          pageLength = 5,
          scrollX = TRUE,
          columnDefs = list(
            list(
              className = "dt-center",
              targets = 0:ncol(table)
            )
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    })

    output$download_markers <- downloadHandler(
      filename = function() {
        paste("Markers_table", ".csv", sep = "")
      },
      content = function(file) {
        req(modelo())
        datos <- data.frame(modelo()$markers$rrBLUP)
        write.csv(datos, file, row.names = TRUE)
      }
    )

    output$corr <- renderPlot({
      req(modelo())
      gblups <- modelo()$results$GBLUP
      var_comp <- modelo()$var_comp$GBLUP
      tryCatch(
        {
          gblups <- gblups %>% dplyr::select(-phenotypic)
          if (ncol(gblups) <= 2) stop("Only one trait selected.")
          h2 <- round(var_comp$Genomic_h2, 2)
          names(h2) <- var_comp$Trait
          ggCor(gblups[, -1],
            colours = c(input$col1, input$col2, input$col3),
            Diag = h2, size_text = input$size
          ) +
            ggtitle("Genotypic Correlation")
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

    output$descargar_corr <- downloadHandler(
      filename = function() {
        paste("corrPlot", input$type, sep = ".")
      },
      content = function(file) {
        req(modelo())
        gblups <- modelo()$results$GBLUP
        var_comp <- modelo()$var_comp$GBLUP
        gblups <- gblups %>% dplyr::select(-phenotypic)
        h2 <- round(var_comp$Genomic_h2, 2)
        names(h2) <- var_comp$Trait
        gg <- ggCor(gblups[, -1],
          colours = c(input$col1, input$col2, input$col3),
          Diag = h2,
          size_text = input$size
        ) +
          ggtitle("Genotypic Correlation")
        if (input$type == "png") {
          png(file, width = input$png.wid.c, height = input$png.hei.c)
          print(gg)
          dev.off()
        } else {
          pdf(file, width = input$pdf.wid.c, height = input$pdf.hei.c)
          print(gg)
          dev.off()
        }
      }
    )

    observe({
      toggle("configuration", anim = TRUE, time = 1, animType = "fade")
    }) %>%
      bindEvent(input$config)
  })
}

## To be copied in the UI
# mod_GBLUP_ui("GBLUP_1")

## To be copied in the server
# mod_GBLUP_server("GBLUP_1")
