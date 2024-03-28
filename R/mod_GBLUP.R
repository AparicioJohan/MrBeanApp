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
          title = tagList(
            shiny::icon("file-upload", verify_fa = FALSE),
            "Import Data",
            actionBttn(
              inputId = ns("example"),
              label = NULL,
              style = "material-circle",
              color = "warning",
              size = "xs",
              icon = icon("question")
            )
          ),
          solidHeader = FALSE,
          width = 12,
          status = "success",
          maximizable = TRUE,
          closable = FALSE,
          fileInput(
            inputId = ns("phenotypic"),
            width = "100%",
            label = with_red_star("Phenotypic Data"),
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
                label = with_red_star("Genotypic Data"),
                multiple = TRUE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values",
                  "text/tab-separated-values",
                  ".csv",
                  ".tsv"
                )
              ),
              helpText("Numeric format (-1, 0, 1)."),
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
        ),
        shinyjs::hidden(
          div(
            id = ns("marker_details"),
            bs4InfoBoxOutput(ns("info_gen"), width = 12)
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
                title = tagList(
                  icon = icon("cogs", verify_fa = FALSE), 
                  "Components"
                ),
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
                pickerInput(
                  inputId = ns("filter_gen"),
                  label = tagList(
                    "Ignore Genotypes?",
                    icon = tooltip(
                      icon("question-circle", verify_fa = FALSE),
                      title = "Subset of genotypes",
                      placement = "top"
                    )
                  ),
                  choices = NULL,
                  options = list(
                    `actions-box` = TRUE, size = 5, `live-search` = TRUE
                  ),
                  multiple = TRUE, width = "100%"
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
              maximizable = T,
              closable = F,
              status = "success",
              side = "left",
              type = "tabs",
              tabPanel(
                title = "Plot",
                icon = icon("table-cells", verify_fa = FALSE),
                shinycssloaders::withSpinner(
                  echarts4r::echarts4rOutput(ns("comparison")),
                  type = 5,
                  color = "#28a745"
                ),
                materialSwitch(
                  ns("plot_type"),
                  label = "Variance or Heritability",
                  status = "success",
                  right = T,
                  width = "100%"
                )
              ),
              tabPanel(
                title = "Summary",
                icon = icon("circle-arrow-right", verify_fa = FALSE),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("summary_model")),
                  type = 5,
                  color = "#28a745"
                ),
                fluidRow(
                  col_3(),
                  col_6(
                    actionBttn(
                      inputId = ns("obs_pred"),
                      icon = icon("check", verify_fa = FALSE),
                      size = "sm",
                      label = "View",
                      style = "unite",
                      color = "warning",
                      block = T
                    )
                  ),
                  col_3()
                ),
                downloadButton(
                  ns("download_summary"),
                  label = "Download Table",
                  class = "btn-success",
                  style = "color: white ; background-color: #28a745; float:left"
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
                fluidRow(
                  col_2(),
                  col_4(
                    actionBttn(
                      inputId = ns("dendo"),
                      icon = icon("check", verify_fa = FALSE),
                      size = "sm",
                      label = "Dendrogram",
                      style = "unite",
                      color = "warning",
                      block = TRUE
                    )
                  ),
                  col_4(
                    actionBttn(
                      inputId = ns("pca"),
                      icon = icon("check", verify_fa = FALSE),
                      size = "sm",
                      label = "PCA",
                      style = "unite",
                      color = "warning",
                      block = TRUE
                    )
                  )
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
                    col_3(
                      rep_br(1),
                      prettyCheckbox(
                        inputId = ns("include_predicted"),
                        label = "Include Predictions?",
                        icon = icon("check"),
                        outline = TRUE,
                        fill = FALSE,
                        shape = "square",
                        animation = "tada",
                        value = TRUE,
                        status = "success",
                        bigger = TRUE
                      )
                    ),
                    col_2(),
                    col_6(
                      sliderInput(
                        inputId = ns("size"),
                        label = "Text Size",
                        min = 1, max = 8, value = 4, step = 1
                      )
                    )
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
                        ns("col3"), "Maximun", "#4285F4"
                      )
                    )
                  )
                ),
                downloadButton(
                  ns("desire"),
                  label = "Desire-Gain File",
                  class = "btn-success",
                  style = "color: white ; background-color: #28a745; float:left"
                )
              ),
              tabPanel(
                title = "GBLUPs", icon = icon("table"),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("gblups_table")),
                  type = 6,
                  color = "#28a745"
                ),
                fluidRow(
                  col_3(),
                  col_6(
                    br(),
                    actionBttn(
                      inputId = ns("plot_reliability"),
                      icon = icon("check", verify_fa = FALSE),
                      size = "sm",
                      label = "Reliability",
                      style = "unite",
                      color = "warning",
                      block = T
                    )
                  ),
                  col_3()
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
                fluidRow(
                  col_3(),
                  col_6(
                    br(),
                    actionBttn(
                      inputId = ns("markers_plot"),
                      icon = icon("check", verify_fa = FALSE),
                      size = "sm",
                      label = "Markers",
                      style = "unite",
                      color = "warning",
                      block = T
                    )
                  ),
                  col_3()
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
          dt_phenotypic <- data.table::fread(
            file = file_phen$datapath,
            header = input$header_phen,
            sep = ","
          ) %>%
            as.data.frame()
          file_gen <- input$genotypic
          num_files <- length(file_gen$datapath)
          if (num_files == 1) {
            ext <- tools::file_ext(file_gen$datapath)
            if (!ext %in% c("csv", "CSV")) {
              stop("Only csv allowed in the genotypic data")
            }
            dt_genotypic <- data.table::fread(
              file = file_gen$datapath,
              header = input$header_gen,
              sep = ","
            ) %>%
              as.data.frame()
            dt_genetic_map <- NULL
          } else if (num_files == 2) {
            ext <- tools::file_ext(file_gen$datapath[[1]])
            if (!ext %in% c("csv", "CSV")) {
              stop("Only csv allowed in the genotypic data")
            }
            tmp_list <- list(
              a = data.table::fread(
                file = file_gen$datapath[[1]],
                header = input$header_gen,
                sep = ","
              ) %>%
                as.data.frame(),
              b = data.table::fread(
                file = file_gen$datapath[[2]],
                header = input$header_gen,
                sep = ","
              ) %>%
                as.data.frame()
            )
            dim_a <- ncol(tmp_list$a)
            dim_b <- ncol(tmp_list$b)
            if (dim_a > dim_b) {
              dt_genotypic <- tmp_list$a
              dt_genetic_map <- tmp_list$b
            } else {
              dt_genotypic <- tmp_list$b
              dt_genetic_map <- tmp_list$a
            }
          } else {
            stop("The maximun number of files should be two (marker data and
                 genetic map.")
          }
          data_imported <- list(
            dt_phenotypic = dt_phenotypic,
            dt_genotypic = dt_genotypic,
            dt_map = dt_genetic_map
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

    output$info_gen <- renderbs4InfoBox({
      req(data_read())
      dt_gen <- data_read()[["dt_genotypic"]]
      n_markers <- ncol(dt_gen) - 1
      n_gen <- nrow(dt_gen)
      bs4InfoBox(
        title = "Dimension of G:",
        color = "success",
        iconElevation = 2,
        value = paste0("(Ind = ", n_gen, ", Markers = ", n_markers, ")"),
        icon = shiny::icon("pagelines"),
        elevation = 1
      )
    })

    observe({
      if (!is.null(data_read())) {
        show(id = "marker_details", animType = "fade", anim = TRUE)
      } else {
        hide(id = "marker_details", anim = TRUE, animType = "slide")
      }
    })

    observe({
      req(data_read())
      tryCatch(
        {
          dt <- data_read()[["dt_phenotypic"]]
          lvl <- as.character(dt[, input$genotype])
          updatePickerInput(session, inputId = "filter_gen", choices = lvl)
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
    }) %>%
      bindEvent(input$genotype)

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
          if (!is.null(input$filter_gen)) {
            pheno <- pheno %>%
              dplyr::filter(!.data[[input$genotype]] %in% input$filter_gen)
          }
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
        utils::write.csv(datos, file, row.names = FALSE)
      }
    )

    output$comparison <- echarts4r::renderEcharts4r({
      req(modelo())
      table <- modelo()$var_comp$GBLUP
      if (input$plot_type) {
        comp <- table %>%
          e_charts(Trait) %>%
          e_bar(Genomic_h2, name = "Genomic Heritability") %>%
          e_legend(
            show = T,
            bottom = "bottom",
            left = "center",
            orient = "horizontal"
          ) %>%
          e_title("Heritability", subtext = "By trait") %>%
          e_tooltip() %>%
          e_toolbox_feature(feature = "saveAsImage") %>%
          e_toolbox_feature(feature = "dataView") %>%
          e_flip_coords() %>%
          e_grid(left = "20%")
        comp
      } else {
        comp <- table %>%
          dplyr::mutate(
            Total = VarG + VarE,
            VarG = VarG / Total,
            VarE = VarE / Total
          ) %>%
          e_charts(Trait) %>%
          e_bar(VarE, name = "Residual Variance") %>%
          e_bar(VarG, name = "Genotypic Variance") %>%
          e_legend(
            show = T,
            bottom = "bottom",
            left = "center",
            orient = "horizontal"
          ) %>%
          e_title("Variance Components", subtext = "As a percentage") %>%
          e_tooltip() %>%
          e_toolbox_feature(feature = "saveAsImage") %>%
          e_toolbox_feature(feature = "dataView") %>%
          e_flip_coords() %>%
          e_grid(left = "20%")
        comp
      }
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
        utils::write.csv(datos, file, row.names = TRUE)
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
        utils::write.csv(datos, file, row.names = TRUE)
      }
    )

    output$corr <- renderPlot({
      req(modelo())
      if (input$include_predicted) {
        type_pred <- c("fit", "prediction")
      } else {
        type_pred <- "fit"
      }
      gblups <- modelo()$results$GBLUP %>%
        dplyr::filter(type %in% type_pred) %>%
        dplyr::select(trait, level, predicted.value) %>%
        tidyr::spread(trait, value = "predicted.value")
      var_comp <- modelo()$var_comp$GBLUP
      tryCatch(
        {
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
        if (input$include_predicted) {
          type_pred <- c("fit", "prediction")
        } else {
          type_pred <- "fit"
        }
        gblups <- modelo()$results$GBLUP %>%
          dplyr::filter(type %in% type_pred) %>%
          dplyr::select(trait, level, predicted.value) %>%
          tidyr::spread(trait, value = "predicted.value")
        var_comp <- modelo()$var_comp$GBLUP
        h2 <- round(var_comp$Genomic_h2, 2)
        names(h2) <- var_comp$Trait
        gg <- ggCor(gblups[, -1],
          colours = c(input$col1, input$col2, input$col3),
          Diag = h2,
          size_text = input$size
        ) +
          ggtitle("Genotypic Correlation")
        if (input$type == "png") {
          grDevices::png(file, width = input$png.wid.c, height = input$png.hei.c)
          print(gg)
          grDevices::dev.off()
        } else {
          grDevices::pdf(file, width = input$pdf.wid.c, height = input$pdf.hei.c)
          print(gg)
          grDevices::dev.off()
        }
      }
    )

    observe({
      toggle("configuration", anim = TRUE, time = 1, animType = "fade")
    }) %>%
      bindEvent(input$config)


    output$plot_dend <- renderPlot({
      input$dendo
      input$box
      input$horiz
      input$size_dendo
      input$size_line
      input$num_k
      req(modelo())
      isolate({
        if (input$include_predicted) {
          type_pred <- c("fit", "prediction")
        } else {
          type_pred <- "fit"
        }
        gblups <- modelo()$results$GBLUP %>%
          dplyr::filter(type %in% type_pred) %>%
          dplyr::select(trait, level, predicted.value) %>%
          tidyr::spread(trait, value = "predicted.value")
        tryCatch(
          {
            if (ncol(gblups) <= 2) stop("Only one trait selected.")
            corr <- cor(gblups[, -1], use = "pairwise.complete.obs")
            validate(
              need(
                input$num_k <= ncol(corr),
                "The number of clusters should be less"
              )
            )
            res <- factoextra::hcut(corr, k = input$num_k, stand = FALSE)
            dend <- factoextra::fviz_dend(
              res,
              rect = input$box,
              cex = input$size_dendo,
              lwd = input$size_line, main = "Cluster Dendrogram",
              horiz = input$horiz
            )
            dend
          },
          error = function(e) {
            shinytoastr::toastr_error(
              title = "Error in Dendrogram:",
              conditionMessage(e),
              position = "bottom-full-width",
              showMethod = "slideDown",
              hideMethod = "hide",
              hideEasing = "linear"
            )
          }
        )
      })
    })

    observeEvent(input$dendo,
      {
        showModal(modalDialog(
          title = "Dendrogram", size = "l", easyClose = T,
          dropdown(
            prettyRadioButtons(
              inputId = ns("filetype3"),
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
              condition = "input.filetype3=='png'", ns = ns,
              sliderInput(
                inputId = ns("png.wid.den"),
                min = 200, max = 2000, value = 900,
                label = "Width pixels"
              ),
              sliderInput(
                inputId = ns("png.hei.den"),
                min = 200, max = 2000, value = 600,
                label = "Height pixels"
              )
            ),
            conditionalPanel(
              condition = "input.filetype3=='pdf'", ns = ns,
              sliderInput(
                inputId = ns("pdf.wid.den"),
                min = 2, max = 20, value = 10, label = "Width"
              ),
              sliderInput(
                inputId = ns("pdf.hei.den"),
                min = 2, max = 20, value = 8, label = "Height"
              )
            ),
            downloadButton(ns("descargar3"), "Download Plot",
              class = "btn-success",
              style = " color: white ; background-color: #28a745"
            ), br(),
            animate = shinyWidgets::animateOptions(
              enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
              exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
            ),
            style = "unite",
            icon = icon("gear", verify_fa = FALSE),
            tooltip = tooltipOptions(title = "Click to Download!"),
            status = "warning",
            width = "300px"
          ),
          shinycssloaders::withSpinner(
            plotOutput(ns("plot_dend")),
            type = 6,
            color = "#28a745"
          ),
          icon = icon("circle-arrow-right", verify_fa = FALSE),
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

    output$descargar3 <- downloadHandler(
      filename = function() {
        paste("dendrogram", input$filetype3, sep = ".")
      },
      content = function(file) {
        req(modelo())
        if (input$include_predicted) {
          type_pred <- c("fit", "prediction")
        } else {
          type_pred <- "fit"
        }
        gblups <- modelo()$results$GBLUP %>%
          dplyr::filter(type %in% type_pred) %>%
          dplyr::select(trait, level, predicted.value) %>%
          tidyr::spread(trait, value = "predicted.value")
        tryCatch(
          {
            if (ncol(gblups) <= 2) stop("Only one trait selected.")
            corr <- cor(gblups[, -1], use = "pairwise.complete.obs")
            validate(
              need(
                input$num_k <= ncol(corr),
                "The number of clusters should be less"
              )
            )
            res <- factoextra::hcut(corr, k = input$num_k, stand = FALSE)
            dend <- factoextra::fviz_dend(
              res,
              rect = input$box,
              cex = input$size_dendo,
              lwd = input$size_line, main = "Cluster Dendrogram",
              horiz = input$horiz
            )
            dend
          },
          error = function(e) {
            shinytoastr::toastr_error(
              title = "Error in Dendrogram:",
              conditionMessage(e),
              position = "bottom-full-width",
              showMethod = "slideDown",
              hideMethod = "hide",
              hideEasing = "linear"
            )
          }
        )
        if (input$filetype3 == "png") {
          grDevices::png(file, width = input$png.wid.den, height = input$png.hei.den)
          print(dend)
          grDevices::dev.off()
        } else {
          grDevices::pdf(file, width = input$pdf.wid.den, height = input$pdf.hei.den)
          print(dend)
          grDevices::dev.off()
        }
      }
    )

    output$observed_pred <- renderPlot({
      req(modelo())
      req(input$selected)
      trait_selected <- input$selected
      tryCatch(
        {
          BLUPS <- modelo()$results$GBLUP %>%
            dplyr::filter(trait %in% trait_selected) %>%
            dplyr::select(type, level, observed, predicted.value)

          plot_corr <- BLUPS %>%
            ggplot2::ggplot(
              ggplot2::aes(
                x = observed,
                y = predicted.value
              )
            ) +
            ggplot2::geom_point(
              alpha = input$alpha,
              size = input$point_size,
              color = "grey"
            ) +
            ggplot2::theme_bw(base_size = input$legend_size) +
            ggplot2::labs(x = "Observed", y = "Predicted") +
            ggpubr::stat_cor(size = input$point_size) +
            ggplot2::geom_smooth(method = "lm", se = FALSE) +
            ggplot2::coord_fixed()
          plot_corr
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

    observeEvent(input$obs_pred,
      {
        showModal(modalDialog(
          title = "Accuracy",
          size = "l",
          easyClose = T,
          pickerInput(
            inputId = ns("selected"),
            label = "Trait",
            choices = input$variables,
            selected = input$variables[1],
            options = list(
              title = "Select a trait...",
              size = 5
            ),
            width = "fit",
            inline = TRUE
          ),
          shinycssloaders::withSpinner(
            plotOutput(ns("observed_pred")),
            type = 6,
            color = "#28a745"
          ),
          fluidRow(
            col_4(
              sliderTextInput(
                inputId = ns("point_size"), label = "Point Size:",
                choices = c(1:10),
                grid = TRUE, selected = 5, width = "100%"
              )
            ),
            col_4(
              sliderTextInput(
                inputId = ns("legend_size"), label = "Legend Size:",
                choices = c(8:20),
                grid = TRUE, selected = 15, width = "100%"
              )
            ),
            col_4(
              sliderTextInput(
                inputId = ns("alpha"), label = "Alpha:",
                choices = c(0.2, 0.4, 0.6, 0.8, 1),
                grid = TRUE, selected = 0.6, width = "100%"
              )
            )
          )
        ))
      },
      ignoreInit = T,
      ignoreNULL = T
    )

    output$pred_reliab <- plotly::renderPlotly({
      req(modelo())
      req(input$selected_to_relia)
      trait_selected <- input$selected_to_relia
      tryCatch(
        {
          table_dt <- modelo()$results$GBLUP %>%
            dplyr::filter(trait %in% trait_selected) %>%
            dplyr::select(type, level, predicted.value, reliability)
          plot_corr <- table_dt %>%
            ggplot2::ggplot(
              ggplot2::aes(
                x = reliability,
                y = predicted.value,
                color = type,
                label = level
              )
            ) +
            ggplot2::geom_point(
              alpha = input$alpha_rel,
              size = input$point_size_rel
            ) +
            ggplot2::theme_bw(base_size = input$legend_size_rel) +
            ggplot2::labs(x = "Reliability", y = "Predicted") +
            ggplot2::geom_vline(xintercept = 0, linetype = 2, color = "grey")
          plot_corr
          plotly::ggplotly(plot_corr)
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


    observeEvent(input$plot_reliability,
      {
        showModal(modalDialog(
          title = "Reliability",
          size = "l",
          easyClose = T,
          pickerInput(
            inputId = ns("selected_to_relia"),
            label = "Trait",
            choices = input$variables,
            selected = input$variables[1],
            options = list(
              title = "Select a trait...",
              size = 5
            ),
            width = "fit",
            inline = TRUE
          ),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("pred_reliab")),
            type = 6,
            color = "#28a745"
          ),
          fluidRow(
            col_4(
              sliderTextInput(
                inputId = ns("point_size_rel"), label = "Point Size:",
                choices = c(1:10),
                grid = TRUE, selected = 2, width = "100%"
              )
            ),
            col_4(
              sliderTextInput(
                inputId = ns("legend_size_rel"), label = "Legend Size:",
                choices = c(6:15),
                grid = TRUE, selected = 12, width = "100%"
              )
            ),
            col_4(
              sliderTextInput(
                inputId = ns("alpha_rel"), label = "Alpha:",
                choices = c(0.2, 0.4, 0.6, 0.8, 1),
                grid = TRUE, selected = 0.4, width = "100%"
              )
            )
          )
        ))
      },
      ignoreInit = T,
      ignoreNULL = T
    )

    output$plot_pca <- renderPlot({
      input$pca
      input$type
      input$type_plot
      input$number
      input$ind_pca
      input$var_pca
      input$scale
      input$invisible_pca
      input$include_predicted_pca
      isolate({
        req(modelo())
        if (input$include_predicted_pca) {
          type_pred <- c("fit", "prediction")
        } else {
          type_pred <- "fit"
        }
        data <- modelo()$results$GBLUP %>%
          dplyr::filter(type %in% type_pred) %>%
          dplyr::select(trait, level, predicted.value) %>%
          tidyr::spread(trait, value = "predicted.value") %>%
          tibble::column_to_rownames("level")
        tryCatch(
          {
            res.pca <- stats::prcomp(data, scale. = T)
            if (input$type_plot == "var") {
              res.pca.non <- stats::prcomp(data, scale. = input$scale)
              factoextra::fviz_pca_var(
                res.pca.non,
                col.var = "steelblue",
                repel = TRUE,
                alpha.var = 0.2,
                labelsize = 5
              )
            } else if (input$type_plot == "ind") {
              top <- as.numeric(input$number)
              if (top > nrow(data)) {
                stop("You have to select a lower number for making this plot.")
              }
              fa12_scores <- res.pca$x[, 1:2] %>%
                data.frame() %>%
                tibble::rownames_to_column("Genotypes")
              fa12_scores$Score <- sqrt(fa12_scores$PC1^2 + fa12_scores$PC2^2)
              gen <- fa12_scores %>%
                dplyr::arrange(dplyr::desc(Score)) %>%
                dplyr::top_n(top) %>%
                dplyr::pull(Genotypes)
              factoextra::fviz_pca_ind(
                res.pca,
                repel = TRUE,
                alpha.ind = 0.5,
                select.ind = list(name = gen),
                labelsize = 4
              )
            } else {
              geom.ind <- c("point", "text")
              geom.var <- c("arrow", "text")
              invisible <- "none"
              if (!is.null(input$ind_pca)) geom.ind <- input$ind_pca
              if (!is.null(input$var_pca)) geom.var <- input$var_pca
              if (!is.null(input$invisible_pca)) invisible <- input$invisible_pca
              factoextra::fviz_pca_biplot(
                res.pca,
                repel = FALSE,
                alpha.ind = 0.5,
                geom.ind = geom.ind,
                geom.var = geom.var,
                invisible = invisible
              )
            }
          },
          error = function(e) {
            shinytoastr::toastr_error(
              title = "Error in PCA:",
              conditionMessage(e),
              position = "bottom-full-width",
              showMethod = "slideDown",
              hideMethod = "hide",
              hideEasing = "linear"
            )
          }
        )
      })
    })

    observeEvent(input$pca,
      {
        showModal(modalDialog(
          title = tagList(icon = icon("chart-pie"), "PCA"),
          size = "l",
          easyClose = TRUE,
          prettyRadioButtons(
            inputId = ns("type_plot"),
            label = "Choose:",
            choices = c(
              "Biplot" = "bip",
              "Variables" = "var",
              "Individuals" = "ind"
            ),
            selected = "bip",
            inline = TRUE,
            status = "danger",
            fill = TRUE,
            icon = icon("check"),
            animation = "jelly"
          ),
          prettyCheckbox(
            inputId = ns("include_predicted_pca"),
            label = "Include Predictions?",
            icon = icon("check"),
            outline = TRUE,
            fill = FALSE,
            shape = "square",
            animation = "tada",
            value = TRUE,
            status = "success",
            bigger = TRUE
          ),
          shinycssloaders::withSpinner(
            plotOutput(ns("plot_pca")),
            type = 6,
            color = "#28a745"
          ),
          conditionalPanel(
            condition = "input.type_plot=='ind'",
            ns = ns,
            fluidRow(
              col_4(),
              col_4(
                pickerInput(
                  inputId = ns("number"),
                  label = "Top (n)",
                  choices = 4:1000, selected = 20,
                  options = list(
                    size = 5
                  )
                )
              ),
              col_4()
            )
          ),
          conditionalPanel(
            condition = "input.type_plot=='var'",
            ns = ns,
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
            condition = "input.type_plot=='bip'", ns = ns,
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
        ))
      },
      ignoreInit = T,
      ignoreNULL = T
    )

    output$markers_ggplot <- renderPlot({
      req(modelo())
      req(input$trait_marker)
      trait_selected <- input$trait_marker
      map <- data_read()[["dt_map"]]
      tryCatch(
        {
          marker_info <- modelo()$markers$rrBLUP
          if (is.null(map)) {
            map <- data.frame(
              marker = marker_info$marker,
              position = seq_along(marker_info$marker),
              chr = 1)
          }
          names(map) <- c("marker", "position", "chr")
          map$chr <- as.factor(map$chr)
          marker_plot(
            marker = marker_info,
            map = map,
            trait_selected = trait_selected,
            type = input$points_line,
            point_size = input$point_size_mark,
            legend_size = input$legend_size_mark,
            alpha = input$alpha
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

    observeEvent(input$markers_plot,
      {
        showModal(modalDialog(
          title = "Marker Effects",
          size = "xl",
          easyClose = T,
          pickerInput(
            inputId = ns("trait_marker"),
            label = tagList(
              "Trait",
              icon = tooltip(
                icon("question-circle", verify_fa = FALSE),
                title = "Select a trait",
                placement = "top"
              )
            ),
            choices = input$variables,
            selected = input$variables[1],
            options = list(
              `actions-box` = TRUE, size = 5, `live-search` = TRUE
            ),
            multiple = TRUE,
            width = "100%"
          ),
          shinycssloaders::withSpinner(
            plotOutput(ns("markers_ggplot"), height = "500"),
            type = 6,
            color = "#28a745"
          ),
          hr(),
          fluidRow(
            col_2(
              prettyRadioButtons(
                inputId = ns("points_line"),
                label = "Choose:", 
                choices = c("Points" = "point", "Line" = "line"),
                inline = TRUE, 
                status = "danger",
                fill = TRUE,
                icon = icon("check"), 
                bigger = TRUE,
                animation = "jelly"
              )
            ),
            col_3(
              sliderTextInput(
                inputId = ns("point_size_mark"), label = "Point Size:",
                choices = seq(0.5, 6, by = 0.1),
                grid = TRUE, selected = 2, width = "100%"
              )
            ),
            col_3(
              sliderTextInput(
                inputId = ns("legend_size_mark"), label = "Legend Size:",
                choices = c(8:20),
                grid = TRUE,
                selected = 15, 
                width = "100%"
              )
            ),
            col_3(
              sliderTextInput(
                inputId = ns("alpha"), label = "Transparency",
                choices = seq(0.1, 1, by = 0.1),
                grid = TRUE,
                selected = 1, 
                width = "100%"
              )
            )
          )
        ))
      },
      ignoreInit = T,
      ignoreNULL = T
    )
    
    output$desire <- downloadHandler(
      filename = function() {
        paste("desire_gain", ".txt", sep = "")
      },
      content = function(file) {
        req(modelo())
        if (input$include_predicted) {
          type_pred <- c("fit", "prediction")
        } else {
          type_pred <- "fit"
        }
        tryCatch(
          {
            gblups <- modelo()$results$GBLUP %>%
              dplyr::filter(type %in% type_pred) %>%
              dplyr::select(trait, level, predicted.value) %>%
              tidyr::spread(trait, value = "predicted.value")
            var_comp <- modelo()$var_comp$GBLUP
            corGen <- cor(
              gblups %>% dplyr::select_if(is.numeric),
              use = "pairwise.complete.obs"
            )
            if (ncol(gblups) <= 2) stop("Only one trait selected.")
            h2 <- var_comp$Genomic_h2
            sd_g <- sqrt(var_comp$VarG)
            ds <- desireFormat(
              n_traits = length(h2),
              traits = var_comp$Trait,
              heritability = h2, 
              sd = sd_g,
              corPhen = NULL, 
              corGen = corGen
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
        utils::write.table(
          ds,
          file = file,
          quote = FALSE, 
          col.names = FALSE, 
          row.names = FALSE)
      }
    )
    


    example_pheno <- data.frame(
      genotype = c(paste("TRT_", LETTERS[1:4], sep = "")),
      trait_1 = c(12.1, 11.5, 14, 13.6),
      trait_2 = c(80.5, 91.1, 89.2, 79.8),
      "..." = rep(".", 4),
      trait_k = c(25, 32.2, 28.6, 30.8)
    )
    example_geno <- data.frame(
      genotype = c(paste("TRT_", LETTERS[1:4], sep = "")),
      marker_1 = c(-1, -1, 0, 1),
      marker_2 = c(1, -1, 1, 0),
      marker_3 = c(-1, -1, 0, -1),
      "..." = rep(".", 4),
      marker_n = c(0, -1, 0, 1)
    )
    example_map <- data.frame(
      marker = c(paste("marker_", 1:4, sep = "")),
      position = c(0, 0.66, 1, 1.5),
      chr = c(1, 1, 1, 1)
    )
    observe({
      showModal(
        modalDialog(
          size = "l",
          title = div(tags$h3("Help message", style = "color: red;")),
          h4("Please, follow the format shown in the following example.
             Make sure to upload a CSV file!"),
          h6("Phenotypic Data"),
          renderTable(example_pheno,
            bordered = TRUE,
            align = "c",
            striped = TRUE
          ),
          hr(),
          h6("Genotypic Data"),
          renderTable(example_geno,
            bordered = TRUE,
            align = "c",
            striped = TRUE,
            digits = 0
          ),
          hr(),
          h6("Genetic Map (Opcional)"),
          renderTable(example_map,
                      bordered = TRUE,
                      align = "c",
                      striped = TRUE,
                      digits = 2
          ),
          easyClose = FALSE
        )
      )
    }) %>%
      bindEvent(input$example, ignoreInit = TRUE)

    return(list(
      model = modelo
    ))
  })
}

## To be copied in the UI
# mod_GBLUP_ui("GBLUP_1")

## To be copied in the server
# mod_GBLUP_server("GBLUP_1")
