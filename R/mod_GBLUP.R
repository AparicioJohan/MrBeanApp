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
              id = "tabcard",
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
                  col_3(),
                  col_6(
                    actionBttn(
                      inputId = ns("dendo"),
                      label = "Dendogram",
                      style = "jelly",
                      color = "warning",
                      block = T,
                      icon = icon("check")
                    )
                  ),
                  col_3()
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
        write.csv(datos, file, row.names = FALSE)
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
              title = "Error in Dendogram:",
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
          title = "Dendogram", size = "l", easyClose = T,
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
        paste("dendogram", input$filetype3, sep = ".")
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
              title = "Error in Dendogram:",
              conditionMessage(e),
              position = "bottom-full-width",
              showMethod = "slideDown",
              hideMethod = "hide",
              hideEasing = "linear"
            )
          }
        )
        if (input$filetype3 == "png") {
          png(file, width = input$png.wid.den, height = input$png.hei.den)
          print(dend)
          dev.off()
        } else {
          pdf(file, width = input$pdf.wid.den, height = input$pdf.hei.den)
          print(dend)
          dev.off()
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
            ggplot(
              aes(
                x = observed,
                y = predicted.value
              )
            ) +
            geom_point(
              alpha = input$alpha,
              size = input$point_size,
              color = "grey"
            ) +
            theme_bw(base_size = input$legend_size) +
            labs(x = "Observed", y = "Predicted") +
            ggpubr::stat_cor(size = input$point_size) +
            geom_smooth(method = "lm", se = FALSE) +
            coord_fixed()
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
            ggplot(
              aes(
                x = reliability,
                y = predicted.value,
                color = type,
                label = level
              )
            ) +
            geom_point(
              alpha = input$alpha_rel,
              size = input$point_size_rel
            ) +
            theme_bw(base_size = input$legend_size_rel) +
            labs(x = "Reliability", y = "Predicted") + 
            geom_vline(xintercept = 0, linetype = 2, color = "grey")
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
    observe({
      showModal(
        modalDialog(
          size = "l",
          title = div(tags$h3("Help message", style = "color: red;")),
          h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
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
