#' MSA_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MSA_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("notfound"),
      HTML("<center><img src='www/code.svg' width='60%' height='60%'></center>")
    ),
    fluidRow(
      column(
        width = 5,
        fluidRow(
          column(
            12,
            shinyjs::hidden(
              div(
                id = ns("only"),
                fluidRow(
                  bs4Dash::box(
                    width = 12,
                    status = "success",
                    solidHeader = FALSE,
                    maximizable = TRUE,
                    title = tagList(
                      icon = icon("info-circle", verify_fa = FALSE),
                      "Variance Component Estimates"
                    ),
                    echarts4r::echarts4rOutput(ns("comparison"))
                  )
                )
              )
            )
          ),
          column(
            12,
            shinyjs::hidden(
              div(
                id = ns("summ"),
                fluidRow(
                  bs4TabCard(
                    width = 12, 
                    id = "tabcards", 
                    maximizable = TRUE, 
                    solidHeader = FALSE, 
                    closable = FALSE,
                    status = "success",
                    side = "left",
                    type = "tabs",
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
                          condition = "input.type=='png'", ns = ns,
                          sliderInput(
                            inputId = ns("png.wid.c"), 
                            min = 200, 
                            max = 2000,
                            value = 900,
                            label = "Width pixels"
                          ),
                          sliderInput(
                            inputId = ns("png.hei.c"), 
                            min = 200, 
                            max = 2000,
                            value = 600,
                            label = "Height pixels"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.type=='pdf'", ns = ns,
                          sliderInput(
                            inputId = ns("pdf.wid.c"),
                            min = 2, 
                            max = 20, 
                            value = 10, 
                            label = "Width"
                          ),
                          sliderInput(
                            inputId = ns("pdf.hei.c"),
                            min = 2, 
                            max = 20, 
                            value = 8, 
                            label = "Height"
                          )
                        ),
                        downloadButton(
                          ns("descargar2"), 
                          "Download Plot",
                          class = "btn-success",
                          style = " color: white ; background-color: #28a745"
                        ), br(),
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
                      )
                    ),
                    tabPanel(
                      title = "Summary",
                      icon = icon("info-circle", verify_fa = FALSE),
                      shinycssloaders::withSpinner(
                        DT::dataTableOutput(ns("table")),
                        type = 5, 
                        color = "#28a745"
                      ),
                      downloadButton(
                        ns("downloadsummary"),
                        "Download Table",
                        class = "btn-success",
                        style = "color: white ; background-color: #28a745; float:left"
                      )
                    ),
                    tabPanel(
                      title = "Predictions",
                      icon = icon("table"),
                      DT::dataTableOutput(ns("effects")),
                      downloadButton(
                        ns("downloadeffects"),
                        "Download Table",
                        class = "btn-success",
                        style = " color: white ; background-color: #28a745; float:left"
                      ),
                      downloadButton(
                        ns("spread_effects"),
                        "Spread Table",
                        class = "btn-danger",
                        style = " color: white ; background-color: #d9534f; float:left"
                      )
                    ),
                    tabPanel(
                      title = "Potential Outliers",
                      icon = icon("exclamation-triangle", verify_fa = FALSE),
                      DT::dataTableOutput(ns("extrem")),
                      downloadButton(
                        ns("downloadDTclean"),
                        "Download Data-Cleaned",
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
      ),
      column(
        7,
        shinyjs::hidden(
          div(
            id = ns("second"),
            fluidRow(
              bs4Dash::box(
                width = 12,
                status = "success",
                solidHeader = FALSE,
                maximizable = T,
                title = tagList(icon = icon("braille"), "Spatial Trend"),
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
                    condition = "input.typefile=='png'", ns = ns,
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
                    condition = "input.typefile=='pdf'", ns = ns,
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
                    style = " color: white ; background-color: #28a745"
                  ), br(),
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
                materialSwitch(
                  ns("tog_plot"), 
                  label = "Percentage",
                  status = "success", 
                  value = FALSE
                ),
                fluidRow(
                  col_3(),
                  col_4(
                    selectInput(
                      ns("selected"), 
                      label = HTML("<center> Experiment </center>"),
                      choices = "",
                      width = "100%"
                    )
                  ),
                  col_3(
                    rep_br(1),
                    actionBttn(
                      inputId = ns("sum_mod"),
                      label = "summary",
                      style = "unite", 
                      size = "sm", 
                      block = F,
                      color = "warning",
                      icon = icon("spinner")
                    )
                  ),
                  col_2()
                )
              ),
              bs4Dash::box(
                width = 12, 
                status = "success",
                solidHeader = FALSE,
                title = tagList(
                  icon = icon("sort-numeric-up", verify_fa = FALSE),
                  "Predictions Plot"
                ),
                collapsible = T, 
                maximizable = T,
                echarts4r::echarts4rOutput(ns("ranking"))
              )
            )
          )
        )
      )
    )
  )
}

#' MSA_results Server Function
#'
#' @noRd
mod_MSA_results_server <- function(input, output, session, msa) {
  ns <- session$ns

  models <- reactive({
    req(msa$run())
    req(msa$modelo())
    msa$modelo()
  })

  # observeEvent(msa$run(),{
  #   print(models())
  # }, ignoreInit = T)

  summary_msa <- reactive({
    req(msa$run())
    req(models())
    inf <- msa_table(models(), msa$gen_ran(),  msa$rLimit())
    return(inf)
  })

  observe({
    if (msa$run() == "" | is.null(msa$run())) {
      show(id = "help", anim = TRUE, animType = "slide")
    } else {
      hide(id = "help", anim = TRUE, animType = "slide")
    }
  })

  output$table <- DT::renderDataTable(
    if (is.null(summary_msa())) {
      return()
    } else {
      DT::datatable(
        {
          summary_msa()
        },
        option = list(
          pageLength = 5,
          scrollX = TRUE, 
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(summary_msa()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  blups <- reactive({
    req(msa$run())
    req(models())
    effects <- multi_msa_effects(models())
    return(effects)
  })

  observe({
    if (is.null(blups())) {
      show(id = "notfound", anim = TRUE, animType = "slide")
    } else {
      hide(id = "notfound", anim = TRUE, animType = "slide")
    }
  })

  output$effects <- DT::renderDataTable(
    if (is.null(blups())) {
      return()
    } else {
      DT::datatable(
        {
          blups()
        },
        option = list(
          pageLength = 5,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(blups()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  output$comparison <- echarts4r::renderEcharts4r({
    req(msa$run())
    req(summary_msa())
    if (msa$gen_ran()) {
      comp <- summary_msa() %>%
        e_charts(Experiment) %>%
        e_bar(varE, name = "Residual variance") %>%
        e_bar(varG, name = "Genotypic Variance") %>%
        e_legend(show = T, bottom = "bottom", left = "center", orient = "horizontal") %>%
        e_title("Variance Comparison", subtext = "By experiment") %>%
        e_tooltip() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords() %>%
        e_grid(left = "20%")
      comp
    } else {
      comp <- summary_msa() %>%
        e_charts(Experiment) %>%
        e_bar(varE, name = "Residual variance") %>%
        e_legend(show = T, bottom = "bottom", left = "center", orient = "horizontal") %>%
        e_title("Residual Variance", subtext = "By experiment") %>%
        e_tooltip() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords() %>%
        e_grid(left = "20%")
      comp
    }
  })

  observe({
    if (!is.null(blups())) {
      show(id = "only", anim = TRUE, animType = "slide")
      show(id = "summ", anim = TRUE, animType = "slide")
      show(id = "second", anim = TRUE, animType = "slide")
    } else {
      hide(id = "only", anim = TRUE, animType = "slide")
      hide(id = "summ", anim = TRUE, animType = "slide")
      hide(id = "second", anim = TRUE, animType = "slide")
    }
  })

  observeEvent(blups(), {
    msa$run()
    exp <- names(models())
    updateSelectInput(session, "selected", choices = exp, selected = exp[1])
  })

  output$plot_spats <- renderPlot({
    msa$run()
    input$tog_plot
    input$selected
    isolate({
      mod_selected <- models()[[input$selected]]
      req(mod_selected)
      spaTrend <- ifelse(input$tog_plot == TRUE, "percentage", "raw")
      plot(mod_selected, spaTrend = spaTrend)
    })
  })


  output$ranking <- echarts4r::renderEcharts4r({
    msa$run()
    req(blups())
    tmp_effects <- blups() %>%
      dplyr::group_by(Experiment) %>%
      dplyr::mutate(predicted.values = predicted.values - mean(predicted.values, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Experiment %in% input$selected) %>%
      dplyr::mutate(
        lower = predicted.values - 1.645 * standard.errors,
        upper = predicted.values + 1.645 * standard.errors
      ) %>%
      dplyr::arrange(dplyr::desc(predicted.values)) %>%
      droplevels()
    names(tmp_effects)[2] <- "Genotype"
    value <- ifelse(nrow(tmp_effects) >= 200, 2, 0)
    g8 <- tmp_effects %>%
      e_charts(Genotype) %>%
      e_scatter(predicted.values, bind = Genotype, symbol_size = 10) %>%
      e_tooltip() %>%
      e_error_bar(lower = lower, upper = upper) %>%
      e_toolbox_feature(feature = "dataView") %>%
      e_toolbox_feature(feature = "dataZoom") %>%
      e_legend(show = T, type = "scroll") %>%
      e_title(text = "Genotype Ranking") %>%
      e_x_axis(axisLabel = list(interval = value, rotate = 75, fontSize = 8, margin = 8))
    g8
  })


  output$summary2 <- renderPrint({
    msa$run()
    mod_selected <- models()[[input$selected]]
    req(mod_selected)
    summary(mod_selected)
  })

  observeEvent(input$sum_mod, {
    showModal(modalDialog(
      title = "Summary", size = "l", easyClose = T,
      shinycssloaders::withSpinner(
        verbatimTextOutput(ns("summary2")),
        type = 5, color = "#28a745"
      )
    ))
  })

  ## Interactive  Correlation

  # output$correlation <- echarts4r::renderEcharts4r({
  #   msa$run()
  #   req(blups())
  #   req(summary_msa())
  #
  #   tryCatch(
  #     {
  #       var2 <- names(summary_msa())[2]
  #       # exp <- summary_msa() %>% dplyr::filter(!.data[[var2]]==0) %>% dplyr::pull(Experiment) %>% as.character()  # check when genotype is fixed
  #       bl <- blups() # %>% dplyr::filter(Experiment %in% exp) %>% droplevels()
  #       bl <- bl[,1:3] %>% tidyr::spread(., "Experiment", "predicted.values" )
  #
  #       CC <- round(cor(bl[,-1], use = "pairwise.complete.obs"),3)
  #       g3 <- CC %>%
  #         e_charts() %>%
  #         e_correlations(order = "hclust",visual_map = F) %>%
  #         e_tooltip() %>%   e_visual_map( color=c("#4285f4" ,"white","#db4437"),
  #                                         min = -1 ,
  #                                         max =  1 ,
  #                                         orient= 'horizontal',
  #                                         left= 'center',
  #                                         bottom = 'bottom'
  #         ) %>%
  #         e_title("Correlation", "Between experiments") %>%
  #         e_toolbox_feature(feature = "saveAsImage") %>%
  #         e_x_axis(axisLabel = list(interval = 0, rotate = -45, fontSize=12 , margin=8  ))  %>% # rotate
  #         e_grid(left = "20%",height = "60%")
  #     },
  #     error = function(e) {
  #       shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
  #                                 showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
  #     }
  #   )
  #   if(!exists("g3")) g3 <- NULL
  #
  #   return(g3)
  #
  # })

  output$corr <- renderPlot({
    msa$run()
    isolate({
      req(blups())
      req(summary_msa())
      tryCatch(
        {
          bl <- blups()
          bl <- bl[, 1:3] %>% tidyr::spread(., "Experiment", "predicted.values")
          resum <- msa_table(models(), msa$gen_ran())
          names(resum)[1] <- "Experiment"
          if (msa$gen_ran() == TRUE) {
            Heritability <- resum$h2
            names(Heritability) <- resum$Experiment
            ggCor(bl[, -1], colours = c("#db4437", "white", "#4285f4"), Diag = Heritability)
          } else {
            ggCor(bl[, -1], colours = c("#db4437", "white", "#4285f4"))
          }
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:", conditionMessage(e), position = "bottom-full-width",
            showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
          )
        }
      )
    })
  })


  # Residuals table

  data_extreme <- reactive({
    msa$run()
    isolate({
      req(models())
      effects <- table_outlier(
        models(),
        id = "Exp",
        k = msa$rLimit()
      ) %>% dplyr::select(-Index, -l, -u)
      return(effects)
    })
  })

  output$extrem <- DT::renderDataTable(
    if (is.null(data_extreme())) {
      return()
    } else {
      DT::datatable(
        {
          data_extreme()
        },
        option = list(
          pageLength = 5, 
          scrollX = TRUE, 
          columnDefs = list(
            list(className = "dt-center", targets = 0:ncol(data_extreme()))
          )
        ),
        filter = "top",
        selection = "multiple"
      )
    }
  )


  # Download PLOT SPATIAL
  output$descargar <- downloadHandler(
    filename = function() {
      req(input$selected)
      paste(paste0("plotSpATS_", input$selected), input$typefile, sep = ".")
    },
    content = function(file) {
      if (input$typefile == "png") {
        png(file, width = input$png.wid, height = input$png.hei)
        spaTrend <- ifelse(input$tog_plot == TRUE, "percentage", "raw")
        plot(models()[[input$selected]], spaTrend = spaTrend, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, axis.args = list(cex.axis = 1.2))
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid, height = input$pdf.hei)
        spaTrend <- ifelse(input$tog_plot == TRUE, "percentage", "raw")
        plot(models()[[input$selected]], spaTrend = spaTrend, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, axis.args = list(cex.axis = 1.2))
        dev.off()
      }
    }
  )

  # download effects

  output$downloadeffects <- downloadHandler(
    filename = function() {
      paste("Effects_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      req(blups())
      datos <- data.frame(blups())
      write.csv(datos, file, row.names = FALSE)
    }
  )

  output$spread_effects <- downloadHandler(
    filename = function() {
      paste("Effects_mrbean_spread", ".csv", sep = "")
    },
    content = function(file) {
      req(blups())
      datos <- data.frame(blups()[, 1:3] %>% tidyr::spread(., "Experiment", "predicted.values"))
      write.csv(datos, file, row.names = FALSE)
    }
  )

  # Download summary

  output$downloadsummary <- downloadHandler(
    filename = function() {
      paste("summary_MSA_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      req(summary_msa())
      datos <- data.frame(summary_msa())
      write.csv(datos, file, row.names = FALSE)
    }
  )


  # Download correlation plot

  output$descargar2 <- downloadHandler(
    filename = function() {
      paste("corrPlot", input$type, sep = ".")
    },
    content = function(file) {
      if (input$type == "png") {
        png(file, width = input$png.wid.c, height = input$png.hei.c)
        req(blups())
        bl <- blups()
        bl <- bl[, 1:3] %>% tidyr::spread(., "Experiment", "predicted.values")
        resum <- msa_table(models(), msa$gen_ran())
        names(resum)[1] <- "Experiment"
        if (msa$gen_ran() == TRUE) {
          Heritability <- resum$h2
          names(Heritability) <- resum$Experiment
          g1 <- ggCor(bl[, -1], colours = c("#db4437", "white", "#4285f4"), Diag = Heritability)
          print(g1)
        } else {
          g1 <- ggCor(bl[, -1], colours = c("#db4437", "white", "#4285f4"))
          print(g1)
        }
        dev.off()
      } else {
        pdf(file, width = input$pdf.wid.c, height = input$pdf.hei.c)
        req(blups())
        bl <- blups()
        bl <- bl[, 1:3] %>% tidyr::spread(., "Experiment", "predicted.values")
        resum <- msa_table(models(), msa$gen_ran())
        names(resum)[1] <- "Experiment"
        if (msa$gen_ran() == TRUE) {
          Heritability <- resum$h2
          names(Heritability) <- resum$Experiment
          g1 <- ggCor(bl[, -1], colours = c("#db4437", "white", "#4285f4"), Diag = Heritability)
          print(g1)
        } else {
          g1 <- ggCor(bl[, -1], colours = c("#db4437", "white", "#4285f4"))
          print(g1)
        }
        dev.off()
      }
    }
  )


  # Download data Clean

  output$downloadDTclean <- downloadHandler(
    filename = function() {
      paste("DT_cleaned_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      req(models())

      clean_data_SpATS <- function(models) {
        data <- lapply(models, res_raw_data, msa$rLimit())
        data <- data.table::data.table(plyr::ldply(data[], data.frame))
        return(data)
      }
      datos <- clean_data_SpATS(models = models())

      write.csv(datos, file, row.names = FALSE)
    }
  )
}

## To be copied in the UI
# mod_MSA_results_ui("MSA_results_ui_1")

## To be copied in the server
# callModule(mod_MSA_results_server, "MSA_results_ui_1")
