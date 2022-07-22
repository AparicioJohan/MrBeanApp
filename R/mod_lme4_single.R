#' lme4_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_lme4_single_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4InfoBoxOutput(ns("respinfo"), width = 3), bs4InfoBoxOutput(ns("ngenot"), width = 3),
      bs4InfoBoxOutput(ns("rep"), width = 3), bs4InfoBoxOutput(ns("block"), width = 3)
    ),
    fluidRow(
      bs4Card(
        title = tagList(shiny::icon("question-circle", verify_fa = FALSE), "Help"),
        solidHeader = FALSE, footer = "Let's start", width = 3, status = "danger",
        h3("Factors in the formula:"),
        hr(),
        HTML("<ul>
                                            <li><strong>Fixed:</strong> Rep + Block  </li>
                                            <li><strong>Random:</strong> (1|Rep) + (1|Block) </li>
                                            <li><strong>Interaction Fixed:</strong> Rep:Block </li>
                                            <li><strong>Interaction Random:</strong> (1|Rep:Block) </li>
                                            </ul>  ")
      ),
      bs4Card(
        width = 3, status = "success", solidHeader = FALSE,
        title = tagList(icon = icon("ruler"), "Trait and Genotype"),
        # background = "blue",
        selectInput(
          inputId = ns("variable2"),
          label = "Response variable", choices = "", width = "100%"
        ),
        selectInput(
          inputId = ns("genotipo2"),
          label = "Genotype",
          choices = "", width = "100%"
        ),
        awesomeCheckbox(
          inputId = ns("res_ran2"),
          label = "Random Genotype",
          value = TRUE, status = "danger"
        )
      ),
      # checkboxInput(inputId=ns('res_ran2'),
      #               label='Random Genotype?',
      #               value=TRUE)),
      bs4Card(
        width = 3, status = "success", solidHeader = FALSE, title = tagList(icon = icon("tasks", verify_fa = FALSE), "Model"),
        prettyRadioButtons(
          inputId = ns("Id039"),
          label = "Choose:",
          choices = c("Alpha-Lattice" = 1, "Row-Col" = 5, "RCBD" = 2, "CRD" = 4, "Other model" = 3),
          icon = icon("check"),
          bigger = TRUE,
          status = "success",
          animation = "jelly"
        ),
        actionButton(ns("run"),
          label = "Run", icon = icon("check"), class = "btn-danger",
          style = "display:rigth ;color: white  ; background-color: #dd4b39"
        ),
        actionButton(ns("infomix"),
          label = "Info Model", class = "btn-success",
          style = "display:rigth ;color: white  ; background-color: #00a65a", icon = icon("info")
        ),
        br(), br(),
        actionButton(ns("multcmp"), label = "Multiple Comp", icon = icon("compress-arrows-alt", verify_fa = FALSE)),
        br(), br(),
        actionLink(inputId = ns("Rlinklme"), label = "Residuals", icon = icon("arrow-right"), style = "color: #28a745")
      ),
      column(
        3,
        conditionalPanel(
          condition = "input.Id039==1|input.Id039==2|input.Id039==4|input.Id039==5", ns = ns,
          bs4Card(
            width = NULL, status = "success", solidHeader = FALSE, title = tagList(icon = icon("tasks", verify_fa = FALSE), "Components"),
            conditionalPanel(
              condition = "input.Id039==1|input.Id039==2|input.Id039==5", ns = ns,
              selectInput(inputId = ns("Id086"), label = "Replicate", choices = "", width = "100%"),
              awesomeCheckbox(
                inputId = ns("rep_ran"),
                label = "Random Replicate",
                value = FALSE, status = "danger"
              )
            ),
            conditionalPanel(
              condition = "input.Id039==1", ns = ns,
              selectInput(inputId = ns("Id087"), label = "Block", choices = "", width = "100%"),
              awesomeCheckbox(
                inputId = ns("block_ran"),
                label = "Random Block",
                value = TRUE, status = "danger"
              ),
            ),
            conditionalPanel(
              condition = "input.Id039==5", ns = ns,
              fluidRow(
                col_6(
                  selectInput(inputId = ns("ccol"), label = "Col", choices = "", width = "100%"),
                  awesomeCheckbox(
                    inputId = ns("col_ran"),
                    label = "Random Col",
                    value = TRUE, status = "danger"
                  )
                ),
                col_6(
                  selectInput(inputId = ns("rrow"), label = "Row", choices = "", width = "100%"),
                  awesomeCheckbox(
                    inputId = ns("row_ran"),
                    label = "Random Row",
                    value = TRUE, status = "danger"
                  )
                )
              )
            ),
            selectizeInput(ns("Id089"), "Covariate ",
              width = "100%",
              choices = "", multiple = TRUE
            )
          )
        ),
        # bsModal(id = "mixedmodal",title = "Information about Mixed Model",trigger = ns("infomix"),size = "large",
        #         fluidRow( valueBoxOutput(ns("varres"),width = 4),
        #                   valueBoxOutput(ns("vargen"),width = 4),
        #                   valueBoxOutput(ns("hcullis"),width = 4)),
        #         DT::dataTableOutput(ns("glance")),
        #         shinycssloaders::withSpinner(verbatimTextOutput(ns("printranova")),type = 5,color = "#28a745"),
        #         DT::dataTableOutput(ns("anovamix")) ,br()
        # ),
        conditionalPanel(
          condition = "input.Id039==3", ns = ns,
          fluidRow(
            bs4Card(
              title = "Formula", status = "success", solidHeader = FALSE,
              textInput(ns("formula"),
                label = strong("Write the formula:"),
                value = "", placeholder = " (1|Line) + (1|Rep) + (1|Rep:Block)", width = "100%"
              ),
              infoBoxOutput(ns("form"), width = 12), width = 12
            )
          )
        )
      )
    ),
    fluidRow(
      bs4Card(
        width = 6, status = "success", solidHeader = FALSE,
        title = tagList(icon = icon("leaf"), "Summary Model"),
        shinycssloaders::withSpinner(verbatimTextOutput(ns("summaryalpha")), type = 6, color = "#28a745")
      ),
      bs4Card(
        width = 6, status = "success", solidHeader = FALSE, style = "overflow-x: scroll;",
        title = tagList(icon = icon("sort-numeric-up", verify_fa = FALSE), "Effects"),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("blups_mixed")), type = 6, color = "#28a745"),
        downloadButton(ns("desc_mixed"), "Download Table",
          class = "btn-success",
          style = "display:rigth ;color: white  ; background-color: #00a65a"
        )
      )
    )
  )
}

#' lme4_single Server Function
#'
#' @noRd
mod_lme4_single_server <- function(input, output, session, data) {
  ns <- session$ns


  observeEvent(data$data(), {
    updateSelectInput(session, "variable2", choices = names(data$data()), selected = "YdHa_clean")
    updateSelectInput(session, "genotipo2", choices = names(data$data()), selected = "line")
    updateSelectInput(session, "Id086", choices = names(data$data()), selected = "rep")
    updateSelectInput(session, "Id087", choices = names(data$data()), selected = "NNNNN")
    updateSelectInput(session, "Id089", choices = names(data$data()), selected = NULL)
    updateSelectInput(session, "ccol", choices = names(data$data()), selected = "col")
    updateSelectInput(session, "rrow", choices = names(data$data()), selected = "row")
  })


  output$form <- renderInfoBox({
    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")
    )
    if (isTRUE(input$res_ran2)) {
      gen <- paste("(1|", input$genotipo2, ")")
    } else {
      gen <- paste(input$genotipo2)
    }
    if (input$formula != "") {
      gen <- paste0(gen, " + ")
    } else {
      gen <- gen
    }
    infoBox(
      title = "Formula",
      value = paste0(input$variable2, " ~ ", gen, input$formula),
      subtitle = "Check this", width = 3, icon = shiny::icon("code"), elevation = 3
    )
  })

  ## Modelo alpha
  alpha <- eventReactive(input$run, {
    validate(need(input$variable2 != "", " "), need(input$genotipo2 != "", " "))
    dt <- data$data()
    rownames(dt) <- NULL
    dt[, input$genotipo2] <- as.factor(dt[, input$genotipo2])
    dt$Response <- dt[, input$variable2]
    dt$Gen <- as.factor(dt[, input$genotipo2])

    tryCatch(
      {
        if (sum(is.na(dt$Response)) > 0.9 * length(dt$Response)) stop("Missing data in the response")
      },
      error = function(e) {
        toastr_error(title = "Warning:", conditionMessage(e), position = "bottom-right", progressBar = TRUE)
      }
    )
    validate(need(sum(is.na(dt$Response)) < 0.9 * length(dt$Response), "Check the formula"))

    tryCatch(
      {
        lme4_single(
          data = dt,
          response = input$variable2,
          genotype = input$genotipo2,
          res_ran = input$res_ran2,
          model = input$Id039,
          replicate = input$Id086,
          rep_ran = input$rep_ran,
          block = input$Id087,
          block_ran = input$block_ran,
          col = input$ccol,
          col_ran = input$col_ran,
          row = input$rrow,
          row_ran = input$row_ran,
          covariate = input$Id089,
          formula = input$formula
        )
      },
      error = function(e) {
        shinytoastr::toastr_error(
          title = "Error:", conditionMessage(e), position = "bottom-full-width",
          showMethod = "slideDown", hideMethod = "hide"
        )
      }
    )
  }) # ,ignoreNULL = T, ignoreInit = T

  # observe({
  #   print(class(alpha()))
  #   print(alpha())
  # })


  output$summaryalpha <- renderPrint({
    validate(need(input$variable2 != "", "Complete the areas above"))
    validate(need(input$genotipo2 != "", "Please select the Genotype"))
    if (input$Id039 == 1 | input$Id039 == 2) validate(need(input$Id086 != "", "Select the Replicate"))
    if (input$Id039 == 1) validate(need(input$Id087 != "", "Select Block "))
    validate(need(input$run != 0, "Run the model"))
    if (input$run == 0) {
      return()
    } else {
      isolate({
        req(alpha())
        print(summary(alpha(), correlation = FALSE))
      })
    }
  })

  blup_mix <- reactive({
    validate(
      need(input$variable2 != "", " "),
      need(input$genotipo2 != "", " ")
    )

    if (input$Id039 == 1 | input$Id039 == 2) validate(need(input$Id086 != "", "Select the Replicate"))
    if (input$Id039 == 1) validate(need(input$Id087 != "", "Select Block "))

    if (input$run == 0) {
      return()
    } else {
      isolate({
        req(alpha())
        lme4_effects(alpha(),
          genotype = input$genotipo2,
          res_ran = input$res_ran2, model_class = input$Id039
        )
      })
    }
  })

  output$blups_mixed <- DT::renderDataTable(
    if (input$run == 0) {
      return()
    } else {
      isolate({
        req(alpha())
        DT::datatable(
          {
            blup_mix()
          },
          option = list(pageLength = 5, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:ncol(blup_mix())))),
          filter = "top",
          selection = "multiple"
        )
      })
    }
  )

  # VALUEBOX for Variance residual
  output$varres <- renderbs4ValueBox({
    validate(need(input$run != 0, " "))
    req(alpha())
    vc.e <- sqrt(VarE(alpha()))
    v <- round(vc.e, 2)
    bs4ValueBox(v,
      subtitle = "Residual SD",
      icon = shiny::icon("arrow-down"),
      color = "danger", elevation = 3,
      footer = HTML("<center> Looking for low <center>")
    )
  })

  # VALUEBOX for genotypic variance
  output$vargen <- renderbs4ValueBox({
    validate(need(input$run != 0, " "))
    gen <- ifelse(input$Id039 == 3, input$genotipo2, "Gen")
    req(alpha())
    req(isTRUE(input$res_ran2))
    vc.g <- sqrt(VarG(alpha(), gen))
    v <- round(vc.g, 2)
    bs4ValueBox(v,
      subtitle = "Genotypic SD",
      icon = shiny::icon("arrow-up"),
      color = "success", elevation = 3,
      footer = HTML("<center> Looking for high <center>")
    )
  })

  # VALUEBOX for HERITABILIY
  output$hcullis <- renderbs4ValueBox({
    req(alpha())
    req(isTRUE(input$res_ran2))
    H <- h.cullis(alpha(), ifelse(input$Id039 == 3, input$genotipo2, "Gen"))
    H <- round(H, 2)
    bs4ValueBox(H,
      subtitle = "Heritability",
      icon = shiny::icon("seedling"),
      color = "info", elevation = 3,
      footer = HTML("<center> 0 = Bad / 1 = Good  <center>")
    )
  })


  output$glance <- DT::renderDataTable({ # Modal BLUPs Spatial
    if (input$run == 0) {
      return()
    } else {
      isolate({
        req(alpha())
        DT::datatable(
          {
            if (class(alpha()) == "lm") {
              round(broom.mixed::glance(alpha())[, -c(1:5)], 2)
            } else {
              round((broom.mixed::glance(alpha())), 2)
            }
          },
          options = list(paging = FALSE, scrollX = TRUE, searching = FALSE)
        )
      })
    }
  })

  output$printranova <- renderPrint({
    validate(need(input$run != 0, "Run the model"))
    req(alpha())
    if (class(alpha()) == "lm") {
      return()
    } else {
      dt <- alpha()@frame
      lmerTest::ranova(alpha())
    }
  })

  output$anovamix <- DT::renderDataTable({
    req(alpha())
    nfixed <- nrow(anova(alpha()))
    if (input$run == 0) {
      return()
    } else if (nfixed < 1) {
      validate(need(nfixed > 1, "ANOVA is only calculated for fixed effects"))
    } else {
      isolate({
        if (class(alpha()) == "lm") {
          k <- suppressWarnings((broom.mixed::tidy(anova(alpha()))))
        } else {
          k <- suppressWarnings((broom.mixed::tidy(anova(alpha(), ddf = "Kenward-Roger", type = 1))))
        }
        DT::datatable(
          {
            dplyr::mutate_if(k, is.numeric, round, digits = 2) # cbind(term=k[,1],round(k[,2:7],2))
          },
          options = list(paging = FALSE, scrollX = TRUE, searching = FALSE)
        )
      })
    }
  })

  output$respinfo <- renderbs4InfoBox({
    validate(need(input$variable2 != "", " "))
    var <- input$variable2
    bs4InfoBox(
      title = "Response Variable",
      color = "info",
      gradient = TRUE,
      fill = TRUE,
      value = var,
      icon = shiny::icon("ruler"), elevation = 3
    )
  })

  output$ngenot <- renderbs4InfoBox({
    validate(need(input$genotipo2 != "", " "))
    ngen <- length(unique(as.factor(data$data()[, input$genotipo2])))
    bs4InfoBox(
      title = "Num of Genotypes",
      color = "success",
      gradient = TRUE,
      fill = TRUE,
      value = ngen,
      icon = shiny::icon("sort-amount-down", verify_fa = FALSE), elevation = 3
    )
  })

  output$rep <- renderbs4InfoBox({
    validate(need(input$Id086 != "", " "))
    nreps <- length(unique(as.factor(data$data()[, input$Id086])))
    bs4InfoBox(
      title = "Num of Replicates",
      color = "danger",
      gradient = TRUE,
      fill = TRUE,
      value = nreps,
      icon = shiny::icon("sync", verify_fa = FALSE), elevation = 3
    )
  })

  output$block <- renderbs4InfoBox({
    validate(need(input$Id087 != "", " "))
    nbl <- length(unique(as.factor(data$data()[, input$Id087])))
    bs4InfoBox(
      title = "Num of Blocks",
      color = "warning",
      gradient = TRUE,
      fill = TRUE,
      value = nbl,
      icon = shiny::icon("table-cells", verify_fa = FALSE), elevation = 3
    )
  })

  # info mix

  observeEvent(input$infomix, {
    showModal(modalDialog(
      title = "Information about the Model", size = "l", easyClose = T,
      fluidRow(
        valueBoxOutput(ns("varres"), width = 4),
        valueBoxOutput(ns("vargen"), width = 4),
        valueBoxOutput(ns("hcullis"), width = 4)
      ),
      DT::dataTableOutput(ns("glance")),
      shinycssloaders::withSpinner(verbatimTextOutput(ns("printranova")), type = 5, color = "#28a745"),
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("anovamix")), type = 5, color = "#28a745"),
      footer = tagList(modalButton("Cancel"))
    ))
  })

  # DEscargar blups Mixed model

  output$desc_mixed <- downloadHandler(
    filename = function() {
      paste("Effect_Mix_lme4", ".csv", sep = "")
    },
    content = function(file) {
      BLUPS <- blup_mix()
      write.csv(BLUPS, file, row.names = FALSE)
    }
  )



  # Multiple comp -----------------------------------------------------------

  differences <- reactive({
    input$run
    isolate({
      req(input$genotipo2)
      req(alpha())
      validate(need(input$res_ran2 != TRUE, "Multiple Comparisons only when the genotype is taken as fixed factor."))
      predict <- blup_mix()
      ngen <- nrow(predict)
      validate(need(ngen < 50, "Too many genotypes for Multiple Comparisons"))

      tryCatch(
        {
          comp <- mult_comp(
            model = alpha(),
            res_ran = input$res_ran2,
            genotype = input$genotipo2,
            model_class = input$Id039,
            ngen = ngen
          )
        },
        error = function(e) {
          shinytoastr::toastr_error(
            title = "Error:", conditionMessage(e), position = "bottom-full-width",
            showMethod = "slideDown", hideMethod = "hide"
          )
        }
      )
      if (!exists("comp")) comp <- NULL
      return(comp)
    })
  })

  output$table_multcomp <- DT::renderDataTable(
    if (input$run == 0) {
      return()
    } else {
      req(differences())
      DT::datatable(
        {
          differences() %>% dplyr::mutate_if(is.numeric, round, 3)
        },
        option = list(pageLength = 10, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:ncol(differences())))),
        filter = "top",
        selection = "multiple"
      )
    }
  )

  observeEvent(input$multcmp, {
    showModal(modalDialog(
      title = "Multiple Comparisons", size = "l", easyClose = T,
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("table_multcomp")), type = 5, color = "#28a745"),
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
      paste("multp_comparisons_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(differences(), file, row.names = FALSE)
    }
  )



  return(list(
    model = alpha,
    effects = blup_mix,
    run = reactive(input$run),
    res_ran2 = reactive(input$res_ran2),
    Rlink = reactive(input$Rlinklme)
  ))
}

## To be copied in the UI
# mod_lme4_single_ui("lme4_single_ui_1")

## To be copied in the server
# callModule(mod_lme4_single_server, "lme4_single_ui_1")
