#' spats_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spats_single_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Single-Site Spatial Analysis</h1>'),
    fluidRow(
      bs4Dash::box( width = 3,status = "success", solidHeader = FALSE,
                    title = tagList(icon=icon("braille"), "SpATS",
                                    actionButton(ns("btn"),
                                                 tagList(icon=icon("question-circle"), "Guide"),
                                                 style= "color: white ; background-color: #dd4b39", class="btn-danger")),   # background = "light-blue"  
                    # actionButton(ns("btn"),
                    #              tagList(icon=icon("question-circle"), "Guide"),
                    #              style= "color: white ; background-color: #dd4b39", class="btn-danger"),
                    # hr(),
                    rintrojs::introBox(
                      
                      selectInput(inputId=ns("variable"),
                                  label= with_red_star("Response Variable"),
                                  choices="", width = "100%"),
                      data.step = 1,data.intro = "Select the column that contains the phenotypic response variable.",data.position = "right",color="red") ,
                    rintrojs::introBox(
                      selectInput(inputId=ns("genotipo"),
                                  label=with_red_star("Genotype"),
                                  choices="", width = "100%"),
                      awesomeCheckbox(inputId = ns('res_ran') ,
                                     label='Random Genotype',  
                                     value = TRUE ,status = "danger"  ),
                      shinyjs::hidden(
                        pickerInput(
                          inputId = ns("selected"),
                          label = tagList( "Checks",
                                           icon=tooltip(icon("question-circle"),
                                                             title = "Select Checks",
                                                             placement = "top")
                          ), 
                          choices = NULL,
                          options = list(
                            `actions-box` = TRUE, size = 5, `live-search` = TRUE), 
                          multiple = TRUE, width = "100%"
                        )
                      ), data.step = 2,data.intro = "Select the column that contains the genotype IDs.
                                                                                         Check/Uncheck the box if you want to treat this as a random/fixed
                                                                                              effect factor in the MLM.",data.position = "right",color="red"   ) ),
      
      
      bs4Dash::box(width = 2, status = "success", solidHeader = FALSE,collapsible = TRUE ,
                   title=tagList(icon=icon("th"), "Coordinates"),
                   rintrojs::introBox(
                     selectInput(inputId=ns("column"),label=with_red_star("Column"),  choices=""),
                     selectInput(inputId=ns("fila"),
                                 label=with_red_star("Row"),
                                 choices=""),data.step = 3,data.intro = "Select the columns in your dataset that contain the Row and Column coordinates for the plots in your trial.",data.position = "right" ) ),
      
      
      bs4Dash::box(width = 2, title =tagList(icon=icon("tractor"), 'Factors')  ,status = "success", solidHeader = FALSE,collapsible = TRUE ,
                   rintrojs::introBox(
                     selectizeInput(ns("show_fixed"), "Fixed ",
                                    choices = "", multiple = TRUE),
                     
                     selectizeInput(ns("show_random"), "Random",
                                    choices = "", multiple = TRUE),
                     data.step = 4,data.intro = "In case you want to include additional qualitative variables in the MLM, select them here as either fixed or random effect factors.",data.position = "bottom",color="red"),
                   rintrojs::introBox(
                     selectizeInput(ns("covariate"), "Covariate",
                                    choices = "", multiple = TRUE,selected=NULL),
                     data.step = 5,data.intro = "In case you want to include additional quantitative variables in the MLM.",data.position = "bottom",color="red")),
      bs4Dash::box(width = 2, status = "success",solidHeader = FALSE, title=tagList(icon=icon("tasks"), "Model"),collapsible = TRUE ,
                   rintrojs::introBox(
                     actionButton(ns("action"), label = "Run Model", class="btn-success",
                                  style="display:rigth; color: white ; background-color: #28a745"),
                     disabled(actionButton(ns("inf"),label = "Info-Box",style="display:rigth")),
                     br(),br(),
                     disabled(actionButton(ns("spatial"),label = "Spatial Trend",style="display:rigth")),
                     br(),hr(),
                     disabled(actionButton(ns("tabBut"), "View BLUPs/BLUEs")),br(),br(),
                     disabled(actionButton(ns("coeff"), "Coefficients")),br(),br(),
                     disabled(actionLink(inputId = ns("Rlink"), label = "Residuals", icon = icon("arrow-right"), style = "color: #28a745")),
                     data.step=6,data.intro = "Use this control panel to run the model and display the 3D spatial trend.",data.hint = "Good",data.position = "bottom-middle-aligned")
      ),
      # bsModal("modalExample", "BLUPs/BLUEs", ns("tabBut"), size = "large",
      #         shinycssloaders::withSpinner(DT::dataTableOutput(ns("distTable")),type = 6,color = "#28a745"),
      #         br(), br(),
      #         downloadButton(ns("downloadData"), 
      #                        "Download BLUPs/BLUEs", class="btn-success",
      #                        style= " color: white ; background-color: #28a745; float:left"),
      #         br()
      #         ),
      
      bs4Dash::box(width = 3, status = "success",solidHeader = FALSE,title =tagList(icon=icon("sliders-h"), "Segments and Report") ,collapsible = TRUE ,
                   rintrojs::introBox(
                     materialSwitch(ns("able"),label = "Segments",status = "success"),
                     uiOutput(ns("segcol")),uiOutput(ns("segrow")),data.step=7,data.intro="Enable this box in case you have a large-scale trial (nColumns > 100 ; nRows > 100)
                                                   to reduce the number of segments for smoothing the spatial components in the MLM.",data.position = "bottom" ),
                   hr(),
                   awesomeCheckbox(inputId = ns('outliers'),
                                   label='Remove Outliers',  
                                   value = FALSE ,status = "danger"),
                   numericInput(ns("times"), "Number of Times to Check", value = 1, min=1, max=3, step=1, width = "100%" ),
                   hr(),
                   radioButtons('format', 'Report Format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                   disabled(downloadButton('downloadReport')))
      
    )
  )
}
    
#' spats_single Server Function
#'
#' @noRd 
mod_spats_single_server <- function(input, output, session, data){
  ns <- session$ns
  
  
  observeEvent(data$data(),{
    dt <- data$data()
    updateSelectInput(session, "variable", choices=names(dt), selected = "YdHa_clean")
    updateSelectInput(session, "column", choices=names(dt),selected = "col")
    updateSelectInput(session, "fila", choices=names(dt),selected = "row")
    updateSelectInput(session, "factor", choices=names(dt),selected = "rep")
    updateSelectInput(session, "genotipo", choices=names(dt),selected = "line")
    updateSelectInput(session, "show_fixed", choices=names(dt),selected = NULL)
    updateSelectInput(session, "show_random", choices=names(dt),selected = NULL)
    updateSelectInput(session, "covariate", choices=names(dt),selected = NULL)
    
  })
  
  observe({
    shinyjs::toggle(id = "selected", anim = T, time = 1, animType = "fade", condition = input$genotipo != "")
    req(input$genotipo)
    req(data$data())
    req(input$genotipo  %in% names(data$data()))
    lvl <- as.character(unique(data$data()[,input$genotipo]))
    updatePickerInput(session, inputId = "selected", choices = lvl)
  })
  
  output$segcol <- renderUI({
    validate(
      need(input$column != "", " Fill the area Column "),
      need(input$fila != "", " Fill the area Row ") )
    dt <- data$data()
    dt$col_f = factor( dt[,input$column])
    sliderInput(ns("segcol"), label = "Num of col segments", min = 1, max =  nlevels(dt$col_f)+30, value =  nlevels(dt$col_f) ,width = "100%" )
  })
  
  output$segrow <- renderUI({
    validate(
      need(input$column != "", " "),
      need(input$fila != "", " ") )
    dt <- data$data()
    dt$row_f = factor( dt[,input$fila])
    sliderInput(ns("segrow"), label = "Num of row segments", min = 1, max =  nlevels(dt$row_f)+30, value =  nlevels(dt$row_f), width = "100%" )
  })
  
  observeEvent(input$action, {
    
    if (any(c(input$variable,input$genotipo,input$column,input$fila)=="") ) {
      sendSweetAlert(
        session = session,
        title = "Warning",
        text = HTML("<center> It's necessary that you fill the fields </center>"),
        type = "warning",
        html = T
      )
    }
  })
  
  observeEvent(input$action, {
    shinyjs::enable("inf")
    shinyjs::enable("spatial")
    shinyjs::enable("tabBut")
    shinyjs::enable("coeff")
    shinyjs::enable("downloadReport")
    shinyjs::enable("Rlink")
  })
  
  observeEvent(!input$able, toggle("segcol",anim = TRUE,time = 1,animType = "fade"))
  observeEvent(!input$able, toggle("segrow",anim = TRUE,time = 1,animType = "fade"))
  
  observeEvent(!input$outliers, toggle("times",anim = TRUE,time = 1,animType = "fade"))

  
  # GUIA
  observeEvent(input$btn,
               rintrojs::introjs(session,options = list("nextLabel"="Next",
                                              "prevLabel"="Back",
                                              "skipLabel"="Skip")))
  
  # Modelo SPATS
  
  observeEvent(input$action,{
    if(any(c(input$variable,input$genotipo,input$column,input$fila)=="")){
      return()
    } else {
      shinytoastr::toastr_info(
        title = "Fitting model...",
        message = HTML("<div class='overlay'>  <h2><i class='fa fa-refresh fa-spin'></i></div> <h2> "),
        position =  "bottom-right",progressBar = TRUE, closeButton = T, timeOut = 1000 )
    }
  })
  
  Modelo <- eventReactive(input$action, {
    
      validate(
        need(input$variable != "", " "),
        need(input$genotipo != "", " "),
        need(input$column != "", " "),
        need(input$fila != "", " ") )
    
    dt <- data$data()
    dupl <- sum(duplicated(dt[,c(input$column,input$fila)]))
    dt$Response  <- dt[ ,input$variable] 
    #------- check response ------
  
    tryCatch(
      { 
        if(sum(is.na(dt$Response)) > 0.98*length(dt$Response ) ) stop("Missing data in the response")
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
      }
    )
    if(sum(is.na(dt$Response)) > 0.98*length(dt$Response )) return()
    
    #---------------------------
    
    if (dupl>=1) {    # Duplicated Row-Col
      Modelo <- try(silent = T)
      tryCatch(
        { 
          if(class(Modelo)=="try-error") stop("Duplicated row & column coordinates")
        },
        error = function(e) {
          shinytoastr::toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
        }
      )
      return()
    } else {
      Modelo <- SpATS_mrbean(data$data(), input$variable, input$genotipo, 
                   input$column, input$fila, input$able , input$segcol, input$segrow,
                   input$show_fixed, input$show_random, input$res_ran, input$covariate,
                   input$outliers, input$times, input$selected  )
      Modelo
    }
    
  },ignoreNULL = FALSE) 
  
  
  

  # Coefficients ------------------------------------------------------------

  ## to output blups
  co.spats <- reactive({
    req(Modelo())
    coef <- coef.SpATS(Modelo())
    coef
  })
  
  # SALIDA de los coeff en DT TAble
  output$distTable2 <- DT::renderDataTable(
    if (input$action==0) {return()}
    else {
      DT::datatable({
        co.spats()
      },
      option=list(pageLength=10, scrollX = TRUE,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(co.spats())))),
      filter="top",
      selection="multiple"
      )} )
  
  
  observeEvent(input$coeff,{
    showModal(modalDialog(
      title = "Coefficients", size = "l", easyClose = T,
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("distTable2")),type = 6,color = "#28a745"),
      footer = tagList(
        downloadButton(ns("downloadData2"), 
                       "Download Coefficients", class="btn-success",
                       style= " color: white ; background-color: #28a745; float:left"),
        modalButton("Cancel")
      )
    ))
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("coeff_SpATS_Model_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(co.spats(), file, row.names = FALSE)
    }
  )
  
  

  # BLUPs / BLUEs -----------------------------------------------------------

  blup <- reactive({
    validate(
      need(input$variable != "", " "),
      need(input$genotipo != "", " "),
      need(input$column != "", " "),
      need(input$fila != "", " ") )
    req(Modelo())

    BLUPS <- msa_effects(Modelo())
    BLUPS

  })

  # SALIDA de los datos en DT TAble
  output$distTable <- DT::renderDataTable(
    if (input$action==0) {return()}
    else {
      DT::datatable({
        blup() %>% dplyr::mutate_if(is.numeric, round, 3)
      },
      option=list(pageLength=10, scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0:ncol(blup())))),
      filter="top",
      selection="multiple"
      )} )
  
  
  
  observeEvent(input$tabBut,{
    showModal(modalDialog(
      title = "BLUPs/BLUEs", size = "l", easyClose = T,
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("distTable")),type = 6,color = "#28a745"),
      footer = tagList(
        downloadButton(ns("downloadData"), 
                       "Download Predictions", class="btn-success",
                       style= " color: white ; background-color: #28a745; float:left"),
        modalButton("Cancel")
      )
    ))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("effects_SpATS_Model_mrbean", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(blup(), file, row.names = FALSE)
    }
  )
  
  
    # observe({
    #   print(class(Modelo()))
    # })
  
  return(
    list(Modelo  = Modelo,
         Effects = blup,
         action  = reactive(input$action),
         spatial = reactive(input$spatial),
         res_ran = reactive(input$res_ran),
         inf     = reactive(input$inf),
         Rlink   = reactive(input$Rlink))
  )

  
}
    
## To be copied in the UI
# mod_spats_single_ui("spats_single_ui_1")
    
## To be copied in the server
# callModule(mod_spats_single_server, "spats_single_ui_1")
 
