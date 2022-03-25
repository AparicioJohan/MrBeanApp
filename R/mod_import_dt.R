#' import_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_dt_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(title = tagList(shiny::icon("upload"), "Source"),
                   solidHeader = FALSE,status = "success",maximizable = F,closable = F,
                   width = 3,
                   radioGroupButtons(
                     inputId = ns("Id004"),
                     choices = c("Example Data"=1, "Import Data"=2, "BrAPI"=3), # , "BRAPI" = 4
                     status = "success",selected = 1
                    ),
                   conditionalPanel("input.Id004==1",h6('Use the example database to try the different modules of Mr. Bean'), ns = ns),
                   conditionalPanel("input.Id004==2",h6('Import external data preferably csv/txt files.'), ns = ns)
      ),
      
      column(width = 3,
             conditionalPanel("input.Id004==2", ns = ns,
                              bs4Dash::box(title =  tagList(shiny::icon("file-upload"), "Import Data"),solidHeader = FALSE,width = 12, status = "success",
                                           maximizable = T,closable = F,
                                           fileInput(inputId=ns('file1'),width = "100%",
                                                     label='Load your database',
                                                     accept = c(
                                                       'text/csv',
                                                       'text/comma-separated-values',
                                                       'text/tab-separated-values',
                                                       'text/plain',
                                                       '.csv',
                                                       '.tsv','xlsx'
                                                     )),
                                           helpText("Default max. file size is 100MB"),
                                           prettyCheckbox(
                                             inputId = ns("header"), label = "Include Header?", icon = icon("check"),outline = TRUE,fill = FALSE,shape="square",
                                             animation = "tada", value=TRUE,status = "success"
                                           )
                              ))
       ),
      column(width = 3,
             conditionalPanel("input.Id004==2", ns=ns,
                              shinyjs::hidden(
                                div(id = ns("when_file1"),
                                    bs4Dash::box(title =  tagList(shiny::icon("wrench"), "Attributes"),solidHeader = FALSE,
                                                 maximizable = T,closable = F,
                                                 radioButtons(inputId=ns("miss"),label="Missing value character: ",
                                                              choices = list("NA",'Empty', "Other"),inline = T),
                                                 conditionalPanel("input.miss=='Other'", ns=ns, 
                                                                  textInput(ns("datamiss"), label = "String", width = "100%")),
                                                 selectInput(inputId=ns("sep"),
                                                             label = "Cell separation character:", 
                                                             choices = list(Tab='\t', Comma=',',
                                                                            Semicolon=';', 'Space'=' '),
                                                             selected = ';', width = "100%"),
                                                 uiOutput(ns("oshet")), # sheet
                                                 width = 12, status = "success"
                                                 )
                                )
                              )
                              )
       ),
      column(width = 3,
             conditionalPanel("input.Id004==2", ns=ns,
                              shinyjs::hidden(
                                div(id = ns("when_file2"),
                                    bs4Dash::box(title =  tagList(shiny::icon("filter"), "Subset"),solidHeader = FALSE,
                                                 maximizable = T,closable = F,
                                                 prettyCheckbox(
                                                   inputId = ns("subset"), label = "Select a data subset", icon = icon("check"),outline = TRUE,fill = FALSE,shape="square",
                                                   animation = "tada", value=FALSE,status = "success"
                                                 ),
                                                 selectInput(inputId=ns("varsubset"),width = "100%",
                                                             label= tagList( "Subset variable",tags$a(icon("exclamation-circle"))),
                                                             choices=""),
                                                 selectInput(inputId=ns("levelessub"),multiple = T,width = "100%",
                                                             label= tagList( "Which level?",tags$a(icon("exclamation-circle"))),
                                                             choices=""),
                                                 width = 12, status = "success"
                                                 )
                                    )
                                )
                              )  
      )
      
    ),
    conditionalPanel("input.Id004==3", ns = ns,
                     fluidRow(
                       column(width = 4,
                              fluidRow(
                                bs4Dash::box(title =  tagList(shiny::icon("question-circle"), "Help"), 
                                             solidHeader = FALSE,width = 12,status = "success",
                                             h3("How to connect BrAPI in MrBean?"),
                                             hr(),
                                             includeHTML(
                                               system.file("app/www/icon.html", package = "MrBean")
                                             )
                                )
                              )
                       ),
                       column(width = 4,
                              fluidRow(
                                bs4Dash::box(title = tagList(shiny::icon("users"), "BMS"),solidHeader = FALSE,width = 12,status = "success",
                                             textInput(inputId = ns("urlbms"), 
                                                       label = tagList(
                                                         shiny::icon("server"), 
                                                         "Server",
                                                         tooltip(icon("question-circle"),
                                                                 title = "For example: \n https://cassavabase.org \n https://sweetpotatobase.org \n https://bms.ciat.cgiar.org/", 
                                                                 placement = "top")
                                                         ), 
                                                       value = "https://bms.ciat.cgiar.org/ibpworkbench/controller/auth/login", width = "100%"),
                                             awesomeCheckbox(inputId = ns('no_auth') ,
                                                             label='No authentication required?',  
                                                             value = FALSE, status = "danger"  ),
                                             prettyRadioButtons(
                                               inputId = ns("engine"),
                                               label = "Engine:", 
                                               choices = c("BMS" = "bms", "BreedBase" = "breedbase"),
                                               icon = icon("check"), 
                                               inline = TRUE,
                                               bigger = TRUE,
                                               status = "success",
                                               animation = "jelly"
                                             ),
                                             conditionalPanel("input.no_auth==false", ns = ns,
                                                              textInput(ns("user"),label = tagList(shiny::icon("user"), "User:"),placeholder = "username",width = "100%" ),
                                                              passwordInput(ns("password"), tagList(shiny::icon("key"), "Password:"),width = "100%",placeholder = "*****************")
                                                              ),
                                             actionButton(ns("mysql"),label = "Conect",icon = icon("sync")),
                                             strong(a("Can't Log In?", href="http://bms.ciat.cgiar.org:48080/ibpworkbench/controller/auth/login"))
                                )
                              )
                       ),
                       column(width = 4,
                              fluidRow(
                                bs4Dash::box(title= tagList(shiny::icon("cogs"), "Information"),status = "success",width = 12,solidHeader = FALSE,
                                             selectInput(inputId=ns("Id008"),
                                                         label=  "Crops",
                                                         choices="",width = "100%"),
                                             
                                             selectInput(inputId=ns("program"),
                                                         label= tagList( "Which program?",tags$a(icon("exclamation-circle"))),
                                                         choices="",width = "100%"),
                                             selectInput(inputId=ns("trial"),
                                                         label= tagList( "Which trial?",tags$a(icon("exclamation-circle"))),
                                                         choices="",width = "100%", multiple = T),
                                             selectInput(inputId=ns("study"),
                                                         label= tagList( "Which study?",tags$a(icon("exclamation-circle"))),
                                                         choices="",width = "100%", multiple = T),
                                             fluidRow(
                                               col_3(),
                                               col_6(
                                                 actionBttn(inputId = ns("ok2"),
                                                            label = "Search!",
                                                            style = "jelly",color = "success",block = T, icon = icon("check") )
                                               ),
                                               col_3()
                                             )
                                             
                                )
                              )
                       )
                     )
                     ),
    conditionalPanel("input.Id004==4", ns = ns,
                     fluidRow(
                       column(width = 4,
                              fluidRow(
                                bs4Dash::box(title =  tagList(shiny::icon("question-circle"), "Help"), 
                                             solidHeader = FALSE,width = 12,status = "success",
                                             h3("How to use BRAPI in Mr.Bean?"),
                                             hr(),
                                             includeHTML(
                                               system.file("app/www/icon2.html", package = "MrBean")
                                             )
                                )
                              )
                       ),
                       column(width = 4,
                              fluidRow(
                                bs4Dash::box(title= tagList(shiny::icon("cogs"), "Information"),status = "success",width = 12,solidHeader = FALSE,
                                             fluidRow(
                                               col_12(
                                                 selectInput(inputId=ns("brapi_crops"),
                                                             label=  "Database",
                                                             choices= "",
                                                             # selected = "",
                                                             width = "100%")
                                               )
                                             ),
                                             selectInput(inputId=ns("brapi_program"),
                                                         label= tagList( "Which program?",tags$a(icon("exclamation-circle"))),
                                                         choices="",width = "100%"),
                                             selectInput(inputId=ns("brapi_trial"),
                                                         label= tagList( "Which trial?",tags$a(icon("exclamation-circle"))),
                                                         choices="",width = "100%", multiple = T),
                                             fluidRow(
                                               col_3(),
                                               col_6(
                                                 actionBttn(inputId = ns("ok"),
                                                            label = "Search!",
                                                            style = "jelly",color = "success",block = T, icon = icon("check") )
                                               ),
                                               col_3()
                                             )
                                )
                              )
                       )
                     )
                     
                     ),
    
    
    
    fluidRow(bs4Dash::box(collapsed = F,maximizable = T,closable = F,
                          shinycssloaders::withSpinner( 
                            DT::dataTableOutput(ns("data")),type = 5,color = "#28a745" 
                            ),#style = "overflow-x: scroll;",  # overflow-y: scroll;
                          width = 12,title =  tagList(shiny::icon("file-import"), "Data"),status = "success",solidHeader = FALSE,collapsible = TRUE) 
             ),br() 
  )
}
    
#' import_dt Server Function
#'
#' @noRd 
mod_import_dt_server <- function(input, output, session){
  ns <- session$ns
  
  
  observeEvent(input$file1,
               shinyjs::show("when_file1",animType = "fade",anim = TRUE))
  
  output$oshet <- renderUI({
    inFile <- input$file1
    Ext <- tools::file_ext(inFile$datapath)
    req(input$file1,Ext=="xlsx"|Ext=="xls")
    selectInput(inputId = ns("sheet"),label =  "Sheet Excel", choices=readxl::excel_sheets(inFile$datapath), width = "100%")
  })

  
  observeEvent(input$file1,
               shinyjs::show("when_file2",animType = "fade",anim = TRUE))
  

# BRAPI interface ---------------------------------------------------------
  
  w <- Waiter$new(
    html = HTML("<center> <div class='ball-loader'></div> </center>"), 
    color = transparent(0.3)
  )
  
  observe({
    updateSelectInput(session, "brapi_crops", choices= brapirv1::brapi_db() %>% names(), selected = "YdHa_clean")
  })

  # brapi data
  
  programs_brapi <- reactive({
    crop <- input$brapi_crops
    tryCatch(
      { 
        list_programs <- cropbrapi(brapi_crop = crop)
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                  showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
        w$hide()
      }
    )
    if(!exists("list_programs")) list_programs <- NULL
    return(list_programs)
  })
  
  observeEvent(input$brapi_crops,{
    if(is.null(programs_brapi())){
      options <- ""
    } else {options = programs_brapi()$list_programs}
    updateSelectInput(session, inputId = "brapi_program",choices = options , selected = "NNNNN")
  })
  
  brapi_trials <- reactive({
    w$show()
    program <- input$brapi_program
    tmp <- programs_brapi()
    tryCatch(
      { 
        list_trials <- trialbrapi(brapi_program = program, cropbrapi = tmp)
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                  showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
        w$hide()
      }
    )
    w$hide()
    if(!exists("list_trials")) list_trials <- NULL
    return(list_trials)
  })
  
  observeEvent(input$brapi_program,{
    if(is.null(brapi_trials())){
      options <- ""
    } else {options = brapi_trials()$list_trials}
    suppressWarnings(updateSelectInput(session, inputId = "brapi_trial",choices = options , selected = "NNNNN"))
  })
  

  Dtbrapi <- reactive({
    input$ok
    isolate({
      w$show()
      trials <- input$brapi_trial
      tmp <- brapi_trials()
      tryCatch(
        { 
          datos <- databrapi(brapi_trials = trials, trialbrapi = tmp)
        },
        error = function(e) {
          shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                    showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
          w$hide()
        }
      )
      w$hide()
      if(!exists("datos")) datos <- NULL
      return(datos)
    })
  })
  

# BMS interface -----------------------------------------------------------
  

  bmscon <- reactive({
    input$mysql
    isolate({
      tryCatch(
        {
          tmpbms <- qbmsbrapi(url = input$urlbms, 
                              username = input$user,
                              password = input$password, 
                              engine = input$engine, 
                              no_auth = input$no_auth)
        },
        error = function(e) {
          shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                    showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
        }
      )
      if(!exists("tmpbms")) tmpbms <- NULL
      return(tmpbms)
    })
  })
  
  crops <- reactive({
    # input$mysql
    # isolate({
    return(bmscon()$crops)
    # })
  })
  
  observeEvent(input$mysql, {
    if (is.null(bmscon())) {
      shinyalert::shinyalert(title = "Incorrect username or password", type = "error",confirmButtonCol = "#28a745")
    } else {    
      shinyalert::shinyalert(title = paste0("Welcome to ", input$engine, "!"), type = "success", text = "",confirmButtonCol = "#28a745",
                           imageUrl= ifelse(input$engine == "bms",  "www/0.png" , "www/brapi.png"),
                           animation ="slide-from-top" )
      updateSelectInput(session , inputId = "Id008", choices = crops(), selected = "NNNNN" )
      }
  })
  
  programs <- reactive({
    crop <- input$Id008
    tryCatch(
      {
        list_programs <- qbmsprograms(crop = crop)
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                  showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
      }
    )
    if(!exists("list_programs")) list_programs <- NULL
    return(list_programs[[1]])
  })

  observeEvent(input$Id008,{
    if(is.null(programs())){
      return()
    } else {
      updateSelectInput(session, inputId = "program",choices = programs(), selected = "NNNNN" )
    }
  }, ignoreInit = TRUE)
  
  trials <- reactive({
    w$show()
    tryCatch(
      {
        list_trials <- qbmstrials(program = input$program)
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                  showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
        w$hide()
      }
    )
    w$hide()
    if(!exists("list_trials")) list_trials <- NULL
    return(list_trials$trialName)
  })

  observeEvent(input$program,{
    if(is.null(trials())){
      options <- ""
    } else {options = trials()}
    suppressWarnings(updateSelectInput(session, inputId = "trial",choices = options , selected = "NNNNN"))
  }, ignoreInit = TRUE)
  
  
  studies <- reactive({
    w$show()
    tryCatch(
      {
        list_studies <-lapply(input$trial, qbmsstudies)
        names(list_studies) <- input$trial
        dt_std <- data.frame(plyr::ldply(list_studies[], data.frame, .id = "trial"))
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                  showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
        w$hide()
      }
    )
    w$hide()
    if(!exists("dt_std")) dt_std <- NULL
    return(dt_std)
  })
  
  observeEvent(input$trial,{
    if(is.null(studies())){
      options <- ""
    } else {options = studies()[[2]]}
    suppressWarnings(updateSelectInput(session, inputId = "study",choices = options , selected = "NNNNN"))
  }, ignoreInit = TRUE)

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
          shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                    showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
          w$hide()
        }
      )
      w$hide()
      if(!exists("datos")) datos <- NULL
      return(datos)
    })
      
    
  })
  
  dataset <- reactive({
    tryCatch(
      {
        dataReact( file =  input$file1,
                   choice = input$Id004,
                   header = input$header,
                   sep = input$sep,
                   miss = input$miss, 
                   string = input$datamiss , 
                   sheet = input$sheet,
                   dataBMS = DtReact(),
                   dataBRAPI = Dtbrapi())
      },
      error = function(e) {
        shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
                                  showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
      }
    )
  })
  
  
  # Subset data
  
  observeEvent(input$file1,{
    updatePrettyCheckbox(session = session, inputId = "subset", value = F)
  })
  
  observe({
    updateSelectInput(session, "varsubset", choices=names(dataset()),selected = "NNNNN")
  })
  
  observeEvent(input$subset==F,{
    toggle("varsubset",anim = TRUE,time = 1,animType = "fade")
  })
  
  observeEvent(input$varsubset,{
    toggle("levelessub",anim = TRUE,time = 1,animType = "fade",condition = input$varsubset!="")
    req(input$varsubset)
    lvl <- dataset()[,input$varsubset]
    updateSelectInput(session, "levelessub", choices=lvl,selected = "NNNNN")
  })
  
  
  dataset_sub <- reactive({
    dts(input$subset,input$varsubset,input$levelessub,data=dataset())
  })
  
  output$data <- DT::renderDataTable({
    req(dataset_sub())
    # DT::datatable(dataset_sub(),filter = 'top', selection="multiple",
    #               options = list(scrollX = TRUE, pageLength = 5,
    #                              columnnDefs=list(list(className = 'dt-center', targets = 0:ncol(dataset_sub()))) ))
    DT::datatable({
      dataset_sub() 
    },
    option=list(pageLength=3, scrollX = TRUE,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(dataset_sub())))),
    filter="top",
    selection="multiple"
    )
    
  })
  
  
  return(list(data = dataset_sub))
  
  
}
    
## To be copied in the UI
# mod_import_dt_ui("import_dt_ui_1")
    
## To be copied in the server
# callModule(mod_import_dt_server, "import_dt_ui_1")
 
