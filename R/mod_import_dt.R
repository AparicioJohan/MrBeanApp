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
                     choices = c("Example Data"=1, "Import Data"=2, "BMS Connect"=3),
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
    uiOutput(ns("bmsOut")),
    
    
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
  
  
  output$bmsOut <- renderUI({
    
    if(input$Id004=="3"){
        fluidRow(
          column(width = 4,
                 fluidRow(
                 bs4Dash::box(title =  tagList(shiny::icon("question-circle"), "Help"), 
                              solidHeader = FALSE,width = 12,status = "success",
                              h3("How to use BMS within Mr.Bean?"),
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
                              materialSwitch(ns("config"),label = "Configuration",status = "success", right = T, width = "100%"),
                              div(id=ns("bms_config"),
                                  fluidRow(
                                    col_12(
                                      textInput(inputId = ns("protocol"), label = "Protocol", value = "http://", width = "100%"),
                                      textInput(inputId = ns("bdIP"), label = "BMS Server IP Address", value = "bms.ciat.cgiar.org", width = "100%"),
                                    )
                                  )
                              ),
                              textInput(ns("user"),label = tagList(shiny::icon("user"), "User:"),placeholder = "username",width = "100%" ),
                              passwordInput(ns("password"), tagList(shiny::icon("key"), "Password:"),width = "100%",placeholder = "*****************"),
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
                                          choices="",width = "100%", multiple = T)
                              )
                        )
                 )
        )
    }

  })
  
  observeEvent(sum(input$config)==0, toggle("bms_config",anim = TRUE,time = 1,animType = "fade"))


  # BMS ---------------------------------------------------------------------

  bmscon <- reactive({
    conectBMS(user = input$user, password = input$password, protocol = input$protocol , db = input$bdIP)
  })
  
  crops <- reactive({
    cropBMS(conection = bmscon())
  })
  
  observeEvent(input$mysql, {
    if (is.null(bmscon())) {
      shinyalert::shinyalert(title = "Incorrect username or password", type = "error",confirmButtonCol = "#28a745")
    } else {    
      shinyalert::shinyalert(title = "Welcome to BMS!", type = "success", text = input$user,confirmButtonCol = "#28a745",
                           imageUrl="www/0.png",
                           animation ="slide-from-top" )
      updateSelectInput(session , inputId = "Id008", choices = crops() )
      }
  })
  
  programs <- reactive({
    programBMS(conection = bmscon(), crop = input$Id008 )
  })
  
  observeEvent(input$Id008,{
    updateSelectInput(session, inputId = "program",choices = programs() )
  })
  
  trials <- reactive({
    trialBMS(conection = bmscon(), cropSele = input$Id008, programSele = input$program )
  })
  
  observeEvent(input$program,{
     if(!is.null(trials())){
       updateSelectInput(session, inputId = "trial",choices = trials())
       shinytoastr::toastr_info("Succesful! Now select the Trial",position =  "bottom-right")
     } else {
       req(input$program)
       shinytoastr::toastr_error(title = "Warning:",
         paste0("There are no trials in '",  input$program ,"' program."),
         position =  "bottom-right",progressBar = TRUE)
       updateSelectInput(session, inputId = "trial",choices = " ")
       }
  })
  

  # data --------------------------------------------------------------------

  DtReact <- reactive({
    BMS(conection = bmscon() , crop = input$Id008 , programSel = input$program , trialSel =  input$trial)
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
                   dataBMS = DtReact() )
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
 
