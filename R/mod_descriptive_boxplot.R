#' descrip_boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descrip_boxplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box( 
        dropdown(
          tags$h3("Input List"),
          uiOutput(ns("vary")),
          uiOutput(ns("varx")),
          awesomeCheckbox(
            inputId = ns("factor_scat"),
            label = "Grouping variable?",
            value = F,
            status = "danger"
          ),
          uiOutput(ns("factor2")),
          actionButton(
            ns("actionplot"), 
            label = "Plot", 
            class = "btn-success",
            style = "display:rigth ;color: white  ; background-color: #28a745"
          ),
          animate = shinyWidgets::animateOptions(
            enter = shinyWidgets::animations$fading_entrances[["fadeInRightBig"]],
            exit = shinyWidgets::animations$fading_exits[["fadeOutRightBig"]]
          ),
          style = "unite",
          icon = icon("gear", verify_fa = FALSE),
          status = "warning", 
          width = "300px"
        ),
        shinycssloaders::withSpinner( 
          plotly::plotlyOutput(ns("plot")),
          type = 5,
          color = "#28a745"
        ),
        width = 12,
        title = tagList(shiny::icon("stats", lib = "glyphicon"), "Boxplot"),
        status = "success",
        solidHeader = FALSE, 
        maximizable = T
      )
    )
  )
}
    
#' descrip_boxplot Server Functions
#'
#' @noRd 
mod_descrip_boxplot_server <- function(id, data, plot = 2 ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$varx <- renderUI({
      selectInput(
        ns("variablex"), 
        "Select variable for X-axis", 
        choices = names(data$data()),
        selected = "col")
    })
    
    output$vary <- renderUI({
      selectInput(
        ns("variabley"), 
        "Select variable for Y-axis ", 
        choices = names(data$data()), 
        selected = "row"
      )
    })
    
    output$factor2 <- renderUI({
      selectInput(
        ns("factor2"), 
        "Select a grouping variable", 
        choices = names(data$data()),
        selected = "yield")
    })
    
    observe({
      toggle(
        id = "factor2", 
        condition = input$factor_scat, 
        animType = "fade",
        anim = TRUE
      )
    })
    
    
    output$plot <- plotly::renderPlotly({
      req(input$actionplot)
      if(isTRUE(input$factor_scat)){
        isolate({
          req(input$factor2)
          dt <- data$data()
          dt[,input$factor2] <- as.factor(dt[,input$factor2])
          if(plot==1){
            
          } else {
            dt[,input$variablex] <- as.factor(dt[,input$variablex])
            gra <- ggplot(dt,
                          aes_string(
                            x = input$variablex, 
                            y = input$variabley,
                            fill = input$factor2)) +
              geom_boxplot() + 
              theme_bw() + 
              ggtitle("Boxplot") +
              facet_wrap(~.data[[input$factor2]], scales = "free_x")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          plotly::ggplotly(gra)
        })} else {
          isolate({
            dt <- data$data()
            dt[,input$factor2] <- as.factor(dt[,input$factor2])
            if(plot==1){
              
            } else {
              dt[,input$variablex] <- as.factor(dt[,input$variablex])
              gra <- ggplot(dt,
                            aes_string(
                              x = input$variablex, 
                              y = input$variabley,
                              fill = input$variablex)
              ) +
                geom_boxplot() + 
                theme_bw() +
                ggtitle("Boxplot")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
            plotly::ggplotly(gra)
          })
        }
    }) 
  })
}
    
## To be copied in the UI
# mod_descrip_boxplot_ui("descrip_boxplot_1")
    
## To be copied in the server
# mod_descrip_boxplot_server("descrip_boxplot_1")
