#' descrip_raw UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descrip_raw_ui <- function(id, title=c("Scatterplot", "Boxplot"), In , Out, corr=T){
  ns <- NS(id)
  tagList(
   fluidRow(
      # bs4InfoBoxOutput(ns("Yelement"), width = 6),
      # bs4InfoBoxOutput(ns("Xelement"), width = 6)
    ),
   
   fluidRow(
     bs4Dash::box( 
      dropdown(
        
        tags$h3("Input List"),
        
        uiOutput(ns("vary")),
        uiOutput(ns("varx")),
        awesomeCheckbox(inputId = ns("factor_scat"),
                        label = "Grouping variable?",
                        value = F,
                        status = "danger"),
        uiOutput(ns("factor2")),
        actionButton(ns("actionplot"), label = "Plot", 
                     class = "btn-success", style="display:rigth ;color: white  ; background-color: #28a745"),
        
        animate = shinyWidgets::animateOptions(
          enter = shinyWidgets::animations$fading_entrances[[In]],
          exit =  shinyWidgets::animations$fading_exits[[Out]]
        ),
        style = "unite", icon = icon("gear"),
        status = "warning", width = "300px"
      ),
      shinycssloaders::withSpinner( 
        plotly::plotlyOutput(ns("plot")),type = 5,color = "#28a745"
        ),
      if(corr) uiOutput(ns("correlation")),
        width = 12,title = title ,status = "success",solidHeader = TRUE, maximizable = T
      )
    )
  )
}
    
#' descrip_raw Server Function
#'
#' @noRd 
mod_descrip_raw_server <- function(input, output, session, data, plot = c(1,2) ){
  ns <- session$ns
  
  
  output$varx <- renderUI({
    selectInput(ns("variablex"), "Select variable for X-axis", choices = names(data$data()), selected = "col")
  })
  output$vary <- renderUI({
    selectInput(ns("variabley"), "Select variable for Y-axis ", choices=names(data$data()), selected = "row")
  })
  output$factor2 <- renderUI({
    selectInput(ns("factor2"), "Select a grouping variable", choices=names(data$data()),selected = "yield")
  })
  
  observe({
    toggle(id = "factor2", condition = input$factor_scat, animType = "fade",anim = TRUE)
  })
  
  
  output$plot <- plotly::renderPlotly({
    req(input$actionplot)
    if(isTRUE(input$factor_scat)){
      isolate({
        req(input$factor2)
        dt <- data$data()
        dt[,input$factor2] <- as.factor(dt[,input$factor2])
        
        if(plot==1){
          gra <- ggplot(dt,aes_string(x=input$variablex, y=input$variabley,color=input$factor2)) +
            geom_point() + theme_bw()
        } else {
          dt[,input$variablex] <- as.factor(dt[,input$variablex])
          gra <- ggplot(dt,aes_string(x=input$variablex, y=input$variabley,fill=input$factor2)) +
            geom_boxplot() + theme_bw() + ggtitle("Boxplot") +
            facet_wrap(~.data[[input$factor2]], scales = "free_x")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        plotly::ggplotly(gra)
      })} else {
        isolate({
          dt <- data$data()
          dt[,input$factor2] <- as.factor(dt[,input$factor2])
          if(plot==1){
            gra <- ggplot(dt,aes_string(x=input$variablex, y=input$variabley)) +
              geom_point() + theme_bw()
          } else {
            dt[,input$variablex] <- as.factor(dt[,input$variablex])
            gra <- ggplot(dt,aes_string(x=input$variablex, y=input$variabley,fill=input$variablex)) +
              geom_boxplot() + theme_bw() + ggtitle("Boxplot")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          plotly::ggplotly(gra)
        })
      }
  }) 
  
  

# Correlation -------------------------------------------------------------

  output$correlation <- renderUI({

    req(input$actionplot)
    
    isolate({
    if(plot==1){
      req(input$variablex)
      req(input$variabley)
      x <- data$data()[,input$variablex]
      y <- data$data()[,input$variabley]
      req(is.numeric(x))
      req(is.numeric(y))
        C <- round(cor(x, y, use = "pairwise.complete.obs"),3)
        P <- round(cor.test(x, y)$p.value,3)
        Corr <- paste("Correlation =", C, "/ p.value =", P)
        return(
          tagList(
            br(),
            materialSwitch(ns("corr_togg"),label = "Correlation",status = "success", right = T, width = "100%"),
            div(id=ns("corr_show"),
                h5(Corr) # style="text-align: center;"
                )
              )
            )
    } else {
      return()
    }
    })
  })  
  
  observeEvent(input$corr_togg==TRUE, toggle("corr_show",anim = TRUE,time = 1,animType = "fade"))
  

# valuebox ----------------------------------------------------------------

  
  output$Yelement <- renderInfoBox({
    req(input$actionplot)
    
    isolate({
      y <- length(unique(data$data()[,input$variabley]))
      st <- ifelse(plot==1,'success', "")
      title <- ifelse(plot==1,
                      paste(input$variabley),
                      paste(input$variabley))
      bs4InfoBox(
        title = title,
        value = y,
        icon = "ruler-vertical",
        status = st,
        iconElevation = 3
      )
    })
  })  
  
  output$Xelement <- renderInfoBox({
    req(input$actionplot)
    
    isolate({
      x <- length(unique(data$data()[,input$variablex]))
      st <- ifelse(plot==1,"info", "danger")
      title <- ifelse(plot==1,
                      paste(input$variablex),
                      paste(input$variablex))
      bs4InfoBox(
        title = title,
        value = x,
        icon = "ruler-horizontal",
        status = st,
        iconElevation = 3
      )
    })
  })  
  
}
    
## To be copied in the UI
# mod_descrip_raw_ui("descrip_raw_ui_1")
    
## To be copied in the server
# callModule(mod_descrip_raw_server, "descrip_raw_ui_1")
 
