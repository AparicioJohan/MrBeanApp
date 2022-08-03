#' GBLUP_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_GBLUP_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("notfound"),
      rep_br(2),
      HTML("<center><img src='www/code.svg' width='60%' height='60%'></center>")
    ),
    shinyjs::hidden(
      div(
        id = ns("second"),
        HTML('<h1 style="font-weight: bold; color: #00a65a;">GBLUP</h1>'),
        fluidRow(
          column(
            12,
            fluidRow(
              bs4Card(
                pickerInput(
                  inputId = ns("selected"),
                  label = "Trait",
                  choices = "",
                  options = list(
                    title = "Select a trait...", 
                    size = 5
                  ), 
                  width = 'fit',
                  inline = TRUE
                ),
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
                ),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("plot_gblups")),
                  type = 5,
                  color = "#28a745"
                ),
                title = tagList(
                  icon = icon("sort-numeric-up", verify_fa = FALSE), 
                  "Predictions Plot"
                ),
                solidHeader = FALSE, 
                status = "success",
                width = 12, 
                collapsed = FALSE,
                maximizable = TRUE
              )
            )
          )
        )
      )
    )
  )
}

#' GBLUP_results Server Functions
#'
#' @noRd
mod_GBLUP_results_server <- function(id, gblup) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      toggle(id = "notfound", condition = is.null(gblup$model()))
      toggle(id = "second", condition = !is.null(gblup$model()))
    })
    
    observe({
      req(gblup$model())
      table <- gblup$model()$results$GBLUP
      traits <- as.character(unique(table$trait))
      updatePickerInput(session, 
                        inputId = "selected", 
                        choices = traits,
                        selected = traits[1])
    })
    
    
    output$plot_gblups <- plotly::renderPlotly({
      req(gblup$model())
      req(input$selected)
      if (input$include_predicted) {
        type_pred <- c("fit", "prediction")
      } else {
        type_pred <- "fit"
      }
      trait_selected <- input$selected
      tryCatch(
        {
          BLUPS <- gblup$model()$results$GBLUP %>% 
            dplyr::filter(type %in% type_pred) %>% 
            dplyr::filter(trait %in% trait_selected) %>% 
            dplyr::select(
              type, level, predicted.value, standard.error, reliability
            )
          BLUPS$Lu <- BLUPS[, 3] - 1.645 * BLUPS[, 4]
          BLUPS$Ls <- BLUPS[, 3] + 1.645 * BLUPS[, 4]
          v <- as.character(BLUPS[order(BLUPS[, 3], decreasing = TRUE), 2])
          names(BLUPS)[2] <- "Line"
          names(BLUPS)[3] <- "GBLUP"
          lvls_type <- length(unique(BLUPS$type))
          if (lvls_type >= 2) {
            q <- ggplot(
              BLUPS,
              aes(x = Line, y = GBLUP, color = type, label = reliability)
              )
          } else {
            q <- ggplot(BLUPS, aes(x = Line, y = GBLUP, label = reliability))
          }
          p <- q +
            geom_point(size = 1) +
            geom_errorbar(aes(ymax = Ls, ymin = Lu)) +
            theme_bw() +
            geom_hline(
              yintercept = mean(BLUPS[, 3]), 
              linetype = 2, 
              color = "grey"
            ) +
            theme(
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()
            ) +
            ylab(names(BLUPS)[3]) +
            scale_x_discrete(limits = v)
          plotly::ggplotly(p)
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
    
  })
}

## To be copied in the UI
# mod_GBLUP_results_ui("GBLUP_results_1")

## To be copied in the server
# mod_GBLUP_results_server("GBLUP_results_1")
