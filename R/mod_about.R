#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        cardProfile(
          image = "www/johan.jpg",
          title = HTML(
            paste(
              bs4Badge("Johan Aparicio",
                rounded = T,
                position = "right",
                color = "success"
              )
            )
          ),
          subtitle = HTML(
            paste(
              bs4Badge("Statistician",
                rounded = T,
                position = "right",
                color = "success"
              ),
              rep_br(1),
              "<a href='https://www.linkedin.com/in/johan-steven-aparicio-arce-b68976193/'>
              <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
              </a>"
            )
          )
        )
      ),
      column(
        width = 3,
        cardProfile(
          image = "www/salvador.png",
          title = HTML(
            paste(
              bs4Badge("Salvador Gezan",
                rounded = T,
                position = "right",
                color = "success"
              )
            )
          ),
          subtitle = HTML(
            paste(
              bs4Badge("Statistics-Genetics",
                rounded = T,
                position = "right",
                color = "success"
              ),
              rep_br(1),
              "<a href='https://www.linkedin.com/in/salvador-gezan-54768a1a/'>
              <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
              </a>"
            )
          )
        )
      ),
      column(
        width = 3,
        cardProfile(
          image = "www/Daniel.jpg",
          title = HTML(
            paste(
              bs4Badge("Daniel Ariza",
                rounded = T,
                position = "right",
                color = "success"
              )
            )
          ),
          subtitle = HTML(
            paste(
              bs4Badge("Agronomist",
                rounded = T,
                position = "right",
                color = "success"
              ),
              rep_br(1),
              "<a href='https://www.linkedin.com/in/daniel-ariza-suarez/'>
              <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
              </a>"
            )
          )
        )
      ),
      column(
        width = 3,
        cardProfile(
          image = "www/Bodo2.jpg",
          title = HTML(
            paste(
              bs4Badge("Bodo Raatz",
                rounded = T,
                position = "right",
                color = "success"
              )
            )
          ),
          subtitle = HTML(
            paste(
              bs4Badge("Geneticist",
                rounded = T,
                position = "right",
                color = "success"
              ),
              rep_br(1),
              "<a href='https://www.linkedin.com/in/bodo-raatz-76786584/'>
              <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
              </a>"
            )
          )
        )
      )
    ),
    fluidRow(
      col_3(),
      col_3(
        img(src = "/www/alliance.jpg", width = "100%")
      ),
      col_3(
        img(src = "/www/ndsulogo.jpg", width = "100%")
      )
    ),
    br(),
    bs4Card(
      title = div(icon("jsfiddle"), "Rsession"), width = 12,
      verbatimTextOutput(ns("Rsession")),
      br()
    )
  )
}

#' about Server Function
#'
#' @noRd
mod_about_server <- function(input, output, session) {
  ns <- session$ns
  output$Rsession <- renderPrint(
    print(sessionInfo())
  )
}

## To be copied in the UI
# mod_about_ui("about_ui_1")

## To be copied in the server
# callModule(mod_about_server, "about_ui_1")
