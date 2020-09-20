#' article UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_article_ui <- function(id){
  
  tagList(
    uiOutput(NS(id, "pdf_view"))
  )
}
    
#' article Server Function
#'
#' @noRd 
mod_article_server <- function(id){
  moduleServer(id, function(input, output, session) {
    # Add iframe to view pdf
    output$pdf_view <- renderUI({
      tags$iframe(style = "height:600px; width:100%", src = "https://mfr.de-1.osf.io/render?url=https://osf.io/sq89t/?direct%26mode=render%26action=download%26mode=render")
      })
    })
}
    
## To be copied in the UI
# mod_article_ui("article")
    
## To be copied in the server
# mod_article_server("article")
