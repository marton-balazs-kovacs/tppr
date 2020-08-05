#' read_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_data_ui <- function(id){

  tagList(
    selectInput(NS(id, "data_type"), "Select data type", c("live", "test", "pilot"), "live")
  )
}
    
#' read_data Server Function
#'
#' @noRd 
mod_read_data_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Read data ---------------------------
    target_data_pre <- reactive({read_data(type = input$data_type)})
    
    # Data management ---------------------------
    observe({
      # Trigger observer
      invalidateLater(refresh_time)
      
    }) 
  })
}
    
## To be copied in the UI
# mod_read_data_ui("read_data")
    
## To be copied in the server
# mod_read_data_server("read_data")
 
