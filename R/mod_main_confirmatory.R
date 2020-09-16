#' main_confirmatory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_confirmatory_ui <- function(id){

  tagList(
    h1("Main Confirmatory Analysis results"),
    plotOutput(NS(id, "plot")),
    mod_footer_ui("footer")
  )
}
    
#' main_confirmatory Server Function
#'
#' @noRd 
mod_main_confirmatory_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Read exploratory results ---------------------------
    cumulative_res <- readRDS(url(""))
    
    # Generate plot ---------------------------
    output$plot <- renderPlot({
      plot_confirmatory(cumulative_res)
      })
    
    # Add footer ---------------------------
    mod_footer_server("footer")
    })
}
    
## To be copied in the UI
# mod_main_confirmatory_ui("main_confirmatory")
    
## To be copied in the server
# mod_main_confirmatory_server("main_confirmatory")
 
