#' exploratory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exploratory_ui <- function(id){

  tagList(
    h1("Histogram overlay of the expected and observed distribution of successful guess rate"),
    plotOutput(NS(id, "plot")),
    mod_footer_ui("footer")
  )
}
    
#' exploratory Server Function
#'
#' @noRd 
mod_exploratory_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Read exploratory results ---------------------------
    exploratory_res <- readRDS(url(""))
    
    # Generate plot ---------------------------
    output$plot <- renderPlot({
      plot_exploratory(success_rates_theoretical_prop,
                       success_rates_empirical_prop,
                       possible_success_rates)
    })
    
    # Add footer ---------------------------
    mod_footer_server("footer")
  })
}
    
## To be copied in the UI
# mod_exploratory_ui("exploratory")
    
## To be copied in the server
# mod_exploratory_server("exploratory")
 
