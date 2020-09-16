#' robustness UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_robustness_ui <- function(id){

  tagList(
    h1("Result of the Bayesian Parameter Estimation Robustness Analysis"),
    plotOutput(NS(id, "plot")),
    mod_footer_ui("footer")
  )
}
    
#' robustness Server Function
#'
#' @noRd 
mod_robustness_server <- function(id, refresh_time){
    moduleServer(id, function(input, output, session) {
      # Read robustness results ---------------------------
      robustness_bf_res <- readRDS(url(""))
      
      # Generate plot ---------------------------
    output$plot <- renderPlot({
      plot_robustness(posterior_density,
                      hdi_mode,
                      hdi_l,
                      hdi_u,
                      mixed_ci_width,
                      mixed_ci_l,
                      mixed_ci_u)
        
  })
    
    # Add footer ---------------------------
    mod_footer_server("footer")
  })
}
    
## To be copied in the UI
# mod_robustness_ui("robustness")
    
## To be copied in the server
# mod_robustness_server("robustness")
 
