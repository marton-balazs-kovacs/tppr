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
    textOutput(NS(id, "warning"), style="color:red"),
    plotOutput(NS(id, "plot"))
  )
}
    
#' robustness Server Function
#'
#' @noRd 
mod_robustness_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      # plot results of the Bayesian parameter estimation used as a robustness test
      plot(scale, values$posterior_density_current, type="l", lty=1, xlab="x value", xlim = c(0.45, 0.55),
           ylab="Density")
      abline(v=c(M0_prob, ROPE), lty = c(1, 2))
      density_table = as.data.frame(cbind(scale, values$posterior_density_current))
      names(density_table) = c("scale", "posterior_density")
      height_lim_lb = density_table[density_table[, "scale"] == values$HDI_lb_current, "posterior_density"]
      height_lim_ub = density_table[density_table[, "scale"] == values$HDI_ub_current, "posterior_density"]
      clip(0,1,-10,height_lim_lb)
      abline(v=values$HDI_lb_current, lty = 3)
      clip(0,1,-10,height_lim_ub)
      abline(v=values$HDI_ub_current, lty = 3)
    })
  })
}
    
## To be copied in the UI
# mod_robustness_ui("robustness")
    
## To be copied in the server
# mod_robustness_server("robustness")
 
