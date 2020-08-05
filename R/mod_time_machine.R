#' time_machine UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_time_machine_ui <- function(id){
  tagList(
 h1("Evidence evolving with the accumulation of data"),
 plotOutput(NS(id, "plot"))
  )
}
    
#' time_machine Server Function
#'
#' @noRd 
mod_time_machine_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      ggplot(values$BF_results_for_plotting_loop, aes(y = Bayes_factor_01, x = BF_type))+
        geom_point()+
        geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=c(Inference_threshold_BF_high), ymax=c(Inf)), alpha = 0.2, fill=c("pink"))+
        geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=c(Inference_threshold_BF_low), ymax=c(Inference_threshold_BF_high)), alpha = 0.2, fill=c("grey80"))+
        geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=c(0), ymax=c(Inference_threshold_BF_low)), alpha = 0.2, fill=c("lightgreen"))+
        geom_point(size = 3.5, shape = 21, fill = "white")+
        scale_y_log10(limits = c(0.005,200), breaks=c(0.01, Inference_threshold_BF_low, 0.1, 0.33, 0, 3, 10, Inference_threshold_BF_high, 100))+
        geom_hline(yintercept = c(Inference_threshold_BF_low, Inference_threshold_BF_high), linetype = "dashed")+
        geom_text(aes(x=0.5, y=c(100, 1, 0.01), label=c("Supports M0", "Inconclusive", "Supports M1"), angle = 270))
    })
  })
}
    
## To be copied in the UI
# mod_time_machine_ui("time_machine")
    
## To be copied in the server
# mod_time_machine_server("time_machine")
 
