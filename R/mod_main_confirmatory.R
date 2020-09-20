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
    h1("Main Confirmatory Analysis results", class = "tab-title"),
    plotly::plotlyOutput(NS(id, "plot")),
    hr(),
    mod_footer_ui(NS(id, "footer"))
  )
}
    
#' main_confirmatory Server Function
#'
#' @noRd 
mod_main_confirmatory_server <- function(id, push_time, refresh_time, current, at_checkpoint){
  moduleServer(id, function(input, output, session) {
    waitress <- waiter::Waitress$new("#main_confirmatory-plot", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Generate plot ---------------------------
    output$plot <- plotly::renderPlotly({
      waitress$start()
      tppr::plot_confirmatory(current()$cumulative_bayes, animated = TRUE)
      })

    # Add footer ---------------------------
    mod_footer_server("footer",
                      push_time = push_time,
                      refresh_time = refresh_time,
                      current = current,
                      at_checkpoint = at_checkpoint)
    })
}
    
## To be copied in the UI
# mod_main_confirmatory_ui("main_confirmatory")
    
## To be copied in the server
# mod_main_confirmatory_server("main_confirmatory")
 
