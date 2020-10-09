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
    waiter <- waiter::Waiter$new(
      id = "main_confirmatory-plot",
      html = tagList(
        div(
          tags$img(src = "www/favicon.png", height = "150px"),
          h5("Loading the plot...", style = "color: #6d5c7c;"),
          style="display:inline-block; width: 150px;")
      ),
      color = "#FBEEEF")
    
    # Generate plot ---------------------------
    output$plot <- plotly::renderPlotly({
      waiter$show()
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
 
