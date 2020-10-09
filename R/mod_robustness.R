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
    h1("Result of the Bayesian Parameter Estimation Robustness Analysis", class = "tab-title"),
    plotOutput(NS(id, "plot")),
    hr(),
    mod_footer_ui(NS(id,"footer"))
  )
}
    
#' robustness Server Function
#'
#' @noRd 
mod_robustness_server <- function(id, push_time, refresh_time, current, at_checkpoint){
    moduleServer(id, function(input, output, session) {
      waiter <- waiter::Waiter$new(
        id = "robustness-plot",
        html = tagList(
          div(
            tags$img(src = "www/favicon.png", height = "150px"),
            h5("Loading the plot...", style = "color: #6d5c7c;"),
            style="display:inline-block; width: 150px;")
            ),
        color = "#FBEEEF")
      
      # Generate plot ---------------------------
      output$plot <- renderPlot({
        waiter$show()
        tppr::plot_robustness(posterior_density = current()$robustness_bayes$posterior_density,
                              hdi_mode = current()$robustness_bayes$hdi_mode,
                              hdi_l = current()$robustness_bayes$hdi_l,
                              hdi_u = current()$robustness_bayes$hdi_u,
                              include_nhst = FALSE)
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
# mod_robustness_ui("robustness")
    
## To be copied in the server
# mod_robustness_server("robustness")
 
