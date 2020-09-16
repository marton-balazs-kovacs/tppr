#' footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_footer_ui <- function(id){

  tagList(
    textOutput(NS(id, "stopping_text")),
    textOutput(NS(id, "warning_text"), style="color:red"),
    textOutput(NS(id, "refresh")),
    textOutput(NS(id, "refresh_rate")),
    p("To support any model, all three Bayes Factor values need to pass the threshold")
  )
}
    
#' footer Server Function
#'
#' @noRd 
mod_footer_server <- function(id){
  moduleServer(id, function(input, output, session) {
    
    output$stopping_text <- renderText({
      text_helper_stop()
      })
    
    output$warning_text <- renderText({

      })
    
  output$refresh <- renderText({
    invalidateLater(refresh_time)
    paste("The data on this page was last refreshed at:  ", Sys.time(), "  (time zone:  ", Sys.timezone(), " ).", sep = "")
  })
  
  output$refresh_rate <- renderText({
    paste("the data is refreshed every ", refresh_time/1000, " seconds", sep = "")
  })
})
}
    
## To be copied in the UI
# mod_footer_ui("footer")
    
## To be copied in the server
# mod_footer_server("footer")
 
