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
    br(),
    textOutput(NS(id, "warning_text")),
    hr(),
    textOutput(NS(id, "refresh")),
    br(),
    textOutput(NS(id, "refresh_rate"))
  )
}
    
#' footer Server Function
#'
#' @noRd 
mod_footer_server <- function(id, push_time, refresh_time, current, at_checkpoint){
  moduleServer(id, function(input, output, session) {

    output$stopping_text <- renderText({
      if (is.na(current()$checkpoint$current_checkpoint)) {
       text_helper_stop(checkpoint_next = 1)
      } else {
        if (at_checkpoint()$confiramtory_inference %in% c("Ongoing", "Inconclusive")) {
          text_helper_stop(checkpoint_next = current()$checkpoint$next_checkpoint)
        } else if (at_checkpoint()$confiramtory_inference %in% c("M1", "M0")) {
          "Data collection stopped because one of the stopping rules has been triggered."
        }
      }
      })
    
    output$warning_text <- renderText({
      if (is.na(current()$checkpoint$current_checkpoint)) {
        text_helper_warning(checkpoint_next = 1)
      } else {
        if (at_checkpoint()$confiramtory_inference %in% c("Ongoing", "Inconclusive")) {
          text_helper_warning(checkpoint_next = current()$checkpoint$next_checkpoint)
        } else if (at_checkpoint()$confiramtory_inference %in% c("M1", "M0")) {
          "The datacollection stopped, therefore the results at the lastly \\
    analysed checkpoint show the final results of the project." 
        }
      }
      })
    
    output$refresh <- renderText({
      glue::glue("The data on this page was last collected at: {push_time}",
                 push_time = as.POSIXct(as.numeric(push_time()), origin = '1970-01-01', tz = 'GMT'))
      })
  
    output$refresh_rate <- renderText({
      glue::glue("The app searches for new data in every {refresh_time / 1000} seconds",
                 refresh_time = refresh_time)
      })
    })
  }
    
## To be copied in the UI
# mod_footer_ui("footer")
    
## To be copied in the server
# mod_footer_server("footer")
 
