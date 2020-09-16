#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){

  tagList(
    wellPanel(
      h2("Summary"),
      h3("Current summary results"),
      textOutput(NS(id, "current_general")),
      h3("Summary results at the latest checkpoint"),
      textOutput(NS(id, "checkpoint_general"))),
    wellPanel(
      h2("Inferences"),
      h3("Current inferences"),
      textOutput(NS(id, "current_inference")),
      h3("Inferences at the latest checkpoint"),
      textOutput(NS(id, "checkpoint_inference"))),
    mod_footer_ui(NS(id, "footer"))
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Calculate sample current descriptive ---------------------------
    output$current_general <- renderText({
      text_helper_current_general()
    })
    
    # Calculate sample checkpoint descriptive ---------------------------
    output$checkpoint_general <- renderText({
      if (is.null(checkpoint_toggle)) {
        glue::glue("The number of valid erotic trials do not reach the first preset \\
                   checkpoint at {}. Still {} trials needed to reach the checkpoint. \\
                   This result will only be presented after the first checkpoint.")
      } else {
        text_helper_checkpoint_general()
      }
    })
    
    # Inference current ---------------------------
    output$current_inference <- renderText({
      text_helper_current_confirmatory()
      text_helper_robustness()
    })
    
    # Inference checkpoint ---------------------------
    output$checkpoint_inference <- renderText({
      if (is.null(checkpoint_toggle)) {
        glue::glue("The number of valid erotic trials do not reach the first preset \\
                   checkpoint at {}. Still {} trials needed to reach the checkpoint. \\
                   This result will only be presented after the first checkpoint.")
      } else {
        text_helper_checkpoint_confirmatory()
        text_helper_robustness()
      }
    })
    
    # Add warning in the footer ---------------------------
    mod_footer_server("footer")
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary")
    
## To be copied in the server
# mod_summary_server("summary")
 
