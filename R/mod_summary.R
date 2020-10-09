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
      h2("Summary", class = "summary-title"),
      textOutput(NS(id, "current_summary")),
      hr(),
      htmlOutput(NS(id, "checkpoint_summary")),
      class = "summary-panel",
      id = "#summary"),
    mod_footer_ui(NS(id, "footer"))
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(id, current, at_checkpoint, push_time, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Calculate sample current descriptive ---------------------------
    output$current_summary <- renderText({
      text_helper_current_general(current()$descriptive, current()$robustness_bayes)
    })
    
    outputOptions(output, "current_summary", suspendWhenHidden = FALSE)
    # Add checkpoint specific summary ---------------------------
    # This will be shown only if at least one checkpoint is passed
    output$checkpoint_summary <- renderUI({
      if (!is.null(at_checkpoint())) {
        general <- text_helper_checkpoint_general(at_checkpoint()$descriptive, at_checkpoint()$robustness_bayes)
        inference <- text_helper_checkpoint_confirmatory(at_checkpoint()$confiramtory_inference)
        HTML(paste(general, inference, sep = '<br/>'))
      }
    })
    
    outputOptions(output, "checkpoint_summary", suspendWhenHidden = FALSE)
    # Add warning in the footer ---------------------------
    mod_footer_server("footer",
                      push_time = push_time,
                      refresh_time = refresh_time,
                      current = current,
                      at_checkpoint = at_checkpoint)
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary")
    
## To be copied in the server
# mod_summary_server("summary")
 
