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
      h3("Current summary results", class = "summary-title"),
      textOutput(NS(id, "current_general")),
      h3("Summary results at the latest checkpoint", class = "summary-title"),
      textOutput(NS(id, "checkpoint_general")),
      class = "summary-panel",
      id = "#summary"),
    wellPanel(
      h2("Inferences", class = "summary-title"),
      h3("Current inferences", class = "summary-title"),
      textOutput(NS(id, "current_inference")),
      h3("Inferences at the latest checkpoint", class = "summary-title"),
      textOutput(NS(id, "checkpoint_inference")),
      class = "summary-panel",
      id = "#inference"),
    mod_footer_ui(NS(id, "footer"))
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(id, current, at_checkpoint, push_time, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Calculate sample current descriptive ---------------------------
    output$current_general <- renderText({
      text_helper_current_general(current()$descriptive, current()$robustness_bayes)
    })
    
    # Calculate sample checkpoint descriptive ---------------------------
    output$checkpoint_general <- renderText({
      if (is.null(at_checkpoint())) {
        glue::glue("The number of valid erotic trials do not reach the first preset \\
                   checkpoint at {n_first}. Still {n_needed} trials needed to reach the checkpoint. \\
                   This result will only be presented after the first checkpoint.",
                   n_first = tppr::analysis_params$when_to_check[1],
                   n_needed = n_first - current()$descriptive$total_n)
        } else {
          text_helper_checkpoint_general(at_checkpoint()$descriptive, at_checkpoint()$robustness_bayes)
          }
    })
    
    # Inference current ---------------------------
    output$current_inference <- renderText({
      last_bayes <- 
        current()$cumulative_bayes %>% 
        dplyr::slice_max(total_n)
      
      text_helper_current_confirmatory(last_bayes$BF_replication, last_bayes$BF_uniform, last_bayes$BF_BUJ)
      current_inference_bayes <- inference_confirmatory_bf(c(last_bayes$BF_replication, last_bayes$BF_uniform, last_bayes$BF_BUJ))
      text_helper_current_robustness(current_inference_bayes, current()$robustness_bayes$inference_robustness_bayes)
    })
    
    # Inference checkpoint ---------------------------
    output$checkpoint_inference <- renderText({
      if (is.null(at_checkpoint())) {
        glue::glue("The number of valid erotic trials do not reach the first preset \\
                   checkpoint at {n_first}. Still {n_needed} trials needed to reach the checkpoint. \\
                   This result will only be presented after the first checkpoint.",
                   n_first = tppr::analysis_params$when_to_check[1],
                   n_needed = n_first - current()$descriptive$total_n)
      } else {
        text_helper_checkpoint_confirmatory(at_checkpoint()$confiramtory_inference)
      }
    })
    
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
 
