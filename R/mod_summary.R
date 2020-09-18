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
mod_summary_server <- function(id, current, at_checkpoint, push_time, refresh_time){
  moduleServer(id, function(input, output, session) {
    # Calculate sample current descriptive ---------------------------
    output$current_general <- renderText({
      text_helper_current_general(current$descriptive, current$robustness_bayes)
    })
    
    # Calculate sample checkpoint descriptive ---------------------------
    output$checkpoint_general <- renderText({
      if (is.null(at_checkpoint)) {
        glue::glue("The number of valid erotic trials do not reach the first preset \\
                   checkpoint at {}. Still {} trials needed to reach the checkpoint. \\
                   This result will only be presented after the first checkpoint.")
      } else {
        text_helper_checkpoint_general(at_checkpoint$descriptive, at_checkpoint$robustness_bayes)
      }
    })
    
    # Inference current ---------------------------
    output$current_inference <- renderText({
      last_bayes <- 
        current$cumulative_bayes %>% 
        dplyr::slice_max(total_n)
      
      text_helper_current_confirmatory(last_bayes$BF_replication, last_bayes$BF_uniform, last_bayes$BF_BUJ)
      current_inference_bayes <- inference_confirmatory_bf(c(last_bayes$BF_replication, last_bayes$BF_uniform, last_bayes$BF_BUJ))
      text_helper_current_robustness(current_inference_bayes, current$robustness_bayes$inference_robustness_bayes)
    })
    
    # Inference checkpoint ---------------------------
    output$checkpoint_inference <- renderText({
      if (is.null(at_checkpoint)) {
        glue::glue("The number of valid erotic trials do not reach the first preset \\
                   checkpoint at {}. Still {} trials needed to reach the checkpoint. \\
                   This result will only be presented after the first checkpoint.")
      } else {
        # checkpoint_inference_confirmatory <- inference_confirmatory_combined(at_checkpoint$confirmatory_mixed$n_iteration,
        #                                                                      at_checkpoint$confirmatory_mixed$mixed_nhst_inference,
        #                                                                      at_checkpoint$confirmatory_bayes$bf_inference)
        text_helper_checkpoint_confirmatory(at_checkpoint$confiramtory_inference)
        # text_helper_checkpoint_robustness()
      }
    })
    
    # Add warning in the footer ---------------------------
    mod_footer_server("footer", push_time = push_time)
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary")
    
## To be copied in the server
# mod_summary_server("summary")
 
