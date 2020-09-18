#' read_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_data_ui <- function(id){

  tagList(

  )
}
    
#' read_data Server Function
#'
#' @noRd 
mod_read_data_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    read_url_safe <- purrr::safely(read_url)
    
    push_time <- reactive({
      invalidateLater(refresh_time)
      read_url(result_url$time_log)$webhook_push_time
    })
    
    current <- eventReactive(push_time, {
      list(
        checkpoint = read_url_safe(result_url$checkpoint),
        cumulative_bayes = read_url(result_url$current_cumulative_bayes),
        robustness_bayes = read_url(result_url$current_robustness_bayes),
        exploratory = read_url(result_url$current_exploratory),
        descriptive = read_url(result_url$current_descriptive)
      )
    })
    
    at_checkpoint <- eventReactive(push_time, {
      if (is.null(current$checkpoint$error)) {
        tibble::lst(
          confirmatory_mixed = read_url(result_url$checkpoint_confirmatory_mixed),
          confirmatory_bayes = read_url(result_url$checkpoint_confirmatory_bayes),
          robustness_bayes = read_url(result_url$checkpoint_robustness_bayes),
          robustness_nhst = read_url(result_url$checkpoint_robustness_nhst),
          exploratory = read_url(result_url$checkpoint_exploratory),
          descriptive = read_url(result_url$checkpoint_descriptive),
          confiramtory_inference = inference_confirmatory_combined(confirmatory_mixed$n_iteration,
                                                                   confirmatory_mixed$mixed_nhst_inference,
                                                                   confirmatory_bayes$bf_inference)
        )
      } else {
        NULL
      }
    })

    return(
      list(
        current = current,
        at_checkpoint = at_checkpoint,
        push_time = push_time
      )
    )
  })
}
    
## To be copied in the UI
# mod_read_data_ui("read_data")
    
## To be copied in the server
# mod_read_data_server("read_data")
