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
    # Init reactive value
    latest_push <- reactiveVal(0)
    
    # Check for new data
    observe({
      invalidateLater(refresh_time)
      
      push_time <- read_url(result_url$time_log)$webhook_push_time
      
      if (push_time > latest_push()) {
        latest_push(push_time)
      }
    })
    
    current <- eventReactive(latest_push(), {
      list(
        checkpoint = read_url(result_url$checkpoint),
        cumulative_bayes = read_url(result_url$current_cumulative_bayes),
        robustness_bayes = read_url(result_url$current_robustness_bayes),
        exploratory = read_url(result_url$current_exploratory),
        descriptive = read_url(result_url$current_descriptive)
      )
    })
    
    at_checkpoint <- eventReactive(latest_push(), {
      if (!is.na(current()$checkpoint$current_checkpoint)) {
        tibble::lst(
          confirmatory_mixed = read_url(result_url$checkpoint_confirmatory_mixed),
          confirmatory_bayes = read_url(result_url$checkpoint_confirmatory_bayes),
          robustness_bayes = read_url(result_url$checkpoint_robustness_bayes),
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
        push_time = latest_push
      )
    )
  })
}
    
## To be copied in the UI
# mod_read_data_ui("read_data")
    
## To be copied in the server
# mod_read_data_server("read_data")
