#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Shiny app refresh rime
  refresh_time <- 30000
  
  # Opening page
  mod_welcome_server("welcome")
  
  # # Read the raw data
  c(current, at_checkpoint, push_time) %<-% mod_read_data_server("read_data", refresh_time = refresh_time)

  # Summary results
  mod_summary_server("summary",
                     current = current, at_checkpoint = at_checkpoint,
                     push_time = push_time, refresh_time = refresh_time)

  # Main confirmatory results
  mod_main_confirmatory_server("main_confirmatory",
                               current = current, at_checkpoint = at_checkpoint,
                               push_time = push_time, refresh_time = refresh_time)

  # Robustness results
  mod_robustness_server("robustness",
                        current = current, at_checkpoint = at_checkpoint,
                        push_time = push_time, refresh_time = refresh_time)

  # Exploratory results
  mod_exploratory_server("exploratory",
                         current = current, at_checkpoint = at_checkpoint,
                         push_time = push_time, refresh_time = refresh_time)

  # Show preprint
  mod_article_server("article")
  
  # Hide on load waiter
  waiter::waiter_hide()
}
