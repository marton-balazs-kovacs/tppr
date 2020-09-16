#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Shiny app refresh rime
  refresh_time <- 10000
  
  # Opening page
  mod_welcome_server("welcome")
  
  # Read the raw data
  read_ouput <- mod_read_data_server("read_data")
  
  # Summary results
  mod_summary_server("summary")
  
  # Main confirmatory results
  mod_main_confirmatory_server("main_confirmatory")
  
  # Robustness results
  mod_robustness_server("robustness")
  
  # Exploratory results
  mod_exploratory_server("exploratory")
  
  # Show preprint
  mod_article_server("article")
}
