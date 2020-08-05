#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Shiny app refresh rime
  refresh_time <- 10000
  
  # Include article as iframe
  mod_article_server("article")
}
