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
    read_safe <- purrr::safely(readRDS)
    
    push_time <- reactive({
      readRDS(
        url(
          "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/master/tppr_time_log.rds", 
          method="libcurl")
        )[[3]]
    })
    

    return(
      raw_data,
      checkpoint
    )
  })
}
    
## To be copied in the UI
# mod_read_data_ui("read_data")
    
## To be copied in the server
# mod_read_data_server("read_data")
 
