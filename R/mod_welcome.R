#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id){

  tagList(
    includeMarkdown(app_sys("app/www/intro.rmd")),
    br(),
    mod_footer_ui("footer")
  )
}
    
#' welcome Server Function
#'
#' @noRd 
mod_welcome_server <- function(input, output, session){
  moduleServer(id, function(input, output, session) {
    mod_footer_server("footer")
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome")
    
## To be copied in the server
# mod_welcome_server("welcome")
 
