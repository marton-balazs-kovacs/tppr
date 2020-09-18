#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage(
      title = 
        div(
          img(src="www/favicon.png", height = "100px"),
          h2(id = "title", "Transparent Psi Project")),
      tabPanel("Welcome",
               mod_welcome_ui("welcome")),
      # tabPanel("Summray results",
      #          mod_summary_ui("summary")),
      tabPanel("Main confirmatory results",
               mod_main_confirmatory_ui("main_confirmatory"))
      # tabPanel("Robustness results",
      #          mod_robustness_server("robustness")),
      # tabPanel("Exploratory results",
      #          mod_exploratory_ui("exploratory")),
      # tabPanel("Preprint",
      #          mod_article_ui("article"))
    )
  )
}
#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tppr'
    )
  )
}
