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
          div(id = "title-text", "Transparent Psi Project"),
      tabPanel("Welcome",
               mod_welcome_ui("welcome")),
      tabPanel("Summary results",
               mod_summary_ui("summary")),
      tabPanel("Main confirmatory results",
               mod_main_confirmatory_ui("main_confirmatory")),
      tabPanel("Robustness results",
               mod_robustness_ui("robustness")),
      tabPanel("Exploratory results",
               mod_exploratory_ui("exploratory")),
      tabPanel("Preprint",
               mod_article_ui("article"))
    ),
    
    # Enabling waiter JS functions
    waiter::use_waiter(),
    waiter::use_waitress(color = "#A96A7C"),
    
    # Add waiter load on start
    waiter::waiter_show_on_load(html =  tagList(
      tags$img(src = "www/favicon.png", height = "250px"),
      h4("The app is loading...", style = "color: #6d5c7c;")), color = "#FBEEEF")
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
    golem::favicon(ext = "png"),
    # Add custom css stylesheet
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tppr'
    )
  )
}
