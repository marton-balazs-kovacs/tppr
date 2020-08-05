#' exploratory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exploratory_ui <- function(id){

  tagList(
    h1("Histogram overlay of the expected and observed distribution of successful guess rate"),
    textOutput(NS(id, "warning"), style="color:red"),
    plotOutput(NS(id, "plot"))
  )
}
    
#' exploratory Server Function
#'
#' @noRd 
mod_exploratory_server <- function(id, refresh_time){
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      ggplot(values$histogram_plot_data, aes(y = proportion, x = success, group = group))+
        geom_bar(aes(fill = group), alpha = 0.5, stat = "identity", position = "identity")+
        scale_fill_manual(values = c("darkgrey", "black")) +
        xlab("Successful guess rate") +
        ylab("Proportion") +
        theme(panel.border = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              axis.line = element_line(colour = "black", size = 1.2),
              axis.text.x = element_text(angle = 90, color = "black", face = "bold", size = 12, margin = margin(1,0,0,0,"mm"), vjust = 0.5),
              axis.text.y = element_text(face = "bold", color = "black", size = 12),
              axis.title = element_text(size = 16))
    })
  })
}
    
## To be copied in the UI
# mod_exploratory_ui("exploratory")
    
## To be copied in the server
# mod_exploratory_server("exploratory")
 
