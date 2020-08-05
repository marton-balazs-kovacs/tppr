#' Plotting the results of the exploratory analysis
#' 
#' The function visualize the results of the exploratory
#' analysis \code{\link{analysis_exploratory}}.
#' 
#' @family plotting functions
#' 
#' @param exploratory_results tibble, output of the \code{\link{analysis_exploratory}} function
#' 
#' @return The function returns a ggplot object.
#' @export
plot_exploratory <- function(exploratory_results) {
  # Process input argument ---------------------------
  success_rates_theoretical_prop <- exploratory_results$success_rates_theoretical_prop
  success_rates_empirical_prop <- exploratory_results$success_rates_empirical_prop
  possible_success_rates <- exploratory_results$possible_success_rates
  
  # Prepare plot data ---------------------------
  fig_3_plot_data <- 
    tibble::tibble(
      proportion = c(success_rates_theoretical_prop, success_rates_empirical_prop),
      group = c(
        rep("Expected if M0 is true", length(success_rates_theoretical_prop)),
        rep("Observed", length(success_rates_empirical_prop))
        ),
      success = rep(as.character(possible_success_rates), 2)
      )
  
  # Create plot ---------------------------
  figure_3 <- 
    fig_3_plot_data %>% 
    ggplot2::ggplot() + 
    ggplot2::aes(x = success, y = proportion, group = group) +
    ggplot2::geom_bar(ggplot2::aes(fill = group), alpha = 0.5, stat = "identity", position = "identity")+
    ggplot2::scale_fill_manual(values = c("darkgrey", "black")) +
    ggplot2::labs(x = "Successful guess rate", y = "Proportion") +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom",
          axis.line = ggplot2::element_line(colour = "black", size = 1.2),
          axis.text.x = ggplot2::element_text(angle = 90, color = "black", face = "bold", size = 12, margin = ggplot2::margin(1,0,0,0,"mm"), vjust = 0.5),
          axis.text.y = ggplot2::element_text(face = "bold", color = "black", size = 12),
          axis.title = ggplot2::element_text(size = 16))
  
  # Return outputs ---------------------------
  return(figure_3)
}
