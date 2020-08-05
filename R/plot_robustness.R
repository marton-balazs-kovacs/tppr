#' Plotting the results of the robustness analysis
#' 
#' The function visualize the results of the robustness
#' analysis \code{\link{analysis_robustness}}.
#' 
#' @family plotting functions
#' 
#' @param robustness_results tibble, output of the \code{\link{analysis_robustness}} function
#' 
#' @return The function returns a ggplot object.
#' @export
plot_robustness <- function(robustness_results) {
  # Process input argument ---------------------------
  # Results of the robustness analysis
  posterior_density <- robustness_results$robustness_bayes_res$posterior_density
  hdi_mode <- robustness_results$robustness_bayes_res$hdi_mode
  hdi_l <- robustness_results$robustness_bayes_res$hdi_l
  hdi_u <- robustness_results$robustness_bayes_res$hdi_u
  # Results of the confirmatory analysis
  mixed_ci_width <- robustness_results$confirmatory_mixed_ci_width
  mixed_ci_l <- robustness_results$confirmatory_mixed_ci_l
  mixed_ci_u <- robustness_results$confirmatory_mixed_ci_u
  
  # Figure 2 displays the Confidence interval computed based on the 
  # final mixed model primary analysis and the posterior distribution of the 
  # parameter based on the Bayesian Parameter Estimation
  # Prepare plot data ---------------------------
  fig_2_sample <- 
    tibble::tibble(
      value = sample(x = analysis_params$scale, size = 1000000, replace = TRUE, prob = posterior_density))
  
  fig_2_sample_within_hdi <-
    tibble::tibble(
      value = dplyr::filter(fig_2_sample, value > (hdi_l - 0.0001) & value < (hdi_u + 0.0001)))
  
  # Create plot base
  figure_2_base <- 
    fig_2_sample %>% 
    ggplot2::ggplot() +
    ggplot2::aes(x = value) +
    ggplot2::geom_density(ggplot2::aes(y = ..scaled..), color = "white")
  
  # Get data from plot base
  fig_2_base_data <- ggplot2::ggplot_build(figure_2_base)$data[[1]]
  
  fig_2_segment_data <- 
    tibble::tibble(
      x1 = c(hdi_l, hdi_u),
      y1 = c(0, 0),
      xend = c(hdi_l, hdi_u),
      yend = c(approx(x = fig_2_base_data$x, y = fig_2_base_data$y, xout = hdi_l)$y,
               approx(x = fig_2_base_data$x, y = fig_2_base_data$y, xout = hdi_u)$y), 
      lty = "solid")
  
  fig_2_segment_data_prop_ci <- 
    tibble::tibble(
      x1 = c(mixed_ci_l, mixed_ci_l, mixed_ci_u),
      y1 = c(-0.04, -0.02, -0.02),
      xend = c(mixed_ci_u, mixed_ci_l, mixed_ci_u),
      yend = c(-0.04, -0.06, -0.06),
      lty = "solid")
  
  fig_2_segment_data_rope_equlim <- 
    tibble::tibble(
      x1 = c(analysis_params$rope, analysis_params$p_equiv_test),
      y1 = c(0, 0),
      xend = c(analysis_params$rope, analysis_params$p_equiv_test),
      yend = c(Inf, -Inf))
  
  # Create plot ---------------------------
  figure_2 <- 
    figure_2_base + 
    ggplot2::geom_area(data = dplyr::filter(fig_2_base_data, x >= hdi_l & x <= hdi_u),
                       ggplot2::aes(x = x, y = y),
              fill = "light gray") +
    ggplot2::geom_line(data = fig_2_base_data,
                       ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_segment(data = fig_2_segment_data,
                          ggplot2::aes(x = x1, xend = xend, y = y1, yend = yend))+
    ggplot2::geom_segment(data = fig_2_segment_data_prop_ci,
                          ggplot2::aes(x = x1, xend = xend, y = y1, yend = yend))+
    ggplot2::geom_segment(data = fig_2_segment_data_rope_equlim,
                          ggplot2::aes(x = x1, xend = xend, y = y1, yend = yend),
                 linetype = c("dashed", "dotted"))+
    ggplot2::annotate("text",
             x = hdi_l,
             y = 0.1,
             label = paste0("90% HDI: [", round(hdi_u, 3), ", ", round(hdi_mode, 3), "]"),
             size = 4,
             fontface = 2) +
    ggplot2::annotate("text",
             x = mean(c(mixed_ci_l, mixed_ci_u)),
             y = -0.08,
             label = paste0(mixed_ci_width," CI: [", round(mixed_ci_l, 3), ", ", round(mixed_ci_u, 3), "]"),
             size = 4,
             fontface = 2) +
    ggplot2::geom_vline(xintercept = analysis_params$m0_prob, size = 1.3)+
    ggplot2::labs(
      x = "successful guess probability",
      y = "scaled density")+
    ggplot2::ylim(-0.1, 1)+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          #        legend.position = "bottom",
          axis.line = ggplot2::element_line(colour = "black"),
          axis.text.x = ggplot2::element_text(color = "black", face = "bold", size = 12, margin = ggplot2::margin(1, 0, 0, 0, "mm"), vjust = 0.5),
          axis.text.y = ggplot2::element_text(face = "bold", color = "black", size = 12),
          axis.title = ggplot2::element_text(size = 16))
  
  # Return outputs ---------------------------
  return(figure_2)
}
