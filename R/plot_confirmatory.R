#' Plotting the results of the primary confirmatory analysis
#' 
#' The function visualize the results of the confirmatory
#' Bayes factor analysis \code{\link{cumulative_bayes_factor}}.
#' The Bayes factors calculated at each new experimental trial
#' are displayed on this figure.
#' 
#' @family plotting functions
#' 
#' @param cumulative_results dataframe, output of the \code{\link{cumulative_bayes_factor}} function
#' 
#' @return The function returns a ggplot object.
#' @export
plot_confirmatory <- function(cumulative_results) {
  # Prepare plot data ---------------------------
  # Get checkpoint information
  highest_checkpoint <- tell_checkpoint(cumulative_results)$current_checkpoint
  check_range <- tppr::analysis_params$when_to_check[1:highest_checkpoint]
  
  figure_1_data <-
    cumulative_results %>% 
    dplyr::select(-success) %>% 
    tidyr::gather(key = "bf_type", value = "bf_value", -total_n)
  
  # Bayes factors at each predetermined sequential stopping point
  ## This df contains the Bayes factor analysis results at each checking point until the last checking point
  bf_table <- 
    figure_1_data %>% 
    dplyr::filter(total_n %in% check_range) %>% 
    dplyr::rename(checkpoint = total_n)

  # Final Bayes factor results to set the scale of the plot
  ## This df contains the Bayes factor analysis results at the last checkpoint
  bf_table_last <- 
    bf_table %>% 
    dplyr::slice_max(checkpoint)
  
  # Setting limits and breaks and label positions
  fig_1_y_axis_breaks <- 
    if (min(bf_table$bf_value) < 0.01 | max(bf_table$bf_value) > 100) {
      c(0.002, 0.01, tppr::analysis_params$inference_threshold_bf_low, 0.1, 0.33, 0, 3, 10, tppr::analysis_params$inference_threshold_bf_high, 100, 500)
    } else {
      c(0.01, tppr::analysis_params$inference_threshold_bf_low, 0.1, 0.33, 0, 3, 10, tppr::analysis_params$inference_threshold_bf_high, 100)
    }
  
  fig_1_y_axis_limits <- 
    if (min(bf_table$bf_value) < 0.01 | max(bf_table$bf_value) > 100) {
      c(0.001, 1000)
    } else {
      c(0.005, 200)
    }
  
  fig_1_y_axis_text_position <- 
    if (min(bf_table$bf_value) < 0.01 | max(bf_table$bf_value) > 100) {
      c(200, 1, 0.005)
    } else {
      c(100, 1, 0.01)
    }
  
  fig_1_x_axis_breaks <- 
    c(0,
      plyr::round_any(bf_table_last$checkpoint * 1 / 3, 1000),
      plyr::round_any(bf_table_last$checkpoint * 2 / 3, 1000),
      bf_table_last$checkpoint)
  
  figure_1_data <-
    figure_1_data %>% 
    dplyr::group_by(bf_type) %>%
    dplyr::mutate(bf_value = smoothie(total_n, bf_value))
  
  # Create plot ---------------------------
  figure_1 <- 
    figure_1_data %>% 
    ggplot2::ggplot()+
    ggplot2::aes(y = bf_value, x = total_n, group = bf_type) +
    ggplot2::geom_line(ggplot2::aes(linetype = bf_type), size = 1.2) +
    ggplot2::scale_linetype_manual(name = "Prior", labels = c("BUJ", "Replication", "Uniform"), values = c("solid", "dashed", "twodash")) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = c(tppr::analysis_params$inference_threshold_bf_high), ymax = c(Inf), alpha = 0.4, fill = c("grey60")) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = c(tppr::analysis_params$inference_threshold_bf_low), ymax = c(tppr::analysis_params$inference_threshold_bf_high), alpha = 0.2, fill = c("grey80")) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = c(0), ymax = c(tppr::analysis_params$inference_threshold_bf_low), alpha = 0.4, fill = c("grey60")) +
    ggplot2::scale_y_log10(limits = fig_1_y_axis_limits,
                           breaks = fig_1_y_axis_breaks) +
    ggplot2::scale_x_continuous(breaks = fig_1_x_axis_breaks) +
    ggplot2::geom_hline(yintercept = c(tppr::analysis_params$inference_threshold_bf_low,
                                       tppr::analysis_params$inference_threshold_bf_high),
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = bf_table_last$checkpoint,
                        linetype = "dotted") +
    ggplot2::geom_point(data = bf_table_last,
                        ggplot2::aes(y = bf_value, x = checkpoint, group = bf_type),
                        size = 3.5,
                        shape = 21,
                        fill = "white") +
    ggplot2::annotate("text",
                      x = -500,
                      y = c(fig_1_y_axis_text_position),
                      label = c("Supports M0", "Inconclusive", "Supports M1"),
                      angle = 270) +
    ggplot2::labs(x = "Number of experimental trials",
                  y = "Bayes factor") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  # Return outputs ---------------------------
  # ggplot returns this warning:
  # "Transformation introduced infinite values in continuous y-axis"
  # but this is not important so we suppress it
  return(suppressWarnings(figure_1))
}
