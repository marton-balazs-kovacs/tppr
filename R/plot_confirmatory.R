#' Plotting the results of the primary confirmatory analysis
#' 
#' The function visualize the results of the confirmatory
#' analysis \code{\link{analyis_confirmatory}}. The Bayes factors
#' calculated at each new experimental trial are displayed on this figure.
#' 
#' @family plotting functions
#' 
#' @param confirmatory_result tibble, output of the \code{\link{analyis_confirmatory}} function
#' 
#' @return The function returns a ggplot object.
#' @export
plot_confirmatory <- function(confirmatory_results) {
  # Prepare plot data ---------------------------
  # Bayes factors at each predetermined sequential stopping point
  ## This df contains the Bayes factor analysis results at each checking point until the last checking point
  bf_table <- 
    confirmatory_results %>% 
    dplyr::select(checkpoint, bf_res) %>% 
    tidyr::unnest_longer(bf_res) %>% 
    dplyr::rename(bf_value = bf_res,
                  bf_type = bf_res_id) %>% 
    dplyr::mutate(bf_type = dplyr::case_when(bf_type == "bf_replication" ~ "BF_replication",
                                             bf_type == "bf_uniform" ~ "BF_uniform",
                                             bf_type == "bf_buj" ~ "BF_BUJ"))
  
  # Final Bayes factor results to set the scale of the plot
  ## This df contains the Bayes factor analysis results at the last checkpoint
  bf_table_last <- 
    bf_table %>% 
    dplyr::slice_max(checkpoint) %>% 
    tidyr::spread(key = "bf_type", value = "bf_value")
  
  # Setting limits and breaks and label positions
  fig_1_y_axis_breaks <- 
    if (min(bf_table$bf_value) < 0.01 | max(bf_table$bf_value) > 100) {
      c(0.002, 0.01, analysis_params$inference_threshold_bf_low, 0.1, 0.33, 0, 3, 10, analysis_params$inference_threshold_bf_high, 100, 500)
    } else {
      c(0.01, analysis_params$inference_threshold_bf_low, 0.1, 0.33, 0, 3, 10, analysis_params$inference_threshold_bf_high, 100)
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
  
  # Calculating cumulative successes
  ## This df contains data until the last checkpoint
  cumulative_successes_trial_n_table <- 
    confirmatory_results %>% 
    dplyr::slice_max(n_iteration) %>% 
    dplyr::select(split_data) %>% 
    tidyr::unnest(split_data) %>% 
    dplyr::transmute(success = cumsum(sides_match),
                     total_n = 1:nrow(.))
  
  # Calculating Bayes factors for each new experimental trial with all three priors
  # Calculating these takes about 3 minutes on an i7-6600 2.6GHz GPU, depending on how many data points are there
  bf_replication_cumulative <- 
    purrr::map2(cumulative_successes_trial_n_table$success, cumulative_successes_trial_n_table$total_n,
                ~ BF01_beta(y = .x, N = .y, y_prior = analysis_params$y_prior, N_prior = analysis_params$n_prior, interval = c(0.5, 1), null_prob = analysis_params$m0_prob))
  
  bf_uniform_cumulative <- 
    purrr::map2(cumulative_successes_trial_n_table$success, cumulative_successes_trial_n_table$total_n,
                ~ BF01_beta(y = .x, N = .y, y_prior = 0, N_prior = 0, interval = c(0.5, 1), null_prob = analysis_params$m0_prob)) 
  
  bf_buj_cumulative <- 
    purrr::map2(cumulative_successes_trial_n_table$success, cumulative_successes_trial_n_table$total_n,
                ~ BF01_beta(y = .x, N = .y, y_prior = 6, N_prior = 12, interval = c(0.5, 1), null_prob = analysis_params$m0_prob))
  
  # Fitting smoothing spline to get a smooth curve
  fit_replication <- smooth.spline(cumulative_successes_trial_n_table$total_n, bf_replication_cumulative, df = 80)
  fit_uniform <- smooth.spline(cumulative_successes_trial_n_table$total_n, bf_uniform_cumulative, df = 80)
  fit_buj <- smooth.spline(cumulative_successes_trial_n_table$total_n, bf_buj_cumulative, df = 80)
  
  bf_replication_cumulative_spline <- predict(fit_replication)$y
  bf_uniform_cumulative_spline <- predict(fit_uniform)$y
  bf_buj_cumulative_spline <- predict(fit_buj)$y
  
  # Put all data needed for plotting in a data frame
  fig_1_plot_data_full <- 
    tibble::tibble(
      bf_value = c(bf_replication_cumulative_spline, bf_uniform_cumulative_spline, bf_buj_cumulative_spline),
      total_n = rep(cumulative_successes_trial_n_table$total_n, 3),
      bf_type = rep(c("BF_replication", "BF_uniform", "BF_BUJ"), each = length(cumulative_successes_trial_n_table$total_n))
      )
  
  # Create plot ---------------------------
  figure_1 <- 
    fig_1_plot_data_full %>% 
    ggplot2::ggplot()+
    ggplot2::aes(y = bf_value, x = total_n, group = bf_type) +
    ggplot2::geom_line(ggplot2::aes(linetype = bf_type), size = 1.2)+
    ggplot2::scale_linetype_manual(name = "Prior", labels = c("BUJ", "Replication", "Uniform"), values = c("solid", "dashed", "twodash")) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = c(analysis_params$inference_threshold_bf_high), ymax = c(Inf), alpha = 0.4, fill = c("grey60")) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = c(analysis_params$inference_threshold_bf_low), ymax = c(analysis_params$inference_threshold_bf_high), alpha = 0.2, fill = c("grey80")) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = c(0), ymax = c(analysis_params$inference_threshold_bf_low), alpha = 0.4, fill = c("grey60")) +
    ggplot2::scale_y_log10(limits = fig_1_y_axis_limits,
                           breaks = fig_1_y_axis_breaks) +
    ggplot2::scale_x_continuous(breaks = fig_1_x_axis_breaks) +
    ggplot2::geom_hline(yintercept = c(analysis_params$inference_threshold_bf_low,
                                       analysis_params$inference_threshold_bf_high),
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = bf_table_last$checkpoint,
                        linetype = "dotted") +
    ggplot2::geom_point(data = bf_table,
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
