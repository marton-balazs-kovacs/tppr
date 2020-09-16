#' Exploratory analysis
#' 
#' This function runs the exploratory analysis that
#' aims to explore the empirical distribution of successful
#' guess rate in the population, by contrasting it to a theoretical
#' distribution expected if the true successful guess chance is homogeneously
#' 50% in the population. This may provide information for future research on
#' potential irregularities in the distribution of guess rates.
#' 
#' @section Note: Exploratory analysis results will not affect
#'   the conclusions of our study.
#'   
#' @family analysis functions
#' 
#' @param df tibble
#' 
#' @return The function returns a list of three, the possible success
#' rates, the the proportion of observed successful guess rate, and the
#' proportion of proportion of simulated successful guess rate.
#' @export
#' @examples 
#' \donttest{
#' analysis_exploratory(df = example_m0)
#' }
analysis_exploratory <- function(df) {
  # Check whether the input df contains only erotic trials or not
  if (!all(df$reward_type == "erotic")) {
    df <- clean_data(raw_data = df)
  }
  
  # Comparison of expected and observed distributions ---------------------------
  # Calculate proportion of successful guesses for each participant in the observed data
  # We only include participants where there is no missing erotic trials
  success_proportions_empirical_finishedalltrials <-
    df %>% 
    dplyr::group_by(participant_ID) %>% 
    dplyr::filter(dplyr::n() == 18) %>% 
    dplyr::summarise(mean_match = mean(sides_match)) %>% 
    dplyr::pull(mean_match) %>% 
    round(., 2)
  
  # Determine possible values of success rates ---------------------------
  num_trials <- 0:18
  possible_success_rates <- round(1 / (18 / num_trials), 2)
  
  # Determine the distribution in the theoretical sample ---------------------------
  success_rates_theoretical <-
    purrr::map_int(possible_success_rates, ~ sum(tppr::analysis_params$success_proportions_theoretical == .x))
  
  success_rates_theoretical_prop <- matrix(success_rates_theoretical / sum(success_rates_theoretical))
  
  # Determine the distribution in the empirical sample  ---------------------------
  success_rates_empirical <- 
    purrr::map_int(possible_success_rates, ~ sum(success_proportions_empirical_finishedalltrials == .x))

  success_rates_empirical_prop <- matrix(success_rates_empirical / sum(success_rates_empirical))
  
  # Earth mover's distance
  emd <- emdist::emd2d(success_rates_theoretical_prop, success_rates_empirical_prop)
  
  # Difference between finished and not finished trials ---------------------------
  # Exploring the difference in success rate between those who did and those who did not finish all experimental trials
  success_proportions_empirical_didnotfinishalltrials <- 
    df %>% 
    dplyr::group_by(participant_ID) %>% 
    dplyr::filter(dplyr::n() == 18) %>% 
    dplyr::summarise(mean_match = mean(sides_match)) %>% 
    dplyr::pull(mean_match)

  mean_success_rate_finishedalltrials <- mean(success_proportions_empirical_finishedalltrials)
  success_rate_finishedalltrials_se <- sd(success_proportions_empirical_finishedalltrials) / sqrt(length(success_proportions_empirical_finishedalltrials))
  success_rate_finishedalltrials_ci_lb <- mean_success_rate_finishedalltrials - 1.96 * success_rate_finishedalltrials_se
  success_rate_finishedalltrials_ci_ub <- mean_success_rate_finishedalltrials + 1.96 * success_rate_finishedalltrials_se
  
  mean_success_rate_didnotfinishalltrials <- mean(success_proportions_empirical_didnotfinishalltrials)
  success_rate_didnotfinishalltrials_se <- sd(success_proportions_empirical_didnotfinishalltrials) / sqrt(length(success_proportions_empirical_didnotfinishalltrials))
  success_rate_didnotfinishalltrials_ci_lb <- mean_success_rate_didnotfinishalltrials - 1.96 * success_rate_didnotfinishalltrials_se
  success_rate_didnotfinishalltrials_ci_ub <- mean_success_rate_didnotfinishalltrials + 1.96 * success_rate_didnotfinishalltrials_se
  
  # Return outputs ---------------------------
  return(
    tibble::lst(
      success_rates_theoretical_prop,
      success_rates_empirical_prop,
      possible_success_rates
    )
  )
}
