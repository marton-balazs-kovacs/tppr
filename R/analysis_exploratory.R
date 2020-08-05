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
#' @param confirmatory_result tibble, output of the \code{\link{analyis_confirmatory}} function
#' 
#' @return The function returns a list of three, the possible success
#' rates, the the proportion of observed successful guess rate, and the
#' proportion of proportion of simulated successful guess rate.
#' @export
#' @examples 
#' \donttest{
#' # Running the primary confirmatory analysis
#' confirmatory_results <- analysis_confirmatory(raw_data = example_m0)
#' # Running the exploratory analysis
#' analysis_exploratory(confirmatory_results = confirmatory_results)
#' }
analysis_exploratory <- function(confirmatory_results) {
  # Process input argument ---------------------------
  # Extract the data used for the calculation at the last checkpoint
  processed_data <- 
    confirmatory_results %>% 
    dplyr::slice_max(n_iteration) %>% 
    dplyr::select(split_data) %>% 
    tidyr::unnest(split_data)
  
  # Comparison of expected and observed distributions ---------------------------
  # Calculate proportion of successful guesses for each participant in the observed data
  # We only include participants where there is no missing erotic trials
  success_proportions_empirical_finishedalltrials <-
    processed_data %>% 
    dplyr::group_by(participant_ID) %>% 
    dplyr::filter(dplyr::n() == 18) %>% 
    dplyr::summarise(mean_match = mean(sides_match)) %>% 
    dplyr::pull(mean_match) %>% 
    round(., 2)
  
  # Samples 1,000,000 participants from a population with a 50% successful guess chance
  # homogeneous in the population we call this the theoretical sample, because it
  # approximates the theoretical null model.
  # TODO: decide whether this should be in the analysis_params or here (if the former the app is faster but cannot submit to cran)
  success_proportions_theoretical <- 
    round(
      rbinom(analysis_params$sim_null_participant_num,
             size = analysis_params$trial_size_per_participant,
             prob = analysis_params$m0_prob) / analysis_params$trial_size_per_participant,
      2)
  
  # Determine possible values of success rates ---------------------------
  num_trials <- 0:18
  possible_success_rates <- round(1 / (18 / num_trials), 2)
  
  # Determine the distribution in the theoretical sample ---------------------------
  success_rates_theoretical <-
    purrr::map_int(possible_success_rates, ~ sum(success_proportions_theoretical == .x))
  
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
    processed_data %>% 
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
    list(
      success_rates_theoretical_prop = success_rates_theoretical_prop,
      success_rates_empirical_prop = success_rates_empirical_prop,
      possible_success_rates = possible_success_rates
    )
  )
}
