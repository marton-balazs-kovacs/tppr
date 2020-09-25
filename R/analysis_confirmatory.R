#' Primary analysis: Mixed-effects logistic regression
#' 
#' This function calculates the mixed-effects logistic regression for
#' the primary analysis. A random intercept is placed on the participants
#' to predict the outcome of the guesses in erotic trials. Then the function
#' calculates the Wald confidence interval around the estimate of the odds
#' ratio of success and transforms it to logit probability.
#' 
#' @section Note: If the mixed-effects logistic regression is run by multiple
#'   times during the primary confirmatory analysis the alpha level is corrected
#'   by for each iteration.
#'   
#' @family confirmatory functions
#'   
#' @param processed_data a dataframe containing only erotic and non empty trials
#' @param n_iteration numeric, the nth time the analysis is conducted
#' 
#' @return A list of three containing the width, the lower and the upper border
#' of the confidence interval of 95%.
#' @export
#' @examples  
#' \donttest{
#' # Including only erotic trials
#' tpp_processed_data <- clean_data(raw_data = example_m0)
#' # Running the confirmatory analysis
#' mixed_effect_result <- confirmatory_mixed_effect(processed_data, n_iteration = 1)
#' # Checking the results
#' mixed_effect_result
#' }
confirmatory_mixed_effect <- function(processed_data, n_iteration = 1) {
  # Advance the counter to see how much adjustment needs to be made to the
  # NHST inference threshold due to multiple testing
  comparisons_mixed_nhst <- n_iteration * 2 # We multiply by 2 at each sequential stopping point because we do two tests at each stop point, one for M0 and one for M1
  
  # sides_match needs to be numeric
  # Build mixed logistic regression model and extract model coefficient and SE
  mod_mixed <- lme4::glmer(sides_match ~ 1 + (1|participant_ID), data = processed_data, family = "binomial")
  estimate_mixed <- summary(mod_mixed)$coefficients[1,1]
  se_mixed <- summary(mod_mixed)$coefficients[1,2]
  
  # Compute confidence interval on the probability scale, and save into results_table
  mixed_ci_width <- 1 - (tppr::analysis_params$inference_threshold_nhst / comparisons_mixed_nhst)
  wald_ci_mixed_logit <- c(estimate_mixed - se_mixed * qnorm(1 - ((tppr::analysis_params$inference_threshold_nhst/comparisons_mixed_nhst) / 2)),
                           estimate_mixed + se_mixed * qnorm(1 - ((tppr::analysis_params$inference_threshold_nhst/comparisons_mixed_nhst) / 2)))
  
  # Converting the results to the probability scale
  wald_ci_mixed <- logit2prob(wald_ci_mixed_logit)
  
  # Mixed effect inference ---------------------------
  mixed_nhst_inference <- inference_confirmatory_mixed_effect(mixed_ci_u = wald_ci_mixed[2], mixed_ci_l = wald_ci_mixed[1])
  
  # Return output ---------------------------
  return(
    list(
      mixed_ci_width = mixed_ci_width,
      mixed_ci_l = wald_ci_mixed[1],
      mixed_ci_u = wald_ci_mixed[2],
      n_iteration = n_iteration,
      mixed_nhst_inference = mixed_nhst_inference
    )
  )
}

#' Primary analysis: Bayes factors analysis
#'
#' The function computes the BF01 based on the successful guesses and the
#' total number of erotic trials. M0 is a point-value model denoting
#' that the chance of each successful guess is 50%. For M1 the function uses
#' three different beta priors on the probability of correct guesses:
#' knowledge-base prior, uniform prior, replication prior. In all cases we use
#' a one-tailed hypothesis testing (p > 0.5 and p < 1).
#' 
#' @family confirmatory functions
#' 
#' @param success numeric, total number of successful guesses on erotic trials
#' @param total_n numeric, total number of all erotic trials
#' 
#' @return The function returns a list of three numeric values with the rounded
#' Bayes factor calculated with each M1 beta prior.
#' @export
#' @examples
#' \donttest{
#' # Including only erotic trials
#' tpp_processed_data <- clean_data(raw_data = tppr::example_m0)
#' # Running the confirmatory analysis
#' confirmatory_bayes_factor(success = sum(tpp_processed_data$sides_match, na.rm = TRUE),
#'                           total_n = nrow(tpp_processed_data))
#' }
confirmatory_bayes_factor <- function(success, total_n) {
  # Replication prior  ---------------------------
  # The Bem 2011 experiment 1 results providing the prior information
  bf_replication <- BF01_beta(y = success, N = total_n, y_prior = tppr::analysis_params$y_prior, N_prior = tppr::analysis_params$n_prior, interval = c(0.5, 1), null_prob = tppr::analysis_params$m0_prob) #numbers higher than 1 support the null
  
  # Unniform prior  ---------------------------
  ## Using a non-informative flat prior distribution with alpha = 1 and beta = 1
  bf_uniform <- BF01_beta(y = success, N = total_n, y_prior = 0, N_prior = 0, interval = c(0.5, 1), null_prob = tppr::analysis_params$m0_prob) #numbers higher than 1 support the null
  
  # Konwledge-base prior ---------------------------
  ## The BUJ prior is calculated from Bem's paper where the prior distribution is defined as a
  ## normal distribution with a mean at 0 and 90th percentile is at medium effect size d = 0.5 
  ## (we assume that this is one-tailed). Source: Bem, D. J., Utts, J., & Johnson, W. O. (2011). 
  ## Must psychologists change the way they analyze their data? Journal of Personality and Social Psychology, 101(4), 716-719.
  ## We simulate this in this binomial framework with a one-tailed beta distribution with alpha = 7 and beta = 7.
  ## This distribution has 90% of its probability mass under p = 0.712, which we determined 
  ## to be equivalent to d = 0.5 medium effect size. We used the formula to convert d to log odds ratio logodds = d*pi/sqrt(3), 
  ## found here: Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). 
  ## Converting Among Effect Sizes. In Introduction to Meta-Analysis (pp. 45-49): John Wiley & Sons, Ltd.
  ## Then, log odds ratio was converted to probability using the formula: p = exp(x)/(1+exp(x))
  ## The final equation: exp(d*pi/sqrt(3))/(1+exp(d*pi/sqrt(3)))
  bf_buj <- BF01_beta(y = success, N = total_n, y_prior = 6, N_prior = 12, interval = c(0.5, 1), null_prob = tppr::analysis_params$m0_prob) #numbers higher than 1 support the null
  
  
  # Bayes factor inference ---------------------------
  # Replication Bayes factor 
  bf_inference <- inference_confirmatory_bf(c(bf_replication, bf_uniform, bf_buj))
  
  # Return output ---------------------------
  return(
    list(
      bf_replication = round(bf_replication, 3),
      bf_uniform = round(bf_uniform, 3),
      bf_buj = round(bf_buj, 3),
      bf_inference = bf_inference
    )
  )
}

#' Primary confirmatory analysis
#' 
#' This function conducts the primary confirmatory analysis
#' at the lastly passed checkpoint. In the analysis the mixed-effect logistic
#' regression (see \code{\link{confirmatory_mixed_effect}}) and three Bayes factor
#' analysis (see \code{\link{confirmatory_bayes_factor}}) are conducted.
#' Final inference is made based on the inferences of the four conducted
#' analyses.
#' 
#' @family analysis functions, confirmatory functions
#' 
#' @param df dataframe, the input dataframe
#' 
#' @return The output is a list containing the inputs, the results
#'   of the four analysis, and the inferences at the last checkpoint.
#'   The list contains:
#'   \itemize{
#'   \item highest_checkpoint, integer, the index of the investigated checkpoint
#'   \item success, integer, the number of successful guesses
#'   \item total_n, integer, the number of erotic trials
#'   \item n_iteration, integer, the number of iteration (see \code{\link{confirmatory_mixed_effect}})
#'   \item mixed_effect_res, df, the results of the mixed-effect logistic regression
#'     \item mixed_ci_width, numeric, width of the confidence interval
#'     \item mixed_ci_lb, numeric, lower limit of the confidence interval 5%
#'     \item mixed_ci_ub, numeric, upper limit of the confidence interval 95%
#'     \item mixed_nhst_inference, character, inference based on the result
#'   \item bf_res, df, the results of the three Bayes factor analysis
#'     \item bf_replication, numeric, rounded Bf of analysis with prior based on Bem 2011 experiment 1 results
#'     \item bf_uniform, numeric, rounded Bf of analysis with uniform prior
#'     \item bf_buj, numeric, rounded Bf of analysis with BUJ prior
#'     \item bf_inference, character, inference based on the three Bayes factors
#'   \item inference, character, the summarized final inference
#'   }
#' @export
#' @examples
#' \donttest{
#' # Running the confirmatory analysis
#' confirmatory_result <- analysis_confirmatory(df = example_m0)
#' # Checking the inferences at each checking point
#' confirmatory_result$inference
#' }
analysis_confirmatory <- function(df) {
  # Check whether the input df contains only erotic trials or not
  if (!all(df$reward_type == "erotic")) {
    df <- clean_data(raw_data = df)
  }
  
  # Get checkpoint information
  highest_checkpoint <- tell_checkpoint(df)$current_checkpoint
  
  if (is.na(highest_checkpoint)) {
    stop("The number of trials are not exceeding the first stopping point.")
  }
  
  checkpoint_n <- tppr::analysis_params$when_to_check[highest_checkpoint]
  
  # Preparing data for sequential analysis
  checkpoint_data <-
    df %>% 
    dplyr::slice(1:checkpoint_n)
  
  # Number of successful trials
  success <- sum(as.logical(checkpoint_data$sides_match), na.rm = T)
  
  # Number of trials
  total_n <- nrow(checkpoint_data)
  
  # Running sequential mixed effect and Bayes factor analysis
  # This is a counter to count the number of tests conducted using the mixed model due to sequential testing.
  mixed_effect_res <- confirmatory_mixed_effect(processed_data = checkpoint_data, n_iteration = highest_checkpoint)
  
  bf_res <- confirmatory_bayes_factor(success = success, total_n = total_n)

  # Deriving inferences
  inference <- inference_confirmatory_combined(n_iteration = highest_checkpoint,
                                               confirmatory_nhst_inference = mixed_effect_res$mixed_nhst_inference,
                                               confirmatory_bf_inference = bf_res$bf_inference)
  # Return output ---------------------------
  return(
    tibble::lst(
      highest_checkpoint,
      success,
      total_n,
      mixed_effect_res,
      bf_res,
      inference
    )
  )
}

#' Calculating cumulative success
#' 
#' This function calculates the number of trials and the
#' number of successful trials cumulatively. If raw data
#' are provided the function drops all the non-erotic
#' trials (see \code{\link{clean_data}}).
#' 
#' @param df dataframe, the input dataframe
#' 
#' @return The function returns a dataframe with the number
#' of trials in one column and the number of successful trials
#' in the other column.
#' 
#' @export
#' @examples
#' \donttest{
#' # Get total number of trials and success cumulatively
#' cumulative_success_res <- cumulative_success(df = example_m0)
#' }
cumulative_success <- function(df) {
  # Check whether the input df contains only erotic trials or not
  if (!all(df$reward_type == "erotic")) {
    df <- clean_data(raw_data = df)
  }
  
  # Calculative total_n and success cumulatively
  df %>% 
    dplyr::transmute(total_n = 1:nrow(.),
                     success = cumsum(sides_match))
}

#' Calculating cumulative Bayes factors
#' 
#' This function calculates the cumulative Bayes factors
#' with three priors. The output of this function is used for
#' \code{\link{plot_confirmatory}} as an input. The function
#' can accept a dataframe that is already only containing the
#' cumulative success variable \code{\link{cumulative_success}},
#' or a raw dataframe. 
#' 
#' @param df dataframe, the input dataframe
#' 
#' @return The function returns a wide formatted dataframe with
#' the number of trials, and the Bayes factors with the three different
#' trials.
#' 
#' @export
#' @examples
#' \donttest{
#' # Calculate the cumulative Bayes factors
#' cumulative_results <- cumulative_bayes_factor(df = example_m0)
#' }
cumulative_bayes_factor <- function(df) {
  # Calculating cumulative successes
  if ("success" %not_in% colnames(df)) {
    df <- cumulative_success(df = df)
  }
  
  # Calculating Bayes factors for each new experimental trial with all three priors
  df %>% 
    dplyr::mutate(BF_replication = purrr::map2_dbl(success, total_n,
                                                   ~ BF01_beta(y = .x,
                                                               N = .y,
                                                               y_prior = tppr::analysis_params$y_prior,
                                                               N_prior = tppr::analysis_params$n_prior,
                                                               interval = c(0.5, 1),
                                                               null_prob = tppr::analysis_params$m0_prob)),
                  BF_uniform = purrr::map2_dbl(success, total_n,
                                               ~ BF01_beta(y = .x,
                                                           N = .y,
                                                           y_prior = 0,
                                                           N_prior = 0,
                                                           interval = c(0.5, 1),
                                                           null_prob = tppr::analysis_params$m0_prob)),
                  BF_BUJ = purrr::map2_dbl(success, total_n,
                                           ~ BF01_beta(y = .x,
                                                       N = .y,
                                                       y_prior = 6,
                                                       N_prior = 12,
                                                       interval = c(0.5, 1),
                                                       null_prob = tppr::analysis_params$m0_prob)))
}
