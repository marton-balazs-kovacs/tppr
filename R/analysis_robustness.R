#' Robustness test of Bf results with NHST      
#' 
#' Robustness of Bf results is tested with NHST proportion tests.
#' In this function we perform both an equivalence test and
#' an equality test to draw statistical inference.
#' 
#' @param success numeric, total number of successful guesses on erotic trials
#' @param total_n numeric, total number of all erotic trials
#' 
#' @return The function returns a list of three, the p-value of the
#' equivalence test, the p-value of the equality test, and the inference 
#' \code{\link{inference_robustness_nhst}} made based on the values
#' and alpha determined in `r analysis_param$inference_threshold_nhst`.
#' @export
robustness_nhst_analysis <- function(success, total_n) {
  # Equivalence test  ---------------------------
  equivalence_test_p <- prop.test(x = success, n = total_n, p = tppr::analysis_params$p_equiv_test, alternative = "less")$p.value
  
  # Equality test  ---------------------------
  equality_test_p <- prop.test(x = success, n = total_n, p = tppr::analysis_params$m0_prob, alternative = "greater")$p.value
  
  # Inference  ---------------------------
  inference_robustness_nhst <- inference_robustness_nhst(equivalence_test_p, equality_test_p)
  
  # Return output  ---------------------------
  return(
    tibble::lst(
      equivalence_test_p,
      equality_test_p,
      inference_robustness_nhst
      )
    )
}

#' Robustness test of Bf results with Bayesian parameter estimation 
#'
#' Robustness of BF results is tested by calculating HDI of the posterior
#' distribution and checking its relation to the region of practical equivalence (ROPE),
#' promoted in Kruschke, J. K., & Liddell, T. M. (2017). The Bayesian New Statistics:
#' Hypothesis testing, estimation, meta-analysis, and power analysis from a Bayesian perspective.
#' Psychonomic Bulletin & Review, 1-29.
#'  
#' @param success numeric, total number of successful guesses on erotic trials
#' @param total_n numeric, total number of all erotic trials
#' 
#' @return The function returns a list of five, the posterior density, the
#' mode of the HDI, the upper and lower bound of the HDI and the inference
#' that was based on these values (see \code{\link{inference_robustness_bf}}).
#' @export
robustness_bf_analysis <- function(success, total_n) {
  # Calculate posterior distribution using beta distribution updating ---------------------------
  prior_alpha <- tppr::analysis_params$y_prior + 1
  prior_beta <- tppr::analysis_params$n_prior - tppr::analysis_params$y_prior + 1
  
  posterior_alpha <- prior_alpha + success
  posterior_beta <- prior_beta + total_n - success
  
  posterior_density <- dbeta(tppr::analysis_params$scale, posterior_alpha, posterior_beta)
  
  # Calculate HDI for the posterior distribution  ---------------------------
  # (here we calculate the upper and lower bound of the 90% of the probability mass
  # because we use a one-tailed test. This means that the total probability mass below
  # the upper bound of the 90% HDI will be 95%).
  hdi_result <- mode_HDI(scale = tppr::analysis_params$scale,
                         density = posterior_density,
                         crit_width = 1 - tppr::analysis_params$inference_threshold_robustness_bayes_par_est * 2,
                         n_samples = 1e6)
  
  # Parameters for decision making
  hdi_mode <- hdi_result[1]
  hdi_l <- hdi_result[2]
  hdi_u <- hdi_result[3]
  
  # Probability that the parameter falls outside of the ROPE  ---------------------------
  probability_parameter_higher_than_rope <- sum(posterior_density[tppr::analysis_params$scale > tppr::analysis_params$rope]) / sum(posterior_density)
  
  # Inference ---------------------------
  inference_robustness_bayes <- inference_robustness_bf(hdi_l, hdi_u)
  
  # Return output ---------------------------
  return(
    tibble::lst(
      posterior_density,
      hdi_mode,
      hdi_l,
      hdi_u,
      inference_robustness_bayes
      )
    )
}

#' Robustness analysis
#' 
#' Two alternative variants of the Bayesian proportion test in the primary
#' confirmatory analysis: frequentist proportion test (see \code{\link{robustness_nhst_analysis}}),
#' and full-Bayesian hypothesis testing using parameter estimation (see \code{\link{robustness_bf_analysis}}).
#' 
#' @section Note: Robustness analysis results will not affect
#'   the conclusions of our study.
#'   
#' @param confirmatory_results list, output of the \code{\link{analysis_confirmatory}} function
#' 
#' @return The function returns a list containing the results
#' of the frequentist proportion test and the Bayesian parameter estimation
#' as nested lists, the final inference of the robustness test as a character
#' vector, and the results of the confirmatory mixed effect regression \code{\link{confirmatory_mixed_effect}}.
#' 
#' @export
#' @examples 
#' \donttest{
#' # Running the primary confirmatory analysis
#' confirmatory_results <- analysis_confirmatory(df = example_m0)
#' # Running the robustness analysis
#' analysis_robustness(confirmatory_results = confirmatory_results)
#' }
analysis_robustness <- function(confirmatory_results) {
  # Robustness test of BF results with NHST ---------------------------
  robustness_nhst_res <- robustness_nhst_analysis(success = confirmatory_results$success,
                                                  total_n = confirmatory_results$total_n)
  
  # Robustness test of BF results with Bayesian parameter estimation ---------------------------
  robustness_bayes_res <- robustness_bf_analysis(success = confirmatory_results$success,
                                                 total_n = confirmatory_results$total_n)
  
  # Combined robustness test ---------------------------
  robustness_inference <- 
    inference_robustness_combined(confirmatory_analysis_inference = confirmatory_results$inference,
                                  inference_robustness_nhst = robustness_nhst_res$inference_robustness_nhst,
                                  inference_robustness_bayes_par_est = robustness_bayes_res$inference_robustness_bayes)
  
  # Return output ---------------------------
  return(
    list(
      robustness_nhst_res = robustness_nhst_res,
      robustness_bayes_res = robustness_bayes_res,
      robustness_inference = robustness_inference,
      confirmatory_mixed_ci_width = confirmatory_results$mixed_effect_res$mixed_ci_width,
      confirmatory_mixed_ci_l = confirmatory_results$mixed_effect_res$mixed_ci_l,
      confirmatory_mixed_ci_u = confirmatory_results$mixed_effect_res$mixed_ci_u
    )
  )
}
