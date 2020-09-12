#' Inference for Bayesian proportion tests
#' 
#' This function make the inference for a Bayesian
#' proportion test based on the result of the test. If
#' the Bayes factor is lower than 1/25 the test supports
#' M1, but if the Bayes factor is higher than 25 the test
#' supports M0, otherwise inconclusive.
#' 
#' @family inference functions, confirmatory functions
#' 
#' @param bf numeric, the resulting Bayes factor
#' 
#' @return The function returns a character value that is
#' either M1, M0 or Inconclusive.
#' @export
inference_confirmatory_bf <- function(bf) {
  if(tppr::analysis_params$inference_threshold_bf_low >= bf) {return("M1")
  } else if(tppr::analysis_params$inference_threshold_bf_high <= bf) {return("M0")
  } else {return("Inconclusive")}
}

#' Inference for mixed effect test
#' 
#' This function make the inference for the mixed-effects
#' logistic regression test. If the upper bound of the resulting
#' confidence interval is lower than 0.51, the test supports M0,
#' if the lower bound is higher than 0.5, the test supports M1.
#' Otherwise inconclusive.
#' 
#' @family inference functions, confirmatory functions
#' 
#' @param mixed_ci_u numeric, the upper bound of the confidence interval
#' @param mixed_ci_l numeric, the lower bound of the confidence interval
#' 
#' @return The function returns a character value that is
#' either M1, M0 or Inconclusive.
#' @export
inference_confirmatory_mixed_effect <- function(mixed_ci_u, mixed_ci_l) {
  # Statistical inference based on the results of the mixed model analysis  
  minimum_effect <- tppr::analysis_params$m0_prob + tppr::analysis_params$minimum_effect_threshold_nhst
  # Note: hardcoded if else loop is 3 times faster but this code is more concise, it is a decision to make
  dplyr::if_else(mixed_ci_u < minimum_effect,
                 "M0",
                 dplyr::if_else(mixed_ci_l > tppr::analysis_params$m0_prob,
                                "M1",
                                "Inconclusive"))
}

#' Inference for the confirmatory analysis
#' 
#' This function calculates the final inference for the
#' confirmatory analysis (see \code{\link{analysis_confirmatory}}).
#' If all the four primary tests support M0 or M1 the final inference
#' is conclusive. If after the last checkpoint not all the four
#' tests support M0 or M1 the inference is inconclusive, otherwise
#' the study is ongoing.
#' 
#' @family inference functions, confirmatory functions
#' 
#' @param n_iteration numeric, the number of iterations
#' @param total_n numeric, the total number of erotic trials
#' @param mixed_ci_u numeric, the upper bound of the confidence interval
#' @param mixed_ci_l numeric, the lower bound of the confidence interval
#' @param bf_replication numeric, replication prior Bf
#' @param bf_uniform numeric, uniform prior Bf
#' @param bf_buj numeric, knowledge-base prior Bf
#' 
#' @return The function returns a character value that is
#' either M1, M0, Inconclusive or Ongoing.
#' @export
inference_confirmatory_combined <- function(n_iteration, total_n, mixed_ci_u, mixed_ci_l, bf_replication, bf_uniform, bf_buj) {
  # Mixed effect inference ---------------------------
  mixed_nhst_inference <- inference_confirmatory_mixed_effect(mixed_ci_u, mixed_ci_l)
  
  # Bayes factor inference ---------------------------
  # Replication Bayes factor 
  bf_replication_inference <- inference_confirmatory_bf(bf_replication)
  
  # Bayes factor with uniform prior 
  bf_uniform_inference <- inference_confirmatory_bf(bf_uniform)
  
  # Bayes factor with BUJ prior
  bf_buj_inference <- inference_confirmatory_bf(bf_buj)
  
  # Main analysis inference ---------------------------
  # Determine final inference (supported model) based on the inferences drawn
  # from the mixed model and the Bayes factors.
  if (all(c(mixed_nhst_inference, bf_replication_inference, bf_uniform_inference, bf_buj_inference) == "M1")) {
    primary_analysis_inference <- "M1"
  } else if (all(c(mixed_nhst_inference, bf_replication_inference, bf_uniform_inference, bf_buj_inference) == "M0")) {
    primary_analysis_inference <- "M0"
  } else if ((n_iteration != which.max(tppr::analysis_params$when_to_check)) & (total_n < max(tppr::analysis_params$when_to_check))) {
    primary_analysis_inference <- "Ongoing"
  } else {
    primary_analysis_inference <- "Inconclusive"
  }
  
  # Return output ---------------------------
  return(primary_analysis_inference)
}

#' Inference for robustness test using NHST
#' 
#' This function makes the inference for the robustness
#' analysis test that uses frequentist proportion test
#' (see \code{\link{robustness_nhst_analysis}}).
#' 
#' @family inference functions, robustness functions
#' 
#' @param equivalence_test_p numeric, the p-value of the equivalnce test
#' @param equality_test_p numeric, the p-value of the equiality test
#' 
#' @return The function returns a character that is either M0, M1 or Inconclusive.
#' @export
inference_robustness_nhst <- function(equivalence_test_p, equality_test_p) {
  if (tppr::analysis_params$inference_threshold_nhst > equivalence_test_p) {
    inference_robustness_nhst <- "M0"
  } else if (tppr::analysis_params$inference_threshold_nhst > equality_test_p) {
    inference_robustness_nhst <- "M1" 
  } else {
    inference_robustness_nhst <- "Inconclusive"
  }
  
  # Return output ---------------------------
  return(inference_robustness_nhst)
}

#' Inference for robustness test using Bayesian parameter estimation
#' 
#' This function makes the inference for the robustness
#' analysis test that uses Bayesian parameter estimation  of the
#' main parameter value (see \code{\link{robustness_bf_analysis}}).
#' 
#' @family inference functions, robustness functions
#' 
#' @param hdi_l numeric, the lower bound of the HDI
#' @param hdi_u numeric, the upper bound of the HDI
#' 
#' @return The function returns a character that is either M0, M1 or Inconclusive.
#' @export
inference_robustness_bf <- function(hdi_l, hdi_u) {
  if (hdi_l >= tppr::analysis_params$rope) {
    inference_robustness_bayes_par_est <- "M1"
  } else if (hdi_u <= tppr::analysis_params$rope) {
    inference_robustness_bayes_par_est <- "M0"
  } else {
    inference_robustness_bayes_par_est <- "Inconclusive"
  }
  
  # Return output ---------------------------
  return(inference_robustness_bayes_par_est)
}

#' Inference for robustness analysis
#' 
#' The main analysis inference is only robust if all robustness
#' tests came to the same inference as the final inference
#' of the primary analysis (see \code{\link{inference_confirmatory_combined}}).
#' 
#' @family inference functions, robustness functions
#' 
#' @param confirmatory_analysis_inference character
#' @param inference_robustness_nhst character
#' @param inference_robustness_bayes_par_est character
#' 
#' @return The function returns a character value.
#' @export
inference_robustness_combined <- function(confirmatory_analysis_inference, inference_robustness_nhst, inference_robustness_bayes_par_est) {
  inferences <- c(inference_robustness_nhst, inference_robustness_bayes_par_est)
  inference_robustness <- 
    if (all(inferences == inferences[1])) {
      inferences[1]
    } else {
      "mixed"
    }
  
  res <- 
    if (confirmatory_analysis_inference == "Inconclusive") {
      "NA, main inference inconclusive"
    } else if (confirmatory_analysis_inference == "Ongoing") {
      "NA, main analysis ongoing"
    } else if (confirmatory_analysis_inference == inference_robustness) {
      "Robust"
    } else {
      "Not robust"
    }
  # Return output ---------------------------
  return(res)
}