#' Preset parameters for the analysis
#' 
#' Dataset containing the preset parameters that are needed
#' for the analysis as a list. The parameters correspond to
#' the values of the paper.
#' 
#' @format A large list with 18 elements:
#' \describe{
#'   \item{trial_size_per_participant}{numeric, number of erotic trials performed by participants in the study (if no missing trials)}
#'   \item{m0_prob}{numeric, probability of success if M0 is true}
#'   \item{when_to_check}{integer, interim analysis points (in total number of ONLY EROTIC trials performed)}
#'   \item{inference_threshold_bf_high}{numeric, threshold to infer support for M0 (high)}
#'   \item{inference_threshold_bf_low}{numeric, threshold to infer support for M1 (low)}
#'   \item{y_prior}{integer, number of successes in erotic trials in Bem's experiment 1}
#'   \item{n_prior}{integer, number of erotic trials in Bem's experiment 1}
#'   \item{minimum_effect_threshold_nhst}{numeric, smallest effect size of interest in the NHST equivalence test}
#'   \item{inference_threshold_robustness_nhst}{numeric, p threshold for the NHST proportion test robustness test}
#'   \item{minimum_effect_threshold_bayes_par_est}{numeric}
#'   \item{scale}{numeric}
#'   \item{inference_threshold_robustness_bayes_par_est}{numeric, threshold to set the HDI width to check against the ROPE}
#'   \item{rope}{numeric}
#'   \item{sim_null_participant_num}{integer, number of participants to sample from a population with H0 success rate}
#'   \item{success_proportions_theoretical}{numeric, the theoretical sample that approximates the theoretical null model}
#'   \item{inference_threshold_nhst}{numeric, p threshold for the NHST tests}
#'   \item{p_equiv_test}{numeric, p for the equivalence test}
#'   \item{success_proportions_theoretical}{numeric, samples 1,000,000 participants from a population with a 50\% successful guess chance}
#'}
"analysis_params"
