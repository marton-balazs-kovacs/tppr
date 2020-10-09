#' Text helper functions
#' 
#' The following functions help to produce the 
#' interactive text outputs throughout the \emph{tppr}
#' shiny app, based on the results of the analysis.
#' 
#' @export
text_helper_current_general <- function(sample_descriptives_current_res, robustness_bf_res) {
  # Create text output ---------------------------
  glue::glue("The study currently has {total_n} erotic trials gathered from \\
  a total of {sample_size_participants_atleast1erotictrial} \\
  participants. There has been {n_sessions_terminated} incomplete \\
  study sessions so far resulting in a total of {n_missing_erotic_trials} ({prop_missing_erotic_trials}%) missing data \\
  points due to incomplete sessions. We observed a total of {prop_success}% successful guesses within \\
  {total_n} erotic trials (posterior mode = {hdi_mode * 100}%, posterior 90% HDI = {hdi_l * 100}%, {hdi_u * 100}%).",
             total_n = sample_descriptives_current_res$total_n,
             sample_size_participants_atleast1erotictrial = sample_descriptives_current_res$sample_size_participants_atleast1erotictrial,
             n_sessions_terminated = sample_descriptives_current_res$n_sessions_terminated,
             n_missing_erotic_trials = sample_descriptives_current_res$n_missing_erotic_trials,
             prop_missing_erotic_trials = sample_descriptives_current_res$prop_missing_erotic_trials,
             prop_success = sample_descriptives_current_res$prop_success,
             hdi_mode = robustness_bf_res$hdi_mode,
             hdi_l = robustness_bf_res$hdi_l,
             hdi_u = robustness_bf_res$hdi_u)
}

#' @rdname text_helper_current_general
#' @export
text_helper_current_confirmatory <- function(bf_replication, bf_uniform, bf_buj) {
  # Create text output ---------------------------
  reached_text <- function(output_val) {glue::glue("Observing this success rate is {output_val} times more likely \\
                             if humans can guess future randomly determined events than if they \\
                             are guessing randomly. Taken at face value, the data provide strong \\
                             evidence that the probability of successfully guessing later computer-generated \\
                             random events is higher than chance level as previously reported by \\
                             Bem (2011) and others (Bem, Tressoldi, Rabeyron, & Duggan, 2015).")}
  
  # Inference ---------------------------
  inference <- inference_confirmatory_bf(c(bf_replication, bf_uniform, bf_buj))
  
  # Return output ---------------------------
  if (inference == "M1") {
    reached_text(round(1 / max(c(bf_replication, bf_uniform, bf_buj)), 0))
    } else if (inference == "M0") {
      reached_text(round(min(c(bf_replication, bf_uniform, bf_buj)), 0))
      }else if (inference == "Inconclusive" & max(c(bf_replication, bf_uniform, bf_buj)) < 1) {
        reached_text(round(1 / max(c(bf_replication, bf_uniform, bf_buj)), 0))
        } else if (inference == "Inconclusive" & min(c(bf_replication, bf_uniform, bf_buj)) > 1) {
          reached_text(round(min(c(bf_replication, bf_uniform, bf_buj)), 0))
          } else {
            paste0("This study outcome did not reach the pre-specified criteria of strong support for either model.")
            }
}

#' @rdname text_helper_current_general
#' @export
text_helper_current_robustness <- function(inference_confrimatory_bf, inference_robustness_bf) {
  inference_robustness <- 
    if (inference_confrimatory_bf == "Inconclusive") {
      "NA, main inference inconclusive"
    } else if (inference_confrimatory_bf == "Ongoing") {
      "NA, main analysis ongoing"
    } else if (inference_confrimatory_bf == inference_robustness_bf) {
      "Robust"
    } else {
      "Not robust"
    }
  
  if (inference_robustness == "Robust") {
    "The results proved to be robust to different statistical approaches, increasing our confidence in our inference."
    } else if (inference_robustness == "Not robust") {
      "The results did not prove to be robust to different statistical approaches."
      } else {
        "As the main inference is inconclusive or the datacollection is still ongoing the robustness check results are irrelevant."
      }
}

#' @rdname text_helper_current_general
#' @export
text_helper_checkpoint_general <- function(sample_descriptives_res, robustness_bf_res) {
  # Create text output ---------------------------
  glue::glue("The following information reflects live study sessions at the latest passed checkpoint. \\
  In the main analysis we include {total_n} erotic trials from {sample_size_participants_atleast1erotictrial} participants. There has been {n_missing_erotic_trials} \\
  points due to incomplete sessions. The mean ESQ score is {esq_mean}, while the mean SSQ score is {ssq_mean} . \\
  We observed a total of {prop_success}% successful guesses within {total_n} erotic \\
  trials (posterior mode = {hdi_mode * 100}%, posterior 90% HDI = {hdi_l * 100}%, {hdi_u * 100}%).",
             total_n = sample_descriptives_res$total_n,
             sample_size_participants_atleast1erotictrial = sample_descriptives_res$sample_size_participants_atleast1erotictrial,
             n_missing_erotic_trials = sample_descriptives_res$n_missing_erotic_trials,
             esq_mean = sample_descriptives_res$esp_q_desc$mean,
             ssq_mean = sample_descriptives_res$ss_q_desc$mean,
             prop_success = sample_descriptives_res$prop_success,
             hdi_mode = robustness_bf_res$hdi_mode,
             hdi_l = robustness_bf_res$hdi_l,
             hdi_u = robustness_bf_res$hdi_u)
}

#' @rdname text_helper_current_general
#' @export
text_helper_checkpoint_confirmatory <- function(inference_confirmatory_checkpoint) {
  if (inference_confirmatory_checkpoint  == "M1") {
    "At the latest passed checkpoint the inference based on the results of \
             the mixed logistic regression modell and the Bayes factors calculated with \
             three different priors all support the M1 modell."
  } else if (inference_confirmatory_checkpoint  == "M0") {
    "At the latest passed checkpoint the inference based on the results of \
             the mixed logistic regression modell and the Bayes factors calculated with \
             three different priors all support the M0 modell."
  } else if (inference_confirmatory_checkpoint  == "Ongoing") {
    "At the latest passed checkpoint the inference based on the results of \
             the mixed logistic regression modell and the Bayes factors calculated with \
             three different priors do not lead to a conclusive inference but the data collection \
    is still ongoing."
  } else {
    "At the last checkpoint the inference based on the results of \
             the mixed logistic regression modell and the Bayes factors calculated with \
             three different priors seems to be lead to an inconclusive inference."
  }
}

#' @rdname text_helper_current_general
#' @export
text_helper_stop <- function(checkpoint_next) {
  glue::glue("None of the stopping rules have been triggered yet, so data \\
        collection is still in progress. The next crucial \\
        test will be at reaching {checkpoint_next} erotic trials.",
             checkpoint_next = tppr::analysis_params$when_to_check[checkpoint_next])
}

#' @rdname text_helper_current_general
#' @export
text_helper_warning <- function(checkpoint_next) {
  glue::glue("Result not yet final! \\
                 Data presented on this page represent the current trend \\
                 calculated from the data. The results should not be over-interpreted! \\
                 Random variations may cause the data to cross the decision thresholds. \\
                 Statistical decisions will only be drawn at the pre-specified stopping points. \\
                 The next stopping point will be at reaching \\
                 {checkpoint_next} trials.",
             checkpoint_next = tppr::analysis_params$when_to_check[checkpoint_next])
}