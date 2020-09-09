# Analysis parameters
## These are the analysis parameters currently specified in our protocol

create_analysis_params <- function() {
    # Descriptive ---------------------------
    # Number of erotic trials performed by participants in the study (if no missing trials)
    trial_size_per_participant <- 18
    # Probability of success if M0 is true
    m0_prob <- 0.5
    # Interim analysis points (in total number of ONLY EROTIC trials performed)
    when_to_check <- c(37836, 62388, 86958, 111528, 136080)

    # Thresholds to infer support for M0 (high) or M1 (low)
    inference_threshold_bf_high <- 25
    inference_threshold_bf_low <- 1 / inference_threshold_bf_high
    # This information is used both for calculating replication Bayes factor, and the Bayesian parameter estimation robustness test. 
    # Here we use data from Bem's experiment 1, 828 successes within 1560 erotic trials
    y_prior <- 828 # Number of successes in erotic trials in Bem's experiment 1
    n_prior <- 1560 # Number of erotic trials in Bem's experiment 1
    # Smallest effect size of interest in the NHST equivalence test 
    minimum_effect_threshold_nhst <- 0.01
    # p threshold for the NHST proportion test robustness test
    inference_threshold_robustness_nhst <- 0.005
    # In the Bayesian parameter estimation robustness test this will determine the region of practical 
    # equivalence (ROPE) interval. The ROPE is interpreted similarly to SESOI, but not entirely the same. 
    # See Kruschke, J. K., & Liddell, T. M. (2017). The Bayesian New Statistics: Hypothesis testing, 
    # estimation, meta-analysis, and power analysis from a Bayesian perspective. 
    # Psychonomic Bulletin & Review, 1-29. 
    minimum_effect_threshold_bayes_par_est <- 0.006
    # Robustness analysis ---------------------------
    scale <- seq(0, 1, length = 10001)
    # This threshold is used to set the HDI width to check against the ROPE in the Bayesian parameter 
    # estimation robustness test, if the parameter is set to 0.05 for example, it means that we would 
    # expect that 95% of the probability mass would be within the ROPE to accept a hypothesis.
    inference_threshold_robustness_bayes_par_est <- 0.05
    rope <- m0_prob + minimum_effect_threshold_bayes_par_est
    # Exploratory analysis ---------------------------
    # Samples 1,000,000 participants from a population with H0 success rate.
    # This is used for the stochastic dominance test as the null model
    # we call this the theoretical sample, because it approximates the theoretical null model.
    sim_null_participant_num <- 1000000
    success_proportions_theoretical <- rbinom(sim_null_participant_num, size = trial_size_per_participant, prob = m0_prob) / trial_size_per_participant
    # p threshold for the NHST tests
    # these are used in both the mixed model analysis in the primary analysis
    # and the proportion test in the robustness analysis
    # although, in the primary analysis this is adjusted for multiple testing
    # using Bonferroni's correction
    inference_threshold_nhst <- 0.005
    # This is the p for the equivalence test
    p_equiv_test <- m0_prob + minimum_effect_threshold_nhst
    # Samples 1,000,000 participants from a population with a 50% successful guess chance
    # homogeneous in the population we call this the theoretical sample, because it
    # approximates the theoretical null model.
    success_proportions_theoretical <- 
        round(
            rbinom(sim_null_participant_num,
                   size = trial_size_per_participant,
                   prob = m0_prob) / trial_size_per_participant,
            2)
    mget(ls())
}

analysis_params <- create_analysis_params()

usethis::use_data(analysis_params, overwrite = TRUE)
