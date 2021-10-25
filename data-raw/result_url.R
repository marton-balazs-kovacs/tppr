result_url <- 
  list(
    time_log = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_time_log.rds",
    checkpoint = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_checkpoint.rds",
    checkpoint_confirmatory_mixed = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_confirmatory_mixed_checkpoint.rds",
    checkpoint_confirmatory_bayes = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_confirmatory_bayes_checkpoint.rds",
    checkpoint_exploratory = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_exploratory_checkpoint.rds",
    checkpoint_robustness_bayes = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_robustness_bayes_checkpoint.rds",
    checkpoint_robustness_nhst = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_robustness_nhst_checkpoint.rds",
    checkpoint_descriptive = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_descriptive.rds",
    current_cumulative_bayes = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_confirmatory_bayes.rds",
    current_robustness_bayes = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_robustness_bayes.rds",
    current_exploratory = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_exploratory.rds",
    current_descriptive = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_descriptive_current.rds",
    current_confirmatory_plot = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_confirmatory_plot.rds",
    current_robustness_plot = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_robustness_plot.rds",
    current_exploratory_plot = "http://raw.githubusercontent.com/marton-balazs-kovacs/tppr_results/main/tppr_results_exploratory_plot.rds"
  )

usethis::use_data(result_url, overwrite = TRUE, internal = TRUE)
