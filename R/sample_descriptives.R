#' Sample and study characteristics
#' 
#' This function calculates all sample and study characteristics
#' mentioned in the results section of the manuscript from the
#' raw data (see \code{\link{read_data}}). The function calculates
#' the descriptive results at the latest passed checkpoint if not
#' determined otherwise with the `which_checkpoint` param.
#' 
#' @section Note:
#'   This function can be used only in which the number of erotic
#'   trials reach the first checkpoint at least. Otherwise please
#'   use the \code{\link{sample_descriptives_current}} function
#'   to see a simple study characteristics summary.
#' 
#' @param raw_data dataframe, all trials included in the study
#' @param which_checkpoint integer, index of the checkpoint to use
#' 
#' @return The function returns a list of descriptive results regarding
#' the sample until the latest or prespecified checkpoint.
#' @export
#' @examples 
#' \donttest{
#' sample_descriptives(raw_data = example_m0, which_checkpoint = NA_integer_)
#' }
sample_descriptives <- function(raw_data, which_checkpoint = NA_integer_) {
  # Get checkpoint information ---------------------------
  checkpoint_inf <- tell_checkpoint(df = raw_data)
  
  # Validation ---------------------------
  if (is.na(checkpoint_inf$current_checkpoint)) {
    stop("The number of trials are not exceeding the first stopping point.")
  }
  
  if(!is.na(which_checkpoint) & which_checkpoint > checkpoint_inf$current_checkpoint) {
    stop(paste("The checkpoint index cannot pass the number of reached checkpoints, which is", checkpoint_inf$current_checkpoint))
  }
  
  # Drop non erotic and empty trials
  processed_data <- clean_data(raw_data = raw_data)
  
  # Get row_counter value of the last row in the processed df
  ## This is needed to slice the raw df until the needed checkpoint
  if (is.na(which_checkpoint)) {
    last_row <- 
      processed_data %>% 
      dplyr::slice(tppr::analysis_params$when_to_check[checkpoint_inf$current_checkpoint]) %>% 
      dplyr::pull(row_counter)
  } else {
    last_row <- 
      processed_data %>% 
      dplyr::slice(tppr::analysis_params$when_to_check[which_checkpoint]) %>% 
      dplyr::pull(row_counter)
  }

  # Sample descriptive: all trials ---------------------------
  # Raw data filtered until the current checkpoint
  raw_data_untilstudystop <- 
    raw_data %>% 
    dplyr::slice(seq(match(last_row, row_counter)))
  
  # Number of participants started the session in total
  n_participants_started_session_total <- dplyr::n_distinct(raw_data$participant_ID, na.rm = TRUE)
  
  # Number of participants started the session until the last checkpoint
  n_participants_started_session_untilstudystop <- dplyr::n_distinct(raw_data_untilstudystop$participant_ID, na.rm = TRUE)
  
  # Number of participants who started the session after the last checkpoint
  n_participants_started_session_afterstudystop <- n_participants_started_session_total - n_participants_started_session_untilstudystop
  
  # Sample descriptive: just erotic trials ---------------------------
  # Processed data filtered until the current checkpoint 
  processed_data_untilstudystop <-
    processed_data %>% 
    dplyr::slice(seq(match(last_row, row_counter)))
  
  # Number of participants with at least one erotic trial
  sample_size_participants_atleast1erotictrial <- dplyr::n_distinct(processed_data$participant_ID, na.rm = TRUE)

  # Number of participants with at least one erotic trials who made it into the main analysis
  n_participants_data_included_in_main_analysis <- dplyr::n_distinct(processed_data_untilstudystop$participant_ID, na.rm = TRUE)
  
  # Proportion of participants who did not have any erotic trials
  proportion_participants_novaliddata_untilstudystop <- (n_participants_started_session_untilstudystop - n_participants_data_included_in_main_analysis) / n_participants_started_session_untilstudystop
  
  # Other descriptive ---------------------------
  # Total number of valid erotic trials until the checkpoint
  total_n <- nrow(processed_data_untilstudystop)
  
  # Getting the first row of each participant included in the main analysis
  first_rows_of_each_participant <-
    processed_data_untilstudystop %>% 
    dplyr::group_by(participant_ID) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::ungroup()
  
  # Age descriptive
  age_desc <-
    first_rows_of_each_participant %>% 
    dplyr::count(age) %>% 
    dplyr::mutate(N = sum(n),
                  prop = n / N * 100)
  
  age_range_of_most_participants <-
    age_desc %>% 
    dplyr::slice_max(n) %>% 
    dplyr::pull(age)
  
  age_range_of_most_participants_proportion <-
    age_desc %>% 
    dplyr::slice_max(prop) %>% 
    dplyr::pull(prop)
  
  # Gender descriptive
  sex_desc <-
    first_rows_of_each_participant %>% 
    dplyr::count(sex) %>% 
    dplyr::mutate(N = sum(n),
                  prop = n / N * 100)
  
  n_sex_women <-
    sex_desc %>%   
    dplyr::filter(sex == "Female") %>% 
    dplyr::pull(n)
  
  n_sex_men <-
    sex_desc %>%   
    dplyr::filter(sex == "Male") %>% 
    dplyr::pull(n)
  
  proportion_sex_women <-
    sex_desc %>% 
    dplyr::filter(sex == "Female") %>% 
    dplyr::pull(prop)
  
  proportion_sex_men <-
    sex_desc %>% 
    dplyr::filter(sex == "Male") %>% 
    dplyr::pull(prop)
  
  # ESP score descriptive
  esp_q_desc <- 
    first_rows_of_each_participant %>% 
    dplyr::mutate(ESP_Q_item_1 = as.numeric(ordered(factor(ESP_Q_item_1),
                                                  levels = c("Definitely Does not",
                                                             "Probably does not",
                                                             "Don't know",
                                                             "Probably does",
                                                             "Definitely does")))) %>% 
    dplyr::summarise(mean = round(mean(ESP_Q_item_1, na.rm = T), 2),
                     sd = round(sd(ESP_Q_item_1, na.rm = T), 2))
  
  # SS score descriptive
  ss_q_desc <- 
    first_rows_of_each_participant %>% 
    dplyr::mutate_at(
      dplyr::vars(dplyr::contains("SS_Q")),
      list(~ as.numeric(ordered(factor(.), levels = c("very untrue", "untrue", "between true and untrue", "true", "very true"))))) %>% 
    dplyr::mutate(SS_Q_item_1 = 6 - SS_Q_item_1)
  
  ss_q_desc$SS_Q_average_score <- apply(cbind(ss_q_desc$SS_Q_item_1, ss_q_desc$SS_Q_item_2), 1, mean)
  
  ss_q_desc <- 
    ss_q_desc %>% 
    dplyr::summarise(mean = round(mean(SS_Q_average_score), 2),
                     sd = round(sd(SS_Q_average_score), 2))
  
  # Guessed image descriptive
  guessed_side_left <-
    processed_data_untilstudystop %>% 
    dplyr::count(guessed_side) %>%
    dplyr::mutate(N = sum(n),
                  prop = n / N * 100) %>% 
    dplyr::filter(guessed_side == "left")
  
  n_guessed_side_left <-
    guessed_side_left %>% 
    dplyr::pull(n)
  
  proportion_guessed_side_left <-
    guessed_side_left %>% 
    dplyr::pull(prop)
  
  # Target image descriptive
  target_side_left <-
    processed_data_untilstudystop %>% 
    dplyr::count(target_side) %>%
    dplyr::mutate(N = sum(n),
                  prop = n / N * 100) %>% 
    dplyr::filter(target_side == "left")
  
  n_target_side_left <-
    target_side_left %>% 
    dplyr::pull(n)
  
  proportion_target_side_left <-
    target_side_left %>% 
    dplyr::pull(prop)
  
  # Number of erotic trials per participant
  n_erotic_trials_per_participant <- 
    processed_data_untilstudystop %>% 
    dplyr::count(participant_ID) %>% 
    dplyr::pull(n)
  
  # Number of missing erotic trials
  n_missing_erotic_trials <- sum(18 - n_erotic_trials_per_participant)
  
  # Number and proportion of sessions where the participant did not finish the experiment
  sessions_desc <- 
    processed_data_untilstudystop %>% 
    dplyr::count(participant_ID) %>% 
    dplyr::mutate(finished = dplyr::case_when(n == 18 ~ 1L,
                                              n != 18 ~ 0L)) %>% 
    dplyr::count(finished) %>% 
    dplyr::mutate(N = sum(n),
                  prop = n / N * 100) %>% 
    dplyr::filter(finished == 0L)
  
  n_sessions_terminated <-
    sessions_desc %>% 
    dplyr::pull(n)
  
  proportion_sessions_terminated <-
    sessions_desc %>% 
    dplyr::pull(prop)
  
  # Proportion of successful guesses
  prop_success <- round(mean(processed_data_untilstudystop$sides_match), 4) * 100
  
  # Return output ---------------------------
  return(
    tibble::lst(
      n_participants_started_session_total,
      n_participants_started_session_untilstudystop,
      n_participants_started_session_afterstudystop,
      sample_size_participants_atleast1erotictrial,
      n_participants_data_included_in_main_analysis,
      proportion_participants_novaliddata_untilstudystop,
      total_n,
      age_range_of_most_participants,
      age_range_of_most_participants_proportion,
      n_sex_women,
      n_sex_men,
      proportion_sex_women,
      proportion_sex_men,
      esp_q_desc,
      ss_q_desc,
      n_guessed_side_left,
      proportion_guessed_side_left,
      n_target_side_left,
      proportion_target_side_left,
      n_missing_erotic_trials,
      n_sessions_terminated,
      proportion_sessions_terminated,
      prop_success
    )
  )
}
#' Current descriptive
#' 
#' This function calculates the most important descriptive statistics
#' for all the collected trials. The function is made to be used
#' in the \emph{tppr} Shiny app mostly.
#' 
#' @param processed_data dataframe, containing only the erotic trials
#' 
#' @return The function returns a list of descriptive statistics.
#' 
#' @export
sample_descriptives_current <- function(processed_data) {
  # Calculate parameters ---------------------------
  # Total number of trials
  total_n <- nrow(processed_data)
  
  # Total number of participants
  sample_size_participants_atleast1erotictrial <- dplyr::n_distinct(processed_data$participant_ID, na.rm = TRUE)
  
  # Number of erotic trials per participant
  n_erotic_trials_per_participant <- 
    processed_data %>% 
    dplyr::count(participant_ID) %>% 
    dplyr::pull(n)
  
  # Number of missing erotic trials
  n_missing_erotic_trials <- sum(18 - n_erotic_trials_per_participant)

  prop_missing_erotic_trials <- round(n_missing_erotic_trials / total_n * 100, 2)
  
  # Number of participants who did not finish the experiment
  n_sessions_terminated <- 
    processed_data %>% 
    dplyr::count(participant_ID) %>% 
    dplyr::filter(n != 18) %>%
    dplyr::distinct(participant_ID) %>% 
    dplyr::count() %>% 
    dplyr::pull(n)
  
  # Guesses
  prop_success <- round(mean(processed_data$sides_match), 4) * 100
  
  # Return output ---------------------------
  return(
    tibble::lst(
      total_n,
      sample_size_participants_atleast1erotictrial,
      n_sessions_terminated,
      n_missing_erotic_trials,
      prop_missing_erotic_trials,
      prop_success
    )
  )
}
