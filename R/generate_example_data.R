#' Generate example session
#' 
#' This function generates the example data of one participant.
#' 
#' @inheritParams generate_example_data
#' 
#' @return The function returns a dataframe containing the realistic
#' example data of one participant. The returned dataframe has 22 variables
#' and 1 to 41 rows depending on the results of the random probability
#' functions.
#' @export
generate_session <- function(n_iteration, erotic_trial_size_per_participant, m0_prob, m1_prob, esp_user_percentage, chance_for_refuse_consent, chance_for_stopping_session) {
  # Number of trials per participant
  n_all_trials <- erotic_trial_size_per_participant * 2 + 5
  
  # The participant has a chance to stop the session at any trial
  # we calculate the number of completed trials for the session
  stop_index <-
    purrr::detect_index(
      runif(n_all_trials, min = 0, max = 1) < chance_for_stopping_session,
      ~ .x == TRUE)

  if (stop_index == 0) {
    n_trials <- n_all_trials
  } else {
    n_trials <- stop_index
  }

  # Set up laboratory info
  lab_characteristics_sim <- 
    tibble::tibble(
      exp_ID = paste0("exp_", 1:10),
      exp_ASGS = 1:10,
      lab_ID = rep(paste0("lab_", 1:5), each = 2),
      lab_ASGS = 10:1
    )
  
  # Select randomly one lab
  lab_chars <- 
    lab_characteristics_sim %>% 
    dplyr::slice_sample(1)
  
  # Save column names that will be NA if consent form is no
  participant_info_cols <- 
    c("refused_to_answer_sexual_orientation_question","age", "sex" , "final_consent", 
      "ESP_Q_item_1", "ESP_Q_item_2", "ESP_Q_item_3", "SS_Q_item_1", "SS_Q_item_2", 
      "trial_number", "guessed_side", "target_side", "reward_type", "sides_match")
  
  # Determine whether the participant agreed to the consent form or not
  consent_screen_answer <- sample(c("yes", "no"), prob = c(1 - chance_for_refuse_consent, chance_for_refuse_consent), 1)

  if (consent_screen_answer == "no") {
    # Create table with the participant information
    res <- 
      tibble::tibble_row(
        timestamp = "simulation",
        participant_ID = glue::glue("part_{n_iteration}"),
        experimenter_ID_code = lab_chars$exp_ID,
        experimenter_ASGS_total_score = lab_chars$exp_ASGS,
        laboratory_ID_code = lab_chars$lab_ID,
        sitePI_ASGS_total_score = lab_chars$lab_ASGS,
        session_type = "live",
        consent_screen_answer = consent_screen_answer
      )
    
    res <-
      res %>% 
      tibble::add_column(!!!rlang::set_names(as.list(rep(NA_character_, length(participant_info_cols))), nm = participant_info_cols))
    
    } else if (consent_screen_answer == "yes") {
      # Set up participant info
      ## Fix
      esp_user <- rbinom(1, 1, prob = esp_user_percentage)
      prob <- m0_prob + (m1_prob - m0_prob) * esp_user
      ESP_Q_item_1 <- sample(c("Definitely Does not", "Probably does not", "Don't know", "Probably does", "Definitely does"), 1)
      ESP_Q_item_2 <- sample(c("Definitely no", "Probably no", "Don't know", "Probably yes", "Definitely yes"), 1)
      ESP_Q_item_3 <- sample(c("No, never", "Only a few times", "Occasionally", "Regurarly in the past", "Regurarly no"), 1)
      SS_Q_item_1 <- sample(c("very untrue", "untrue", "between true and untrue", "true", "very true"), 1)
      SS_Q_item_2 <- sample(c("very untrue", "untrue", "between true and untrue", "true", "very true"), 1)
      
      res <- 
        tibble::tibble(
          timestamp = rep("simulation", n_trials),
          participant_ID = rep(glue::glue("part_{n_iteration}"), n_trials),
          experimenter_ID_code = rep(lab_chars$exp_ID, n_trials),
          experimenter_ASGS_total_score = rep(lab_chars$exp_ASGS, n_trials),
          laboratory_ID_code = rep(lab_chars$lab_ID, n_trials),
          sitePI_ASGS_total_score = rep(lab_chars$lab_ASGS, n_trials),
          session_type = rep("live", n_trials)
          )
      
      ## Only if test trials are started
      if (n_trials > 5) {
        trial_number <- 1:(n_trials - 5)
        flip <- rbinom(n_trials - 5, 1, prob = prob)
        guessed_side <- sample(c("left", "right"), n_trials - 5, replace = T)
        target_side <- dplyr::if_else(flip == 0,
                                      dplyr::if_else(guessed_side == "left",
                                                     "right",
                                                     "left"),
                                      dplyr::if_else(guessed_side == "left",
                                                     "left",
                                                     "right"))
        reward_type <- sample(rep(c("erotic", "neutral"), each = erotic_trial_size_per_participant), replace = F)[1:(n_trials -5)]
        sides_match <- dplyr::if_else(guessed_side == target_side,
                                      "true",
                                      "false")
        res <- 
          res %>% 
          tibble::add_column(
            consent_screen_answer = c(
              NA_character_,
              rep(consent_screen_answer, n_trials - 1)),
            refused_to_answer_sexual_orientation_question = c(
              rep(NA_character_, 2),
              rep("no", n_trials - 2)),
            age = c(
              rep(NA_character_, 2),
              rep(sample(c("18-29", "30-44"), 1), n_trials - 2)),
            sex = c(
              rep(NA_character_, 2),
              rep(sample(c("Female", "Male"), 1), n_trials - 2)),
            final_consent = c(
              rep(NA_character_, 3),
              rep("yes", n_trials - 3)),
            ESP_Q_item_1 = c(
              rep(NA_character_, 3),
              rep(ESP_Q_item_1, n_trials - 3)),
            ESP_Q_item_2 = c(
              rep(NA_character_, 3),
              rep(ESP_Q_item_2, n_trials - 3)),
            ESP_Q_item_3 = c(
              rep(NA_character_, 3),
              rep(ESP_Q_item_3, n_trials - 3)),
            SS_Q_item_1 = c(
              rep(NA_character_, 4),
              rep(SS_Q_item_1, n_trials - 4)),
            SS_Q_item_2 = c(
              rep(NA_character_, 4),
              rep(SS_Q_item_2, n_trials - 4)),
            trial_number = c(
              rep(NA_character_, 5),
              trial_number),
            guessed_side = c(
              rep(NA_character_, 5),
              guessed_side),
            target_side = c(
              rep(NA_character_, 5),
              target_side),
            reward_type = c(
              rep(NA_character_, 5),
              reward_type),
            sides_match = c(
              rep(NA_character_, 5),
              sides_match)
            )
        
        } else if (n_trials == 5) {
          res <- 
            res %>% 
            tibble::add_column(
              consent_screen_answer = c(
                NA_character_,
                rep(consent_screen_answer, n_trials - 1)),
              refused_to_answer_sexual_orientation_question = c(
                rep(NA_character_, 2),
                rep("no", n_trials - 2)),
              age = c(
                rep(NA_character_, 2),
                rep(sample(c("18-29", "30-44"), 1), n_trials - 2)),
              sex = c(
                rep(NA_character_, 2),
                rep(sample(c("Female", "Male"), 1), n_trials - 2)),
              final_consent = c(
                rep(NA_character_, 3),
                rep("yes", n_trials - 3)),
              ESP_Q_item_1 = c(
                rep(NA_character_, 3),
                rep(ESP_Q_item_1, n_trials - 3)),
              ESP_Q_item_2 = c(
                rep(NA_character_, 3),
                rep(ESP_Q_item_2, n_trials - 3)),
              ESP_Q_item_3 = c(
                rep(NA_character_, 3),
                rep(ESP_Q_item_3, n_trials - 3)),
              SS_Q_item_1 = c(
                rep(NA_character_, 4),
                rep(SS_Q_item_1, n_trials - 4)),
              SS_Q_item_2 = c(
                rep(NA_character_, 4),
                rep(SS_Q_item_2, n_trials - 4)),
              trial_number = rep(NA_character_, 5),
              guessed_side = rep(NA_character_, 5),
              target_side = rep(NA_character_, 5),
              reward_type = rep(NA_character_, 5),
              sides_match = rep(NA_character_, 5)
              )
          
          } else if (n_trials == 4) {
            res <- 
              res %>% 
              tibble::add_column(
                consent_screen_answer = c(
                  NA_character_,
                  rep(consent_screen_answer, n_trials - 1)),
                refused_to_answer_sexual_orientation_question = c(
                  rep(NA_character_, 2),
                  rep("no", n_trials - 2)),
                age = c(
                  rep(NA_character_, 2),
                  rep(sample(c("18-29", "30-44"), 1), n_trials - 2)),
                sex = c(
                  rep(NA_character_, 2),
                  rep(sample(c("Female", "Male"), 1), n_trials - 2)),
                final_consent = c(
                  rep(NA_character_, 3),
                  rep("yes", n_trials - 3)),
                ESP_Q_item_1 = c(
                  rep(NA_character_, 3),
                  rep(ESP_Q_item_1, n_trials - 3)),
                ESP_Q_item_2 = c(
                  rep(NA_character_, 3),
                  rep(ESP_Q_item_2, n_trials - 3)),
                ESP_Q_item_3 = c(
                  rep(NA_character_, 3),
                  rep(ESP_Q_item_3, n_trials - 3)),
                SS_Q_item_1 = rep(NA_character_, 4),
                SS_Q_item_2 = rep(NA_character_, 4),
                trial_number = rep(NA_character_, 4),
                guessed_side = rep(NA_character_, 4),
                target_side = rep(NA_character_, 4),
                reward_type = rep(NA_character_, 4),
                sides_match = rep(NA_character_, 4)
                )
            
            } else if (n_trials == 3) {
              res <- 
                res %>% 
                tibble::add_column(
                  consent_screen_answer = c(
                    NA_character_,
                    rep(consent_screen_answer, n_trials - 1)),
                  refused_to_answer_sexual_orientation_question = c(
                    rep(NA_character_, 2),
                    rep("no", n_trials - 2)),
                  age = c(
                    rep(NA_character_, 2),
                    rep(sample(c("18-29", "30-44"), 1), n_trials - 2)),
                  sex = c(
                    rep(NA_character_, 2),
                    rep(sample(c("Female", "Male"), 1), n_trials - 2)),
                  final_consent = rep(NA_character_, 3),
                  ESP_Q_item_1 = rep(NA_character_, 3),
                  ESP_Q_item_2 = rep(NA_character_, 3),
                  ESP_Q_item_3 = rep(NA_character_, 3),
                  SS_Q_item_1 = rep(NA_character_, 3),
                  SS_Q_item_2 = rep(NA_character_, 3),
                  trial_number = rep(NA_character_, 3),
                  guessed_side = rep(NA_character_, 3),
                  target_side = rep(NA_character_, 3),
                  reward_type = rep(NA_character_, 3),
                  sides_match = rep(NA_character_, 3)
                  )
              
              } else if (n_trials == 2) {
                res <- 
                  res %>% 
                  tibble::add_column(
                    consent_screen_answer = c(
                      NA_character_,
                      rep(consent_screen_answer, n_trials - 1)),
                    refused_to_answer_sexual_orientation_question = rep(NA_character_, 2),
                    age = rep(NA_character_, 2),
                    sex = rep(NA_character_, 2),
                    final_consent = rep(NA_character_, 2),
                    ESP_Q_item_1 = rep(NA_character_, 2),
                    ESP_Q_item_2 = rep(NA_character_, 2),
                    ESP_Q_item_3 = rep(NA_character_, 2),
                    SS_Q_item_1 = rep(NA_character_, 2),
                    SS_Q_item_2 = rep(NA_character_, 2),
                    trial_number = rep(NA_character_, 2),
                    guessed_side = rep(NA_character_, 2),
                    target_side = rep(NA_character_, 2),
                    reward_type = rep(NA_character_, 2),
                    sides_match = rep(NA_character_, 2)
                  )
              }
      }
  
  # Return output ---------------------------
  return(res)
}

#' Generate example data
#' 
#' This code generates realistic example data for
#' the Transparent Psi Project.
#' 
#' @section More about the params:
#'   Set esp_user_percentage to 1 to simulate that everyone has the same level
#'   of ESP ability, or set it to 0 to simulate that no one has ESP ability. Set
#'   the value to something in between to simulate that only a portion of the population can
#'   use ESP, and the others are just guessing randomly. E.g.,  
#'   example esp_user_percentage = 0.03 and m1_prob = 0.65 simulates that only 3% 
#'   of the total population can use ESP, and prediction success rate in this subgroup is 65%.
#'   Setting chance_for_stopping_session to 0.002 would generate roughly 2% missing data,
#'   but that cannot introduce bias in our confirmatory analysis.
#' 
#' @param num_sim number of simulated sessions (1 regular session contains 36 trials, 18 erotic and 18 non-erotic)
#' @param erotic_trial_size_per_participant number of erotic trials performed per participant
#' @param m0_prob probability of successful guess if M0 is true
#' @param m1_prob probability of successful guess among ESP-users in the simulated sample
#' @param esp_user_percentage the percentage of ESP-users, or ESP-capable individuals in the population
#' @param chance_for_refuse_consent chance of refusing consent or being excluded due to one of the eligibility criteria (e.g. underage)
#' @param chance_for_stopping_session chance of stopping prematurely in each trial, to simulate some missing data due to unexpected events
#' 
#' @return A dataframe containing the same data structure as expected
#' from real data collected in the study.
#' @export
generate_example_data <- function(num_sim = 6000, erotic_trial_size_per_participant = 18, m0_prob = 0.5, m1_prob = 0.51, esp_user_percentage = 1, chance_for_refuse_consent = 0.01, chance_for_stopping_session = 0.002) {
  # Set up progress bar ---------------------------
  generate_session_progress <- function(n_iteration, erotic_trial_size_per_participant, m0_prob, m1_prob, esp_user_percentage, chance_for_refuse_consent, chance_for_stopping_session) {
    pb$tick()$print()
    generate_session(n_iteration, erotic_trial_size_per_participant, m0_prob, m1_prob, esp_user_percentage, chance_for_refuse_consent, chance_for_stopping_session)
  }
  
  pb <- progress_estimated(num_sim)
  
  # Generate example data ---------------------------
  n_iteration <- seq_along(1:num_sim)
  
  res <-
    n_iteration %>% 
    purrr::map_df(~ generate_session_progress(n_iteration = .x,
                                              erotic_trial_size_per_participant = erotic_trial_size_per_participant,
                                     m0_prob = m0_prob,
                                     m1_prob = m1_prob,
                                     esp_user_percentage = esp_user_percentage,
                                     chance_for_refuse_consent = chance_for_refuse_consent,
                                     chance_for_stopping_session = chance_for_stopping_session))
  
  # ADD row_counter variable
  
  # Return output ---------------------------
  return(res)
}
