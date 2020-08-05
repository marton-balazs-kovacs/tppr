#' Get datafile download links from a Github folder
#' 
#' This function gets the download urls of every file in a Github
#' folder.
#' 
#' @param folder_path character, url to the Github folder that contains the datafiles
#' 
#' @return The function returns the download links as a character vector.
get_links <- function(folder_path) {
  httr::GET(folder_path) %>% 
    httr::content(.) %>% 
    purrr::map_chr(., "download_url")
}

#' Function to read and merge data
#' 
#' The test, pilot and live datafiles are stored in the projects Github repository.
#' This function reads, orders by date and merge the individual datafiles of the
#' chosen repository.
#' 
#' @param type character, either "test", "pilot" or "live"
#' 
#' @return The function returns a raw dataframe that contains all the trials from every
#' participant.
#' @export
#' @examples
#' \donttest{
#' tpp_raw_data <- read_data(type = "test")
#' }
read_data <- function(type) {
  # Get links ---------------------------
  ## Get the names of the datafiles
  links <- switch(type,
                  "live" = get_links("https://api.github.com/repos/gyopak/transparent-psi-results/contents/live_data"),
                  "test" = get_links("https://api.github.com/repos/gyopak/transparent-psi-results/contents/test_data"),
                  "pilot" = get_links("https://api.github.com/repos/gyopak/transparent-psi-results/contents/pilot_data"),
                  stop("Invalid data type! Choose \"test\", \"pilot\" or \"live\""))
  
  ## Sort datafiles by file creation order starting with the oldest files
  start_dates <-
    links %>% 
    stringr::str_extract(., "[^._]+(?=[^_]*$)") %>% 
    lubridate::dmy(.)
  
  links <- links[order(start_dates)]
  
  ## Save column names
  col_names <- c("timestamp", "participant_ID", "experimenter_ID_code", "experimenter_ASGS_total_score", "laboratory_ID_code", "sitePI_ASGS_total_score", "session_type", "consent_screen_answer", "refused_to_answer_sexual_orientation_question", "age", "sex", "final_consent", "ESP_Q_item_1", "ESP_Q_item_2", "ESP_Q_item_3", "SS_Q_item_1", "SS_Q_item_2", "trial_number", "guessed_side", "target_side", "reward_type", "sides_match")
  
  ## The first live and pilot file have missing column names
  if (type %in% c("live", "pilot")) {
    # Get the url of the first file
    first_file_url <- links[1]
    # Download the data
    first_file_data <- vroom::vroom(first_file_url, delim = ",", col_names = col_names, col_types = "cfcicicccccccccccicccl")
    # Delete the file from the vector of links
    links <- links[-1]
  }
  
  # Download data from github ---------------------------
  source_data <-
    links %>% 
    purrr::map_df(.,
                  ~ vroom::vroom(.,
                                 delim = ",",
                                 col_types = "cfcicicccccccccccicccl"))
  
  ## Merge the first file with the rest of the datafiles for pilot and live
  if (type %in% c("live", "pilot")) {
    source_data <- dplyr::bind_rows(first_file_data, source_data)
  }
  
  # Data wrangling ---------------------------
  source_data <-
    source_data %>% 
    dplyr::mutate(row_counter = dplyr::row_number(),
                  # Should this be logical or numeric? The original code is confusing.
                  sides_match = as.numeric(sides_match))
  
  ## Sessions conducted with the test accounts or without lab_IDs are excluded
  lab_ids_to_exclude <- c("", "18155ef201564afbb81f6a8b74aa9a033eac51ec6595510eca9606938ffaced3", "ece83ceb8611d1926746e5bb3597ed1e8cb5d336521331b31961d5c0348883cf")
  raw_data <- 
    source_data %>% 
    dplyr::filter(not_na(laboratory_ID_code),
                  laboratory_ID_code %not_in% lab_ids_to_exclude)
  
  ## Excluding test sessions that were conducted with a valid experimenter ID and as pilot session type
  if (type == "pilot"){
    participant_ids_to_exclude <- c("5e45139f-e642-4539-8958-6906c3f6b9c6", "2a8349db-4868-44b9-853f-9c7205d834d2")
    raw_data <- 
      raw_data %>% 
      dplyr::filter(participant_ID %not_in% participant_ids_to_exclude)
  }
  
  ## Number of participants tested
  sample_size_participants_started_session <- dplyr::n_distinct(raw_data$participant_ID, na.rm = TRUE)
  message(paste("There are", sample_size_participants_started_session, "participants who started the experiment."))
  
  # Return output ---------------------------
  return(raw_data)
}

#' Function to extracts erotic trials
#' 
#' This function takes the raw data which contains all the trials
#' (see \code{\link{read_data}}), and excludes non-erotic trials, and
#' trials with missing trial numbers.
#' 
#' @param raw_data dataframe, df containing all trials
#' 
#' @return The output is the processed dataframe, that can be analyzed with
#' the analysis functions \code{\link{analyis_confrimatory}}, \code{\link{analyis_robustness}},
#' \code{\link{analyis_exploratory}}.
#' @export
#' @examples 
#' \donttest{
#' tpp_processed_data <- clean_data(raw_data = example_m0)
#' }
clean_data <- function(raw_data) {
  ## Extract data from erotic trials 
  processed_data <- 
    raw_data %>% 
    dplyr::filter(not_na(trial_number),
                  reward_type == "erotic")
  
  return(processed_data)
}

#' Function to return checkpoint information
#' 
#' This function checks the number of trials in the input
#' dataframe and determines the index of the currently passed
#' checkpoint and the next checkpoint based on the stopping points
#' determined in the sequential analysis plan. If raw data are provided
#' the function drops all the non-erotic trials (see \code{\link{clean_data}}).
#' The function uses the following checking points: (`r analysis_params$when_to_check`).
#' 
#' @param df dataframe, the input dataframe
#' 
#' @return The function returns a list of four. The number of trials,
#' the index of the currently passed checkpoint, the index of the
#' next checkpoint, the row_counter of the last row.
#' @export
#' @examples
#' \donttest{
#' # Including only erotic trials
#' tpp_processed_data <- clean_data(raw_data = example_m0)
#' # Get checkpoint
#' tell_checkpoint(df = tpp_processed_data)
#' }
tell_checkpoint <- function(df) {
  # Load analysis params
  # data("analysis_params", envir = environment(), package = "tppr")
  
  # Saving params to help readability
  when_to_check <- analysis_params$when_to_check
  
  # Check whether the input df contains only erotic trials or not
  if (!all(df$reward_type == "erotic")) {
    df <- clean_data(raw_data = df)
  }
  
  # Total number of valid erotic trials
  total_n <- nrow(df)
  
  # Find the index of the current and next checkpoint index
  if (min(when_to_check) > total_n) {
    stop("The number of trials are not exceeding the first stopping point.")
  } else if (min(when_to_check) < total_n & max(when_to_check) > total_n) {
    current_checkpoint <- max(which(when_to_check < total_n))
    next_checkpoint <- min(which(when_to_check > total_n))
  } else if (max(when_to_check) < total_n) {
    current_checkpoint <- 10L
    next_checkpoint <- NA_integer_
  }
  
  # Find the row_counter of the last row
  last_row <- 
    df %>% 
    dplyr::slice(when_to_check[current_checkpoint]) %>% 
    dplyr::pull(row_counter)
  
  # Return output ---------------------------
  return(
    list(
      total_n = total_n,
      current_checkpoint = current_checkpoint,
      next_checkpoint = next_checkpoint,
      last_row = last_row
    )
  )
}

#' Function to split data according to stopping points
#' 
#' This function cuts the processed data at each checkpoint
#' that is passed and saves the result to a list of dataframes.
#' The function also calculates the total number of erotic trials,
#' and the total number of successful guesses for the each checkpoint.
#' Finally, it saves the number of iterations that is needed for the
#' correction of alpha for NHST tests.
#' 
#' @param processed_data, dataframe, df containing only erotic trials
#' 
#' @return The function returns a dataframe with the following structure:
#'   \itemize{
#'   \item checkpoint, integer,the number of investigated erotic trials at a given checkpoint
#'   \item split_data, df, the input dataframe until with trials from 1 to the given checkpoint
#'   \item success, integer, the number of successful guesses
#'   \item total_n, integer, the number of erotic trials
#'   \item n_iteration, integer, the number of iteration (see \code{\link{confirmatory_mixed_effect}})
#'   }
#' @export
#' @examples 
#' \donttest{
#' # Including only erotic trials
#' tpp_processed_data <- clean_data(raw_data = example_m0)
#' # Splitting data by checkpoints
#' tpp_split_data <- split_data(processed_data = tpp_processed_data)
#' }
split_data <- function(processed_data) {
  # Get checkpoint information
  highest_checkpoint <- tell_checkpoint(processed_data)$current_checkpoint
  check_range <- analysis_params$when_to_check[1:highest_checkpoint]
  
  # Split data for sequential analysis
  split_data <-
    tidyr::tibble(checkpoint = check_range) %>% 
    # Split the data according the checkpoints
    dplyr::mutate(split_data = purrr::map(checkpoint, 
                                          ~ dplyr::slice(processed_data, 1:.x)),
                  # Number of successful trials
                  success = purrr::map_dbl(split_data,
                                           ~ sum(as.logical(.x$sides_match), na.rm = T)),
                  # Number of trials
                  total_n = purrr::map_int(split_data, nrow),
                  # This is a counter to count the number of tests conducted using the mixed model due to sequential testing.
                  n_iteration = 1:highest_checkpoint)
  
  # Return output
  return(
    split_data
  )
}