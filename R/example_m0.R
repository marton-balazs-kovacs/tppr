#' Example dataset with support for M0
#' 
#' This dataset contains generated example data that
#' supports the M0 hypothesis.
#' 
#' @format Dataframe with 117757 rows and 23 variables.
#' \describe{
#'   \item{timestamp}{character, time of the recording of the trial}
#'   \item{participant_ID}{character, unique id of the participant}
#'   \item{experimenter_ID_code}{character, unique id of the experimenter}
#'   \item{experimenter_ASGS_total_score}{numeric, }
#'   \item{laboratory_ID_code}{character, unique id of the laboritory}
#'   \item{sitePI_ASGS_total_score}{numeric, }
#'   \item{session_type}{character, }
#'   \item{consent_screen_answer}{character, }
#'   \item{refused_to_answer_sexual_orientation_question}{character, }
#'   \item{age}{character, age group of the participant}
#'   \item{sex}{character, gender of the participant}
#'   \item{final_consent}{character, whether the participant accepted the consent form}
#'   \item{ESP_Q_item_1}{character, }
#'   \item{ESP_Q_item_2}{character, }
#'   \item{ESP_Q_item_3}{character, }
#'   \item{SS_Q_item_1}{character, }
#'   \item{SS_Q_item_2}{character, }
#'   \item{trial_number}{numeric, }
#'   \item{guessed_side}{character, which side the participant guessed in the current trial}
#'   \item{target_side}{character, which side hides the target}
#'   \item{reward_type}{character, whether the reward type is neautral or erotic}
#'   \item{sides_match}{logical, whether the guessed side and the target sides match}
#'   \item{row_counter}{integer, unique id of the row}
#' }
#' @source \url{"https://raw.githubusercontent.com/kekecsz/Transparent_psi_RR_materials/master/TPP_example_data_M0.csv"}
"example_m0"