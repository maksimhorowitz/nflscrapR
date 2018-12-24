# This file contains code for functions that are simply expected points and win
# probability calculators. The purpose of this is to make it easier for hypothetical
# situation calculations as well as for use in Shiny apps.

#' Compute expected points for provided play-by-play dataset
#' 
#' Given a dataset of plays and the necessary variables, this function returns
#' the original dataset with columns appended from the expected points model.
#' The user is also allowed to input values of the different scoring events,
#' for instance if they believe a TD is more likely to be worth 6 or 8 points.
#' 
#' @param pbp_data Play-by-play dataset to estimate expected points for.
#' @param half_seconds_remaining String denoting the name of the column of the
#' pbp_data containing the number of seconds remaining in the current half.
#' @param yard_line_100 String denoting the name of the column of the pbp_data
#' containing the yards away from the opponents' endzone.
#' @param down String denoting the name of the column of the pbp_data containing
#' the down of the play.
#' @param yards_to_go String denoting the name of the column of the pbp_data 
#' containing the yards to go for a first down of the play.
#' @param goal_to_go String denoting the name of the column of the pbp_data
#' containing the indicator for whether or not the play is in a goal down 
#' situation.
#' @param td_value Numeric point value for a TD occurring, default is 7.
#' @param fg_value Numeric point value for a FG occurring, default is 3.
#' @param safety_value Numeric point value for a safety occurring, default is 2.
#' @return The original pbp_data with the following columns appended to it:
#' ep (expected points), no_score_prob (probability of no score occurring until
#' the end of the half), opp_fg_prob (probability of opposing team scoring a 
#' field goal next), opp_safety_prob (probability of opposing team scoring a 
#' safety next), opp_td_prob (probability of opposing team scoring a TD next),
#' fg_prob (probability of possession team scoring a field goal next), 
#' safety_prob (probability of possession team scoring a safety next), and 
#' td_prob (probability of possession team scoring a touchdown next).
#' @export

calculate_expected_points <- function(pbp_data, half_seconds_remaining,
                                      yard_line_100, down, yards_to_go,
                                      goal_to_go, td_value = 7, fg_value = 3,
                                      safety_value = 2) {
  # First assert that each of the given column names for the necessary variables
  # are in the provided play-by-play dataset:
  assertthat::assert_that(all(c(half_seconds_remaining, yard_line_100, down,
                                yards_to_go, goal_to_go) %in% 
                                colnames(pbp_data)),
                          msg = paste0("The provided variable names for: ",
                                       paste0(c(half_seconds_remaining, yard_line_100, down,
                                         yards_to_go, goal_to_go)[which(!(c(half_seconds_remaining, yard_line_100, down,
                                                                            yards_to_go, goal_to_go) %in% 
                                                                            colnames(pbp_data)))],
                                         collapse = ", "), " are not in pbp_data!"))
  
  # Create a copy of the dataset for the EP model:
  model_pbp_data <- pbp_data
  
  # Generate the variable names to match what is necessary for the EP model:
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == half_seconds_remaining)] <- "TimeSecs_Remaining"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == yard_line_100)] <- "yrdline100"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == down)] <- "down"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == yards_to_go)] <- "yards_to_go"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == goal_to_go)] <- "GoalToGo"
  
  # Compute the log yards to go for the model:
  model_pbp_data$log_ydstogo <- log(as.numeric(model_pbp_data$yards_to_go))
  # Create the under two minute warning indicator:
  model_pbp_data$Under_TwoMinute_Warning <- ifelse(model_pbp_data$TimeSecs_Remaining < 120,
                                                   1, 0)
  # Convert the down variable to a factor:
  model_pbp_data$down <- as.factor(model_pbp_data$down)
  
  # Now generate the predictions from the EP model:
  # First get the predictions from the base ep_model:
  if (nrow(model_pbp_data) > 1) {
    base_ep_preds <- as.data.frame(predict(ep_model, newdata = model_pbp_data, type = "probs"))
  } else{
    base_ep_preds <- as.data.frame(matrix(predict(ep_model, newdata = model_pbp_data, type = "probs"),
                                          ncol = 7))
  }
  colnames(base_ep_preds) <- c("No_Score", "Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                               "Field_Goal", "Safety", "Touchdown")
  
  # Rename the columns to be consistent with the nflscrapR play-by-play datasets:
  base_ep_preds <- base_ep_preds %>%
    dplyr::rename(no_score_prob = No_Score,
                  opp_fg_prob = Opp_Field_Goal,
                  opp_safety_prob = Opp_Safety,
                  opp_td_prob = Opp_Touchdown,
                  fg_prob = Field_Goal,
                  safety_prob = Safety,
                  td_prob = Touchdown) %>%
    # Calculate the expected points:
    dplyr::mutate(ep = (0 * no_score_prob) + (-fg_value * opp_fg_prob) + 
                    (-safety_value * opp_safety_prob) +
                    (-td_value * opp_td_prob) + (fg_value * fg_prob) +
                    (safety_value * safety_prob) + (td_value * td_prob))
  
  # Now append to the original dataset and return:
  pbp_data %>%
    dplyr::bind_cols(base_ep_preds) %>%
    return
  
}

#' Compute win probability for the provided play-by-play dataset.
#' 
#' Given a dataset of plays and the necessary variables, this function returns
#' the original dataset with the win probability from the nflscrapR model.
#' 
#' @param pbp_data Play-by-play dataset to estimate expected points for.
#' @param half_seconds_remaining String denoting the name of the column of the
#' pbp_data containing the number of seconds remaining in the current half.
#' @param game_seconds_remaining String denoting the name of the column of the
#' pbp_data containing the number of seconds remaining in the game.
#' @param score_differential String denoting the name of the column of the pbp_data
#' containing the score differential with respect to the possession team.
#' @param quarter String denoting the name of the column of the pbp_data containing
#' the quarter of the play (anything above 4 is considered overtime).
#' @param posteam_timeouts_pre String denoting the name of the column of the pbp_data 
#' containing the number of timeouts remaining for the team with possession.
#' @param oppteam_timeouts_pre String denoting the name of the column of the pbp_data
#' containing the number of timeouts remaining for the opposing team.
#' @param ep String denoting the name of the column of the pbp_data containing
#' the expected points with respect to the possession team for the play.
#' @return The original pbp_data with a column named wp containing the win probability
#' with respect to the team with possession.
#' @export

calculate_win_probability <- function(pbp_data, half_seconds_remaining,
                                      game_seconds_remaining, score_differential,
                                      quarter, posteam_timeouts_pre,
                                      oppteam_timeouts_pre, ep) {
  
  # First assert that each of the given column names for the necessary variables
  # are in the provided play-by-play dataset:
  assertthat::assert_that(all(c(half_seconds_remaining,
                                game_seconds_remaining, score_differential,
                                quarter, posteam_timeouts_pre,
                                oppteam_timeouts_pre, ep) %in% 
                                colnames(pbp_data)),
                          msg = paste0("The provided variable names for: ",
                                       paste0(c(half_seconds_remaining,
                                                game_seconds_remaining, score_differential,
                                                quarter, posteam_timeouts_pre,
                                                oppteam_timeouts_pre, ep)[which(!(c(half_seconds_remaining,
                                                                                    game_seconds_remaining, score_differential,
                                                                                    quarter, posteam_timeouts_pre,
                                                                                    oppteam_timeouts_pre, ep) %in% 
                                                                                   colnames(pbp_data)))],
                                              collapse = ", "), " are not in pbp_data!"))
  
  # Create a copy of the dataset for the WP model:
  model_pbp_data <- pbp_data
  
  # Generate the variable names to match what is necessary for the EP model:
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == half_seconds_remaining)] <- "TimeSecs_Remaining"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == game_seconds_remaining)] <- "game_seconds_remaining"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == score_differential)] <- "score_differential"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == quarter)] <- "quarter"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == posteam_timeouts_pre)] <- "posteam_timeouts_pre"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == oppteam_timeouts_pre)] <- "oppteam_timeouts_pre"
  colnames(model_pbp_data)[which(colnames(model_pbp_data) == ep)] <- "ep"
  
  
  # Compute the expected score differential for the model:
  model_pbp_data$ExpScoreDiff <- model_pbp_data$ep + model_pbp_data$score_differential
  # Now the ratio:
  model_pbp_data$ExpScoreDiff_Time_Ratio = model_pbp_data$ExpScoreDiff / 
    (model_pbp_data$game_seconds_remaining + 1)
  # Create the under two minute warning indicator:
  model_pbp_data$Under_TwoMinute_Warning <- ifelse(model_pbp_data$TimeSecs_Remaining < 120,
                                                   1, 0)
  # Create the half indicator (ignoring overtime plays here due to various rules):
  model_pbp_data$Half_Ind = ifelse(as.numeric(model_pbp_data$quarter) %in% c(1, 2), 
                                   "Half1", "Half2")
  
  # Create the win probability column:
  pbp_data$wp <- as.numeric(mgcv::predict.bam(wp_model, newdata = model_pbp_data,
                                              type = "response"))
  
  # Now return the dataset:
  return(pbp_data)
  
}

