#' Scrape an individual game's play-by-play data from NFL.com
#' 
#' Depending on the year and type of the given game id, this function returns
#' the play-by-play data available from NFL.com from either JSON (games starting
#' in 2009) or parsed HTML (regular and post-season games starting in 1998, 
#' pre-season games starting in 2000). The necessary info regarding the game's 
#' play-by-play data to be scraped (type and season) is provided by the 
#' \code{\link{scrape_game_ids}} function. In addition to all of the play-level data
#' provided by the NFL, outputs from the `nflscrapR` expected points and win 
#' probability models are also included, allowing an individual to conduct their
#' own analysis with more advanced football statistics. A detailed description 
#' of the models methodologies can be found here: \url{https://arxiv.org/abs/1802.00998}.
#' This function acts as a wrapper function for the 
#' \code{\link{scrape_json_play_by_play}} and \code{\link{scrape_html_play_by_play}} 
#' functions, deciding which to use depending on the input.
#' 
#' @param game_id Ten digit game id (either numeric or character) associated 
#' with a given NFL game.
#' @param type String indicating the type of game, must either be: "pre", 
#' "reg", or "post". This argument is required for entry to ensure proper 
#' handling of the type of play-by-play data for the given game.
#' @param season Numeric 4-digit year associated with an NFL season for the given
#' game id. 
#' @param check_url Indicator for whether or not to check if the game's url 
#' exists (default is 1, meaning yes). This is used to work with the 
#' \code{\link{scrape_season_play_by_play}} function that will perform its own
#' check prior to scraping individual games.
#' @return Data frame where each individual row represents a single play in 
#' the game containing detailed information depending on the availability of 
#' data for the game. See \code{\link{scrape_json_play_by_play}} and 
#' \code{\link{scrape_html_play_by_play}} for documentation about the 
#' columns returned.
#' @examples
#' # Scrape the play-by-play data for the 2017 Super Bowl by first getting all
#' # of the post-season game ids then use the required info to scrape the 
#' # play-by-play data (knowing that it's the last game):
#' playoff_game_ids_17 <- scrape_game_ids(2017, type = "post")
#' sb_17_id <- playoff_game_ids_17$game_id[nrow(playoff_game_ids_17)]
#' sb_17_pbp <- scrape_game_play_by_play(game_id = sb_17_id, type = "post", 
#'                                       season = 2017)
#' @export

scrape_game_play_by_play <- function(game_id, type, season, check_url = 1) {
  
  # First check that the type is one of the required options:
  assertthat::assert_that(tolower(type) %in% c("reg", "pre", "post"),
                          msg = "Input for type is not valid! Should either be 'reg', 'pre', or 'post'.")
  
  # Next check that if the type is pre then the season is at least 1999:
  if (tolower(type) == "pre") {
    assertthat::assert_that(as.numeric(season) >= 2000,
                            msg = "Preseason game ids with data are only available starting in 2000!")
    # Otherwise check to see that it's at least 1998
  } else {
    assertthat::assert_that(as.numeric(season) >= 1998,
                            msg = "Regular and post-season game ids with data are only available starting in 1998!")
  }
  
  # Scrape JSON data for games since 2009, HTML for everything prior to that:
  if (season >= 2009) {
    game_pbp <- scrape_json_play_by_play(game_id, check_url)
  } else {
    # game_pbp <- scrape_html_play_by_play(game_id, check_url)
    # This will be added later:
    print("Game data prior to 2009 coming soon!")
    game_pbp <- NA
  }
  
  # Return the game's data:
  return(game_pbp)
}


#' Scrape an individual game's JSON play-by-play data from NFL.com
#' 
#' This function returns the play-by-play data available from NFL.com's JSON feed 
#' (for games starting in 2009). 
#' 
#' @param game_id Ten digit game id (either numeric or character) associated 
#' with a given NFL game.
#' @param check_url Indicator for whether or not to check if the game's url 
#' exists (default is 1, meaning yes). This is used to work with the 
#' \code{\link{scrape_season_play_by_play}} function that will perform its own
#' check prior to scraping individual games.
#' @return Data frame where each individual row represents a single play in 
#' the game containing the following detailed information:
#' \itemize{
#' \item{play_id} - Numeric play id that when used with game_id and drive provides
#' the unique identifier for a single play.
#' \item{game_id} - Ten digit identifier for NFL game.
#' \item{home_team} - String abbreviation for the home team.
#' \item{away_team} - String abbreviation for the away team.
#' \item{posteam} - String abbreviation for the team with possession.
#' \item{posteam_type} - String indicating whether the posteam team is home or away.
#' \item{defteam} - String abbreviation for the team on defense.
#' \item{side_of_field} - String abbreviation for which team's side of the field
#' the team with possession is currently on.
#' \item{yardline_100} - Numeric distance in the number of yards from the opponent's
#' endzone for the posteam.
#' \item{game_date} - Date of the game.
#' \item{quarter_seconds_remaining} - Numeric seconds remaining in the quarter.
#' \item{half_seconds_remaining} - Numeric seconds remaining in the half.
#' \item{game_seconds_remaining} - Numeric seconds remaining in the game.
#' \item{game_half} - String indicating which half the play is in, either Half1, 
#' Half2, or Overtime.
#' \item{quarter_end} - Binary indicator for whether or not the row of the data
#' is marking the end of a quarter.
#' \item{drive} - Numeric drive number in the game.
#' \item{sp} - Binary indicator for whether or not a score occurred on the play.
#' \item{qtr} - Quarter of the game (5 is overtime).
#' \item{down} - The down for the given play.
#' \item{goal_to_go} - Binary indicator for whether or not the posteam is in a 
#' goal down situation.
#' \item{time} - Time at start of play provided in string format as minutes:seconds 
#' remaining in the quarter.
#' \item{yrdln} - String indicating the current field position for a given play.
#' \item{ydstogo} - Numeric yards in distance from either the first down marker or
#' the endzone in goal down situations.
#' \item{ydsnet} - Numeric value for total yards gained on the given drive.
#' \item{desc} - Detailed string description for the given play.
#' \item{play_type} - String indicating the type of play: pass (includes sacks), 
#' run (includes scrambles), punt, field_goal, kickoff, extra_point, 
#' qb_kneel, qb_spike, no_play (timeouts and penalties), and missing for rows
#' indicating end of play.
#' \item{yards_gained} - Numeric yards gained (or lost) for the given play.
#' \item{shotgun} - Binary indicator for whether or not the play was in shotgun
#' formation.
#' \item{no_huddle} - Binary indicator for whether or not the play was in no_huddle formation.
#' \item{qb_dropback} - Binary indicator for whether or not the QB dropped back on
#' the play (pass attempt, sack, or scrambled).
#' \item{qb_kneel} - Binary indicator for whether or not the QB took a knee.
#' \item{qb_spike} - Binary indicator for whether or not the QB spiked the ball.
#' \item{qb_scramble} - Binary indicator for whether or not the QB scrambled.
#' \item{pass_length} - String indicator for pass length: short or deep.
#' \item{pass_location} - String indicator for pass location: left, middle, or right.
#' \item{air_yards} - Numeric value for distance in yards perpendicular to the line
#' of scrimmage at where the targeted receiver either caught or didn't catch the ball.
#' \item{yards_after_catch} - Numeric value for distance in yards perpendicular to
#' the yard line where the receiver made the reception to where the play ended.
#' \item{run_location} - String indicator for location of run: left, middle, or right.
#' \item{run_gap} - String indicator for line gap of run: end, guard, or tackle
#' \item{field_goal_result} - String indicator for result of field goal attempt:
#' made, missed, or blocked.
#' \item{kick_distance} - Numeric distance in yards for kickoffs, field goals, 
#' and punts.
#' \item{extra_point_result} - String indicator for the result of the extra point
#' attempt: good, failed, blocked, safety (touchback in defensive endzone is 1
#' point apparently), or aborted.
#' \item{two_point_conv_result} - String indicator for result of two point conversion
#' attempt: success, failure, safety (touchback in defensive endzone is 1 point 
#' apparently), or return.
#' \item{home_timeouts_remaining} - Numeric timeouts remaining in the half
#' for the home team.
#' \item{away_timeouts_remaining} - Numeric timeouts remaining in the half for
#' the away team.
#' \item{timeout} - Binary indicator for whether or not a timeout was called.
#' \item{timeout_team} - String abbreviation for which team called the timeout.
#' \item{td_team} - String abbreviation for which team scored the touchdown.
#' \item{posteam_timeouts_remaining} - Number of timeouts remaining for the 
#' possession team.
#' \item{defteam_timeouts_remaining} - Number of timeouts remaining for the 
#' team on defense.
#' \item{total_home_score} - Score for the home team at the start of the play.
#' \item{total_away_score} - Score for the away team at the start of the play.
#' \item{posteam_score} - Score the posteam at the start of the play.
#' \item{defteam_score} - Score the defteam at the start of the play.
#' \item{score_differential} - Score differential between the posteam and defteam
#' at the start of the play.
#' \item{posteam_score_post} - Score for the posteam at the end of the play.
#' \item{defteam_score_post} - Score for the defteam at the end of the play.
#' \item{score_differential_post} - Score differential between the posteam and 
#' defteam at the end of the play.
#' \item{no_score_prob} - Predicted probability of no score occurring for the rest
#' of the half based on the expected points model.
#' \item{opp_fg_prob} - Predicted probability of the defteam scoring a FG next.
#' \item{opp_safety_prob} - Predicted probability of the defteam scoring a safety
#' next.
#' \item{opp_td_prob} - Predicted probability of the defteam scoring a TD next.
#' \item{fg_prob} - Predicted probability of the posteam scoring a FG next.
#' \item{safety_prob} - Predicted probability of the posteam scoring a safety next.
#' \item{td_prob} - Predicted probability of the posteam scoring a TD next.
#' \item{extra_point_prob} - Predicted probability of the posteam scoring an extra point.
#' \item{two_point_conversion_prob} - Predicted probability of the posteam scoring
#' the two point conversion.
#' \item{ep} - Using the scoring event probabilities, the estimated expected points
#' with respect to the possession team for the given play.
#' \item{epa} - Expected points added (EPA) by the posteam for the given play.
#' \item{total_home_epa} - Cumulative total EPA for the home team in the game so far.
#' \item{total_away_epa} - Cumulative total EPA for the away team in the game so far.
#' \item{total_home_rush_epa} - Cumulative total rushing EPA for the home team in
#' the game so far.
#' \item{total_away_rush_epa} - Cumulative total rushing EPA for the away team in
#' the game so far.
#' \item{total_home_pass_epa} - Cumulative total passing EPA for the home team in
#' the game so far.
#' \item{total_away_pass_epa} - Cumulative total passing EPA for the away team in
#' the game so far.
#' \item{air_epa} - EPA from the air yards alone. For completions this represents
#' the actual value provided through the air. For incompletions this represents
#' the hypothetical value that could've been added through the air if the pass
#' was completed.
#' \item{yac_epa} - EPA from the yards after catch alone. For completions this
#' represents the actual value provided after the catch. For incompletions this
#' represents the difference between the hypothetical air_epa and the play's raw
#' observed EPA (how much the incomplete pass cost the posteam).
#' \item{comp_air_epa} - EPA from the air yards alone only for completions.
#' \item{comp_yac_epa} - EPA from the yards after catch alone only for completions.
#' \item{total_home_comp_air_epa} - Cumulative total completions air EPA for the home team in
#' the game so far.
#' \item{total_away_comp_air_epa} - Cumulative total completions air EPA for the away team in
#' the game so far.
#' \item{total_home_comp_yac_epa} - Cumulative total completions yac EPA for the home team in
#' the game so far.
#' \item{total_away_comp_yac_epa} - Cumulative total completions yac EPA for the away team in
#' the game so far.
#' \item{total_home_raw_air_epa} - Cumulative total raw air EPA for the home team in
#' the game so far.
#' \item{total_away_raw_air_epa} - Cumulative total raw air EPA for the away team in
#' the game so far.
#' \item{total_home_raw_yac_epa} - Cumulative total raw yac EPA for the home team in
#' the game so far.
#' \item{total_away_raw_yac_epa} - Cumulative total raw yac EPA for the away team in
#' the game so far.
#' \item{wp} - Estimated win probabiity for the posteam given the current situation
#' at the start of the given play.
#' \item{def_wp} - Estimated win probability for the defteam.
#' \item{home_wp} - Estimated win probability for the home team.
#' \item{away_wp} - Estimated win probability for the away team.
#' \item{wpa} - Win probability added (WPA) for the posteam.
#' \item{home_wp_post} - Estimated win probability for the home team at the start of the play.
#' \item{away_wp_post} - Estimated win probability for the away team at the start of the play.
#' \item{total_home_rush_wpa} - Cumulative total rushing WPA for the home team in
#' the game so far.
#' \item{total_away_rush_wpa} - Cumulative total rushing WPA for the away team in
#' the game so far.
#' \item{total_home_pass_wpa} - Cumulative total passing WPA for the home team in
#' the game so far.
#' \item{total_away_pass_wpa} - Cumulative total passing WPA for the away team in
#' the game so far.
#' \item{air_wpa} - WPA through the air (same logic as air_epa).
#' \item{yac_wpa} - WPA from yards after the catch (same logic as yac_epa).
#' \item{comp_air_wpa} - The air_wpa for completions only.
#' \item{comp_yac_wpa} - The yac_wpa for completions only.
#' \item{total_home_comp_air_wpa} - Cumulative total completions air WPA for the home team in
#' the game so far.
#' \item{total_away_comp_air_wpa} - Cumulative total completions air WPA for the away team in
#' the game so far.
#' \item{total_home_comp_yac_wpa} - Cumulative total completions yac WPA for the home team in
#' the game so far.
#' \item{total_away_comp_yac_wpa} - Cumulative total completions yac WPA for the away team in
#' the game so far.
#' \item{total_home_raw_air_wpa} - Cumulative total raw air WPA for the home team in
#' the game so far.
#' \item{total_away_raw_air_wpa} - Cumulative total raw air WPA for the away team in
#' the game so far.
#' \item{total_home_raw_yac_wpa} - Cumulative total raw yac WPA for the home team in
#' the game so far.
#' \item{total_away_raw_yac_wpa} - Cumulative total raw yac WPA for the away team in
#' the game so far.
#' \item{punt_blocked} - Binary indicator for if the punt was blocked.
#' \item{first_down_rush} - Binary indicator for if a running play converted the first down.
#' \item{first_down_pass} - Binary indicator for if a passing play converted the first down.
#' \item{first_down_penalty} - Binary indicator for if a penalty converted the first down.
#' \item{third_down_converted} - Binary indicator for if the first down was converted on third down.
#' \item{third_down_failed} - Binary indicator for if the posteam failed to convert first down on third down.
#' \item{fourth_down_converted} - Binary indicator for if the first down was converted on fourth down.
#' \item{fourth_down_failed} - Binary indicator for if the posteam failed to convert first down on fourth down.
#' \item{incomplete_pass} - Binary indicator for if the pass was incomplete.
#' \item{interception} - Binary indicator for if the pass was intercepted.
#' \item{touchback} - Binary indicator for if a touchback occurred on the play.
#' \item{punt_inside_twenty} - Binary indicator for if the punt ended inside the twenty yard line.
#' \item{punt_in_endzone} - Binary indicator for if the punt was in the endzone.
#' \item{punt_out_of_bounds} - Binary indicator for if the punt went out of bounds.
#' \item{punt_downed} - Binary indicator for if the punt was downed.
#' \item{punt_fair_catch} - Binary indicator for if the punt was caught with a fair catch.
#' \item{kickoff_inside_twenty} - Binary indicator for if the kickoff ended inside the twenty yard line.
#' \item{kickoff_in_endzone} - Binary indicator for if the kickoff was in the endzone.
#' \item{kickoff_out_of_bounds} - Binary indicator for if the kickoff went out of bounds.
#' \item{kickoff_downed} - Binary indicator for if the kickoff was downed.
#' \item{kickoff_fair_catch} - Binary indicator for if the kickoff was caught with a fair catch.
#' \item{fumble_forced} - Binary indicator for if the fumble was forced.
#' \item{fumble_not_forced} - Binary indicator for if the fumble was not forced.
#' \item{fumble_out_of_bounds} - Binary indicator for if the fumble went out of bounds.
#' \item{solo_tackle} - Binary indicator if the play had a solo tackle (could be multiple
#' due to fumbles).
#' \item{safety} - Binary indicator for whether or not a safety occurred.
#' \item{penalty} - Binary indicator for whether or not a penalty occurred.
#' \item{tackled_for_loss} - Binary indicator for whether or not a tackle for loss occurred.
#' \item{fumble_lost} - Binary indicator for if the fumble was lost.
#' \item{own_kickoff_recovery} - Binary indicator for if the kicking team recovered the
#' kickoff.
#' \item{own_kickoff_recovery_td} - Binary indicator for if the kicking team recovered
#' the kickoff and scored a TD.
#' \item{qb_hit} - Binary indicator if the QB was hit on the play.
#' \item{rush_attempt} - Binary indicator for if the play was a run.
#' \item{pass_attempt} - Binary indicator for if the play was a pass attempt (includes sacks).
#' \item{sack} - Binary indicator for if the play ended in a sack.
#' \item{touchdown} - Binary indicator for if the play resulted in a TD.
#' \item{pass_touchdown} - Binary indicator for if the play resulted in a passing TD.
#' \item{rush_touchdown} - Binary indicator for if the play resulted in a rushing TD.
#' \item{return_touchdown} - Binary indicator for if the play resulted in a return TD.
#' \item{extra_point_attempt} - Binary indicator for extra point attempt.
#' \item{two_point_attempt} - Binary indicator for two point conversion attempt.
#' \item{field_goal_attempt} - Binary indicator for field goal attempt.
#' \item{kickoff_attempt} - Binary indicator for kickoff.
#' \item{punt_attempt} - Binary indicator for punts.
#' \item{fumble} - Binary indicator for if a fumble occurred.
#' \item{complete_pass} - Binary indicator for if the pass was completed.
#' \item{assist_tackle} - Binary indicator for if an assist tackle occurred.
#' \item{lateral_reception} - Binary indicator for if a lateral occurred on the
#' reception.
#' \item{lateral_rush} - Binary indicator for if a lateral occurred on a run.
#' \item{lateral_return} - Binary indicator for if a lateral occurred on a return.
#' \item{lateral_recovery} - Binary indicator for if a lateral occurred on a 
#' fumble recovery.
#' \item{passer_player_id} - Unique identifier for the player that attempted the pass.
#' \item{passer_player_name} - String name for the player that attempted the pass.
#' \item{receiver_player_id} - Unique identifier for the receiver that was targeted on the pass.
#' \item{receiver_player_name} - String name for the targeted receiver.
#' \item{rusher_player_id} - Unique identifier for the player that attempted the run.
#' \item{rusher_player_name} - String name for the player that attempted the run.
#' \item{lateral_receiver_player_id} - Unique identifier for the player that received the lateral on a reception.
#' \item{lateral_receiver_player_name} - String name for the player that received the lateral on a reception.
#' \item{lateral_rusher_player_id} - Unique identifier for the player that received the lateral on a run.
#' \item{lateral_rusher_player_name} - String name for the player that received the lateral on a run.
#' \item{lateral_sack_player_id} - Unique identifier for the player that received the lateral on a sack.
#' \item{lateral_sack_player_name} - String name for the player that received the lateral on a sack.
#' \item{interception_player_id} - Unique identifier for the player that intercepted the pass.
#' \item{interception_player_name} - String name for the player that intercepted the pass.
#' \item{lateral_interception_player_id} - Unique indentifier for the player that received the lateral on an interception.
#' \item{lateral_interception_player_name} - String name for the player that received the lateral on an interception.
#' \item{punt_returner_player_id} - Unique identifier for the punt returner.
#' \item{punt_returner_player_name} - String name for the punt returner.
#' \item{lateral_punt_returner_player_id} - Unique identifier for the player that received the lateral on a punt return.
#' \item{lateral_punt_returner_player_name} - String name for the player that received the lateral on a punt return.
#' \item{kickoff_returner_player_name} - String name for the kickoff returner.
#' \item{kickoff_returner_player_id} - Unique identifier for the kickoff returner.
#' \item{lateral_kickoff_returner_player_id} - Unique identifier for the player that received the lateral on a kickoff return.
#' \item{lateral_kickoff_returner_player_name} - String name for the player that received the lateral on a kickoff return.
#' \item{punter_player_id} - Unique identifier for the punter. 
#' \item{punter_player_name} - String name for the punter.
#' \item{kicker_player_name} - String name for the kicker on FG or kickoff.
#' \item{kicker_player_id} - Unique identifier for the kicker on FG or kickoff.
#' \item{own_kickoff_recovery_player_id} - Unique identifier for the player that recovered their own kickoff.
#' \item{own_kickoff_recovery_player_name} - String name for the player that recovered their own kickoff.
#' \item{blocked_player_id} - Unique identifier for the player that blocked the punt or FG.
#' \item{blocked_player_name} - String name for the player that blocked the punt or FG.
#' \item{tackle_for_loss_1_player_id} - Unique identifier for one of the potential players with the tackle for loss.
#' \item{tackle_for_loss_1_player_name} - String name for one of the potential players with the tackle for loss.
#' \item{tackle_for_loss_2_player_id} - Unique identifier for one of the potential players with the tackle for loss.
#' \item{tackle_for_loss_2_player_name} - String name for one of the potential players with the tackle for loss.
#' \item{qb_hit_1_player_id} - Unique identifier for one of the potential players that hit the QB.
#' \item{qb_hit_1_player_name} - String name for one of the potential players that hit the QB.
#' \item{qb_hit_2_player_id} - Unique identifier for one of the potential players that hit the QB.
#' \item{qb_hit_2_player_name} - String name for one of the potential players that hit the QB.
#' \item{forced_fumble_player_1_team} - Team of one of the players with a forced fumble.
#' \item{forced_fumble_player_1_player_id} - Unique identifier of one of the players with a forced fumble.
#' \item{forced_fumble_player_1_player_name} - String name of one of the players with a forced fumble.
#' \item{forced_fumble_player_2_team} - Team of one of the players with a forced fumble.
#' \item{forced_fumble_player_2_player_id} - Unique identifier of one of the players with a forced fumble.
#' \item{forced_fumble_player_2_player_name} - String name of one of the players with a forced fumble.
#' \item{solo_tackle_1_team} - Team of one of the players with a solo tackle.
#' \item{solo_tackle_2_team} - Team of one of the players with a solo tackle.
#' \item{solo_tackle_1_player_id} - Unique identifier of one of the players with a solo tackle.
#' \item{solo_tackle_2_player_id} - Unique identifier of one of the players with a solo tackle.
#' \item{solo_tackle_1_player_name} - String name of one of the players with a solo tackle.
#' \item{solo_tackle_2_player_name} - String name of one of the players with a solo tackle.
#' \item{assist_tackle_1_player_id} - Unique identifier of one of the players with a tackle assist.
#' \item{assist_tackle_1_player_name} - String name of one of the players with a tackle assist.
#' \item{assist_tackle_1_team} - Team of one of the players with a tackle assist.
#' \item{assist_tackle_2_player_id} - Unique identifier of one of the players with a tackle assist.
#' \item{assist_tackle_2_player_name} - String name of one of the players with a tackle assist.
#' \item{assist_tackle_2_team} - Team of one of the players with a tackle assist.
#' \item{assist_tackle_3_player_id} - Unique identifier of one of the players with a tackle assist.
#' \item{assist_tackle_3_player_name} - String name of one of the players with a tackle assist.
#' \item{assist_tackle_3_team} - Team of one of the players with a tackle assist.
#' \item{assist_tackle_4_player_id} - Unique identifier of one of the players with a tackle assist.
#' \item{assist_tackle_4_player_name} - String name of one of the players with a tackle assist.
#' \item{assist_tackle_4_team} - Team of one of the players with a tackle assist.
#' \item{pass_defense_1_player_id} - Unique identifier of one of the players with a pass defense.
#' \item{pass_defense_1_player_name} - String name of one of the players with a pass defense.
#' \item{pass_defense_2_player_id} - Unique identifier of one of the players with a pass defense.
#' \item{pass_defense_2_player_name} - String name of one of the players with a pass defense.
#' \item{fumbled_1_team} - Team of one of the players with a fumble.
#' \item{fumbled_1_player_id} - Unique identifier of one of the players with a fumble.
#' \item{fumbled_1_player_name} - String name of one of the players with a fumble.
#' \item{fumbled_2_player_id} - Unique identifier of one of the players with a fumble.
#' \item{fumbled_2_player_name} - String name of one of the players with a fumble.
#' \item{fumbled_2_team} - Team of one of the players with a fumble.
#' \item{fumble_recovery_1_team} - Team of one of the players with a fumble recovery.
#' \item{fumble_recovery_1_yards} - Yards gained by one of the players with a fumble recovery.
#' \item{fumble_recovery_1_player_id} - Unique identifier of one of the players with a fumble recovery.
#' \item{fumble_recovery_1_player_name} - String name of one of the players with a fumble recovery.
#' \item{fumble_recovery_2_team} - Team of one of the players with a fumble recovery.
#' \item{fumble_recovery_2_yards} - Yards gained by one of the players with a fumble recovery.
#' \item{fumble_recovery_2_player_id} - Unique identifier of one of the players with a fumble recovery.
#' \item{fumble_recovery_2_player_name} - String name of one of the players with a fumble recovery.
#' \item{return_team} - String abbreviation of the return team.
#' \item{return_yards} - Yards gained by the return team.
#' \item{penalty_team} - String abbreviation of the team with the penalty.
#' \item{penalty_player_id} - Unique identifier for the player with the penalty.
#' \item{penalty_player_name} - String name for the player with the penalty.
#' \item{penalty_yards} - Yards gained (or lost) by the posteam from the penalty.
#' \item{replay_or_challenge} - Binary indicator for whether or not a replay or
#' challenge.
#' \item{replay_or_challenge_result} - String indicating the result of the 
#' replay or challenge.
#' \item{penalty_type} - String indicating the penalty type.
#' \item{defensive_two_point_attempt} - Binary indicator whether or not the defense
#' was able to have an attempt on a two point conversion, this results following
#' a turnover.
#' \item{defensive_two_point_conv} - Binary indicator whether or not the defense
#' successfully scored on the two point conversion.
#' \item{defensive_extra_point_attempt} - Binary indicator whether or not the 
#' defense was able to have an attempt on an extra point attempt, this results
#' following a blocked attempt that the defense recovers the ball.
#' \item{defensive_extra_point_conv} - Binary indicator whether or not the
#' defense successfully scored on an extra point attempt. 
#' }
#' @examples
#' # Scrape the play-by-play data for the 2017 Super Bowl by first getting all
#' # of the post-season game ids then use the required info to scrape the 
#' # play-by-play data (knowing that it's the last game):
#' playoff_game_ids_17 <- scrape_game_ids(2017, type = "post")
#' sb_17_id <- playoff_game_ids_17$game_id[nrow(playoff_game_ids_17)]
#' sb_17_pbp <- scrape_json_play_by_play(game_id = sb_17_id)
#' @export

scrape_json_play_by_play <- function(game_id, check_url = 1) {
  
  #Warn users about errors in the NFL data for certain games
  if (game_id == 2013092206)
    warning(
      "Due to an error in the NFL's API, plays from the 3rd drive of game 2013092206 will be missing from the play by play table."
    )
  
  if (game_id %in% c(2013112401, 2013120101))
    warning(
      paste0(
        "Due to an error in the NFL's API, the stats for game ",
        as.character(game_id),
        " are incomplete. Most yardage statistics cannot be determined and will be NA in the table."
      )
    )
  
  # First create the game's url:
  game_url <- create_game_json_url(game_id)
  
  # Next check to make sure it exists if check_url == 1:
  if (check_url == 1) {
    assertthat::assert_that(RCurl::url.exists(game_url),
                            msg = "The url for this game is not available yet, please try again later.")
  }
  
  # Make the character look-up vector for the NFL GSIS stat ids
  stat_names <- c("punt_blocked", "first_down_rush",
                  "first_down_pass", "first_down_penalty",
                  "third_down_converted", "third_down_failed",
                  "fourth_down_converted", "fourth_down_failed",
                  "rushing_yards", "rushing_yards_td",
                  "lateral_rushing_yards", "lateral_rushing_yards_td",
                  "incomplete_pass", "passing_yards", 
                  "passing_yards_td", "interception",
                  "sack_yards", "receiving_yards",
                  "receiving_yards_td", "lateral_receiving_yards",
                  "lateral_receiving_yards_td",
                  "interception_return_yards", 
                  "interception_return_yards_td",
                  "lateral_interception_return_yards",
                  "lateral_interception_return_yards_td",
                  "punting_yards", "punt_inside_twenty",
                  "punt_in_endzone", "punt_touchback_kicking",
                  "punt_return_yards", "punt_return_yards_td",
                  "lateral_punt_return_yards",
                  "lateral_punt_return_yards_td",
                  "punt_out_of_bounds", "punt_downed",
                  "punt_fair_catch", "punt_touchback_receiving",
                  "kickoff_yards", "kickoff_inside_twenty",
                  "kickoff_in_endzone", "kickoff_touchback_kicking",
                  "kickoff_return_yards", "kickoff_return_yards_td",
                  "lateral_kickoff_return_yards", 
                  "lateral_kickoff_return_yards_td",
                  "kickoff_out_of_bounds", "kickoff_fair_catch",
                  "kickoff_touchback_receiving", 
                  "fumble_forced", "fumble_not_forced",
                  "fumble_out_of_bounds", 
                  "own_fumble_recovery_yards",
                  "own_fumble_recovery_yards_td",
                  "lateral_own_fumble_recovery_yards",
                  "lateral_own_fumble_recovery_yards_td",
                  "opp_fumble_recovery_yards",
                  "opp_fumble_recovery_yards_td",
                  "lateral_opp_fumble_recovery_yards",
                  "lateral_opp_fumble_recovery_yards_td",
                  "miscellaneous_yards", 
                  "miscellaneous_yards_td",
                  "timeout", "field_goal_yards_missed",
                  "field_goal_yards_made",
                  "field_goal_yards_blocked",
                  "extra_point_good", "extra_point_failed",
                  "extra_point_blocked",
                  "two_point_rush_good", "two_point_rush_failed",
                  "two_point_pass_good", "two_point_pass_failed",
                  "solo_tackle", "assisted_tackle",
                  "tackle_assist", "solo_sack_yards",
                  "assist_sack_yards", "pass_defense_player",
                  "punt_blocked_player", "extra_point_blocked_player",
                  "field_goal_blocked_player", "safety_tackle",
                  "forced_fumble_player", "penalty_yards",
                  "tackled_for_loss", "extra_point_safety",
                  "two_point_rush_safety", "two_point_pass_safety",
                  "kickoff_downed", "lateral_sack_yards",
                  "two_point_pass_reception_good",
                  "two_point_pass_reception_failed",
                  "fumble_lost", "own_kickoff_recovery",
                  "own_kickoff_recovery_td",
                  "qb_hit", "air_yards_complete",
                  "air_yards_incomplete",
                  "yards_after_catch",
                  "targeted_receiver",
                  "tackle_for_loss_player",
                  "extra_point_aborted",
                  "tackle_for_loss_yards",
                  "kickoff_yard_length",
                  "two_point_return",
                  "defensive_two_point_attempt",
                  "defensive_two_point_conv",
                  "defensive_extra_point_attempt",
                  "defensive_extra_point_conv")
  stat_ids <- rep(NA, 406)  
  stat_ids[c(2:16, 19:64, 68:80, 82:89,
             91, 93, 95:96, 99, 100, 102:108,
             110:113, 115, 120, 301, 402, 
             410, 420, 403, 404, 405, 406)] <- stat_names
  
  
  # Next access the JSON feed for the game, catching any errors that potentially
  # occur due to the NFL's connection (or internet issues):
  game_json <- tryCatch(RJSONIO::fromJSON(RCurl::getURL(game_url, encoding = "gzip")),
                        error = function(cond) { 
                          message("Connection to NFL.com disrupted, please re-run code.")
                          message(paste("Here is the game's url:", game_url))
                          message("Here's the original error message:")
                          message(cond)
                          # Just return NA in case of error
                          return(NA)
                        }
  )
  
  # Store the number of drives in the game 
  n_drives <- length(game_json[[1]]$drives) - 1
  
  # Generate a dataframe for the basic play level information regarding the
  # field position, down, etc. that is the first nine elements for each play - 
  # information that doesn't directly involve any players (the if-else here 
  # is due to the one game that is not properly coded regarding its third drive):
  if (game_id != 2013092206) {
    game_pbp <- suppressWarnings(purrr::map_dfr(1:n_drives,
                                                function(x) {
                                                  cbind("drive" = x,
                                                        data.frame(do.call(
                                                          rbind,
                                                          game_json[[1]]$drives[[x]]$plays
                                                        ))[, c(1:9)])
                                                }))
  } else {
    game_pbp <- suppressWarnings(purrr::map_dfr(c(1, 2, 4:n_drives),
                                                function(x) {
                                                  cbind("drive" = x,
                                                        data.frame(do.call(
                                                          rbind,
                                                          game_json[[1]]$drives[[x]]$plays
                                                        ))[, c(1:9)])
                                                }))
  }
  
  # Unlist the variables accessed so far:
  game_pbp <- game_pbp %>%
    dplyr::mutate_all(function(x) lapply(x, function(y) ifelse(is.null(y), NA, y))) %>%
    dplyr::mutate_all(unlist)
  
  # First define a function that given a drive returns a dataframe
  # with a row summarizing all of the information about a play (going with
  # some top-down design here calling functions that are defined below),
  # resulting in a row for every play in the drive:
  get_drive_play_data <- function(drive) {
    
    # For each play in the drive return a data frame that has the play info,
    # including a column with the play id:
    suppressWarnings(purrr::map_dfr(c(1:length(drive$plays)),
                                    function(x) {
                                      get_play_data(drive$plays[[x]])
                                    })) %>%
      dplyr::mutate(play_id = names(drive$plays))
  }
  
  # Next define the get_play_data function that returns a single row summarising
  # the entire play from the player level data using the NFL GSIS stat ids:
  get_play_data <- function(play) {
    
    # Check the number of players in the play, if there is at least one then
    # go through the mapping process of stat ids, otherwise return a row full
    # of NA values: 
    
    if (length(play$players) > 0) {
      
      # Vector of player ids involved in the play:
      player_ids <- names(play$players)
      
      # Generate a data frame containing the player level information:
      play_player_data <- lapply(c(1:length(player_ids)),
                                 function(x) as.data.frame(do.call(rbind,
                                                                   play$players[[x]]))) %>% 
        dplyr::bind_rows() %>%
        dplyr::mutate_all(function(x) lapply(x, function(y) ifelse(is.null(y), NA, y))) %>%
        dplyr::mutate_all(unlist) %>%
        #suppressWarnings(purrr::flatten_dfr(play$players)) %>%
        #as.data.frame() %>%
        # Add the player ids:
        dplyr::mutate(player_id = purrr::map(c(1:length(player_ids)),
                                             function(x) {
                                               rep(player_ids[x],
                                                   length(play$players[[x]]))
                                             }) %>% purrr::flatten_chr(),
                      # Look-up the NFL stat names to use for each statId
                      nfl_stat =  stat_ids[statId]) %>%
        # Remove duplicates
        dplyr::distinct() %>%
        # Rename the team and player name columns:
        dplyr::rename(team = clubcode, player_name = playerName) %>%
        # Arrange by the sequence number:
        dplyr::arrange(sequence) %>%
        dplyr::mutate(yards = as.numeric(yards))
      
      # Now use this dataframe to generate a single row summarizing the NFL's API
      # data, but simplest way is to do this in pieces by breaking up the certain
      # NFL stats which are for: (1) indicators, (2) players, (3) teams, and 
      # (4) yards. All of the desired columns that are not in the play will be
      # just populated with NA (this will be found by comparing at the end 
      # which columns are there)
      
      # (1)
      # First the indicator variables, this will serve as a way of initializing
      # the row of play data to return. Start by making a vector of the indicator
      # statistics to check for:
      indicator_stats <- c("punt_blocked", "first_down_rush",
                           "first_down_pass", "first_down_penalty",
                           "third_down_converted", "third_down_failed",
                           "fourth_down_converted", "fourth_down_failed",
                           "incomplete_pass", "interception",
                           "punt_inside_twenty", "punt_in_endzone",
                           "punt_out_of_bounds", "punt_downed", 
                           "punt_fair_catch", "kickoff_inside_twenty",
                           "kickoff_in_endzone", "kickoff_out_of_bounds",
                           "kickoff_fair_catch", "fumble_forced",
                           "fumble_not_forced", "fumble_out_of_bounds",
                           "timeout", "field_goal_yards_missed",
                           "field_goal_yards_made",
                           "field_goal_yards_blocked",
                           "extra_point_good", "extra_point_failed",
                           "extra_point_blocked", "two_point_rush_good",
                           "two_point_rush_failed",
                           "two_point_pass_good", "two_point_pass_failed",
                           "solo_tackle", "safety_tackle", "penalty_yards",
                           "tackled_for_loss", "extra_point_safety",
                           "two_point_rush_safety", "two_point_pass_safety",
                           "kickoff_downed", "two_point_pass_reception_good",
                           "two_point_pass_reception_failed", 
                           "fumble_lost", "own_kickoff_recovery",
                           "own_kickoff_recovery_td", "qb_hit", "extra_point_aborted",
                           "two_point_return", "defensive_two_point_attempt",
                           "defensive_two_point_conv",
                           "defensive_extra_point_attempt",
                           "defensive_extra_point_conv")
      
      # Go through each of the indicator variables to see if the player level
      # dataset has it the NFL stat or not, initializing the play_data row:
      play_data <- purrr::map(indicator_stats,
                              function(x) {
                                dplyr::if_else(x %in% play_player_data$nfl_stat,
                                               1, 0)
                              }) %>%
        purrr::set_names(indicator_stats) %>%
        as.data.frame() %>%
        # Rename some of the variables that were used for indicators that also
        # had additional info not necessary in their name:
        dplyr::rename(safety = safety_tackle, penalty = penalty_yards,
                      field_goal_missed = field_goal_yards_missed,
                      field_goal_made = field_goal_yards_made,
                      field_goal_blocked = field_goal_yards_blocked) %>%
        # Now make indicators that are based on the presence of potentially
        # mutliple variables:
        dplyr::mutate(rush_attempt = dplyr::if_else(any(c("rushing_yards",
                                                          "rushing_yards_td",
                                                          "lateral_rushing_yards",
                                                          "lateral_rushing_yards_td",
                                                          "two_point_rush_good",
                                                          "two_point_rush_failed",
                                                          "two_point_rush_safety") %in%
                                                          play_player_data$nfl_stat),
                                                    1, 0),
                      pass_attempt = dplyr::if_else(any(c("passing_yards",
                                                          "passing_yards_td",
                                                          "incomplete_pass",
                                                          "interception",
                                                          "sack_yards",
                                                          "receiving_yards",
                                                          "receiving_yards_td", 
                                                          "lateral_receiving_yards",
                                                          "lateral_receiving_yards_td",
                                                          "interception_return_yards", 
                                                          "interception_return_yards_td",
                                                          "lateral_interception_return_yards",
                                                          "lateral_interception_return_yards_td",
                                                          "air_yards_complete",
                                                          "air_yards_incomplete",
                                                          "yards_after_catch",
                                                          "targeted_receiver",
                                                          "two_point_pass_good", 
                                                          "two_point_pass_failed",
                                                          "two_point_pass_safety",
                                                          "two_point_pass_reception_good",
                                                          "two_point_pass_reception_failed") %in%
                                                          play_player_data$nfl_stat),
                                                    1, 0),
                      sack = dplyr::if_else(any(c("sack_yards", "solo_sack_yards",
                                                  "assist_sack_yards") %in%
                                                  play_player_data$nfl_stat), 1, 0),
                      touchdown = dplyr::if_else(any(c("rushing_yards_td",
                                                       "lateral_rushing_yards_td",
                                                       "passing_yards_td",
                                                       "receiving_yards_td",
                                                       "lateral_receiving_yards_td",
                                                       "interception_return_yards_td",
                                                       "lateral_interception_return_yards_td",
                                                       "kickoff_return_yards_td",
                                                       "lateral_kickoff_return_yards_td",
                                                       "own_fumble_recovery_yards_td",
                                                       "lateral_own_fumble_recovery_yards_td",
                                                       "opp_fumble_recovery_yards_td",
                                                       "lateral_opp_fumble_recovery_yards_td",
                                                       "miscellaneous_yards_td",
                                                       "own_kickoff_recovery_td",
                                                       "punt_return_yards_td",
                                                       "lateral_punt_return_yards_td") %in%
                                                       play_player_data$nfl_stat),
                                                 1, 0),
                      pass_touchdown = dplyr::if_else(any(c("passing_yards_td",
                                                            "receiving_yards_td",
                                                            "lateral_receiving_yards_td") %in%
                                                            play_player_data$nfl_stat),
                                                      1, 0),
                      rush_touchdown = dplyr::if_else(any(c("rushing_yards_td",
                                                            "lateral_rushing_yards_td") %in%
                                                            play_player_data$nfl_stat),
                                                      1, 0),
                      return_touchdown = dplyr::if_else(any(c("interception_return_yards_td",
                                                              "lateral_interception_return_yards_td",
                                                              "kickoff_return_yards_td",
                                                              "lateral_kickoff_return_yards_td",
                                                              "punt_return_yards_td",
                                                              "lateral_punt_return_yards_td") %in%
                                                              play_player_data$nfl_stat),
                                                        1, 0),
                      extra_point_attempt = dplyr::if_else(any(c("extra_point_good",
                                                                 "extra_point_failed",
                                                                 "extra_point_blocked",
                                                                 "extra_point_safety",
                                                                 "extra_point_aborted") %in%
                                                                 play_player_data$nfl_stat),
                                                           1, 0),
                      two_point_attempt = dplyr::if_else(any(c("two_point_rush_good",
                                                               "two_point_rush_failed",
                                                               "two_point_pass_good", 
                                                               "two_point_pass_failed",
                                                               "two_point_rush_safety", 
                                                               "two_point_pass_safety",
                                                               "two_point_pass_reception_good",
                                                               "two_point_pass_reception_failed",
                                                               "two_point_return") %in%
                                                               play_player_data$nfl_stat),
                                                         1, 0),
                      field_goal_attempt = dplyr::if_else(any(c("field_goal_yards_missed",
                                                                "field_goal_yards_made",
                                                                "field_goal_yards_blocked",
                                                                "field_goal_blocked_player") %in%
                                                                play_player_data$nfl_stat),
                                                          1, 0),
                      kickoff_attempt = dplyr::if_else(any(c("kickoff_yards", 
                                                             "kickoff_inside_twenty",
                                                             "kickoff_in_endzone", 
                                                             "kickoff_touchback_kicking",
                                                             "kickoff_return_yards", 
                                                             "kickoff_return_yards_td",
                                                             "lateral_kickoff_return_yards", 
                                                             "lateral_kickoff_return_yards_td",
                                                             "kickoff_out_of_bounds", 
                                                             "kickoff_fair_catch",
                                                             "kickoff_touchback_receiving",
                                                             "kickoff_downed",
                                                             "own_kickoff_recovery",
                                                             "own_kickoff_recovery_td",
                                                             "kickoff_yard_length") %in%
                                                             play_player_data$nfl_stat),
                                                       1, 0),
                      punt_attempt = dplyr::if_else(any(c("punt_blocked", 
                                                          "punting_yards", 
                                                          "punt_inside_twenty",
                                                          "punt_in_endzone", 
                                                          "punt_touchback_kicking",
                                                          "punt_return_yards",
                                                          "punt_return_yards_td",
                                                          "lateral_punt_return_yards",
                                                          "lateral_punt_return_yards_td",
                                                          "punt_out_of_bounds", 
                                                          "punt_downed",
                                                          "punt_fair_catch", 
                                                          "punt_touchback_receiving",
                                                          "punt_blocked_player") %in%
                                                          play_player_data$nfl_stat), 
                                                    1, 0),
                      fumble = dplyr::if_else(any(c("fumble_forced", "fumble_not_forced",
                                                    "fumble_out_of_bounds", 
                                                    "own_fumble_recovery_yards",
                                                    "own_fumble_recovery_yards_td",
                                                    "lateral_own_fumble_recovery_yards",
                                                    "lateral_own_fumble_recovery_yards_td",
                                                    "opp_fumble_recovery_yards",
                                                    "opp_fumble_recovery_yards_td",
                                                    "lateral_opp_fumble_recovery_yards",
                                                    "lateral_opp_fumble_recovery_yards_td",
                                                    "forced_fumble_player",
                                                    "fumble_lost") %in%
                                                    play_player_data$nfl_stat),
                                              1, 0),
                      complete_pass = dplyr::if_else(any(c("passing_yards", 
                                                           "passing_yards_td",
                                                           "receiving_yards",
                                                           "receiving_yards_td", 
                                                           "lateral_receiving_yards",
                                                           "lateral_receiving_yards_td",
                                                           "air_yards_complete",
                                                           "yards_after_catch") %in%
                                                           play_player_data$nfl_stat),
                                                     1, 0),
                      assist_tackle = dplyr::if_else(any(c("assisted_tackle",
                                                           "tackle_assist",
                                                           "assist_sack_yards") %in%
                                                           play_player_data$nfl_stat),
                                                     1, 0),
                      lateral_reception = dplyr::if_else(any(c("lateral_receiving_yards",
                                                               "lateral_receiving_yards_td") %in%
                                                               play_player_data$nfl_stat),
                                                         1, 0),
                      lateral_rush = dplyr::if_else(any(c("lateral_rushing_yards", 
                                                          "lateral_rushing_yards_td") %in%
                                                          play_player_data$nfl_stat),
                                                    1, 0),
                      lateral_return = dplyr::if_else(any(c("lateral_interception_return_yards",
                                                            "lateral_interception_return_yards_td",
                                                            "lateral_punt_return_yards",
                                                            "lateral_punt_return_yards_td",
                                                            "lateral_kickoff_return_yards", 
                                                            "lateral_kickoff_return_yards_td") %in%
                                                            play_player_data$nfl_stat),
                                                      1, 0),
                      lateral_recovery = dplyr::if_else(any(c("lateral_own_fumble_recovery_yards",
                                                              "lateral_own_fumble_recovery_yards_td",
                                                              "lateral_opp_fumble_recovery_yards",
                                                              "lateral_opp_fumble_recovery_yards_td") %in%
                                                              play_player_data$nfl_stat),
                                                        1, 0))
      
      # (2)
      # Next thing is to get the players involved in the play in various ways.
      # One thing to keep in mind is that on turnovers you can have players on
      # offense then make the tackle. This will happen on fumbles when a tackle
      # is made, forcing a fumble, then the team that fumbled will have tackle(s)
      # as well. To address this issue, there will just be numbered solo tackle,
      # assist tackle, forced fumble players along with columns with their 
      # respective teams. This is the simplest solution for analysis.
      
      # First get the passer information, checking to see if there was a 
      # pass attempt or not:
      if (!(any(c("passing_yards",
                  "passing_yards_td",
                  "incomplete_pass",
                  "interception",
                  "sack_yards",
                  "air_yards_complete",
                  "air_yards_incomplete",
                  "two_point_pass_good", 
                  "two_point_pass_failed",
                  "two_point_pass_safety") %in% play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(passer_player_id = NA,
                        passer_player_name = NA)
      } else {
        # Get the player info by looking at the passing rows only, with
        # distinct names and ids - grabbing only the first one (in case
        # there's multiple entered):
        passing_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("passing_yards",
                                        "passing_yards_td",
                                        "incomplete_pass",
                                        "interception",
                                        "sack_yards",
                                        "air_yards_complete",
                                        "air_yards_incomplete",
                                        "two_point_pass_good", 
                                        "two_point_pass_failed",
                                        "two_point_pass_safety")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(passer_player_id = player_id,
                        passer_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(passing_player_data)
        rm(passing_player_data)
      }
      
      # Next get the receiver information:
      if (!(any(c("receiving_yards",
                  "receiving_yards_td", 
                  "yards_after_catch",
                  "targeted_receiver",
                  "two_point_pass_reception_good",
                  "two_point_pass_reception_failed") %in% play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(receiver_player_id = NA,
                        receiver_player_name = NA)
      } else {
        # Get the player info by looking at the receiving rows only, with
        # distinct names and ids - grabbing only the first one (in case
        # there's multiple entered):
        receiving_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("receiving_yards",
                                        "receiving_yards_td", 
                                        "yards_after_catch",
                                        "targeted_receiver",
                                        "two_point_pass_reception_good",
                                        "two_point_pass_reception_failed")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(receiver_player_id = player_id,
                        receiver_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(receiving_player_data)
        rm(receiving_player_data)
      }
      
      # Next the rusher information:
      if (!(any(c("rushing_yards",
                  "rushing_yards_td",
                  "two_point_rush_good",
                  "two_point_rush_failed",
                  "two_point_rush_safety") %in% play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(rusher_player_id = NA,
                        rusher_player_name = NA)
      } else {
        # Get the player info by looking at the rushing rows only, with
        # distinct names and ids - grabbing only the first one (in case
        # there's multiple entered):
        rushing_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("rushing_yards",
                                        "rushing_yards_td",
                                        "two_point_rush_good",
                                        "two_point_rush_failed",
                                        "two_point_rush_safety")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(rusher_player_id = player_id,
                        rusher_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(rushing_player_data)
        rm(rushing_player_data)
      }
      
      # Next the lateral receiver:
      if (!(any(c("lateral_receiving_yards", "lateral_receiving_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(lateral_receiver_player_id = NA,
                        lateral_receiver_player_name = NA)
      } else {
        # Get the player info by looking at the lateral receiving rows only, with
        # distinct names and ids - grabbing only the first one (in case
        # there's multiple entered):
        lateral_receiving_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("lateral_receiving_yards",
                                        "lateral_receiving_yards_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(lateral_receiver_player_id = player_id,
                        lateral_receiver_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(lateral_receiving_player_data)
        rm(lateral_receiving_player_data)
      }
      
      # Next the lateral rusher information:
      if (!(any(c("lateral_rushing_yards", "lateral_rushing_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(lateral_rusher_player_id = NA,
                        lateral_rusher_player_name = NA)
      } else {
        # Get the player info by looking at the lateral rushing rows only, with
        # distinct names and ids - grabbing only the first one (in case
        # there's multiple entered):
        lateral_rushing_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("lateral_rushing_yards",
                                        "lateral_rushing_yards_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(lateral_rusher_player_id = player_id,
                        lateral_rusher_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(lateral_rushing_player_data)
        rm(lateral_rushing_player_data)
      }
      
      # Lateral sack player information:
      # Next the lateral rusher information:
      if (!("lateral_sack_yards" %in% play_player_data$nfl_stat)) {
        play_data <- play_data %>%
          dplyr::mutate(lateral_sack_player_id = NA,
                        lateral_sack_player_name = NA)
      } else {
        # Get the player info by looking at the lateral sack rows only, with
        # distinct names and ids - grabbing only the first one (in case
        # there's multiple entered):
        lateral_sack_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("lateral_sack_yards")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(lateral_sack_player_id = player_id,
                        lateral_sack_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(lateral_sack_player_data)
        rm(lateral_sack_player_data)
      }
      
      # Interception player information:
      if (!(any(c("interception_return_yards", "interception_return_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(interception_player_id = NA,
                        interception_player_name = NA)
      } else {
        # Get the player info by looking at the interception return rows only:
        interception_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("interception_return_yards",
                                        "interception_return_yards_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(interception_player_id = player_id,
                        interception_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(interception_player_data)
        rm(interception_player_data)
      }
      
      # Lateral interception player information:
      if (!(any(c("lateral_interception_return_yards", "lateral_interception_return_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(lateral_interception_player_id = NA,
                        lateral_interception_player_name = NA)
      } else {
        # Get the player info by looking at the lateral interception return rows only:
        lateral_interception_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("lateral_interception_return_yards",
                                        "lateral_interception_return_yards_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(lateral_interception_player_id = player_id,
                        lateral_interception_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(lateral_interception_player_data)
        rm(lateral_interception_player_data)
      }
      
      # Punt returner player information:
      if (!(any(c("punt_return_yards", "punt_return_yards_td",
                  "punt_fair_catch") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(punt_returner_player_id = NA,
                        punt_returner_player_name = NA)
      } else {
        # Get the player info by looking at the punt return rows only:
        punt_returner_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("punt_return_yards", "punt_return_yards_td",
                                        "punt_fair_catch")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(punt_returner_player_id = player_id,
                        punt_returner_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(punt_returner_player_data)
        rm(punt_returner_player_data)
      }
      
      # Lateral punt returner player information:
      if (!(any(c("lateral_punt_return_yards", "lateral_punt_return_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(lateral_punt_returner_player_id = NA,
                        lateral_punt_returner_player_name = NA)
      } else {
        # Get the player info by looking at the lateral punt return rows only:
        lateral_punt_returner_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("lateral_punt_return_yards",
                                        "lateral_punt_return_yards_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(lateral_punt_returner_player_id = player_id,
                        lateral_punt_returner_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(lateral_punt_returner_player_data)
        rm(lateral_punt_returner_player_data)
      }
      
      # Kickoff returner player information:
      if (!(any(c("kickoff_return_yards", "kickoff_return_yards_td",
                  "kickoff_fair_catch") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(kickoff_returner_player_id = NA,
                        kickoff_returner_player_name = NA)
      } else {
        # Get the player info by looking at the kickoff return rows only:
        kickoff_returner_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("kickoff_return_yards", "kickoff_return_yards_td",
                                        "kickoff_fair_catch")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(kickoff_returner_player_id = player_id,
                        kickoff_returner_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(kickoff_returner_player_data)
        rm(kickoff_returner_player_data)
      }
      
      # Lateral kickoff returner player information:
      if (!(any(c("lateral_kickoff_return_yards", "lateral_kickoff_return_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(lateral_kickoff_returner_player_id = NA,
                        lateral_kickoff_returner_player_name = NA)
      } else {
        # Get the player info by looking at the lateral kickoff return rows only:
        lateral_kickoff_returner_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("lateral_kickoff_return_yards",
                                        "lateral_kickoff_return_yards_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(lateral_kickoff_returner_player_id = player_id,
                        lateral_kickoff_returner_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(lateral_kickoff_returner_player_data)
        rm(lateral_kickoff_returner_player_data)
      }
      
      # Punter information:
      if (!(any(c("punting_yards", "punt_inside_twenty",
                  "punt_in_endzone", "punt_touchback_kicking",
                  "punt_out_of_bounds") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(punter_player_id = NA,
                        punter_player_name = NA)
      } else {
        # Get the player info by looking at the punt rows only:
        punter_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("punting_yards", "punt_inside_twenty",
                                        "punt_in_endzone", "punt_touchback_kicking",
                                        "punt_out_of_bounds")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(punter_player_id = player_id,
                        punter_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(punter_player_data)
        rm(punter_player_data)
      }
      
      # Kicker information:
      if (!(any(c("kickoff_yards", "kickoff_inside_twenty",
                  "kickoff_in_endzone", "kickoff_touchback_kicking",
                  "kickoff_out_of_bounds", "field_goal_yards_missed",
                  "field_goal_yards_made",
                  "field_goal_yards_blocked",
                  "extra_point_good", "extra_point_failed",
                  "extra_point_blocked", "kickoff_yard_length") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(kicker_player_id = NA,
                        kicker_player_name = NA)
      } else {
        # Get the player info by looking at the kicking rows only:
        kicker_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("kickoff_yards", "kickoff_inside_twenty",
                                        "kickoff_in_endzone", "kickoff_touchback_kicking",
                                        "kickoff_out_of_bounds", "field_goal_yards_missed",
                                        "field_goal_yards_made",
                                        "field_goal_yards_blocked",
                                        "extra_point_good", "extra_point_failed",
                                        "extra_point_blocked", "kickoff_yard_length")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(kicker_player_id = player_id,
                        kicker_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(kicker_player_data)
        rm(kicker_player_data)
      }
      
      # Own kickoff recovery player information:
      if (!(any(c("own_kickoff_recovery",
                  "own_kickoff_recovery_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(own_kickoff_recovery_player_id = NA,
                        own_kickoff_recovery_player_name = NA)
      } else {
        # Get the player info by looking at the own kickoff recovery rows only:
        own_kickoff_recovery_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("own_kickoff_recovery",
                                        "own_kickoff_recovery_td")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(own_kickoff_recovery_player_id = player_id,
                        own_kickoff_recovery_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(own_kickoff_recovery_player_data)
        rm(own_kickoff_recovery_player_data)
      }
      
      
      
      # Blocking player information:
      if (!(any(c("punt_blocked_player", "extra_point_blocked_player",
                  "field_goal_blocked_player") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(blocked_player_id = NA,
                        blocked_player_name = NA)
      } else {
        # Get the player info by looking at the blocked rows only:
        blocked_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("punt_blocked_player", "extra_point_blocked_player",
                                        "field_goal_blocked_player")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(blocked_player_id = player_id,
                        blocked_player_name = player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(blocked_player_data)
        rm(blocked_player_data)
      }
      
      # Tackle for loss information (this is a tricky one):
      if (!(any(c("tackle_for_loss_player") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(tackle_for_loss_1_player_id = NA,
                        tackle_for_loss_1_player_name = NA,
                        tackle_for_loss_2_player_id = NA,
                        tackle_for_loss_2_player_name = NA)
      } else {
        # Get the player info by looking at the tackle for loss return rows only,
        # allowing for two to account multiple players involved
        tackle_for_loss_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("tackle_for_loss_player")) %>%
          dplyr::select(team, player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("tackle_for_loss", 1:dplyr::n(), sep = "_"))
        
        # Now make two separate rows: containing the player_id and player_name:
        tackle_for_loss_id_row <- tackle_for_loss_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        tackle_for_loss_name_row <- tackle_for_loss_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(tackle_for_loss_id_row,
                           tackle_for_loss_name_row)
        rm(tackle_for_loss_player_data,
           tackle_for_loss_id_row,
           tackle_for_loss_name_row)
        
        # Check to see if there any of the possible numbers for solo tackles
        # is missing, just make NA:
        if (!(all(c("tackle_for_loss_1_player_id",
                    "tackle_for_loss_1_player_name",
                    "tackle_for_loss_2_player_id",
                    "tackle_for_loss_2_player_name") %in% colnames(play_data)))) {
          
          tackle_for_loss_cols <- c("tackle_for_loss_1_player_id",
                                    "tackle_for_loss_1_player_name",
                                    "tackle_for_loss_2_player_id",
                                    "tackle_for_loss_2_player_name")
          
          # Find which ones are missing:
          missing_tackle_for_loss <- which(!(tackle_for_loss_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_tackle_for_loss_row <- as.list(rep(NA, length(missing_tackle_for_loss))) %>%
            purrr::set_names(tackle_for_loss_cols[missing_tackle_for_loss]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data (this should be a pretty flexible way to handle
          # having a various numbers of tacklers):
          play_data <- play_data %>%
            dplyr::bind_cols(missing_tackle_for_loss_row)
          rm(missing_tackle_for_loss_row)
        }
      }
      
      # Same thing for QB hit players:
      if (!(any(c("qb_hit") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(qb_hit_1_player_id = NA,
                        qb_hit_1_player_name = NA,
                        qb_hit_2_player_id = NA,
                        qb_hit_2_player_name = NA)
      } else {
        # Get the player info by looking at the qb hit rows only,
        # allowing for two to account multiple players involved
        qb_hit_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("qb_hit")) %>%
          dplyr::select(team, player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("qb_hit", 1:dplyr::n(), sep = "_"))
        
        # Now make two separate rows: containing the player_id and player_name:
        qb_hit_id_row <- qb_hit_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        qb_hit_name_row <- qb_hit_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(qb_hit_id_row,
                           qb_hit_name_row)
        rm(qb_hit_player_data,
           qb_hit_id_row,
           qb_hit_name_row)
        
        # Check to see if there any of the possible numbers for qb_hit
        # is missing, just make NA:
        if (!(all(c("qb_hit_1_player_id",
                    "qb_hit_1_player_name",
                    "qb_hit_2_player_id",
                    "qb_hit_2_player_name") %in% colnames(play_data)))) {
          
          qb_hit_cols <- c("qb_hit_1_player_id",
                           "qb_hit_1_player_name",
                           "qb_hit_2_player_id",
                           "qb_hit_2_player_name")
          
          # Find which ones are missing:
          missing_qb_hit <- which(!(qb_hit_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_qb_hit_row <- as.list(rep(NA, length(missing_qb_hit))) %>%
            purrr::set_names(qb_hit_cols[missing_qb_hit]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data (this should be a pretty flexible way to handle
          # having a various numbers of tacklers):
          play_data <- play_data %>%
            dplyr::bind_cols(missing_qb_hit_row)
          rm(missing_qb_hit_row)
        }
      }
      
      
      # Same logic as well for forced fumble player:
      if (!(any(c("forced_fumble_player") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(forced_fumble_player_1_player_id = NA,
                        forced_fumble_player_1_player_name = NA,
                        forced_fumble_player_1_team = NA,
                        forced_fumble_player_2_player_id = NA,
                        forced_fumble_player_2_player_name = NA,
                        forced_fumble_player_2_team = NA)
      } else {
        # Get the player info by looking at the forced_fumble_player rows only,
        # allowing for two to account multiple players involved
        forced_fumble_player_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("forced_fumble_player")) %>%
          dplyr::select(team, player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("forced_fumble_player", 1:dplyr::n(), sep = "_"))
        
        # Now make 3 separate rows: containing the team, player_id, and player_name:
        forced_fumble_player_team_row <- forced_fumble_player_player_data %>%
          dplyr::select(position, team) %>%
          dplyr::mutate(position = paste(position, "team", sep = "_")) %>%
          tidyr::spread(position, team)
        
        forced_fumble_player_id_row <- forced_fumble_player_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        forced_fumble_player_name_row <- forced_fumble_player_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(forced_fumble_player_id_row,
                           forced_fumble_player_name_row,
                           forced_fumble_player_team_row)
        rm(forced_fumble_player_player_data,
           forced_fumble_player_id_row,
           forced_fumble_player_name_row,
           forced_fumble_player_team_row)
        
        # Check to see if there any of the possible numbers for forced_fumble_player
        # is missing, just make NA:
        if (!(all(c("forced_fumble_player_1_player_id",
                    "forced_fumble_player_1_player_name",
                    "forced_fumble_player_1_team",
                    "forced_fumble_player_2_player_id",
                    "forced_fumble_player_2_player_name",
                    "forced_fumble_player_2_team") %in% colnames(play_data)))) {
          
          forced_fumble_player_cols <- c("forced_fumble_player_1_player_id",
                                         "forced_fumble_player_1_player_name",
                                         "forced_fumble_player_1_team",
                                         "forced_fumble_player_2_player_id",
                                         "forced_fumble_player_2_player_name",
                                         "forced_fumble_player_2_team")
          
          # Find which ones are missing:
          missing_forced_fumble_player <- which(!(forced_fumble_player_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_forced_fumble_player_row <- as.list(rep(NA, length(missing_forced_fumble_player))) %>%
            purrr::set_names(forced_fumble_player_cols[missing_forced_fumble_player]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data (this should be a pretty flexible way to handle
          # having a various numbers of tacklers):
          play_data <- play_data %>%
            dplyr::bind_cols(missing_forced_fumble_player_row)
          rm(missing_forced_fumble_player_row)
        }
      }
      
      # Solo tackler information (this is a tricky one):
      if (!(any(c("solo_tackle") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(solo_tackle_1_player_id = NA,
                        solo_tackle_1_player_name = NA,
                        solo_tackle_1_team = NA,
                        solo_tackle_2_player_id = NA,
                        solo_tackle_2_player_name = NA,
                        solo_tackle_2_team = NA)
      } else {
        # Get the player info by looking at the solo tackle return rows only,
        # allowing for two to account for turnovers:
        solo_tackle_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("solo_tackle")) %>%
          dplyr::select(team, player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("solo_tackle", 1:dplyr::n(), sep = "_"))
        
        # Now make three separate rows: containing the team, player_id, and
        # player_name:
        solo_tackle_team_row <- solo_tackle_player_data %>%
          dplyr::select(position, team) %>%
          dplyr::mutate(position = paste(position, "team", sep = "_")) %>%
          tidyr::spread(position, team)
        
        solo_tackle_id_row <- solo_tackle_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        solo_tackle_name_row <- solo_tackle_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(solo_tackle_team_row, solo_tackle_id_row,
                           solo_tackle_name_row)
        rm(solo_tackle_player_data,
           solo_tackle_team_row,
           solo_tackle_id_row,
           solo_tackle_name_row)
        
        # Check to see if there any of the possible numbers for solo tackles
        # is missing, just make NA:
        if (!(all(c("solo_tackle_1_player_id",
                    "solo_tackle_1_player_name",
                    "solo_tackle_1_team",
                    "solo_tackle_2_player_id",
                    "solo_tackle_2_player_name",
                    "solo_tackle_2_team") %in% colnames(play_data)))) {
          
          solo_tackle_cols <- c("solo_tackle_1_player_id",
                                "solo_tackle_1_player_name",
                                "solo_tackle_1_team",
                                "solo_tackle_2_player_id",
                                "solo_tackle_2_player_name",
                                "solo_tackle_2_team")
          # Find which ones are missing:
          missing_solo_tackles <- which(!(solo_tackle_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_solo_tackles_row <- as.list(rep(NA, length(missing_solo_tackles))) %>%
            purrr::set_names(solo_tackle_cols[missing_solo_tackles]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data (this should be a pretty flexible way to handle
          # having a various numbers of tacklers due to turnovers):
          play_data <- play_data %>%
            dplyr::bind_cols(missing_solo_tackles_row)
          rm(missing_solo_tackles_row)
        }
      }
      
      # Next do the same thing for assist tackles except this will go up to four:
      if (!(any(c("tackle_assist", "assisted_tackle") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(assist_tackle_1_player_id = NA,
                        assist_tackle_1_player_name = NA,
                        assist_tackle_1_team = NA,
                        assist_tackle_2_player_id = NA,
                        assist_tackle_2_player_name = NA,
                        assist_tackle_2_team = NA,
                        assist_tackle_3_player_id = NA,
                        assist_tackle_3_player_name = NA,
                        assist_tackle_3_team = NA,
                        assist_tackle_4_player_id = NA,
                        assist_tackle_4_player_name = NA,
                        assist_tackle_4_team = NA)
      } else {
        # Get the player info by looking at the assist tackle return rows only,
        # allowing for four to account for turnovers:
        assist_tackle_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("tackle_assist", "assisted_tackle")) %>%
          dplyr::select(team, player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:4) %>%
          dplyr::mutate(position = paste("assist_tackle", 1:dplyr::n(), sep = "_"))
        
        # Now make three separate rows: containing the team, player_id, and
        # player_name:
        assist_tackle_team_row <- assist_tackle_player_data %>%
          dplyr::select(position, team) %>%
          dplyr::mutate(position = paste(position, "team", sep = "_")) %>%
          tidyr::spread(position, team)
        
        assist_tackle_id_row <- assist_tackle_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        assist_tackle_name_row <- assist_tackle_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(assist_tackle_team_row, assist_tackle_id_row,
                           assist_tackle_name_row)
        rm(assist_tackle_player_data,
           assist_tackle_name_row,
           assist_tackle_id_row,
           assist_tackle_team_row)
        
        # Check to see if there any of the possible numbers for assist tackles
        # are missing, just make NA:
        if (!(all(c("assist_tackle_1_player_id",
                    "assist_tackle_1_player_name",
                    "assist_tackle_1_team",
                    "assist_tackle_2_player_id",
                    "assist_tackle_2_player_name",
                    "assist_tackle_2_team",
                    "assist_tackle_3_player_id",
                    "assist_tackle_3_player_name",
                    "assist_tackle_3_team",
                    "assist_tackle_4_player_id",
                    "assist_tackle_4_player_name",
                    "assist_tackle_4_team") %in% colnames(play_data)))) {
          
          assist_tackle_cols <- c("assist_tackle_1_player_id",
                                  "assist_tackle_1_player_name",
                                  "assist_tackle_1_team",
                                  "assist_tackle_2_player_id",
                                  "assist_tackle_2_player_name",
                                  "assist_tackle_2_team",
                                  "assist_tackle_3_player_id",
                                  "assist_tackle_3_player_name",
                                  "assist_tackle_3_team",
                                  "assist_tackle_4_player_id",
                                  "assist_tackle_4_player_name",
                                  "assist_tackle_4_team")
          # Find which ones are missing:
          missing_assist_tackles <- which(!(assist_tackle_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_assist_tackles_row <- as.list(rep(NA, length(missing_assist_tackles))) %>%
            purrr::set_names(assist_tackle_cols[missing_assist_tackles]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data (this should be a pretty flexible way to handle
          # having a various numbers of tacklers due to turnovers):
          play_data <- play_data %>%
            dplyr::bind_cols(missing_assist_tackles_row)
          rm(missing_assist_tackles_row)
        }
      }
      
      # Same style for pass defense player except do not need team since this
      # will only be defense:
      if (!(any(c("pass_defense_player") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(pass_defense_1_player_id = NA,
                        pass_defense_1_player_name = NA,
                        pass_defense_2_player_id = NA,
                        pass_defense_2_player_name = NA)
      } else {
        # Get the player info by looking at the solo tackle return rows only,
        # allowing for two to account for turnovers:
        pass_defense_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("pass_defense_player")) %>%
          dplyr::select(player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("pass_defense", 1:dplyr::n(), sep = "_"))
        
        # Now make two separate rows: containing the player_id and player_name:
        pass_defense_id_row <- pass_defense_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        pass_defense_name_row <- pass_defense_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(pass_defense_id_row,
                           pass_defense_name_row)
        rm(pass_defense_player_data,
           pass_defense_id_row,
           pass_defense_name_row)
        
        # Check to see if there any of the possible numbers for solo tackles
        # is missing, just make NA:
        if (!(all(c("pass_defense_1_player_id",
                    "pass_defense_1_player_name",
                    "pass_defense_2_player_id",
                    "pass_defense_2_player_name") %in% colnames(play_data)))) {
          
          pass_defense_cols <- c("pass_defense_1_player_id",
                                 "pass_defense_1_player_name",
                                 "pass_defense_2_player_id",
                                 "pass_defense_2_player_name")
          # Find which ones are missing:
          missing_pass_defense <- which(!(pass_defense_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_pass_defense_row <- as.list(rep(NA, length(missing_pass_defense))) %>%
            purrr::set_names(pass_defense_cols[missing_pass_defense]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Flexible way to handle either one or two players:
          play_data <- play_data %>%
            dplyr::bind_cols(missing_pass_defense_row)
          rm(missing_pass_defense_row)
        }
      }
      
      # Similar for players that fumbled since there can be multiple fumbles
      # on a play by both teams, will limit to two here since three is really rare
      if (!(any(c("fumble_forced", "fumble_not_forced",
                  "fumble_out_of_bounds", "fumble_lost") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(fumbled_1_player_id = NA,
                        fumbled_1_player_name = NA,
                        fumbled_1_team = NA,
                        fumbled_2_player_id = NA,
                        fumbled_2_player_name = NA,
                        fumbled_2_team = NA)
      } else {
        # Get the player info by looking at the fumble rows only,
        # allowing for two:
        fumbled_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("fumble_forced", "fumble_not_forced",
                                        "fumble_out_of_bounds", "fumble_lost")) %>%
          dplyr::select(team, player_name, player_id) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("fumbled", 1:dplyr::n(), sep = "_"))
        
        # Now make three separate rows: containing the team, player_id, and
        # player_name:
        fumbled_team_row <- fumbled_player_data %>%
          dplyr::select(position, team) %>%
          dplyr::mutate(position = paste(position, "team", sep = "_")) %>%
          tidyr::spread(position, team)
        
        fumbled_id_row <- fumbled_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        fumbled_name_row <- fumbled_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(fumbled_team_row, fumbled_id_row,
                           fumbled_name_row)
        rm(fumbled_player_data,
           fumbled_team_row,
           fumbled_id_row,
           fumbled_name_row)
        
        # Check to see if there any of the possible numbers for fumbled players
        # is missing, just make NA:
        if (!(all(c("fumbled_1_player_id",
                    "fumbled_1_player_name",
                    "fumbled_1_team",
                    "fumbled_2_player_id",
                    "fumbled_2_player_name",
                    "fumbled_2_team") %in% colnames(play_data)))) {
          
          fumbled_cols <- c("fumbled_1_player_id",
                            "fumbled_1_player_name",
                            "fumbled_1_team",
                            "fumbled_2_player_id",
                            "fumbled_2_player_name",
                            "fumbled_2_team")
          # Find which ones are missing:
          missing_fumbled <- which(!(fumbled_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_fumbled_row <- as.list(rep(NA, length(missing_fumbled))) %>%
            purrr::set_names(fumbled_cols[missing_fumbled]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data:
          play_data <- play_data %>%
            dplyr::bind_cols(missing_fumbled_row)
          rm(missing_fumbled_row)
        }
      }
      
      # Again for fumble recovery players:
      if (!(any(c("own_fumble_recovery_yards",
                  "own_fumble_recovery_yards_td",
                  "opp_fumble_recovery_yards",
                  "opp_fumble_recovery_yards_td") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(fumble_recovery_1_player_id = NA,
                        fumble_recovery_1_player_name = NA,
                        fumble_recovery_1_team = NA,
                        fumble_recovery_1_yards = NA,
                        fumble_recovery_2_player_id = NA,
                        fumble_recovery_2_player_name = NA,
                        fumble_recovery_2_team = NA,
                        fumble_recovery_2_yards = NA)
      } else {
        # Get the player info by looking at the fumble recovery rows only,
        # allowing for two:
        fumble_recovery_player_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("own_fumble_recovery_yards",
                                        "own_fumble_recovery_yards_td",
                                        "opp_fumble_recovery_yards",
                                        "opp_fumble_recovery_yards_td")) %>%
          dplyr::select(team, player_name, player_id, yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1:2) %>%
          dplyr::mutate(position = paste("fumble_recovery", 1:dplyr::n(), sep = "_"))
        
        # Now make 4 separate rows: containing the team, player_id, yards, and
        # player_name:
        fumble_recovery_team_row <- fumble_recovery_player_data %>%
          dplyr::select(position, team) %>%
          dplyr::mutate(position = paste(position, "team", sep = "_")) %>%
          tidyr::spread(position, team)
        
        fumble_recovery_yards_row <- fumble_recovery_player_data %>%
          dplyr::select(position, yards) %>%
          dplyr::mutate(position = paste(position, "yards", sep = "_")) %>%
          tidyr::spread(position, yards)
        
        fumble_recovery_id_row <- fumble_recovery_player_data %>%
          dplyr::select(position, player_id) %>%
          dplyr::mutate(position = paste(position, "player_id", sep = "_")) %>%
          tidyr::spread(position, player_id)
        
        fumble_recovery_name_row <- fumble_recovery_player_data %>%
          dplyr::select(position, player_name) %>%
          dplyr::mutate(position = paste(position, "player_name", sep = "_")) %>%
          tidyr::spread(position, player_name)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(fumble_recovery_team_row, 
                           fumble_recovery_yards_row,
                           fumble_recovery_id_row,
                           fumble_recovery_name_row)
        rm(fumble_recovery_player_data,
           fumble_recovery_id_row,
           fumble_recovery_name_row,
           fumble_recovery_team_row,
           fumble_recovery_yards_row)
        
        # Check to see if there any of the possible numbers for fumble recovery players
        # is missing, just make NA:
        if (!(all(c("fumble_recovery_1_player_id",
                    "fumble_recovery_1_player_name",
                    "fumble_recovery_1_team",
                    "fumble_recovery_1_yards",
                    "fumble_recovery_2_player_id",
                    "fumble_recovery_2_player_name",
                    "fumble_recovery_2_team",
                    "fumble_recovery_2_yards") %in% colnames(play_data)))) {
          
          fumble_recovery_cols <- c("fumble_recovery_1_player_id",
                                    "fumble_recovery_1_player_name",
                                    "fumble_recovery_1_team",
                                    "fumble_recovery_1_yards",
                                    "fumble_recovery_2_player_id",
                                    "fumble_recovery_2_player_name",
                                    "fumble_recovery_2_team",
                                    "fumble_recovery_2_yards")
          # Find which ones are missing:
          missing_fumble_recovery <- which(!(fumble_recovery_cols %in% colnames(play_data)))
          # Create a dataframe row of NAs for these to be appended to the play row:
          missing_fumble_recovery_row <- as.list(rep(NA, length(missing_fumble_recovery))) %>%
            purrr::set_names(fumble_recovery_cols[missing_fumble_recovery]) %>%
            purrr::flatten_dfc() %>% as.data.frame()
          
          # Now join this data:
          play_data <- play_data %>%
            dplyr::bind_cols(missing_fumble_recovery_row)
          rm(missing_fumble_recovery_row)
        }
      }
      
      # (3) Team level data (beyond the player team identifiers that are
      # already gathered above)
      
      # Touchdown team:
      if (!(any(stringr::str_detect(play_player_data$nfl_stat, "_td")))) {
        play_data <- play_data %>%
          dplyr::mutate(td_team = NA)
      } else {
        # Get the player info by looking at the kickoff return rows only:
        td_team_data <- play_player_data %>%
          dplyr::filter(stringr::str_detect(nfl_stat, "_td")) %>%
          dplyr::select(team) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(td_team = team)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(td_team_data)
        rm(td_team_data)
      }
      
      # Return team:
      if (!(any(stringr::str_detect(play_player_data$nfl_stat, "_return")) |
            play_player_data$nfl_stat %in% c("punt_touchback_receiving",
                                             "punt_downed",
                                             "punt_fair_catch",
                                             "kickoff_fair_catch",
                                             "kickoff_touchback_receiving"))) {
        play_data <- play_data %>%
          dplyr::mutate(return_team = NA)
      } else {
        return_team_data <- play_player_data %>%
          dplyr::filter(stringr::str_detect(nfl_stat, "_return") |
                          nfl_stat %in% c("punt_touchback_receiving",
                                          "punt_downed","punt_fair_catch",
                                          "kickoff_fair_catch",
                                          "kickoff_touchback_receiving")) %>%
          dplyr::select(team) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(return_team = team)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(return_team_data)
        rm(return_team_data)
      }
      
      # Timeout team:
      if (!("timeout" %in% play_player_data$nfl_stat)) {
        play_data <- play_data %>%
          dplyr::mutate(timeout_team = NA)
      } else {
        timeout_team_data <- play_player_data %>%
          dplyr::filter(nfl_stat == "timeout") %>%
          dplyr::select(team) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(timeout_team = team)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(timeout_team_data)
        rm(timeout_team_data)
      }
      
      # Penalty information (team, player, and yards):
      if(!(c("penalty_yards") %in% play_player_data$nfl_stat)) {
        play_data <- play_data %>%
          dplyr::mutate(penalty_team = NA,
                        penalty_player_id = NA,
                        penalty_player_name = NA,
                        penalty_yards = NA)
      } else {
        penalty_data <- play_player_data %>%
          dplyr::filter(nfl_stat == "penalty_yards") %>%
          dplyr::select(team, player_name, player_id, yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(penalty_team = team,
                        penalty_player_id = player_id,
                        penalty_player_name = player_name,
                        penalty_yards = yards)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(penalty_data)
        rm(penalty_data)
      }
      
      # (4) Yards associated with the various statistics of the plays, 
      # also creates an indicator to know that the penalty was in addition
      # to the yards gained:
      
      # Yards gained (or lost) by the possession team:
      if (!(any(c("rushing_yards", "rushing_yards_td",
                  "lateral_rushing_yards", "lateral_rushing_yards_td",
                  "passing_yards", "passing_yards_td",
                  "sack_yards", "receiving_yards",
                  "receiving_yards_td", "lateral_receiving_yards",
                  "lateral_receiving_yards_td") %in% play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(yards_gained = 0, penalty_fix = 0)
        
      } else {
        yards_gained_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("rushing_yards", "rushing_yards_td",
                                        "lateral_rushing_yards", "lateral_rushing_yards_td",
                                        "passing_yards", "passing_yards_td",
                                        "sack_yards", "receiving_yards",
                                        "receiving_yards_td", "lateral_receiving_yards",
                                        "lateral_receiving_yards_td")) %>%
          dplyr::select(yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(yards_gained = yards) %>%
          dplyr::mutate(penalty_fix = 1)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(yards_gained_data)
        rm(yards_gained_data)
      }
      
      # Return yards for INTs, kickoffs, and punts:
      if (!(any(c("interception_return_yards", "interception_return_yards_td",
                  "lateral_interception_return_yards", "lateral_interception_return_yards_td",
                  "punt_return_yards", "punt_return_yards_td",
                  "lateral_punt_return_yards", "lateral_punt_return_yards_td",
                  "kickoff_return_yards", "kickoff_return_yards_td",
                  "lateral_kickoff_return_yards", "lateral_kickoff_return_yards_td") %in% play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(return_yards = 0, return_penalty_fix = 0)
        
      } else {
        return_yards_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("interception_return_yards", "interception_return_yards_td",
                                        "lateral_interception_return_yards", "lateral_interception_return_yards_td",
                                        "punt_return_yards", "punt_return_yards_td",
                                        "lateral_punt_return_yards", "lateral_punt_return_yards_td",
                                        "kickoff_return_yards", "kickoff_return_yards_td",
                                        "lateral_kickoff_return_yards", "lateral_kickoff_return_yards_td")) %>%
          dplyr::select(yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(return_yards = yards) %>%
          dplyr::mutate(return_penalty_fix = 1)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(return_yards_data)
        rm(return_yards_data)
      }
      
      # Air yards:
      if (!(any(c("air_yards_complete", "air_yards_incomplete") %in%
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(air_yards = NA)
      } else {
        air_yards_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("air_yards_complete", "air_yards_incomplete")) %>%
          dplyr::select(yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(air_yards = yards)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(air_yards_data)
        rm(air_yards_data)
      }
      
      # Yards after catch:
      if (!(any(c("yards_after_catch") %in%
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(yards_after_catch = NA)
      } else {
        yards_after_catch_data <- play_player_data %>%
          dplyr::filter(nfl_stat %in% c("yards_after_catch")) %>%
          dplyr::select(yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(yards_after_catch = yards)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(yards_after_catch_data)
        rm(yards_after_catch_data)
      }
      
      # Kickoff, punt, and field goal distance:
      if (!(any(c("punting_yards", "kickoff_yards",
                  "field_goal_yards_missed",
                  "field_goal_yards_made",
                  "field_goal_yards_blocked") %in% 
                play_player_data$nfl_stat))) {
        play_data <- play_data %>%
          dplyr::mutate(kick_distance = NA)
      } else {
        kick_distance_data = play_player_data %>%
          dplyr::filter(nfl_stat %in% c("punting_yards", "kickoff_yards",
                                        "field_goal_yards_missed",
                                        "field_goal_yards_made",
                                        "field_goal_yards_blocked")) %>%
          dplyr::select(yards) %>%
          dplyr::distinct() %>%
          dplyr::slice(1) %>%
          dplyr::rename(kick_distance = yards)
        
        # Join this data:
        play_data <- play_data %>%
          dplyr::bind_cols(kick_distance_data)
        rm(kick_distance_data)
      }
      
      
      
      # Put the columns in the following order:
      play_data <- play_data %>%
        dplyr::select(punt_blocked, first_down_rush, first_down_pass, 
                      first_down_penalty, third_down_converted, third_down_failed, 
                      fourth_down_converted, fourth_down_failed, incomplete_pass, 
                      interception, punt_inside_twenty, punt_in_endzone, punt_out_of_bounds, 
                      punt_downed, punt_fair_catch, kickoff_inside_twenty, kickoff_in_endzone, 
                      kickoff_out_of_bounds, kickoff_fair_catch, fumble_forced, fumble_not_forced, 
                      fumble_out_of_bounds, timeout, field_goal_missed, field_goal_made, 
                      field_goal_blocked, extra_point_good, extra_point_failed, extra_point_blocked, 
                      two_point_rush_good, two_point_rush_failed, two_point_pass_good, 
                      two_point_pass_failed, solo_tackle, safety, penalty, tackled_for_loss, 
                      extra_point_safety, two_point_rush_safety, two_point_pass_safety,
                      kickoff_downed, two_point_pass_reception_good, two_point_pass_reception_failed, 
                      fumble_lost, own_kickoff_recovery, own_kickoff_recovery_td, qb_hit, 
                      extra_point_aborted, two_point_return, rush_attempt, pass_attempt, sack, 
                      touchdown, pass_touchdown, rush_touchdown, return_touchdown, extra_point_attempt,
                      two_point_attempt, field_goal_attempt, kickoff_attempt, punt_attempt, 
                      fumble, complete_pass, assist_tackle, lateral_reception, lateral_rush, 
                      lateral_return, lateral_recovery, passer_player_id, passer_player_name, 
                      receiver_player_id, receiver_player_name, rusher_player_id, rusher_player_name, 
                      lateral_receiver_player_id, lateral_receiver_player_name, lateral_rusher_player_id, 
                      lateral_rusher_player_name, lateral_sack_player_id, lateral_sack_player_name,
                      interception_player_id, interception_player_name, lateral_interception_player_id, 
                      lateral_interception_player_name, punt_returner_player_id, punt_returner_player_name, 
                      lateral_punt_returner_player_id, lateral_punt_returner_player_name, 
                      kickoff_returner_player_name, kickoff_returner_player_id, lateral_kickoff_returner_player_id, 
                      lateral_kickoff_returner_player_name, punter_player_id, punter_player_name, 
                      kicker_player_name, kicker_player_id, own_kickoff_recovery_player_id, 
                      own_kickoff_recovery_player_name, blocked_player_id, blocked_player_name, 
                      tackle_for_loss_1_player_id, tackle_for_loss_1_player_name, 
                      tackle_for_loss_2_player_id, tackle_for_loss_2_player_name,
                      qb_hit_1_player_id, qb_hit_1_player_name, 
                      qb_hit_2_player_id, qb_hit_2_player_name,
                      forced_fumble_player_1_team, 
                      forced_fumble_player_1_player_id,
                      forced_fumble_player_1_player_name,
                      forced_fumble_player_2_team, 
                      forced_fumble_player_2_player_id,
                      forced_fumble_player_2_player_name,
                      solo_tackle_1_team, solo_tackle_2_team, solo_tackle_1_player_id, 
                      solo_tackle_2_player_id, solo_tackle_1_player_name, solo_tackle_2_player_name,
                      assist_tackle_1_player_id, assist_tackle_1_player_name, assist_tackle_1_team, 
                      assist_tackle_2_player_id, assist_tackle_2_player_name, assist_tackle_2_team, 
                      assist_tackle_3_player_id, assist_tackle_3_player_name, assist_tackle_3_team, 
                      assist_tackle_4_player_id, assist_tackle_4_player_name, assist_tackle_4_team, 
                      pass_defense_1_player_id, pass_defense_1_player_name, pass_defense_2_player_id,
                      pass_defense_2_player_name, fumbled_1_team, fumbled_1_player_id, fumbled_1_player_name, 
                      fumbled_2_player_id, fumbled_2_player_name, fumbled_2_team, 
                      fumble_recovery_1_team, fumble_recovery_1_yards,
                      fumble_recovery_1_player_id, fumble_recovery_1_player_name, 
                      fumble_recovery_2_team, fumble_recovery_2_yards,
                      fumble_recovery_2_player_id, fumble_recovery_2_player_name, 
                      td_team, return_team, timeout_team, yards_gained,
                      return_yards, air_yards, yards_after_catch, penalty_team,
                      penalty_player_id, penalty_player_name, penalty_yards,
                      kick_distance, defensive_two_point_attempt,
                      defensive_two_point_conv,
                      defensive_extra_point_attempt,
                      defensive_extra_point_conv, penalty_fix, return_penalty_fix)
      
      # Else play_data is missing everything: 
    } else {
      play_data <- data.frame(punt_blocked = NA, first_down_rush = NA, 
                              first_down_pass = NA, first_down_penalty = NA, 
                              third_down_converted = NA, third_down_failed = NA, 
                              fourth_down_converted = NA, fourth_down_failed = NA, 
                              incomplete_pass = NA, interception = NA, punt_inside_twenty = NA, 
                              punt_in_endzone = NA, punt_out_of_bounds = NA, punt_downed = NA, 
                              punt_fair_catch = NA, kickoff_inside_twenty = NA, kickoff_in_endzone = NA, 
                              kickoff_out_of_bounds = NA, kickoff_fair_catch = NA, fumble_forced = NA, 
                              fumble_not_forced = NA, fumble_out_of_bounds = NA, timeout = NA, 
                              field_goal_missed = NA, field_goal_made = NA, field_goal_blocked = NA, 
                              extra_point_good = NA, extra_point_failed = NA, extra_point_blocked = NA, 
                              two_point_rush_good = NA, two_point_rush_failed = NA, two_point_pass_good = NA, 
                              two_point_pass_failed = NA, solo_tackle = NA, safety = NA, penalty = NA, 
                              tackled_for_loss = NA, extra_point_safety = NA, two_point_rush_safety = NA, 
                              two_point_pass_safety = NA, kickoff_downed = NA, two_point_pass_reception_good = NA, 
                              two_point_pass_reception_failed = NA, fumble_lost = NA, own_kickoff_recovery = NA, 
                              own_kickoff_recovery_td = NA, qb_hit = NA, extra_point_aborted = NA, 
                              two_point_return = NA, rush_attempt = NA, pass_attempt = NA, sack = NA, 
                              touchdown = NA, pass_touchdown = NA, rush_touchdown = NA, return_touchdown = NA, 
                              extra_point_attempt = NA, two_point_attempt = NA, field_goal_attempt = NA, 
                              kickoff_attempt = NA, punt_attempt = NA, fumble = NA, complete_pass = NA, 
                              assist_tackle = NA, lateral_reception = NA, lateral_rush = NA, lateral_return = NA, 
                              lateral_recovery = NA, passer_player_id = NA, passer_player_name = NA, 
                              receiver_player_id = NA, receiver_player_name = NA, rusher_player_id = NA, 
                              rusher_player_name = NA, lateral_receiver_player_id = NA, lateral_receiver_player_name = NA,
                              lateral_rusher_player_id = NA, lateral_rusher_player_name = NA, lateral_sack_player_id = NA, 
                              lateral_sack_player_name = NA, interception_player_id = NA, interception_player_name = NA, 
                              lateral_interception_player_id = NA, lateral_interception_player_name = NA, 
                              punt_returner_player_id = NA, punt_returner_player_name = NA, 
                              lateral_punt_returner_player_id = NA, lateral_punt_returner_player_name = NA, 
                              kickoff_returner_player_name = NA, kickoff_returner_player_id = NA, 
                              lateral_kickoff_returner_player_id = NA, lateral_kickoff_returner_player_name = NA,
                              punter_player_id = NA, punter_player_name = NA, kicker_player_name = NA, 
                              kicker_player_id = NA, own_kickoff_recovery_player_id = NA, 
                              own_kickoff_recovery_player_name = NA, blocked_player_id = NA, 
                              blocked_player_name = NA, tackle_for_loss_1_player_id = NA, 
                              tackle_for_loss_1_player_name = NA, 
                              tackle_for_loss_2_player_id = NA, 
                              tackle_for_loss_2_player_name = NA,
                              qb_hit_1_player_id = NA, qb_hit_1_player_name = NA, 
                              qb_hit_2_player_id = NA, qb_hit_2_player_name = NA,
                              forced_fumble_player_1_team = NA, 
                              forced_fumble_player_1_player_id = NA,
                              forced_fumble_player_1_player_name = NA,
                              forced_fumble_player_2_team = NA, 
                              forced_fumble_player_2_player_id = NA,
                              forced_fumble_player_2_player_name = NA,
                              solo_tackle_1_team = NA, 
                              solo_tackle_2_team = NA, solo_tackle_1_player_id = NA, 
                              solo_tackle_2_player_id = NA, solo_tackle_1_player_name = NA, 
                              solo_tackle_2_player_name = NA, assist_tackle_1_player_id = NA, 
                              assist_tackle_1_player_name = NA, assist_tackle_1_team = NA, 
                              assist_tackle_2_player_id = NA, assist_tackle_2_player_name = NA, 
                              assist_tackle_2_team = NA, assist_tackle_3_player_id = NA, 
                              assist_tackle_3_player_name = NA, assist_tackle_3_team = NA, 
                              assist_tackle_4_player_id = NA, assist_tackle_4_player_name = NA, 
                              assist_tackle_4_team = NA, pass_defense_1_player_id = NA, 
                              pass_defense_1_player_name = NA, pass_defense_2_player_id = NA, 
                              pass_defense_2_player_name = NA, fumbled_1_team = NA, 
                              fumbled_1_player_id = NA, fumbled_1_player_name = NA, 
                              fumbled_2_player_id = NA, fumbled_2_player_name = NA, 
                              fumbled_2_team = NA, 
                              fumble_recovery_1_team = NA, 
                              fumble_recovery_1_yards = NA,
                              fumble_recovery_1_player_id = NA, 
                              fumble_recovery_1_player_name = NA, 
                              fumble_recovery_2_team = NA,                       
                              fumble_recovery_2_yards = NA,
                              fumble_recovery_2_player_id = NA,
                              fumble_recovery_2_player_name = NA,
                              td_team = NA, return_team = NA,
                              timeout_team = NA, yards_gained = 0,
                              return_yards = 0, air_yards = NA,
                              yards_after_catch = NA, penalty_team = NA,
                              penalty_player_id = NA, 
                              penalty_player_name = NA, 
                              penalty_yards = NA,
                              kick_distance = NA,
                              defensive_two_point_attempt = NA,
                              defensive_two_point_conv = NA,
                              defensive_extra_point_attempt = NA,
                              defensive_extra_point_conv = NA, penalty_fix = 0,
                              return_penalty_fix = 0)
    }
    
    
    return(play_data)
    
  }
  
  # Apply the get_drive_play_data function to each drive in the game,
  # catch the situation with GameID == 2013092206 and drive == 3
  if (game_id != 2013092206) {
    drive_play_data <- suppressWarnings(purrr::map_dfr(1:n_drives,
                                                       function(x) {
                                                         get_drive_play_data(game_json[[1]]$drives[[x]])
                                                       }))
    
  } else {
    drive_play_data <- suppressWarnings(purrr::map_dfr(c(1, 2, 4:n_drives),
                                                       function(x) {
                                                         get_drive_play_data(game_json[[1]]$drives[[x]])
                                                       }))
  }
  
  # Add to the game_pbp:
  game_pbp <- dplyr::bind_cols(game_pbp, drive_play_data)
  
  # Now add various other information accessible outside of the API data for
  # individual plays.
  
  # Create columns for which teams are home and away:
  game_pbp <- game_pbp %>%
    dplyr::mutate(game_id = game_id,
                  home_team = game_json[[1]]$home$abbr,
                  away_team = game_json[[1]]$away$abbr,
                  # Create and modify columns for quarter end
                  quarter_end = stringr::str_detect(desc,
                                                    "(END QUARTER)|(END GAME)|(End of quarter)") %>%
                    as.numeric(),
                  # Modify the time column for the quarter end:
                  time = dplyr::if_else(quarter_end == 1, "00:00", time),
                  # Fill in the rows with missing posteam with the lag:
                  posteam = ifelse(quarter_end == 1 | posteam == "",
                                   dplyr::lag(posteam),
                                   posteam),
                  # Denote whether the home or away team has possession:
                  posteam_type = dplyr::if_else(posteam == home_team,
                                                "home",
                                                "away"),
                  # Column denoting which team is on defense:
                  defteam = dplyr::if_else(posteam_type == "home",
                                           away_team, home_team),
                  # Make the possession team for kickoffs be the return team, since that is
                  # more intuitive from the EPA / WPA point of view:
                  posteam = dplyr::if_else(kickoff_attempt == 1,
                                           dplyr::if_else(posteam_type == "home",
                                                          away_team, home_team),
                                           posteam),
                  defteam = dplyr::if_else(kickoff_attempt == 1,
                                           dplyr::if_else(posteam_type == "home",
                                                          home_team, away_team),
                                           defteam),
                  # Now flip the posteam_type as well:
                  posteam_type = dplyr::if_else(kickoff_attempt == 1,
                                                dplyr::if_else(posteam_type == "home",
                                                               "away", "home"),
                                                posteam_type),
                  # Next need to parse the provided yrdln field into both the side of the 
                  # field the possession team is on as well as the distance from their
                  # opponents' endzone. First update the yrdln field so that the 50 also has
                  # the same format as the rest of the field positions with a space and also
                  # edit the early season plays that were marked with NULL for timeouts:
                  yrdln = dplyr::if_else(yrdln == "50", "MID 50", yrdln),
                  yrdln = dplyr::if_else(nchar(yrdln) == 0 | is.null(yrdln) |
                                           yrdln == "NULL",
                                         dplyr::lag(yrdln), yrdln),
                  # Create two columns: one for side of field and the other as the numeric 
                  # distance from the opponents end zone:
                  side_of_field = purrr::map_chr(stringr::str_split(yrdln, " "),
                                                 function(x) x[1]),
                  yardline_100 = as.numeric(purrr::map_chr(stringr::str_split(yrdln, " "),
                                                           function(x) x[2])),
                  yardline_100 = dplyr::if_else(side_of_field == posteam |
                                                  yardline_100 == 50,
                                                100 - yardline_100, yardline_100))
  
  # Now create a column for the game date using the game_id and game_url:
  date_parse <- stringr::str_extract(game_url, pattern = "/[0-9]{10}/") %>%
    stringr::str_extract(pattern = "[0-9]{8}")
  date_year <- stringr::str_sub(date_parse, 1, 4)
  date_month <- stringr::str_sub(date_parse, 5, 6)
  date_day <- stringr::str_sub(date_parse, nchar(date_parse) - 1, 
                               nchar(date_parse))
  game_pbp <- game_pbp %>%
    dplyr::mutate(game_year = date_year,
                  game_month = date_month,
                  game_date = as.Date(paste(date_month, 
                                            date_day, 
                                            date_year, sep = "/"),
                                      format = "%m/%d/%Y"),
                  # Need to create columns for properly formatting the time columns:
                  # Create a column with the time in seconds remaining for the quarter:
                  quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time)),
                  # Create a column with the time in seconds remaining for each half:
                  half_seconds_remaining = dplyr::if_else(qtr %in% c(1, 3),
                                                          quarter_seconds_remaining + 900,
                                                          quarter_seconds_remaining),
                  # Create a column with the time in seconds remaining for the game:
                  game_seconds_remaining = dplyr::if_else(qtr %in% c(1, 2, 3),
                                                          quarter_seconds_remaining + (900 * (4 - as.numeric(qtr))),
                                                          quarter_seconds_remaining),
                  # Add column for replay or challenge:
                  replay_or_challenge = stringr::str_detect(desc, 
                                                            "(Replay Official reviewed)|( challenge(d)? )") %>%
                    as.numeric(),
                  # Result of replay or challenge:
                  replay_or_challenge_result = dplyr::if_else(replay_or_challenge == 1,
                                                              dplyr::if_else(
                                                                stringr::str_detect(tolower(desc),
                                                                                    "( upheld)|( reversed)|( confirmed)"),
                                                                stringr::str_extract(tolower(desc),
                                                                                   "( upheld)|( reversed)|( confirmed)") %>%
                                                                stringr::str_trim(), "denied"),
                                                                NA_character_),
                  # Using the various two point indicators, create a column denoting the result
                  # outcome for two point conversions:
                  two_point_conv_result = dplyr::if_else((two_point_rush_good == 1 | 
                                                            two_point_pass_good == 1 |
                                                            two_point_pass_reception_good == 1) &
                                                           two_point_attempt == 1,
                                                         "success", NA_character_),
                  two_point_conv_result = dplyr::if_else((two_point_rush_failed == 1 | 
                                                            two_point_pass_failed == 1 |
                                                            two_point_pass_reception_failed == 1) &
                                                           two_point_attempt == 1,
                                                         "failure", two_point_conv_result),
                  two_point_conv_result = dplyr::if_else((two_point_rush_safety == 1 | 
                                                            two_point_pass_safety == 1) &
                                                           two_point_attempt == 1,
                                                         "safety", two_point_conv_result),
                  two_point_conv_result = dplyr::if_else(two_point_return == 1 &
                                                           two_point_attempt == 1,
                                                         "return", two_point_conv_result),  
                  # If the result was a success, make the yards_gained to be 2:
                  yards_gained = dplyr::if_else(!is.na(two_point_conv_result) &
                                                  two_point_conv_result == "success",
                                                2, yards_gained),
                  # Extract the penalty type:
                  penalty_type = dplyr::if_else(penalty == 1, 
                                                desc %>%
                                                  stringr::str_extract("PENALTY on (.){2,35},.+, [0-9]{1,2} yard(s),") %>% 
                                                  stringr::str_extract(", (([:alpha:])+([:space:])?)+,") %>% 
                                                  stringr::str_remove_all(",") %>% 
                                                  stringr::str_trim(), NA_character_),
                  # Make plays marked with down == 0 as NA:
                  down = dplyr::if_else(down == 0,
                                        NA_real_, down),
                  # Create the goal_to_go indicator variable:
                  goal_to_go = dplyr::if_else(side_of_field != posteam &
                                                ((ydstogo == yardline_100) |
                                                   (ydstogo <= 1 & yardline_100 == 1)),
                                              1, 0),
                  # Using the field goal indicators make a column with the field goal result:
                  field_goal_result = dplyr::if_else(field_goal_attempt == 1 &
                                                       field_goal_made == 1,
                                                     "made", NA_character_),
                  field_goal_result = dplyr::if_else(field_goal_attempt == 1 &
                                                       field_goal_missed == 1,
                                                     "missed", field_goal_result),
                  field_goal_result = dplyr::if_else(field_goal_attempt == 1 &
                                                       field_goal_blocked == 1,
                                                     "blocked", field_goal_result),
                  # Set the kick_distance for extra points by adding 18 to the yardline_100:
                  kick_distance = dplyr::if_else(extra_point_attempt == 1,
                                                 yardline_100 + 18,
                                                 kick_distance),
                  # Using the indicators make a column with the extra point result:
                  extra_point_result = dplyr::if_else(extra_point_attempt == 1 &
                                                        extra_point_good == 1,
                                                      "good", NA_character_),
                  extra_point_result = dplyr::if_else(extra_point_attempt == 1 &
                                                        extra_point_failed == 1,
                                                      "failed", extra_point_result),
                  extra_point_result = dplyr::if_else(extra_point_attempt == 1 &
                                                        extra_point_blocked == 1,
                                                      "blocked", extra_point_result),
                  extra_point_result = dplyr::if_else(extra_point_attempt == 1 &
                                                        extra_point_safety == 1,
                                                      "safety", extra_point_result),
                  extra_point_result = dplyr::if_else(extra_point_attempt == 1 &
                                                        extra_point_aborted == 1,
                                                      "aborted", extra_point_result),
                  # Create the column denoting the categorical description of the pass length:
                  pass_length = dplyr::if_else(two_point_attempt == 0 &
                                                 sack == 0 &
                                                 pass_attempt == 1,
                                               desc %>% stringr::str_extract("pass (incomplete )?(short|deep)") %>% 
                                                 stringr::str_extract("short|deep"), NA_character_),
                  # Create the column denoting the categorical location of the pass:
                  pass_location = dplyr::if_else(two_point_attempt == 0 &
                                                   sack == 0 &
                                                   pass_attempt == 1,
                                                 desc %>% stringr::str_extract("(short|deep) (left|middle|right)") %>% 
                                                   stringr::str_extract("left|middle|right"), NA_character_),
                  # Indicator columns for both QB kneels, spikes, scrambles,
                  # no huddle, shotgun plays:
                  qb_kneel = stringr::str_detect(desc, " kneels ") %>% as.numeric(),
                  qb_spike = stringr::str_detect(desc, " spiked ") %>% as.numeric(),
                  qb_scramble = stringr::str_detect(desc, " scrambles ") %>% as.numeric(),
                  shotgun = stringr::str_detect(desc, "Shotgun") %>% as.numeric(),
                  no_huddle = stringr::str_detect(desc, "No Huddle") %>% as.numeric(),
                  # Create a play type column: either pass, run, field_goal, extra_point,
                  # kickoff, punt, qb_kneel, qb_spike, or no_play (which includes timeouts and
                  # penalties):
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & penalty_fix == 1)) & 
                                                (pass_attempt == 1 |
                                                               incomplete_pass == 1 |
                                                               two_point_pass_good == 1 |
                                                               two_point_pass_failed == 1 |
                                                               two_point_pass_safety == 1 |
                                                               two_point_pass_reception_good == 1 |
                                                               two_point_pass_reception_failed == 1 |
                                                               pass_attempt == 1 |
                                                               pass_touchdown == 1 |
                                                               complete_pass == 1),
                                             "pass", "no_play"),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & penalty_fix == 1)) & 
                                               (two_point_rush_good == 1 |
                                                               two_point_rush_failed == 1 |
                                                               two_point_rush_safety == 1 |
                                                               rush_attempt == 1 |
                                                               rush_touchdown == 1),
                                             "run", play_type),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & return_penalty_fix == 1) |
                                                (penalty == 1 & (punt_inside_twenty == 1 |
                                                                 punt_in_endzone == 1 |
                                                                 punt_out_of_bounds == 1 |
                                                                 punt_downed == 1 |
                                                                 punt_fair_catch == 1))) & 
                                               punt_attempt == 1,
                                             "punt", play_type),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & return_penalty_fix == 1) |
                                                (penalty == 1 & (kickoff_inside_twenty == 1 |
                                                                 kickoff_in_endzone == 1 |
                                                                 kickoff_out_of_bounds == 1 | 
                                                                 kickoff_downed == 1 | 
                                                                 kickoff_fair_catch == 1))) & 
                                               kickoff_attempt == 1,
                                             "kickoff", play_type),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & penalty_fix == 1)) & qb_spike == 1,
                                             "qb_spike", play_type),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & penalty_fix == 1)) & qb_kneel == 1,
                                             "qb_kneel", play_type),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & penalty_fix == 1)) & field_goal_attempt == 1,
                                             "field_goal", play_type),
                  play_type = dplyr::if_else((penalty == 0 | 
                                                (penalty == 1 & penalty_fix == 1)) & extra_point_attempt == 1,
                                             "extra_point", play_type),
                  # Indicator for QB dropbacks (exclude spikes and kneels):
                  qb_dropback = dplyr::if_else(play_type == "pass" | 
                                                 (play_type == "run" &
                                                    qb_scramble == 1),
                                               1, 0),
                  # Columns denoting the run location and gap:
                  run_location = dplyr::if_else(two_point_attempt == 0 &
                                                  rush_attempt == 1,
                                                desc %>% stringr::str_extract(" (left|middle|right) ") %>% 
                                                  stringr::str_trim(), NA_character_),
                  run_gap = dplyr::if_else(two_point_attempt == 0 &
                                             rush_attempt == 1,
                                           desc %>% stringr::str_extract(" (guard|tackle|end) ") %>% 
                                             stringr::str_trim(), NA_character_),
                  # Column to indicate the half:
                  game_half = dplyr::if_else(qtr %in% c(1, 2), "Half1", NA_character_),
                  game_half = dplyr::if_else(qtr %in% c(3, 4), "Half2", game_half),
                  game_half = dplyr::if_else(qtr >= 5, "Overtime", game_half),
                  # Create columns to denote the timeouts remaining for each team, making 
                  # columns for both home/away and pos/def (this will involve creating 
                  # temporary columns that will not be included):
                  # Initialize both home and away to have 3 timeouts for each
                  # half except overtime where they have 2:
                  home_timeouts_remaining = dplyr::if_else(qtr %in% c(1, 2, 3, 4),
                                                           3, 2),
                  away_timeouts_remaining = dplyr::if_else(qtr %in% c(1, 2, 3, 4),
                                                           3, 2),
                  home_timeout_used = dplyr::if_else(timeout == 1 & 
                                                       timeout_team == home_team,
                                                     1, 0),
                  away_timeout_used = dplyr::if_else(timeout == 1 &
                                                       timeout_team == away_team,
                                                     1, 0),
                  home_timeout_used = dplyr::if_else(is.na(home_timeout_used),
                                                     0, home_timeout_used),
                  away_timeout_used = dplyr::if_else(is.na(away_timeout_used),
                                                     0, away_timeout_used)) %>%
    # Group by the game_half to then create cumulative timeouts used for both
    # the home and away teams:
    dplyr::group_by(game_half) %>%
    dplyr::mutate(total_home_timeouts_used = cumsum(home_timeout_used),
                  total_away_timeouts_used = cumsum(away_timeout_used)) %>%
    dplyr::ungroup() %>%
    # Now just take the difference between the timeouts remaining 
    # columns and the total timeouts used, and create the columns for both
    # the pos and def team timeouts remaining:
    dplyr::mutate(home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
                  away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
                  posteam_timeouts_remaining = dplyr::if_else(posteam == home_team,
                                                              home_timeouts_remaining,
                                                              away_timeouts_remaining),
                  defteam_timeouts_remaining = dplyr::if_else(defteam == home_team,
                                                              home_timeouts_remaining,
                                                              away_timeouts_remaining),
                  # Same type of logic to calculate the score for each team and the score
                  # differential in the game. First create columns to track how many points
                  # were scored on a particular play based on various scoring indicators for
                  # both the home and away teams: 
                  home_points_scored = dplyr::if_else(touchdown == 1 &
                                                        td_team == home_team,
                                                      6, 0),
                  home_points_scored = dplyr::if_else(posteam == home_team &
                                                        field_goal_made == 1,
                                                      3, home_points_scored),
                  home_points_scored = dplyr::if_else(posteam == home_team &
                                                        (extra_point_good == 1 |
                                                           extra_point_safety == 1 |
                                                           two_point_rush_safety == 1 |
                                                           two_point_pass_safety == 1),
                                                      1, home_points_scored),
                  home_points_scored = dplyr::if_else(posteam == home_team &
                                                        (two_point_rush_good == 1 |
                                                           two_point_pass_good == 1 |
                                                           two_point_pass_reception_good == 1),
                                                      2, home_points_scored),
                  home_points_scored = dplyr::if_else(defteam == home_team &
                                                        (safety == 1 | two_point_return == 1),
                                                      2, home_points_scored),
                  away_points_scored = dplyr::if_else(touchdown == 1 &
                                                        td_team == away_team,
                                                      6, 0),
                  away_points_scored = dplyr::if_else(posteam == away_team &
                                                        field_goal_made == 1,
                                                      3, away_points_scored),
                  away_points_scored = dplyr::if_else(posteam == away_team &
                                                        (extra_point_good == 1 |
                                                           extra_point_safety == 1 |
                                                           two_point_rush_safety == 1 |
                                                           two_point_pass_safety == 1),
                                                      1, away_points_scored),
                  away_points_scored = dplyr::if_else(posteam == away_team &
                                                        (two_point_rush_good == 1 |
                                                           two_point_pass_good == 1 |
                                                           two_point_pass_reception_good == 1),
                                                      2, away_points_scored),
                  away_points_scored = dplyr::if_else(defteam == away_team &
                                                        (safety == 1 | two_point_return == 1),
                                                      2, away_points_scored),
                  home_points_scored = dplyr::if_else(is.na(home_points_scored),
                                                      0, home_points_scored),
                  away_points_scored = dplyr::if_else(is.na(away_points_scored),
                                                      0, away_points_scored),
                  # Now create cumulative totals:
                  total_home_score = cumsum(home_points_scored),
                  total_away_score = cumsum(away_points_scored),
                  posteam_score = dplyr::if_else(posteam == home_team,
                                                 dplyr::lag(total_home_score), 
                                                 dplyr::lag(total_away_score)),
                  defteam_score = dplyr::if_else(defteam == home_team,
                                                 dplyr::lag(total_home_score),
                                                 dplyr::lag(total_away_score)),
                  score_differential = posteam_score - defteam_score,
                  abs_score_differential = abs(score_differential),
                  # Make post score differential columns to be used for the final
                  # game indicators in the win probability calculations:
                  posteam_score_post = dplyr::if_else(posteam == home_team,
                                                      total_home_score, 
                                                      total_away_score),
                  defteam_score_post = dplyr::if_else(defteam == home_team,
                                                      total_home_score,
                                                      total_away_score),
                  score_differential_post = posteam_score_post - defteam_score_post,
                  abs_score_differential_post = abs(posteam_score_post - defteam_score_post),
                  # Create a variable for whether or not a touchback occurred, this
                  # will apply to any type of play:
                  touchback = as.numeric(stringr::str_detect(tolower(desc), "touchback")))
  
  # App the expected points columns:
  game_pbp %>%
    add_ep_variables() %>%
    # air and yac EPA variables:
    add_air_yac_ep_variables() %>%
    # win probability variables:
    add_wp_variables() %>%
    # air and yac WPA variables:
    add_air_yac_wp_variables() %>%
    # Now select only the necessary variables in the desired order:
    dplyr::select(play_id, game_id, home_team, away_team, 
                  posteam, posteam_type, defteam, side_of_field, yardline_100, 
                  game_date, quarter_seconds_remaining, half_seconds_remaining, 
                  game_seconds_remaining, game_half, quarter_end, drive, sp, qtr, down, goal_to_go, time, yrdln, ydstogo, ydsnet, 
                  desc, play_type, yards_gained, shotgun, no_huddle, qb_dropback, qb_kneel, qb_spike, 
                  qb_scramble, pass_length, pass_location, air_yards, yards_after_catch,
                  run_location, run_gap, field_goal_result, kick_distance, extra_point_result, two_point_conv_result,
                  home_timeouts_remaining, away_timeouts_remaining, timeout, timeout_team, td_team,
                  posteam_timeouts_remaining, defteam_timeouts_remaining, 
                  total_home_score, total_away_score, posteam_score, 
                  defteam_score, score_differential, posteam_score_post, 
                  defteam_score_post, score_differential_post, 
                  no_score_prob, opp_fg_prob, opp_safety_prob, opp_td_prob, fg_prob,
                  safety_prob, td_prob, extra_point_prob, 
                  two_point_conversion_prob, ep, epa, 
                  total_home_epa, total_away_epa, 
                  total_home_rush_epa, total_away_rush_epa, 
                  total_home_pass_epa, total_away_pass_epa, air_epa, yac_epa, comp_air_epa, 
                  comp_yac_epa, total_home_comp_air_epa, total_away_comp_air_epa, 
                  total_home_comp_yac_epa, total_away_comp_yac_epa, 
                  total_home_raw_air_epa, total_away_raw_air_epa, total_home_raw_yac_epa, 
                  total_away_raw_yac_epa, wp, def_wp, home_wp, away_wp, wpa, home_wp_post, 
                  away_wp_post, total_home_rush_wpa, total_away_rush_wpa, 
                  total_home_pass_wpa, total_away_pass_wpa, air_wpa, yac_wpa, comp_air_wpa, comp_yac_wpa, 
                  total_home_comp_air_wpa, total_away_comp_air_wpa, 
                  total_home_comp_yac_wpa, total_away_comp_yac_wpa, 
                  total_home_raw_air_wpa, total_away_raw_air_wpa, total_home_raw_yac_wpa, 
                  total_away_raw_yac_wpa, punt_blocked, first_down_rush, first_down_pass, 
                  first_down_penalty, third_down_converted, third_down_failed, 
                  fourth_down_converted, fourth_down_failed, incomplete_pass, touchback,
                  interception, punt_inside_twenty, punt_in_endzone, punt_out_of_bounds, 
                  punt_downed, punt_fair_catch, kickoff_inside_twenty, kickoff_in_endzone, 
                  kickoff_out_of_bounds, kickoff_downed, kickoff_fair_catch, fumble_forced, fumble_not_forced, 
                  fumble_out_of_bounds, solo_tackle, safety, penalty, tackled_for_loss, 
                  fumble_lost, own_kickoff_recovery, own_kickoff_recovery_td, qb_hit, 
                  rush_attempt, pass_attempt, sack, 
                  touchdown, pass_touchdown, rush_touchdown, return_touchdown, extra_point_attempt, 
                  two_point_attempt, field_goal_attempt, kickoff_attempt, punt_attempt, fumble, 
                  complete_pass, assist_tackle, lateral_reception, lateral_rush, lateral_return, 
                  lateral_recovery, passer_player_id, passer_player_name, receiver_player_id, 
                  receiver_player_name, rusher_player_id, rusher_player_name, lateral_receiver_player_id, 
                  lateral_receiver_player_name, lateral_rusher_player_id, lateral_rusher_player_name, 
                  lateral_sack_player_id, lateral_sack_player_name, interception_player_id, 
                  interception_player_name, lateral_interception_player_id, lateral_interception_player_name,
                  punt_returner_player_id, punt_returner_player_name, lateral_punt_returner_player_id, 
                  lateral_punt_returner_player_name, kickoff_returner_player_name, kickoff_returner_player_id, 
                  lateral_kickoff_returner_player_id, lateral_kickoff_returner_player_name, punter_player_id,
                  punter_player_name, kicker_player_name, kicker_player_id, own_kickoff_recovery_player_id, 
                  own_kickoff_recovery_player_name, blocked_player_id, blocked_player_name, 
                  tackle_for_loss_1_player_id, tackle_for_loss_1_player_name, tackle_for_loss_2_player_id, 
                  tackle_for_loss_2_player_name, qb_hit_1_player_id, qb_hit_1_player_name, 
                  qb_hit_2_player_id, qb_hit_2_player_name, forced_fumble_player_1_team, 
                  forced_fumble_player_1_player_id, forced_fumble_player_1_player_name, 
                  forced_fumble_player_2_team, forced_fumble_player_2_player_id, 
                  forced_fumble_player_2_player_name, solo_tackle_1_team, solo_tackle_2_team, 
                  solo_tackle_1_player_id, solo_tackle_2_player_id, solo_tackle_1_player_name, 
                  solo_tackle_2_player_name, assist_tackle_1_player_id, assist_tackle_1_player_name, 
                  assist_tackle_1_team, assist_tackle_2_player_id, assist_tackle_2_player_name, 
                  assist_tackle_2_team, assist_tackle_3_player_id, assist_tackle_3_player_name, 
                  assist_tackle_3_team, assist_tackle_4_player_id, assist_tackle_4_player_name, 
                  assist_tackle_4_team, pass_defense_1_player_id, pass_defense_1_player_name, 
                  pass_defense_2_player_id, pass_defense_2_player_name, fumbled_1_team, 
                  fumbled_1_player_id, fumbled_1_player_name, fumbled_2_player_id, 
                  fumbled_2_player_name, fumbled_2_team, fumble_recovery_1_team, 
                  fumble_recovery_1_yards, fumble_recovery_1_player_id, fumble_recovery_1_player_name, 
                  fumble_recovery_2_team, fumble_recovery_2_yards, fumble_recovery_2_player_id, 
                  fumble_recovery_2_player_name, return_team, 
                  return_yards, penalty_team, penalty_player_id, 
                  penalty_player_name, penalty_yards, replay_or_challenge, 
                  replay_or_challenge_result, penalty_type,
                  defensive_two_point_attempt,
                  defensive_two_point_conv,
                  defensive_extra_point_attempt,
                  defensive_extra_point_conv) %>%
    return
}

  
#' Scrape season play-by-play for a given NFL season (either pre, regular, or post-season) 
#' 
#' Depending on the year and type of the given game id, this function returns
#' the play-by-play data available from NFL.com from either JSON (games starting
#' in 2009) or parsed HTML (regular and post-season games starting in 1998, 
#' pre-season games starting in 2000). The necessary info regarding the game's 
#' play-by-play data to be scraped (type and season) is provided by the 
#' \code{\link{scrape_game_ids}} function. In addition to all of the play-level data
#' provided by the NFL, outputs from the `nflscrapR` expected points and win 
#' probability models are also included, allowing an individual to conduct their
#' own analysis with more advanced football statistics. A detailed description 
#' of the models methodologies can be found here: \url{https://arxiv.org/abs/1802.00998}.
#' This function acts as a wrapper function for calling the 
#' \code{\link{scrape_game_play_by_play}} function over all of the games meeting
#' the criteria of the user's input.
#' 
#' @param season Numeric 4-digit year associated with an NFL season
#' @param type String indicating the type of game play-by-play, must either be
#' "pre", "reg", or "post" (default is "reg").
#' @param weeks Numeric vector indicating which weeks of preseason (0 to 4 except
#' for 2011 season without Hall of Fame game) or regular season games (1 to 17)
#' to grab (default value of NULL corresponds to selecting all available weeks).
#' @param teams String vector indicating which teams (based on the abbreviation)
#' the function should grab game info for (default value of NULL corresponds to
#' selecting all available teams).
#' @return Data frame where each individual row represents a single play in 
#' the game containing detailed information depending on the availability of 
#' data for the game. See \code{\link{scrape_json_play_by_play}} and 
#' \code{\link{scrape_html_play_by_play}} for documentation about the 
#' columns returned.
#' @examples
#' # Scrape the play-by-play data for the 2017 Super Bowl by first getting all
#' # of the post-season game ids then use the required info to scrape the 
#' # play-by-play data (knowing that it's the last game):
#' playoff_game_ids_17 <- scrape_game_ids(2017, type = "post")
#' sb_17_id <- playoff_game_ids_17$game_id[nrow(playoff_game_ids_17)]
#' sb_17_pbp <- scrape_game_play_by_play(game_id = sb_17_id, type = "post", 
#'                                       season = 2017)
#' @export

scrape_season_play_by_play <- function(season, type = "reg", weeks = NULL, teams = NULL) {
  
  # Gather the game ids based on the inputs:
  game_ids <- scrape_game_ids(season, type, weeks, teams) %>%
    # Remove the pre-season games that were broken in the NFL API:
    dplyr::filter(!(game_id %in% c(2014081503, 2016080751))) %>%
    dplyr::pull(game_id)
  
  # Go through each game and check if the URL exists:
  game_ids_check <- purrr::map_lgl(game_ids,
                                   function(x) {
                                     game_url <- create_game_json_url(x)
                                     RCurl::url.exists(game_url)
                                   })
  
  # If none of the games are available exit:
  assertthat::assert_that(length(game_ids[game_ids_check]) > 0,
                          msg = "There are no games available for your inputs!")
  
  # Scrape each game's play-by-play and return:
  purrr::map_dfr(game_ids[game_ids_check], 
                 function(x) {
                   scrape_game_play_by_play(game_id = x, type, season,
                                            check_url = 0)
                 }) %>% 
    return
}