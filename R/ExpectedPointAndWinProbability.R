################################################################## 
### Win Probabiity and Expected Point Functions for PBP        ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
# Date: 11/01/2016                                               #
################################################################## 
################# Expected Point Fucntion #####################

## Still need to add an update model.  How do we add models to nflscrapR

#' Expected point function to add an expected point variable for each play in
#' the play by play
#' @description This function takes in the output of the game level play by play
#' function and returns the same dataframe with a new column added for expected
#' points for each valid play.  The expected point model can loaded in the same 
#' way that datasets can be loaded for this package
#' @param dataset (data.frame object) A data.frame as exported from the 
#' game_play_by_play function
#' @return The input dataframe with the addition of an expected point column
#' @export
expected_points <- function(dataset) {
  
  # Changing down into a factor variable
  # Adding NA as a level in the factor
  dataset$down <- factor(dataset$down)
  dataset$down <- addNA(dataset$down)
  
  # Changing quarter to factor
  dataset$qtr <- factor(dataset$qtr)
  
  # Beg and end quarter efect 
  dataset$begQeffect <- ifelse(dataset$TimeSecs >= 3100 |
                                 is.element(dataset$TimeSecs,1500:1800),
                               1, 0)
  
  dataset$endQeffect <- ifelse(dataset$TimeSecs <= 500 |
                                 is.element(dataset$TimeSecs, 
                                            2200:1800), 1, 0)
  
  #################################################################
  # Scoring matrix to weight probabilities of different score types
  scoring.weights <- matrix(c(0, -3, -2, -7, 3, 2, 7), ncol = 1)
  
  dataset$ExpPts <- as.numeric(round(predict(multi.w.time.int2, 
                                          newdata = dataset, 
         type = "prob") %*% scoring.weights, 2))
  
  ## Removing Expected Points From Timeouts, Quarter End, End of Game, 
  ## Two Minute Warning
  
  playtypes.no.ep <- which(dataset$PlayType %in% c("Timeouts", "Quarter End", 
                                                   "End of Game",
                                                   "Two Minute Warning"))
  
  dataset$ExpPts[playtypes.no.ep] <- NA
  
  ### Adding Expected Points Added (EPA) Column ###
  
  # Create multiple types of EPA columns
  # for each of the possible cases,
  # grouping by GameID (will then just use
  # an ifelse statement to decide which one 
  # to use as the final EPA):
  dataset <- dplyr::group_by(dataset,GameID)
  dataset <- dplyr::mutate(dataset,
                           # Team keeps possession (most general case):
                           EPA_base = dplyr::lead(ExpPts) - ExpPts,
                           # Team keeps possession but either Timeout, Two Minute Warning,
                           # Quarter End is the following row:
                           EPA_base_nxt = dplyr::lead(ExpPts,2) - ExpPts,
                           # Change of possession without defense scoring
                           # and no timeout, two minute warning, or quarter end follows:
                           EPA_change_no_score = -dplyr::lead(ExpPts) - ExpPts,
                           # Change of possession without defense scoring
                           # but either Timeout, Two Minute Warning,
                           # Quarter End is the following row:
                           EPA_change_no_score_nxt = -dplyr::lead(ExpPts,2) - ExpPts,
                           # Change of possession with defense scoring touchdown:
                           EPA_change_score = -6 - ExpPts,
                           # Offense touchdown:
                           EPA_off_td = 6 - ExpPts,
                           # Offense fieldgoal:
                           EPA_off_fg = 3 - ExpPts,
                           # Offense extra-point conversion:
                           EPA_off_ep = 1 - ExpPts,
                           # Offense two-point conversion:
                           EPA_off_tp = 2 - ExpPts,
                           # Safety:
                           EPA_safety = -2 - ExpPts)
  
  # Now make the if-else statements to decide which column to use,
  # need to make indicator columns first due to missing values
  # that cause errors with the extra points and two point conversions:
  dataset$EPA_base_ind <- with(dataset, ifelse(PlayType != "No Play" & GameID == dplyr::lead(GameID) & Drive == dplyr::lead(Drive) & sp == 0 & dplyr::lead(PlayType) %in% c("Pass","Run","Punt","Sack","Field Goal","No Play","QB Kneel","Spike"),1,0))
  dataset$EPA_base_nxt_ind <- with(dataset, ifelse(PlayType != "No Play" & GameID == dplyr::lead(GameID) & Drive == dplyr::lead(Drive) & sp == 0 & dplyr::lead(PlayType) %in% c("Quarter End","Two Minute Warning","Timeout"),1,0))
  dataset$EPA_change_no_score_nxt_ind <- with(dataset, ifelse(PlayType != "No Play" & GameID == dplyr::lead(GameID) & sp == 0  & (Drive != dplyr::lead(Drive) | Drive != dplyr::lead(Drive,2)) & dplyr::lead(PlayType) %in% c("Quarter End","Two Minute Warning","Timeout"),1,0))
  dataset$EPA_change_no_score_ind <- with(dataset,ifelse(PlayType != "No Play" & GameID == dplyr::lead(GameID) & sp == 0 & Drive != dplyr::lead(Drive) & dplyr::lead(PlayType) %in% c("Pass","Run","Punt","Sack","Field Goal","No Play","QB Kneel","Spike"),1,0))
  dataset$EPA_change_score_ind <- with(dataset, ifelse(PlayType != "No Play" & Touchdown == 1 & (InterceptionThrown == 1 | (Fumble == 1 & RecFumbTeam != posteam)),1,0))
  dataset$EPA_off_td_ind <- with(dataset, ifelse(PlayType != "No Play" & Touchdown == 1 & (InterceptionThrown != 1 & Fumble != 1), 1,0))
  dataset$EPA_off_fg_ind <- with(dataset, ifelse(PlayType != "No Play" & FieldGoalResult == "Good",1,0))
  dataset$EPA_off_ep_ind <- with(dataset, ifelse(PlayType != "No Play" & ExPointResult == "Made",1,0))
  dataset$EPA_off_tp_ind <- with(dataset, ifelse(PlayType != "No Play" & TwoPointConv == "Success", 1,0))
  dataset$EPA_safety_ind <- with(dataset, ifelse(PlayType != "No Play" & Safety == 1,1,0))
  
  # Replace the missings with 0 due to how ifelse treats missings
  dataset$EPA_base_ind[is.na(dataset$EPA_base_ind)] <- 0 
  dataset$EPA_base_nxt_ind[is.na(dataset$EPA_base_nxt_ind)] <- 0
  dataset$EPA_change_no_score_nxt_ind[is.na(dataset$EPA_change_no_score_nxt_ind)] <- 0
  dataset$EPA_change_no_score_ind[is.na(dataset$EPA_change_no_score_ind)] <- 0 
  dataset$EPA_change_score_ind[is.na(dataset$EPA_change_score_ind)] <- 0
  dataset$EPA_off_td_ind[is.na(dataset$EPA_off_td_ind)] <- 0
  dataset$EPA_off_fg_ind[is.na(dataset$EPA_off_fg_ind)] <- 0
  dataset$EPA_off_ep_ind[is.na(dataset$EPA_off_ep_ind)] <- 0
  dataset$EPA_off_tp_ind[is.na(dataset$EPA_off_tp_ind)] <- 0
  dataset$EPA_safety_ind[is.na(dataset$EPA_safety_ind)] <- 0
  
  # Assign EPA using these indicator columns: 
  dataset$EPA <- with(dataset, ifelse(EPA_base_ind == 1,EPA_base,
                                      ifelse(EPA_base_nxt_ind == 1,EPA_base_nxt,
                                             ifelse(EPA_change_no_score_nxt_ind == 1,EPA_change_no_score_nxt,
                                                    ifelse(EPA_change_no_score_ind == 1,EPA_change_no_score,
                                                           ifelse(EPA_change_score_ind == 1,EPA_change_score,
                                                                  ifelse(EPA_off_td_ind == 1,EPA_off_td,
                                                                         ifelse(EPA_off_fg_ind == 1,EPA_off_fg,
                                                                                ifelse(EPA_off_ep_ind == 1,EPA_off_ep,
                                                                                       ifelse(EPA_off_tp_ind == 1,EPA_off_tp,
                                                                                              ifelse(EPA_safety_ind==1,EPA_safety,NA)))))))))))
  
  # Now drop the unnecessary columns
  dataset <- dplyr::select(dataset, -c(EPA_base,EPA_base_nxt,
                                       EPA_change_no_score,EPA_change_no_score_nxt,
                                       EPA_change_score,EPA_off_td,EPA_off_fg,EPA_off_ep,
                                       EPA_off_tp,EPA_safety,EPA_base_ind,EPA_base_nxt_ind,
                                       EPA_change_no_score_ind,EPA_change_no_score_nxt_ind,
                                       EPA_change_score_ind,EPA_off_td_ind,EPA_off_fg_ind,EPA_off_ep_ind,
                                       EPA_off_tp_ind,EPA_safety_ind,begQeffect,endQeffect))
  
  # Drop the factor unused levels from down
  
  dataset$down <- droplevels(dataset$down, levels = "NA")
  
  # Return the dataset
  
  return(dplyr::ungroup(dataset))
  
}

################# Win Probability Fucntion #####################

#' Win probability function to add win probability columns for the offense and 
#' defense for each play in the game
#' @description This function takes in the output of the game level play by play
#' function and returns the same dataframe with six new columns representing the
#' win probability for the home and away team pre and post snap and win probability
#' added.  The model used to calculate win probability is a genarlized addative
#'  model with three explanatory variables.
#' @param dataset (data.frame object) A data.frame as exported from the 
#' game_play_by_play function
#' @return The input dataframe with the addition of four win probability columns 
#' for the home and away 
#' @export
win_probability <- function(dataset) {
  
  dataset$down <- as.character(dataset$down)
  # Categorize special teams plays into one play type
  dataset$down[which(dataset$PlayType == "Kickoff")] <- "SpecialTeams"
  dataset$down[which(dataset$PlayType == "Extra Point")] <- "SpecialTeams"
  dataset$down[which(dataset$PlayType == "Onside Kick")] <- "SpecialTeams"
  dataset$down[which(!is.na(dataset$TwoPointConv))] <- "SpecialTeams"
  
  dataset$down <- as.factor(dataset$down)
  
  ## Add inverse time variable ##
  dataset$invtime <- 1/(dataset$TimeSecs + .0000000001)
  
  OffWinProb <- as.numeric(round(mgcv::predict.gam(object = win.prob.model.gam3, 
                                      newdata = dataset, type = "response"), 3))
  
  DefWinProb <- 1 - OffWinProb

  ####### Creating Home team and Away team Win probability Pre-play,     #######
  ####### Post-play, and win prob added                                  ####### 
  
  # Home: Pre-play
  
  dataset$Home.WP.pre <- NA

  dataset$Home.WP.pre[which(dataset$posteam == dataset$HomeTeam)] <- OffWinProb[which(dataset$posteam == dataset$HomeTeam)]
  dataset$Home.WP.pre[which(dataset$DefensiveTeam == dataset$HomeTeam)] <- DefWinProb[which(dataset$DefensiveTeam == dataset$HomeTeam)]
  
  # Away: Pre-play
  dataset$Away.WP.pre <- NA
  
  dataset$Away.WP.pre[which(dataset$posteam == dataset$AwayTeam)] <- OffWinProb[which(dataset$posteam == dataset$AwayTeam)]
  dataset$Away.WP.pre[which(dataset$DefensiveTeam == dataset$AwayTeam)] <- DefWinProb[which(dataset$DefensiveTeam == dataset$AwayTeam)]
  
  ####### Adding Post Play Win Prob and WPA #######
  
  # Post Play Win Prob - Home Team #
  dataset$Home.WP.post <- NA
  dataset$Home.WP.post[!is.na(dataset$Home.WP.pre)] <- dplyr::lead(dataset$Home.WP.pre[!is.na(dataset$Home.WP.pre)])
  
  # Post Play Win Prob - Away Team #
  dataset$Away.WP.post <- NA
  dataset$Away.WP.post[!is.na(dataset$Away.WP.pre)] <- dplyr::lead(dataset$Away.WP.pre[!is.na(dataset$Away.WP.pre)])
  
  ####### Adding Win Probability Added #######
  
  # Home
  dataset$Home.WPA <- round(dataset$Home.WP.post - dataset$Home.WP.pre, 2)
  
  # Away
  dataset$Away.WPA <- round(dataset$Away.WP.post - dataset$Away.WP.pre, 2)
  
  # WPA for the play
  dataset$WPA <- ifelse(dataset$posteam == dataset$HomeTeam,dataset$Home.WPA,dataset$Away.WPA)
  
  ### Formatting ###
  ## Change down back to numeric ##
  
  dataset$down[which(dataset$down == "SpecialTeams")] <- NA
  
  dataset$down <- as.numeric(dataset$down)
  
  invtime.index <- which(colnames(dataset) == "invtime")
  
  ## Final output dataframe ##
  dataset[,-invtime.index]
}

