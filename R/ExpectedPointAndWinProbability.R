################################################################## 
### Win Probabiity and Expected Point Functions for PBP        ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
# Date: 11/01/2016                                               #
################################################################## 
################# Expected Point Function #####################

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
  dataset$down <- factor(dataset$down)
  
  # Create a variable that is time remaining until end of half and game:
  dataset$TimeSecs_Remaining <- ifelse(dataset$qtr %in% c(1,2),
                                       dataset$TimeSecs - 1800,
                                        ifelse(dataset$qtr == 5,
                                               dataset$TimeSecs + 900,
                                               dataset$TimeSecs))
  
  # Ratio of time to yard line
  dataset <- dplyr::mutate(dataset,
                           Time_Yard_Ratio = (1+TimeSecs_Remaining)/(1+yrdline100))
  
  # Define the predict_EP_prob() function:
  # INPUT:  - data: play-by-play dataset
  #         - ep_model: multinom EP model to predict probabilities
  #                     of the next scoring event for basic plays
  #         - fg_model: gam FG model to predict FG success rate
  # OUTPUT: - play-by-play dataset with predicted probabilities for
  #           each of the type of next scoring events, and additionally
  #           the probability of the PAT attempts
  
  predict_EP_prob <- function(data, ep_model, fgxp_model){
    # First get the predictions from the base ep_model:
    base_ep_preds <- as.data.frame(predict(ep_model, newdata = data, type = "probs"))
    colnames(base_ep_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                 "Field_Goal","Safety","Touchdown")
    # ----------------------------------------------------------------------------
    # Now make another dataset that to get the EP probabilities from a missed FG:
    missed_fg_data <- data
    # Subtract 5.065401 from TimeSecs:
    missed_fg_data$TimeSecs_Remaining <- missed_fg_data$TimeSecs_Remaining - 5.065401
    missed_fg_data <- dplyr::mutate(missed_fg_data,Time_Yard_Ratio = (1+TimeSecs_Remaining)/(1+yrdline100))
    
    # Correct the yrdline100:
    missed_fg_data$yrdline100 <- 100 - (missed_fg_data$yrdline100 + 8)
    # Not GoalToGo:
    missed_fg_data$GoalToGo <- rep(0,nrow(data))
    # Now first down:
    missed_fg_data$down <- rep("1",nrow(data))
    # 10 ydstogo:
    missed_fg_data$ydstogo <- rep(10,nrow(data))
    
    # Get the new predicted probabilites:
    missed_fg_ep_preds <- as.data.frame(predict(ep_model, newdata = missed_fg_data, type = "probs"))
    colnames(missed_fg_ep_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                 "Field_Goal","Safety","Touchdown")
    # Find the rows where TimeSecs became 0 or negative and make all the probs equal to 0:
    end_game_i <- which(missed_fg_data$TimeSecs <= 0)
    missed_fg_ep_preds[end_game_i,] <- rep(0,ncol(missed_fg_ep_preds))
    
    # Get the probability of making the field goal:
    make_fg_prob <- as.numeric(predict(fgxp_model, newdata= data, type="response"))
    
    # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
    missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
    # Find the FG attempts:
    fg_attempt_i <- which(data$PlayType %in% c("Field Goal","Run") & !is.na(data$FieldGoalResult))
    
    # Now update the probabilities for the FG attempts (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
    base_ep_preds[fg_attempt_i, "Field_Goal"] <- make_fg_prob[fg_attempt_i] + missed_fg_ep_preds[fg_attempt_i,"Opp_Field_Goal"]
    # Update the other columns based on the opposite possession:
    base_ep_preds[fg_attempt_i, "Touchdown"] <- missed_fg_ep_preds[fg_attempt_i,"Opp_Touchdown"]
    base_ep_preds[fg_attempt_i, "Opp_Field_Goal"] <- missed_fg_ep_preds[fg_attempt_i,"Field_Goal"]
    base_ep_preds[fg_attempt_i, "Opp_Touchdown"] <- missed_fg_ep_preds[fg_attempt_i,"Touchdown"]
    base_ep_preds[fg_attempt_i, "Safety"] <- missed_fg_ep_preds[fg_attempt_i,"Opp_Safety"]
    base_ep_preds[fg_attempt_i, "Opp_Safety"] <- missed_fg_ep_preds[fg_attempt_i,"Safety"]
    base_ep_preds[fg_attempt_i, "No_Score"] <- missed_fg_ep_preds[fg_attempt_i,"No_Score"]
    
    # ----------------------------------------------------------------------------------
    # Calculate the EP for receiving a touchback (from the point of view for recieving team)
    # and update the columns for Kickoff plays:
    kickoff_data <- data
    kickoff_data <- dplyr::mutate(kickoff_data,
                                  Season = as.numeric(substr(as.character(GameID),1,4)),
                                  Month = as.numeric(substr(as.character(GameID),5,6)))
    
    # Change the yard line to be 80 for 2009-2015 and 75 otherwise
    # (accounting for the fact that Jan 2016 is in the 2015 season:
    kickoff_data$yrdline100 <- with(kickoff_data,
                                    ifelse(Season < 2016 | (Season == 2016 & Month < 4), 80, 75))
    # Not GoalToGo:
    kickoff_data$GoalToGo <- rep(0,nrow(data))
    # Now first down:
    kickoff_data$down <- rep("1",nrow(data))
    # 10 ydstogo:
    kickoff_data$ydstogo <- rep(10,nrow(data))
    kickoff_data <- dplyr::mutate(kickoff_data,Time_Yard_Ratio = (1+TimeSecs_Remaining)/(1+yrdline100))
    
    # Get the new predicted probabilites:
    kickoff_preds <- as.data.frame(predict(ep_model, newdata = kickoff_data, type = "probs"))
    colnames(kickoff_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                      "Field_Goal","Safety","Touchdown")
    # Find the FG attempts:
    kickoff_i <- which(data$PlayType == "Kickoff")
    
    # Now update the probabilities:
    base_ep_preds[kickoff_i, "Field_Goal"] <- kickoff_preds[kickoff_i, "Field_Goal"]
    base_ep_preds[kickoff_i, "Touchdown"] <- kickoff_preds[kickoff_i, "Touchdown"]
    base_ep_preds[kickoff_i, "Opp_Field_Goal"] <- kickoff_preds[kickoff_i, "Opp_Field_Goal"]
    base_ep_preds[kickoff_i, "Opp_Touchdown"] <- kickoff_preds[kickoff_i, "Opp_Touchdown"]
    base_ep_preds[kickoff_i, "Safety"] <- kickoff_preds[kickoff_i, "Safety"]
    base_ep_preds[kickoff_i, "Opp_Safety"] <- kickoff_preds[kickoff_i, "Opp_Safety"]
    base_ep_preds[kickoff_i, "No_Score"] <- kickoff_preds[kickoff_i, "No_Score"]
    
    # ----------------------------------------------------------------------------------
    # Insert probabilities of 0 for everything but No_Score for QB Kneels:
    # Find the QB Kneels:
    qb_kneels_i <- which(data$PlayType == "QB Kneel")
    
    # Now update the probabilities:
    base_ep_preds[qb_kneels_i, "Field_Goal"] <- 0
    base_ep_preds[qb_kneels_i, "Touchdown"] <- 0
    base_ep_preds[qb_kneels_i, "Opp_Field_Goal"] <- 0
    base_ep_preds[qb_kneels_i, "Opp_Touchdown"] <- 0
    base_ep_preds[qb_kneels_i, "Safety"] <- 0
    base_ep_preds[qb_kneels_i, "Opp_Safety"] <- 0
    base_ep_preds[qb_kneels_i, "No_Score"] <- 1
    
    
    # ----------------------------------------------------------------------------------
    # Create two new columns, ExPoint_Prob and TwoPoint_Prob, for the PAT events:
    base_ep_preds$ExPoint_Prob <- 0
    base_ep_preds$TwoPoint_Prob <- 0
    
    # Find the indices for these types of plays:
    extrapoint_i <- which(!is.na(data$ExPointResult))
    twopoint_i <- which(!is.na(data$TwoPointConv))
    
    # Assign the make_fg_probs of the extra-point PATs:
    base_ep_preds$ExPoint_Prob[extrapoint_i] <- make_fg_prob[extrapoint_i]
    
    # Assign the TwoPoint_Prob with the historical success rate:
    base_ep_preds$TwoPoint_Prob[twopoint_i] <- 0.4735
    
    # ----------------------------------------------------------------------------------
    # Insert NAs for all other types of plays:
    missing_i <- which(data$PlayType %in% c("Quarter End", "Two Minute Warning", "Timeout",
                                            "End of Game", "Half End"))
    # Now update the probabilities for missing and PATs:
    base_ep_preds$Field_Goal[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    base_ep_preds$Touchdown[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    base_ep_preds$Opp_Field_Goal[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    base_ep_preds$Opp_Touchdown[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    base_ep_preds$Safety[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    base_ep_preds$Opp_Safety[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    base_ep_preds$No_Score[c(missing_i,extrapoint_i,twopoint_i)] <- 0
    
    # Rename the events to all have _Prob at the end of them:
    base_ep_preds <- dplyr::rename(base_ep_preds,Field_Goal_Prob=Field_Goal,Touchdown_Prob=Touchdown,
                                   Opp_Field_Goal_Prob=Opp_Field_Goal,Opp_Touchdown_Prob=Opp_Touchdown,
                                   Safety_Prob=Safety,Opp_Safety_Prob=Opp_Safety,No_Score_Prob=No_Score)  
    
    # Return the final probabilities:
    return(base_ep_preds)
  }
  
  # Use the predict_EP_Prob on the pbp_data:
  pbp_ep_probs <- predict_EP_prob(dataset, current_ep_model4, current_fgxp_model)
  
  # Join them together:
  dataset <- cbind(dataset, pbp_ep_probs)
  
  # Calculate the ExpPts:
  pbp_data_ep <- dplyr::mutate(dataset,
                               ExpPts = (0*No_Score_Prob) + (-3 * Opp_Field_Goal_Prob) + 
                                                (-2 * Opp_Safety_Prob) +
                                                (-7 * Opp_Touchdown_Prob) + (3 * Field_Goal_Prob) +
                                                (2 * Safety_Prob) + (7 * Touchdown_Prob) +
                                                (1 * ExPoint_Prob) + (2 * TwoPoint_Prob))
  
  
  #################################################################
  
  # Calculate EPA:

  ### Adding Expected Points Added (EPA) column 
  ### and Probability Touchdown Added (PTDA) column

  # Create multiple types of EPA columns
  # for each of the possible cases,
  # grouping by GameID (will then just use
  # an ifelse statement to decide which one 
  # to use as the final EPA):
  pbp_data_epa <- dplyr::group_by(pbp_data_ep,GameID)
  pbp_data_epa <- dplyr::mutate(pbp_data_epa,
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
                              EPA_change_score = -7 - ExpPts,
                              # Offense touchdown:
                              EPA_off_td = 7 - ExpPts,
                              # Offense fieldgoal:
                              EPA_off_fg = 3 - ExpPts,
                              # Offense extra-point conversion:
                              EPA_off_ep = 1 - ExpPts,
                              # Offense two-point conversion:
                              EPA_off_tp = 2 - ExpPts,
                              # Opponent Safety:
                              EPA_safety = -2 - ExpPts,
                              # End of half/game or timeout or QB Kneel:
                              EPA_endtime = 0,
                              # Team keeps possession (most general case):
                              PTDA_base = dplyr::lead(Touchdown_Prob) - Touchdown_Prob,
                              # Team keeps possession but either Timeout, Two Minute Warning,
                              # Quarter End is the following row:
                              PTDA_base_nxt = dplyr::lead(Touchdown_Prob,2) - Touchdown_Prob,
                              # Change of possession without defense scoring
                              # and no timeout, two minute warning, or quarter end follows:
                              PTDA_change_no_score = dplyr::lead(Opp_Touchdown_Prob) - Touchdown_Prob,
                              # Change of possession without defense scoring
                              # but either Timeout, Two Minute Warning,
                              # Quarter End is the following row:
                              PTDA_change_no_score_nxt = dplyr::lead(Opp_Touchdown_Prob,2) - Touchdown_Prob,
                              # Change of possession with defense scoring touchdown:
                              PTDA_change_score = 0 - Touchdown_Prob,
                              # Offense touchdown:
                              PTDA_off_td = 1 - Touchdown_Prob,
                              # Offense fieldgoal:
                              PTDA_off_fg = 0 - Touchdown_Prob,
                              # Offense extra-point conversion:
                              PTDA_off_ep = 0 - Touchdown_Prob,
                              # Offense two-point conversion:
                              PTDA_off_tp = 0 - Touchdown_Prob,
                              # Opponent Safety:
                              PTDA_safety = 0 - Touchdown_Prob,
                              # End of half/game or timeout or QB Kneel:
                              PTDA_endtime = 0)

  # Now make the if-else statements to decide which column to use,
  # need to make indicator columns first due to missing values
  # that cause errors with the extra points and two point conversions:
  pbp_data_epa$EPA_base_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & GameID == dplyr::lead(GameID) & Drive == dplyr::lead(Drive) & sp == 0 & dplyr::lead(PlayType) %in% c("Pass","Run","Punt","Sack","Field Goal","No Play","QB Kneel","Spike"),1,0))
  pbp_data_epa$EPA_base_nxt_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & GameID == dplyr::lead(GameID) & Drive == dplyr::lead(Drive) & sp == 0 & dplyr::lead(PlayType) %in% c("Quarter End","Two Minute Warning","Timeout"),1,0))
  pbp_data_epa$EPA_change_no_score_nxt_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & GameID == dplyr::lead(GameID) & sp == 0  & (Drive != dplyr::lead(Drive) | Drive != dplyr::lead(Drive,2)) & dplyr::lead(PlayType) %in% c("Quarter End","Two Minute Warning","Timeout"),1,0))
  pbp_data_epa$EPA_change_no_score_ind <- with(pbp_data_epa,ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & GameID == dplyr::lead(GameID) & sp == 0 & Drive != dplyr::lead(Drive) & dplyr::lead(PlayType) %in% c("Pass","Run","Punt","Sack","Field Goal","No Play","QB Kneel","Spike"),1,0))
  pbp_data_epa$EPA_change_score_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & Touchdown == 1 & (InterceptionThrown == 1 | (Fumble == 1 & RecFumbTeam != posteam)),1,0))
  pbp_data_epa$EPA_off_td_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & Touchdown == 1 & (InterceptionThrown != 1 & Fumble != 1), 1,0))
  pbp_data_epa$EPA_off_fg_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & FieldGoalResult == "Good",1,0))
  pbp_data_epa$EPA_off_ep_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & ExPointResult == "Made",1,0))
  pbp_data_epa$EPA_off_tp_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & TwoPointConv == "Success", 1,0))
  pbp_data_epa$EPA_safety_ind <- with(pbp_data_epa, ifelse(PlayType != "No Play" & PlayType != "Timeout" & PlayType != "Half End" & PlayType != "Quarter End" & PlayType != "End of Game" & PlayType != "QB Kneel" & Safety == 1,1,0))
  pbp_data_epa$EPA_endtime_ind <- with(pbp_data_epa, ifelse(PlayType %in% c("Half End","Quarter End",
                                                                          "End of Game","Timeout","QB Kneel") | (GameID == dplyr::lead(GameID) & Touchdown != 1 & is.na(FieldGoalResult) & is.na(ExPointResult) & is.na(TwoPointConv) & Safety != 1 & ((dplyr::lead(PlayType) %in% c("Half End","End of Game")) | (qtr == 2 & dplyr::lead(qtr)==3) | (qtr == 4 & dplyr::lead(qtr)==5))),1,0))

  # Replace the missings with 0 due to how ifelse treats missings
  pbp_data_epa$EPA_base_ind[is.na(pbp_data_epa$EPA_base_ind)] <- 0 
  pbp_data_epa$EPA_base_nxt_ind[is.na(pbp_data_epa$EPA_base_nxt_ind)] <- 0
  pbp_data_epa$EPA_change_no_score_nxt_ind[is.na(pbp_data_epa$EPA_change_no_score_nxt_ind)] <- 0
  pbp_data_epa$EPA_change_no_score_ind[is.na(pbp_data_epa$EPA_change_no_score_ind)] <- 0 
  pbp_data_epa$EPA_change_score_ind[is.na(pbp_data_epa$EPA_change_score_ind)] <- 0
  pbp_data_epa$EPA_off_td_ind[is.na(pbp_data_epa$EPA_off_td_ind)] <- 0
  pbp_data_epa$EPA_off_fg_ind[is.na(pbp_data_epa$EPA_off_fg_ind)] <- 0
  pbp_data_epa$EPA_off_ep_ind[is.na(pbp_data_epa$EPA_off_ep_ind)] <- 0
  pbp_data_epa$EPA_off_tp_ind[is.na(pbp_data_epa$EPA_off_tp_ind)] <- 0
  pbp_data_epa$EPA_safety_ind[is.na(pbp_data_epa$EPA_safety_ind)] <- 0
  pbp_data_epa$EPA_endtime_ind[is.na(pbp_data_epa$EPA_endtime_ind)] <- 0

  # Assign EPA using these indicator columns: 
  pbp_data_epa$EPA <- with(pbp_data_epa, ifelse(EPA_base_ind == 1,EPA_base,
                                                  ifelse(EPA_base_nxt_ind == 1,EPA_base_nxt,
                                                         ifelse(EPA_change_no_score_nxt_ind == 1,EPA_change_no_score_nxt,
                                                                ifelse(EPA_change_no_score_ind == 1,EPA_change_no_score,
                                                                       ifelse(EPA_change_score_ind == 1,EPA_change_score,
                                                                              ifelse(EPA_off_td_ind == 1,EPA_off_td,
                                                                                     ifelse(EPA_off_fg_ind == 1,EPA_off_fg,
                                                                                            ifelse(EPA_off_ep_ind == 1,EPA_off_ep,
                                                                                                   ifelse(EPA_off_tp_ind == 1,EPA_off_tp,
                                                                                                          ifelse(EPA_safety_ind==1,EPA_safety,
                                                                                                                 ifelse(EPA_endtime_ind==1,EPA_endtime,NA))))))))))))

  # Assign PTDA using these indicator columns: 
  pbp_data_epa$PTDA <- with(pbp_data_epa, ifelse(EPA_base_ind == 1,PTDA_base,
                                               ifelse(EPA_base_nxt_ind == 1,PTDA_base_nxt,
                                                      ifelse(EPA_change_no_score_nxt_ind == 1,PTDA_change_no_score_nxt,
                                                             ifelse(EPA_change_no_score_ind == 1,PTDA_change_no_score,
                                                                    ifelse(EPA_change_score_ind == 1,PTDA_change_score,
                                                                           ifelse(EPA_off_td_ind == 1,PTDA_off_td,
                                                                                  ifelse(EPA_off_fg_ind == 1,PTDA_off_fg,
                                                                                         ifelse(EPA_off_ep_ind == 1,PTDA_off_ep,
                                                                                                ifelse(EPA_off_tp_ind == 1,PTDA_off_tp,
                                                                                                       ifelse(EPA_safety_ind==1,PTDA_safety,
                                                                                                              ifelse(EPA_endtime_ind==1,PTDA_endtime,NA))))))))))))


  # Now drop the unnecessary columns
  pbp_data_epa_final <- dplyr::select(pbp_data_epa, -c(EPA_base,EPA_base_nxt,
                                                     EPA_change_no_score,EPA_change_no_score_nxt,
                                                     EPA_change_score,EPA_off_td,EPA_off_fg,EPA_off_ep,
                                                     EPA_off_tp,EPA_safety,EPA_base_ind,EPA_base_nxt_ind,
                                                     EPA_change_no_score_ind,EPA_change_no_score_nxt_ind,
                                                     EPA_change_score_ind,EPA_off_td_ind,EPA_off_fg_ind,EPA_off_ep_ind,
                                                     EPA_off_tp_ind,EPA_safety_ind, EPA_endtime_ind, EPA_endtime,
                                                     PTDA_base,PTDA_base_nxt,
                                                     PTDA_change_no_score,PTDA_change_no_score_nxt,
                                                     PTDA_change_score,PTDA_off_td,PTDA_off_fg,PTDA_off_ep,
                                                     PTDA_off_tp,PTDA_safety,PTDA_endtime))

  
  # Return the dataset
  
  return(dplyr::ungroup(pbp_data_epa_final))
  
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

