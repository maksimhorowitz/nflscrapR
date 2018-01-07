################################################################## 
### Win Probabiity and Expected Point Functions for PBP        ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
# Date: 11/01/2016                                               #
################################################################## 
################# Expected Point Function #####################

#' Expected point function to calculate expected points for each play in
#' the play by play, and the expected points added in three ways, 
#' basic EPA, air yards EPA, and yards after catch EPA 
#' @description This function takes in the output of the game level play by play
#' function and returns the same dataframe with a new column added for expected
#' points for each valid play.  The expected point model can loaded in the same 
#' way that datasets can be loaded for this package
#' @param dataset (data.frame object) A data.frame as exported from the 
#' game_play_by_play function
#' @return The input dataframe with the addition of expected points columns
#' @export
expected_points <- function(dataset) {
  
  # Changing down into a factor variable
  dataset$down <- factor(dataset$down)
  
  # Add columns for season and month:
  dataset <- dplyr::mutate(dataset,
                           Season = as.numeric(substr(as.character(GameID),1,4)),
                           Month = as.numeric(substr(as.character(GameID),5,6)))
  
  # Create a variable that is time remaining until end of half and game:
  dataset$TimeSecs_Remaining <- ifelse(dataset$qtr %in% c(1,2),
                                       dataset$TimeSecs - 1800,
                                       ifelse(dataset$qtr == 5 & 
                                                (dataset$Season == 2017 & 
                                                   dataset$Month > 4),
                                              dataset$TimeSecs + 600,
                                              ifelse(dataset$qtr == 5 &
                                                       (dataset$Season < 2017 |
                                                          (dataset$Season == 2017 &
                                                             dataset$Month <= 4)),
                                                     dataset$TimeSecs + 900,
                                                     dataset$TimeSecs)))
  
  # Create log_ydstogo:
  dataset <- dplyr::mutate(dataset, log_ydstogo = log(ydstogo))
  
  # Create Under_TwoMinute_Warning indicator
  dataset$Under_TwoMinute_Warning <- ifelse(dataset$TimeSecs_Remaining < 120,1,0)
  
  
  # Define the predict_EP_prob() function:
  # INPUT:  - data: play-by-play dataset
  #         - ep_model: multinom EP model to predict probabilities
  #                     of the next scoring event for basic plays
  #         - fg_model: bam FG model to predict FG success rate
  # OUTPUT: - play-by-play dataset with predicted probabilities for
  #           each of the type of next scoring events, and additionally
  #           the probability of the PAT attempts
  
  predict_EP_prob <- function(data, ep_model, fgxp_model){
    # First get the predictions from the base ep_model:
    if (nrow(data) > 1) {
      base_ep_preds <- as.data.frame(predict(ep_model, newdata = data, type = "probs"))
    } else{
      base_ep_preds <- as.data.frame(matrix(predict(ep_model, newdata = data, type = "probs"),
                                                       ncol = 7))
    }
    colnames(base_ep_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                 "Field_Goal","Safety","Touchdown")
    # ----------------------------------------------------------------------------
    # Now make another dataset that to get the EP probabilities from a missed FG:
    missed_fg_data <- data
    # Subtract 5.065401 from TimeSecs:
    missed_fg_data$TimeSecs_Remaining <- missed_fg_data$TimeSecs_Remaining - 5.065401

    # Correct the yrdline100:
    missed_fg_data$yrdline100 <- 100 - (missed_fg_data$yrdline100 + 8)
    # Not GoalToGo:
    missed_fg_data$GoalToGo <- rep(0,nrow(data))
    # Now first down:
    missed_fg_data$down <- rep("1",nrow(data))
    # 10 ydstogo:
    missed_fg_data$ydstogo <- rep(10,nrow(data))
    # Create log_ydstogo:
    missed_fg_data <- dplyr::mutate(missed_fg_data, log_ydstogo = log(ydstogo))
    
    # Create Under_TwoMinute_Warning indicator
    missed_fg_data$Under_TwoMinute_Warning <- ifelse(missed_fg_data$TimeSecs_Remaining < 120,1,0)
    
    # Get the new predicted probabilites:
    if (nrow(missed_fg_data) > 1) {
      missed_fg_ep_preds <- as.data.frame(predict(ep_model, newdata = missed_fg_data, type = "probs"))
    } else{
      missed_fg_ep_preds <- as.data.frame(matrix(predict(ep_model, newdata = missed_fg_data, type = "probs"),
                                            ncol = 7))
    }
    colnames(missed_fg_ep_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                 "Field_Goal","Safety","Touchdown")
    # Find the rows where TimeSecs_Remaining became 0 or negative and make all the probs equal to 0:
    end_game_i <- which(missed_fg_data$TimeSecs_Remaining <= 0)
    missed_fg_ep_preds[end_game_i,] <- rep(0,ncol(missed_fg_ep_preds))
    
    # Get the probability of making the field goal:
    make_fg_prob <- as.numeric(mgcv::predict.bam(fgxp_model, newdata= data, type="response"))
    
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
    # Create log_ydstogo:
    kickoff_data <- dplyr::mutate(kickoff_data, log_ydstogo = log(ydstogo))
    
    # Get the new predicted probabilites:
    if (nrow(kickoff_data) > 1) {
      kickoff_preds <- as.data.frame(predict(ep_model, newdata = kickoff_data, type = "probs"))
    } else{
      kickoff_preds <- as.data.frame(matrix(predict(ep_model, newdata = kickoff_data, type = "probs"),
                                                 ncol = 7))
    }
    colnames(kickoff_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                      "Field_Goal","Safety","Touchdown")
    # Find the kickoffs:
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
  pbp_ep_probs <- predict_EP_prob(dataset, ep_model, fg_model)
  
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
                                # Offense touchdown (including kickoff returns):
                                EPA_off_td = 7 - ExpPts,
                                # Offense fieldgoal:
                                EPA_off_fg = 3 - ExpPts,
                                # Offense extra-point conversion:
                                EPA_off_ep = 1 - ExpPts,
                                # Offense two-point conversion:
                                EPA_off_tp = 2 - ExpPts,
                                # Missing PAT:
                                EPA_PAT_fail = 0 - ExpPts,
                                # Opponent Safety:
                                EPA_safety = -2 - ExpPts,
                                # End of half/game or timeout or QB Kneel:
                                EPA_endtime = 0,
                                # Defense scoring touchdown (including punt returns):
                                EPA_change_score = -7 - ExpPts,
                                # Change of possession without defense scoring
                                # and no timeout, two minute warning, or quarter end follows:
                                EPA_change_no_score = -dplyr::lead(ExpPts) - ExpPts,
                                # Change of possession without defense scoring
                                # but either Timeout, Two Minute Warning,
                                # Quarter End is the following row:
                                EPA_change_no_score_nxt = -dplyr::lead(ExpPts,2) - ExpPts,
                                # Team keeps possession but either Timeout, Two Minute Warning,
                                # Quarter End is the following row:
                                EPA_base_nxt = dplyr::lead(ExpPts,2) - ExpPts,
                                # Team keeps possession (most general case):
                                EPA_base = dplyr::lead(ExpPts) - ExpPts,
                                # Now the same for PTDA:
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
                                PTDA_off_ep = 0,
                                # Offense two-point conversion:
                                PTDA_off_tp = 0,
                                # Offense PAT fail:
                                PTDA_PAT_fail = 0,
                                # Opponent Safety:
                                PTDA_safety = 0 - Touchdown_Prob,
                                # End of half/game or timeout or QB Kneel:
                                PTDA_endtime = 0)

  # Define the scoring plays first:
  pbp_data_epa$EPA_off_td_ind <- with(pbp_data_epa, 
                                      ifelse(sp == 1 & Touchdown == 1 & 
                                               ((PlayType %in% c("Pass","Run") &
                                               (InterceptionThrown != 1 & 
                                                  Fumble != 1)) | (PlayType == "Kickoff" &
                                                                     ReturnResult == "Touchdown")), 1,0))
  pbp_data_epa$EPA_off_fg_ind <- with(pbp_data_epa,
                                      ifelse(PlayType %in% c("Field Goal","Run") &
                                               FieldGoalResult == "Good", 1, 0))
  pbp_data_epa$EPA_off_ep_ind <- with(pbp_data_epa,
                                      ifelse(ExPointResult == "Made" & PlayType != "No Play", 1, 0))
  pbp_data_epa$EPA_off_tp_ind <- with(pbp_data_epa,
                                      ifelse(TwoPointConv == "Success" & PlayType != "No Play", 1, 0))
  pbp_data_epa$EPA_PAT_fail_ind <- with(pbp_data_epa,
                                        ifelse(PlayType != "No Play" & (ExPointResult %in% c("Missed", "Aborted", "Blocked") | 
                                                                          TwoPointConv == "Failure"), 1, 0))
  pbp_data_epa$EPA_safety_ind <- with(pbp_data_epa,
                                      ifelse(PlayType != "No Play" & Safety == 1, 1, 0))
  pbp_data_epa$EPA_endtime_ind <- with(pbp_data_epa,
                                       ifelse(PlayType %in% c("Half End","Quarter End",
                                                              "End of Game","Timeout","QB Kneel") | 
                                                (GameID == dplyr::lead(GameID) & sp != 1 & 
                                                   Touchdown != 1 & 
                                                   is.na(FieldGoalResult) & is.na(ExPointResult) & 
                                                   is.na(TwoPointConv) & Safety != 1 & 
                                                   ((dplyr::lead(PlayType) %in% c("Half End","End of Game")) | 
                                                      (qtr == 2 & dplyr::lead(qtr)==3) | 
                                                      (qtr == 4 & dplyr::lead(qtr)==5))),1,0))
  pbp_data_epa$EPA_change_score_ind <- with(pbp_data_epa,
                                            ifelse(PlayType != "No Play" & sp == 1 & 
                                                     Touchdown == 1 & 
                                                     (InterceptionThrown == 1 | 
                                                        (Fumble == 1 & RecFumbTeam != posteam) |
                                                        (PlayType == "Punt" & ReturnResult == "Touchdown")), 1, 0))
  pbp_data_epa$EPA_change_no_score_nxt_ind <- with(pbp_data_epa,
                                               ifelse(GameID == dplyr::lead(GameID) & 
                                                        GameID == dplyr::lead(GameID,2) &
                                                        sp != 1  & 
                                                        dplyr::lead(PlayType) %in% c("Quarter End",
                                                                                     "Two Minute Warning",
                                                                                     "Timeout") &
                                                        (Drive != dplyr::lead(Drive,2)) &
                                                        (posteam != dplyr::lead(posteam,2)), 1, 0))
  pbp_data_epa$EPA_base_nxt_ind <- with(pbp_data_epa,
                                        ifelse(GameID == dplyr::lead(GameID) & 
                                                 GameID == dplyr::lead(GameID,2) &
                                                 sp != 1  & 
                                                 dplyr::lead(PlayType) %in% c("Quarter End",
                                                                              "Two Minute Warning",
                                                                              "Timeout") &
                                                 (Drive == dplyr::lead(Drive,2)), 1, 0))
  pbp_data_epa$EPA_change_no_score_ind <- with(pbp_data_epa,
                                               ifelse(GameID == dplyr::lead(GameID) & 
                                                        Drive != dplyr::lead(Drive) &
                                                        posteam != dplyr::lead(posteam) &
                                                        dplyr::lead(PlayType) %in% 
                                                        c("Pass","Run","Punt","Sack",
                                                          "Field Goal","No Play",
                                                          "QB Kneel","Spike"), 1, 0))
  
  
  # Replace the missings with 0 due to how ifelse treats missings
  pbp_data_epa$EPA_PAT_fail_ind[is.na(pbp_data_epa$EPA_PAT_fail_ind)] <- 0 
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
  pbp_data_epa$EPA <- with(pbp_data_epa,
                           ifelse(EPA_off_td_ind == 1, EPA_off_td,
                                  ifelse(EPA_off_fg_ind == 1, EPA_off_fg,
                                         ifelse(EPA_off_ep_ind == 1, EPA_off_ep,
                                                ifelse(EPA_off_tp_ind == 1, EPA_off_tp,
                                                       ifelse(EPA_PAT_fail_ind == 1, EPA_PAT_fail,
                                                              ifelse(EPA_safety_ind == 1, EPA_safety,
                                                                     ifelse(EPA_endtime_ind == 1, EPA_endtime,
                                                                            ifelse(EPA_change_score_ind == 1, EPA_change_score,
                                                                                   ifelse(EPA_change_no_score_nxt_ind == 1, EPA_change_no_score_nxt,
                                                                                          ifelse(EPA_base_nxt_ind == 1, EPA_base_nxt,
                                                                                                 ifelse(EPA_change_no_score_ind == 1, EPA_change_no_score,
                                                                                                        EPA_base))))))))))))

  # Assign PTDA using these indicator columns: 
  pbp_data_epa$PTDA <- with(pbp_data_epa,
                           ifelse(EPA_off_td_ind == 1, PTDA_off_td,
                                  ifelse(EPA_off_fg_ind == 1, PTDA_off_fg,
                                         ifelse(EPA_off_ep_ind == 1, PTDA_off_ep,
                                                ifelse(EPA_off_tp_ind == 1, PTDA_off_tp,
                                                       ifelse(EPA_PAT_fail_ind == 1, PTDA_PAT_fail,
                                                              ifelse(EPA_safety_ind == 1, PTDA_safety,
                                                                     ifelse(EPA_endtime_ind == 1, PTDA_endtime,
                                                                            ifelse(EPA_change_score_ind == 1, PTDA_change_score,
                                                                            ifelse(EPA_change_no_score_nxt_ind == 1, PTDA_change_no_score_nxt,
                                                                                   ifelse(EPA_base_nxt_ind == 1, PTDA_base_nxt,
                                                                                          ifelse(EPA_change_no_score_ind == 1, PTDA_change_no_score,
                                                                                                 PTDA_base))))))))))))

  # Now drop the unnecessary columns
  pbp_data_epa_final <- dplyr::select(pbp_data_epa, -c(EPA_base,EPA_base_nxt,
                                                     EPA_change_no_score,EPA_change_no_score_nxt,
                                                     EPA_change_score,EPA_off_td,EPA_off_fg,EPA_off_ep,
                                                     EPA_off_tp,EPA_safety,EPA_base_nxt_ind,
                                                     EPA_change_no_score_ind,EPA_change_no_score_nxt_ind,
                                                     EPA_change_score_ind,EPA_off_td_ind,EPA_off_fg_ind,EPA_off_ep_ind,
                                                     EPA_off_tp_ind,EPA_safety_ind, EPA_endtime_ind, EPA_endtime,
                                                     PTDA_base,PTDA_base_nxt,PTDA_PAT_fail,
                                                     PTDA_change_no_score,PTDA_change_no_score_nxt,
                                                     PTDA_change_score,PTDA_off_td,PTDA_off_fg,PTDA_off_ep,
                                                     PTDA_off_tp,PTDA_safety,PTDA_endtime, EPA_PAT_fail, EPA_PAT_fail_ind))

  
  # Ungroup the dataset
  pbp_data_epa_final <- dplyr::ungroup(pbp_data_epa_final)

  # Calculate the airEPA and yacEPA:
  
  # Find all Pass Attempts that are also actual plays,
  pass_plays_i <- which(pbp_data_epa_final$PassAttempt == 1 & pbp_data_epa_final$PlayType != "No Play")
  
  # Create a dataframe of pass_plays:
  
  pass_pbp_data <- pbp_data_epa_final[pass_plays_i,]
  
  # Using the AirYards need to update the following:
  # - yrdline100
  # - TimeSecs_Remaining
  # - GoalToGo
  # - ydstogo
  # - log_ydstogo
  # - Under_TwoMinute_Warning
  # - down
  
  # First rename the old columns to update for calculating the EP from the air:
  pass_pbp_data <- dplyr::rename(pass_pbp_data,old_yrdline100=yrdline100,
                                 old_ydstogo=ydstogo, old_TimeSecs_Remaining=TimeSecs_Remaining,
                                 old_GoalToGo=GoalToGo,old_down=down)
  
  # Create an indicator column for the air yards failing to convert the first down:
  pass_pbp_data$Turnover_Ind <- ifelse(pass_pbp_data$old_down == 4 & pass_pbp_data$AirYards < pass_pbp_data$old_ydstogo,
                                       1,0)
  # Adjust the field position variables:
  pass_pbp_data$yrdline100 <- ifelse(pass_pbp_data$Turnover_Ind == 0,
                                     pass_pbp_data$old_yrdline100 - pass_pbp_data$AirYards,
                                     100 - (pass_pbp_data$old_yrdline100 - pass_pbp_data$AirYards))
  
  pass_pbp_data$ydstogo <- ifelse(pass_pbp_data$AirYards >= pass_pbp_data$old_ydstogo | 
                                    pass_pbp_data$Turnover_Ind == 1,
                                  10, pass_pbp_data$old_ydstogo - pass_pbp_data$AirYards)
  # Create log_ydstogo:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data, log_ydstogo = log(ydstogo))
  
  pass_pbp_data$down <- ifelse(pass_pbp_data$AirYards >= pass_pbp_data$old_ydstogo | 
                                 pass_pbp_data$Turnover_Ind == 1,
                               1, as.numeric(pass_pbp_data$old_down) + 1)
  
  pass_pbp_data$GoalToGo <- ifelse((pass_pbp_data$old_GoalToGo == 1 & pass_pbp_data$Turnover_Ind==0) |
                                     (pass_pbp_data$Turnover_Ind == 0 & pass_pbp_data$old_GoalToGo == 0 & pass_pbp_data$yrdline100 <= 10) |
                                     (pass_pbp_data$Turnover_Ind == 1 & pass_pbp_data$yrdline100 <= 10),1,0)
  
  # Adjust the time with the average incomplete pass time:
  pass_pbp_data$TimeSecs_Remaining <- pass_pbp_data$old_TimeSecs_Remaining - 5.704673
  
  # Create Under_TwoMinute_Warning indicator
  pass_pbp_data$Under_TwoMinute_Warning <- ifelse(pass_pbp_data$TimeSecs_Remaining < 120,1,0)
  
  # Make the new down a factor:
  pass_pbp_data$down <- as.factor(pass_pbp_data$down)
  
  # Get the new predicted probabilites:
  if (nrow(pass_pbp_data) > 1) {
    pass_pbp_data_preds <- as.data.frame(predict(ep_model, newdata = pass_pbp_data, type = "probs"))
  } else{
    pass_pbp_data_preds <- as.data.frame(matrix(predict(ep_model, newdata = pass_pbp_data, type = "probs"),
                                          ncol = 7))
  }
  colnames(pass_pbp_data_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                               "Field_Goal","Safety","Touchdown")
  # Convert to air EP:
  pass_pbp_data_preds <- dplyr::mutate(pass_pbp_data_preds, airEP = (Opp_Safety*-2) + (Opp_Field_Goal*-3) + 
                                   (Opp_Touchdown*-7) + (Safety*2) + (Field_Goal*3) + (Touchdown*7))
  
  # Return back to the passing data:
  pass_pbp_data$airEP <- pass_pbp_data_preds$airEP
  
  # For the plays that have TimeSecs_Remaining 0 or less, set airEP to 0:
  pass_pbp_data$airEP[which(pass_pbp_data$TimeSecs_Remaining<=0)] <- 0
  
  # Calculate the airEPA based on 4 scenarios:
  pass_pbp_data$airEPA <- with(pass_pbp_data, ifelse(old_yrdline100 - AirYards <= 0,
                                                     7 - ExpPts,
                                                     ifelse(old_yrdline100 - AirYards > 99,
                                                            -2 - ExpPts,
                                                            ifelse(Turnover_Ind == 1,
                                                                   (-1*airEP) - ExpPts,airEP - ExpPts))))
  
  # If the play is a two-point conversion then change the airEPA to NA since
  # no air yards are provided:
  pass_pbp_data$airEPA <- with(pass_pbp_data, ifelse(!is.na(TwoPointConv),
                                                     NA,airEPA))
  
  # Calculate the yards after catch EPA:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data,yacEPA = EPA - airEPA)
  
  # if Yards after catch is 0 make yacEPA set to 0:
  pass_pbp_data$yacEPA <- ifelse(pass_pbp_data$YardsAfterCatch == 0 & pass_pbp_data$Reception==1,
                                 0, pass_pbp_data$yacEPA)
  # if Yards after catch is 0 make airEPA set to EPA:
  pass_pbp_data$airEPA <- ifelse(pass_pbp_data$YardsAfterCatch == 0 & pass_pbp_data$Reception==1,
                                 pass_pbp_data$EPA, pass_pbp_data$airEPA)
  
  # Now add airEPA and yacEPA to the original dataset:
  pbp_data_epa_final$airEPA <- NA
  pbp_data_epa_final$yacEPA <- NA
  pbp_data_epa_final$airEPA[pass_plays_i] <- pass_pbp_data$airEPA
  pbp_data_epa_final$yacEPA[pass_plays_i] <- pass_pbp_data$yacEPA
  
  # Return the dataset:
  return(pbp_data_epa_final)
  
}

################# Win Probability Function #####################

#' Win probability function to add win probability columns for the home and 
#' away teams for each play in the game
#' @description This function takes in the output of the game level play by play
#' function and returns the same dataframe with six new columns representing the
#' win probability for the home and away team pre and post snap and win probability
#' added.  The model used to calculate win probability is a generalized additive
#' model using the expected score differential, time remaining in game,
#' also accounting for the timeouts remaining for both teams. Overtime win 
#' probability is calculated separately using the scoring event probabilities.
#' @param dataset (data.frame object) A data.frame as exported from the 
#' game_play_by_play function
#' @return The input dataframe with the addition of six win probability columns 
#' giving the win probability for the possession team, the WPA for each play,
#' the pre and post win probability for both the home and away teams.
#' @export
win_probability <- function(dataset) {
  # Initialize the vector to store the predicted win probability
  # with respect to the possession team:
  OffWinProb <- rep(NA, nrow(dataset))
  
  # Changing down into a factor variable
  dataset$down <- factor(dataset$down)
  
  # Get the Season and Month for rule changes:
  dataset <- dplyr::mutate(dataset,
                           Season = as.numeric(substr(as.character(GameID),1,4)),
                           Month = as.numeric(substr(as.character(GameID),5,6)))

  
  # Create a variable that is time remaining until end of half and game:
  dataset$TimeSecs_Remaining <- ifelse(dataset$qtr %in% c(1,2),
                                       dataset$TimeSecs - 1800,
                                       ifelse(dataset$qtr == 5 & 
                                                (dataset$Season == 2017 & 
                                                   dataset$Month > 4),
                                              dataset$TimeSecs + 600,
                                              ifelse(dataset$qtr == 5 &
                                                       (dataset$Season < 2017 |
                                                          (dataset$Season == 2017 &
                                                             dataset$Month <= 4)),
                                                     dataset$TimeSecs + 900,
                                                     dataset$TimeSecs)))
  
  # Expected Score Differential
  dataset <- dplyr::mutate(dataset, ExpScoreDiff = ExpPts + ScoreDiff)
  
  
  # Ratio of time to yard line
  dataset <- dplyr::mutate(dataset,
                           Time_Yard_Ratio = (1+TimeSecs_Remaining)/(1+yrdline100))
  
  # Opponents timeouts remaining:
  dataset$oppteam_timeouts_pre <- ifelse(dataset$posteam == dataset$HomeTeam,
                                         dataset$AwayTimeouts_Remaining_Pre,
                                         dataset$HomeTimeouts_Remaining_Pre)
  
  # Under Two Minute Warning Flag
  dataset$Under_TwoMinute_Warning <- ifelse(dataset$TimeSecs_Remaining < 120,1,0)
  
  
  # Due to NFL errors make a floor at 0 for the timeouts:
  dataset$posteam_timeouts_pre <- ifelse(dataset$posteam_timeouts_pre < 0,
                                         0,dataset$posteam_timeouts_pre)
  dataset$oppteam_timeouts_pre <- ifelse(dataset$oppteam_timeouts_pre < 0,
                                         0,dataset$oppteam_timeouts_pre)
  
  # Define a form of the TimeSecs_Adj that just takes the original TimeSecs but
  # resets the overtime back to 900 or 600 depending on year:
  
  dataset$TimeSecs_Adj <- ifelse(dataset$qtr == 5 & 
                                   (dataset$Season == 2017 & 
                                      dataset$Month > 4),
                                 dataset$TimeSecs + 600,
                                 ifelse(dataset$qtr == 5 &
                                          (dataset$Season < 2017 |
                                             (dataset$Season == 2017 &
                                                dataset$Month <= 4)),
                                        dataset$TimeSecs + 900,
                                        dataset$TimeSecs))
  
  # Define a new variable, ratio of Expected Score Differential to TimeSecs_Adj:
  
  dataset <- dplyr::mutate(dataset,
                           ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecs_Adj + 1))
  
  
  # First check if there's any overtime plays:
  if (any(dataset$qtr == 5)){
    # Find the rows that are overtime:
    overtime_i <- which(dataset$qtr == 5)
    
    # Separate the dataset into regular_df and overtime_df:
    regular_df <- dataset[-overtime_i,]
    overtime_df <- dataset[overtime_i,]
    
    # Use the win prob model to predict the win probability for 
    # regulation time plays:
    regular_df$Half_Ind <- with(regular_df,
                                ifelse(qtr %in% c(1,2),"Half1","Half2"))
    regular_df$Half_Ind <- as.factor(regular_df$Half_Ind)
    
    OffWinProb[-overtime_i] <- as.numeric(mgcv::predict.bam(wp_model,newdata=regular_df,
                                                            type = "response"))
    
    # Separate routine for overtime:
    
    # Create a column that is just the first drive of overtime repeated:
    overtime_df$First_Drive <- rep(min(overtime_df$Drive,na.rm=TRUE),nrow(overtime_df))
    
    # Calculate the difference in drive number
    overtime_df <- dplyr::mutate(overtime_df, Drive_Diff = Drive - First_Drive)
    
    # Create an indicator column that means the posteam is losing by 3 and
    # its the second drive of overtime:
    
    overtime_df$One_FG_Game <- ifelse(overtime_df$ScoreDiff == -3 & overtime_df$Drive_Diff == 1,1,0)
    
    # Now create a copy of the dataset to then make the EP predictions for when
    # a field goal is scored and its not sudden death:
    overtime_df_ko <- overtime_df
    
    # Change the yard line to be 80 for 2009-2015 and 75 otherwise
    # (accounting for the fact that Jan 2016 is in the 2015 season:
    overtime_df_ko$yrdline100 <- with(overtime_df_ko,
                                        ifelse(Season < 2016 | (Season == 2016 & Month < 4), 80, 75))
    # Not GoalToGo:
    overtime_df_ko$GoalToGo <- rep(0,nrow(overtime_df_ko))
    # Now first down:
    overtime_df_ko$down <- rep("1",nrow(overtime_df_ko))
    # 10 ydstogo:
    overtime_df_ko$ydstogo <- rep(10,nrow(overtime_df_ko))
    # Create log_ydstogo:
    overtime_df_ko <- dplyr::mutate(overtime_df_ko, log_ydstogo = log(ydstogo))
    
    # Create Under_TwoMinute_Warning indicator
    overtime_df_ko$Under_TwoMinute_Warning <- ifelse(overtime_df_ko$TimeSecs_Remaining < 120,1,0)
    
    # Get the predictions from the EP model and calculate the necessary probability:
    if (nrow(overtime_df_ko) > 1) {
      overtime_df_ko_preds <- as.data.frame(predict(ep_model, newdata = overtime_df_ko, type = "probs"))
    } else{
      overtime_df_ko_preds <- as.data.frame(matrix(predict(ep_model, newdata = overtime_df_ko, type = "probs"),
                                                  ncol = 7))
    }
    colnames(overtime_df_ko_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                          "Field_Goal","Safety","Touchdown")
    overtime_df_ko_preds <- dplyr::mutate(overtime_df_ko_preds,
                                            Win_Back = No_Score + Opp_Field_Goal + Opp_Safety + Opp_Touchdown)
    
    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    overtime_df$Sudden_Death_WP <- overtime_df$Field_Goal_Prob + overtime_df$Touchdown_Prob + overtime_df$Safety_Prob
    overtime_df$One_FG_WP <- overtime_df$Touchdown_Prob + (overtime_df$Field_Goal_Prob*overtime_df_ko_preds$Win_Back)
    
    
    # Decide which win probability to use:
    OffWinProb[overtime_i] <- ifelse(overtime_df$Season >= 2012  & (overtime_df$Drive_Diff == 0 | (overtime_df$Drive_Diff == 1 & overtime_df$One_FG_Game == 1)),
                                     overtime_df$One_FG_WP, overtime_df$Sudden_Death_WP)
    
    
  } else {
    
    dataset$Half_Ind <- with(dataset,
                             ifelse(qtr %in% c(1,2),"Half1","Half2"))
    dataset$Half_Ind <- as.factor(dataset$Half_Ind)
    OffWinProb <- as.numeric(mgcv::predict.bam(wp_model,newdata=dataset,
                                                            type = "response"))
  }
  
  
  # Defensive win probability:
  DefWinProb <- 1 - OffWinProb

  ####### Creating Home team and Away team Win probability Pre-play,     #######
  ####### Post-play, and win prob added                                  ####### 
  
  # Possession team Win_Prob
  
  dataset$Win_Prob <- OffWinProb
  
  # Home: Pre-play
  
  dataset$Home_WP_pre <- ifelse(dataset$posteam == dataset$HomeTeam, OffWinProb, DefWinProb)
  
  # Away: Pre-play
  
  dataset$Away_WP_pre <- ifelse(dataset$posteam == dataset$AwayTeam, OffWinProb, DefWinProb)
  
  # Create the possible WPA values
  dataset <- dplyr::mutate(dataset,
                           # Team keeps possession (most general case):
                           WPA_base = dplyr::lead(Win_Prob) - Win_Prob,
                           # Team keeps possession but either Timeout, Two Minute Warning,
                           # Quarter End is the following row:
                           WPA_base_nxt = dplyr::lead(Win_Prob,2) - Win_Prob,
                           # Change of possession and no timeout, 
                           # two minute warning, or quarter end follows:
                           WPA_change = (1 - dplyr::lead(Win_Prob)) - Win_Prob,
                           # Change of possession but either Timeout,
                           # Two Minute Warning, or
                           # Quarter End is the following row:
                           WPA_change_nxt = (1 - dplyr::lead(Win_Prob,2)) - Win_Prob,
                           # End of quarter, half or end rows:
                           WPA_halfend_to = 0)
  # Create a WPA column for the last play of the game:
  dataset$WPA_final <- ifelse(dplyr::lead(dataset$ScoreDiff) > 0 & dplyr::lead(dataset$posteam) == dataset$HomeTeam,
                              1 - dataset$Home_WP_pre,
                              ifelse(dplyr::lead(dataset$ScoreDiff) > 0 & dplyr::lead(dataset$posteam) == dataset$AwayTeam,
                                     1 - dataset$Away_WP_pre,
                                     ifelse(dplyr::lead(dataset$ScoreDiff) < 0 & dplyr::lead(dataset$posteam) == dataset$HomeTeam,
                                            0 - dataset$Home_WP_pre,
                                            ifelse(dplyr::lead(dataset$ScoreDiff) < 0 & dplyr::lead(dataset$posteam) == dataset$AwayTeam,
                                                   0 - dataset$Away_WP_pre, 0))))


  dataset$WPA_base_nxt_ind <- with(dataset, 
                                   ifelse(GameID == dplyr::lead(GameID) & 
                                            posteam == dplyr::lead(posteam,2) &
                                            Drive == dplyr::lead(Drive,2) & 
                                            dplyr::lead(PlayType) %in% 
                                            c("Quarter End","Two Minute Warning","Timeout"),1,0))
  dataset$WPA_change_nxt_ind <- with(dataset, 
                                     ifelse(GameID == dplyr::lead(GameID) & 
                                              Drive != dplyr::lead(Drive,2) & 
                                              posteam != dplyr::lead(posteam,2) &
                                              dplyr::lead(PlayType) %in% 
                                              c("Quarter End","Two Minute Warning","Timeout"),1,0))
  dataset$WPA_change_ind <- with(dataset,
                                 ifelse(GameID == dplyr::lead(GameID) & 
                                          Drive != dplyr::lead(Drive) & 
                                          posteam != dplyr::lead(posteam) &
                                          dplyr::lead(PlayType) %in% 
                                          c("Pass","Run","Punt","Sack",
                                            "Field Goal","No Play","QB Kneel",
                                            "Spike","Kickoff"),1,0))
  dataset$WPA_halfend_to_ind <- with(dataset, ifelse(PlayType %in% c("Half End","Quarter End",
                                                                     "End of Game","Timeout") | 
                                                       (GameID == dplyr::lead(GameID) & sp != 1 &
                                                          Touchdown != 1 & 
                                                          is.na(FieldGoalResult) & 
                                                          is.na(ExPointResult) & 
                                                          is.na(TwoPointConv) & 
                                                          Safety != 1 & 
                                                          ((dplyr::lead(PlayType) %in% 
                                                              c("Half End")) | 
                                                             (qtr == 2 & dplyr::lead(qtr)==3) | 
                                                             (qtr == 4 & dplyr::lead(qtr)==5))),1,0))
  dataset$WPA_final_ind <- with(dataset, ifelse(dplyr::lead(PlayType) %in% "End of Game", 1, 0))
  
  # Replace the missings with 0 due to how ifelse treats missings
  dataset$WPA_base_nxt_ind[is.na(dataset$WPA_base_nxt_ind)] <- 0
  dataset$WPA_change_nxt_ind[is.na(dataset$WPA_change_nxt_ind)] <- 0
  dataset$WPA_change_ind[is.na(dataset$WPA_change_ind)] <- 0 
  dataset$WPA_halfend_to_ind[is.na(dataset$WPA_halfend_to_ind)] <- 0
  dataset$WPA_final_ind[is.na(dataset$WPA_final_ind)] <- 0

  
  # Assign WPA using these indicator columns: 
  dataset$WPA <- with(dataset, 
                      ifelse(WPA_final_ind == 1,WPA_final,
                             ifelse(WPA_halfend_to_ind == 1, WPA_halfend_to,
                                    ifelse(WPA_change_nxt_ind == 1, WPA_change_nxt,
                                           ifelse(WPA_base_nxt_ind == 1, WPA_base_nxt,
                                                  ifelse(WPA_change_ind == 1, WPA_change,
                                                  WPA_base))))))
    # Home and Away post:
  
  dataset$Home_WP_post <- ifelse(dataset$posteam == dataset$HomeTeam,
                                 dataset$Home_WP_pre + dataset$WPA,
                                 dataset$Home_WP_pre - dataset$WPA)
  dataset$Away_WP_post <- ifelse(dataset$posteam == dataset$AwayTeam,
                                 dataset$Away_WP_pre + dataset$WPA,
                                 dataset$Away_WP_pre - dataset$WPA)
  
  # For plays with playtype of End of Game, use the previous play's WP_post columns
  # as the pre and post, since those are already set to be 1 and 0:
  dataset$Home_WP_pre <- with(dataset,
                              ifelse(PlayType == "End of Game", dplyr::lag(Home_WP_post),
                                     Home_WP_pre))
  dataset$Home_WP_post <- with(dataset,
                              ifelse(PlayType == "End of Game", dplyr::lag(Home_WP_post),
                                     Home_WP_post))
  dataset$Away_WP_pre <- with(dataset,
                              ifelse(PlayType == "End of Game", dplyr::lag(Away_WP_post),
                                     Away_WP_pre))
  dataset$Away_WP_post <- with(dataset,
                               ifelse(PlayType == "End of Game", dplyr::lag(Away_WP_post),
                                      Away_WP_post))
  
  
  # Now drop the unnecessary columns
  dataset <- dplyr::select(dataset, -c(WPA_base,WPA_base_nxt,WPA_change_nxt,WPA_change,
                                       WPA_halfend_to, WPA_final,
                                       WPA_base_nxt_ind, WPA_change_nxt_ind,
                                       WPA_change_ind, WPA_halfend_to_ind, WPA_final_ind,
                                       Half_Ind))
  
  
  # Calculate the airWPA and yacWPA:
  
  # Find all Pass Attempts that are also actual plays,
  pass_plays_i <- which(dataset$PassAttempt == 1 & dataset$PlayType != "No Play")
  
  # Create a dataframe of pass_plays:
  
  pass_pbp_data <- dataset[pass_plays_i,]
  
  # Since airEPA has already been calculated, only need to update
  # the columns related to time as well as calculate a new 
  # ExpScoreDiff using the airEPA. Additionally if the air yards
  # result in a turnover then need to flip variables accordingly.
  
  # First calculate the new ExpScoreDiff using airEPA:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data, ExpScoreDiff = ExpPts + airEPA + ScoreDiff)
  
  # Adjust the time with the average incomplete pass time:
  pass_pbp_data$TimeSecs_Remaining <- pass_pbp_data$TimeSecs_Remaining - 5.704673
  
  # Adjust Under_TwoMinute_Warning indicator:
  pass_pbp_data$Under_TwoMinute_Warning <- ifelse(pass_pbp_data$TimeSecs_Remaining < 120,1,0)
  
  # Adjust TimeSecs_Adj:
  pass_pbp_data$TimeSecs_Adj <- pass_pbp_data$TimeSecs_Adj - 5.704673
  
  # Adjust the ExpScoreDiff_Time_Ratio:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data,
                           ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecs_Adj + 1))
  
  # For now treat as no overtime - then will adjust accordingly afterwards if there
  # are any overtime plays:
  pass_pbp_data$Half_Ind <- with(pass_pbp_data,
                           ifelse(qtr %in% c(1,2),"Half1","Half2"))
  pass_pbp_data$Half_Ind <- as.factor(pass_pbp_data$Half_Ind)
  
  # Create an indicator column for the air yards failing to convert the first down:
  pass_pbp_data$Turnover_Ind <- ifelse(pass_pbp_data$down == 4 & pass_pbp_data$AirYards < pass_pbp_data$ydstogo,
                                       1,0)
  
  # If the Turnover_Ind is 1 then flip the variables with the score and timeouts,
  # by first storing the old timeout columns:
  pass_pbp_data$old_posteam_timeouts_pre <- pass_pbp_data$posteam_timeouts_pre
  pass_pbp_data$old_oppteam_timeouts_pre <- pass_pbp_data$oppteam_timeouts_pre
  
  # Then using ifelse to adjust each:
  pass_pbp_data$ExpScoreDiff <- ifelse(pass_pbp_data$Turnover_Ind == 1,
                                       -1 * pass_pbp_data$ExpScoreDiff, pass_pbp_data$ExpScoreDiff)
  pass_pbp_data$ExpScoreDiff_Time_Ratio <- ifelse(pass_pbp_data$Turnover_Ind == 1,
                                                  -1 * pass_pbp_data$ExpScoreDiff_Time_Ratio, pass_pbp_data$ExpScoreDiff_Time_Ratio)
  pass_pbp_data$posteam_timeouts_pre <- ifelse(pass_pbp_data$Turnover_Ind == 1,
                                               pass_pbp_data$old_oppteam_timeouts_pre,
                                               pass_pbp_data$old_posteam_timeouts_pre)
  pass_pbp_data$oppteam_timeouts_pre <- ifelse(pass_pbp_data$Turnover_Ind == 1,
                                               pass_pbp_data$old_posteam_timeouts_pre,
                                               pass_pbp_data$old_oppteam_timeouts_pre)
  
  # Calculate the airWP:
  pass_pbp_data$airWP <- as.numeric(mgcv::predict.bam(wp_model,newdata=pass_pbp_data,
                                             type = "response"))
  
  # Now for plays marked with Turnover_Ind, use 1 - airWP to flip back to the original
  # team with possession:
  pass_pbp_data$airWP <- ifelse(pass_pbp_data$Turnover_Ind == 1,
                                1 - pass_pbp_data$airWP, pass_pbp_data$airWP)
  
  # For the plays that have TimeSecs_Remaining 0 or less, set airWP to 0:
  pass_pbp_data$airWP[which(pass_pbp_data$TimeSecs_Remaining<=0)] <- 0
  
  # Calculate the airWPA and yacWPA:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data, airWPA = airWP - Win_Prob,
                                 yacWPA = WPA - airWPA)
  
  
  # If the play is a two-point conversion then change the airWPA to NA since
  # no air yards are provided:
  pass_pbp_data$airWPA <- with(pass_pbp_data, ifelse(!is.na(TwoPointConv),
                                                     NA,airWPA))
  pass_pbp_data$yacWPA <- with(pass_pbp_data, ifelse(!is.na(TwoPointConv),
                                                     NA,yacWPA))
  
  # Check to see if there is any overtime plays, if so then need to calculate
  # by essentially taking the same process as the airEP calculation and using
  # the resulting probabilities for overtime:
  
  # First check if there's any overtime plays:
  if (any(pass_pbp_data$qtr == 5)){
    # Find the rows that are overtime:
    pass_overtime_i <- which(pass_pbp_data$qtr == 5)
    pass_overtime_df <- pass_pbp_data[pass_overtime_i,]
    
    # Use the already created overtime_df and overtime_df_ko_preds, and make
    # the same adjustments as the airEPA and yacEPA calculations:
    
    # Find all Pass Attempts that are also actual plays in overtime:
    overtime_pass_plays_i <- which(overtime_df$PassAttempt == 1 & overtime_df$PlayType != "No Play")
    overtime_pass_df <- overtime_df[overtime_pass_plays_i,]
    overtime_df_ko_preds_pass <- overtime_df_ko_preds[overtime_pass_plays_i,]
    
    # Using the AirYards need to update the following:
    # - yrdline100
    # - TimeSecs_Remaining
    # - GoalToGo
    # - ydstogo
    # - log_ydstogo
    # - Under_TwoMinute_Warning
    # - down
    
    # First rename the old columns to update for calculating the EP from the air:
    overtime_pass_df <- dplyr::rename(overtime_pass_df,old_yrdline100=yrdline100,
                                   old_ydstogo=ydstogo, old_TimeSecs_Remaining=TimeSecs_Remaining,
                                   old_GoalToGo=GoalToGo,old_down=down)
    
    # Create an indicator column for the air yards failing to convert the first down:
    overtime_pass_df$Turnover_Ind <- ifelse(overtime_pass_df$old_down == 4 & overtime_pass_df$AirYards < overtime_pass_df$old_ydstogo,
                                         1,0)
    # Adjust the field position variables:
    overtime_pass_df$yrdline100 <- ifelse(overtime_pass_df$Turnover_Ind == 0,
                                          overtime_pass_df$old_yrdline100 - overtime_pass_df$AirYards,
                                       100 - (overtime_pass_df$old_yrdline100 - overtime_pass_df$AirYards))
    
    overtime_pass_df$ydstogo <- ifelse(overtime_pass_df$AirYards >= overtime_pass_df$old_ydstogo | 
                                         overtime_pass_df$Turnover_Ind == 1,
                                    10, overtime_pass_df$old_ydstogo - overtime_pass_df$AirYards)
    # Create log_ydstogo:
    overtime_pass_df <- dplyr::mutate(overtime_pass_df, log_ydstogo = log(ydstogo))
    
    overtime_pass_df$down <- ifelse(overtime_pass_df$AirYards >= overtime_pass_df$old_ydstogo | 
                                      overtime_pass_df$Turnover_Ind == 1,
                                 1, as.numeric(overtime_pass_df$old_down) + 1)
    
    overtime_pass_df$GoalToGo <- ifelse((overtime_pass_df$old_GoalToGo == 1 & overtime_pass_df$Turnover_Ind==0) |
                                       (overtime_pass_df$Turnover_Ind == 0 & overtime_pass_df$old_GoalToGo == 0 & overtime_pass_df$yrdline100 <= 10) |
                                       (overtime_pass_df$Turnover_Ind == 1 & overtime_pass_df$yrdline100 <= 10),1,0)
    
    # Adjust the time with the average incomplete pass time:
    overtime_pass_df$TimeSecs_Remaining <- overtime_pass_df$old_TimeSecs_Remaining - 5.704673
    
    # Create Under_TwoMinute_Warning indicator
    overtime_pass_df$Under_TwoMinute_Warning <- ifelse(overtime_pass_df$TimeSecs_Remaining < 120,1,0)
    
    # Make the new down a factor:
    overtime_pass_df$down <- as.factor(overtime_pass_df$down)

    # Get the new predicted probabilites:
    if (nrow(overtime_pass_df) > 1) {
      overtime_pass_data_preds <- as.data.frame(predict(ep_model, newdata = overtime_pass_df, type = "probs"))
    } else{
      overtime_pass_data_preds <- as.data.frame(matrix(predict(ep_model, newdata = overtime_pass_df, type = "probs"),
                                                       ncol = 7))
    }
    colnames(overtime_pass_data_preds) <- c("No_Score","Opp_Field_Goal","Opp_Safety","Opp_Touchdown",
                                            "Field_Goal","Safety","Touchdown")
    # For the turnover plays flip the scoring probabilities:
    overtime_pass_data_preds <- dplyr::mutate(overtime_pass_data_preds,
                                              old_Opp_Field_Goal = Opp_Field_Goal,
                                              old_Opp_Safety = Opp_Safety,
                                              old_Opp_Touchdown = Opp_Touchdown,
                                              old_Field_Goal = Field_Goal,
                                              old_Safety = Safety,
                                              old_Touchdown = Touchdown)
    overtime_pass_data_preds$Opp_Field_Goal <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                      overtime_pass_data_preds$old_Field_Goal,
                                                      overtime_pass_data_preds$Opp_Field_Goal)
    overtime_pass_data_preds$Opp_Safety <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                  overtime_pass_data_preds$old_Safety,
                                                  overtime_pass_data_preds$Opp_Safety)
    overtime_pass_data_preds$Opp_Touchdown <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                     overtime_pass_data_preds$old_Touchdown,
                                                     overtime_pass_data_preds$Opp_Touchdown)
    overtime_pass_data_preds$Field_Goal <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                  overtime_pass_data_preds$old_Opp_Field_Goal,
                                                  overtime_pass_data_preds$Field_Goal)
    overtime_pass_data_preds$Safety <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                              overtime_pass_data_preds$old_Opp_Safety,
                                              overtime_pass_data_preds$Safety)
    overtime_pass_data_preds$Touchdown <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                 overtime_pass_data_preds$old_Opp_Touchdown,
                                                 overtime_pass_data_preds$Touchdown)
    
    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    pass_overtime_df$Sudden_Death_airWP <- with(overtime_pass_data_preds, Field_Goal + Touchdown + Safety)
    pass_overtime_df$One_FG_airWP <- overtime_pass_data_preds$Touchdown + (overtime_pass_data_preds$Field_Goal*overtime_df_ko_preds_pass$Win_Back)
    
    # Decide which win probability to use:
    pass_overtime_df$airWP <- ifelse(overtime_pass_df$Season >= 2012  & (overtime_pass_df$Drive_Diff == 0 | (overtime_pass_df$Drive_Diff == 1 & overtime_pass_df$One_FG_Game == 1)),
                                     pass_overtime_df$One_FG_airWP, pass_overtime_df$Sudden_Death_airWP)
    
    # For the plays that have TimeSecs_Remaining 0 or less, set airWP to 0:
    pass_overtime_df$airWP[which(overtime_pass_df$TimeSecs_Remaining<=0)] <- 0
    
    # Calculate the airWPA and yacWPA:
    pass_overtime_df <- dplyr::mutate(pass_overtime_df, airWPA = airWP - Win_Prob,
                                   yacWPA = WPA - airWPA)
    
    # If the play is a two-point conversion then change the airWPA to NA since
    # no air yards are provided:
    pass_overtime_df$airWPA <- with(pass_overtime_df, ifelse(!is.na(TwoPointConv),
                                                       NA,airWPA))
    pass_overtime_df$yacWPA <- with(pass_overtime_df, ifelse(!is.na(TwoPointConv),
                                                       NA,yacWPA))
    
    
    pass_overtime_df <- pass_pbp_data[pass_overtime_i,]
    
    # Now update the overtime rows in the original pass_pbp_data for airWPA and yacWPA:
    pass_pbp_data$airWPA[pass_overtime_i] <- pass_overtime_df$airWPA
    pass_pbp_data$yacWPA[pass_overtime_i] <- pass_overtime_df$yacWPA
  }
  
  # if Yards after catch is 0 make yacWPA set to 0:
  pass_pbp_data$yacWPA <- ifelse(pass_pbp_data$YardsAfterCatch == 0 & pass_pbp_data$Reception==1,
                                 0, pass_pbp_data$yacWPA)
  # if Yards after catch is 0 make airWPA set to WPA:
  pass_pbp_data$airWPA <- ifelse(pass_pbp_data$YardsAfterCatch == 0 & pass_pbp_data$Reception==1,
                                 pass_pbp_data$WPA, pass_pbp_data$airWPA)
  
  # Now add airWPA and yacWPA to the original dataset:
  dataset$airWPA <- NA
  dataset$yacWPA <- NA
  dataset$airWPA[pass_plays_i] <- pass_pbp_data$airWPA
  dataset$yacWPA[pass_plays_i] <- pass_pbp_data$yacWPA
  
  
  return(dataset)
  
}

