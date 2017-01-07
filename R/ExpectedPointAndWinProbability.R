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
  
  
  
  remove.cols <- which(colnames(dataset) %in% c("begQeffect", "endQeffect"))
  
  dataset$down <- droplevels(dataset$down, levels = "NA")
  
  dataset[,-remove.cols]
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
  dataset$Home.WPA <- dataset$Home.WP.post - dataset$Home.WP.pre
  
  # Away
  dataset$Away.WPA <- dataset$Away.WP.post - dataset$Away.WP.pre
  
  ### Formatting ###
  ## Change down back to numeric ##
  
  dataset$down[which(dataset$down == "SpecialTeams")] <- NA
  
  dataset$down <- as.numeric(dataset$down)
  
  invtime.index <- which(colnames(dataset) == "invtime")
  
  ## Final output dataframe ##
  dataset[,-invtime.index]
}

