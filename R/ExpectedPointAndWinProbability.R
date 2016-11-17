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
  
  dataset$expectedpoints <- round(nnet::predict.nnet(multi.w.time.int2, 
                                          newdata = dataset, 
         type = "prob") %*% scoring.weights, 2)
  
  remove.cols <- which(colnames(dataset) %in% c("begQeffect", "endQeffect"))
  
  dataset$down <- droplevels(dataset$down, levels = "NA")
  
  dataset[,-remove.cols]
}

################# Win Probability Fucntion #####################

#' Win probability function to add win probability columns for the offense and 
#' defense for each play in the game
#' @description This function takes in the output of the game level play by play
#' function and returns the same dataframe with two new columns representing the
#' win probability for the offensive and defensive teams.  The model used to 
#' calculate win probability can be further examined by loading it into your
#' workspace from this package.
#' @param dataset (data.frame object) A data.frame as exported from the 
#' game_play_by_play function
#' @return The input dataframe with the addition of win probability columns for 
#' both the offense and defense
#' @export
win_probability <- function(dataset) {
  
  # Categorize special teams plays into one play type
  dataset$down[which(dataset$PlayType == "Kickoff")] <- "SpecialTeams"
  dataset$down[which(dataset$PlayType == "Extra Point")] <- "SpecialTeams"
  dataset$down[which(dataset$PlayType == "Onside Kick")] <- "SpecialTeams"
  dataset$down[which(!is.na(dataset$TwoPointConv))] <- "SpecialTeams"
  
  dataset$down <- as.factor(dataset$down)
  ## Add inverse time variable ##
  dataset$invtime <- 1/(dataset$TimeSecs + .0000000001)
  
  dataset$OffWinProb <- round(mgcv::predict.gam(object = win.prob.model.gam2, 
                                      newdata = dataset, type = "response"), 3)
  
  dataset$DefWinProb <- 1 - dataset$OffWinProb
  
  # Change down back to numeric
  
  dataset$down[which(dataset$down == "SpecialTeams")] <- NA
  
  dataset$down <- as.numeric(dataset$down)
  
  invtime.index <- which(colnames(dataset) == "invtime")
  dataset[,-invtime.index]
}

