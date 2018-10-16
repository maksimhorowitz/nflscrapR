################################################################## 
### Play-by-play, Drive Summary, and Simple Box Score Function ###
# Authors: Maksim Horowitz, Ron Yurko                            #
# Code Style Guide: Google R Format                              #
################################################################## 

# Play-by Play Function
#' Parsed Descriptive Play-by-Play Dataset for a Single Game
#' @description This function intakes the JSON play-by-play data of a single
#'  game and parses the play description column into individual variables 
#'  allowing the user to segment the game in a variety of different ways for 
#'  model building and analysis. WARNING: This function is deprecated and will be removed from the package
#'  after the 2018-19 season.
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @details Through list manipulation using the do.call and rbind functions
#'  a 13 column dataframe with basic information populates directly from the NFL 
#'  JSON API.  These columns include the following:
#' \itemize{
#'  \item{"Drive"} - Drive number
#'  \item{"sp"} - Whether the play resulted in a score (any kind of score)
#'  \item{"qtr"} - Quarter of Game
#'  \item{"down"} - Down of the given play
#'  \item{"time"} - Time at start of play
#'  \item{"yrdln"} - Between 0 and 50
#'  \item{"ydstogo"} - Yards to go for a first down
#'  \item{"ydsnet"} - Total yards gained on a given drive
#'  \item{"posteam"} - The team on offense
#'  \item{"AirYards"} - Number of yards the ball was thrown in the air for both
#'  complete and incomplete pass attempts (negative means behind line of scrimmage)
#'  \item{"YardsAfterCatch"} - Number of yards receiver gained after catch
#'  \item{"QBHit"} -  Binary: 1 if the QB was knocked to the ground else 0
#'  \item{"desc"} - A detailed description of what occured during the play
#' }
#' 
#' Through string manipulation and parsing of the description column using  
#' base R and stringR, columns were added to the original dataframe allowing 
#' the user to have a detailed breakdown of the events of each play. Also
#' provided are calculations for the expected points and win probability
#' for each play using models built entirely on nflscrapR data. 
#' The added variables are specified below:
#' \itemize{
#'  \item{"Date"} - Date of game
#'  \item{"GameID"} - The ID of the specified game
#'  \item{"play_id"} - Play id within a game
#'  \item{"TimeSecs"} - Time remaining in game in seconds
#'  \item{"PlayTimeDiff"} - The time difference between plays in seconds
#'  \item{"DefensiveTeam"} - The defensive team on the play (for punts the 
#'  receiving team is on defense, for kickoffs the receiving team is on offense)
#'  \item{"TimeUnder"} - Minutes remaining in half
#'  \item{"SideofField"} - The side of the field that the line of scrimmage 
#'  is on
#'  \item{yrdline100} - Distance to opponents endzone, ranges from 1-99
#'  situation
#'  \item{GoalToGo} - Binary: 1 if the play is in a goal down situation else 0
#'  \item{"FirstDown"} - Binary: 1if the play resulted in a first down conversion
#'  else 0
#'  \item{"PlayAttempted"} - A variable used to count the number of plays in a 
#'  game (should always be equal to 1)
#'  \item{"Yards.Gained"} - Amount of yards gained on the play
#'  \item{"Touchdown"} - Binary: 1 if the play resulted in a TD else 0
#'  \item{"ExPointResult"} - Result of the extra-point: Made, Missed, Blocked, 
#'  Aborted
#'  \item{"TwoPointConv"} - Result of two-point conversion: Success or Failure
#'  \item{"DefTwoPoint"} - Result of defensive two-point conversion: Success or Failure
#'  \item{"Safety"} - Binary: 1 if safety was recorded else 0
#'  \item{"Onsidekick"} - Binary: 1 if the kickoff was an onside kick
#'  \item{"PuntResult} - Result of punt: Clean or Blocked
#'  \item{"PlayType"} - The type of play that occured, potential values are:
#'        \itemize{
#'                  \item{Kickoff, Punt}
#'                  \item{Pass, Sack, Run}
#'                  \item{Field Goal, Extra Point}
#'                  \item{Quarter End, Two Minute Warning, Half End, End of Game}
#'                  \item{No Play, QB Kneel, Spike, Timeout}
#'          }  
#'  \item{"Passer"} - The passer on the play if it was a pass play
#'  \item{"Passer_ID"} - NFL GSIS player ID for the passer
#'  \item{"PassAttempt"} - Binary: 1 if a pass was attempted else 0
#'  \item{"PassOutcome"} - Pass Result: Complete or Incomplete Pass  
#'  \item{"PassLength"} - Categorical variable indicating the length of the pass:
#'  Short or Deep
#'  \item{"PassLocation"} - Location of the pass: left, middle, right
#'  \item{"InterceptionThrown"} - Binary: 1 if an interception else 0
#'  \item{"Interceptor"} - The player who intercepted the ball
#'  \item{"Rusher"} - The runner on the play if it was a running play
#'  \item{"Rusher_ID"} - NFL GSIS player ID for the rusher
#'  \item{"RushAttempt"} - Binary: 1 if a run was attempted else 0
#'  \item{"RunLocation"} - Location of the run: left, middle, right
#'  \item{"RunGap"} - Gap of the run: guard, tackle, end 
#'  \item{"Receiver"} - The targeted receiver of a play
#'  \item{"Receiver_ID"} - NFL GSIS player ID for the receiver
#'  \item{"Reception"} - Binary: 1 if a reception was recorded else 0
#'  \item{"ReturnResult"} - Result of a punt, kickoff, interception, or 
#'  fumble return: Fair Catch, Touchback, Touchdown
#'  \item{"Returner"} - The punt or kickoff returner
#'  \item{"BlockingPlayer"} - The player who blocked the extra point, 
#'  field goal, or punt
#'  \item{"Tackler1"} - The primary tackler on the play
#'  \item{"Tackler2"} - The secondary tackler on the play
#'  \item{"FieldGoalResult"} - Outcome of a fieldgoal: No Good, Good, Blocked
#'  \item{"FieldGoalDistance"} - Field goal length in yards
#'  \item{"Fumble"} - Binary: 1 if a fumble occured else no 
#'  \item{"RecFumbTeam"} - Team that recovered the fumble
#'  \item{"RecFumbPlayer"} - Player that recovered the fumble
#'  \item{"Sack"} - Binary: 1 if a sack was recorded else 0
#'  \item{"Challenge.Replay"} - Binary: 1 if play was reviewed by the replay official
#'  else 0
#'  \item{"ChalReplayResult"} - Result of the replay review: Upheld or Reversed
#'  \item{"Accepted.Penalty"} - Binary: 1 if penalty was accepted else 0
#'  \item{"PenalizedTeam"} - The penalized team on the play
#'  \item{"PenaltyType"} - Type of penalty on the play, alues include:
#'        \itemize{    
#'                  \item{Unnecessary Roughness, Roughing the Passer}
#'                  \item{Illegal Formation, Defensive Offside}
#'                  \item{Delay of Game, False Start, Illegal Shift}
#'                  \item{Illegal Block Above the Waist, Personal Foul}
#'                  \item{Unnecessary Roughness, Illegal Blindside Block}
#'                  \item{Defensive Pass Interference, Offensive Pass Interference}
#'                  \item{Fair Catch Interference, Unsportsmanlike Conduct}
#'                  \item{Running Into the Kicker, Illegal Kick}
#'                  \item{Illegal Contact, Defensive Holding}
#'                  \item{Illegal Motion, Low Block}
#'                  \item{Illegal Substitution, Neutral Zone Infraction}
#'                  \item{Ineligible Downfield Pass, Roughing the Passer}
#'                  \item{Illegal Use of Hands, Defensive Delay of Game}
#'                  \item{Defensive 12 On-field, Offensive Offside}
#'                  \item{Tripping, Taunting, Chop Block}
#'                  \item{Interference with Opportunity to Catch, Illegal Touch Pass}
#'                  \item{Illegal Touch Kick, Offside on Free Kick}
#'                  \item{Intentional Grounding, Horse Collar}
#'                  \item{Illegal Forward Pass, Player Out of Bounds on Punt}
#'                  \item{Clipping, Roughing the Kicker, Ineligible Downfield Kick}
#'                  \item{Offensive 12 On-field, Disqualification}
#'       } 
#'  \item{"PenalizedPlayer"} - The penalized player
#'  \item{"Penalty.Yards"} - The number of yards that the penalty resulted in
#'  \item{"PosTeamScore"} - The score of the possession team (offensive team)
#'  \item{"DefTeamScore"} - The score of the defensive team
#'  \item{"ScoreDiff"} - The difference in score between the offensive and 
#'  defensive teams (offensive.score - def.score)
#'  \item{"AbsScoreDiff"} - Absolute value of the score differential
#'  \item{"HomeTeam"} - The home team
#'  \item{"AwayTeam"} - The away team
#'  \item{"Timeout_Indicator"} - Binary: 1 if a timeout was charge else 0
#'  \item{"Timeout_Team"} - Team charged with penalty (None if no timeout)
#'  \item{"posteam_timeouts_pre"} - Number of timeouts remaining for possession
#'  team at the start of the play
#'  \item{"HomeTimeouts_Remaining_Pre"} - Number of timeouts remaining for home
#'  team at the start of the play
#'  \item{"AwayTimeouts_Remaining_Pre"} - Number of timeouts remaining for away
#'  team at the start of the play
#'  \item{"HomeTimeouts_Remaining_Post"} - Number of timeouts remaining for home
#'  team at the end of the play (handles loss of timeout from lost challenge)
#'  \item{"AwayTimeouts_Remaining_Post"} - Number of timeouts remaining for away
#'  team at the end of the play (handles loss of timeout from lost challenge)
#'  \item{"No_Score_Prob"} - Probability of no score occurring within the half
#'  \item{"Opp_Field_Goal_Prob"} - Probability of the defensive team scoring a
#'  field goal next
#'  \item{"Opp_Safety_Prob"} - Probability of the defensive team scoring a 
#'  safety next
#'  \item{"Opp_Touchdown_Prob"} - Probability of the defensive team scoring a 
#'  touchdown next
#'  \item{"Field_Goal_Prob"} - Probability of the possession team scoring a 
#'  field goal next
#'  \item{"Safety_Prob"} - Probability of the possession team scoring a safety
#'  next
#'  \item{"Touchdown_Prob"} - Probability of the possession team scoring a 
#'  touchdown next
#'  \item{"ExPoint_Prob"} - Probability of the possession team making the PAT
#'  \item{"TwoPoint_Prob"} - Probability of the possession team converting 
#'  the two-point conversion
#'  \item{"ExpPts"} - The expected points for the possession team at the
#'  start of the play
#'  \item{"EPA"} - Expected points added with respect to the possession
#'  team considering the result of the play
#'  \item{"airEPA"} - Expected points added from air yards
#'  \item{"yacEPA"} - Expected points added from yards after catch
#'  \item{"Home_WP_pre"} - The win probability for the home team at the start
#'  of the play
#'  \item{"Away_WP_pre"} - The win probability for the away team at the start
#'  of the play
#'  \item{"Home_WP_post"} - The win probability for the home team at the
#'  end of the play
#'  \item{"Away_WP_post"} - The win probability for the away team at the
#'  end of the play
#'  \item{"Win_Prob"} - The win probability added for team with possession
#'  \item{"WPA"} - The win probability added with respect to the
#'  possession team
#'  \item{"airWPA"} - Win probability added from air yards
#'  \item{"yacWPA"} - Win probability added from yards after catch
#'  
#'  }
#'  
#' @return A dataframe with 99 columns specifying various statistics and 
#' outcomes associated with each play of the specified NFL game.
#' @examples
#' # Parsed play-by-play of the final game in the 2015 NFL season 
#' 
#' # Save the gameID into a variable 
#' nfl2015.finalregseasongame.gameID <- "2016010310"
#' 
#' # Input the variable into the function to output the desired dataframe
#' finalgame2015.pbp <- game_play_by_play(nfl2015.finalregseasongame.gameID) 
#' 
#' # Subset the dataframe based on passing plays
#' subset(finalgame2015.pbp, PlayType == "Pass")
#' @export
game_play_by_play <- function(GameID) {
  # Google R stlye format
  
  #########################
  #########################
  
  # Converting JSON data
  
  # Converting GameID into URL string
  urlstring <- proper_jsonurl_formatting(GameID)
  
  nfl.json <- tryCatch(RJSONIO::fromJSON(RCurl::getURL(urlstring)),
                       error=function(cond) {
                         message("Connection to API disrupted, please re-run code. If multiple failures, then there is no data available for this game yet.")
                         message(paste("Here is the url:", urlstring))
                         message("Here's the original error message:")
                         message(cond)
                         # Choose a return value in case of error
                         return(NA)
                       })
  
  number.drives <- length(nfl.json[[1]]$drives) - 1
  
  # For now hard coded a fix for the drive in the Patriots vs. TB game
  # that has an empty drive 3 list:
  
  if (GameID == 2013092206){
    PBP <- lapply(c(1, 2, 4:number.drives),
                  function(x) cbind("Drive"=x,data.frame(do.call(rbind,nfl.json[[1]]$drives[[x]]$plays))[,c(1:9)])) %>%
      dplyr::bind_rows()
  } else {
    PBP <- lapply(c(1:number.drives),
                  function(x) cbind("Drive"=x,data.frame(do.call(rbind,nfl.json[[1]]$drives[[x]]$plays))[,c(1:9)])) %>%
      dplyr::bind_rows()
  }
  
  # Search through the 'players' lists for each play (the 'plays' list within
  # each drive) to find the air yards, yards after catch, and if the QB was hit
  
  # Define the get_play_stats function which takes in a game's drive list and
  # returns a dataframe with a row for each play and columns for the AirYards,
  # YardsAfterCatch, and QBHit indicator column:
  get_play_stats <- function(drive_plays_list){
    result <- lapply(c(1:length(drive_plays_list$plays)),
                     function(x) suppressWarnings(find_extra_stats(drive_plays_list$plays[[x]]))) %>% 
      dplyr::bind_rows() %>%
      # Include a column for the play_id
      cbind(data.frame(play_id = names(drive_plays_list$plays)))
    return(result)
  }
  
  # Define a function that takes in the players part of a play
  # and returns a dataframe of the stats with IDs:
  
  return_play_stat_df <- function(players_list){
    # Check the number of players in the play,
    # if empty return "None":
    if (length(players_list$players) == 0){
      result <- as.data.frame(list("statId" = "None"))
    } else{
      # Return the dataframe
      result <- lapply(c(1:length(players_list$players)),
                       function(x) as.data.frame(do.call(rbind,players_list$players[[x]]))) %>% dplyr::bind_rows()
      # Get the Player IDs:
      player_ids <- names(players_list$players)
      # For each player, get the number of rows for them and repeat their ID that many times:
      player_ids_vector <- as.vector(unlist(sapply(c(1:length(player_ids)),
                                                   function(x) rep(player_ids[x],length(players_list$players[[x]])))))
      result$PlayerID <- player_ids_vector
    }
    return(result)
  }
  
  
  # Define a fuction that returns as a dataframe the AirYards, YardsAfterCatch, and QBHit:
  
  find_extra_stats <- function(play_list){
    play_stat_df <- return_play_stat_df(play_list)
    # If play_stat_df equals None, then return NA for each:
    if (play_stat_df$statId == "None"){
      result <- as.data.frame(list("AirYards" = 0,
                                   "YardsAfterCatch" = 0,
                                   "QBHit" = 0,
                                   "Timeout_Indicator" = 0,
                                   "Timeout_Team" = as.character("None"),
                                   "Passer_ID" = as.character("None"),
                                   "Rusher_ID" = as.character("None"),
                                   "Receiver_ID" = as.character("None")))
    } else{
      # Find AirYards:
      if (111 %in% play_stat_df$statId){
        airyards <- play_stat_df$yards[which(play_stat_df$statId == 111)]
        # Check to see if it's a list
        if (typeof(airyards) == "list"){
          airyards <- unlist(airyards)
        }
        # Only the first one
        airyards <- airyards[1]
        # If NULL then make it 0:
        airyards <- ifelse(is.null(airyards),0,airyards)
      } else if (112 %in% play_stat_df$statId){
        airyards <- play_stat_df$yards[which(play_stat_df$statId == 112)]
        # Check to see if it's a list
        if (typeof(airyards) == "list"){
          airyards <- unlist(airyards)
        }
        # Only the first one
        airyards <- airyards[1]
        # If NULL then make it 0:
        airyards <- ifelse(is.null(airyards), 0, airyards)
      } else {
        airyards <- 0
      }
      # YardsAfterCatch:
      if (113 %in% play_stat_df$statId){
        yac <- play_stat_df$yards[which(play_stat_df$statId == 113)]
        # Check to see if it's a list
        if (typeof(yac) == "list"){
          yac <- unlist(yac)
        }
        # Only the first one
        yac <- yac[1]
        # If NULL then make it 0:
        yac <- ifelse(is.null(yac), 0, yac)
      } else {
        yac <- 0
      }
      # QBHit:
      if (110 %in% play_stat_df$statId){
        qbhit <- 1
      } else {
        qbhit <- 0
      }
      # Timeout:
      if (68 %in% play_stat_df$statId){
        to <- 1
        to_team <- as.character(play_stat_df$clubcode[which(play_stat_df$statId == 68)])
        # Only the first one
        to_team <- to_team[1]
      } else {
        to <- 0
        to_team <- as.character("None")
      }
      # Passer ID:
      if (any(c(14, 15, 16, 111, 112, 20) %in% play_stat_df$statId)){
        passer_id <- as.character(play_stat_df$PlayerID[which(play_stat_df$statId %in% c(14, 15, 16, 111, 112, 20))])
        # Only the first one
        passer_id <- passer_id[1]
      } else {
        passer_id <- "None"
      }
      # Rusher ID:
      if (any(c(10, 11, 12, 13) %in% play_stat_df$statId)){
        rusher_id <- as.character(play_stat_df$PlayerID[which(play_stat_df$statId %in% c(10, 11, 12, 13))])
        # Only the first one
        rusher_id <- rusher_id[1]
      } else {
        rusher_id <- "None"
      }
      # Receiver ID:
      if (any(c(21, 22, 113, 115) %in% play_stat_df$statId)){
        receiver_id <- as.character(play_stat_df$PlayerID[which(play_stat_df$statId %in% c(21, 22, 113, 115))])
        # Only the first one
        receiver_id <- receiver_id[1]
      } else {
        receiver_id <- "None"
      }
      # Return as a dataframe:
      result <- as.data.frame(list("AirYards" = airyards,
                                   "YardsAfterCatch" = yac,
                                   "QBHit" = qbhit,
                                   "Timeout_Indicator" = to,
                                   "Timeout_Team" = to_team,
                                   "Passer_ID" = passer_id,
                                   "Rusher_ID" = rusher_id,
                                   "Receiver_ID" = receiver_id))
    }
    colnames(result) <- c("AirYards","YardsAfterCatch","QBHit","Timeout_Indicator","Timeout_Team",
                          "Passer_ID","Rusher_ID","Receiver_ID")
    result$Timeout_Team <- as.character(result$Timeout_Team)
    result$Passer_ID <- as.character(result$Passer_ID)
    result$Rusher_ID <- as.character(result$Rusher_ID)
    result$Receiver_ID <- as.character(result$Receiver_ID)
    return(result)
  }
  
  
  # Generate the dataframe with the extra stats:
  # Catch the situation with GameID == 2013092206 and drive == 3
  if (GameID == 2013092206) {
    pbp_extrastats <- lapply(c(1,2,4:number.drives),
                             function(x) suppressWarnings(get_play_stats(nfl.json[[1]]$drives[[x]]))) %>% 
      dplyr::bind_rows()
  } else {
    pbp_extrastats <- lapply(c(1:number.drives),
                             function(x) suppressWarnings(get_play_stats(nfl.json[[1]]$drives[[x]]))) %>% 
      dplyr::bind_rows()
  }
  
  # Add to the PBP dataset:
  
  PBP <- cbind(PBP, pbp_extrastats)
  
  # Adjusting Possession Team
  PBP$posteam <- ifelse(PBP$posteam == "NULL", dplyr::lag(PBP$posteam),
                                              PBP$posteam)
  
  # Fixing Possession team for Kick-Offs
  kickoff.index <- which(sapply(PBP$desc, regexpr,
                                pattern =
                                  "kicks") != -1)
  pos.teams <- unlist(unique(PBP$posteam))[1:2]
  correct.kickoff.pos <- ifelse(PBP$posteam[kickoff.index] == pos.teams[1],
                                pos.teams[2], pos.teams[1])
  PBP[kickoff.index, "posteam"] <- correct.kickoff.pos

  # Yard Line Information
  
  # In the earlier seasons when there was a dead ball (i.e. timeout)
  # the yardline info was left blank or NULL. Also if the ball was at midfield then
  # there was no team associated so I had to add a space to make the strsplit
  # work
  yline.info.1 <- ifelse(PBP$yrdln == "50", "MID 50", PBP$yrdln)  
  yline.info.1 <- ifelse(nchar(PBP$yrdln) == 0 |
                         PBP$yrdln == "NULL", dplyr::lag(PBP$yrdln), 
                         yline.info.1)
  

  yline.info <- sapply(yline.info.1, strsplit, split = " ")
  
  PBP$SideofField <- sapply(yline.info, FUN = function(x) x[1])
  PBP$yrdln <- as.numeric(sapply(yline.info, FUN = function(x) x[2]))
  
  # Yard Line on 100 yards Scale: Distance from Opponent Endzone
  
  PBP$yrdline100 <- ifelse(PBP$SideofField == PBP$posteam | PBP$yrdln == 50, 
                          100 - PBP$yrdln, PBP$yrdln )
  
  # Game Date  
  date.step1 <- stringr::str_extract(urlstring, pattern = "/[0-9]{10}/")
  date.step2 <- stringr::str_extract(date.step1, pattern = "[0-9]{8}")
  year <- substr(date.step2, start = 1, stop = 4)
  month <- substr(date.step2, start = 5, stop = 6)
  day <- substr(date.step2, start = nchar(date.step2)-1, 
                stop = nchar(date.step2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  PBP$Date <- date
  PBP$GameID <- stringr::str_extract(date.step1, pattern = "[0-9]{10}")
  
  # Adding Zero time to Quarter End
  
  quarter.end <- which(sapply(PBP$desc, regexpr, 
                              pattern = "END QUARTER|END GAME") != -1)
  
  PBP$time[quarter.end] <- "00:00"
  
  # Time in Seconds 
  qtr.timeinsecs <- lubridate::period_to_seconds(lubridate::ms(PBP$time))
  
  # Quarter 1
  qtr.timeinsecs[which(PBP$qtr == 1)] <- qtr.timeinsecs[
                                            which(PBP$qtr == 1)] + (900*3)
  # Quarter 2
  qtr.timeinsecs[which(PBP$qtr == 2)] <- qtr.timeinsecs[
                                            which(PBP$qtr == 2)] + (900*2)
  # Quarter 3
  qtr.timeinsecs[which(PBP$qtr == 3)] <- qtr.timeinsecs[
                                            which(PBP$qtr == 3)] + 900
  # Overtime 
  qtr.timeinsecs[which(PBP$qtr == 5)] <- qtr.timeinsecs[
                                            which(PBP$qtr == 5)] - 900
  
  PBP$TimeSecs <- qtr.timeinsecs
  
  # Time Difference (in seconds)
  plays.time.diff <- abs(c(0, diff(qtr.timeinsecs)))
  
  PBP$PlayTimeDiff <- plays.time.diff
  
  ## Challenge or Replay Review ##
  
  # Binary 
  PBP$Challenge.Replay <- 0
  
  replay.offic <- grep(PBP$desc, pattern = "Replay Official reviewed")
  challenged <- grep(PBP$desc, pattern = "challenge")
  
  PBP$Challenge.Replay[c(replay.offic, challenged)] <- 1
  
  # Results

  PBP$ChalReplayResult <- NA
  
  upheld.play <- grep(PBP$desc, pattern = "the play was Upheld")
  reversed.play <- grep(PBP$desc, pattern = "the play was REVERSED")
  
  PBP$ChalReplayResult[upheld.play] <- "Upheld"
  PBP$ChalReplayResult[reversed.play] <- "Reversed"
  
  ######################################
  # Picking Apart the Description Column
  ######################################
  
  # Yards Gained
  yards.step1 <- sapply(PBP$desc, stringr::str_extract, 
                        pattern = "for (-)?([0-9]{1,2})?")
  
  PBP$Yards.Gained <- as.numeric( ifelse( grepl(x = yards.step1, 
                                               pattern = "(-)?([0-9]{1,2})"), 
                                          stringr::str_extract(yards.step1, 
                                                            "(-)?([0-9]{1,2})"), 
                                          "0")
                                  )
  
  # Two Point Conversion 
  PBP$TwoPointConv <- NA
  
  two.point.result.ind <- which(sapply(PBP$desc, regexpr, 
                                       pattern = 
                                         "TWO-POINT CONVERSION ATTEMPT") != -1)
  
  two.point.result2 <- stringr::str_extract_all(PBP$desc[two.point.result.ind],
                                                pattern = "ATTEMPT FAILS|SUCCEEDS")
  
  two.point.result.final1 <- unlist(lapply(two.point.result2, tail, 1))
  two.point.result.final2 <- ifelse(two.point.result.final1 == "ATTEMPT FAILS",
                                    "Failure", "Success")
  
  if (length(two.point.result.final2) != 0) {
  PBP$TwoPointConv[two.point.result.ind] <- two.point.result.final2
  }

  # Penalty - Binary Column 
  PBP$Accepted.Penalty <- NA
  penalty.play <- sapply(PBP$desc, stringr::str_extract, pattern = "PENALTY")
  PBP$Accepted.Penalty <- ifelse(!is.na(penalty.play), 1, 0)
  
  # Penalized Team
  penalized.team.s1 <- sapply(PBP$desc, stringr::str_extract,  
                                            "PENALTY on [A-Z]{2,3}")
  PBP$PenalizedTeam <- stringr::str_extract(penalized.team.s1,  
                                            "[A-Z]{2,3}$")
  
  PBP$PenalizedTeam <- ifelse(PBP$PenalizedTeam == "BLT", 
                              "BAL", 
                              ifelse(PBP$PenalizedTeam == "JAX",
                                     "JAC",
                                     ifelse(PBP$PenalizedTeam == "HST",
                                            "HOU", 
                                            ifelse(PBP$PenalizedTeam == "SL",
                                                   "STL", 
                                                   ifelse(PBP$PenalizedTeam == "ARZ",
                                                          "ARI",
                                                          PBP$PenalizedTeam)))))
  
  # Penalty - What was the penalty?
  penalty.type.s1 <- sapply(PBP$desc, stringr::str_extract,  
                                  pattern ="PENALTY(.){5,25},.+, [0-9] yard(s)")
  penalty.type.s2 <- stringr::str_extract(pattern = ",.+,", penalty.type.s1)
  penalty.type.final <- stringr::str_sub(penalty.type.s2, 3, -2)
  
  PBP$PenaltyType <- penalty.type.final

  #   Penalized Player 
  PBP$PenalizedPlayer <- NA
  penalized.player.int <- sapply(PBP$desc[ which(PBP$Accepted.Penalty == 1) ], 
                                 stringr::str_extract, 
                                 pattern = 
  "[A-Z]{2,3}-[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  penalized.player2 <- stringr::str_extract(penalized.player.int, 
                                            pattern = 
             "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  PBP$PenalizedPlayer[PBP$Accepted.Penalty == 1] <- penalized.player2
  
  # Penalty Yards 
  PBP$Penalty.Yards <- NA
  penalty.yards.step1 <- sapply(PBP$desc, stringr::str_extract, 
                                pattern = ", [0-9]{1,2} yard(s?), enforced")
  PBP$Penalty.Yards <- ifelse(!is.na(penalty.yards.step1), 
                              as.numeric(stringr::str_extract(
                                                            penalty.yards.step1, 
                                                            "[0-9]{1,2}")
                                         ), 0)
  
  # Modifying Down Column
  PBP$down <- unlist(PBP$down)
  PBP$down[which(PBP$down == 0)] <- NA

  # Defenseive Team Column
  PBP$DefensiveTeam <- NA
  teams.step1 <- stringr::str_extract(unlist(unique(PBP$posteam)), "[A-Z]{2,3}")
  
  teams <- teams.step1[which(!is.na(teams.step1))]
  Team1 <- teams[1]
  Team2 <- teams[2]
  
  PBP$DefensiveTeam[which(PBP$posteam == Team1)] <- Team2
  PBP$DefensiveTeam[which(PBP$posteam == Team2)] <- Team1
  
  ### Type of Play Initialized ###
  
  PBP$PlayType <- NA
  
  ## Passer ##
  passer.step1 <- sapply(PBP$desc, stringr::str_extract, 
                         pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? pass")
  PBP$Passer <- stringr::str_extract(passer.step1, 
                                     pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  ## Receiver ##

  receiver.step1 <- sapply(PBP$desc, stringr::str_extract, 
                           pattern = 
      "pass ((incomplete )?[a-z]{4,5} [a-z]{4,6} )?to [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  PBP$Receiver <- stringr::str_extract(receiver.step1, 
                              pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  ## Tacklers ##
  
  tacklers.step1 <- sapply(PBP$desc, stringr::str_extract, 
                           pattern = "(yard(s?)|no gain) \\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?(;|,)?( )?([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?)?\\)\\.")
  
  # Identifying the tacklers on the play (either one or two)
  tacklers1 <- stringr::str_extract(tacklers.step1,
                           pattern = 
                     "\\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  tacklers1 <- stringr::str_extract(tacklers1, 
                           pattern = 
                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  # Pulling out tacklers names
  tacklers2 <- stringr::str_extract(tacklers.step1, 
                           pattern = 
                    "(;|,)( )[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  tacklers2 <- stringr::str_extract(tacklers2,
                           pattern = 
                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  PBP$Tackler1 <- tacklers1
  PBP$Tackler2 <- tacklers2
  
  # Player who blocks punt or field goal or extra point
  
  player.blocking1 <- stringr::str_extract(PBP$desc, 
                       pattern = "BLOCKED by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  player.blocking2 <- stringr::str_extract(player.blocking1, 
                                           pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
    
  PBP$BlockingPlayer <- player.blocking2
  # Timeouts
  ## Needs to be before Field Goal
  timeouts <- which(sapply(PBP$desc, regexpr, 
                           pattern = "[A-z]imeout( #[1-5] by)?") != -1)
  
  PBP$PlayType[timeouts] <- "Timeout"
  
  # Field Goal
  fieldgoal <- which(sapply(PBP$desc, regexpr, 
                            pattern = "field goal") != -1)
  fieldgoal.null <- which(sapply(PBP$desc, regexpr, 
                                 pattern = "field goal(.)+NULLIFIED") != -1)
  fieldgoal.rev <- which(sapply(PBP$desc, regexpr, 
                                pattern = "field goal(.)+REVERSED") != -1)
  fieldgoal <- setdiff(fieldgoal, c(fieldgoal.null, fieldgoal.rev))
  
  missed.fg <- which(sapply(PBP$desc, regexpr, 
                            pattern = "field goal is No Good") != -1)
  blocked.fg  <- which(sapply(PBP$desc, regexpr, 
                              pattern = "field goal is BLOCKED") != -1)
  made.fg  <- which(sapply(PBP$desc, regexpr, 
                           pattern = "field goal is GOOD") != -1)
  
  PBP$PlayType[fieldgoal] <- "Field Goal"
  
  # Field Goal Distance 
  fieldgoaldist.prelim <- sapply(PBP$desc[fieldgoal], stringr::str_extract, 
                                 pattern = "[0-9]{1,2} yard field goal")
  
  fieldgoaldist <- sapply(fieldgoaldist.prelim, stringr::str_extract,
                          pattern = "[0-9]{1,2}")
  
  PBP$FieldGoalDistance <- NA
  
  PBP$FieldGoalDistance[fieldgoal] <- fieldgoaldist
  
  # Field Goal Result
  PBP$FieldGoalResult <- NA
  PBP$FieldGoalResult[missed.fg] <- "No Good"
  PBP$FieldGoalResult[blocked.fg] <- "Blocked"
  PBP$FieldGoalResult[made.fg] <- "Good"
  
  # Pass Plays
  PBP$PassOutcome <- NA
  pass.play <- which(sapply( PBP$desc, regexpr, pattern = "pass( )") != -1)
  incomplete.pass.play <- which(sapply(PBP$desc, regexpr, 
                                       pattern = 
                                         "(pass incomplete)|INTERCEPTED|(is incomplete)") != -1)
  
  PBP$PlayType[pass.play] <- "Pass"
  
  # Pass Outcome
  PBP$PassOutcome[incomplete.pass.play] <- "Incomplete Pass"
  PBP$PassOutcome[ setdiff(pass.play, incomplete.pass.play) ] <- "Complete"
  
  # Pass Length
  PBP$PassLength <- NA
  short.pass <- which(sapply(PBP$desc, regexpr, 
                             pattern = "pass (incomplete )?short") != -1)
  deep.pass <- which(sapply(PBP$desc, regexpr, 
                            pattern = "pass (incomplete )?deep") != -1)
  PBP$PassLength[short.pass] <- "Short"
  PBP$PassLength[deep.pass] <- "Deep"
  
  # Pass Location
  PBP$PassLocation <- NA
  pass.left <- which(sapply(PBP$desc, regexpr, 
                            pattern = "(short|deep) left") != -1)
  pass.reft <- which(sapply(PBP$desc, regexpr, 
                             pattern = "(short|deep) right") != -1)   
  pass.middle <- which(sapply(PBP$desc, regexpr, 
                              pattern = "(short|deep) middle") != -1) 
  PBP$PassLocation[pass.left] <- "left"
  PBP$PassLocation[pass.reft] <- "right"
  PBP$PassLocation[pass.middle] <- "middle"
  
  # Pass Attempt
  PBP$PassAttempt <- NA
  PBP$PassAttempt <- ifelse( sapply(PBP$desc, grepl, 
                                   pattern = "pass"), 1, 0)
  
  # Reception Made
  PBP$Reception <- 0
  PBP$Reception[setdiff(pass.play,incomplete.pass.play)] <- 1
  
  # Interception Thrown
  PBP$InterceptionThrown <- ifelse(
                                  sapply(PBP$desc, grepl, 
                                        pattern = "INTERCEPTED"), 1, 0
                                  )
  
  # Punt
  punt.play <- which(sapply(PBP$desc, regexpr, pattern = "punt(s)?") != -1)
  
  PBP$PlayType[punt.play] <- "Punt"
  
  # Punt Result
  PBP$PuntResult <- NA
  
  blocked.punts <- grep(PBP$desc[punt.play], pattern = "BLOCKED")
  
  PBP$PuntResult[punt.play] <- "Clean"
  PBP$PuntResult[punt.play][blocked.punts] <- "Blocked"
  
  # Extra Point
  extrapoint.good <- which(sapply(PBP$desc, regexpr,
                                    pattern = "extra point is GOOD") != -1)
  
  extrapoint.nogood <- which(sapply(PBP$desc, regexpr, 
                                    pattern = "(extra point is No Good)") != -1)
  
  extrapoint.blocked <- which(sapply(PBP$desc, regexpr, 
                                    pattern = "(extra point is Blocked)") != -1)
  
  extrapoint.aborted <- which(sapply(PBP$desc, regexpr, 
                                     pattern = "(extra point is Aborted)") != -1)
  
  PBP$PlayType[c(extrapoint.good, 
                 extrapoint.nogood,
                 extrapoint.blocked,
                 extrapoint.aborted)] <- "Extra Point"
  
  # Extra Point Result
  PBP$ExPointResult <- NA
  PBP$ExPointResult[extrapoint.good] <- "Made"
  PBP$ExPointResult[extrapoint.nogood] <- "Missed"
  PBP$ExPointResult[extrapoint.blocked] <- "Blocked"
  PBP$ExPointResult[extrapoint.aborted] <- "Aborted"
  
  # Touchdown Play 
  
  touchdown.step1 <- grep(PBP$desc, pattern = "TOUCHDOWN")
  nullified <- grep(PBP$desc[touchdown.step1], pattern = "NULLIFIED")
  reversed <- grep(PBP$desc[touchdown.step1], pattern = "(REVERSED)$")
  
  if (length(c(nullified, reversed)) > 0) {
    touchdown.step1 <- touchdown.step1[-c(nullified, reversed)]
  } 
  
  PBP$Touchdown <- 0
  PBP$Touchdown[touchdown.step1] <- 1
  
  # Defensive 2-pt conversion
  def.twopt.suc <- which(sapply(PBP$desc, regexpr,
                            pattern = "DEFENSIVE TWO-POINT ATTEMPT\\. (.){1,70}\\. ATTEMPT SUCCEEDS") != -1)
  
  def.twopt.fail <- which(sapply(PBP$desc, regexpr,
                                pattern = "DEFENSIVE TWO-POINT ATTEMPT\\. (.){1,70}\\. ATTEMPT FAILS") != -1)
  
  PBP$DefTwoPoint <- NA
  PBP$DefTwoPoint[def.twopt.suc] <- "Success"
  PBP$DefTwoPoint[def.twopt.fail] <- "Failure"
  
  all.2pts <- intersect(c(def.twopt.suc, def.twopt.fail), two.point.result.ind)
  PBP$TwoPointConv[all.2pts] <- "Failure"
  
  # Fumbles
  
  PBP$Fumble <- 0
  fumble.index1 <- which(sapply(PBP$desc, regexpr, pattern = "FUMBLE") != -1) 
  fumble.overruled <- which(sapply(PBP$desc[fumble.index1], 
                               regexpr, 
                               pattern = "(NULLIFIED)|(Reversed)") != -1)
  fumble.index <- setdiff(fumble.index1, fumble.overruled)
  PBP$Fumble[fumble.index] <- 1
  
  # Quarter End
  end.quarter <- which(sapply(PBP$desc, regexpr, 
                              pattern = "(END QUARTER)|(End of quarter)") != -1)
  
  PBP$PlayType[end.quarter] <- "Quarter End"

  # Half End 
  end.half <- which(sapply(PBP$desc, regexpr, 
                           pattern = "End of half") != -1)
  
  PBP$PlayType[end.half] <- "Half End"
  
  # 2 Minute Warning
  two.minute.warning <- which(sapply(PBP$desc, regexpr, 
                                     pattern = "Two-Minute Warning") != -1)
  
  PBP$PlayType[two.minute.warning] <- "Two Minute Warning"
  
  # Sack 
  sack.plays <- which(sapply(PBP$desc, regexpr, pattern = "sacked") != -1)
  
  PBP$PlayType[sack.plays] <- "Sack"
  
  # Sack- Binary
  PBP$Sack <- 0
  PBP$Sack[sack.plays] <- 1
  
  # Tacklers on a sack play
  
  sackers <- sapply(PBP$desc[which(PBP$Sack == 1 & is.na(PBP$Tackler1))], stringr::str_extract, 
         pattern = "(split by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? and [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\))|(\\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?, [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\))|\\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\)|Sack by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")

  # Sack Player 1
  
  sackplayer1 <- stringr::str_extract(sackers,
                                      pattern = 
                                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")

  # Sack Player 2
  sackplayer2 <- stringr::str_extract(sackers,
                                      pattern = 
                                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\)")
  
  sackplayer2 <- stringr::str_extract(sackplayer2,
                                      pattern = 
                                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
 # Adding them to the Tacklers Column
  
  # Tackler 2 must go first so that second conditional remains true
  PBP$Tackler2[which(PBP$Sack == 1 & is.na(PBP$Tackler1))] <- sackplayer2
  PBP$Tackler1[which(PBP$Sack == 1 & is.na(PBP$Tackler1))] <- sackplayer1
  
  # No Play
  no.play <- which(sapply(PBP$desc, regexpr, 
                          pattern = "(No Play)|(\\(Kick formation\\) PENALTY)") != -1)#|((\\(Kick formation\\) )?PENALTY)") != -1)
  
  no.play <- which(sapply(PBP$desc, regexpr, 
                          pattern = "(No Play)|(\\(Kick formation\\) PENALTY)") != -1)#|((\\(Kick formation\\) )?PENALTY)") != -1)
  
  def.penalty.noplay <- which(PBP$PenalizedTeam[no.play] != PBP$posteam[no.play])
  
  PBP$PlayType[no.play] <- "No Play"

  # Safety - Binary
  safety.plays <- which(sapply(PBP$desc, regexpr, pattern = "SAFETY") != -1)
  PBP$Safety <- 0
  PBP$Safety[safety.plays] <- 1
  
  # QB Kneel
  qb.kneel <- which(sapply(PBP$desc, regexpr, pattern = "kneels") != -1)
  
  PBP$PlayType[qb.kneel] <- "QB Kneel"
  
  # Kick Off
  kickoff <- which(sapply(PBP$desc, regexpr, 
                          pattern = "(kick(s)? [0-9]{2,3})|((O|o)nside)") != -1)
  
  PBP$PlayType[kickoff] <- "Kickoff"

  
  # Onside Kick
  
  onside <- sapply(PBP$desc, regexpr, pattern = "(O|o)nside( (K|k)ick)?")
  
  PBP$Onsidekick <- ifelse(onside != -1, 1, 0)
  
  # Spike
  spike.play <- which(sapply(PBP$desc, regexpr, pattern = "spiked") != -1)
  
  PBP$PlayType[spike.play] <- "Spike"
  
  # End of Game 
  end.game <- which(sapply(PBP$desc, regexpr,
                           pattern = "(END GAME)|(End of game)") != -1)
  
  PBP$PlayType[end.game] <- "End of Game"
  
  # First Down 
  PBP$FirstDown <- 0
  
  first.downplays <- which(PBP$down == 1)
  first.downs <- first.downplays-1
  PBP$FirstDown[first.downs] <- ifelse(PBP$down[first.downs] ==0, NA, 1)
  
  # Running Play
  running.play <- which(is.na(PBP$PlayType))
  PBP$PlayType[running.play] <- "Run"
  
  # Fix plays that are run plays but were challenged and lost, so a timeout was
  # lost
  running.play2 <- which(sapply(PBP$desc[which(PBP$PlayType == "Timeout")], 
                                regexpr, 
                                pattern = "(((left|right) (tackle|end|guard))|(up the middle)) (for|to)") != -1)

  PBP[which(PBP$PlayType == "Timeout"),"PlayType"][running.play2] <- "Run"
  
  PBP$RushAttempt <- ifelse(PBP$PlayType == "Run", 1, 0)
    
    ##################
    ### SOLVE: Look at timeout play and search for "(left|right) (tack|end|guard) for..."

  # Run Direction
  PBP$RunLocation <- NA
  
  run.left <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                           pattern = "left") != -1)
  run.right <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                            pattern = "right") != -1)   
  run.middle <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                             pattern = "middle") != -1) 
  
  PBP[c(running.play, running.play2),"RunLocation"][run.left] <- "left"
  PBP[c(running.play, running.play2),"RunLocation"][run.right] <- "right"
  PBP[c(running.play, running.play2),"RunLocation"][run.middle] <- "middle"
  
  # Run Gap
  PBP$RunGap <- NA
  
  run.guard <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                           pattern = "guard") != -1)
  
  run.tackle <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                            pattern = "tackle") != -1)   
  
  run.end <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                         pattern = "end") != -1) 

  PBP[c(running.play, running.play2),"RunGap"][run.guard] <- "guard"
  PBP[c(running.play, running.play2),"RunGap"][run.tackle] <- "tackle"
  PBP[c(running.play, running.play2),"RunGap"][run.end] <- "end"
  
  # Rusher
  
  rusherStep1 <- sapply(PBP[which(PBP$PlayType == "Run"),"desc"], 
                        stringr::str_extract, 
                        pattern = "[A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  # Catch the Aborted phrase:
  rusherStep1 <- ifelse(stringr::str_detect(rusherStep1," Aborted"),
                        gsub(" Aborted","",rusherStep1),rusherStep1)
  
  PBP[c(running.play, running.play2),"Rusher"] <- rusherStep1
  
  # Changing to correctly reflect the rusher 

  elidgiblePlays <- grep(PBP[which( (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 0) | (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 1 &PBP$posteam != PBP$PenalizedTeam)),"desc"],
                         pattern = " (report(ed|s)?)?( in )?as eligible(.){15,}")
  
  if (length(elidgiblePlays) > 0) {
    
  rusherStep2 <- sapply(PBP[which( (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 0) | (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 1 &PBP$posteam != PBP$PenalizedTeam)),"desc"], 
         stringr::str_extract, 
         pattern = "(as eligible( receiver(s)?| for [A-Z]{2,3})?(\\.|,)?( ){1,2}(Direct snap to [A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\.( ){1,2})?[A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?)|(as eligible( receiver)?(\\.)?(.)+(\\.)? (\\(Shotgun\\) )?[A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?)|(\\(((Run formation)|([0-9]{1,2}:[0-9]{1,2}))\\) [A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? (.){10,80}report(s|ed) as eligible(.){11,})")

  rusherStep3 <- sapply(rusherStep2, 
                        stringr::str_extract, 
                        pattern = "([A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?)$|\\(((Run formation)|([0-9]{1,2}:[0-9]{1,2})|(Shotgun))\\) [A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?") 
  
  rusherStep3.1 <- sapply(rusherStep3, 
                          stringr::str_extract, 
                          pattern = "([A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?)$")
                                     #([A-z]{1,4}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?)$) 
  
  rusherStepfinal <- rusherStep3.1[!is.na(rusherStep3.1)]
  
  PBP$Rusher[which(PBP$PlayType == "Run")][elidgiblePlays] <- rusherStepfinal
  
  }
  
  ## Punt and Kick Return Outcome ##
  
  # Punt Outcome
  punt.tds <- which(sapply(PBP$desc[punt.play], regexpr, 
                           pattern = "TOUCHDOWN") != -1)
  punt.tds.null <- which(sapply(PBP$desc[punt.play], regexpr, 
                           pattern = "NULLIFIED") != -1)
  punt.tds.rev <- which(sapply(PBP$desc[punt.play], regexpr, 
                           pattern = "REVERSED") != -1)
  punt.tds <- setdiff(punt.tds, c(punt.tds.null, punt.tds.rev))
  
  punts.touchbacks <- which(sapply(PBP$desc[punt.play], regexpr, 
                                   pattern = "Touchback") != -1)
  punts.faircatch <- which(sapply(PBP$desc[punt.play], regexpr, 
                                  pattern = "fair catch") != -1)
  # Kickoff Outcome
  kick.tds <- which(sapply(PBP$desc[kickoff], regexpr, 
                           pattern = "TOUCHDOWN") != -1)
  kick.tds.null <- which(sapply(PBP$desc[kickoff], regexpr, 
                           pattern = "NULLIFIED") != -1)
  kick.tds.rev <- which(sapply(PBP$desc[kickoff], regexpr, 
                           pattern = "REVERSED") != -1)
  kick.tds <- setdiff(kick.tds, c(kick.tds.null, kick.tds.rev))
  
  kick.tds.null <- which(sapply(PBP$desc[kickoff], regexpr, 
                           pattern = "NULLIFIED") != -1)
  kick.tds.rev <- which(sapply(PBP$desc[kickoff], regexpr, 
                                pattern = "REVERSED") != -1)
  kick.tds <- setdiff(kick.tds, c(kick.tds.null, kick.tds.rev))
  
  kick.touchbacks <- which(sapply(PBP$desc[kickoff], regexpr, 
                          pattern = "Touchback") != -1)
  kick.faircatch <- which(sapply(PBP$desc[kickoff], regexpr, 
                                 pattern = "fair catch") != -1)
  kick.kneels <- which(sapply(PBP$desc[kickoff], regexpr, 
                              pattern = "kneel(s)?") != -1)

  # Interception Outcome
  intercept.td <- which(sapply(PBP$desc[which(PBP$InterceptionThrown == 1)], 
                               regexpr, pattern = "TOUCHDOWN") != -1)
  intercept.td.null <- which(sapply(PBP$desc[which(PBP$InterceptionThrown == 1)] 
                               , regexpr, pattern = "NULLIFIED") != -1)
  intercept.td.rev <- which(sapply(PBP$desc[which(PBP$InterceptionThrown == 1)], 
                               regexpr, pattern = "REVERSED") != -1)
  
  intercept.td <- setdiff(intercept.td, c(intercept.td.null, intercept.td.rev))

  # Fumble Outcome
  fumble.td <- which(sapply(PBP$desc[fumble.index], 
                               regexpr, pattern = "TOUCHDOWN") != -1)
  # May be able to remove bottom two lines
  fumble.td.null <- which(sapply(PBP$desc[fumble.index], 
                            regexpr, pattern = "NULLIFIED") != -1)
  fumble.td.rev <- which(sapply(PBP$desc[fumble.index], 
                                 regexpr, pattern = "REVERSED") != -1)
  
  fumble.td <- setdiff(fumble.td, c(fumble.td.null, fumble.td.rev))
  
  PBP$ReturnResult <- NA
  PBP$ReturnResult[punt.play][punt.tds] <- "Touchdown"
  PBP$ReturnResult[punt.play][punts.touchbacks] <- "Touchback"
  PBP$ReturnResult[punt.play][punts.faircatch] <- "Fair Catch"
  PBP$ReturnResult[kickoff][kick.tds] <- "Touchdown"
  PBP$ReturnResult[kickoff][kick.touchbacks] <- "Touchback"
  PBP$ReturnResult[kickoff][c(kick.faircatch,kick.kneels)] <- "Fair Catch"
  PBP$ReturnResult[which(PBP$InterceptionThrown == 1)][intercept.td] <- "Touchdown"
  PBP$ReturnResult[fumble.index][fumble.td] <- "Touchdown"

  ##  Returner ##
  # Punt Returner
  
  # Fair Catches
  punt.returner1 <- sapply(PBP$desc[punt.play], stringr::str_extract, 
                           pattern = "by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\.$")
  
  punt.returner2 <- sapply(punt.returner1, stringr::str_extract,
                           pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  # Touchdowns or Returns
  punt.returner3 <- sapply(PBP$desc[punt.play], stringr::str_extract, 
                           pattern = "(\\. [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? to [A-Z]{2,3} [0-9]{1,2})|\\. [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? for [0-9]{1,2} yard(s)")
  
  punt.returner4 <- sapply(punt.returner3, stringr::str_extract, 
                           pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")

  # Kickoff Returner
  # Fair Catches
  kickret1 <- sapply(PBP$desc[kickoff], stringr::str_extract, 
                     pattern = "by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?\\.$")
  
  kickret2 <- sapply(kickret1, stringr::str_extract,
                     pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  # Touchdowns or Returns
  kickret3 <- sapply(PBP$desc[kickoff], stringr::str_extract, 
                     pattern = "(\\. [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? to [A-Z]{2,3} [0-9]{1,2})|(\\. [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? for [0-9]{1,2} yard(s))|(\\. [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)? pushed)")
  
  kickret4 <- sapply(kickret3, stringr::str_extract, 
                     pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  # All Returners
  all.returners <- rep(NA, time = nrow(PBP))
  all.returners[kickoff][which(!is.na(kickret2))] <- kickret2[which(!is.na(kickret2))]
  all.returners[kickoff][which(!is.na(kickret4))] <- kickret4[which(!is.na(kickret4))]
  all.returners[punt.play][which(!is.na(punt.returner2))] <- punt.returner2[which(!is.na(punt.returner2))]
  all.returners[punt.play][which(!is.na(punt.returner4))] <- punt.returner4[which(!is.na(punt.returner4))]
  
  PBP$Returner <- all.returners
  
  # Interceptor 
  interceptor1 <- sapply(PBP$desc[which(PBP$InterceptionThrown == 1)], 
                         stringr::str_extract, 
                         pattern = "INTERCEPTED by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  interceptor2 <- sapply(interceptor1, stringr::str_extract, 
                         pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  PBP$Interceptor <- NA
  PBP$Interceptor[which(PBP$InterceptionThrown == 1)] <- interceptor2

  # Fumbler Recovery Team and Player
  
  recover.step1 <- sapply(PBP$desc[fumble.index], stringr::str_extract, 
                          pattern = "[A-Z]{2,3}-[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  recover.team <- sapply(recover.step1, stringr::str_extract, 
                         pattern = "[A-Z]{2,3}")
  
  recover.player <- sapply(recover.step1, stringr::str_extract, 
                           pattern = "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( (S|J)r\\.)?")
  
  PBP$RecFumbTeam <- NA
  PBP$RecFumbTeam[fumble.index] <-  ifelse(recover.team == "BLT", 
                                           "BAL", 
                                           ifelse(recover.team == "JAX",
                                                  "JAC",
                                                  ifelse(recover.team == "HST",
                                                         "HOU", 
                                                         ifelse(recover.team == "SL",
                                                                "STL", 
                                                                recover.team))))
  PBP$RecFumbPlayer <- NA
  PBP$RecFumbPlayer[fumble.index] <- recover.player
  
  # The next few variables are counting variables
  # Used to help set up model for predictions 

  # Plays 
  PBP$PlayAttempted <- 1
  
  time.format <- as.POSIXct(paste("00:",
                           unlist(PBP$time), 
                           sep = ""), format = "%H:%M:%S")
  # For the rows when "end of half occurs", need to lag the time 
  # for the previous play.  Run multiple times for back to back plays with 
  # NA time
  time.format[which(is.na(time.format))] <- time.format[which(is.na(time.format))-1]
  time.format[which(is.na(time.format))] <- time.format[which(is.na(time.format))-1]
  time.format[which(is.na(time.format))] <- time.format[which(is.na(time.format))-1]
  # Time Under
  
  PBP$TimeUnder <- substr(lubridate::ceiling_date(time.format, "minute"), 
                          15, 16)
  PBP$TimeUnder <- as.numeric(as.character(PBP$TimeUnder))
  
  # Calculating Score of Game for Possesion team and Defensive Team
  
  team.home.score <- rep(0, times = nrow(PBP))
  team.away.score <- rep(0, times = nrow(PBP))
   
  away.team.name <- nfl.json[[1]]$away$abbr
  home.team.name <- nfl.json[[1]]$home$abbr
  
  ## Away Team ##
  # Regular offensive passing, rushing
  team.away.score[which(PBP$Touchdown %>% dplyr::lag() == 1 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & !PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & !PBP$PlayType %>% dplyr::lag()  %in% "Kickoff"
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Give points for Kickoff TDs
  team.away.score[which(PBP$Touchdown  %>% dplyr::lag() == 1 
                        & PBP$posteam  %>% dplyr::lag() == away.team.name
                        & PBP$ReturnResult  %>% dplyr::lag() %in% "Touchdown"
                        & PBP$PlayType  %>% dplyr::lag() %in% "Kickoff",
                        PBP$sp %>% dplyr::lag() == 1)] <- 6
  # Give points for Punt Return TDs
  team.away.score[which(PBP$Touchdown %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == home.team.name
                        & PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & PBP$PlayType %>% dplyr::lag()  %in% "Punt"
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Give points for Interceptions
  team.away.score[which(PBP$Touchdown  %>% dplyr::lag() == 1 
                        & PBP$posteam  %>% dplyr::lag() == home.team.name
                        & PBP$ReturnResult  %>% dplyr::lag() %in% "Touchdown"
                        & !is.na(PBP$Interceptor %>% dplyr::lag())
                        & PBP$sp  %>% dplyr::lag() == 1)] <- 6
  # Make sure to give away team points for fumble ret for TD
  team.away.score[which(PBP$Touchdown %>% dplyr::lag() == 1 
                        & PBP$posteam %>% dplyr::lag()  == home.team.name 
                        & PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & !PBP$PlayType %>% dplyr::lag()  %in% "Kickoff"
                        & PBP$RecFumbTeam %>% dplyr::lag()  == away.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Fumble and the team that fumbled recovers and scores a TD
  team.away.score[which(PBP$Touchdown %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name 
                        & PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & PBP$RecFumbTeam %>% dplyr::lag()  == away.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Points for two point conversion
  team.away.score[which(PBP$TwoPointConv %>% dplyr::lag()  == "Success" 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 2
  # Points for safeties
  team.away.score[which(PBP$Safety %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == home.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 2
  # Points for made extra point
  team.away.score[which(PBP$ExPointResult %>% dplyr::lag()  == "Made" 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 1
  # Points for made field goal
  team.away.score[which(PBP$FieldGoalResult %>% dplyr::lag()  == "Good" 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 3
  
  team.away.score <- cumsum(team.away.score)
  
  away.team.pos <- which(PBP$posteam == away.team.name)
  away.team.def <- which(PBP$DefensiveTeam == away.team.name)
  
  ## Home Team ##
  # Regular offensive passing, rushing
  team.home.score[PBP$Touchdown %>% dplyr::lag()  == 1 
                  & PBP$posteam %>% dplyr::lag()  == home.team.name 
                  & !PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                  & !PBP$PlayType %>% dplyr::lag()  %in% "Kickoff"
                  & PBP$sp %>% dplyr::lag()  == 1] <- 6
  # Give points for Kickoffs
  team.home.score[which(PBP$Touchdown %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == home.team.name
                        & PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & PBP$PlayType %>% dplyr::lag()  %in% "Kickoff"
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Give points for Punts 
  team.home.score[which(PBP$Touchdown %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & PBP$PlayType %>% dplyr::lag()  %in% "Punt"
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Give points for Interceptions
  team.home.score[which(PBP$Touchdown %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & PBP$ReturnResult %>% dplyr::lag()  %in% "Touchdown"
                        & !is.na(PBP$Interceptor %>% dplyr::lag() )
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  
  team.home.score[which(PBP$Touchdown %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name 
                        & PBP$ReturnResult %in% "Touchdown"
                        & !PBP$PlayType %>% dplyr::lag()  %in% "Kickoff"
                        & PBP$RecFumbTeam %>% dplyr::lag()  == home.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 6
  # Fumble and the team that fumbled recovered and scored a TD
  team.home.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name 
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$RecFumbTeam == home.team.name
                        & PBP$sp == 1)] <- 6
  # Points for two point conversion
  team.home.score[which(PBP$TwoPointConv %>% dplyr::lag()  == "Success" 
                        & PBP$posteam %>% dplyr::lag()  == home.team.name)] <- 2
  # Points for safeties
  team.home.score[which(PBP$Safety %>% dplyr::lag()  == 1 
                        & PBP$posteam %>% dplyr::lag()  == away.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 2
  # Points for made extra point
  team.home.score[which(PBP$ExPointResult %>% dplyr::lag()  == "Made" 
                        & PBP$posteam %>% dplyr::lag()  == home.team.name
                        & PBP$sp %>% dplyr::lag()  == 1)] <- 1
  # Points for made field goal
  team.home.score[which(PBP$FieldGoalResult %>% dplyr::lag()  == "Good" 
                        & PBP$posteam %>% dplyr::lag() == home.team.name
                        & PBP$sp %>% dplyr::lag() == 1)] <- 3
  
  team.home.score <- cumsum(team.home.score)
  
  home.team.pos <- which(PBP$posteam == home.team.name)
  home.team.def <- which(PBP$DefensiveTeam == home.team.name)
  
  
  ## Possesion and Defensive Team Scores
  PBP$PosTeamScore <- NA
  PBP$DefTeamScore <- NA
  
  ### Inputting Scores
  
  PBP$PosTeamScore[home.team.pos] <- team.home.score[home.team.pos]
  PBP$PosTeamScore[away.team.pos] <- team.away.score[away.team.pos]
  
  PBP$DefTeamScore[home.team.def] <- team.home.score[home.team.def]
  PBP$DefTeamScore[away.team.def] <- team.away.score[away.team.def]
  
  PBP$DefTeamScore[1] <- 0
  PBP$PosTeamScore[1] <- 0 
  
  PBP$DefTeamScore <- ifelse(is.na(PBP$DefTeamScore), lag(PBP$DefTeamScore),
                                    PBP$DefTeamScore)
  PBP$PosTeamScore <- ifelse(is.na(PBP$PosTeamScore), lag(PBP$PosTeamScore),
                                    PBP$PosTeamScore)
  
  # Score Differential #
  
  PBP$ScoreDiff <- PBP$PosTeamScore - PBP$DefTeamScore
  
  ##############################################################################
  ### Allows All Finals Plays of Game to Have a Score Difference ###
  
  PBP$ScoreDiff <- ifelse(PBP$PlayType == "End of Game", dplyr::lag(PBP$ScoreDiff)
                          , PBP$ScoreDiff)
  
  ##############################################################################
  
  # Abs Score Differential #
  
  PBP$AbsScoreDiff <- abs(PBP$PosTeamScore - PBP$DefTeamScore)
  
  # Goal to Go
  
  PBP$GoalToGo <- ifelse(PBP$SideofField != PBP$posteam & 
                        ((PBP$ydstogo == PBP$yrdline100) |
                        (PBP$ydstogo <= 1 & PBP$yrdline100 == 1)),
                         1,0)
  ##################
  
  ## Unlisting Listed Columns 
  PBP$sp <- unlist(PBP$sp)
  PBP$qtr <- unlist(PBP$qtr)
  PBP$time <- unlist(PBP$time)
  PBP$ydstogo <- unlist(PBP$ydstogo)
  PBP$ydsnet <- unlist(PBP$ydsnet)
  PBP$posteam <- unlist(PBP$posteam)
  PBP$desc <- unlist(PBP$desc)
  PBP$FieldGoalDistance <- unlist(PBP$FieldGoalDistance)
  PBP$Interceptor <- unlist(PBP$Interceptor)
  PBP$RecFumbPlayer <- unlist(PBP$RecFumbPlayer)
  
  ### Adding Posteam for final play of game ###
  ### Needed for win probability purposes #####
  PBP$posteam <- ifelse(PBP$PlayType == "End of Game", dplyr::lag(PBP$posteam),
                        PBP$posteam)
  
  ## Same for Defensive Team ##
  PBP$DefensiveTeam <- ifelse(PBP$PlayType == "End of Game", 
                              dplyr::lag(PBP$DefensiveTeam),
                              PBP$DefensiveTeam)
  
  ## Home Team ##
  PBP$HomeTeam <- nfl.json[[1]]$home$abbr
  
  ## Away Team 
  PBP$AwayTeam <- nfl.json[[1]]$away$abbr
  
  ## Half Indicator
  PBP$Half_Ind <- with(PBP,ifelse(qtr %in% c(1,2),"Half1",
                                  ifelse(qtr %in% c(3,4),"Half2","Overtime")))
  
  ## Keep track of timeouts remaining
  PBP$HomeTimeouts_Remaining_Pre <- vector(length = nrow(PBP))
  PBP$AwayTimeouts_Remaining_Pre <- vector(length = nrow(PBP))
  PBP$HomeTimeouts_Remaining_Post <- vector(length = nrow(PBP))
  PBP$AwayTimeouts_Remaining_Post <- vector(length = nrow(PBP))
  
  for (i in 1:nrow(PBP)){
    # if its the first play in the game set all timeouts remaining
    # columns to be set to 3, or if the it's the first play in
    # the second half
    if (i == 1){
      PBP$HomeTimeouts_Remaining_Pre[i] <- 3
      PBP$AwayTimeouts_Remaining_Pre[i] <- 3
      PBP$HomeTimeouts_Remaining_Post[i] <- 3
      PBP$AwayTimeouts_Remaining_Post[i] <- 3
    }  else if (PBP$Half_Ind[i-1] == "Half1" & PBP$Half_Ind[i] == "Half2"){
      PBP$HomeTimeouts_Remaining_Pre[i] <- 3
      PBP$AwayTimeouts_Remaining_Pre[i] <- 3
      PBP$HomeTimeouts_Remaining_Post[i] <- 3
      PBP$AwayTimeouts_Remaining_Post[i] <- 3
    } else if (PBP$Half_Ind[i-1] == "Half2" &  PBP$Half_Ind[i] == "Overtime") {
      # if it's the start of overtime then go to 2 timeouts
      PBP$HomeTimeouts_Remaining_Pre[i] <- 2
      PBP$AwayTimeouts_Remaining_Pre[i] <- 2
      PBP$HomeTimeouts_Remaining_Post[i] <- 2
      PBP$AwayTimeouts_Remaining_Post[i] <- 2
    } else {
      # the _Pre for columns are the _Post of the previous row
      PBP$HomeTimeouts_Remaining_Pre[i] <- PBP$HomeTimeouts_Remaining_Post[i-1]
      PBP$AwayTimeouts_Remaining_Pre[i] <- PBP$AwayTimeouts_Remaining_Post[i-1]
      PBP$HomeTimeouts_Remaining_Post[i] <- PBP$HomeTimeouts_Remaining_Post[i-1]
      PBP$AwayTimeouts_Remaining_Post[i] <- PBP$AwayTimeouts_Remaining_Post[i-1]
    }
    # If Timeout_Indicator == 1 then subtract 1 from which team
    # called the timeout:
    if (PBP$Timeout_Indicator[i] == 1){
      if (PBP$Timeout_Team[i] == PBP$HomeTeam[i]){
        PBP$HomeTimeouts_Remaining_Post[i] <- PBP$HomeTimeouts_Remaining_Pre[i] - 1
      } else{
        PBP$AwayTimeouts_Remaining_Post[i] <- PBP$AwayTimeouts_Remaining_Pre[i] - 1
      }
    }
  }
  
  # Create a column, posteam_timeouts_rem:
  PBP$posteam_timeouts_pre <- ifelse(PBP$posteam == PBP$HomeTeam,
                                     PBP$HomeTimeouts_Remaining_Pre,
                                     PBP$AwayTimeouts_Remaining_Pre)
  
  
  # Fixing error row in 2010 game:
  
  if (GameID == 2010112107){
    err_play_index <- which(PBP$qtr == 2 & PBP$down == 2 &
                              PBP$TimeSecs == 1862)
    PBP[err_play_index,"Passer"] <- "D.Brees"
    PBP[err_play_index,"PassAttempt"] <- 1
    PBP[err_play_index,"PassOutcome"] <- "Complete"
    PBP[err_play_index,"PassLength"] <- "Deep"
    PBP[err_play_index,"PassLocation"] <- "left"
    PBP[err_play_index,"PassLength"] <- 20
    PBP[err_play_index,"Yards.Gained"] <- 20
    PBP[err_play_index,"Touchdown"] <- 1
  }
  
  ## Adding in Expected Points ##
  
  PBP <- expected_points(PBP)
  
  ## Adding in Win Probability ##
  
  PBP <- win_probability(PBP)
  
  ##############################################################################
  ## Adding Row for End of Game if not in the data ##
  # if (PBP$desc[nrow(PBP)] %in% c("(END GAME)", "(End of game)")) {
  #   
  #   PBP[nrow(PBP)+1,] <- PBP[nrow(PBP),]
  #   
  #   PBP$GameID[nrow(PBP)] <- PBP$GameID[nrow(PBP)-1]
  #   PBP$Date[nrow(PBP)] <- PBP$Date[nrow(PBP)-1]
  #   PBP$time[nrow(PBP)] <- "00:00"
  #   PBP$TimeUnder[nrow(PBP)] <- 0
  #   PBP$TimeSecs[nrow(PBP)] <- 0
  #   PBP$desc[nrow(PBP)] <- "End of Game"
  #   PBP$PlayType[nrow(PBP)] <- "End of Game"
  # }
  
  ##############################################################################
  ## Final OutPut ##
  PBP[,c("Date", "GameID", "play_id", "Drive", "qtr", "down", "time", "TimeUnder", 
         "TimeSecs", "PlayTimeDiff", "SideofField", "yrdln", "yrdline100",
         "ydstogo", "ydsnet", "GoalToGo", "FirstDown", 
         "posteam", "DefensiveTeam", "desc", "PlayAttempted", "Yards.Gained", 
         "sp", "Touchdown", "ExPointResult", "TwoPointConv", "DefTwoPoint", 
         "Safety", "Onsidekick", "PuntResult", "PlayType", "Passer","Passer_ID", "PassAttempt", "PassOutcome", 
         "PassLength","AirYards","YardsAfterCatch","QBHit", "PassLocation", "InterceptionThrown", "Interceptor",
         "Rusher","Rusher_ID", "RushAttempt", "RunLocation", "RunGap",  "Receiver","Receiver_ID",
         "Reception", "ReturnResult", "Returner", "BlockingPlayer","Tackler1", "Tackler2", 
         "FieldGoalResult", "FieldGoalDistance", 
         "Fumble", "RecFumbTeam", "RecFumbPlayer", "Sack", "Challenge.Replay",
         "ChalReplayResult", "Accepted.Penalty", "PenalizedTeam", "PenaltyType", 
         "PenalizedPlayer", "Penalty.Yards", "PosTeamScore", "DefTeamScore", 
         "ScoreDiff", "AbsScoreDiff", "HomeTeam", "AwayTeam",
         "Timeout_Indicator","Timeout_Team","posteam_timeouts_pre",
         "HomeTimeouts_Remaining_Pre","AwayTimeouts_Remaining_Pre",
         "HomeTimeouts_Remaining_Post","AwayTimeouts_Remaining_Post",
         "No_Score_Prob","Opp_Field_Goal_Prob","Opp_Safety_Prob",
         "Opp_Touchdown_Prob","Field_Goal_Prob","Safety_Prob",
         "Touchdown_Prob","ExPoint_Prob","TwoPoint_Prob",
         "ExpPts","EPA","airEPA","yacEPA","Home_WP_pre",  "Away_WP_pre", "Home_WP_post", 
         "Away_WP_post","Win_Prob","WPA","airWPA","yacWPA")]
}

################################################################## 
#' Parsed Descriptive Play-by-Play Function for a Full Season
#' @description This function outputs all plays of an entire season in one dataframe.  
#' It calls the game_play_by_play function and applies it over every 
#' game in the season by extracting each game ID and url in the specified season.
#' WARNING: This function is deprecated and will be removed from the package
#' after the 2018-19 season.
#' 
#' @param Season (numeric) A 4-digit year corresponding to an NFL season of 
#' interest
#' @details This function calls the extracting_gameids, 
#' proper_jsonurl_formatting, and game_play_by_play to aggregate all the plays 
#' from a given season.  This dataframe is prime for use with the dplyr and 
#' plyr packages.
#' @return A dataframe contains all the play-by-play information for a single
#'      season.  This includes all the 99 variables collected in our 
#'      game_play_by_play function (see documentation for game_play_by_play for
#'      details) and a column for the Season.
#' @examples
#' # Play-by-play data from all games in 2010
#' pbp.data.2010 <- season_play_by_play(2010)
#' 
#' # Looking at all Pittsburgh Steelers offensive plays 
#' subset(pbp.data.2010, posteam = "PIT")
#' @export
season_play_by_play <- function(Season) {
  # Google R style format

  # Below the function put together the proper URLs for each game in each 
  # season and runs the game_play_by_play function across the entire season
  game_ids <- extracting_gameids(Season)
  
  # Create a boolean vector showing which of the game_ids has URLs
  # that exist - so the function can run during games:
  game_ids_exist <- sapply(game_ids, function(x) RCurl::url.exists(proper_jsonurl_formatting(x)))
  
  # Check to see that the url is not empty
  
  # Only use the game_ids that exist:
  game_ids <- game_ids[game_ids_exist]
  
  # Apply the play by play function to all of the game_ids
  # Also added catch if the play by play feed is throws an error
  # for games that are intiialized early.
  
  pbp_data_unformatted <- lapply(game_ids, 
                                 FUN = function (x) {
                                        tryCatch(game_play_by_play(x),
                                                 error = function(e) {})}
                                 )
  
  df_pbp_data <- dplyr::bind_rows(pbp_data_unformatted) %>%
                  dplyr::mutate(Season = Season)
  
  # Output #
  df_pbp_data
}

################################################################## 

# Drive Summary Function
#' Drive Summary and Results 
#' @description This function outputs the results dataframe of each drive of a 
#' given game
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @details The resulting dataframe has 16 variables associated with a specific 
#' aspect of a drive including the scoring result, number of plays, the duration 
#' of the drive, and the offensive and defensive teams.  All 16 variables are
#' explained in more detail below:
#' \itemize{
#'  \item{"GameID"} - The ID of the given Game
#'  \item{DriveNumber"} - The respective drive number in the game
#'  \item{"posteam"} - The offensive team on the drive
#'  \item{"qtr"} - The quarter at the end of the drive
#'  \item{"fds"} - Number of first downs in the drive
#'  \item{"result"} - End result of the drive
#'  \item{"penyds"} - Net penalty yards of the drive for the offensive team
#'  \item{"ydsgained"} - Number of yards gained on the drive
#'  \item{"numplays"} - Number of plays on the drive
#'  \item{"postime"} - The duration of the drive
#'  \item{"StartQrt"} - The quarter at the beginning of the drive
#'  \item{"StartTime} - The time left in the quarter at the start of the drive
#'  \item{"StartYardln"} - Yardline at the start of the drive
#'  \item{"StartTeam"} - The offensive team on the drive 
#'  \item{"EndQrt"} - The quarter at the end of the drive
#'  \item{"EndTime} - The time left in the quarter at the end of the drive
#'  \item{"EndYardln"} - Yardline at the end of the drive
#'  \item{"EndTeam"} - The offensive team on the drive
#'  }
#' @return A dataframe that has the summary statistics for each drive
#'      final output includes first downs, drive result, penalty yards, 
#'      of plays, time of possession, quarter at the start of the drive, 
#'      time at start of drive, yardline at start of drive, 
#'      team with possession at start, end of drive quarter, end of drive time, 
#'      end of drive yard line, end of drive team with possession
#' @examples
#' # Parsed drive Summarize of final game in 2015 NFL Season
#' nfl2015.finalregseasongame.gameID <- "2016010310"
#' drive_summary(nfl2015.finalregseasongame.gameID) 
#' @export
drive_summary <- function(GameID) {
  # Google R stlye format
  ######################
  ######################
  
  # Generating Game URL
  urlstring <- proper_jsonurl_formatting(GameID)
  
  # Converting JSON data
  nfl.json.data <- RJSONIO::fromJSON(RCurl::getURL(urlstring))
  
  # Creating Dataframe of Drive Outcomes:
  
  # Create a warning message for the Patriots vs. TB game
  # that has an empty drive 3 list (GameID is 2013092206),
  # and delete the empty drive. For all other games just
  # rbind together.
  
  if (GameID == "2013092206"){
    warning("Drive 3 is missing from game data.")
    drive.data.fix <- nfl.json.data[[1]]$drives
    drive.data.fix[[3]] <- NULL
    drive.data <- data.frame(do.call(rbind,drive.data.fix))
  } else {
    drive.data <- data.frame(do.call(rbind, (nfl.json.data[[1]]$drives)))
  }
  
  # Gathering Start of Drive Time, Location, and Quarter Info
  start.data <- data.frame(do.call(rbind, (drive.data$start))) 
  colnames(start.data) <- c("StartQrt", "StartTime", "StartYardln", "StartTeam")
  
  # Gathering End of Drive Time, Location, and Quarter Info
  end.data <- data.frame(do.call(rbind, (drive.data$end)))
  colnames(end.data) <- c("EndQrt", "EndTime", "EndYardln", "EndTeam")
  
  start.index <- which(colnames(drive.data) == "start")
  end.index <- which(colnames(drive.data) == "end")
  
  # Combining all datasets into one
  drive.data.final <- cbind(drive.data[, -c(start.index,end.index)], 
                            start.data, end.data)
  drive.data.final$GameID <- GameID
  
  # Create DriveNumber column but create a condition for 
  # the missing drive 3 game:
  if (GameID == "2013092206"){
    drive.data.final$DriveNumber <- c(1,2,4:(nrow(drive.data.final)+1))
  } else{
    drive.data.final$DriveNumber <- 1:nrow(drive.data.final)
  }
  # Removing last row and 4th column of irrelevant information
  drive.data.final <- drive.data.final[-nrow(drive.data),-c(3,4)]
  
  # Unlisting Columns 
  
  drive.data.final <-  drive.data.final %>%
    dplyr::mutate_all(unlist)
  
  # Output
  drive.data.final[,c("GameID", "DriveNumber", "posteam", "qtr", "fds",
                   "result", "penyds", "ydsgained", "numplays", "postime",
                   "StartQrt", "StartTime", "StartYardln", "StartTeam",
                   "EndQrt", "EndTime", "EndYardln", "EndTeam")]
}


################################################################## 
# Simple Box Score 
#' Simple Game Boxscore
#' @description This function pulls data from an NFL url and contructs it into a formatted 
#' boxscore.
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @param home (boolean): home = TRUE will pull home stats, 
#'                  home = FALSE pulls away stats
#' @return A list of player statistics including passing, rushing, receiving, 
#' defense, kicking, kick return, and punt return statistics for the specified 
#' game.
#' @examples
#' # Parsed drive summaries of final game in 2015 NFL season
#' nfl2015.finalregseasongame.gameID <- "2016010310"
#' simple_boxscore(nfl2015.finalregseasongame.gameID, home = TRUE) 
#' @export
simple_boxscore <- function(GameID, home = TRUE) {
  
  # Google R stlye format
  ##################
  ##################
  
  # Generating Game URL
  urlstring <- proper_jsonurl_formatting(GameID)

  # Start of Function
  nfl.json.data <- RJSONIO::fromJSON(RCurl::getURL(urlstring))
  
  # Date of Game   
  datestep1 <- stringr::str_extract(urlstring, pattern = "/[0-9]{10}/")
  datestep2 <- stringr::str_extract(datestep1, pattern = "[0-9]{8}")
  year <- substr(datestep2, start = 1, stop = 4)
  month <- substr(datestep2, start = 5, stop = 6)
  day <- substr(datestep2, start = nchar(datestep2)-1, stop = nchar(datestep2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  #   Parsing Data
  if (home == TRUE) {
    home.team.name <- nfl.json.data[[1]]$home$abbr
    # Passing Stats
    qb.stats <- data.frame(stat = "passing", date, GameID, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$passing, c)))
    qb.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$passing, 
                                          c)))
    # Running Stats
    rb.stats <- data.frame(stat = "rush", date, GameID, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$rushing, c)))
    rb.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$rushing, 
                                          c)))
    # Receiving Stats
    wr.stats <- data.frame(stat = "receiving", date, GameID, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$receiving, c)))
    wr.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$receiving, 
                                          c)))
    # Defensive Stats
    def.stats <- data.frame(stat = "defense", date, GameID, home.team.name, 
                           t(sapply(nfl.json.data[[1]]$home$stats$defense, c)))
    def.stats$playerID <- rownames(
                                  t(sapply(nfl.json.data[[1]]$home$stats$defense 
                                           , c)))
    # Kicking Stats
    kicker.stats <- data.frame(stat = "kicking", date, GameID, home.team.name, 
                              t(sapply(nfl.json.data[[1]]$home$stats$kicking, 
                                       c)))
    kicker.stats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$kicking, 
             c)))
    # Fumble Stats
    fumb.stats <- data.frame(stat = "fumbles", date, GameID, home.team.name, 
                            t(sapply(nfl.json.data[[1]]$home$stats$fumbles, c)))
    fumb.stats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$fumbles, 
             c)))
    # Kick Return Stats
    kr.stats <- data.frame(stat = "kickreturn", date, GameID, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$kickret, c)))
    kr.stats$playerID <- rownames(t(
                                  sapply(nfl.json.data[[1]]$home$stats$kickret, 
             c)))
    # Punt Return Stats
    pr.stats <- data.frame(stat = "puntreturn", date, GameID, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$puntret, c)))
    pr.stats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$puntret, 
             c)))
    # List of Stats
    home.team.stats <- list(HomePassing = qb.stats, 
                            HomeRushing = rb.stats, 
                            HomeReceiving = wr.stats, 
                            HomeDef = def.stats, 
                            HomeKicking = kicker.stats, 
                            HomeFumbles = fumb.stats, HomeKR = kr.stats, 
                            HomePR = pr.stats)
    home.team.stats
  } else {
    
    away.team.name <- nfl.json.data[[1]]$away$abbr
    
    # Passing Away Stats
    qb.away.stats <- data.frame(stat = "passing", GameID, away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$passing, 
                                       c)))
    qb.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$passing, 
                                              c)))
    # Running Away Stats
    rb.away.stats <- data.frame(stat = "rushing", date, GameID, away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$rushing, 
                                       c)))
    rb.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$rushing, 
                                              c)))
    # Receiving Away Stats
    wr.away.stats <- data.frame(stat = "receiving", date, GameID, away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$receiving, 
                                       c)))
    wr.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$receiving, 
                                              c)))
    # Defensive Away Stats
    def.away.stats <- data.frame(stat = "defense", date, GameID, away.team.name, 
                               t(sapply(nfl.json.data[[1]]$away$stats$defense, 
                                        c)))
    def.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$defense, 
                                               c)))
    # Kicking Away Stats
    kicker.away.stats <- data.frame(stat = "kicking", date, GameID, away.team.name, 
                                  t(sapply(nfl.json.data[[1]]$away$stats$kicking
                                           , c)))
    kicker.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$kicking, 
                                                  c)))
    # Fumble Away Stats
    fumb.away.stats <- data.frame(stat = "fumbles", date, GameID, away.team.name, 
                                t(sapply(nfl.json.data[[1]]$away$stats$fumbles, 
                                         c)))
    fumb.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$fumbles, 
                                                c)))
    # Kick Return Away Stats
    kr.away.stats <- data.frame(stat = "kickreturn", date, GameID, away.team.name,
                              t(sapply(nfl.json.data[[1]]$away$stats$kickret, 
                                       c)))
    kr.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$kickret, 
                                              c)))
    # Punt Return Away Stats
    pr.away.stats <- data.frame(stat = "puntreturn", date, GameID, 
                                away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$puntret, 
                                       c)))
    pr.away.stats$playerID <- rownames(t(sapply(
                                          nfl.json.data[[1]]$away$stats$puntret, 
                                              c)))
    # List of Away Stats
    awayTeamStats <- list(AwayPassing = qb.away.stats, 
                          AwayRushing = rb.away.stats, 
                          AwayReceiving = wr.away.stats, 
                          AwayDef = def.away.stats, 
                          AwayKicking = kicker.away.stats, 
                          AwayFumb = fumb.away.stats, 
                          AwayKR =  kr.away.stats, 
                          AwayPR = pr.away.stats)
    awayTeamStats
  }
}