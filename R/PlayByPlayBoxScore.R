################################################################## 
### Play-by-play, Drive Summary, and Simple Box Score Function ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
################################################################## 

# Play-by Play Function 

#' Parsed Descriptive Play-by-Play Function for a Single Game
#' @description This function intakes the JSON play-by-play data of a single
#'  game and parses the play description column individual variables allowing 
#'  the user to segment the game in a variety of different ways allowing for
#'  in-depth analysis.
#' @param URLString (character) A URL string with the location of a single 
#' NFL game JSON API
#' @details Through simple list manipulation using the dp.call function and 
#' rbind this function creates a 10 column dataframe with basic information 
#'  the NFL JSON API.  These columns include the following:
#' \itemize{
#'  \item{"Drive"}
#'  \item{"sp"} - Whether the play resulted in a score (any kind of score)
#'  \item{"qrt"}
#'  \item{"down"}
#'  \item{"time"} - Time at start of play
#'  \item{"yrdln"} - Between 0 and 50
#'  \item{"ydstogo"} - For a first down
#'  \item{"ydsnet"}
#'  \item{"posteam"} - The offensive team
#'  \item{"desc"} - A detailed description of what occured during the play
#' }
#' 
#' Through string manipulation and parsing of the desc column, 40 columns were 
#' added to the original dataframe allowing the user to have a detailed 
#' understanding of the events of each play.  The added variables are specified 
#' below:
#' \itemize{
#'  \item{"Date", "GameID"}
#'  \item{"DefensiveTeam", "SideofField"}
#'  \item{"GoalToGo", "FirstDown", "PlayAttempted"} 
#'  \item{"Yards.Gained", "TimeUnder"}
#'  \item{"ExPointResult", "TwoPointConv", "FieldGoalResult", 
#'        "FieldGoalDistance", "TouchDown}
#'  \item{"PlayType"}
#'  \item{"Passer", "PassAttempt", "PassOutcome", "PassLength", "PassLocation",
#'        "InterceptionThrown"}
#'  \item{"Rusher", "RushAttempt", "RunLocation", "RunGap"}
#'  \item{"Receiver", "Reception"}
#'  \item{"Tackler1", "Tackler2", "Fumble", "Sack"}
#'  \item{"Accepted.Penalty", "PenalizedTeam", "Penalty Type", 
#'        "Penalized Player", "Penalty.Yards"}
#'  \item{"PosTeamScore", "DefTeamScore", "ScoreDiff", "AbsScoreDiff"}
#'  }
#'  
#' @return A dataframe with 50 columns specifying various statistics associated 
#' with each play of the specified NFL game.
#' @examples
#' # Parsed play-by-play of the final game in the 2015 NFL season 
#' nfl2015.finalregseasongame <- "http://www.nfl.com/liveupdate/game-center/2016010310/2016010310_gtd.json"
#' Game_PBP(nfl2015.finalregseasongame) 
game_play_by_play <- function(URLString) {
  # Google R stlye format
  
  #########################
  #########################
  
  # Converting JSON data
  nfl.json <- RJSONIO::fromJSON(RCurl::getURL(URLString))
  number.drives <- length(nfl.json[[1]]$drives) - 1
  PBP <- NULL
  for (ii in 1:number.drives) {
   PBP <- rbind(PBP, cbind("Drive" = ii,
                           data.frame(do.call(rbind, 
                                     (nfl.json[[1]]$drives[[ii]]$plays))
                                     )[,c(1:9)])
                )
  }
  
  # Fixing Possession team for Kick-Offs
  kickoff.index <- which(sapply(PBP$desc, regexpr, 
                                pattern = 
                                  "kicks") != -1)
  pos.teams <- unlist(unique(PBP$posteam))[1:2]
  correct.kickoff.pos <- ifelse(PBP$posteam[kickoff.index] == pos.teams[1], 
                                pos.teams[2], pos.teams[1])
  PBP[kickoff.index, "posteam"] <- correct.kickoff.pos
  
  # Yard Line Information
  yline.info <- sapply(PBP$yrdln, strsplit, split = " ")
  
  PBP$SideofField <- sapply(yline.info, FUN = function(x) x[1])
  PBP$yrdln <- sapply(yline.info, FUN = function(x) x[2])
  
  # Game Date  
  date.step1 <- stringr::str_extract(URLString, pattern = "/[0-9]{10}/")
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
  
  PBP$TimeSecs <- qtr.timeinsecs
  
  # Time Difference (in seconds)
  plays.time.diff <- abs(c(0, diff(qtr.timeinsecs)))
  
  PBP$PlayTimeDiff <- plays.time.diff
  
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
  
  # Touchdown Play 
  touchdown.step1 <- sapply(PBP$desc, stringr::str_extract, 
                            pattern = "TOUCHDOWN")
  nullified <- grep(PBP$desc, pattern = "TOUCHDOWN NULLIFIED")
  touchdown.step1[nullified] <- NA
  PBP$Touchdown <- ifelse(!is.na(touchdown.step1), 1, 0)
  
  # Two Point Conversion 
  PBP$TwoPointConv <- NA
  
  two.point.success <- which(sapply(PBP$desc, regexpr, 
                                    pattern = 
                                      "TWO-POINT CONVERSION ATTEMPT\\. (.){1,70}\\. ATTEMPT SUCCEEDS") != -1)
  two.point.failure <- which(sapply(PBP$desc, regexpr, 
                                    pattern = 
                                      "TWO-POINT CONVERSION ATTEMPT\\. (.){1,70}\\. ATTEMPT FAILS") != -1)
  
  PBP$TwoPointConv[two.point.success] <- "Success"
  PBP$TwoPointConv[two.point.failure] <- "Failure"
  
  # Penalty - Binary Column 
  PBP$Accepted.Penalty <- NA
  penalty.play <- sapply(PBP$desc, stringr::str_extract, pattern = "PENALTY")
  PBP$Accepted.Penalty <- ifelse(!is.na(penalty.play), 1, 0)
  
  # Penalized Team
  penalized.team.s1 <- sapply(PBP$desc, stringr::str_extract,  
                                            "PENALTY on [A-Z]{2,3}")
  PBP$PenalizedTeam <- stringr::str_extract(penalized.team.s1,  
                                            "[A-Z]{2,3}$")
  
  # Penalty - What was the penalty?
  penalty.type.s1 <- sapply(PBP$desc, stringr::str_extract,  
                                          pattern ="PENALTY(.){5,25},.+, [0-9]")
  penalty.type.s2 <- stringr::str_extract(pattern = ",.+,", penalty.type.s1)
  penalty.type.final <- stringr::str_sub(penalty.type.s2, 3, -2)
  
  PBP$PenaltyType <- penalty.type.final
  
  #   Penalized Player 
  PBP$PenalizedPlayer <- NA
  penalized.player.int <- sapply(PBP$desc[ which(PBP$Accepted.Penalty == 1) ], 
                                 stringr::str_extract, 
                                 pattern = 
  "[A-Z]{2,3}-[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?( (S|J)r)?")
  
  penalized.player2 <- stringr::str_extract(penalized.player.int, 
                                            pattern = 
             "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?( (S|J)r)?")
  
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
                         pattern = "[A-Z]\\.[A-Z][A-z]{1,20} pass")
  PBP$Passer <- stringr::str_extract(passer.step1, 
                                     pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  
  ## Receiver ##
  
  receiver.step1 <- sapply(PBP$desc, stringr::str_extract, 
                           pattern = 
      "pass (incomplete)?( )?[a-z]{4,5} [a-z]{4,6} to [A-Z]\\.[A-Z][A-z]{1,20}")
  
  PBP$Receiver <- stringr::str_extract(receiver.step1, 
                              pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  
  ## Tacklers ##
  
  tacklers.step1 <- sapply(PBP$desc, stringr::str_extract, 
                           pattern = "(yard(s?)|no gain) \\([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?(;)?( )?([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?)?\\)\\.")
  
  # Identifying the tacklers on the play (either one or two)
  tacklers1 <- stringr::str_extract(tacklers.step1,
                           pattern = 
                     "\\([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  tacklers1 <- stringr::str_extract(tacklers1, 
                           pattern = 
                        "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  
  # Pulling out tacklers names
  tacklers2 <- stringr::str_extract(tacklers.step1, 
                           pattern = 
                    ";( )[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  tacklers2 <- stringr::str_extract(tacklers2,
                           pattern = 
                        "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  
  PBP$Tackler1 <- tacklers1
  PBP$Tackler2 <- tacklers2
  
  # Pass Plays
  PBP$PassOutcome <- NA
  pass.play <- which(sapply( PBP$desc, regexpr, pattern = "pass") != -1)
  incomplete.pass.play <- which(sapply(PBP$desc, regexpr, 
                                       pattern = 
                                         "(pass incomplete)|INTERCEPTED") != -1)
  
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
  punt.play <- which(sapply(PBP$desc, regexpr, pattern = "punts") != -1)
  
  PBP$PlayType[punt.play] <- "Punt"
  
  # Field Goal
  fieldgoal <- which(sapply(PBP$desc, regexpr, 
                            pattern = "field goal") != -1)
  
  missed.fg <- which(sapply(PBP$desc, regexpr, 
                            pattern = "field goal is No Good") != -1)
  
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
  PBP$FieldGoalResult[setdiff(fieldgoal,missed.fg)] <- "Good"
  
  # Extra Point
  extrapoint.good <- which(sapply(PBP$desc, regexpr,
                                    pattern = "extra point is GOOD") != -1)
  
  extrapoint.nogood <- which(sapply(PBP$desc, regexpr, 
         pattern = "(extra point is No Good) | (extra point is Blocked)") != -1)
  
  PBP$PlayType[c(extrapoint.good,extrapoint.nogood)] <- "Extra Point"
  
  # Extra Point Result
  PBP$ExPointResult <- NA
  PBP$ExPointResult[extrapoint.good] <- "Good"
  PBP$ExPointResult[extrapoint.nogood] <- "No Good"
  
  # Fumbles
  
  PBP$Fumble <- NA
  fumble.index <- which(sapply(PBP$desc, regexpr, pattern = "FUMBLE") != -1) 
  PBP$Fumble[fumble.index] <- "Fumble"
  
  # Timeouts
  timeouts <- which(sapply(PBP$desc, regexpr, 
                           pattern = "[A-z]imeout #[1-5] by") != -1)
  
  PBP$PlayType[timeouts] <- "Timeout"
  
  # Quarter End
  end.quarter <- which(sapply(PBP$desc, regexpr, 
                              pattern = "END QUARTER") != -1)
  
  PBP$PlayType[end.quarter] <- "Quarter End"
  
  # 2 Minute Warning
  two.minute.warning <- which(sapply(PBP$desc, regexpr, 
                                     pattern = "Two-Minute Warning") != -1)
  
  PBP$PlayType[two.minute.warning] <- "Two Minute Warning"
  
  # Sack 
  Sack.Plays <- which(sapply(PBP$desc, regexpr, pattern = "sacked") != -1)
  
  PBP$PlayType[Sack.Plays] <- "Sack"
  
  # Sack- Binary
  PBP$Sack <- 0
  PBP$Sack[Sack.Plays] <- 1
  
  # QB Kneel
  qb.kneel <- which(sapply(PBP$desc, regexpr, pattern = "kneels") != -1)
  
  PBP$PlayType[qb.kneel] <- "QB Kneel"
  
  # Kick Off
  kickoff <- which(sapply(PBP$desc, regexpr, pattern = "kick(s)?") != -1)
  
  PBP$PlayType[kickoff] <- "Kickoff"
  
  # Onside Kick
  
  onside <- which(sapply(PBP$desc, regexpr, pattern = "onside") != -1)
  
  PBP$PlayType[onside] <- "Onside Kick"
  
  # Spike
  spike.play <- which(sapply(PBP$desc, regexpr, pattern = "spiked") != -1)
  
  PBP$PlayType[spike.play] <- "Spike"
  
  # No Play
  no.play <- which(sapply(PBP$desc, regexpr, 
                                pattern = "No Play") != -1)
  
  PBP$PlayType[no.play] <- "No Play"
  
  # End of Game 
  end.game <- which(sapply(PBP$desc, regexpr, pattern = "END GAME") != -1)
  
  PBP$PlayType[end.game] <- "End of Game"
  
  # First Down 
  PBP$FirstDown <- NA
  
  first.downplays <- which(PBP$down == 1)
  first.downs <- first.downplays-1
  PBP$FirstDown[first.downs] <- ifelse(PBP$down[first.downs] ==0, NA, 1)
  
  # Running Play
  running.play <- which(is.na(PBP$PlayType))
  PBP$PlayType[running.play] <- "Run"
  PBP$RushAttempt <- ifelse(PBP$PlayType == "Run", 1,0)
  
  # Run Direction
  PBP$RunLocation <- NA
  
  run.left <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                           pattern = "left") != -1)
  run.right <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                            pattern = "right") != -1)   
  run.middle <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                             pattern = "middle") != -1) 
  
  PBP[running.play,"RunLocation"][run.left] <- "left"
  PBP[running.play,"RunLocation"][run.right] <- "right"
  PBP[running.play,"RunLocation"][run.middle] <- "middle"
  
  # Run Gap
  PBP$RunGap <- NA
  
  run.guard <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                           pattern = "guard") != -1)
  
  run.tackle <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                            pattern = "tackle") != -1)   
  
  run.end <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                         pattern = "end") != -1) 
  
  PBP[running.play,"RunGap"][run.guard] <- "guard"
  PBP[running.play,"RunGap"][run.tackle] <- "tackle"
  PBP[running.play,"RunGap"][run.end] <- "end"
  
  # Rusher
  
  rusherStep1 <- sapply(PBP[which(PBP$PlayType == "Run"),"desc"], 
                        stringr::str_extract, 
                        pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  PBP[running.play,"Rusher"] <- rusherStep1
  
  
  # The next few variables are counting variables
  # Used to help set up model for predictions 
  
  # Plays 
  PBP$PlayAttempted <- 1
  
  # Time Under
  PBP$TimeUnder <- substr(lubridate::ceiling_date(as.POSIXct(paste("00:", 
                                                                   PBP$time, 
                                                                   sep = ""), 
                                                             format = "%H:%M:%S"
                                                             ), "minute"), 
                          15, 16)
  
  PBP$TimeUnder <- as.numeric(as.character(PBP$TimeUnder))
  
  # Calculating Score of Game for Possesion team and Defensive Team
  
  team.home.score <- rep(0, times = nrow(PBP))
  team.away.score <- rep(0, times = nrow(PBP))
   
  away.team.name <- nfl.json[[1]]$away$abbr
  home.team.name <- nfl.json[[1]]$home$abbr
  
  # Away Team
  
  team.away.score[which(PBP$Touchdown == 1 
                      & PBP$posteam == away.team.name)] <- 6
  team.away.score[which(PBP$TwoPointConv == "Success" 
                      & PBP$posteam == away.team.name)] <- 2
  team.away.score[which(PBP$ExPointResult == "Good" 
                      & PBP$posteam == away.team.name)] <- 1
  team.away.score[which(PBP$FieldGoalResult == "Good" 
                      & PBP$posteam == away.team.name)] <- 3
  
  team.away.score <- cumsum(team.away.score)
  
  away.team.pos <- which(PBP$posteam == away.team.name)
  away.team.def <- which(PBP$DefensiveTeam == away.team.name)
  
  # Home Team
  team.home.score[which(PBP$Touchdown == 1 & PBP$posteam == home.team.name)] <- 6
  team.home.score[which(PBP$TwoPointConv == "Success" 
                      & PBP$posteam == home.team.name)] <- 2
  team.home.score[which(PBP$ExPointResult == "Good" 
                      & PBP$posteam == home.team.name)] <- 1
  team.home.score[which(PBP$FieldGoalResult == "Good" 
                      & PBP$posteam == home.team.name)] <- 3
  
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
  
  # Score Differential and Abs Score Differential 
  
  PBP$ScoreDiff <- PBP$PosTeamScore - PBP$DefTeamScore
  PBP$AbsScoreDiff <- abs(PBP$PosTeamScore - PBP$DefTeamScore)
  
  # Goal to Go
  
  PBP$GoalToGo <- ifelse(PBP$posteam != PBP$SideofField & PBP$yrdln <= 10, 1, 0)
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
  
  ## Final OutPut ##
  PBP[,c("Date", "GameID", "Drive", "qtr", "down", "time", "TimeUnder", 
         "TimeSecs", "PlayTimeDiff", "SideofField", "yrdln", 
         "ydstogo", "ydsnet", "GoalToGo", "FirstDown", 
         "posteam", "DefensiveTeam", "desc", "PlayAttempted", "Yards.Gained", 
         "sp", "Touchdown", "ExPointResult", "TwoPointConv", "PlayType", 
         "Passer", "PassAttempt", "PassOutcome", "PassLength", "PassLocation",
         "InterceptionThrown", "Rusher", "RushAttempt", "RunLocation", "RunGap", 
         "Receiver", "Reception", "Tackler1", "Tackler2", "FieldGoalResult", 
         "FieldGoalDistance", "Fumble", "Sack", "Accepted.Penalty", 
         "PenalizedTeam", "PenaltyType", "PenalizedPlayer", "Penalty.Yards", 
         "PosTeamScore", "DefTeamScore", "ScoreDiff", "AbsScoreDiff")]
}

################################################################## 

#' Parsed Descriptive Play-by-Play Function for a Full Season
#' @description This function outputs all plays of an entire season in one dataframe.  
#' It calls upon the game_play_by_play and applies it over every 
#' game in the season.
#' @param Season (numeric) A 4-digit year corresponding to an NFL season of 
#' interest
#'
#' @details This function calls the extracting_gameids, 
#' proper_jsonurl_formatting, and game_play_by_play to aggregate all the plays 
#' from a given season.  This dataframe is prime for use with the dplyr and 
#' plyr packages.
#' @return A dataframe contains all the play-by-play information for a single
#'      season.  This includes all the 50 variables collected in our 
#'      game_play_by_play function (see documentation for game_play_by_play for
#'      details)
#' @examples
#' # Play-by-Play Data from All games in 2010
#' pbp.data.2010 <- season_PBP(2010)
#' # Looking at all Baltimore Ravens Offensive Plays 
#' subset(pbp.data.2010, posteam = "BAL")
season_play_by_play <- function(Season) {
  # Google R stlye format
  
  # Below the function put together the proper URLs for each game in each 
  # season and runs the game_play_by_play function across the entire season
  game_ids <- extracting_gameids(Season)
  game_urls <- sapply(game_ids, proper_jsonurl_formatting)
  pbp_data_unformatted <- lapply(game_urls, FUN = game_play_by_play)
  
  df_pbp_data <- do.call(rbind, pbp_data_unformatted)
  
  df_pbp_data
}

################################################################## 

# Drive Summary Function

#' Drive Summary and Results 
#' @description This function outputs the end result of each drive of a given
#'  game
#' @param URLString: A string with the URL location of a single NFL 
#' game JSON API
#' @return A dataframe that has the summary statistics for each drive
#'      final output includes first downs, drive result, penalty yards, 
#'      of plays, time of possession, quarter at the start of the drive, 
#'      Time at Start of Drive, yardline at start of drive, 
#'      team with possession at start, end of drive quarter, end of drive time, 
#'      end of drive Yard line, end of drive team with possession
#' @examples
#' # Parsed drive Summarize of final game in 2015 NFL Season
#' nfl2015.finalregseasongame <- "http://www.nfl.com/liveupdate/game-center/2016010310/2016010310_gtd.json"
#' drive_summary(nfl2015.finalregseasongame) 
drive_summary <- function(URLString) {
  # Google R stlye format
  ######################
  ######################
  
  # Converting JSON data
  nfl.json.data <- RJSONIO::fromJSON(RCurl::getURL(URLString))
  
  # Creating Dataframe of Drive Outcomes
  drive.data <- data.frame(do.call(rbind, (nfl.json.data[[1]]$drives)))
  
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
  
  # Removing last row and 4th column of irrelevant information
  drive.data.final[-nrow(drive.data), -4]
}


################################################################## 
# Simple Box Score 

#' Drive Summary and Results 
#' @description This function pulls data from an NFL url and contructs it into a formatted 
#' boxscore.
#' @param URLString (character) is a character string of the URL of where to 
#'                         pull the data from
#' @param home (boolean): home = TRUE will pull home stats, 
#'                  home = FALSE pulls away stats
#' @return A list of statistics including passing, rushing, receiving, defense,
#'  kicking, kick return, and punt return statistics for the specified game
#' @examples
#' # Parsed drive Summarize of final game in 2015 NFL Season
#' nfl2015.finalregseasongame <- "http://www.nfl.com/liveupdate/game-center/2016010310/2016010310_gtd.json"
#' simple_boxscore(nfl2015.finalregseasongame, home = TRUE) 

simple_boxscore <- function(URLString, 
                                   home = TRUE) {
  # Google R stlye format
  ##################
  ##################

  #   Start of Function
  
  nfl.json.data <- RJSONIO::fromJSON(RCurl::getURL(URLString))
  
  #   GameID
  game.id <- stringr::str_extract(URLString, pattern = "[0-9]{10}")
  
  # Date of Game   
  datestep1 <- stringr::str_extract(URLString, pattern = "/[0-9]{10}/")
  datestep2 <- stringr::str_extract(datestep1, pattern = "[0-9]{8}")
  year <- substr(datestep2, start = 1, stop = 4)
  month <- substr(datestep2, start = 5, stop = 6)
  day <- substr(datestep2, start = nchar(datestep2)-1, stop = nchar(datestep2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  #   Parsing Data
  if (home == TRUE) {
    home.team.name <- nfl.json.data[[1]]$home$abbr
    # Passing Stats
    qb.stats <- data.frame(stat = "passing", date, game.id, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$passing, c)))
    qb.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$passing, 
                                          c)))
    # Running Stats
    rb.stats <- data.frame(stat = "rush", date, game.id, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$rushing, c)))
    rb.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$rushing, 
                                          c)))
    # Receiving Stats
    wr.stats <- data.frame(stat = "receiving", date, game.id, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$receiving, c)))
    wr.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$receiving, 
                                          c)))
    # Defensive Stats
    def.stats <- data.frame(stat = "defense", date, game.id, home.team.name, 
                           t(sapply(nfl.json.data[[1]]$home$stats$defense, c)))
    def.stats$playerID <- rownames(
                                  t(sapply(nfl.json.data[[1]]$home$stats$defense 
                                           , c)))
    # Kicking Stats
    kicker.stats <- data.frame(stat = "kicking", date, game.id, home.team.name, 
                              t(sapply(nfl.json.data[[1]]$home$stats$kicking, 
                                       c)))
    kicker.stats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$kicking, 
             c)))
    # Fumble Stats
    fumb.stats <- data.frame(stat = "fumbles", date, game.id, home.team.name, 
                            t(sapply(nfl.json.data[[1]]$home$stats$fumbles, c)))
    fumb.stats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$fumbles, 
             c)))
    # Kick Return Stats
    kr.stats <- data.frame(stat = "kickreturn", date, game.id, home.team.name, 
                          t(sapply(nfl.json.data[[1]]$home$stats$kickret, c)))
    kr.stats$playerID <- rownames(t(
                                  sapply(nfl.json.data[[1]]$home$stats$kickret, 
             c)))
    # Punt Return Stats
    pr.stats <- data.frame(stat = "puntreturn", date, game.id, home.team.name, 
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
    qb.away.stats <- data.frame(stat = "passing", game.id, away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$passing, 
                                       c)))
    qb.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$passing, 
                                              c)))
    # Running Away Stats
    rb.away.stats <- data.frame(stat = "rushing", date, game.id, away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$rushing, 
                                       c)))
    rb.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$rushing, 
                                              c)))
    # Receiving Away Stats
    wr.away.stats <- data.frame(stat = "receiving", date, game.id, away.team.name, 
                              t(sapply(nfl.json.data[[1]]$away$stats$receiving, 
                                       c)))
    wr.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$receiving, 
                                              c)))
    # Defensive Away Stats
    def.away.stats <- data.frame(stat = "defense", date, game.id, away.team.name, 
                               t(sapply(nfl.json.data[[1]]$away$stats$defense, 
                                        c)))
    def.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$defense, 
                                               c)))
    # Kicking Away Stats
    kicker.away.stats <- data.frame(stat = "kicking", date, game.id, away.team.name, 
                                  t(sapply(nfl.json.data[[1]]$away$stats$kicking
                                           , c)))
    kicker.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$kicking, 
                                                  c)))
    # Fumble Away Stats
    fumb.away.stats <- data.frame(stat = "fumbles", date, game.id, away.team.name, 
                                t(sapply(nfl.json.data[[1]]$away$stats$fumbles, 
                                         c)))
    fumb.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$fumbles, 
                                                c)))
    # Kick Return Away Stats
    kr.away.stats <- data.frame(stat = "kickreturn", date, game.id, away.team.name,
                              t(sapply(nfl.json.data[[1]]$away$stats$kickret, 
                                       c)))
    kr.away.stats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$kickret, 
                                              c)))
    # Punt Return Away Stats
    pr.away.stats <- data.frame(stat = "puntreturn", date, game.id, 
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