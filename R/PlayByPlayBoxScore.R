################################################################## 
### Play-by-play, Drive Summary, and Simple Box Score Function ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
################################################################## 

# Play-by Play Function
#' Parsed Descriptive Play-by-Play Dataset for a Single Game
#' @description This function intakes the JSON play-by-play data of a single
#'  game and parses the play description column into individual variables 
#'  allowing the user to segment the game in a variety of different ways for 
#'  model building and analysis.
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @details Through list manipulation using the do.call and rbind functions
#'  a 10 column dataframe with basic information populates directly from the NFL 
#'  JSON API.  These columns include the following:
#' \itemize{
#'  \item{"Drive"} - Drive number
#'  \item{"sp"} - Whether the play resulted in a score (any kind of score)
#'  \item{"qrt"} - Quarter of Game
#'  \item{"down"} - Down of the given play
#'  \item{"time"} - Time at start of play
#'  \item{"yrdln"} - Between 0 and 50
#'  \item{"ydstogo"} - For a first down
#'  \item{"ydsnet"} - Total yards gained on a given drive
#'  \item{"posteam"} - The offensive team
#'  \item{"desc"} - A detailed description of what occured during the play
#' }
#' 
#' Through string manipulation and parsing of the description column using the 
#' base R and stringR, 51 columns were added to the original dataframe allowing 
#' the user to have a detailed breakdown of the events of each play.  
#' The added variables are specified below:
#' \itemize{
#'  \item{"Date"} - Date of game
#'  \item{"GameID"} - The ID of the specified game
#'  \item{"TimeSecs"} - Time remaining in game in seconds
#'  \item{"PlayTimeDiff"} - The time difference between plays in seconds
#'  \item{"DefensiveTeam"} - The defensive team on the play (for punts the 
#'  receiving team is on defense, for kickoffs the receiving team is on offense)
#'  \item{"TimeUnder"} - 
#'  \item{"SideofField"} - The side of the field that the line of scrimmage 
#'  is on
#'  \item{yrdline100} - Distance to opponents enzone, ranges from 1-99.
#'  situation
#'  \item{GoalToGo} - Binary variable indicting if the play is in a goal-to-go
#'  situation
#'  \item{"FirstDown"} - Binary: 0 if the play did not result in a first down 
#'  and 1 if it did
#'  \item{"PlayAttempted"} - A variabled used to count the number of plays in a 
#'  game (should always be equal to 1)
#'  \item{"Yards.Gained"} - Amount of yards gained on the play
#'  \item{"Touchdown"} - Binary: 1 if the play resulted in a TD else 0
#'  \item{"ExPointResult"} - Result of the extra-point: Made, Missed, Blocked
#'  \item{"TwoPointConv"} - Result of two-point conversion: Success of Failure
#'  \item{"DefTwoPoint"} - Result of defesnive two-point conversion: Success of Failure
#'  \item{"Safety"} - Binary: 1 if safety was recorded else 0
#'  \item{"Onsidekick"} - Binary: 1 if the Kickoff was an onside kick
#'  \item{"PuntResult} - The resulting action of a punt.  Either a clean punt or a blocked punt
#'  \item{"PlayType"} - The type of play that occured. Potential values are:
#'        \itemize{
#'                  \item{Kickoff, Punt, Onside Kick}
#'                  \item{Passs, Run}
#'                  \item{Sack}
#'                  \item{Field Goal, Extra Point}
#'                  \item{Quarter End, Two Minute Warning, End of Game}
#'                  \item{No Play, QB Kneel, Spike, Timeout}
#'          }  
#'  \item{"Passer"} - The passer on the play if it was a pass play
#'  \item{"PassAttempt"} - Binary variable indicating whether a pass was attempted
#'  or not
#'  \item{"PassOutcome"} - Pass Result: Complete or Incomplete    
#'  \item{"PassLength"} - Categorical variable indicating the length of the pass:
#'  Short or Deep
#'  \item{"PassLocation"} - Categorical variable: left, middle, right
#'  \item{"InterceptionThrown"} - Binary variable indicating whether an 
#'  interception was thrown
#'  \item{"Interceptor"} - The player who intercepted the ball
#'  \item{"Rusher"} - The runner on the play if it was a running play
#'  \item{"RushAttempt"} - Binary variable indicating whether or not a run was 
#'  attempted.
#'  \item{"RunLocation"} - The location of the run - left, middle, right
#'  \item{"RunGap"} - The gap that the running back ran through
#'  \item{"Receiver"} - The player who recorded the reception on a complete pass
#'  \item{"Reception"} - Binary Variable indicating a reception on a completed 
#'  pass: 1 if a reception was recorded else 0
#'  \item{"ReturnResult"} - Result of a punt, kickoff, interception, or 
#'  fumble return
#'  \item{"Returner"} - The punt or kickoff returner
#'  \item{"BlockingPlayer"} - The player who blocked the extra point, 
#'  field goal, or punt
#'  \item{"Tackler1"} - The primary tackler on the play
#'  \item{"Tackler2"} - The secondary tackler on the play
#'  \item{"FieldGoalResult"} - Outcome of a fieldgoal: made, missed, blocked
#'  \item{"FieldGoalDistance"} - Field goal length in yards
#'  \item{"Fumble"} - Binary variable indicating whether a fumble occured or not:
#'  1 if a fumble occured else no 
#'  \item{"RecFumbTeam"} - Team that recovered the fumble
#'  \item{"RecFumbPlayer"} - Player that recovered the fumble
#'  \item{"Sack"} - Binary variable indicating whether a sack was recorded: 1 if
#'  a sack was recorded else 0
#'  \item{"Challenge.Replay"} - Binary variable indicating whether or not the 
#'  play was reviewed by the replay offical on challenges or replay reviews
#'  \item{"ChalReplayResult"} - Result of the replay review: Upheld or Overturned
#'  \item{"Accepted.Penalty"} - Binary variable indicating whether a penalty was 
#'  accpeted on the play
#'  \item{"PenalizedTeam"} - The team who was penalized on the play
#'  \item{"PenaltyType"} - Type of penalty on the play. Values include:
#'        \itemize{    
#'                  \item{Unnecessary Roughness, Roughing the Passer}
#'                  \item{Illegal Formation, Defensive Offside}
#'                  \item{Delay of Game, False Start, Illegal Shift}
#'                  \item{Illegal Block Above the Waist, Personal Foul}
#'                  \item{Unnecessary Roughness, Illegal Blindside Bloc}
#'                  \item{Defensive Pass Interference, Offensive Pass Interference}
#'                  \item{Fair Catch Interferenc, Unsportsmanlike Conduct}
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
#'  \item{"expectedpoints"} - The expected point value of the play before 
#'  the snap      
#'  \item{"OffWinProb"} - The win probability of the offensive team
#'  \item{"DefWinProb"} - The win probability of the defensive team
#'  
#'  }
#'  
#' @return A dataframe with 68 columns specifying various statistics and 
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
  
  nfl.json <- RJSONIO::fromJSON(RCurl::getURL(urlstring))
  number.drives <- length(nfl.json[[1]]$drives) - 1
  PBP <- NULL
  for (ii in 1:number.drives) {
   # For now hard coded a fix for the drive in the Patriots vs. TB game
   # that has an empty drive 3 list
   if (GameID == 2013092206 & ii == 3) {next}
   PBP <- rbind(PBP, cbind("Drive" = ii,
                           data.frame(do.call(rbind, 
                                     (nfl.json[[1]]$drives[[ii]]$plays))
                                     )[,c(1:9)])
                )
  }
  
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
  "[A-Z]{2,3}-[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr)?")
  
  penalized.player2 <- stringr::str_extract(penalized.player.int, 
                                            pattern = 
             "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr)?")
  
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
      "pass ((incomplete )?[a-z]{4,5} [a-z]{4,6} )?to [A-Z]([a-z])?\\.[A-Z][A-z]{1,20}( (S|J)r\\.)?")
  
  PBP$Receiver <- stringr::str_extract(receiver.step1, 
                              pattern = "[A-Z]([a-z])?\\.[A-Z][A-z]{1,20}")
  
  ## Tacklers ##
  
  tacklers.step1 <- sapply(PBP$desc, stringr::str_extract, 
                           pattern = "(yard(s?)|no gain) \\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?(;|,)?( )?([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?)?\\)\\.")
  
  # Identifying the tacklers on the play (either one or two)
  tacklers1 <- stringr::str_extract(tacklers.step1,
                           pattern = 
                     "\\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")
  tacklers1 <- stringr::str_extract(tacklers1, 
                           pattern = 
                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")
  
  # Pulling out tacklers names
  tacklers2 <- stringr::str_extract(tacklers.step1, 
                           pattern = 
                    "(;|,)( )[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")
  tacklers2 <- stringr::str_extract(tacklers2,
                           pattern = 
                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")
  
  PBP$Tackler1 <- tacklers1
  PBP$Tackler2 <- tacklers2
  
  # Player who blocks punt or field goal or extra point
  
  player.blocking1 <- stringr::str_extract(PBP$desc, 
                       pattern = "BLOCKED by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
  player.blocking2 <- stringr::str_extract(player.blocking1, 
                                           pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
    
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
  PBP$ExPointResult[extrapoint.blocked] <- "Aborted"
  
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
         pattern = "(split by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?('o)?( Jr.)? and [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?\\))|(\\([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?, [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?\\))|\\([A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?\\)|Sack by [A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")

  # Sack Player 1
  
  sackplayer1 <- stringr::str_extract(sackers,
                                      pattern = 
                                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")

  # Sack Player 2
  sackplayer2 <- stringr::str_extract(sackers,
                                      pattern = 
                                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?\\)")
  
  sackplayer2 <- stringr::str_extract(sackplayer2,
                                      pattern = 
                                        "[A-z]{1,4}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?( Jr.)?")
  
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
                        pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  PBP[c(running.play, running.play2),"Rusher"] <- rusherStep1
  
  # Changing to correctly reflect the rusher 

  elidgiblePlays <- grep(PBP[which( (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 0) | (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 1 &PBP$posteam != PBP$PenalizedTeam)),"desc"],
                         pattern = " (report(ed|s)?)?( in )?as eligible(.){11,}")
  
  
  if (length(elidgiblePlays) > 0) {
    
  rusherStep2 <- sapply(PBP[which( (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 0) | (PBP$PlayType == "Run" & PBP$Accepted.Penalty == 1 &PBP$posteam != PBP$PenalizedTeam)),"desc"], 
         stringr::str_extract, 
         pattern = "(as eligible( receiver(s)?| for [A-Z]{2,3})?(\\.|,)?( ){1,2}(Direct snap to [A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?\\.( ){1,2})?[A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?)|(as eligible( receiver)?(\\.)?(.)+(\\.)? (\\(Shotgun\\) )?[A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?)|(\\(((Run formation)|([0-9]{1,2}:[0-9]{1,2}))\\) [A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)? (.){10,80}report(s|ed) as eligible(.){11,})")

  rusherStep3 <- sapply(rusherStep2, 
                        stringr::str_extract, 
                        pattern = "([A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?)$|\\(((Run formation)|([0-9]{1,2}:[0-9]{1,2})|(Shotgun))\\) [A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?") 
  
  rusherStep3.1 <- sapply(rusherStep3, 
                          stringr::str_extract, 
                          pattern = "([A-z]{1,3}\\.( )?[A-Z](')?[A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?)$") 
  
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
                           pattern = "by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?\\.$")
  
  punt.returner2 <- sapply(punt.returner1, stringr::str_extract,
                           pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
  # Touchdowns or Returns
  punt.returner3 <- sapply(PBP$desc[punt.play], stringr::str_extract, 
                           pattern = "(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)? to [A-Z]{2,3} [0-9]{1,2})|\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)? for [0-9]{1,2} yard(s)")
  
  punt.returner4 <- sapply(punt.returner3, stringr::str_extract, 
                           pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")

  # Kickoff Returner
  # Fair Catches
  kickret1 <- sapply(PBP$desc[kickoff], stringr::str_extract, 
                     pattern = "by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?\\.$")
  
  kickret2 <- sapply(kickret1, stringr::str_extract,
                     pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
  # Touchdowns or Returns
  kickret3 <- sapply(PBP$desc[kickoff], stringr::str_extract, 
                     pattern = "(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)? to [A-Z]{2,3} [0-9]{1,2})|(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)? for [0-9]{1,2} yard(s))|(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)? pushed)")
  
  kickret4 <- sapply(kickret3, stringr::str_extract, 
                     pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
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
                         pattern = "INTERCEPTED by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
  interceptor2 <- sapply(interceptor1, stringr::str_extract, 
                         pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
  PBP$Interceptor <- NA
  PBP$Interceptor[which(PBP$InterceptionThrown == 1)] <- interceptor2

  # Fumbler Recovery Team and Player
  
  recover.step1 <- sapply(PBP$desc[fumble.index], stringr::str_extract, 
                          pattern = "[A-Z]{2,3}-[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
  recover.team <- sapply(recover.step1, stringr::str_extract, 
                         pattern = "[A-Z]{2,3}")
  
  recover.player <- sapply(recover.step1, stringr::str_extract, 
                           pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-| )?[A-Z][a-z]{1,14})?('o)?")
  
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
  team.away.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == away.team.name
                        & !PBP$ReturnResult %in% "Touchdown"
                        & !PBP$PlayType %in% "Kickoff"
                        & PBP$sp == 1)] <- 6
  # Give points for Kickoff TDs
  team.away.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == away.team.name
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$PlayType %in% "Kickoff",
                        PBP$sp == 1)] <- 6
  # Give points for Punt Return TDs
  team.away.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$PlayType %in% "Punt"
                        & PBP$sp == 1)] <- 6
  # Give points for Interceptions
  team.away.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name
                        & PBP$ReturnResult %in% "Touchdown"
                        & !is.na(PBP$Interceptor)
                        & PBP$sp == 1)] <- 6
  # Make sure to give away team points for fumble ret for TD
  team.away.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name 
                        & PBP$ReturnResult %in% "Touchdown"
                        & !PBP$PlayType %in% "Kickoff"
                        & PBP$RecFumbTeam == away.team.name
                        & PBP$sp == 1)] <- 6
  # Fumble and the team that fumbled recovers and scores a TD
  team.away.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == away.team.name 
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$RecFumbTeam == away.team.name
                        & PBP$sp == 1)] <- 6
  # Points for two point conversion
  team.away.score[which(PBP$TwoPointConv == "Success" 
                        & PBP$posteam == away.team.name
                        & PBP$sp == 1)] <- 2
  # Points for safeties
  team.away.score[which(PBP$Safety == 1 
                        & PBP$posteam == home.team.name
                        & PBP$sp == 1)] <- 2
  # Points for made extra point
  team.away.score[which(PBP$ExPointResult == "Made" 
                        & PBP$posteam == away.team.name
                        & PBP$sp == 1)] <- 1
  # Points for made field goal
  team.away.score[which(PBP$FieldGoalResult == "Good" 
                        & PBP$posteam == away.team.name
                        & PBP$sp == 1)] <- 3
  
  team.away.score <- cumsum(team.away.score)
  
  away.team.pos <- which(PBP$posteam == away.team.name)
  away.team.def <- which(PBP$DefensiveTeam == away.team.name)
  
  ## Home Team ##
  # Regular offensive passing, rushing, or kickoff TD
  team.home.score[PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name 
                        & !PBP$ReturnResult %in% "Touchdown"
                        & !PBP$PlayType %in% "Kickoff"
                        & PBP$sp == 1] <- 6
  # Give points for Kickoffs
  team.home.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$PlayType %in% "Kickoff"
                        & PBP$sp == 1)] <- 6
  # Give points for Punts 
  team.home.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == away.team.name
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$PlayType %in% "Punt"
                        & PBP$sp == 1)] <- 6
  # Give points for Interceptions
  team.home.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == away.team.name
                        & PBP$ReturnResult %in% "Touchdown"
                        & !is.na(PBP$Interceptor)
                        & PBP$sp == 1)] <- 6
  team.home.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == away.team.name 
                        & PBP$ReturnResult %in% "Touchdown"
                        & !PBP$PlayType %in% "Kickoff"
                        & PBP$RecFumbTeam == home.team.name
                        & PBP$sp == 1)] <- 6
  # Fumble and the team that fumbled recovered and scored a TD
  team.home.score[which(PBP$Touchdown == 1 
                        & PBP$posteam == home.team.name 
                        & PBP$ReturnResult %in% "Touchdown"
                        & PBP$RecFumbTeam == home.team.name
                        & PBP$sp == 1)] <- 6
  # Points for two point conversion
  team.home.score[which(PBP$TwoPointConv == "Success" 
                        & PBP$posteam == home.team.name)] <- 2
  # Points for safeties
  team.home.score[which(PBP$Safety == 1 
                        & PBP$posteam == away.team.name
                        & PBP$sp == 1)] <- 2
  # Points for made extra point
  team.home.score[which(PBP$ExPointResult == "Made" 
                        & PBP$posteam == home.team.name
                        & PBP$sp == 1)] <- 1
  # Points for made field goal
  team.home.score[which(PBP$FieldGoalResult == "Good" 
                        & PBP$posteam == home.team.name
                        & PBP$sp == 1)] <- 3
  
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
  
  ##############################################################################
  ### Allows All Finals Plays of Game to Have a Score Difference ###
  
  PBP$ScoreDiff <- ifelse(PBP$PlayType == "End of Game", dplyr::lag(PBP$ScoreDiff)
                          , PBP$ScoreDiff)
  
  ##############################################################################
  
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
  
  # Loading data models #
  if(!exists("multi.w.time.int")) {
    data(sysdata, package = "nflscrapR") 
  }
  
  ## Adding in Win Probability ##
  
  PBP <- win_probability(PBP)
  
  ## Adding in Expected Points ##
  
  PBP <- expected_points(PBP)
  
  #print(colnames(PBP))
  
  ## Final OutPut ##
  PBP[,c("Date", "GameID", "Drive", "qtr", "down", "time", "TimeUnder", 
         "TimeSecs", "PlayTimeDiff", "SideofField", "yrdln", "yrdline100",
         "ydstogo", "ydsnet", "GoalToGo", "FirstDown", 
         "posteam", "DefensiveTeam", "desc", "PlayAttempted", "Yards.Gained", 
         "sp", "Touchdown", "ExPointResult", "TwoPointConv", "DefTwoPoint", 
         "Safety", "Onsidekick", "PuntResult", "PlayType", "Passer", "PassAttempt", "PassOutcome", 
         "PassLength", "PassLocation", "InterceptionThrown", "Interceptor",
         "Rusher", "RushAttempt", "RunLocation", "RunGap",  "Receiver", 
         "Reception", "ReturnResult", "Returner", "BlockingPlayer","Tackler1", "Tackler2", 
         "FieldGoalResult", "FieldGoalDistance", 
         "Fumble", "RecFumbTeam", "RecFumbPlayer", "Sack", "Challenge.Replay",
         "ChalReplayResult", "Accepted.Penalty", "PenalizedTeam", "PenaltyType", 
         "PenalizedPlayer", "Penalty.Yards", "PosTeamScore", "DefTeamScore", 
         "ScoreDiff", "AbsScoreDiff", "expectedpoints",
         "OffWinProb", "DefWinProb")]
}

################################################################## 
#' Parsed Descriptive Play-by-Play Function for a Full Season
#' @description This function outputs all plays of an entire season in one dataframe.  
#' It calls the game_play_by_play function and applies it over every 
#' game in the season by extracting each game ID and url in the specified season.
#' 
#' @param Season (numeric) A 4-digit year corresponding to an NFL season of 
#' interest
#' @param Weeks (numeric) A number corresponding to the number of weeks of data
#' you want to be scraped and included in the output. If you input 3, the first
#' three weeks of play-by-play will be scraped from the associated season.
#'
#' @details This function calls the extracting_gameids, 
#' proper_jsonurl_formatting, and game_play_by_play to aggregate all the plays 
#' from a given season.  This dataframe is prime for use with the dplyr and 
#' plyr packages.
#' @return A dataframe contains all the play-by-play information for a single
#'      season.  This includes all the 62 variables collected in our 
#'      game_play_by_play function (see documentation for game_play_by_play for
#'      details)
#' @examples
#' # Play-by-Play Data from All games in 2010
#' pbp.data.2010 <- season_play_by_play(2010)
#' 
#' # Looking at all Baltimore Ravens Offensive Plays 
#' subset(pbp.data.2010, posteam = "BAL")
#' @export
season_play_by_play <- function(Season, Weeks = 16) {
  # Google R stlye format
  
  # Below the function put together the proper URLs for each game in each 
  # season and runs the game_play_by_play function across the entire season
  game_ids <- extracting_gameids(Season)
  
  if (Weeks %in% 3:15) {
    game_ids <- game_ids[1:(16*Weeks)-1]
  } else if (Weeks %in% 1:2) {
    game_ids <- game_ids[1:(16*Weeks)]
  }
  
  pbp_data_unformatted <- lapply(game_ids, FUN = game_play_by_play)
  
  df_pbp_data <- do.call(rbind, pbp_data_unformatted)
  df_pbp_data$Season <- Season
  df_pbp_data
}

################################################################## 

# Drive Summary Function
#' Drive Summary and Results 
#' @description This function outputs the results dataframe of each drive of a 
#' given game
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @details The outputted dataframe has 16 variables associated with a specific 
#' aspect of a drive including the scoring result, number of plays, the duration 
#' of the drive, and the offensive and defensive teams.  All 16 variable are
#' explained in more detail below:
#' \itemize{
#'  \item{"GameID"} - The ID of the given Game
#'  \item{DriveNumber"} - The respective drive number in the game
#'  \item{"posteam"} - The offensive team on the drive
#'  \item{"qrt"} - The quarter at the end of the drive
#'  \item{"fs"} - Number of first downs in the drive
#'  \item{"result"} - End result of the drive
#'  \item{"penyds"} - Net penalty yards of the drive for the offensive team
#'  \item{"ydsgained"} - Number of yards gained on the drive
#'  \item{"numplaus"} - Number of plays on the drive
#'  \item{"postime"} - The duration of the 
#'  \item{"Startqrt"} - The quarter at the beginning of the drive
#'  \item{"StartTime} - The time left in the quarter at the start of the drive
#'  \item{"StartYardln"} - Yardline at the start of the drive
#'  \item{"StartTeam"} - The offensive team on the drive 
#'  }
#' @return A dataframe that has the summary statistics for each drive
#'      final output includes first downs, drive result, penalty yards, 
#'      of plays, time of possession, quarter at the start of the drive, 
#'      Time at Start of Drive, yardline at start of drive, 
#'      team with possession at start, end of drive quarter, end of drive time, 
#'      end of drive Yard line, end of drive team with possession
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
  drive.data.final$GameID <- GameID
  drive.data.final$DriveNumber <- 1:nrow(drive.data.final)
  
  # Removing last row and 4th column of irrelevant information
  drive.data.final <- drive.data.final[-nrow(drive.data),-c(3,4)]
  
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
#' @return A list of playerstatistics including passing, rushing, receiving, 
#' defense, kicking, kick return, and punt return statistics for the specified 
#' game.
#' @examples
#' # Parsed drive Summarize of final game in 2015 NFL Season
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