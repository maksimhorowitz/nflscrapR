
# Issue play

issue.game <- game_play_by_play(2015091303)
game_play_by_play(2015091303)[which(game_play_by_play(2015091303)$PlayType == "Punt"), "desc"]

# Kick off/Punt Result

Punts <- which(sapply(issue.game$desc, regexpr, pattern = "punts") != -1)

punt.tds <- which(sapply(issue.game$desc[Punts], regexpr, 
                         pattern = "TOUCHDOWN") != -1)
punts.touchbacks <- which(sapply(issue.game$desc[Punts], regexpr, 
                                 pattern = "Touchback") != -1)
punts.faircatch <- which(sapply(issue.game$desc[Punts], regexpr, 
                                 pattern = "fair catch") != -1)

# Fair Catch
punt.returner1 <- sapply(issue.game$desc[Punts], stringr::str_extract, 
                         pattern = "by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?\\.$")

punt.returner2 <- sapply(punt.returner1, stringr::str_extract,
                  pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

# Actual Return or TD

punt.returner3 <- sapply(issue.game$desc[Punts], stringr::str_extract, 
       pattern = "(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})? to [A-Z]{2,3} [0-9]{1,2})|\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})? for [0-9]{1,2} yard(s)")

punt.returner4 <- sapply(punt.returner3, stringr::str_extract, 
                         pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

punt.returner3[which(!is.na(punt.returner3))]
punt.returner4[which(!is.na(punt.returner4))]

# Kickoff

issue.game[which(issue.game$PlayType == "Kickoff"), "desc"]

Kickoff <- which(sapply(issue.game$desc, regexpr, pattern = "kicks") != -1)

kick.tds <- which(sapply(issue.game$desc[Kickoff], regexpr, 
                         pattern = "TOUCHDOWN") != -1)
kick.TB <- which(sapply(issue.game$desc[Kickoff], regexpr, 
                                 pattern = "Touchback") != -1)
kick.faircatch <- which(sapply(issue.game$desc[Kickoff], regexpr, 
                                pattern = "fair catch") != -1)
kick.kneels <- which(sapply(issue.game$desc[Kickoff], regexpr, 
                               pattern = "kneel(s)?") != -1)

# Fair Catch
kickret1 <- sapply(issue.game$desc[Kickoff], stringr::str_extract, 
                         pattern = "by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?\\.$")

kickret2 <- sapply(kickret1, stringr::str_extract,
                         pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

kickret2[which(!is.na(kickret2))]

# Actual Return or TD

kickret3 <- sapply(issue.game$desc[Kickoff], stringr::str_extract, 
                         pattern = "(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})? to [A-Z]{2,3} [0-9]{1,2})|(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})? for [0-9]{1,2} yard(s))|(\\. [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})? pushed)")

kickret4 <- sapply(kickret3, stringr::str_extract, 
                         pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

kickret4[which(!is.na(kickret4))]


## Interception

issue.game[which(issue.game$InterceptionThrown == 1), "desc"]

inter <- ifelse(
  sapply(issue.game$desc, grepl, 
         pattern = "INTERCEPTED"), 1, 0)

interceptor1 <- sapply(issue.game$desc[which(inter ==1 )], stringr::str_extract, 
                   pattern = "INTERCEPTED by [A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

interceptor2 <- sapply(interceptor1, stringr::str_extract, 
                       pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")


###### Fumble Result 

issue.game[which(issue.game$Fumble == 1), "desc"]

fumbles <- which(sapply(issue.game$desc, regexpr, pattern = "FUMBLE") != -1)

recover.step1 <- sapply(issue.game$desc[fumbles], stringr::str_extract, 
             pattern = "[A-Z]{2,3}-[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

recover.team <- sapply(recover.step1, stringr::str_extract, 
                       pattern = "[A-Z]{2,3}")

recover.player <- sapply(recover.step1, stringr::str_extract, 
                       pattern = "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")

#########################################################
#### Calculating Score

team.home.score <- rep(0, times = nrow(issue.game))
team.away.score <- rep(0, times = nrow(issue.game))

away.team.name <- "MIA"
home.team.name <- "WAS"



which(issue.game$Touchdown == 1 
      & issue.game$posteam == away.team.name
      & !issue.game$ReturnResult %in% "Touchdown"
      & !issue.game$PlayType %in% "Kickoff")

which(issue.game$Touchdown == 1 
      & issue.game$posteam == home.team.name 
      & issue.game$ReturnResult %in% "Touchdown"
      & !issue.game$PlayType %in% "Kickoff")

which(issue.game$TwoPointConv == "Success" 
                      & issue.game$posteam == away.team.name) 
which(issue.game$ExPointResult == "Good" 
                      & issue.game$posteam == away.team.name)
which(issue.game$FieldGoalResult == "Good" 
                      & issue.game$posteam == away.team.name)

which(issue.game$Touchdown == 1 
      & issue.game$posteam == home.team.name 
      & !issue.game$ReturnResult %in% "Touchdown"
      & !issue.game$PlayType %in% "Kickoff")
which(issue.game$TwoPointConv == "Success" 
      & issue.game$posteam == home.team.name) 
which(issue.game$ExPointResult == "Good" 
      & issue.game$posteam == home.team.name)
which(issue.game$FieldGoalResult == "Good" 
      & issue.game$posteam == home.team.name)

which(issue.game$Touchdown == 1 
      & issue.game$posteam == away.team.name 
      & issue.game$ReturnResult %in% "Touchdown"
      & !issue.game$PlayType %in% "Kickoff")


other.issue.game <- game_play_by_play(2015100406)

away.team.name <- "CAR"
home.team.name <- "TB"

which(other.issue.game$Touchdown == 1 
      & other.issue.game$posteam == home.team.name
      & other.issue.game$ReturnResult %in% "Touchdown"
      & !other.issue.game$PlayType %in% "Kickoff")

which(other.issue.game$Touchdown == 1 
      & other.issue.game$posteam == away.team.name 
      & other.issue.game$ReturnResult %in% "Touchdown"
      & other.issue.game$RecFumbTeam == home.team.name)

which(other.issue.game$Touchdown == 1 
      & other.issue.game$posteam == away.team.name 
      & other.issue.game$ReturnResult %in% "Touchdown"
      & !other.issue.game$PlayType %in% "Kickoff")
& other.issue.game$RecFumbTeam == home.team.name

View(other.issue.game[which(other.issue.game$sp == 1 & other.issue.game$posteam == "TB"),])



safety.game <- game_play_by_play(2015110804)

safety.game[10,]

which(safety.game$Safety == 1 &
        safety.game$posteam == away.team.name)

## Another issue game

issue.game2 <- game_play_by_play(2012091600)

View(subset(issue.game2, sp ==1 & posteam == "NYG"))

View(subset(issue.game, sp ==1))

### 

pbp2015 <- season_play_by_play(2015)

pbp2015.rr <- subset(pbp2015, ReturnResult == "Touchdown" & sp == 0)

faulty.gameIDs <- pbp2015.rr$GameID

sapply(faulty.gameIDs, FUN = function(x) 
                              {y <- subset(game_play_by_play(x), 
                                ReturnResult == "Touchdown" & sp == 0)
                              nrow(y)}) 

