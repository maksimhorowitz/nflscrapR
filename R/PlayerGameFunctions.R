################################################################## 
###               Player Game Functions                        ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
##################################################################

### Player Games Functions ###

#' Detailed Boxscore for Single NFL Game
#' @description This function is used to neatly read all of a players measurable 
#' statistics from a given game.  Each player's statistics can be viewed on one 
#' line.
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @details This dataframe includes 55 variables including identifiers such as:
#'  \itemize{
#'  \item{"Year", "gameID", "date", "Team", "playerID", "name"}
#'  }
#' Statistics are included for passing, rushing, receiving, kick return,
#' punt return, kicking, defensive, and fumbles.  The outputted columns are as
#' follows:
#' 
#'  \itemize{
#'  \item{"pass.att"} - Number of pass attempts
#'  \item{"pass.comp"} - Number of completed passes
#'  \item{"passyds"} - Number of pass yards
#'  \item{pass.tds"} - Number of passing touchdowns
#'  \item{"pass.ints"} - Number of pass interceptions
#'  \item{"pass.twopta"} - Number of passing two point conversions attempted
#'  \item{"pass.twoptm"} - Number of passing two point conversions converted
#'  \item{"rush.att"} - Number of rush attempts
#'  \item{"rushyds"} - Number of rushing Yards
#'  \item{"rushtds"} - Number of rushing touchdowns
#'  \item{"rushlng"} - Most yards gained on a rush attempt
#'  \item{"rushlngtd"} - Yards gained on longest touchdown run
#'  \item{"rush.twopta"} - Number of rushing two point conversions attempted
#'  \item{"rush.twoptm"} - Number of rushing two point conversions converted
#'  \item{"recept"} - Number of receptions
#'  \item{"recyds"} - Number of receiving yards
#'  \item{"rec.tds"} - Number of receiving touchdowns
#'  \item{"reclng"} - Longest reception
#'  \item{"reclngtd"} - Longest receiving touchdown
#'  \item{"rec.twopta"} - Number of targets on a two point conversion attempt
#'  \item{"rec.twoptm"} - Number of receptions that resulted in a two point conversion success
#'  \item{"kick.rets"} - Number of kickoff returns
#'  \item{"kickret.avg"} - Average number of yards gained on kickoff returns
#'  \item{"kickret.tds"} - Number of kickoff return touchdown
#'  \item{"kick.ret.lng"} - Yards gained on longest kickoff return
#'  \item{"kickret.lngtd"} - Yards gained on longest kickoff return that resulted in a touchdown
#'  \item{"punt.rets"} - Number of punt returns
#'  \item{"puntret.avg"} - Average number of yards gained on punt returns
#'  \item{"puntret.tds"} - Number of punt return touchdowns
#'  \item{"puntret.lng"} - Yards gained on longest punt return
#'  \item{"puntret.lngtd"} - Yards gained on longest punt return that resulted in a touchdown
#'  \item{"fgm"} - Number of field goals made
#'  \item{"fga"} - Number of field goals attempted
#'  \item{"fgyds"} - Yard length of longest made field
#'  \item{"totpts.fg"} - Point value of all made field goals
#'  \item{"xpmade"} - Number of extra points made
#'  \item{"xpmissed"} - Number of extra points missed
#'  \item{"xpa"} - Number of attempted extra points
#'  \item{"xpb"} - Number of extra points blocked
#'  \item{"xppts.tot"} - Point value of all made extra points
#'  \item{"tackles"} - Number of tackles recorded
#'  \item{"asst.tackles"} - Number of assisted tackles
#'  \item{"sacks"} - Number of sacks
#'  \item{"defints"} - Number of defensive interceptions
#'  \item{"forced.fumbs"} - Number of forced fumbles
#'  \item{"totalfumbs"} - Total fumbles associated with a player
#'  \item{"recfumbs"} - Number of recovered fumble
#'  \item{"totalrecfumbs"} - Number of recovered fumble
#'  \item{"fumbyds"} - Number of yards recorded on fumble returns
#'  \item{"fumbslost"} - Number of fumbles lost
#' }
#'  
#' @return This function outputs a single 55 column dataframe containing all 
#' rushing, passing, receiving, kick return, punt return, kicking, fumble, 
#' and defensive statistics for each player in a single  game. Each player is
#'  assigned one line associated wih their statisitcs.
#' @examples
#' # GameID for a random game
#' nfl.data.gameID <- "2013090800"
#' PlayerGameData <- player_game(nfl.data.gameID)
#' head(PlayerGameData)
#' @export
player_game <- function(GameID) {
  
  # Converting GameID into URL string
  urlstring <- proper_jsonurl_formatting(GameID)
  
  # Converting URL into readable JSON format
  nfl.json <- tryCatch(RJSONIO::fromJSON(RCurl::getURL(urlstring)),
                       error=function(cond) {
    message("Connection to API disrupted, please re-run code. If multiple failures, then there is no data available for this game yet.")
    message(paste("Here is the url:", urlstring))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  })
  
  # Here we build the dataframes for the rushing, passing, receving, defense,
  # and fumbling stats
  
  dfpass <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                             t(sapply(nfl.json[[1]][[1]]$stats$passing, c))),
                  data.frame(Team = nfl.json[[1]][[2]]$abbr,
                             t(sapply(nfl.json[[1]][[2]]$stats$passing, c))))
  
  dfrush <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                             t(sapply(nfl.json[[1]][[1]]$stats$rushing, c))),
                  data.frame(Team = nfl.json[[1]][[2]]$abbr,
                             t(sapply(nfl.json[[1]][[2]]$stats$rushing, c))))
  
  dfrec <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                            t(sapply(nfl.json[[1]][[1]]$stats$receiving, c))),
                 data.frame(Team = nfl.json[[1]][[2]]$abbr,
                            t(sapply(nfl.json[[1]][[2]]$stats$receiving, c))))
  
  dfdef <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                            t(sapply(nfl.json[[1]][[1]]$stats$defense, c))),
                 data.frame(Team = nfl.json[[1]][[2]]$abbr,
                            t(sapply(nfl.json[[1]][[2]]$stats$defense, c))))
  # Accounting for the change that one or both teams never punt or never kick
  
  #########
  # Kicking
  #########
  
  if (length(nfl.json[[1]][[1]]$stats$kicking) == 0 & 
        length(nfl.json[[1]][[2]]$stats$kicking) > 0) {
    dfkicking <- data.frame(Team = nfl.json[[1]][[2]]$abbr,
                            t(sapply(nfl.json[[1]][[2]]$stats$kicking, c)))
  } 
  
  else if (length(nfl.json[[1]][[1]]$stats$kicking) > 0 & 
             length(nfl.json[[1]][[2]]$stats$kicking) == 0) {
    dfkicking <- data.frame(Team = nfl.json[[1]][[1]]$abbr,
                            t(sapply(nfl.json[[1]][[1]]$stats$kicking, c)))
  }
  
  else if (length(nfl.json[[1]][[1]]$stats$kicking) == 0 & 
             length(nfl.json[[1]][[2]]$stats$kicking) == 0) {
    dfkicking <- data.frame("Team" = NA , "name" = NA , "fgm" = NA, 
                            "fga" = NA, "fgyds" = NA, 
                            "totpfg" = NA, "xpmade" = NA, "xpmissed" = NA,
                            "xpa" = NA, "xpb" = NA, "xptot" = NA)
  } 
  
  else {
    dfkicking <-  rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                                   t(sapply(nfl.json[[1]][[1]]$stats$kicking, 
                                            c))),
                        data.frame(Team = nfl.json[[1]][[2]]$abbr,
                                   t(sapply(nfl.json[[1]][[2]]$stats$kicking,
                                            c))))
  }
  
  ##########
  # Punt Ret
  ##########
  if (length(nfl.json[[1]][[1]]$stats$puntret) == 0 & 
    length(nfl.json[[1]][[2]]$stats$puntret) > 0) {
    dfpuntret <- data.frame(Team = nfl.json[[1]][[2]]$abbr,
                         t(sapply(nfl.json[[1]][[2]]$stats$puntret, c)))
  } 
  
  else if (length(nfl.json[[1]][[1]]$stats$puntret) > 0 & 
             length(nfl.json[[1]][[2]]$stats$puntret) == 0) {
    dfpuntret <- data.frame(Team = nfl.json[[1]][[1]]$abbr,
                            t(sapply(nfl.json[[1]][[1]]$stats$puntret, c)))
  }
  
  else if (length(nfl.json[[1]][[1]]$stats$puntret) == 0 & 
             length(nfl.json[[1]][[2]]$stats$puntret) == 0) {
  dfpuntret <- data.frame("Team" = NA , "name" = NA , "punt.rets" = NA, 
                          "puntret.avg" = NA, "puntret.tds" = NA, 
                          "puntret.lng" = NA, "puntret.lngtd" = NA)
  } 
  
  else {
    dfpuntret <-  rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                     t(sapply(nfl.json[[1]][[1]]$stats$puntret, c))),
          data.frame(Team = nfl.json[[1]][[2]]$abbr,
                     t(sapply(nfl.json[[1]][[2]]$stats$puntret, c))))
  }
  
  ##########
  # Kick Ret
  ##########
  
  if (length(nfl.json[[1]][[1]]$stats$kickret) == 0 & 
        length(nfl.json[[1]][[2]]$stats$kickret) > 0) {
    dfkickret <- data.frame(Team = nfl.json[[1]][[2]]$abbr,
                            t(sapply(nfl.json[[1]][[2]]$stats$kickret, c)))
  } 
  
  else if (length(nfl.json[[1]][[1]]$stats$kickret) > 0 & 
             length(nfl.json[[1]][[2]]$stats$kickret) == 0) {
    dfkickret <- data.frame(Team = nfl.json[[1]][[1]]$abbr,
                            t(sapply(nfl.json[[1]][[1]]$stats$kickret, c)))
  }
  
  else if (length(nfl.json[[1]][[1]]$stats$kickret) == 0 & 
             length(nfl.json[[1]][[2]]$stats$kickret) == 0) {
    dfkickret <- data.frame("Team" = NA , "name" = NA , "kick.rets" = NA, 
                            "kickret.avg" = NA, "kickret.tds" = NA, 
                            "kickret.lng" = NA, "kickret.lngtd" = NA)
  } 
  
  else {
    dfkickret <-  rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                                   t(sapply(nfl.json[[1]][[1]]$stats$kickret, 
                                            c))),
                        data.frame(Team = nfl.json[[1]][[2]]$abbr,
                                   t(sapply(nfl.json[[1]][[2]]$stats$kickret, 
                                            c))))
  }
  
  # Adding rushlongtd, and reclongtd for 2009 season which did not include 
  # these variables.  Arbitrarily made 0.
  if (ncol(dfrec) == 8 & ncol(dfrush) == 8) {
    dfrec$reclngtd <- 0
    dfrush$rushlngtd <- 0
    
    # reorder columns to be in uniform order with other seasons
    dfrec <- dfrec[,c(1,2,3,4,5,6,9,7,8)]
    dfrush <- dfrush[,c(1,2,3,4,5,6,9,7,8)]
  }

  # Adding kickret, lngtd for 2009 season which did not include these variable.
  # Arbitrarily made 0.
  if (ncol(dfkickret) == 6) {dfkickret$kickret.lngtd <- 0}
  if (ncol(dfpuntret) == 6) {dfpuntret$puntret.lngtd <- 0}
  
  # Changing colnames of all stat dfs
  colnames(dfpass) <- c("Team", "name", "pass.att", "pass.comp", "passyds", 
                        "pass.tds", "pass.ints", "pass.twopta", "pass.twoptm")
  
  colnames(dfrush) <- c("Team", "name", "rush.att", "rushyds", "rushtds", 
                        "rushlng", "rushlngtd", "rush.twopta", "rush.twoptm")
  
  colnames(dfrec) <- c("Team", "name", "recept", "recyds", "rec.tds", "reclng",
                       "reclngtd", "rec.twopta", "rec.twoptm")
  
  colnames(dfdef) <- c("Team", "name", "tackles", "asst.tackles", "sacks",
                       "defints", "forced.fumbs")
  
  colnames(dfkicking) <- c("Team", "name", "fgm", "fga", "fgyds", "totpts.fg",
                           "xpmade", "xpmissed", "xpa", "xpb", "xppts.tot")
  
  colnames(dfkickret) <- c("Team", "name", "kick.rets", "kickret.avg", 
                           "kickret.tds", "kick.ret.lng", "kickret.lngtd")
  
  colnames(dfpuntret) <- c("Team", "name", "punt.rets", "puntret.avg", 
                           "puntret.tds", "puntret.lng", "puntret.lngtd")
  
  
  # Case when both teams have at least one fumble
  if (length(nfl.json[[1]][[1]]$stats$fumbles) > 0 & 
        length(nfl.json[[1]][[2]]$stats$fumbles) > 0 ) {
    
    dffumb <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                               t(sapply(nfl.json[[1]][[1]]$stats$fumbles, c))),
                    data.frame(Team = nfl.json[[1]][[2]]$abbr,
                               t(sapply(nfl.json[[1]][[2]]$stats$fumbles, c))))
  }
  
  # Case when there are no fumbles for either team
  else if (length(nfl.json[[1]][[1]]$stats$fumbles) == 0 & 
             length(nfl.json[[1]][[2]]$stats$fumbles) == 0 ) {
    dffumb <- NULL
  }
  
  # Case when team 2 fumbles but team 1 does not 
  else if (length(nfl.json[[1]][[1]]$stats$fumbles) == 0 & 
             length(nfl.json[[1]][[2]]$stats$fumbles) > 0) {
    dffumb <- data.frame(Team = nfl.json[[1]][[2]]$abbr,
                         t(sapply(nfl.json[[1]][[2]]$stats$fumbles, c)))
  }
  
  # Case when team 1 fumbles but team 2 does not 
  else {
    dffumb <- data.frame(Team = nfl.json[[1]][[1]]$abbr,
                         t(sapply(nfl.json[[1]][[1]]$stats$fumbles, c)))
  }
  
  if (is.null(dffumb)) {
    
    # Initialize a new variable with the player IDs
    dfpass$playerID <- rownames(dfpass)
    dfrush$playerID <- rownames(dfrush)
    dfrec$playerID <- rownames(dfrec)
    dfkicking$playerID <- rownames(dfkicking)
    dfpuntret$playerID <- rownames(dfpuntret)
    dfkickret$playerID <- rownames(dfkickret)
    dfdef$playerID <- rownames(dfdef)
    
    # This stage is where we merge all the dataframes together so each player 
    # has one line
    final.df <- Reduce(function(x, y) 
    {merge(x, y, by = c("Team", "playerID", "name"),all=TRUE, sort = FALSE)},
    list(dfpass, dfrush, dfrec, dfkickret, dfpuntret, dfkicking, dfdef))
    
    # Adding Fumble columns with 0's due to no occurance of fumbles in game
    final.df$totalfumbs <- 0
    final.df$recfumbs <- 0
    final.df$totalrecfumbs <- 0
    final.df$fumbyds <- 0
    final.df$fumbslost <- 0
  }
  
  else {
    
    # Renaming for dffumb Columns
    colnames(dffumb) <- c("Team", "name", "totalfumbs", "recfumbs", 
                          "totalrecfumbs","fumbyds", "fumbslost")
    
    # Initialize a new variable with the player IDs
    dfpass$playerID <- rownames(dfpass)
    dfrush$playerID <- rownames(dfrush)
    dfrec$playerID <- rownames(dfrec)
    dfkicking$playerID <- rownames(dfkicking)
    dfpuntret$playerID <- rownames(dfpuntret)
    dfkickret$playerID <- rownames(dfkickret)
    dffumb$playerID <- rownames(dffumb)
    dfdef$playerID <- rownames(dfdef)
    
    # This stage is where we merge all the dataframes together so each player 
    # has one line 
    
    final.df <- Reduce(function(x,y) {
      merge(x, y, by = c("Team", "playerID", "name"), all = TRUE, sort = FALSE)}
      , list(dfpass, dfrush, dfrec, dfkickret, dfpuntret,
             dfkicking, dfdef, dffumb))
  }
  
  final.df <- data.frame(Team = final.df[,1],
                         sapply(final.df[,-1], 
                                function(x) ifelse((x == "NULL" | is.na(x)), 
                                                   0, x)))
  rownames(final.df) <- NULL
  
  # GameID
  game.id <- stringr::str_extract(urlstring, pattern = "[0-9]{10}")
  
  # Date of Game   
  datestep1 <- stringr::str_extract(urlstring, pattern = "/[0-9]{10}/")
  datestep2 <- stringr::str_extract(datestep1, pattern = "[0-9]{8}")
  year <- substr(datestep2, start = 1, stop = 4)
  month <- substr(datestep2, start = 5, stop = 6)
  day <- substr(datestep2, start = nchar(datestep2)-1, stop = nchar(datestep2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  # Output dataframe which has the game.id, date of game, and the player info
  # and statistics 
  final.df2 <- data.frame(game.id, date, final.df)
  
  # Unlist the listed variables in order to return the output dataframe in a 
  # friendlier format
  
  final.df2 <- data.frame(lapply(final.df2, unlist))
  
  # Counter for games
  final.df2$games <- 1
  
  # Output dataframe arranged by date and Team name
  final.df2 %>% dplyr::arrange(date, Team)
}

# Everygame in a Given Season
#' Boxscore for Each Game in the Season - One line per player per game
#' @description This function outputs a single dataframe containing all rushing, 
#' passing, receiving, fumble, and defensive statistics for each player in 
#' each game.  Each player is assigned one line associated wih their statisitcs 
#' per each game they record a measured statistic
#' 
#' @param Season (numeric) A 4-digit year associated with a given season  
#' @param Week (numeric) A number corresponding to the number of weeks of data
#' you want to be scraped and included in the output. If you input 3, the first
#' three weeks of player statistics will be scraped from the associated season.
#' @return A single line for each player in each game they played in.  The 
#' output is the same as the player_game function but is run for every game in
#' the specified season
#' @examples
#' # Player-Game function over the entire season in 2010
#' playerstats.2010 <- season_player_game(2010)
#' head(playerstats.2010)
#' 
#' # Plot a graph of different play types
#' library(ggplot2)
#' ggplot(playerstats.2010, aes(x = PlayType)) + geom_bar()
#' @export
season_player_game <- function(Season, Weeks = 16) {
  
  game_ids <- extracting_gameids(Season)
  
  if (Weeks %in% 3:15) {
    game_ids <- game_ids[1:(16*Weeks)-1]
  } else if (Weeks %in% 1:2) {
    game_ids <- game_ids[1:(16*Weeks)]
  }
  
  playergame.season.unformatted <- lapply(game_ids, FUN = player_game)
  
  # Rowbinding all the games from the specified season
  
  suppressWarnings(playergame.season <- dplyr::bind_rows(playergame.season.unformatted))
  
  # Final output dataframe
  data.frame(Season = Season, playergame.season)
}

# Aggregated for Each Player over the Season 
#' Detailed Player Aggregate Season Statistics
#' @description This function outputs a dataframe with the season statistics for
#' each player who recorded atleast one measured statistic in any game throughout
#' the specified season.  This function gives one line per player with the 
#' following statistics: Passing, Rushing, Receiving, Kick Return,
#' Punt Return, Fumbles, and Defense
#' @param Season (numeric) A 4-digit year associated with a given season 
#' @details This function calls season_player_game and then aggregates 
#' across an entire season to gather season totals and season max statistics
#' @return Returns a dataframe with a single line for each player aggregating 
#' their total season statistics
#' @examples
#' # Returns the Season-Total Statistics for Each Player in the 2015 Season
#' agg_player_season(2015)
#' @export
agg_player_season <- function(Season, Weeks = 16) {
  
  # Use the season_playergame function to generate a dataset with all the games
  # in a given season which we will aggregate over
  playerdata.year <- season_player_game(Season, Weeks = Weeks)
  
  # Use dplyr to aggregate
  # Here we use the sum function for cumulative yards
  season.sum.agg <-  dplyr::group_by(playerdata.year,
                                     Season, Team, playerID, name)
  
  season.sum.agg <- dplyr::summarise_each(season.sum.agg, 
                                          dplyr::funs(sum), -date, -rushlng, -rushlngtd
                                        , -reclng, -reclngtd, -game.id, 
                                        -puntret.lng, -puntret.lngtd, 
                                        -kick.ret.lng, -kickret.lngtd)
  
  # Here we find the max "long run" and max "long reception"
  season.max.agg <- dplyr::group_by(playerdata.year, Season, Team, playerID, name) 
  
  season.max.agg <- dplyr::summarise_each(season.max.agg, dplyr::funs(max), 
                                        rushlng, rushlngtd, reclng, reclngtd, 
                                        puntret.lng, puntret.lngtd, 
                                        kick.ret.lng, kickret.lngtd, -games)
  
  # Merging the Two datasets
  season.stats <- merge(season.sum.agg, season.max.agg, 
                      by = c("Season", "Team", "playerID", "name"))
  
  season.stats$Season <- Season
  
  # Final Output
  season.stats
}