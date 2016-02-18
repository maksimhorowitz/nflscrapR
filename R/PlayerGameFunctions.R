### Player Games Functions ###

#' Boxscore for a Single Game
#' @description This function is used to neatly read all a players measurable 
#' statistics from a given game.  Each player's statistics can be viewed on one 
#' line.  
#' @param URLString (character) A URL character string linking to the JSON API 
#' data from a single game
#' @details This dataframe includes 36 variables including identifiers such as:
#'  \itemize{
#'  \item{"gameID", "date", "team", "playerID", "name"}
#'  }
#' Statistics are included for passing, rushing, receiving, defensive, 
#' and fumbles.
#' 
#' @return This function outputs a single dataframe containing all rushing, passing,
#' receiving, fumble, and defensive statistics for each player in a single  game.  
#' Each player is assigned one line associated wih their statisitcs.
#' @examples
#' # URL Link to NFL JSON data for a random game
#' nfl.data <- "http://www.nfl.com/liveupdate/game-center/2013090800/2013090800_gtd.json"
#' PlayerGameTest <- playergame(nfl.data)
#' 
playergame <- function(URLString) {
  
  # Converting URL into readable JSON format
  nfl.json <- RJSONIO::fromJSON(RCurl::getURL(URLString))
  
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
    dfdef$playerID <- rownames(dfdef)
    
    # This stage is where we merge all the dataframes together so each player 
    # has one line
    final.df <- Reduce(function(x, y) 
    {merge(x, y, by = c("Team", "playerID", "name"),all=TRUE, sort = FALSE)},
    list(dfpass, dfrush, dfrec, dfdef))
    
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
    dffumb$playerID <- rownames(dffumb)
    dfdef$playerID <- rownames(dfdef)
    
    # This stage is where we merge all the dataframes together so each player 
    # has one line 
    
    final.df <- Reduce(function(x,y) {
      merge(x, y, by = c("Team", "playerID", "name"), all = TRUE, sort = FALSE)},
      list(dfpass, dfrush, dfrec, dffumb, dfdef))
  }
  
  final.df <- data.frame(Team = final.df[,1],
                         sapply(final.df[,-1], 
                                function(x) ifelse((x == "NULL" | is.na(x)), 
                                                   0, x)))
  rownames(final.df) <- NULL
  
  # GameID
  game.id <- stringr::str_extract(URLString, pattern = "[0-9]{10}")
  
  # Date of Game   
  datestep1 <- stringr::str_extract(URLString, pattern = "/[0-9]{10}/")
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
  
  final.df2$playerID <- unlist(final.df2$playerID)
  final.df2$name <- unlist(final.df2$name)
  final.df2$Team.x <- unlist(final.df2$Team.x)
  final.df2$att.x <- unlist(final.df2$att.x)
  final.df2$cmp <- unlist(final.df2$cmp)
  final.df2$yds.x <- unlist(final.df2$yds.x)
  final.df2$tds.x <- unlist(final.df2$tds.x)
  final.df2$ints <- unlist(final.df2$ints)
  final.df2$twopta.x <- unlist(final.df2$twopta.x)
  final.df2$twoptm.x <- unlist(final.df2$twoptm.x)
  final.df2$att.y <- unlist(final.df2$att.y)
  final.df2$yds.y <- unlist(final.df2$yds.y)
  final.df2$tds.y <- unlist(final.df2$tds.y)
  final.df2$lng.x <- unlist(final.df2$lng.x)
  final.df2$lngtd.x <- unlist(final.df2$lngtd.x)
  final.df2$twopta.y <- unlist(final.df2$twopta.y)
  final.df2$twoptm.y <- unlist(final.df2$twoptm.y)
  final.df2$rec <- unlist(final.df2$rec)
  final.df2$yds <- unlist(final.df2$yds)
  final.df2$tds <- unlist(final.df2$tds)
  final.df2$lng.y <- unlist(final.df2$lng.y)
  final.df2$lngtd.y <- unlist(final.df2$lngtd.y)
  final.df2$twopta <- unlist(final.df2$twopta)
  final.df2$twoptm <- unlist(final.df2$twoptm)
  final.df2$totalfumbs <- unlist(final.df2$totalfumbs)
  final.df2$recfumbs <- unlist(final.df2$recfumbs)
  final.df2$totalrecfumbs <- unlist(final.df2$totalrecfumbs)
  final.df2$fumbyds <- unlist(final.df2$fumbyds)
  final.df2$fumbslost <- unlist(final.df2$fumbslost)
  final.df2$tkl <- unlist(final.df2$tkl)
  final.df2$ast <- unlist(final.df2$ast)
  final.df2$sk <- unlist(final.df2$sk)
  final.df2$int <- unlist(final.df2$int)
  final.df2$ffum <- unlist(final.df2$ffum)
  
  colnames(final.df2)<- c("gameID", "date", "team", "playerID", "name",
                          "passatt","compl","passyds", "passtds", "passint", 
                          "passtwoptattempts", "passtwoptmade", "rushatt",
                          "rushyds", "rushtds", "rushlong", "rushlongtd",
                          "rushtwoptattp", "rushtwoptmade", "recpt", "recyds", 
                          "rectds", "reclong", "reclongtd",
                          "rectwoptatt", "rectwoptmade", "totalfumbs",
                          "fumbsrecovered","totalrecfumbs",
                          "fumbydds", "fumbslost", "tackles", "assistedtkls",
                          "sacks", "defint", "forcedfumbs")
  
  final.df2[order(final.df2$date, final.df2$team),]
}

# Everygame in a Given Season

#' Boxscore for Each Game in the Season - One line per player per game
#' @description This function outputs a single dataframe containing all rushing, 
#' passing, receiving, fumble, and defensive statistics for each player in 
#' each game.  Each player is assigned one line associated wih their statisitcs 
#' per each game they record a measured statistic
#' 
#' @param Season (numeric) A 4-digit year associated with a given season  
#'
#' @return A single line for each player in each game they played in.  The 
#' output is the same as the playergame function but is run for every game in
#' the specified season
#' @examples
#' # Player-Game function over the entire season in 2010
#' # Returns a dataframe with 360 Rows and 36 Columns
#' PlayerGame.Function(2010)

season_playergame <- function(Season) {
  
  game.ids <- extracting_gameids(Season)
  game.urls <- sapply(game.ids, proper_jsonurl_formatting)
  
  playergame.season.unformatted <- lapply(game.urls, FUN = playergame)
  
  # Rowbinding all the games from the specified season
  
  playergame.season <- do.call(rbind, playergame.season.unformatted)
  
  # Final output dataframe
  data.frame(Year = Season, playergameseason)
}

# Aggregated for Each Player over the Season 

#' Players Aggregate Season Statistics (Passing, Rushing, Receiving, Fumbles,
#' and Defense)
#' @description This function outputs a dataframe with the season statistics for
#' each player who recorded atleast one measured statistic in any game throughout
#' the specified season.  This function gives one line per player.
#' @param Season (numeric) A 4-digit year associated with a given season 
#' @details This function calls season_playergame and then does aggregation 
#' across an entire season 
#' @return Returns a dataframe with a single line for each player aggregating 
#' their total season statistics
#' @examples
#' # Returns the Season-Total Statistics for Each Player in the 2015 Season
#' agg_player_season(2015)

agg_player_season <- function(Season) {
  
  # Use the season_playergame function to generate a dataset with all the games
  # in a given season which we will aggregate over
  playerdata.year <- season_playergame(Season)
  
  # Use dplyr to aggregate
  # Here we use the sum function for cumulative yards
  season.sum.agg <-  dplyr::group_by(playerdata.year,
                                   Year, team, playerID, name)
  
  season.sum.agg <- dplyr::summarise_each(season.sum.agg, 
                                        funs(sum), -date, -rushlong, -rushlongtd
                                        , -reclong, -reclongtd, -gameID)
  
  # Here we find the max "long run" and max "long reception"
  season.max.agg <- dplyr::group_by(playerdata.year, Year, team, playerID, name) 
  
  season.max.agg <- dplyr::summarise_each(season.max.agg, funs(max), rushlong, 
                                        rushlongtd, reclong, reclongtd)
  
  # Merging the Two datasets
  merge(season.sum.agg, season.max.agg, 
        by = c("Year", "team", "playerID", "name"))
}