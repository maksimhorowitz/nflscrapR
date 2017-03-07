################################################################## 
###               Game and Roster Functions                    ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
##################################################################

# Games in a Season
#' Game Information for All Games in a Season
#' @description This function intakes a year associated with a given season
#' and outputs all the game matchups for all 17 weeks of the regular season
#' @param Season (numeric): A 4-digit year associated with a given NFL season
#' @param Week (numeric): A number corresponding to the number of weeks of data
#' you want to be scraped and included in the output.
#' @param sleep.seconds (numeric): Allows the user to tell the function to sleep
#' between calls to the API to avoid disrupting the connection. Note, this 
#' will make the function take much longer.
#' @return A dataframe with the gameID, the game date, 
#' the home team abbreviation, and away team abbreviation 
#' @details Reference the stored dataframe nflteams to match team abbreviations
#' with the full team names
#' @examples
#' # All games in 2015 Season
#' season_games(2015) # Will output a dataframe
#' @export
season_games <- function(Season, Weeks = 16, sleep.seconds = 0) {
  
  game_ids <- extracting_gameids(2016)
  
  # If statement to specify the week variable
  if (Weeks %in% 4:13) {
    game_ids <- game_ids[1:(16*Weeks)-1]
  } else if (Weeks %in% c(1:3, 14:15)) {
    game_ids <- game_ids[1:(16*Weeks)]
  }
  
  game_urls <- sapply(game_ids, proper_jsonurl_formatting)
  
  # Game Dates
  year <- substr(game_ids, start = 1, stop = 4)
  month <- substr(game_ids, start = 5, stop = 6)
  day <- substr(game_ids, start = 7, stop = 8)
  date <- as.Date( paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  # Home and Away Teams
  
  games.unformatted <- lapply(game_urls, 
                              FUN = function(x) {
                                Sys.sleep(sleep.seconds)
                                games.df <- cbind(t(sapply(RJSONIO::fromJSON(RCurl::getURL(x))[[1]]$home[2]$abbr,
                                               c)),
                                      t(sapply(RJSONIO::fromJSON(RCurl::getURL(x))[[1]]$away[2]$abbr,
                                               c)),
                                      t(sapply(max(RJSONIO::fromJSON(RCurl::getURL(x))[[1]]$home$score),
                                               c)),
                                      t(sapply(max(RJSONIO::fromJSON(RCurl::getURL(x))[[1]]$away$score),
                                               c)))
                                
                                data.frame(home = games.df[1],
                                           away = games.df[2],
                                           homescore = games.df[3] %>% as.numeric(),
                                           awayscore = games.df[4] %>% as.numeric())
                              })
  
  games <- suppressWarnings(dplyr::bind_rows(games.unformatted) %>% 
            dplyr::mutate(GameID = game_ids, 
                   date = date))

  # Output Dataframe
  
  games %>% dplyr::select(GameID, date, 
                          home, away, homescore, awayscore)
  
}

################################################################## 
#' Season Rosters for Teams
#' @description This function intakes a year and a team abbreviation and outputs
#' a dataframe with each player who has played for the specified team and 
#' recorded a measurable statistic
#' @param Season: A 4-digit year associated with a given NFL season
#' @param TeamInt: A string containing the abbreviations for an NFL Team
#' @param Week (numeric): A number corresponding to the number of weeks of data
#' you want to be scraped and included in the output.
#' @details To find team associated abbrevations use the nflteams dataframe 
#' stored in this package!
#' @return A dataframe with columns associated with season/year, team, playerID,
#' players who played and recorded some measurable statistic, and the 
#' last column specifyng the number of games they played in.
#' @examples
#' # Roster for Baltimore Ravens in 2013
#' season_rosters(2013, TeamInt = "BAL") 
#' @export
season_rosters <- function(Season, Weeks = 16, TeamInt) {
  
  # Use the season_playergame to gather all names of players on a given team
  team.roster.s1 <- subset(season_player_game(Season, Weeks = Weeks), 
                           Team == TeamInt)
  
  # Use dplyr to subset the data and gather games played
  team.roster <- dplyr::group_by(team.roster.s1, Season, Team, playerID, name)
  team.roster <- dplyr::summarize(team.roster, length(playerID))
  
  colnames(team.roster)[ncol(team.roster)] <- "gamesplayed"
  
  # Output Dataframe
  team.roster
}
