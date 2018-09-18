################################################################## 
###       Scraper Functions to put together game URLs          ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
################################################################## 


#' Extract GameIDs for each game in a given NFL season
#' @description This function is a helper for the Proper.PBP.URL.Formatting function.
#' The outputs of extracting_gameids is used in conjunction with Proper.PBP.URL.Formatting
#' to create the URLs for each NFL game in a specified season
#' @param Season (numeric) A numeric 4-digit year associated with an NFL season
#' @param playoffs (boolean) TRUE if you want the playoffs gameIDs for the specified 
#' season.  FALSE (default) if you want the game IDs from the regular season
#' @return A vector of NFL GameIDs from the specified season
#' @examples
#' # Scraping all game IDs from 2010 Season
#' extracting_gameids(2010) 
#' @export
extracting_gameids <- function(Season, playoffs = FALSE) {
  
  # Setting up to Pull Regular Season Weeks
  
  if (playoffs == FALSE) {
    # String for the regular season part of the URL
    url.year.sched <- paste("http://www.nfl.com/schedules", Season, 
                            "REG", sep = "/")
    
    # This Runs through the Week of the season and adds it as part of the URL
    url.schedule.weeks <- sapply(1:17, FUN = function(x) {
      paste(url.year.sched, 
            x, 
            sep = "" )
    })
    # Here I use regular expressions to scrape out the gameids for each game in
    # the specified season
    game.id.list <- sapply(url.schedule.weeks, 
                           FUN = function(x) {
                             sourceHTML <- scrapeR::scrape(url = x, 
                                                           headers = TRUE,
                                                           parse = FALSE)
                             extract.game.id <- stringr::str_extract_all(
                               unlist(sourceHTML),
                               pattern = "data-gameid=\"[0-9]{10}\"")
                             game.ids <- stringr::str_extract_all(
                               unlist(extract.game.id),
                               pattern = "[0-9]{10}")
                           })
    
    game.id.list <- unlist(game.id.list)
    names(game.id.list) <- NULL
  }
  
  else {
    url.schedule.playffs <- paste("http://www.nfl.com/schedules", Season, 
                                  "POST", sep = "/")
    
    playoff.gameid.list <- sapply(url.schedule.playffs, 
                                  FUN = function(x) {
                                    sourceHTML <- scrapeR::scrape(url = x, 
                                                                  headers = TRUE,
                                                                  parse = FALSE)
                                    extract.game.id <- stringr::str_extract_all(
                                      unlist(sourceHTML),
                                      pattern = "data-gameid=\"[0-9]{10}\"")
                                    game.ids <- stringr::str_extract_all(
                                      unlist(extract.game.id),
                                      pattern = "[0-9]{10}")
                                  })
    
    game.id.list <- unlist(playoff.gameid.list)
    names(game.id.list) <- NULL
  }
  
  # Find which game.id.list have happened based on the date,
  # first find the current date and put it in a format for comparing:
  current.date <- stringr::str_replace_all(Sys.Date(),pattern = "-","")
  
  # Create a date form the game_ids to compare to the current_date:
  date.game.id.list <- substr(game.id.list, 1, 8)
  
  # Only use the game_ids that are on or before the given date
  game.id.list <- game.id.list[which(date.game.id.list <= current.date)]
  
  return(game.id.list)
}


#' Formatting URL for location of NFL Game JSON Data
#' @description This function pastes together the proper formatting of 
#' the nfl play by play data JSON URL such that it can be used in our 
#' play-by-play functions.  This function calls the extracting_gameids function
#' 
#' @param GameID (character or numeric) A 10 digit game ID associated with a 
#' given NFL game.
#' @return The url where the game JSON data for the given game can be found.
#' @examples
#' # Save the gameID into a variable 
#' nfl2015.finalregseasongame.gameID <- "2016010310"
#' 
#' # Putting all game IDs in to proper URL format
#' proper_jsonurl_formatting(nfl2015.finalregseasongame.gameID) 
#' @export
proper_jsonurl_formatting <- function(GameID) {
  
  # Paste together the proper location of the JSON data
  paste("http://www.nfl.com/liveupdate/game-center/", GameID, "/",
        GameID, "_gtd.json", sep = "")
}


