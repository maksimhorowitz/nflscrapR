#' Scrape game ids for a given NFL season (either pre, regular, or post season) 
#' 
#' This function returns all available game ids for a given season,
#' with an option specifying which type of game ids to return (either the 
#' pre, regular, or post season).
#' 
#' @param season Numeric 4-digit year associated with an NFL season
#' @param type String indicating the type of game ids to find, must either be
#' "pre", "reg", or "post" (default is "reg").
#' @param playoffs Boolean (TRUE or FALSE) if you want the playoffs game ids 
#' instead of the regular season (default is FALSE returning regular season games).
#' @return String vector of NFL game ids from the specified season and game types.
#' @examples
#' # Scraping all game ids from 2017 regular season:
#' scrape_game_ids(2017) 
#' @export

scrape_game_ids <- function(season, type = "reg") {
  
  # First check that the type is one of the required options:
  assertthat::assert_that(tolower(type) %in% c("reg", "pre", "post"),
                          msg = "Input for type is not valid! Should either be 'reg', 'pre', or 'post'.")
  
  # Next check that if the type is pre then the season is at least 1999:
  if (tolower(type) == "pre") {
    assertthat::assert_that(as.numeric(season) >= 1999,
                            msg = "Preseason game ids are only available starting in 1999!")
  }
  
  # Construct base schedule url for the season and type:
  base_url_schedule <- paste("http://www.nfl.com/schedules", season,
                             toupper(type), sep = "/")
  
  # Define the pipeline that will be used for type of games to scrape:
  
  scrape_game_ids <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-gameid=\"[0-9]{10}\"") %>%
    unlist() %>%
    stringr::str_extract("[0-9]{10}") %>%
    unlist()
  
  # If the type is post then just gather all the game ids from the post season
  # schedule page (since they are not separated by the week unlike the pre or
  # regular season):
  
  if (tolower(type) == "post") {
    
    game_ids <- base_url_schedule %>%
      scrape_game_ids
    
  # Else need to do it by week for the pre and regular season
  } else {
    
    # First create the numeric week vector that controls the pages to scrape
    # the game ids for depending on the type, pre goes from 0 to 4 while 
    # regular season goes from 1 to 17:
    if (tolower(type) == "pre") {
      weeks <- 0:4
    } else {
      weeks <- 1:17
    }
    
    # Now apply the pipeline to each week returning a single vector of game ids:
    game_ids <- purrr::map_chr(weeks, function(x) {
      paste0(base_url_schedule, x) %>%
        scrape_game_ids
    })
  }
      
  # Return the game ids:
  return(game_ids)
}


#' Create the url with the location of NFL game JSON data
#' 
#' This function pastes creates the JSON url for the provided game id (first 
#' checking that it is from the 2009 season or later). The returned url is used
#' in the various play-by-play functions to access data from the NFL's API.
#' 
#' @param game_id (character or numeric) A 10 digit game id associated with a 
#' given NFL game.
#' @return String url where the game JSON data for the given game can be found.
#' @examples
#' # Create the JSON url for the first game of the 2017 season:
#' create_game_json_url(2017090700) 
#' @export

create_game_json_url <- function(game_id) {
  
  # First check to make sure that the first six digits of the game id are at
  # least 200904 (meaning they're at least from the 2009 season and have data
  # from the NFL API):
  assertthat::assert_that(as.numeric(stringr::str_sub(as.character(game_id), 1, 6)) > 200904,
                          msg = "You entered an invalid game id! JSON urls are supported starting with the 2009 season.")
  
  # Paste together the proper location of the JSON data
  paste0("http://www.nfl.com/liveupdate/game-center/", game_id, "/",
         game_id, "_gtd.json")
}


#' Create the url with the location of raw NFL play-by-play HTML
#' 
#' This function pastes creates the url for the provided game id. The returned 
#' url is used in the various play-by-play functions to access play-by-play data
#' from the raw HTML page.
#' 
#' @param game_id (character or numeric) A 10 digit game id associated with a 
#' given NFL game.
#' @return String url where the raw HTML play-by-play data can be found (going 
#' back to 1998 for regular season, 2000 for pre season).
#' @examples
#' # Create the JSON url for the first game of the 1998 season:
#' create_game_html_url(2017090700) 
#' @export

create_game_html_url <- function(game_id) {
  
  # First check to make sure that the first six digits of the game id are at
  # least 199804 (meaning they're at least from the 1998 season and have raw
  # HTML play-by-play:
  assertthat::assert_that(as.numeric(stringr::str_sub(as.character(game_id), 1, 6)) > 199804,
                          msg = "You entered an invalid game id! Raw HTML urls are supported starting with the 1998 season.")
  
  # Paste together the proper location of the raw HTML play-by-play data
  paste0("http://www.nfl.com/widget/gc/2011/tabs/cat-post-playbyplay?gameId=", 
         game_id, "&enableNGS=false")
}


