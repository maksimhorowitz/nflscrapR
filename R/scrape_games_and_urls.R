#' Scrape game ids for a given NFL season (either pre, regular, or post-season) 
#' 
#' This function returns all available game ids for a given season,
#' with an option specifying which type of game ids to return (either the 
#' pre, regular, or post season) as well as the weeks considered.
#' 
#' @param season Numeric 4-digit year associated with an NFL season
#' @param type String indicating the type of game ids to find, must either be
#' "pre", "reg", or "post" (default is "reg").
#' @param weeks Numeric vector indicating which weeks of preseason (0 to 4 except
#' for 2011 season without Hall of Fame game) or regular season games (1 to 17)
#' to grab (default value of NULL corresponds to selecting all available weeks).
#' @param teams String vector indicating which teams (based on the abbreviation)
#' the function should grab game info for (default value of NULL corresponds to
#' selecting all available teams).
#' @return Data frame with columns containing the season type, game_id, week number (if
#' pre or regular season), the year of the season, home team, away team, a variable
#' indicating the state of the game (POST is over, PRE is before or during),
#' game's url, and the home and away team scores if the game is over.
#' @examples
#' # Scraping all game ids from 2017 regular season:
#' scrape_game_ids(2017) 
#' @export

scrape_game_ids <- function(season, type = "reg", weeks = NULL, teams = NULL) {
  
  # First check that the type is one of the required options:
  assertthat::assert_that(tolower(type) %in% c("reg", "pre", "post"),
                          msg = "Input for type is not valid! Should either be 'reg', 'pre', or 'post'.")
  
  # Next check that if the type is pre then the season is at least 1999:
  if (tolower(type) == "pre") {
    assertthat::assert_that(as.numeric(season) >= 2000,
                            msg = "Preseason game ids with data are only available starting in 2000!")
    # Otherwise check to see that it's at least 1998
  } else {
    assertthat::assert_that(as.numeric(season) >= 1998,
                            msg = "Regular and post-season game ids with data are only available starting in 1998!")
  }
  
  # Change the weeks from NULL if type is either pre or reg to their default values
  # (catching the case for 2011)
  
  if (is.null(weeks) & tolower(type) == "pre" & season != 2011) {
    weeks <- 0:4
  }
  if (is.null(weeks) & tolower(type) == "pre" & season == 2011) {
    weeks <- 1:4
  }
  if (is.null(weeks) & tolower(type) == "reg") {
    weeks <- 1:17
  }
  
  # Print out a message if the user entired values for weeks but for post type:
  if (!is.null(weeks) & tolower(type) == "post") {
    print("Ignoring the weeks input given the selected post-season game type.")
  }
  
  # Next check to see that if the type is pre then all of the weeks are between
  # 0 to 4, or 1 to 4 if season is 2011:
  if (tolower(type) == "pre" & season != 2011) {
    assertthat::assert_that(all(weeks >= 0) & all(weeks <= 4),
                            msg = "Please enter appropriate values for the pre-season weeks input between 0 to 4!")
  }
  if (tolower(type) == "pre" & season == 2011) {
    assertthat::assert_that(all(weeks >= 1) & all(weeks <= 4),
                            msg = "Please enter appropriate values for the 2011 pre-season weeks input between 1 to 4!")
  }
  
  # For regular season between 1 and 17:
  if (tolower(type) == "reg") {
    assertthat::assert_that(all(weeks >= 1) & all(weeks <= 17),
                            msg = "Please enter appropriate values for the regular season weeks input between 1 to 17!")
  }
  
  # Construct base schedule url for the season and type:
  base_url_schedule <- paste("http://www.nfl.com/schedules", season,
                             toupper(type), sep = "/")

  # Define the pipeline that will be used for type of games to scrape:
  
  fetch_game_ids <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-gameid=\"[0-9]{10}\"") %>%
    unlist() %>%
    stringr::str_extract("[0-9]{10}") %>%
    unlist()
  
  # Pipeline to get away team abbreviations:
  fetch_away_team_id <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-away-abbr=\"[:upper:]{2,3}\"") %>%
    unlist() %>%
    stringr::str_extract("[:upper:]{2,3}") %>%
    unlist()
  
  # Home team abbreviations:
  fetch_home_team_id <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-home-abbr=\"[:upper:]{2,3}\"") %>%
    unlist() %>%
    stringr::str_extract("[:upper:]{2,3}") %>%
    unlist()
  
  # Pipeline to fetch state of game:
  fetch_gamestate <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-gamestate=\"[:upper:]{2,4}\"") %>%
    unlist() %>%
    stringr::str_extract("[:upper:]{2,4}") %>%
    unlist()
  
  
  # If the type is post then just gather all the game ids from the post season
  # schedule page (since they are not separated by the week unlike the pre or
  # regular season) and put them together in data frame with week as 18 for now:
  
  if (tolower(type) == "post") {
    
    playoff_game_ids <- base_url_schedule %>%
      fetch_game_ids
    
    playoff_game_home <- base_url_schedule %>%
      fetch_home_team_id
    playoff_game_away <- base_url_schedule %>%
      fetch_away_team_id
    playoff_gamestate <- base_url_schedule %>%
      fetch_gamestate
    
    game_ids_df <- data.frame("game_id" = playoff_game_ids,
                              "week" = rep(18, length(playoff_game_ids)),
                              "home_team" = playoff_game_home,
                              "away_team" = playoff_game_away,
                              "state_of_game" = playoff_gamestate)
    
    # Else need to do it by the given weeks for the pre and regular season
  } else {

        # Now apply the pipeline to each week returning a data frame of game ids with
    # a column containing the week
    game_ids_df <- suppressWarnings(purrr::map_dfr(weeks, function(x) {
      game_ids <- paste0(base_url_schedule, x) %>%
        fetch_game_ids
      game_home <- paste0(base_url_schedule, x) %>%
        fetch_home_team_id
      game_away <- paste0(base_url_schedule, x) %>%
        fetch_away_team_id
      gamestate <- paste0(base_url_schedule, x) %>%
        fetch_gamestate
      data.frame("game_id" = game_ids,
                 "week" = rep(x, length(game_ids)),
                 "home_team" = game_home,
                 "away_team" = game_away,
                 "state_of_game" = gamestate)
    }))
  }
  
  # If teams is not null, check to make sure that there are games with
  # the given teams as either home or away and filter down to only
  # include those:
  if (!is.null(teams)) {
    assertthat::assert_that(any(teams %in% game_ids_df$home_team |
                                  teams %in% game_ids_df$away_team),
                            msg = "There are no games available for your entered team(s)!")
    game_ids_df <- game_ids_df %>%
      dplyr::filter(home_team %in% teams | away_team %in% teams)
  }
  
  
  # Return the game ids in a data frame with columns for the season and type:
  game_ids_df %>%
    dplyr::mutate(type = rep(type, nrow(game_ids_df)),
                  season = rep(season, nrow(game_ids_df))) %>%
    dplyr::select(type, game_id, home_team, away_team, week, season, state_of_game) %>%
    as.data.frame() %>%
    # Due to how the NFL displays Thursday Night Football games, only use 
    # the distinct rows:
    dplyr::distinct() %>%
    # Next create a variable with the url for each game:
    dplyr::mutate(game_url = sapply(game_id, create_game_json_url),
                  # Now for each game, if it is over based on the
                  # state_of_game field then access the scores of the
                  # game for the home and away teams:
                  home_score = purrr::map2_dbl(game_url, state_of_game,
                                               .f = function(x, y) {
                                                 ifelse(y == "POST",
                                                        max(RJSONIO::fromJSON(RCurl::getURL(x, encoding = "gzip"))[[1]]$home$score),
                                                        NA)
                                                 }),
                  away_score = purrr::map2_dbl(game_url, state_of_game,
                                               .f = function(x, y) {
                                                 ifelse(y == "POST",
                                                        max(RJSONIO::fromJSON(RCurl::getURL(x, encoding = "gzip"))[[1]]$away$score),
                                                        NA)
                                                 })) %>%
    return
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


