# Author: Ron Yurko
# Purpose: Scripts for scraping team rosters and player IDs from NFL.com
# This code is adapted from the original functions from GameandRosterFunctions.R

#' Get team roster for a single season
#' 
#' Given a year and team abbreviation, return a dataset with each
#' player who has played the specified team and has recorded a measurable statistic.
#' 
#' @param season A 4-digit year associated with a given NFL season
#' @param teams A string vector containing the abbreviations for NFL Team(s)
#' @param type String indicating the type of game, must either be: "pre", 
#' "reg", or "post".
#' @param positions A string vector containing the abbreviations for NFL position(s).
#' Can be any of the following:
#' \itemize{
#' \item{"QUARTERBACK"} (in the default list)
#' \item{"RUNNING_BACK"} (in the default list)
#' \item{"WIDE_RECEIVER"} (in the default list)
#' \item{"TIGHT_END"} (in the default list)
#' \item{"DEFENSIVE_LINEMAN"} 
#' \item{"LINEBACKER"}
#' \item{"DEFENSIVE_BACK"}
#' \item{"KICKOFF_KICKER"}
#' \item{"KICK_RETURNER"}
#' \item{"PUNTER"}
#' \item{"PUNT_RETURNER"}
#' \item{"FIELD_GOAL_KICKER"}
#' }
#' @details To find team associated abbrevations use the \code{\link{nflteams}} dataset 
#' stored in this package!
#' @return A dataset with columns associated with season/year, full player name,
#' team initial, position, and formated player name.
#' @examples
#' # Roster for Steelers in 2018
#' get_season_rosters(2018, teams = "PIT") 
#' @export
get_season_rosters <- function(season, teams, type = "reg",
                               positions = c("QUARTERBACK", "RUNNING_BACK", "WIDE_RECEIVER", 
                                             "TIGHT_END")) {
  
  # First check that the type is one of the required options:
  assertthat::assert_that(tolower(type) %in% c("reg", "pre", "post"),
                          msg = "Input for type is not valid! Should either be 'reg', 'pre', or 'post'.")
  
  
  # Find the players with the given positions and season filtering to the 
  # provided teams:
  positions %>% 
    purrr::map_df(function(position) {
      get_players(position, season, type) 
      }) %>%
    dplyr::filter(Team %in% teams) %>% 
    dplyr::group_by(Player, Team, Pos, GSIS_ID, birthdate) %>% 
    dplyr::slice(n= 1) %>% 
    dplyr::mutate(Season = season,
                  season_type = type) %>% 
    dplyr::select(Season, season_type, Player, name, Team, Pos, GSIS_ID, birthdate) %>%
    dplyr::rename(season = Season,
                  full_player_name = Player,
                  abbr_player_name = name,
                  team = Team,
                  position = Pos,
                  gsis_id = GSIS_ID) %>%
    return()
  
}


################################################################## 
# DO NOT EXPORT
#' Build URL to scrape player season stat pages
#' 
#' This is a sub-function for the get_season_rosters function.
#' 
#' @param position (character string) Specifies a player position page for the URL
#' @param season 4-digit year associated with a given NFL season
#' @param page 1-digit page number to look into
#' @param type A three character string specifying the season type: pre, post, or reg
build_url <- function(position, season, page = 1, type) {
  
  type <- toupper(type)
  
  # season, type, page, position
  base_string <- 'http://www.nfl.com/stats/categorystats?tabSeq=1&season=%s&seasonType=%s&d-447263-p=%s&conference=null&statisticPositionCategory=%s'
  return(sprintf(base_string, season, type, page, position))
  
}

# DO NOT EXPORT
#' Get number of player position pages 
#' 
#' @description For each position, this function extracts the number of pages 
#' there are to scrape. This is a sub-function for the get_season_rosters function
get_page_numbers <- . %>% 
  # get list of pages if it exists
  rvest::html_node('.linkNavigation') %>% 
  # extract text
  rvest::html_text() %>%
  # break it up by |
  stringr::str_split('|') %>%
  # this gives a list, get the first element
  magrittr::extract2(1) %>% 
  # keep just numbers
  stringr::str_extract('\\d+') %>% 
  # convert to integer
  as.integer() %>% 
  # replace NAs with 1
  replace(., is.na(.), 1) %>%
  # find unique and sort
  unique %>% sort

# DO NOT EXPORT
#' Build formatted player name from full player name
#' 
#' This sub-function, called in the get_season_rosters function,
#' takes the full name of each player and formats it into the first initial of 
#' their first name and last initial of their last name.
build_name_abbr <- . %>% 
  # get the result table node
  rvest::html_node('#result') %>% 
  # extract the table
  rvest::html_table() %>% 
  # get columns 2, 3, 4
  magrittr::extract(2:4) %>% 
  # make sure names are what we want
  setNames(nm = c('Player', 'Team', 'Pos')) %>% 
  # get rid of a row if the player is player
  dplyr::filter(Player != 'Player') %>% 
  # get the first initial and last name
  dplyr::mutate(First = stringr::str_sub(Player, 1, 1),
                Last = stringr::str_extract(Player, ' [^ ]+$')) %>% 
  # remove space before last name
  dplyr::mutate(Last = stringr::str_trim(Last)) %>% 
  # combine them into one column
  tidyr::unite(name, First, Last, sep='.', remove=TRUE)

# DO NOT EXPORT
#' Find the GSIS ID for each player on the provided page.
find_page_player_id <- . %>%
  rvest::html_nodes("td:nth-child(2) a") %>% 
  rvest::html_attr("href") %>%
  purrr::map_chr(get_gsis_id)

# DO NOT EXPORT
#' For a player's href, get their GSIS ID from their personal url.
get_gsis_id <- . %>%
  paste("http://www.nfl.com", ., sep = "") %>%
  readLines(n = 1000) %>%
  grep("GSIS ID", ., value = TRUE) %>%
  substr(., nchar(.) - 9, nchar(.)) %>%
  as.character()

# DO NOT EXPORT
#' For a player's href, get their birthdate from their personal url.
get_birthdate <- . %>%
  paste("http://www.nfl.com", ., sep = "") %>%
  rvest::html_nodes('#player-bio p') %>%
  rvest::html_text() %>%
  .[grep("Born", .)] %>%
  stringr::str_extract("[0-9]+/[0-9]+/[0-9]+") %>% 
  lubridate::mdy() %>% 
  as.character()


# DO NOT EXPORT
#' Find the birthdate for each player on the provided page.
find_page_player_birthdate <- . %>%
  rvest::html_nodes("td:nth-child(2) a") %>% 
  rvest::html_attr("href") %>%
  purrr::map_chr(get_birthdate)


# DO NOT EXPORT
#' Scrape player names and positions
#' 
#' This sub-function, calls build_name_abbr and get_page_numbers to
#' scrape player positions by season.
get_players <- function(position, season, type) {
  # Give position name
  message(sprintf('Extracting %s', position))
  
  ## get first page
  first_url <- build_url(position = position, season = season, page=1, type=type)
  first_page <- xml2::read_html(first_url)
  
  # get number of pages
  page_seq <- get_page_numbers(first_page)
  
  # build urls
  page_urls <- build_url(position = position,
                         season = season, page = page_seq, type = type) %>%
               purrr::map(function(x) xml2::read_html(x))
  
  # Extract the player IDs
  player_ids <- page_urls %>%
    purrr::map(find_page_player_id) %>%
    purrr::flatten_chr()
  
   # Extract the player IDs
  player_birthdates <- page_urls %>%
    purrr::map(find_page_player_birthdate) %>%
    purrr::flatten_chr()
  
                      
  # read the pages and extract info, then add the ids:
  page_urls %>%
    # get the name and position, combine everything into a data.frame
    purrr::map_df(build_name_abbr) %>%
    dplyr::mutate(GSIS_ID = player_ids,
                  birthdate = player_birthdates)
  
}

