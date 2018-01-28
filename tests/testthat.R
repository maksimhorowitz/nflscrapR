library(testthat)
library(nflscrapR)

# Testing why OT is not being scraped

OTgame <- game_play_by_play(2017092403)

OTgame09 <- game_play_by_play(2009100401)

games2017 <- season_games(2017)

bal_rost_17 <- season_rosters(2017, "BAL", "RUNNING_BACK")

data.frame(colnames(OTgame09),
           sapply(OTgame09, class)) %>%
  write.csv(file = "/Users/maksimhorowitz/Documents/PBP_Columns.csv")

urlstring <- proper_jsonurl_formatting(GameID)

nfl_api_raw <- RJSONIO::fromJSON(RCurl::getURL(urlstring))

names(nfl_api_raw[[1]]$home$stats)


data.frame(do.call(rbind,nfl_api_raw[[1]]$drives[[1]]$plays)) %>% View


data.frame(do.call(rbind,nfl_api_raw[[1]][[3]][[1]][["plays"]][[3]][["players"]][[2]])) %>% View

nfl_api_raw[[1]][[3]][[1]][["plays"]][[4]][["players"]] %>% names

