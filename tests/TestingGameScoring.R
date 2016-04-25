################### Testing Season Scoring ################### 

pbp2010 <- season_play_by_play(2010)

table(pbp2010$PlayType[which(pbp2010$sp ==1)])

pbp2010[which(pbp2010$sp ==1 & pbp2010$PlayType == "Timeout"), "desc"]

pbp2010[which(pbp2010$sp ==1 & pbp2010$PlayType == "Timeout"), "Passer"]

t <- which(sapply(pbp2010$desc[which(pbp2010$PlayType == "Timeout")], regexpr, 
             pattern = "(left|right) (tackle|end|guard) (for|to)") != -1)

pbp2010[which(pbp2010$PlayType == "Timeout"),"PlayType"][t]


pbp2010[which(pbp2010$sp ==1 & pbp2010$PlayType == "Sack"), c("desc", "sp")]


pbp2009 <- season_play_by_play(2009)

table(pbp2009$PlayType[which(pbp2009$sp ==1)])

pbp2009[which(pbp2009$sp ==1 & pbp2009$PlayType == "Kickoff"), "desc"]

pbp2011 <- season_play_by_play(2011)

table(pbp2011$PlayType[which(pbp2011$sp ==1)])

pbp2012 <- season_play_by_play(2012)

table(pbp2012$PlayType[which(pbp2012$sp ==1)])

pbp2013 <- season_play_by_play(2013)

table(pbp2013$PlayType[which(pbp2013$sp ==1)])

pbp2013[which(pbp2013$sp ==1 & pbp2013$PlayType == "No Play"), "desc"]

pbp2014 <- season_play_by_play(2014)

table(pbp2014$PlayType[which(pbp2014$sp ==1)])

pbp2015 <- season_play_by_play(2015)

table(pbp2015$PlayType[which(pbp2015$sp ==1)])


