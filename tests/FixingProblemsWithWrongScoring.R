# Issue Games:

g1 <- game_play_by_play(2014122804)
nrow(g1)

n1 <- Next_Score(g1)
length(n1)


unlist(stringr::str_extract_all(g1$desc[37], pattern = "TOUCHDOWN"))



which(g1$TimeSecs == 2453)


### Second Game

# DONE
g2 <- game_play_by_play(2010112107)
nrow(g2)

n2 <- Next_Score(g2)
length(n2)

g2$desc[which(g2$desc == "*** play under review ***")]

sapply(g2$desc, stringr::str_extract, 
       pattern = "TOUCHDOWN")

t1 <- which(g2$Touchdown == 1)
t1.a <- which(g2$PlayType == "Extra Point" | !is.na(g2$TwoPointConv)) - 1
  
setdiff(t1, t1)

       g2[dplyr::lead(which(g2$PlayType == "Extra Point")),]

touchdown.step1 <- sapply(PBP$desc, stringr::str_extract, 
                          pattern = "TOUCHDOWN|\* play under review \\*")
nullified <- grep(PBP$desc, pattern = "TOUCHDOWN NULLIFIED")
reversed <- grep(PBP$desc, pattern = "TOUCHDOWN(.)+REVERSED")
touchdown.step1[c(nullified, reversed)] <- NA
PBP$Touchdown <- ifelse(!is.na(touchdown.step1), 1, 0)

# Game 3 

g3 <- game_play_by_play(2011121101)
nrow(g3)

n3 <- Next_Score(g3)
length(n3)


