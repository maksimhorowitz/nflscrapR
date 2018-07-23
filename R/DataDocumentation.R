################################################################## 
### Documenting the data files                                 ###
# Author: Maksim Horowitz                                        #
# Code Style Guide: Google R Format                              #
################################################################## 


#' Dataset of NFL team names, abbreviations, and colors
#'
#' @docType data
#' @format A data frame with one row for each team/abbreviation combo that appears
#' in the NFL play-by-play data with the following columns accessed from the 
#' `teamcolors` package created by Ben Baumer and Gregory Matthews:
#' \describe{
#'  \item{team}{full name of the team}
#'  \item{abbr}{team abbreviation used in play-by-play data}
#'  \item{primary}{team's primary color}
#'  \item{secondary}{team's secondary color}
#'  \item{tertiary}{team's tertiary color}
#'  \item{quaternary}{team's quaternary color}
#'  \item{division}{team's division}
#' }
#'
#' @details Team names, abbreviations, divisions, and team colors.
#'
#' @examples
#' data(nflteams)
#'
"nflteams"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2009 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats09"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2010 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats10"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2011 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats11"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2012 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats12"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2013 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats13"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2014 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats14"

#' NFL Team Names and Abbreviations
#' @format Outputs of the season_playergame() function for the 2015 season.  
#' This datset used in the vingette for nflscrapR.
"playerstats15"
