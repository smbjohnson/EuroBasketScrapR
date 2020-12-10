#' @title EuroBasketScrapR
#'
#' @description Takes input and returns a a \code{dataframe} with the desired data.
#'
#' @param Season List of two year ranges for season
#' @param Country Character of countires
#' @param League Character of leagues
#' @param Tournament Character of tournament
#' @param Team Character of team
#' @param PlayerLastName Character last name of player
#' @param PlayerFirstName Character first name of player
#' @param Position Character position
#' @import tidyverse
#' @return 'EuroBasketScrapR()' returns a \code{dataframe} containing the desired data from EuroBasket.com
#' @export
#'
#' @examples
#' EuroBasketScrap(Position = "Point Guard")
#'

library(tidyverse)

EuroBasketScrapR <- function(Season = unique(EuroBasketData$SEASON),
                             Country = unique(EuroBasketData$COUNTRY),
                             League = unique(EuroBasketData$LEAGUE),
                             Tournament = unique(EuroBasketData$TOURNAMENT),
                             Team = unique(EuroBasketData$TEAM.x),
                             PlayerLastName = unique(EuroBasketData$LAST.x),
                             PlayerFirstName = unique(EuroBasketData$FIRST.x),
                             Position = unique(EuroBasketData$POSITION)){


 x <-  EuroBasketData %>%
    filter(COUNTRY %in% c(Country) &
           LEAGUE %in% c(League) &
           TOURNAMENT %in% c(Tournament) &
           TEAM.x %in% c(Team) &
           LAST.x %in% c(PlayerLastName) &
           FIRST.x %in% c(PlayerFirstName) &
           POSITION %in% c(Position) &
           SEASON %in% c(Season)
          )

 return(x)

}



