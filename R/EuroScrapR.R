#' @title EuroScrapR
#'
#' @description Allows users to select data collected from EuroBasket.com
#'
#' @name EuroScrapR
#'
#' @details 'EuroScrapR()' takes inputs from the users to select only the desired data from Eurobasket.com.
#' Some of these inputs include Season, Country, League, Tournament, Team, Last Name of Player, First Name of Player, and Position.
#' All of these inputs are optional.
#'
#' @param Season List of two year ranges for season
#' @param Country Character of countries
#' @param League Character of leagues
#' @param Tournament Character of tournament
#' @param Team Character of team
#' @param PlayerLastName Character last name of player
#' @param PlayerFirstName Character first name of player
#' @param Position Character position
#'
#' @import tidyverse
#'
#' @importFrom magrittr %>%
#'
#'
#' @return 'EuroScrapR()' returns a \code{dataframe} containing the desired data from EuroBasket.com
#'
#' @export
#'
#' @examples
#' EuroScrapR(Position = "Point Guard")
#' EuroScrapR(Season = "2018-2019")
#' EuroScrapR(Season = c("2017-2018","2018-2019"))

library(tidyverse)


EuroScrapR <- function(Season = unique(EuroBasketData$SEASON),
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



