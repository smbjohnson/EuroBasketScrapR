#' A function that takes the first row of a dataframe and makes it the header
#' @param Season This is a dataframe
#' @param Country
#' @param League
#' @param Tournament
#' @param Team
#' @param PlayerLastName
#' @param PlayerFirstName
#' @param Position
#' @import tidyverse
#' @return 'EuroBasketScrapR()' returns a \code{dataframe} containing the desired data from EuroBasket.com
#' @export
#' @examples
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



