#' @title DraftTeam
#'
#' @description Takes the first row a \code{dataframe} and makes it the header.
#'
#' @name DraftTeam
#'
#' @details 'DraftTeam()' is used to extract the draft year and draft team from a player profile
#'
#' @param x a \code{dataframe}.
#'
#' @import tidyverse
#'
#'
#'
#' @return 'DraftTeam()' returns a \code{dataframe} with both the draft year and team that drafted a player.
#'
#' @export


library(tidyverse)

DraftTeam <- function(x){

  if(nrow(x) <= 2){


    return(x)

  }else if(nrow(x) == 3){

    x <- data.frame(Draft = c('Draft',x[2,1]), Draft.Team = c('Draft.Team',x[3,1]))

    return(x)

  }

}
