#' @title PlayerHrefGrab
#'
#' @description Allows the user to input a stats URL from EuroBasket.com and collects all the player hrefs
#'  on the page.
#'
#' @name PlayerHrefGrab
#'
#' @details 'PlayerHrefGrab' takes a url taken from one of the stats pages on Eurobasket.com and returns
#' a \code{dataframe} with all the player hrefs, team hrefs, and player ids.
#'
#' @param URL a URL taken from one of the stats pages on EuroBasket.com
#'
#' @import xml2
#' @import tidyverse
#' @import plyr
#' @import data.table
#' @import stringr
#'
#'
#'
#'
#' @return 'PlayerHrefGrab()' returns a \code{dataframe} with the player's hrefs, team href, and player ids.
#'
#' @export
#'
#' @examples
#' PlayerHrefGrab(URL = "https://basketball.eurobasket.com/team/Spain/BAXI-Manresa/318?Page=3")

library(xml2)
library(plyr)
library(tidyverse)
library(data.table)
library(stringr)

PlayerHrefGrab <- function(URL){

  PlayerID <- URL %>%
    xml2::read_html() %>%
    xml2::html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    xml2::html_nodes(xpath = '//*[@class = "my_playerName"]') %>%
    xml2::html_nodes("a") %>%
    xml2::html_attr("href") %>%
    unique() %>%
    strsplit(., "[=/]+") %>%
    sapply(., function(x)x[length(x)]) %>%
    as.numeric()


  PlayerHREF <- URL %>%
    xml2::read_html() %>%
    xml2::html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    xml2::html_nodes(xpath = '//*[@class = "my_playerName"]') %>%
    xml2::html_nodes("a") %>%
    xml2::html_attr("href") %>%
    unique() %>%
    data.frame()

  TeamHREF <- URL

  PlayerHREF$TEAM.HREF <- TeamHREF

  PlayerHREF$PLAYER.ID <- PlayerID

  return(PlayerHREF)

}

