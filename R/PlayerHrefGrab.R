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
#' @import rvest
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


PlayerHrefGrab <- function(URL){

  PlayerID <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    html_nodes(xpath = '//*[@class = "my_playerName"]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    strsplit(., "[=/]+") %>%
    sapply(., function(x)x[length(x)]) %>%
    as.numeric()


  PlayerHREF <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    html_nodes(xpath = '//*[@class = "my_playerName"]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    data.frame()

  TeamHREF <- URL

  PlayerHREF$TEAM.HREF <- TeamHREF

  PlayerHREF$PLAYER.ID <- PlayerID

  return(PlayerHREF)

}

