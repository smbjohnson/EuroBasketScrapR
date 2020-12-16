#' @title SeasonStatGrab
#'
#' @description Allows the user to input a url from Eurobasket and scrap the data from the page.
#'
#' @name SeasonStatGrab
#'
#' @details 'SeasonStatGrab' takes a url taken from one of the stats pages on Eurobasket.com and returns
#' a \code{dataframe} with the sraped data.
#'
#' @param URL a URL taken from one of the stats pages on EuroBasket.com
#'
#' @import rvest
#' @import plyr
#' @import tidyverse
#' @import data.table
#' @import stringr
#' @import lubridate
#'
#'
#'
#'
#' @return 'SeasonStatGrab()' returns a \code{dataframe} containing data from one of the EuroBasket.com stat pages
#'
#' @export
#'
#' @examples
#' SeasonStatGrab(URL = "https://basketball.eurobasket.com/team/Spain/BAXI-Manresa/318?Page=3")

library(rvest)
library(plyr)
library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)

SeasonStatGrab <- function(URL){

  TeamTest <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@class = "team-title"]') %>%
    html_text() %>%
    trimws()

  HeaderTest <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@class = "padding-left-10 social"]') %>%
    html_nodes("span") %>%
    html_text() %>%
    str_match(.,"\\((.*?)\\)") %>%
    .[1,2] %>%
    strsplit(.,"-")

  CountryTest <- HeaderTest[[1]][1]
  LeagueTest <- HeaderTest[[1]][2]

  StatsTest <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    html_table(fill=T) %>%
    lapply(.,function(x) setnames(x, toupper(names(x)))) %>%
    lapply(.,rename_rebound)


  PlayerIDTest <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    html_nodes(xpath = '//*[@class = "my_playerName"]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    strsplit(., "[=/]+") %>%
    sapply(., function(x)x[length(x)]) %>%
    as.numeric()


  SeasonTest <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id = "teamstatstbl2"]') %>%
    html_nodes("p") %>%
    html_nodes("b") %>%
    html_text() %>%
    .[!grepl("Team Summary",.)] %>%
    .[!grepl("Games List",.)] %>%
    gsub(" Players Stats","",.)  %>%
    gsub("Season: ", "", .) %>%
    unique(.) %>%
    strsplit(., "\\(") %>%
    lapply(., function(x)gsub(")", "",x)) %>%
    lapply(.,trimws)


  RecordTest <- URL %>%
    read_html() %>%
    html_nodes(xpath = '//*[@style = "height:auto;"]') %>%
    html_table(fill = TRUE) %>%
    .[which(sapply(.,ncol) == 6)] %>%
    data.frame(.) %>%
    CreateHeader() %>%
    split(., sort(as.numeric(rownames(.)))) %>%
    lapply(.,function(x) setnames(x, toupper(names(x))))


  if(length(RecordTest) > 0){

    for(i in 1:length(RecordTest)){


      StatsTest[[i]] <- cbind(StatsTest[[i]], RecordTest[[i]][,-1], row.names = NULL)


    }

  }

  for(i in 1:length(StatsTest)){


    StatsTest[[i]]$SEASON <- SeasonTest[[i]][1]

    StatsTest[[i]]$TOURNAMENT <- SeasonTest[[i]][2]

  }

  StatsFinal <- ldply(StatsTest, data.frame)

  StatsFinal$HREF <- URL

  StatsFinal$TEAM <- TeamTest

  StatsFinal$LEAGUE <-  LeagueTest

  StatsFinal$COUNTRY <- CountryTest

  StatsFinal$PLAYER.ID <- PlayerIDTest

  #This was used to seperate the names but is not important anymore
  # StatsFinal <- separate(StatsFinal, "NAME",into = c("LAST", "FIRST"),sep = "[, ]+")
  #
  # StatsFinal$LAST <- toupper(trimws(gsub("[^[:alnum:]]","", StatsFinal$LAST)))
  #
  # StatsFinal$FIRST <- toupper(trimws(gsub("[^[:alnum:]]","", StatsFinal$FIRST)))

  return(StatsFinal)
}

