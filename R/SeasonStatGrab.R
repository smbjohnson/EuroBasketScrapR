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
#' @import xml2
#' @import plyr
#' @import tidyverse
#' @importFrom data.table setnames
#' @import stringr
#' @import lubridate
#' @importFrom magrittr %>%
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


SeasonStatGrab <- function(URL){

  TeamTest <- URL %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@class = "team-title"]') %>%
    xml2::xml_text() %>%
    trimws()

  HeaderTest <- URL %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@class = "padding-left-10 social"]') %>%
    rvest::html_nodes("span") %>%
    xml2::xml_text() %>%
    str_match(.,"\\((.*?)\\)") %>%
    .[1,2] %>%
    strsplit(.,"-")

  CountryTest <- HeaderTest[[1]][1]
  LeagueTest <- HeaderTest[[1]][2]

  StatsTest <- URL %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    rvest::html_table(fill=T) %>%
    lapply(.,function(x) data.table::setnames(x, toupper(names(x)))) %>%
    lapply(.,rename_rebound)


  PlayerIDTest <- URL %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
    rvest::html_nodes(xpath = '//*[@class = "my_playerB"]') %>%
    # rvest::html_nodes("a") %>%
    xml2::xml_attr("href") %>%
    strsplit(., "[=/]+") %>%
    sapply(., function(x)x[length(x)]) %>%
    as.numeric()


  SeasonTest <- URL %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id = "teamstatstbl2"]') %>%
    rvest::html_nodes("p") %>%
    rvest::html_nodes("b") %>%
    xml2::xml_text() %>%
    .[!grepl("Team Summary",.)] %>%
    .[!grepl("Games List",.)] %>%
    gsub(" Players Stats","",.)  %>%
    gsub("Season: ", "", .) %>%
    unique(.) %>%
    strsplit(., "\\(") %>%
    lapply(., function(x)gsub(")", "",x)) %>%
    lapply(.,trimws)


  RecordTest <- URL %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@style = "height:auto;"]') %>%
    rvest::html_table(fill = TRUE) %>%
    .[which(sapply(.,ncol) == 6)] %>%
    data.frame(.) %>%
    CreateHeader() %>%
    split(., sort(as.numeric(rownames(.)))) %>%
    lapply(.,function(x) data.table::setnames(x, toupper(names(x))))


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

