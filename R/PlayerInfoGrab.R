#' @title PlayerInfoGrab
#'
#' @description Takes a player profile URL from EuroBasket.com and returns a \code{dataframe} with the players info.
#'
#' @name PlayerInfoGrab
#'
#' @details 'PlayerInfoGrab()' Takes a player profile URL from EuroBasket.com and returns a \code{dataframe}
#'  with the players info.
#'
#'
#' @param df a \code{dataframe} with the players' profile URL in the first collumn with the team URL in the second collumn
#'  and the player id in the third collumn. The second and third collumn are optional but it must but the input must be a
#'  \code{dataframe}.
#'
#' @import xml2
#' @import plyr
#' @import tidyverse
#' @importFrom data.table setnames
#' @import stringr
#' @import devtools
#'
#'
#'
#'
#' @return 'PlayerInfoGrab()' returns a \code{dataframe} with the players' info.
#'
#' @export
#'
#' @examples
#' df <- data.frame("https://basketball.asia-basket.com/player/Jimmer-Fredette/China/Shanghai-Sharks/135233",
#'                   "https://basketball.asia-basket.com/team/China/Shanghai-Sharks/1950?Page=3",
#'                   "135233")
#'
#' PlayerInfoGrab(df)
#'
#' df1 <- data.frame("https://basketball.asia-basket.com/player/Jimmer-Fredette/China/Shanghai-Sharks/135233",
#'                   "a",
#'                   "a")
#'
#' PlayerInfoGrab(df1)


PlayerInfoGrab <- function(df){

  x <- df[1,1]
  # y <- df[1,2]
  z <- df[1,3]

  ##Player Name
  PlayerName <- x %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@class = "player-title"]') %>%
    xml2::xml_text() %>%
    gsub("basketball profile", "", .) %>%
    trimws() %>%
    strsplit(.," ")


  #Creates First  Last Name
  if(length(PlayerName[[1]]) == 2){

    NameFirst <-  gsub("[^[:alnum:]]", "",PlayerName[[1]][1])

    NameMiddle <- NA

    NameLast <- gsub("[^[:alnum:]]", "", PlayerName[[1]][2])

  }else if(length(PlayerName[[1]]) == 1){

    NameFirst <- gsub("[^[:alnum:]]", "",PlayerName[[1]][1])

    NameMiddle <- NA

    NameLast <- NA

  }else if(length(PlayerName[[1]]) == 3){

    NameFirst <- gsub("[^[:alnum:]]", "",PlayerName[[1]][1])

    NameMiddle <- gsub("[^[:alnum:]]", "", PlayerName[[1]][2])

    NameLast <- gsub("[^[:alnum:]]", "", PlayerName[[1]][3])
  }



  ##Player Info
  PlayerInfo <- x %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@class = "frunt-news players-info"]') %>%
    rvest::html_nodes("p") %>%
    xml2::xml_text() %>%
    unique() %>%
    strsplit(., ":") %>%
    lapply(., trimws) %>%
    lapply(., data.frame) %>%
    lapply(.,DraftTeam)  %>%
    .[which(sapply(., nrow) >= 1)] %>%
    dplyr::bind_cols() %>%
    CreateHeader()

  if("Height" %in% colnames(PlayerInfo)){

    PlayerInfo <- separate(PlayerInfo, c("Height"), into = c("Height.cm", "Height.ft"), sep = "/")

    PlayerInfo$Height.cm <- as.numeric(trimws(gsub("cm","",PlayerInfo$Height.cm)))

  }

  if("Weight" %in% colnames(PlayerInfo)){

    PlayerInfo <- separate(PlayerInfo, c("Weight"), into = c("Weight.kg", "Weight.lbs"), sep = "/")

    PlayerInfo$Weight.kg <- as.numeric(trimws(gsub("kg","", PlayerInfo$Weight.kg)))
    PlayerInfo$Weight.lbs <- as.numeric(trimws(gsub("lbs","", PlayerInfo$Weight.lbs)))

  }

  if("Born" %in% colnames(PlayerInfo)){

    PlayerInfo$Born <- lubridate::mdy(PlayerInfo$Born)

  }

  if("Agency" %in% colnames(PlayerInfo)){

    PlayerInfo$Agency <- trimws(gsub(" Change","",PlayerInfo$Agency))

  }

  if("Agent" %in% colnames(PlayerInfo)){

    PlayerInfo$Agency <- trimws(gsub(" Change","",PlayerInfo$Agent))

  }


  ##Number of Countries
  NumCtry <- x %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@class = "player-right"]') %>%
    rvest::html_nodes("p") %>%
    rvest::html_nodes("img") %>%
    xml2::xml_attr("alt") %>%
    gsub("-", " ",.)  %>%
    .[. %in% CountryNames] %>%
    unique(.) %>%
    length()


  PlayerInfo[1,'Past countries'] <- NumCtry
  PlayerInfo[1,'First'] <- NameFirst
  PlayerInfo[1,'Middle'] <- NameMiddle
  PlayerInfo[1,'Last'] <- NameLast
  PlayerInfo$PLAYER.HREF <- x
  #PlayerInfo$TEAM.HREF <- y
  PlayerInfo$PLAYER.ID <- z


  data.table::setnames(PlayerInfo, toupper(names(PlayerInfo)))

  return(PlayerInfo)

}





