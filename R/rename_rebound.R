#' @title rename_rebound
#'
#' @description This is a function used in SeasonStatGrab() that changes the abbreviations of rebounds
#' from "OFF" to "RO", "DEF" to "RD", and "TOT" to "REB"
#'
#' @name rename_rebound
#'
#' @details 'rename_rebound' takes a \code{dataframe} while in the function SeasonStatGrab() and
#' changes the abbreviatons for offensive, defensive, and total rebounds to a uniform "RO", "RD", and "REB"
#'
#'
#' @param x a \code{dataframe} given while running the SeasonStatGrab() function
#'
#'
#'
#'
#' @return 'rename_rebound()' retuns a \code{dataframe} in which all the abbreviations for rebound have been
#' given a uniform name
#'
#' @export
#'
#' @examples
#' StatsTest <- x %>%
#' read_html() %>%
#' html_nodes(xpath = '//*[@id = "table_stats1"]') %>%
#' html_table(fill=T) %>%
#' lapply(.,function(x) setnames(x, toupper(names(x)))) %>%
#' lapply(.,rename_rebound)



rename_rebound <- function(x){

  headers1 <- c("OFF", "DEF", "TOT")
  if(sum(headers1 %in% colnames(x)) > 0){

    names(x)[names(x) == "OFF"]  <- "RO"
    names(x)[names(x) == "DEF"] <- "RD"
    names(x)[names(x) == "TOT"] <-  "REB"
    names(x)[1] <- "JER"
    names(x)[6] <-  "FG.PCT"
    names(x)[8] <- "3PT.PCT"
    names(x)[10] <- "FT.PCT"

    return(x)


  }else{

    names(x)[1] <- "JER"
    names(x)[6] <-  "FG.PCT"
    names(x)[8] <- "3PT.PCT"
    names(x)[10] <- "FT.PCT"

    return(x)
  }
}
