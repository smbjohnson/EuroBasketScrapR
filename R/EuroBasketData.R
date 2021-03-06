#' @title EuroBasketData
#'
#' @description A \code{dataframe} that contains all of the data found between the 2015 and 2020 season on EuroBasket.com
#'
#' @format A \code{dataframe} with 62 collumns which are:
#' \describe{
#' \item{JER}{the jersey number of the player at the time}
#' \item{NAME}{The name of the player Last, First}
#' \item{G}{Number of games played by a player in that tournament in that season}
#' \item{MIN}{Average number of minuites played by a play in that tournament in that season}
#' \item{FGM.A}{Number of field goals attempted and made by a player in that tournament in that season}
#' \item{FG.PCT}{Field goal percentage of a player in that tournmanet in that season}
#' \item{X3PM.A}{Number of three point shots attempted and made by a player in that tournament in that season}
#' \item{X3PT.PCT}{Three point percentage of a player in that tournament in that season}
#' \item{FTM.A}{Number of free throws attempted and made by a player in that tournament in that season}
#' \item{FT.PCT}{Free throw percentage by a player in that tournament in that season}
#' \item{RO}{Average number of offensive rebounds by a player in that tournament in that season}
#' \item{RD}{Average number of defensive rebounds by a player in that tournament in that season}
#' \item{REB}{Average number of total rebounds by a player in that tournament in that season}
#' \item{AST}{Average number of assits by a player in that tournament in that season}
#' \item{PF}{Average number of personal fouls by a player in that tournament in that season}
#' \item{ST}{Average number of steals by a player in that tournament in that season}
#' \item{BS}{Average number of blocks by a player in that tournament in that season}
#' \item{TO}{Average number of turnovers by a player in that tournament in that season}
#' \item{PTS}{Average number of points by a player in that tournament in that season}
#' \item{RNK}{IDK}
#' \item{SEASON}{The season the data was taken from}
#' \item{TOURNAMENT}{The tournament the data was taken from}
#' \item{TOTAL.GAMES}{Total number of games played by that team in that tournament in that season}
#' \item{HOME.WON}{Total number of games won at home by that team in that tournament in that season}
#' \item{HOME.LOST}{Total number of games lost at home by that team in that tournament in that season}
#' \item{AWAY.WON}{Total number of games won on the road by that team in that tournament in that season}
#' \item{AWAY.LOST}{Total number of games loast on the road by that team in that tournament in that season}
#' \item{HREF}{The URL for the team stats}
#' \item{LEAGUE}{The league the team is apart of}
#' \item{COUNTRY}{The country the team is found in}
#' \item{CONTINENT}{The Continent the team is found in}
#' \item{PLAYER.ID}{The personal player id of a player}
#' \item{FIRST}{First name of the player all CAPS}
#' \item{MIDDLE}{Middle name of the player all CAPS}
#' \item{LAST}{Last name of the player all CAPS}
#' \item{POSITION}{The position played by a player}
#' \item{NATIONALITY}{The nationality of a player}
#' \item{HEIGHT.CM}{The height of a player in centimeters}
#' \item{HEIGHT.FT}{The height of a player in feet and inches}
#' \item{WEIGHT.KG}{The weight of a player in kilograms}
#' \item{WEIGHT.LBS}{The weight of a plyer in pounds}
#' \item{CURRENT.TEAM}{Team the player currenlty plays for}
#' \item{PAST.COUNTRIES}{The number of past countries a player has played in}
#' \item{NBA.DRAFT.ELIGIBLE}{The year a player would be eligible for the the NBA draft}
#' \item{COMMITTED}{Team the player is commited to}
#' \item{CLASS}{The class in school of a player}
#' \item{DRAFT}{The year the player was drafted in}
#' \item{DRAFT.TEAM}{The team the drafted the player}
#' \item{AGENT}{The agent that represents the player}
#' \item{BORN}{Birthday of the player}
#' \item{BIRTHPLACE}{Birthplace of the player}
#' \item{COLLEGE}{College the player attended}
#' \item{HIGH.SCHOOL}{High school the player attended}
#' \item{E.MAIL}{Email}
#' \item{PLAYER.HREF}{The URL for the player profile}
#' }
"EuroBasketData"
