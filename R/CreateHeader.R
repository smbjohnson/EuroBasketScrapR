#' @title CreateHeader
#'
#' @description Takes the first row a \code{dataframe} and makes it the header.
#'
#' @name CreateHeader
#'
#' @details 'CreateHeader()' takes the first row a \code{dataframe} and makes it the header.
#'
#' @param x a \code{dataframe}.
#'
#'
#'
#'
#' @return 'CreateHeader()' returns a \code{dataframe} where the first row is now the header
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = c("First", "Sam"), y = c("Height", 178))
#' CreateHeader(df)


CreateHeader <- function(x){

  names(x) <- as.matrix(x[1,])

  x <- x[-1,]

  return(x)

}

