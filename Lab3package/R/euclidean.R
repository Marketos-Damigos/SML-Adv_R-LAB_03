#'  Eucledian Algorithm
#' @author Marketos Damgios, Chrystoforos Spyretos
#' @description Implemantation of Eucledian Algorithm in R.
#' @usage euclidean(x, y)
#' @param x as numeric
#' @param y as numeric
#'
#' @return  Greatest common divisor of x and y .
#' @references \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Wikipedia - Eucledian algorithm}
#' @export euclidean
#'
#' @examples 
#' euclidean(123612, 13892347912)
#' euclidean(100,10000)



euclidean <-
function(x,y){
  stopifnot(is.numeric(x) | is.numeric(y))
  x = abs(x)
  y = abs(y)
  if (x == 0){
    return(y)
  }
  return(euclidean(y %% x, x))
}
