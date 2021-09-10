euclidean <-function(x,y){
  if (x == 0){
    return(y)
  }
  return(euclidean(y %% x, x))
}
