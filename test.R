euclidean <-function(x,y){
  if (x == 0){
    return(y)
  }
  return(euclidean(y %% x, x))
}

euclidean(123612, 13892347912)
euclidean(100, 1000)
