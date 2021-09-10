#' Add together two numbers
#' 
#' @param graph A DataFrame.
#' @param init_node An Int.
#' @return Returns the shortest path to every other node from the starting node as a vector.
#' @references \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Wikipedia - Dijkstra's algorithm}
#' @examples
#' dijkstra(graph = wiki_graph,init_node = 1)


dijkstra <-function(graph, init_node) {
  col_names = c("index", "dist", "flag")
  df = data.frame(unique(graph$v1),Inf,0)
  colnames(df) = col_names
  
  df[df$index == init_node,]$dist = 0
  
  while(sum(df$flag) < length(df$flag)){
    min_dist = min(df[df$flag != 1,]$dist)
    min_index = df[df$dist == min_dist,]$index[1]
    
    conns = graph[graph$v1 == min_index,]
    
    for(i in conns$v2){
      if(df[df$index == i,]$dist > (df[df$index == min_index,]$dist + conns[conns$v2 == i,]$w)){
        df[df$index == i,]$dist = df[df$index == min_index,]$dist + conns[conns$v2 == i,]$w
      }
    }
    df[df$dist == min_dist,]$flag = 1
  }
  return(as.vector(df$dist))
}
