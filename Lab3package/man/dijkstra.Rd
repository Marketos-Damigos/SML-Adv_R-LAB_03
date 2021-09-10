\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dijkstra
%%  ~~function to do ... ~~
}
\description{
fdsfsdgfsda
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    col_names = c("index", "dist", "flag")
    df = data.frame(unique(graph$v1), Inf, 0)
    colnames(df) = col_names
    df[df$index == init_node, ]$dist = 0
    while (sum(df$flag) < length(df$flag)) {
        min_dist = min(df[df$flag != 1, ]$dist)
        min_index = df[df$dist == min_dist, ]$index[1]
        conns = graph[graph$v1 == min_index, ]
        for (i in conns$v2) {
            if (df[df$index == i, ]$dist > (df[df$index == min_index, 
                ]$dist + conns[conns$v2 == i, ]$w)) {
                df[df$index == i, ]$dist = df[df$index == min_index, 
                  ]$dist + conns[conns$v2 == i, ]$w
            }
        }
        df[df$dist == min_dist, ]$flag = 1
    }
    return(as.vector(df$dist))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
