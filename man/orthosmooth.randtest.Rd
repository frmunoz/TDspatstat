\name{orthosmooth.randtest}
\alias{orthosmooth.randtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Randomisation test of the contributions of spatial predictors
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthosmooth.randtest(x, MEM, nrepet = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{MEM}{
%%     ~~Describe \code{MEM} here~~
}
  \item{nrepet}{
%%     ~~Describe \code{nrepet} here~~
}
    \item{by}{
%%     ~~Describe \code{by} here~~
}
    \item{length}{
%%     ~~Describe \code{length} here~~
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
function (x, MEM, nrepet = 999) 
{
    R2 <- as.vector(cor(x, MEM)^2)
    debut <- seq(1, by = 7, length = 7)
    fin <- seq(7, by = 7, length = 7)
    R2.smooth <- colSums(sapply(1:7, function(x) R2[debut[x]:fin[x]]))
    sim <- matrix(0, nrepet, 7)
    for (i in 1:nrepet) {
        R2.sim <- as.vector(cor(sample(x), MEM)^2)
        sim[i, ] <- colSums(sapply(1:7, function(x) R2.sim[debut[x]:fin[x]]))
    }
    return(list(obs = R2.smooth, sim = sim))
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
