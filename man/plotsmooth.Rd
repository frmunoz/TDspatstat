\name{plotsmooth}
\alias{plotsmooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting the contributions of spatial predictors}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsmooth(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function (obj) 
{
    par(mar = c(3, 2, 1.5, 0.5))
    themax <- which.max(obj$obs)
    thecol <- rep("grey80", 7)
    thecol[themax] <- "grey40"
    debut <- seq(1, by = 7, length = 7)
    fin <- seq(7, by = 7, length = 7)
    mp <- barplot(obj$obs, col = thecol, ylim = c(0, 1), axes = FALSE)
    axis(2, at = c(0, 1), las = 1, tcl = -0.3)
    axis(1, at = mp[, 1], label = 1:7, tcl = 0, padj = -1)
    mtext(1, text = "Smoothed MEMs", padj = 3)
    mtext(2, text = expression(R^2), padj = -1)
    qu95 <- apply(obj$sim, 2, quantile, 0.95)
    points(mp[, 1], qu95, ty = "b", pch = 3, lty = 3)
    text(mp[themax], obj$obs[themax], obj$pvalue[themax], pos = 3)
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
