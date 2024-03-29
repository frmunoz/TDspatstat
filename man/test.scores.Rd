\name{test.scores}
\alias{test.scores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to compute and test Moran's I for eigenvectors of spatial weighting matrices}
\description{
Function to compute and test Moran's I for eigenvectors of spatial weighting matrices.}
\usage{
test.scores(scores,listw,nsim=999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scores}{
%%     ~~Describe \code{Y} here~~
}
  \item{listw}{
%%     ~~Describe \code{X} here~~
}
  \item{nsim}{
%%     ~~Describe \code{K} here~~
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
function (Y, X, K = nrow(X) - 1, R2thresh = 0.99, adjR2thresh = 0.99, 
    nperm = 999, R2more = 0.001, alpha = 0.05, Xscale = TRUE, 
    Ycenter = TRUE, Yscale = FALSE) 
{
    X <- as.data.frame(X)
    Y <- as.data.frame(Y)
    if (any(is.na(X)) | any(is.na(X))) 
        stop("na entries in table")
    if (nrow(X) != nrow(Y)) 
        stop("different number of rows")
    if (any(apply(X, 2, is.factor)) | any(apply(Y, 2, is.factor))) 
        stop("not yet implemented for factors")
    X <- apply(X, 2, scale, scale = Xscale)
    Y <- apply(Y, 2, scale, scale = Yscale, center = Ycenter)
    nbcovar <- 0
    pval <- rep(1, ncol(X))
    ordre <- rep(0, ncol(X))
    R2 <- rep(0, ncol(X))
    adjR2 <- rep(0, ncol(X))
    Fvalue <- rep(0, ncol(X))
    res <- list()
    res <- .C("forwardsel", as.double(t(X)), as.double(t(Y)), 
        as.integer(nrow(X)), as.integer(ncol(X)), as.integer(ncol(Y)), 
        pval = as.double(pval), ord = as.integer(ordre), Fval = as.double(Fvalue), 
        as.integer(nperm), R2 = as.double(R2), adjR2 = as.double(adjR2), 
        as.integer(K), as.double(R2thresh), as.double(adjR2thresh), 
        as.double(R2more), as.integer(nbcovar), as.double(alpha), 
        PACKAGE = "packfor")[c("ord", "Fval", "pval", "R2", "adjR2")]
    lambdA <- c(res$R2[1], diff(res$R2))
    resmat <- data.frame(res$ord, lambdA, res$R2, res$adjR2, 
        res$Fval, res$pval)
    if (sum(res$ord > 0) == 0) 
        stop("No variables selected. Please change your parameters.")
    resmat <- resmat[res$ord > 0, ]
    resmat <- cbind(I(colnames(X)[resmat[, 1]]), resmat)
    names(resmat) <- c("variables", "order", "R2", "R2Cum", "AdjR2Cum", 
        "F", "pval")
    return(resmat)
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
