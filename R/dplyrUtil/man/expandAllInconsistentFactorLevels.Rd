\name{expandAllInconsistentFactorLevels}
\alias{expandAllInconsistentFactorLevels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
expandAllInconsistentFactorLevels(..., .noWarningCols = character(0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{.noWarningCols}{
%%     ~~Describe \code{.noWarningCols} here~~
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
structure(function (..., .noWarningCols = character(0)) 
{
    dots <- list(...)
    datasets <- if (length(dots) == 1) 
        dots[[1]]
    else dots
    colsToCheck <- intersect(names(datasets[[1]]), names(datasets[[2]]))
    isInconsistentFactor <- sapply(colsToCheck, function(col) {
        any(map_lgl(datasets, ~is.factor(.[[col]]))) && any(map_lgl(datasets, 
            ~!identical(levels(.[[col]]), levels(datasets[[1]][[col]]))))
    })
    colNamesInc <- colsToCheck[isInconsistentFactor]
    colNamesWarn <- setdiff(colNamesInc, .noWarningCols)
    if (length(colNamesWarn)) 
        warning("releveling factors ", paste(colNamesWarn, collapse = ","))
    for (col in colNamesInc) datasets <- expandFactorLevels(datasets, 
        col)
    datasets
  }, ex = function () 
{
    if (exists("expandAllInconsistentFactorLevels")) {
        df1 <- data.frame(f = factor(c("D", "D", "C")))
        df2 <- data.frame(f = factor(c("C", "C", "A")), desc = c("forC1", 
            "forC2", "forA1"))
        if (requireNamespace("dplyr")) 
            dplyr::bind_rows(expandAllInconsistentFactorLevels(df1, 
                df2))
        left_joinFactors(df1, df2)
    }
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
