\name{expandFactorLevels}
\alias{expandFactorLevels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
expandFactorLevels(datasets, varName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasets}{
%%     ~~Describe \code{datasets} here~~
}
  \item{varName}{
%%     ~~Describe \code{varName} here~~
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
function (datasets, varName) 
{
    groupLevels <- lvls_union(lapply(datasets, "[[", varName))
    force(varName)
    ans <- map(datasets, function(dss) {
        mutate(dss, `:=`(!!varName, factor(!!sym(varName), levels = groupLevels)))
    })
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
