\name{mutate_cond}
\alias{mutate_cond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mutate_cond(.data, condition, ..., envir = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.data}{
%%     ~~Describe \code{.data} here~~
}
  \item{condition}{
%%     ~~Describe \code{condition} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{envir}{
%%     ~~Describe \code{envir} here~~
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
structure(function (.data, condition, ..., envir = parent.frame()) 
{
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] \%>\% mutate(...)
    .data
  }, ex = function () 
{
    if (require(dplyr)) {
        ans <- iris \%>\% mutate_cond(Species == "setosa", Petal.Length = 1)
        ans \%>\% filter(Species == "setosa") \%>\% select(Petal.Length) \%>\% 
            head()
        ans \%>\% filter(Species != "setosa") \%>\% select(Petal.Length) \%>\% 
            head()
    }
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
