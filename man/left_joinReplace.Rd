\name{left_joinReplace}
\alias{left_joinReplace}
\title{left joinReplace}
\description{\code{\link{left_join}} with before dropping columns that would be duplicated}
\usage{left_joinReplace(x, y, by, ...)}
\arguments{
  \item{x}{data.frames to join}
  \item{y}{
}
  \item{by}{here must be a character vector}
  \item{\dots}{further arguments to \code{\link{left_join}}}
}
\details{During joing existing columns are duplicated with a 
different name.
This function supports replacing the original columns instead,
allowing for repeated join of similar data.

If x is a list of 2 data.frames then these will be assigned to x and y}
\value{result of \code{\link{left_join}}}

\author{Thomas Wutzler}





