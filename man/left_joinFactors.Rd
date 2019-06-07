\name{left_joinFactors}
\alias{left_joinFactors}
\title{left joinFactors}
\description{left join with homogenizing factors before}
\usage{left_joinFactors(x, y, ..., fJoin = left_join, .noWarningCols = character(0))}
\arguments{
  \item{x}{left tbl to join}
  \item{y}{right tbl to join}
  \item{\dots}{further arguments to \code{\link{left_join}} or \code{fJoin}}
  \item{fJoin}{join function(x,y,...) to use}
  \item{.noWarningCols}{string vector: do not warn for the these
columns}
}

\value{results of \code{\link{left_join}} or given alternative join function}

\author{Thomas Wutzler}





