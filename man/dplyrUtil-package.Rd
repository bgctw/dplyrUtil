\name{dplyrUtil-package}

\alias{dplyrUtil-package}
\alias{dplyrUtil}

\title{Utilities for using dplyr.}

\description{
Some longer description.
}

\details{

Common tasks when using dplyr are
\itemize{
\item{ split-map-combine: \code{\link{mapGroups}}: shorthand for
  \code{split(group_indices(.)) \%>\% map_dfr(...)} }
\item{ mutate acting only on the rows satisfying the condition:
  \code{\link{mutate_cond}}  }
\item{ left_join with before dropping columns that would be duplicated:
    \code{\link{left_joinReplace}}  }
}%itemize

Dealing with unequal factor levels:

In case of unequal factor levels in row-binding tbls or joins,
factors are converted to character.
This package provides function to relevel factors in tbls before
those operations.

\itemize{
\item{ expand a factor variables in all dataset to encompass levels of all sets:
  \code{\link{expandAllInconsistentFactorLevels}}  }
\item{ join with homogenizing factors before: \code{\link{left_joinFactors}}  }
}%itemize

Also have a look at the \href{../doc/index.html}{package vignettes}.
}%details

\author{Thomas Wutzler}



\keyword{ package }





