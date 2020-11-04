\name{map_dfrFactor}
\alias{map_dfrFactor}
\title{map dfrFactor}
\description{variant of map_dfr that releveles unequal factor levels before binding}
\usage{map_dfrFactor(.x, ..., .id = NULL, .isFactorReleveled = TRUE, 
    .noWarningCols = character(0), .omitWarning = FALSE)}
\arguments{
  \item{.x}{list to map over}
  \item{\dots}{further arguments to \code{purrr::map}}
  \item{.id}{argument to \code{\link{bind_rows}}}
  \item{.isFactorReleveled}{set to FALSE to avoid releveling}
  \item{.noWarningCols}{argument
to \code{\link{expandAllInconsistentFactorLevels}}}
  \item{.omitWarning}{
}
}



\author{Thomas Wutzler}





