\name{mapGroups}
\alias{mapGroups}
\title{mapGroups}
\description{split-map-combine}
\usage{mapGroups(data, ..., drop = TRUE, .isFactorReleveled = FALSE, 
    .omitWarning = FALSE)}
\arguments{
  \item{data}{groped data.frame}
  \item{\dots}{function or formula passed to \code{\link{map}}
that receives a data.frame as first argument}
  \item{drop}{logical indicating if levels that do not occur should
be dropped. Set to FALSE if FUN returns a data.frame also
for zero-row inputs.}
  \item{.isFactorReleveled}{
}
  \item{.omitWarning}{
}
}
\details{https://stackoverflow.com/questions/53855897/accessing-grouping-variables-in-purrrmap-with-nested-dataframes
https://coolbutuseless.bitbucket.io/2018/03/03/split-apply-combine-my-search-for-a-replacement-for-group_by---do/
if you create factors inside map_df, one can use map_dfrFactor.}


\author{Thomas Wutzler}





