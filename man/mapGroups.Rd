\name{mapGroups}
\alias{mapGroups}
\title{mapGroups}
\description{split-map-combine}
\usage{mapGroups(data, FUN, ..., drop = TRUE)}
\arguments{
  \item{data}{groped data.frame}
  \item{FUN}{function(data.frmae, ...) -> data.frame to apply to subsets}
  \item{\dots}{further arguments to FUN}
  \item{drop}{logical indicating if levels that do not occur should
be dropped. Set to FALSE if FUN returns a data.frame also 
for zero-row inputs.}
}



\author{Thomas Wutzler}





