\name{mutate_cond}
\alias{mutate_cond}
\title{mutate cond}
\description{mutate acting only on the rows satisfying the condition}
\usage{mutate_cond(.data, condition, ..., envir = parent.frame())}
\arguments{
  \item{.data}{data.frame to be modified}
  \item{condition}{condition for selecting rows to be modified}
  \item{\dots}{further arguments to \code{\link{mutate}}}
  \item{envir}{the frame where the condition is evaluated}
}
\details{is not working on grouped data}


\author{Thomas Wutzler}




\examples{
if (require(dplyr)) {
  ans <- iris \%>\% 
    mutate_cond(
      Species == "setosa"
      , Petal.Length = 1.0
    )
  ans \%>\% filter(Species == "setosa") \%>\% select(Petal.Length) \%>\% head() # 1.0
  ans \%>\% filter(Species != "setosa") \%>\% select(Petal.Length) \%>\% head() # orig
}
}
