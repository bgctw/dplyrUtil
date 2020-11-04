\name{mutate_cond}
\alias{mutate_cond}
\title{mutate cond}
\description{mutate acting only on the rows satisfying the condition}
\usage{mutate_cond(.data, condition, ..., envir = parent.frame(), 
    na.rmCond = FALSE)}
\arguments{
  \item{.data}{data.frame to be modified}
  \item{condition}{condition for selecting rows to be modified}
  \item{\dots}{further arguments to \code{\link{mutate}}}
  \item{envir}{the frame where the condition is evaluated}
  \item{na.rmCond}{set to TRUE to set NA values in cond to FALSE.
By default an error is thrown}
}
\details{condition is evaluated in .data inside the parent frame.
Hence column names can be used.
\code{.data} refers to the subset group of the data.frame processed.}


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
