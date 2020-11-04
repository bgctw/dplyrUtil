\name{expandAllInconsistentFactorLevels}
\alias{expandAllInconsistentFactorLevels}
\title{expandAllInconsistentFactorLevels}
\description{expand a factor variables in all dataset to encompass levels of all sets}
\usage{expandAllInconsistentFactorLevels(..., .noWarningCols = character(0), 
    .omitWarning = FALSE)}
\arguments{
  \item{\dots}{list of data.frames or several data.frames separated by comma}
  \item{.noWarningCols}{string vector: do not warn for the these
columns}
  \item{.omitWarning}{
}
}
\details{If no data.frame is provided, its returns an empty list.

Instead of providing several arguments, one can provide a
a single list of data.frames}
\value{\code{datasets} with updated factor columns}

\author{Thomas Wutzler}




\examples{
if (exists("expandAllInconsistentFactorLevels")) {
  df1 <- data.frame(f = factor(c("D","D","C")))
  df2 <- data.frame(f = factor(c("C","C","A"))
                    , desc = c("forC1","forC2","forA1"))
  if (requireNamespace("dplyr"))
    dplyr::bind_rows(expandAllInconsistentFactorLevels(df1,df2))
  left_joinFactors(df1,df2)
}
}
