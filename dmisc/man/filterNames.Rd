\name{filterNames}
\alias{filterNames}
\title{return filtered names from a vector}
\description{
  return filtered names from a vector
}
\usage{
filterNames(x, INDEX, FUN=which.max, ...)
}
\arguments{
  \item{x, INDEX, FUN, ...}{see \code{\link{tapply}}}
}
\value{
  Calls \code{\link{tapply}}, but keeps the names of the \code{FUN}
  results, instead of the values themselves. The input vector must have
  non-null, unambiguous names.
}
\author{
  Antonio, Fabio Di Narzo
}
\examples{
}
\keyword{ util }
\keyword{ misc }
