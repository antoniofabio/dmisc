\name{disambiguate}
\alias{disambiguate}
\title{make a vector unambiguous}
\description{
  make a vector unambiguous with the help of another vector
}
\usage{
disambiguate(x, y=seq_along(x), fmt="\%s (\%s)")
}
\arguments{
  \item{x, y}{vectors}
  \item{fmt}{format string for \code{\link{sprintf}}}
}
\value{
  the disambiguated vector \code{x}.
}
\author{
  Antonio, Fabio Di Narzo
}
\examples{
x <- c('a', 'b', 'a', 'c')
disambiguate(x)
## [1] "a (1)" "b"     "a (3)" "c"
## We can use different patterns:
disambiguate(x, fmt="\%s \%s")
## [1] "a 1" "b"   "a 2" "c"
disambiguate(x, fmt="\%s/\%s")
## [1] "a/1" "b"   "a/2" "c"
}
\keyword{ util }
\keyword{ misc }
