\name{isUnambiguous}
\alias{isUnambiguous}
\alias{isAmbiguous}
\title{Check if a vector is made of unique values only}
\description{Check if a vector is made of unique values only}
\usage{
isAmbiguous(x)
isUnambiguous(x)
}
\arguments{
  \item{x}{a vector (usually a character vector)}
}
\value{
  \code{isUnambiguous} \code{TRUE} if the vector is made of unique values only and contains
  no missing values, \code{FALSE} otherwise. \code{isAmbiguous} is the
  exact negation of it.
}
\author{
Antonio, Fabio Di Narzo
}
\examples{
isUnambiguous(1:3)
## [1] TRUE
isUnambiguous(c(1,2,NA))
## [1] FALSE
isUnambiguous(c(1,2,2))
## [1] FALSE

\dontshow{
stopifnot(isUnambiguous(1:3))
stopifnot(!isUnambiguous(c(1,2,NA)))
stopifnot(!isUnambiguous(c(1,2,2)))
}

}
\keyword{ manip }
\keyword{ programming }
\keyword{ utilities }
