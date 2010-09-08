\name{beginsWith}
\alias{beginsWith}
\alias{endsWith}
\title{beginsWith, endsWith}
\description{Check if a string starts or ands with a specified substring}
\usage{
beginsWith(x, prefix)
endsWith(x, suffix)
}
\arguments{
  \item{x}{a character vector}
  \item{prefix}{prefix to be checked for}
  \item{suffix}{suffix to be checked for}
}
\value{
  A vector of \code{TRUE}/\code{FALSE} values.
}
\author{
Antonio, Fabio Di Narzo
}
\examples{
beginsWith(c("ciao", "bau", "warf"), "ba")
## [1] FALSE  TRUE FALSE
endsWith(c("ciao", "bau", "warf"), "au")
## [1] FALSE  TRUE FALSE
}
\keyword{ programming }
\keyword{ utilities }
