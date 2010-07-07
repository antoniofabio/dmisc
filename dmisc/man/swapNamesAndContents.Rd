\name{swapNamesAndContents}
\alias{swapNamesAndContents}
\title{swap names and contents of a vector}
\description{swap \sQuote{names} and \sQuote{contents} of a vector}
\usage{
swapNamesAndContents(x)
}
\arguments{
  \item{x}{a (named) vector}
}
\author{
Antonio, Fabio Di Narzo
}
\examples{
swapNamesAndContents(c(a=1, b=2, c=3))
#   1   2   3 
# "a" "b" "c" 
}
\keyword{ programming }
\keyword{ utilities }
