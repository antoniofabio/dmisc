\name{collapse}
\alias{collapse}
\title{collapse a character vector into one string}
\description{collapse a character vector into one string}
\usage{
collapse(x, sep="")
}
\arguments{
  \item{x}{character vector}
  \item{sep}{joining character}
}
\author{
Antonio, Fabio Di Narzo
}
\examples{
collapse(letters)
#[1] "abcdefghijklmnopqrstuvwxyz"

collapse(letters, " ")
#[1] "a b c d e f g h i j k l m n o p q r s t u v w x y z"
}
\keyword{ programming }
\keyword{ utilities }
