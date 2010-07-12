\name{fancyContrasts}
\alias{fancyContrasts}
\title{Build more readable contrasts for factor variables}
\description{
  Build more readable contrasts for factor variables
}
\usage{
fancyContrasts(x, base=1)
}
\arguments{
  \item{x}{a \code{factor} variable}
  \item{base}{reference level}
}
\value{
  A contrasts matrix.
}
\author{
  Antonio, Fabio Di Narzo
}
\examples{
x <- factor(c('a', 'b', 'a', 'c'))
contrasts(x) <- fancyContrasts(x)
y <- seq_along(x)
lm(y ~ x)
}
\keyword{ util }
\keyword{ misc }
