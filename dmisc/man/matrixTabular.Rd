\name{matrixTabular}
\alias{matrixTabularHead}
\alias{matrixTabularTail}
\alias{matrixTabularBody}
\alias{matrixAsTabular}
\alias{matrixSplitRows}
\alias{matrixPadd}
\title{matrix rendering utility functions}
\description{Matrix rendering utility functions}
\usage{
matrixTabularHead(m, align="c")
matrixTabularTail(m)
matrixTabularBody(m)
matrixAsTabular(m, align="c", head=TRUE)
matrixSplitRows(m, nrows=NULL)
matrixPadd(m, nr, nc, ..., padd="")
}
\arguments{
  \item{m}{a matrix}
  \item{align}{latex tabular column alignment specifier}
  \item{head}{should tabular environment \sQuote{head} and \sQuote{tail} be included?}
  \item{nrows}{number of rows of each submatrix}
  \item{nr, nc}{num. rows, num. columns}
  \item{...}{extra arguments to \code{\link{matrix}}}
  \item{padd}{padding element}
}
\value{
  TODO
}
\author{
Antonio, Fabio Di Narzo
}
\examples{
#TODO
}
\keyword{ programming }
\keyword{ utilities }
