\name{lincom}
\alias{lincom}
\alias{topN}
\alias{topNSign}
\alias{topScoreSignedWeights}
\alias{topScoresCompute}
\title{Linear combinations by named vectors of coefficients}
\description{Linear combinations by named vectors of coefficients}
\usage{
lincom(X, weights)
topN(x, n, decreasing=FALSE)
topNSign(x, n, decreasing=FALSE)

topScoreSignedWeights(x, n=1, decreasing=TRUE)
topScoresCompute(X, weights)
}
\arguments{
  \item{X}{\sQuote{vertical} numeric matrix}
  \item{x, weights}{numeric vector}
  \item{n}{output vector length}
  \item{decreasing}{sort by increasing or decreasing order?}
}
\value{
  FIXME
}
\author{
Antonio, Fabio Di Narzo
}
\examples{
X <- matrix(1:15, 5, 3,
  dimnames=list(patients=letters[1:5],
  gene=c("G1", "G2", "G3")))
scores <- c(G1=1, G2=0.5, G3=-1.5)

w <- topScoreSignedWeights(scores, n=2)
# G3 G1 
# -1  1

topScoresCompute(X, w)
# a  b  c  d  e 
#-5 -5 -5 -5 -5

(X[,"G1"] - X[,"G3"])/2
# a  b  c  d  e 
#-5 -5 -5 -5 -5

\dontshow{
stopifnot(identical(topScoresCompute(X, w), (X[,"G1"] - X[,"G3"])/2))
}
}
\keyword{ programming }
\keyword{ utilities }
