topScoreSignedWeights <- function(x, n=1, decreasing=TRUE) {
  stopifnot(!is.null(names(x)))
  return(topNSign(x=x, n=n, decreasing=decreasing))
}
topScoresCompute <- function(X, weights) {
  stopifnot(!is.null(names(weights)))
  stopifnot(!is.null(colnames(X)))
  return(lincom(X, weights/length(weights)))
}
