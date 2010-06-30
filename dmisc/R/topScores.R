topScoreSignedWeights <- function(x, n=1, decreasing=TRUE) {
  stopifnot(!is.null(names(x)))
  o <- order(abs(x), decreasing=decreasing)[seq_len(n)]
  return(structure(ifelse(x[o] >= 0, 1, -1), names=names(x)[o]))
}
topScoresCompute <- function(X, weights) {
  stopifnot(!is.null(names(weights)))
  stopifnot(!is.null(colnames(X)))
  return(rowMeans(sweep(X[,names(weights)], 2, weights, "*")))
}
