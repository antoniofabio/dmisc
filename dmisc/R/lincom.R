## linear combination of the columns of X,
## eventually exploiting `names'
lincom <- function(X, weights) {
  if(!is.null(names(weights))) {
    stopifnot(!is.null(colnames(X)))
    X <- X[, names(weights), drop=FALSE]
  }
  return(rowSums(sweep(X, 2, weights, "*")))
}

topN <- function(x, n, decreasing=FALSE) {
  order(x, decreasing=decreasing)[seq_len(n)]
}

topNSign <- function(x, n, decreasing=FALSE) {
  return(sign(x[topN(abs(x), n=n, decreasing=decreasing)]))
}
