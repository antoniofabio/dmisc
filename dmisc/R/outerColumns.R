outerColumns <- function(A, B=A, FUN) {
  FUN <- match.fun(FUN)
  if(is.vector(A)) {
    if(NROW(A) == NROW(B)) {
      ans <- outerColumns(A=matrix(A), B=B, FUN=FUN)
      return(structure(as.vector(ans), names=colnames(ans)))
    } else {
      stop("arguments 'A' and 'B' should be matrices")
    }
  }
  isBiDimensional <- function(X) length(dim(X)==2)
  stopifnot(isBiDimensional(A) && isBiDimensional(B))
  ans <- t(apply(A, 2, function(a) {
    apply(B, 2, function(b) {
      FUN(a, b)
    })
  }))
  rownames(ans) <- colnames(A)
  colnames(ans) <- colnames(B)
  return(ans)
}
