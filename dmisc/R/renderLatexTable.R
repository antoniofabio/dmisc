collapse <- function(x, sep="") paste(x, collapse=sep)

matrixTabularHead <- function(m, align="c") {
  return(paste("\\begin{tabular}{", collapse(rep(align, NCOL(m))), "}", sep=""))
}
matrixTabularTail <- function(m) {
  return("\\end{tabular}")
}
matrixTabularBody <- function(m) {
  return(paste(apply(m, 1, collapse, sep=" & "), "\\\\"))
}

matrixAsTabular <- function(m, align="c", head=TRUE) {
  ans <- matrixTabularBody(m)
  if(head) {
    ans <- c(matrixTabularHead(m), ans, matrixTabularTail(m))
  }
  return(ans)
}

matrixSplitRows <- function(m, nrows=NULL) {
  if(is.null(nrows))
    return(list(m))
  N <- NROW(m)
  nblocks <- ceiling(N/nrows)
  ans <- list()
  for(j in seq_len(nblocks)) {
    ans[[j]] <- m[seq(from=(j-1) * nrows + 1, to=min(N, j*nrows)),,drop=FALSE]
  }
  return(ans)
}

matrixPadd <- function(m, nr, nc, ..., padd="") {
  padding <- nc*ceiling(length(m) / nc) - length(m)
  x <- c(m, rep(padd, padding))
  matrix(x, nr, nc, ...)
}

##render a vector as a latex table, coloring according to non-NA 'colors' elements
colorLatexTable <- function(x, colors, nc, rowsPerPage=NULL, align="c") {
  colorI <- !is.na(colors)
  coloredX <- x
  coloredX[colorI] <- sprintf("\\cellcolor{%s} %s",
                              colors[colorI], x[colorI])
  m <- matrixPadd(coloredX, nc=nc, byrow=TRUE)
  return(paste(sapply(matrixSplitRows(m, rowsPerPage),
                      matrixAsTabular, align=align),
               collapse="\n"))
}
