matrixAsTabular <- function(m, align="c") {
  head <- paste("\\begin{tabular}{",
                paste(rep(align, NCOL(m)), collapse=""),
                "}\n", sep="")
  body <- paste(paste(apply(m, 1, paste, collapse=" & "),
                      collapse="\\\\\n"), "\\\\\n")
  tail <- "\\end{tabular}\n"
  return(paste(head, body, tail, sep=""))
}

matrixSplitRows <- function(x, nrows) {
  N <- NROW(x)
  nblocks <- ceiling(N/nrows)
  ans <- list()
  for(j in seq_len(nblocks)) {
    ans[[j]] <- x[seq(from=(j-1) * nrows + 1, to=min(N, j*nrows)),,drop=FALSE]
  }
  return(ans)
}

matrixPadd <- function(x, nr, nc, ..., padd="") {
  padding <- nc*ceiling(length(x) / nc) - length(x)
  x <- c(x, rep(padd, padding))
  matrix(x, nr, nc, ...)
}

##render a vector as a latex table, coloring according to non-NA 'colors' elements
colorLatexTable <- function(x, colors, nc, rowsPerPage=NULL, align="c") {
  colorI <- !is.na(colors)
  coloredX <- x
  coloredX[colorI] <- sprintf("\\cellcolor{%s} %s",
                              colors[colorI], x[colorI])
  m <- matrixPadd(coloredX, nc=nc, byrow=TRUE)
  if(is.null(rowsPerPage))
    rowsPerPage <- ceiling(length(coloredX) / nc) + 1
  return(paste(sapply(matrixSplitRows(m, rowsPerPage),
                      matrixAsTabular, align=align),
               collapse="\n"))
}
