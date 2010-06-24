matrixAsTabular <- function(m, align="c") {
  head <- paste("\\begin{tabular}{",
                paste(rep(align, NCOL(m)), collapse=""),
                "}\n", sep="")
  body <- paste(paste(apply(m, 1, paste, collapse=" & "),
                      collapse="\\\\\n"), "\\\\\n")
  tail <- "\\end{tabular}\n"
  return(paste(head, body, tail, sep=""))
}

##render a vector as a latex table, color-coded by vector names
renderLatexTable <- function(x, nc, colors, rowsPerPage=NULL, align="c") {
  tags <- names(x)
  if(missing(colors)) {
    colors <- seq_along(tags)
  }
  if(is.null(names(colors)))
    names(colors) <- unique(tags)
  colors <- colors[intersect(names(colors), tags)]
  coloredX <- sprintf("\\cellcolor{%s} %s", colors[tags], x)
  padding <- nc*ceiling(length(x) / nc) - length(x)
  coloredX <- c(coloredX, rep("", padding))
  if(is.null(rowsPerPage))
    rowsPerPage <- length(coloredX) / nc
  m <- matrix(coloredX, ncol=nc, byrow=TRUE)
  numPages <- ceiling(nrow(m) / rowsPerPage)
  ans <- ""
  for(p in seq_len(numPages)) {
    ip <- seq(from=(p-1)*rowsPerPage + 1, to=min(rowsPerPage*p, NROW(m)))
    ans <- paste(ans, matrixAsTabular(m[ip,,drop=FALSE], align=align))
  }
  return(ans)
}
