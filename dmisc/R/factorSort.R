factorSort <- function(fac) {
  cc <- sort(table(fac), decreasing=TRUE)
  return(factor(as.character(fac), levels=names(cc)))
}
