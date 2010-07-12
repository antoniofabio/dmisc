fancyContrasts <- function(x, base=1) {
  CC <- contr.treatment(levels(x), base=base)
  base <- rownames(CC)[base]
  colnames(CC) <- sprintf(" (%s vs %s)", colnames(CC), rep(base, NCOL(CC)))
  return(CC)
}
