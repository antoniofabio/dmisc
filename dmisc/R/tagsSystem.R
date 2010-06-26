##
## facilities for handling variables tagging/untagging
##  and related manipulations
##
## convert an incidence matrix to a tags vector
tagsFromIM <- function(IM) {
  tags <- colnames(IM)
  return(apply(IM, 1, function(j) paste(tags[j], collapse=",")))
}

## swap roles of 'names' and 'contents' of a vector 'x'
swapNamesAndContents <- function(x) structure(names(x), names=x)
