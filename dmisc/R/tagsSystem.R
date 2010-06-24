##
## facilities for handling variables tagging/untagging
##  and related manipulations
##

tagEmpty <- function() list()
tagClean <- function(Tags) {
  Tags[sapply(Tags, length) > 0]
}
tagAdd <- function(Tags, variables, tag) {
  Tags[[tag]] <- union(Tags[[tag]], variables)
  return(tagClean(Tags))
}
tagRemove <- function(Tags, variables, tag) {
  Tags[[tag]] <- setdiff(Tags[[tag]], variables)
  return(tagClean(Tags))
}
tagNames <- function(Tags) names(Tags)[sapply(Tags, length)>0]
tagUniverse <- function(Tags) unique(unlist(Tags))
tagToIncidenceMatrix <- function(Tags) {
  universe <- tagUniverse(Tags)
  tn <- tagNames(Tags)
  m <- matrix(FALSE, nrow=length(tn), ncol=length(universe),
              dimnames=list(tag=tn, elt=universe))
  for(j in seq_along(tn)) {
    m[j, universe %in% Tags[[j]]] <- TRUE
  }
  return(m)
}

## swap roles of 'names' and 'contents' of a vector 'x'
swapNamesAndContents <- function(x) structure(names(x), names=x)

## convert the tags object 'Tags' into a named vector,
##  where tags are the vector names, eventually collapsed
tagToNamedVector <- function(Tags, IM=tagToIncidenceMatrix(Tags), collapse=", ") {
  swapNamesAndContents(apply(IM, 2, function(x) paste(rownames(IM)[x], collapse=collapse)))
}
## from a named vector, build the corresponding tags object
tagFromNamedVector <- function(vec) {
  vnames <- names(vec)
  T <- tagEmpty()
  for(n in unique(vnames)) {
    T <- tagAdd(T, vec[vnames==n], n)
  }
  return(T)
}
## from a tag object, build a unique partitioning
tagPartition <- function(Tags, universe=tagUniverse(Tags), collapse=", ",
                         noneName="[none]") {
  Tags <- tagAdd(Tags, setdiff(universe, tagUniverse(Tags)), noneName)
  return(tagFromNamedVector(tagToNamedVector(Tags, collapse=collapse)))
}
