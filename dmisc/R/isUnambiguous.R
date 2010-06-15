isUnambiguous <- function(x) {
  all(!is.na(x)) && (length(unique(x)) == length(x))
}
