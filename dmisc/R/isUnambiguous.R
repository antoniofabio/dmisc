isAmbiguous <- function(x) any(is.na(x)) || length(unique(x)) != length(x)
isUnambiguous <- function(x) !isAmbiguous(x)
