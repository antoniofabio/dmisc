beginsWith <- function(x, prefix) grepl(paste("^", prefix, sep=""), x)
endsWith <- function(x, suffix) grepl(paste(suffix, "$", sep=""), x)
