## Antonio, Fabio Di Narzo, 2010
##
## Make duplicated (and empty) entries unique
## by adding a suffix when necessary
##
disambiguate <- function(x, y=seq_along(x), fmt="%s (%s)") {
  stopifnot(is.vector(x) && is.vector(y))
  stopifnot(length(x) == length(y))
  stopifnot(all(!is.na(x)) && all(!is.na(y)))
  x <- as.character(x)
  y <- as.character(y)
  tbl <- table(x)
  duplId <- which(x %in% names(tbl)[tbl>1])
  x[duplId] <- sprintf(fmt, x[duplId], y[duplId])
  return(x)
}

filterNames <- function(x, INDEX, FUN=which.max, ...) {
  stopifnot(!is.null(names(x)))
  stopifnot(isUnambiguous(names(x)))
  FUN <- Compose(names, match.fun(FUN))
  return(tapply(x, INDEX, FUN))
}
