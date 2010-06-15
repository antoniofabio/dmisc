.compose <- function(f,g) {
  f <- match.fun(f)
  g <- match.fun(g)
  function(x) f(g(x))
}

Compose <- function(...) Reduce(.compose, list(...))
