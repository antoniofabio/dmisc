.forest.height <- function(n, rgroups = NULL) {
  unit((n + if(is.null(rgroups)) 0 else length(rgroups)) * 2, "lines")
}
forest.height <- function(n, rgroups = NULL) {
  .forest.height(n, rgroups) + unit(6, "lines")
}
forest.ypos <- function(n, rgroups = NULL) {
  ss <- seq_len(n)
  if(!is.null(rgroups)) {
    ss <- ss + unname(rep(seq_along(rgroups) - 1, rev(rgroups)))
  }
  return(rev(unit(ss * 2 - 1, "lines")))
}

forest.IC <- function(v, l, u,
                      rgroups = NULL,
                      xlim = NULL,
                      adj = unit(0, "lines"), gp = gpar()) {
  pos.y <- forest.ypos(n = length(v), rgroups = rgroups) + adj
  if(is.null(xlim)) {
    eC <- eL <- eR <- rep(FALSE, length(v))
  } else {
    eL <- l < xlim[1]
    eU <- u > xlim[2]
    eC <- v < xlim[1] | v > xlim[2]
    l <- pmin(pmax(l, xlim[1]), xlim[2])
    u <- pmax(pmin(u, xlim[2]), xlim[1])
  }
  grid.segments(unit(l, "native"), pos.y, unit(u, "native"), pos.y, gp = gp)
  if(any(!eC)) {
    grid.circle(unit(v[!eC], "native"), pos.y[!eC],
                r = unit(0.1, "lines"), gp = gp)
  }
  arrow.length <- 1.2 ## mm
  arrow.angle <- 0.5
  a.h <- unit(arrow.length * cos(arrow.angle), unit = "mm")
  a.v <- unit(arrow.length * sin(arrow.angle) , unit = "mm")
  if(any(eL)) {
    yL <- pos.y[eL]
    xL <- unit(rep(xlim[1], sum(eL)), "native")
    grid.segments(xL + a.h, yL - a.v, xL, yL, gp = gp)
    grid.segments(xL + a.h, yL + a.v, xL, yL, gp = gp)
  }
  if(any(eU)) {
    yU <- pos.y[eU]
    xU <- unit(rep(xlim[2], sum(eU)), "native")
    grid.segments(xU - a.h, yU - a.v, xU, yU, gp = gp)
    grid.segments(xU - a.h, yU + a.v, xU, yU, gp = gp)
  }
}

forest.vp <- function(n,
                      height = .forest.height(n, rgroups),
                      xlim = NULL,
                      name = "forestVp",
                      ...) {
  viewport(...,
           height = height,
           xscale = xlim,
           yscale = c(0, n * 2),
           name = name)
}

forest.left.space <- function(labels) max(stringWidth(labels)) + unit(2, "lines")

forest.text.right <- function(M.list, text.right.fmt) sapply(M.list, function(M) sprintf(text.right.fmt, M[,1], M[,2], M[,3]))

forest.right.space <- function(text.right, right.col.sep) {
  text.right.space <- matrix(convertUnit(stringWidth(text.right) + right.col.sep, "mm",
                                         valueOnly = TRUE),
                             ncol = NCOL(text.right))
  return( apply(text.right.space, 2, max) )
}

forest.compare <- function(M.list,
                           rgroups = NULL,
                           vline = NULL,
                           vline.gp = gpar(col = "gray"),
                           xlim = NULL,
                           main = "",
                           xlab = "",
                           xat = NULL,
                           left.labels = rownames(M.list[[1]]),
                           left.space = forest.left.space(left.labels),
                           right.col.sep = unit(2, "lines"),
                           text.right.fmt = "%+2.3f [%+2.3f ; %+2.3f]",
                           text.right = forest.text.right(M.list, text.right.fmt),
                           text.right.space = forest.right.space(text.right, right.col.sep),
                           right.space = unit(sum(text.right.space), "mm") + unit(2, "lines"),
                           colors = seq_along(M.list)) {
  x <- unit(0, "npc")
  y <- unit(1, "npc") - unit(2, "lines")
  just <- c("left", "top")

  pushViewport(viewport(x = x, y = y, just = c("left", "top"),
                        width = left.space,
                        height = .forest.height(length(left.labels), rgroups),
                        name = "left.labels"))
  ## grid.rect(gp = gpar(col = "red"))
  grid.text(left.labels,
            x = unit(1, "lines"),
            y = forest.ypos(length(left.labels), rgroups),
            just = c("left", "center"))
  if(!is.null(rgroups)) {
    rg <- unname(cumsum(rgroups + 1))
    grid.text(names(rgroups),
              x = unit(0.5, "lines"),
              y = unit((length(left.labels) + length(rgroups) - c(0, head(rg, -1))) * 2 - 1,
                "lines"),
              just = c("left", "center"),
              gp = gpar(fontface = "italic"))
  }
  upViewport()
  pushViewport(viewport(x = unit(1, "npc") - right.space, y = y,
                        just = c("left", "top"),
                        width = right.space,
                        height = .forest.height(length(left.labels), rgroups),
                        name = "right.labels"))
  ## grid.rect(gp = gpar(col = "green"))
  xSep <- convertUnit(right.col.sep, "mm", valueOnly = TRUE)
  xPos <- c(xSep, cumsum(tail(text.right.space, -1)) + xSep)
  grid.text(text.right,
            x = unit(rep(xPos, each = NROW(text.right)), units = "mm"),
            y = forest.ypos(length(left.labels), rgroups),
            just = c("left", "center"))
  if(!is.null(colnames(text.right))) {
    grid.text(colnames(text.right),
              x = unit(xPos, units = "mm"),
              y = unit(-2, "lines"),
              just = c("left", "center"))
  }
  upViewport()

  if(is.null(xlim)) {
    xlim <- range(unlist(M.list), na.rm = TRUE)
    xrange <- diff(xlim)
    xlim[1] <- xlim[1] - xrange * 0.02
    xlim[2] <- xlim[2] + xrange * 0.02
  }
  M1 <- M.list[[1]]
  pushViewport(forest.vp(n = NROW(M1),
                         x = x + left.space,
                         y = y,
                         width = unit(1, "npc") - (right.space + left.space),
                         height = .forest.height(length(left.labels), rgroups),
                         xlim = xlim,
                         just = c("left", "top"),
                         name = "ICVp"))
  ## grid.rect(gp = gpar(col = "blue"))
  if(!is.null(vline)) {
    grid.lines(x = unit(c(vline, vline), "native"),
               y = unit(c(0, 1), "npc"),
               gp = vline.gp)
  }

  offsets <- unit(rev(seq_along(M.list) - mean(seq_along(M.list))) / length(M.list),
                  "lines")
  for(j in seq_along(M.list)) {
    B <- M.list[[j]]
    forest.IC(B[,1], B[,2], B[,3],
              rgroups = rgroups,
              xlim = xlim, adj = offsets[j],
              gp = gpar(col = colors[j], fill = colors[j]))
  }
  grid.xaxis(at = xat, gp = gpar(cex = 0.8))
  grid.text(xlab, y = unit(-3, "lines"))
  grid.text(main, y = unit(1, "npc") + unit(1, "lines"),
            gp = gpar(fontface = "bold"))
  ## grid.text(seq(-30, 30), y = unit(seq(-30, 30), "lines"))
  upViewport()
}
