#' Scatterplot matrices
#'
#' A matrix of scatterplots is produced
#'
#' This function is a slight modification of the original
#' \code{\link[graphics]{pairs}} function as proposed here by
#' \href{https://stackoverflow.com/users/2591234/shadow}{shadow} on
#' \href{https://stackoverflow.com}{StackOverflow}:
#' \url{https://stackoverflow.com/questions/22810309/pairs-specifying-axes-limits-of-the-subpanels}
#'
#' @param x the coordinates of points given as numeric columns of a matrix or
#'           data frame. Logical and factor columns are converted to numeric in
#'           the same way that \code{\link[base]{data.matrix}} does.
#'
#' @param labels the names of the variables.
#'
#' @param panel \code{function(x, y, ...)} which is used to plot the contents of
#'              each panel of the display.
#'
#' @param ... arguments to be passed to or from methods. Also,
#'            \code{\link[graphics]{par}} parameters can be given as can
#'            arguments to plot such as \code{main}. \code{par("oma")} will be
#'            set appropriately unless specified.
#'
#' @param lower.panel,upper.panel separate panel functions (or \code{NULL}) to
#'                                be used below and above the diagonal respectively.
#'
#' @param diag.panel optional \code{function(x, ...)} to be applied on the diagonals.
#'
#' @param text.panel optional \code{function(x, y, labels, cex, font, ...)} to
#'                   be applied on the
#'
#' @param label.pos \code{y} position of labels in the text panel.
#'
#' @param line.main if \code{main} is specified, \code{line.main} gives the line
#'                  argument to \code{\link[graphics]{mtext}}() which draws the
#'                  title. You may want to specify \code{oma} when changing
#'                  \code{line.main}.
#'
#' @param cex.labels,font.labels graphics parameters for the text panel.
#'
#' @param row1attop logical. Should the layout be matrix-like with row 1 at the
#'                  top, or graph-like with row 1 at the bottom?
#'
#' @param gap distance between subplots, in margin lines.
#'
#' @param log a character string indicating if logarithmic axes are to be used:
#'            see \code{\link[graphics]{plot.default}}. \code{log = "xy"}
#'            specifies logarithmic axes for all variables.
#'
#' @param xlim,ylim 3-dimension array. The first and second dimensions correspond
#'                  to the row and column indexes of the subplots respectively
#'                  and the third dimension is equal to 2 and correspond to the
#'                  first and second values of the \code{xlim} and \code{ylim}
#'                  arguments to use for each subplot.
#'
#' @author \href{https://stackoverflow.com/users/2591234/shadow}{shadow} user
#'         from \href{https://stackoverflow.com}{StackOverflow}.
#'
#' @source href{https://stackoverflow.com/questions/22810309/pairs-specifying-axes-limits-of-the-subpanels}{Specifying axes limits of the subpanels}
#'         from \href{https://stackoverflow.com}{StackOverflow}.
#'
#' @export
#'
#' @examples
#' data(iris)
#' pairs(iris[1:4], xlim = c(0, 8), ylim = c(0, 8))
#' pairs2(iris[1:4], xlim = c(0, 8), ylim = c(0, 8))
#' # xpecifying limits (now as arrays...)
#' # dims 1-2: panel
#' # dim 3: lower und upper limit
#' my_xlim <- array(0, dim = c(4, 4, 2))
#' my_xlim[, , 2] <- 8
#' my_ylim <- my.xlim
#' my_xlim[1, , 1] <- 4
#' pairs2(iris[1:4], xlim = my.xlim)
#' # careful: the following would work, but does not adjust the labels!
#' my_xlim[2, 3, 2] <- 6
#' pairs(iris[1:4], xlim = my.xlim)
#'
pairs2 <- function(x, labels, panel = points, ..., lower.panel = panel,
                   upper.panel = panel, diag.panel = NULL, text.panel = textPanel,
                   label.pos = 0.5 + has.diag/3, line.main = 3, cex.labels = NULL,
                   font.labels = 1, row1attop = TRUE, gap = 1, log = "",
                   xlim=NULL, ylim=NULL) {

  if (doText <- missing(text.panel) || is.function(text.panel))
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font)
      text(x, y, txt, cex = cex, font = font)

  localAxis <- function(side, x, y, xpd, bg, col = NULL, main, oma, ...) {
    xpd <- NA
    if (side %% 2L == 1L && xl[j]) xpd <- FALSE
    if (side %% 2L == 0L && yl[i]) xpd <- FALSE
    if (side %% 2L == 1L) Axis(x, side = side, xpd = xpd, ...)
    else Axis(y, side = side, xpd = xpd, ...)
  }

  localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
  localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
  localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
  localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq_along(names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) stop("non-numeric argument to 'pairs'")
    }
  } else if (!is.numeric(x)) stop("non-numeric argument to 'pairs'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
    upper.panel <- match.fun(upper.panel)
  if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel))
    diag.panel <- match.fun(diag.panel)
  if (row1attop) {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  if (nc < 2) stop("only one column in the argument to 'pairs'")
  if (doText) {
    if (missing(labels)) {
      labels <- colnames(x)
      if (is.null(labels)) labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels)) doText <- FALSE
  }
  oma <- if ("oma" %in% nmdots) dots$oma
  main <- if ("main" %in% nmdots) dots$main
  if (is.null(oma)) oma <- c(4, 4, if (!is.null(main)) 6 else 4, 4)
  opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  xl <- yl <- logical(nc)
  if (is.numeric(log)) xl[log] <- yl[log] <- TRUE
  else {
    xl[] <- grepl("x", log)
    yl[] <- grepl("y", log)
  }
  for(i in if (row1attop) 1L:nc else nc:1L)
    for (j in 1L:nc) {
      l <- paste0(ifelse(xl[j], "x", ""), ifelse(yl[i], "y", ""))
      if (is.null(xlim) & is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
                  type = "n", ..., log = l)
      if (is.null(xlim) & !is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
                  type = "n", ..., log = l, ylim=ylim[j,i,])
      if (!is.null(xlim) & is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
                  type = "n", ..., log = l, xlim = xlim[j,i,])
      if (!is.null(xlim) & !is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
                  type = "n", ..., log = l, xlim = xlim[j,i,], ylim=ylim[j,i,])

      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        box()
        if (i == 1 && (!(j%%2L) || !has.upper || !has.lower))
          localAxis(1L + 2L * row1attop, x[, j], x[, i], ...)
        if (i == nc && (j%%2L || !has.upper || !has.lower))
          localAxis(3L - 2L * row1attop, x[, j], x[, i],  ...)
        if (j == 1 && (!(i%%2L) || !has.upper || !has.lower))
          localAxis(2L, x[, j], x[, i], ...)
        if (j == nc && (i%%2L || !has.upper || !has.lower))
          localAxis(4L, x[, j], x[, i], ...)
        mfg <- par("mfg")
        if (i == j) {
          if (has.diag) localDiagPanel(as.vector(x[, i]), ...)
          if (doText) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            xlp <- if (xl[i]) 10^0.5 else 0.5
            ylp <- if (yl[j]) 10^label.pos else label.pos
            text.panel(xlp, ylp, labels[i], cex = cex.labels, font = font.labels)
          }
        }
        else if (i < j)
          localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        else localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        if (any(par("mfg") != mfg)) stop("the 'panel' function made a new plot")
      }
      else par(new = FALSE)
    }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) dots$font.main else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) dots$cex.main else par("cex.main")
    mtext(main, 3, line.main, outer = TRUE, at = 0.5, cex = cex.main, font = font.main)
  }
  invisible(NULL)
}
