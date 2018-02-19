#' A graphical correlation matrix
#'
#' Combines \code{[graphics]{pairs}} (through \code{[mcgraph]{pairs2}}) and
#' \code{[stats]{cor.test}}.
#'
#' @param df a data frame.
#'
#' @param digits integer indicating the number of decimal places (round).
#'               Negative values are allowed (see \code{[base]{round}}).
#'
#' @param cex a numerical vector giving the amount by which plotting characters
#'            and symbols should be scaled relative to the default. This works
#'            as a multiple of \code{[graphics]{par}}\code{("cex")}. \code{NULL}
#'            and \code{NA} are equivalent to \code{1.0}. Note that this does
#'            not affect annotation: see \code{[graphics]{plot.default}}.
#'
#' @param pch a vector of plotting characters or symbols: see
#'            \code{[graphics]{points}}.
#'
#' @param bty_upper,bty_lower A character string which determined the type of
#'                            \code{[graphics]{box}} which is drawn about plots.
#'                            If \code{bty} is one of "\code{o}" (the default),
#'                            "\code{l}", "\code{7}", "\code{c}", "\code{u}", or
#'                            "\code{]}" the resulting \code{box} resembles the
#'                            corresponding upper case letter. A value of
#'                            "\code{n}" suppresses the \code{box}. See
#'                            \code{[graphics]{par}} for more information.
#'
#' @param col The colors for lines and points. Multiple colors can be specified
#'            so that each point can be given its own color. If there are fewer
#'            colors than points they are recycled in the standard fashion.
#'            Lines will all be plotted in the first colour specified. See
#'            \code{[graphics]{plot.default}} for more information.
#'
#' @param bg a vector of background colors for open plot symbols,
#'           \code{[graphics]{points}}. Note: this is **not** the same setting
#'           as \code{[graphics]{par}}\code{("bg")}. See
#'           \code{[graphics]{plot.default}} for more information.
#'
#' @param axeslim the \code{x} and \code{y} limits (\code{x1}, \code{x2},
#'                \code{y1}, \code{y2}) of the plot. Note that \code{x1 > x2}
#'                and \code{y1 > y2} are allowed and lead to ‘reversed axes’.
#'                The default value, \code{NULL}, indicates that the range of
#'                the finite values to be plotted should be used.
#'
#' @param ... arguments values passed to \code{[mcgraph]{pairs2}}.
#'
#' @export
#'
#' @author Marc Choisy
#'
paircor <- function(df, digits = 2, cex = 1.5, pch = 21, bty_upper = par("bty"),
                    bty_lower = par("bty"), col = par("col"), bg = par("bg"),
                    axeslim = NULL, ...) {
  if (is.null(axeslim)) axeslim <- range(df)
  .x <- .y <- mean(axeslim)
  n <- length(df)
  xlim <- ylim <- array(c(rep(axeslim[1], n*n), rep(axeslim[2], n*n)), c(n, n, 2))
  pairs2(df,
         upper.panel = function(x, y, ...) {
           points(x, y, type = "n", bty = "n", ...)
           cor.test(x, y) %>%
             `[`(c("estimate", "p.value")) %>%
             unlist() %>%
             round(digits) %>%
             format() %>%
             paste(c("r", "p"), ., sep = " = ") %>%
             paste(collapse = "\n") %>%
             text(.x, .y, ., cex = cex)
           box(bty = bty_upper)
         },
         lower.panel = function(x, y, ...) {
           points(x, y, cex = cex, pch = pch, col = col, bg = bg, bty = "n", ...)
           abline(0, 1)
           box(bty = bty_lower)
         },
         xlim = xlim, ylim = ylim, ...)
}
