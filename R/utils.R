#' Interpolation (required in isobands)
#'
#' @param x position on x axis.
#' @param y position on y axis.
#' @param xout positions to interpolate.
#' @param right if \code{1}, the interpolation at xout with
#'     x[i] < xout <= x[i+1] is y[i+1], if \code{0} it is y[i].
#'
#' @importFrom stats approx
interpolate <- function(x, y, xout, right = 1) {
  stats::approx(x = x, y = y, xout = xout, f = right, method = "constant")$y
}
