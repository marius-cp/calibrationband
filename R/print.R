#' Print monotone confidence bands
#'
#' Printing methods for \code{'calibrationband'} and
#' \code{'summary.calibrationband'} objects.
#'
#' @param x object of class \code{calibrationband}
#' @param ... Further arguments to be passed to or from methods;
#' in particular these passed to \code{\link{autoplot.calibrationband}}
#' @param n number of areas of miscalibration printed.
#'
#' @details
#' \code{print.calibrationband} always sends an autoplot object to the
#' current graphics device and prints a summary to the console.
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso
#'   \code{\link{autoplot.calibrationband}},
#'   \code{\link{summary.calibrationband}}
#'
#' @name print.calibrationband
NULL

#' @rdname print.calibrationband
#' @importFrom ggplot2 autoplot
#'
#' @export

print.calibrationband <- function(x, ...) {
  print(autoplot(x, ...,approx.equi=500, cut.bands = F))
  print(summary(x, ...))
  invisible(x)
}

#' @rdname print.calibrationband
#'
#' @export

print.summary.calibrationband <- function(x, ..., n=3){

  spx <- x
  class(spx) <- class(x)[-1]

  if(identical(nrow(spx), 0L)){
    st <- sprintf("Calibration band captures bisector for all x.\n")
  } else if(nrow(spx)>=1 & nrow(spx)<=n){
    st <- sprintf("Areas of misscalibration (ordered by length).\n")
  } else {
    st <- sprintf("Areas of misscalibration (ordered by length). In addition there are %i more. \n", nrow(spx)-n)
  }


  cat(st)

  if(identical(nrow(spx), 0L)){ }
  else {
    print(spx, n=n)
  }

  invisible(x)

}

