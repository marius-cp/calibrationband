#' Print monotone confidence bands
#' @importFrom ggplot2 autoplot
#' @param x object of class calibrationband
#' @param ... further arguments to be passed to or from methods;
#' @rdname print.calibrationband
#' @export
print.calibrationband <- function(x, ...) {
  print(autoplot(x, ..., diag = 1, points = 700,approx.equi=1, cut.bands = F))
  print(summary(x, ...))
  invisible(x)
}

#' print summary
#' @param x summary object
#' @param ... further arguments to be passed to or from methods;
#' @rdname print.summary.calibrationband
#' @export

print.summary.calibrationband <- function(x, ...){

  spx <- x
  class(spx) <- class(x)[-1]

  if(identical(nrow(spx), 0L)){
    st <- sprintf("Calibration band captures bisector for all x.\n")
  } else if(nrow(spx)>=1 & nrow(spx)<=3){
    st <- sprintf("Areas of misscalibration (ordered by length).\n")
  } else {
    st <- sprintf("Areas of misscalibration (ordered by length). In addition there are %i more. \n", nrow(spx)-3)
  }


  cat(st)

  if(identical(nrow(spx), 0L)){ }
  else {
    print(spx, n=3)
  }

  invisible(x)

}

