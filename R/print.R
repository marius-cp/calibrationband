#' Print monotone confidence bands
#'
#' Printing methods for \code{'calibrationband'} and
#' \code{'summary.calibrationband'} objects.
#'
#' @param x object of class \code{calibrationband}
#' @param ... Further arguments to be passed to or from methods;
#' in particular these passed to \code{\link{autoplot.calibrationband}}
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
  print(autoplot(x, ...,approx.equi=500, cut.bands = FALSE))
  print(summary(x, ...))
  invisible(x)
}

#' @rdname print.calibrationband
#'
#' @export

print.summary.calibrationband <- function(x, ...){

  #if(isFALSE(iso_test)){
  spx <- x
  class(spx) <- class(x)[-1]

  iso_test <- spx$iso_test %>% unique()
  al <- spx$alpha %>% unique()
  n <- spx$n %>% unique()


  mc <-
    spx %>%
    dplyr::filter(id == "miscalibration") %>%
    dplyr::select(min_x,max_x)

  if(identical(nrow(mc), 0L)){
    st <- sprintf("\n Calibration band captures bisector for all x.\n")
  } else if(nrow(mc)>=1 & nrow(mc)<=n){
    st <- sprintf("\n Areas of misscalibration (ordered by length).\n")
  } else {
    st <- sprintf("\n Areas of misscalibration (ordered by length). In addition there are %i more. \n", nrow(spx)-n)
  }


  cat(st)

  if(identical(nrow(mc), 0L)){ }
  else {
    print(
      mc,
      n=n)
  }

  invisible(x)

  #}

  if(isTRUE(iso_test)){

    it <-
      spx %>%
      dplyr::filter(id == "crossings") %>%
      dplyr::select(min_x,max_x)

    if(identical(nrow(it), 0L)){
      st <- sprintf("\n No crossings. Thus, no evidence to reject the null of an isotonic calibration curve. The significance level is %.3f. \n", as.numeric(al))
    } else {
      st <- sprintf("\n Crossing in the ranges below. Reject the null of an isotonic calibration curve. The significance level is %.3f.  \n", as.numeric(al))

    }

    cat(st)


    if(identical(nrow(it), 0L)){ }
    else {
      print(
        it,
        n=n)
    }



  }



}

