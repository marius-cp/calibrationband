#' summarize calibration band  object
#'
#' An object of class \code{calibrationband} contains the calibration band
#' coordinates, the pairs of original observation and  forecast values,
#' and the recalibrated forecasts given by isotonic regression.
#' The function \code{summary.reliabilitydiag} calculates the areas of
#' miscalibration.
#'
#' @param object object of class \code{calibrationband}
#' @param ... further arguments to be passed to or from methods;
#'
#' @return
#'
#' A \code{'summary.reliability'} object, which is also a
#' tibble (see \code{\link[tibble:tibble]{tibble::tibble()}}) with columns:
#' \tabular{ll}{
#' min_x \tab minimal x-coordinate of  misscalibration segemnt (ordered by length). \cr
#' max_x \tab maximal x-coordinate of  misscalibration segemnt (ordered by length).
#' }
#'
#'
#' @export

summary.calibrationband <- function(object, ...){

  sr <- object$cal  %>%
    dplyr::filter(out==1) %>%
    dplyr::arrange(dplyr::desc(range)) %>%
    dplyr::select(min_x,max_x)

  class(sr) <- c("summary.calibrationband", class(sr))
  return(sr)

}
