#' summarize calibration band  object
#'
#' An object of class \code{calibrationband} contains the calibration band
#' coordinates, the pairs of original observation and forecast values,
#' and the recalibrated forecasts obtained by isotonic regression.
#' The function \code{summary.reliabilitydiag} calculates the areas of
#' miscalibration.
#'
#' @param object object of class \code{calibrationband}
#' @param iso_test with \code{default = FALSE}. If \code{TRUE}, the decision of the isotonicity test is reported along side the crossings of the band. If the \code{calibrationband} is calculated with \code{nc=TRUE}, the bands are re-estimated with \code{nc=FALSE} using \code{digits=3}. The \code{alpha} from the \code{calibrationband} is used.
#' @param n number of rows in output table.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @return
#'
#' A \code{'summary.reliability'} object, which is also a
#' tibble (see \code{\link[tibble:tibble]{tibble::tibble()}}) with columns:
#' \tabular{ll}{
#' min_x \tab minimal x-coordinate of  misscalibration segment (ordered by length). \cr
#' max_x \tab maximal x-coordinate of  misscalibration segment (ordered by length).
#' }
#'
#' @examples
#' set.seed(123)
#' s=.8
#' n=10000
#' x <- sort(runif(n))
#'
#' p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
#' dat <- data.frame(pr=x, y=rbinom(n,1,p(x,s)))
#'
#' cb <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method="round", digits =3)
#'
#' summary(cb)
#' print(summary(cb), n=5)
#'
#' @export

summary.calibrationband <- function(object, ..., iso_test = FALSE, n=3){

  #sr <- object$cal  %>%
   # dplyr::filter(out==1) %>%
    #dplyr::arrange(dplyr::desc(range))# %>%
    #dplyr::select(min_x,max_x)

  if(isFALSE(iso_test)){
    crosscheck <- NULL
  } else if(isTRUE(iso_test)){
    crosscheck <- checkcrossings(object) %>%
      dplyr::filter(cross==1) %>%
      dplyr::arrange(dplyr::desc(range)) %>%
      dplyr::mutate(id = "crossings")
  }

  sr <-
  dplyr::bind_rows(
    crosscheck,
    object$cal  %>%
      dplyr::filter(out==1) %>%
      dplyr::arrange(dplyr::desc(range)) %>%
      dplyr::mutate(id = "miscalibration"),
  ) %>%
    dplyr::mutate(
      iso_test = iso_test,
      n = n
    )

  class(sr) <- c("summary.calibrationband", class(sr))
  return(sr)
}


