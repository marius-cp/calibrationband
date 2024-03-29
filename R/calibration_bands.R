## usethis namespace: start
#' @useDynLib calibrationband
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL



#' Confidence bands for monotone probabilities
#'
#' @param x covariate.
#' @param y response variable (in {0,1}).
#' @param alpha type one error probability (1 minus the confidence level).
#' @param method \code{"standard"} for the original method proposed in the
#'     article, \code{"round"} for rounding the covariate, or \code{"YB"} for the bounds by Yang & Barber
#'     (2019).
#' @param digits number of digits for method \code{"round"}. Default is 2.
#'     Has no effect for the other methods.
#' @param nc use non-crossing bands for method \code{"standard"} or
#'     \code{"round"}. Has no effect for method \code{"YB"}. Default is
#'     \code{FALSE}. See also \code{"summary(...,iso_test=TRUE)"} in this context.
#'     Crossings allow to reject the null hypothesis of monotonicity in the calibration curve.
#'
#' @return
#' An object of class \code{calibrationband}, which is a list containing the following entries:
#'
#' \tabular{ll}{
#'  \code{bands} \tab a tibble holding \code{x,lwr,upr} the lower and upper bound,
#'  for each value of \code{x}.
#'  The upper bound extends to the left and the lower bound to the right,
#'  that is, the upper bound for \code{x[i]<s<x[i+1]} is \code{upr[i+1]},
#'  and the lower bound for \code{x[i]<s<x[i+1]} is \code{lwr[i]}.\cr
#'  \code{cal} \tab a tibble holding the areas/segments of calibration (\code{out=0}) and miscalibration (\code{out=1}).  \cr
#'  \code{bins} \tab a tibble of the characteristics of the isotonic bins. \cr
#'  \code{cases} \tab tibble of all predictions and observations.
#'  In addition it holds the column \code{isoy}, which is the isotonic
#'  regression of \code{y} at points \code{x}.   \cr
#'  \code{alpha} \tab the given type one error probability (1 minus the nominal
#'  coverage of the band). \cr
#'  \code{method} \tab the selected method for computing the band. \cr
#' \code{nc} \tab the selected method for non-crossing.\cr
#' \code{digits} \tab the given digits for method \code{"round"}
#'     (or \code{NULL} for method \code{"standard"}).\cr
#'  \code{time} \tab time to compute the upper and lower band.
#' }
#'
#'
#' @export
calibration_bands <- function(
  x,
  y,
  alpha = 0.05,
  method = "standard",
  digits = NULL,
  nc = FALSE
) {
  # compute isotonic regression
  x_original <- x
  ir <- stats::isoreg(x = x, y = y)
  if (!ir$isOrd) {
    x <- x[ir$ord]
    y <- y[ir$ord]
  }
  isoy <- ir$yf
  x_unique <- sort(unique(x))
  N <- length(x_unique)
  n <- length(x)

  if (identical(method, "YB")) {
    ## lower index in each block
    part_lwr <- which(ir$yf > c(-1, ir$yf[-length(y)]))

    ## upper index in each block (zero-based indexing for Rcpp)
    part_upr <- c(part_lwr[-1] - 1, length(y)) - 1

    ## a vector matching each index i to the block it belongs to
    ind_to_block <- match(isoy, isoy[part_lwr]) - 1

    ## zero-based indexing also for lower block
    part_lwr <- part_lwr - 1

    # vector of constants sqrt(2 * log((N^2 + N) / alpha) / k) for k = 1,...,n,
    # so that these constants only need to be computed once
    cc <- sqrt(log((N^2 + N) / alpha) / 2 / seq_len(n))

    time_ <- Sys.time()
    # compute bands
    lwr <- lower_bound(isoy, cc, part_lwr, ind_to_block)
    upr <- upper_bound(isoy, cc, part_upr, ind_to_block)

    time <- Sys.time() - time_


    # remove duplicates
    jumps_lwr <- which(x[-n] < x[-1])
    jumps_upr <- c(1, jumps_lwr + 1)
    jumps_lwr <- c(jumps_lwr, n)
    upr <- upr[jumps_upr]
    lwr <- lwr[jumps_lwr]
    x <- x[jumps_upr]
  } else {
    # aggregate by identical values of x
    ys <- split(y, x)
    ns <- lengths(ys)
    ys <- sapply(ys, sum)

    if (identical(method, "standard")) {
      time_ <- Sys.time()
      lwr <- cp_lower_bound(ys = ys, ns = ns, alpha = alpha)
      upr <- cp_upper_bound(ys = ys, ns = ns, alpha = alpha)
      time <- Sys.time() - time_


      # non-crossing variant
      if (isTRUE(nc)) {
        isoy_short <- isoy[c(which(x[-n] < x[-1]), n)]
        upr <- pmax(upr, isoy_short)
        lwr <- pmin(lwr, isoy_short)
      }

      # replace x
      x <- x_unique

    } else if (identical(method, "round")) {
      x_round <- as.numeric(names(ys))
      if (is.null(digits)) digits <- 2
      d10 <- 10^digits
      # compute upper bound
      x_upr <- floor(x_round * d10) / d10
      ys_upr <- tapply(ys, x_upr, sum)
      ns_upr <- tapply(ns, x_upr, sum)
      x_upr <- tapply(x_round, x_upr, min)
      x_upr <- sort(unique(x_upr))

      # compute lower bound
      x_lwr <- ceiling(x_round * d10) / d10
      ys_lwr <- tapply(ys, x_lwr, sum)
      ns_lwr <- tapply(ns, x_lwr, sum)
      x_lwr <- tapply(x_round, x_lwr, max)
      x_lwr <- sort(unique(x_lwr))

      time_ <- Sys.time()
      upr <- cp_upper_bound(ys_upr, ns_upr, alpha)
      lwr <- cp_lower_bound(ys_lwr, ns_lwr, alpha)
      time <- Sys.time() - time_


      # merge the x values for upper and lower bound
      x_round <- sort(unique(c(x_upr, x_lwr)))
      upr <- upr[match(x_round, x_upr)]
      upr[is.na(upr)] <- 1
      upr <- rev(cummin(rev(upr)))
      lwr <- lwr[match(x_round, x_lwr)]
      lwr[is.na(lwr)] <- 0
      lwr <- cummax(lwr)

      # non-crossing
      if (isTRUE(nc)) {
        isoy_short <- isoy[c(which(x[-n] < x[-1]), n)]
        upr_iso <-
          interpolate(x = x_unique, y = isoy_short, xout = x_round, right = 1)
        upr_iso[is.na(upr_iso)] <- 0
        lwr_iso <-
          interpolate(x = x_unique, y = isoy_short, xout = x_round, right = 0)
        lwr_iso[is.na(lwr_iso)] <- 1
        lwr <- pmin(lwr, lwr_iso)
        upr <- pmax(upr, upr_iso)
      }
      x <- x_round
    }
  }

  # extend to x = 0 and x = 1 in case they are not contained in the data
  if (x[1] > 0) {
    x <- c(0, x)
    upr <- c(upr[1], upr)
    lwr <- c(0, lwr)
  }
  if (x[length(x)] < 1) {
    x <- c(x, 1)
    upr <- c(upr, 1)
    lwr <- c(lwr, lwr[length(lwr)])
  }

  df_bands <-
    tibble::tibble(
      x = x,
      lwr = lwr,
      upr = upr
    )

  red_iKnots <- with(
    ir,
    which(!duplicated(yf[iKnots], fromLast = TRUE)) %>% iKnots[.]
  )
  df_pav <- with(
    ir,
    tibble::tibble(
      case_id = if (isOrd) seq_len(length(y)) else ord,
      x = if (isOrd) x else x[ord],
      y = if (isOrd) y else y[ord],
      bin_id = rep.int(seq_along(red_iKnots), times = diff(c(0, red_iKnots))),
      isoy = yf
    )
  )
  df_bins <- tibble::tibble(
    bin_id = seq_along(red_iKnots),
    n = diff(c(0, red_iKnots)),
    x_min = df_pav$x[c(0, utils::head(red_iKnots,-1)) + 1],
    x_max = df_pav$x[red_iKnots],
    isoy = df_pav$isoy[red_iKnots]
  )

  df_cal <-
    tibble::tibble(
      x_ =  seq(0,1, length.out = 10000),
      lwr_ =interpolate(x=df_bands$x, y= df_bands$lwr, xout=x_, right = 0),
      upr_ =interpolate(x=df_bands$x, y= df_bands$upr, xout=x_, right = 1)
    ) %>%
    dplyr::mutate(
      out = ifelse(upr_ < x_ | lwr_ > x_, 1, 0),
      segment = with(rle(out), rep(seq_along(lengths), times = lengths)) # segment of calibration/miscalibration
      ) %>%
    dplyr::group_by(segment) %>%
    dplyr::summarise(
      out = mean(out),
      min_x = min(x_),
      max_x = max(x_)
    ) %>%
    dplyr::mutate(range = max_x-min_x)

  out <- list(
    bands=df_bands,
    cal = df_cal,
    bins = df_bins,
    cases = df_pav,
    alpha = alpha,
    method = method,
    digits = digits,
    nc = nc,
    time = time
  )

  structure(out, class = "calibrationband")
}
