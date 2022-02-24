#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plotting monotone confidence bands
#' @param object object of class calibrationband
#' @param approx.equi If \code{NULL}, the bands are drawn for each
#' prediction-realization pair. In large data sets this might result in capacity
#' consuming plots. Other choices than \code{NULL} result in equidistant
#' interpolation at by default 700 \code{points}. See bellow.
#' @param points Interpolate at \code{points} equidistant values between 0 and 1
#' (if \code{cut.bands=F}) or between the most extreme prediction values
#' (if \code{cut.bands=T}). Note, for smooth accurate illustration in transition areas
#' (changes between miscalibrated and calibrated areas) we add those critical
#' additional points the initial value of  \code{points}.
#' @param isoreg If \code{NULL}, raws the isotonic/monotonic non-parametric
#' regression curve.
#' @param shaddow If \code{NULL}, raws a blueish ribbon.
#' @param diag If default, a plain, black diagonal line between the coordinates
#' 0,0 and 1,1 is draw. Otherwise, the diagonal line is colored
#' (red if miscalibrated, gray if calibrated).
#' Note, if the latter is selected and if \code{cut.bands=T}, only the area in
#' between the minimal and maximal prediction value is colored.
#' @param cut.bands Cut the bands at most extreme prediction values. Bands will not be extended to 0 and 1 respectively.
#' @param ... further arguments to be passed to or from methods;
#' @importFrom ggalt StatStepribbon
#' @rdname autoplot.calibrationband
#' @export
#'

autoplot.calibrationband <- function(object, ...,
                             approx.equi=NULL,
                             points = 700,
                             isoreg = NULL,
                             shaddow = NULL,
                             diag = "default",
                             cut.bands=FALSE){
  # return a ggplot 2 object of Isobands
  r <- object

  p <- ggplot2::ggplot()

  if(is.null(approx.equi)){

    p_dat <-
      tibble(
        x_ = r$bands$x,
        lwr=r$bands$upr,
        upr=r$bands$lwr
      )

  } else {
    # cal tibble holds information on areas of (mis)calibration
    # use equidistant  points (700) in addition to the important areas to get nicely looking transitions
    add_points <-
      r$cal %>% dplyr::select(min_x,max_x) %>%
      tidyr::pivot_longer(everything(), names_to = c(".value", "set"),
                  names_pattern = "(.)(.)")

    if(identical(cut.bands,T)){
      add_points <- add_points %>% dplyr::filter(m >= min(r$cases$x) & m <= max(r$cases$x))
      }

    band.length <-
      c(seq(
        from=ifelse(identical(cut.bands,T), min(r$cases$x),0),
        to=ifelse(identical(cut.bands,T), max(r$cases$x),1),
        length.out = points
      ),
      add_points$m
      ) %>% sort()


    p_dat <-
      tibble(
        x_ = band.length,
        lwr=interpolate(x=r$bands$x, y= r$bands$lwr, xout=band.length, right = 0),
        upr=interpolate(x=r$bands$x, y= r$bands$upr, xout=band.length, right = 1)
      )
  }

  if(diag=="default"){
    p <- p +  ggplot2::geom_segment(ggplot2::aes(x=0,y=0,xend=1,yend=1))
  } else {
    if(identical(cut.bands,T)){
     diag_dat <-  r$cal %>%
       filter(
         max_x <= max(r$cases$x) &  min_x >= min(r$cases$x)
         )
    } else {
      diag_dat <-  r$cal
    }
    p <-p +
      ggplot2::geom_segment(
        diag_dat,
        mapping=ggplot2::aes(
          x=min_x,xend=max_x,
          y=min_x, yend=max_x, color=out),
        lineend="butt"
      )+
      ggplot2::scale_colour_gradient(
        low = "gray", high = "red", guide = "none", limits=c(0,1)
        )
  }

  if(is.null(approx.equi)){
  p <- p +
    ggplot2::geom_step(
      p_dat,
      mapping = ggplot2::aes(x=x_,y=lwr), direction = "vh"
      )+
    ggplot2::geom_step(
      p_dat,
      mapping = ggplot2::aes(x=x_, y=upr)
      )
  } else {
    p <- p +
      ggplot2::geom_step(
        p_dat,
        mapping = ggplot2::aes(x=x_,y=lwr)
      )+
      ggplot2::geom_step(
        p_dat,
        mapping = ggplot2::aes(x=x_, y=upr),  direction = "vh"
      )
  }


  if(is.null(shaddow)){
    p <- p +
      ggplot2::geom_ribbon(
        p_dat,
        mapping = ggplot2::aes(ymin=lwr,ymax=upr,x=x_), color = NA,
        alpha=.1, fill = "blue",stat=ggalt::StatStepribbon,
        direction = "vh"
        ) +
      ggplot2::geom_ribbon(
        p_dat,
        mapping = ggplot2::aes(ymin=lwr,ymax=upr,x=x_), color = NA,
        alpha=.1, fill = "blue",stat=ggalt::StatStepribbon,
        direction = "hv"
      )

  }


  if(is.null(isoreg)){
    p<- p+
      ggplot2::geom_line(
        data= tidyr::pivot_longer(
          r$bins,
          cols = dplyr::all_of(c("x_min", "x_max")
          ),
          values_to = "x"),
        ggplot2::aes(y=CEP_pav,x=x))
  }


  p <- p+
    ggplot2::xlab("Predicted probability")+
    ggplot2::ylab("Conditional event probability")+
    ggplot2::theme_bw()+
    ggplot2::coord_fixed(ratio = 1)

  p
}


#' Plotting monotone confidence bands
#'
#' @param x output of the \code{calibrationband} functions.
#' @param ... further arguments to be passed to or from methods;
#' @param plot if \code{TRUE}, a simple plot of the confidence bands is
#'     displayed (default \code{FALSE}).
#' @param add if \code{TRUE}, adds the bands to an existing plot (default
#'     \code{FALSE}).
#' @param col color of bands.
#'
#' @details
#' When plotting the monotone confidence band, the upper bound should be
#' extended to the left, that is, the bound at \code{x[i]} is valid on the
#' interval \code{(x[i-1],x[i]]}. The lower bound should be extended to the
#' right, i.e. the bound at x[i] is extended to the interval \code{[x[i],x[i +
#' 1])}. This function creates x and y values for correct plotting of these
#' bounds.
#'
#' @return
#' A list containing the \code{x,y} to be called in the function \code{plot} to
#' plot the confidence band, separated for the upper and lower bound (returned
#' invisibly).
#'
#' @author
#' Alexander Henzi
#' @export

plot.calibrationband <- function(x, ...,
                                 plot = FALSE, add = FALSE, col = "black") {
  x_ <- x$bands$x
  upr <- x$bands$upr
  lwr <- x$bands$lwr

  m <- length(x_)
  ind_1 <- c(1, rep(2:m, each = 2))
  ind_2 <- c(rep(seq_len(m - 1), each = 2), m)
  upr <- upr[ind_1]
  lwr <- lwr[ind_2]

  x_lwr <- x_[ind_1]
  x_upr <- x_[ind_2]

  # plot
  if (plot) {
    if (add) {
      points(x_lwr, lwr, type = "l", col = col)
    } else {
      plot(
        x_lwr,
        lwr,
        type = "l",
        col = col,
        ylim = c(0, 1),
        xlab = "Predicted probability",
        ylab = "Conditional event probability"
      )
    }
    points(x_upr, upr, type = "l",  col = col)
  }

  # export
  invisible(list(x_lwr = x_lwr, x_upr = x_upr, lwr = lwr, upr = upr))
}
