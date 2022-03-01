#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom ggplot2 autolayer
#' @export
ggplot2::autolayer

#' Plotting monotone confidence bands
#' @param object object of class calibrationband
#' @param approx.equi If \code{NULL}, the bands are drawn for each
#' prediction-realization pair. If it is a scalar, say \code{z},  the bounds are
#' approximated at \code{z} equidistant point on the x-axis. Also see the effect of
#' \code{cut.bands} if a scalar is specified. If  \code{approx.equi} is a vector
#' with entries \code{a_1, \dots, a_i in (0,1)}, the bounds are approximated at
#' these particular points.
#'
#' In large data sets, \code{approx.equi = NULL}  might result in
#' capacity consuming plots. In these cases we recommend to consider the other
#' choices.
#' Note, in these cases and for  accurate illustration in transition areas
#' (changes between miscalibrated and calibrated areas) we add important
#' additional points the initial vector/scalar of  \code{approx.equi}.
#' @param p_isoreg  If non \code{NULL} the istonic regression curve is drawn.
#' Contains a list of arguments for \code{ggplot2::geom_line}.
#' @param p_diag  If non \code{NULL}, the diagonal line is drwan.
#' Contains list of arguments for \code{ggplot2::geom_segment}.
#' @param p_ribbon If non \code{NULL}, a ribbon is drwan. Contains a list of
#'  arguments for \code{ggplot2::geom_polygon}.
#' @param cut.bands Cut the bands at most extreme prediction values.
#' Bands will not be extended to 0 and 1 respectively if option is set equal to true.
#' Note, that has no effect if  \code{approx.equi} is a vector.
#' @param ... further arguments to be passed to or from methods;
#' @details
#' \code{autoplot} behaves like any \code{ggplot() + layer()} combination. T
#' That means, customized plots should be created using \code{autoplot} and
#' \code{autolayer}.
#'
#' Setting any of the \code{p_*} arguments to \code{NA} disables that layer.
#'
#' Default parameter values
#'
#' \tabular{ll}{
#' \code{p_isoreg} \tab \code{list(color = "darkgray")} \cr
#' \code{p_diag} \tab \code{list(color = "black", fill="blue", alpha = .1)} \cr
#' \code{p_ribbon} \tab \code{ list(low = "gray", high = "red", guide = "none", limits=c(0,1))}
#' }
# @examples
#  s=.8
#  n=10000
#  x <- sort(runif(n, .05, .95))
#
#  p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
#  dat <- data.frame(pr=x, s=s, cep = p(x,s), y=rbinom(n,1,p(x,s)))
#
#  isoba <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method = "round", digits = 3)
#
# autoplot(isoba)
# autoplot(isoba,approx.equi=NULL, cut.bands = T,
# p_ribbon = NA,
# p_isoreg = NA,
# p_diag = NA
# )+
# ggplot2::autolayer(isoba,cut.bands = T,
# p_ribbon = list(alpha = .1, fill = "red", colour = "green"),
# p_isoreg = list(linetype = "dashed"),
# p_diag = list(low = "red", high = "blue", guide = "none", limits=c(0,1))
# )





#' @rdname autoplot.calibrationband
#' @export
#'
autoplot.calibrationband <- function(object, ...,
                                     approx.equi=NULL,
                                     cut.bands=F,
                                     p_ribbon = NULL,
                                     p_isoreg = NULL,
                                     p_diag = NULL){
  # return a ggplot 2 object of Isobands
  r <- object

  p <- ggplot2::ggplot()+
    autolayer.calibrationband(object=object, ...,
                              approx.equi=approx.equi,
                              cut.bands=cut.bands,
                              p_ribbon=p_ribbon,
                              p_isoreg=p_isoreg,
                              p_diag = p_diag
    )+
    ggplot2::xlab("Predicted probability")+
    ggplot2::ylab("Calibration Curve")+
    ggplot2::theme_bw()+
    ggplot2::xlim(c(0,1))+
    ggplot2::ylim(c(0,1))+
    ggplot2::coord_fixed(ratio = 1)
  p

}

#' @rdname autoplot.calibrationband
#'
#' @export


autolayer.calibrationband <-
  function(object, ...,
           approx.equi=NULL,
           cut.bands=F,
           p_ribbon=NA,
           p_isoreg = NA,
           p_diag = NA
           ){

    r <- object

    # construct plot data
    if(is.null(approx.equi)){
      if(isTRUE(cut.bands)){
        p_dat <-  r$bands %>%
          filter(
            x <= max(r$cases$x) &  x >= min(r$cases$x)
          )
      } else{
        p_dat <- r$bands
      }
    } else if(is.vector(approx.equi)&length(approx.equi)==1){
      # cal tibble holds information on areas of (mis)calibration
      # use equidistant  points (700) in addition to the important areas to get nicely looking transitions
      add_points <-
        r$cal %>% dplyr::select(min_x,max_x) %>%
        tidyr::pivot_longer(everything(), names_to = c(".value", "set"),
                            names_pattern = "(.)(.)")

      if(isTRUE(cut.bands)){
        add_points <- add_points %>%
          dplyr::filter(m >= min(r$cases$x) & m <= max(r$cases$x))
      }

      band.length <-
        c(seq(
          from=ifelse(identical(cut.bands,T), min(r$cases$x),0),
          to=ifelse(identical(cut.bands,T), max(r$cases$x),1),
          length.out = approx.equi
        ),
        add_points$m
        ) %>%
        sort()


      p_dat <-
        tibble(
          x = band.length,
          lwr=interpolate(x=r$bands$x, y= r$bands$lwr, xout=band.length, right = 0),
          upr=interpolate(x=r$bands$x, y= r$bands$upr, xout=band.length, right = 1)
        )
    } else if(is.vector(approx.equi)&length(approx.equi)>1){
      # warn here if to less entries in vector
      add_points <-
        r$cal %>% dplyr::select(min_x,max_x) %>%
        tidyr::pivot_longer(everything(), names_to = c(".value", "set"),
                            names_pattern = "(.)(.)")

      band.length <-
        tibble(m = c(approx.equi,add_points$m)) %>%
        arrange(m)

      if(isTRUE(cut.bands)){
        band.length <- band.length %>%
          dplyr::filter(m >= min(r$cases$x) & m <= max(r$cases$x))
      }

      band.length <- band.length %>% pull()

      p_dat <-
        tibble(
          x = band.length,
          lwr=interpolate(x=r$bands$x, y= r$bands$lwr, xout=band.length, right = 0),
          upr=interpolate(x=r$bands$x, y= r$bands$upr, xout=band.length, right = 1)
        )
    }

    #if(diag!="default"){
      if(identical(cut.bands,T)){
        diag_dat <-  r$cal %>%
          select(!range)%>%
          rowwise() %>%
          mutate(
            min_x = ifelse(min(r$cases$x) >= min_x & min(r$cases$x) <= max_x, min(r$cases$x), min_x),
            max_x = ifelse(max(r$cases$x) >= min_x & max(r$cases$x) <= max_x, max(r$cases$x), max_x),
            #ext = F # extended
          ) %>%
          filter(
            max_x <= max(r$cases$x) &  min_x >= min(r$cases$x)
          ) %>%
          ungroup() %>%
          arrange(min_x)
      } else {
        diag_dat <-  r$cal
      }
   # }


    layerlist <- list()

    # construct plot data for upr/lwr bound and ribbon
    connect.points <- p.dat(bands=p_dat)
    ribbon.dat = ribbon(p.dat = connect.points)

    if(is.null(p_ribbon)){
      p_ribbon <- list(color = "black", fill="blue", alpha = .1)
    }
    if(is.null(p_isoreg)){
      p_isoreg <- list(color = "darkgray")
    }

    if(is.null(p_diag)){
      p_diag <- list(low = "gray", high = "red", guide = "none", limits=c(0,1))
    }

    # add layers
    if (!isTRUE(is.na(p_ribbon))) {
      layerlist <- c(
        layerlist,
        do.call(
          what = ggplot2::geom_polygon,
          args = c(
            list(data = ribbon.dat),
            list(mapping = ggplot2::aes(
              x = .data$x_lwr,
              y=.data$lwr)
            ),
            p_ribbon
          )
        )
      )
    }

    if (!isTRUE(is.na(p_isoreg))) {
      layerlist <- c(
        layerlist,
        do.call(
          what = ggplot2::geom_line,
          args = c(
            list(data = tidyr::pivot_longer(
              r$bins,
              cols = dplyr::all_of(c("x_min", "x_max")),
              values_to = "x")
            ),
            list(mapping = ggplot2::aes(
              x = .data$x, y=.data$CEP_pav)),
            p_isoreg
          )))
    }

    if(!isTRUE(is.na(p_diag))) {
      layerlist <- c(
        layerlist,
        do.call(
          what = ggplot2::geom_segment,
          args = c(
            list(data = diag_dat),
            list(mapping = ggplot2::aes(
              x=.data$min_x,xend=.data$max_x,
              y=.data$min_x, yend=.data$max_x,
              color = out,
            ))))
      )

      layerlist <- c(
        layerlist,
        do.call(
          what = ggplot2::scale_colour_gradient,
          p_diag))
    }

    layerlist
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
