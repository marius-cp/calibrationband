#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom ggplot2 autolayer
#' @export
ggplot2::autolayer

#' Plotting monotone confidence bands
#'
#' Uses the \pkg{ggplot2} package to illustrate monotone confidence bands to assess calibration of
#' prediction methods that issue probability forecasts.
#'
#' @param object object of class \code{calibrationband}
#' @param x object of class \code{calibrationband}
#' @param approx.equi If \code{NULL}, the bands are drawn for each
#' prediction-realization pair. If it is a scalar, say \code{z},  the bounds are
#' approximated at \code{z} equidistant point on the x-axis. Also see the effect of
#' \code{cut.bands} if a scalar is specified.
#'
#' In large data sets, \code{approx.equi = NULL}  might result in
#' capacity-consuming plots. In these cases, we recommend to set \code{approx.equi}
#'  equal to a value that is at least 200.
#'
#' Note, we add important additional points the initial scalar of
#' \code{approx.equi} to assure accurate transition areas (changes between
#' miscalibrated and calibrated areas).
#'
#' @param p_isoreg  If non \code{NULL} the isotonic regression curve is drawn.
#' Contains a list of arguments for \code{ggplot2::geom_line}. See details for default list settings.
#' @param p_diag  If non \code{NULL}, the diagonal line is drawn.
#' Contains list of arguments for \code{ggplot2::geom_segment}.
#' @param p_ribbon If non \code{NULL}, a ribbon is drawn. Contains a list of
#'  arguments for \code{ggplot2::geom_polygon}. See details for default list settings.
#' @param cut.bands Cut the bands at most extreme prediction values.
#' Bands will not be extended to 0 and 1 respectively if option is set equal to true.
#' @param ... Further arguments to be passed to or from methods.
#' @return An object inheriting from class \code{'ggplot'}.
#' @details
#' When plotting the monotone confidence band, the upper bound should be
#' extended to the left, that is, the bound at \code{x[i]} is valid on the
#' interval \code{(x[i-1],x[i]]}. The lower bound should be extended to the
#' right, i.e. the bound at x[i] is extended to the interval \code{[x[i],x[i +
#' 1])}. This function creates x and y values for correct plotting of these
#' bounds.
#'
#' \code{autoplot} behaves like any \code{ggplot() + layer()} combination.
#' That means, customized plots should be created using \code{autoplot} and
#' \code{autolayer}.
#'
#' Setting any of the \code{p_*} arguments to \code{NA} disables that layer.
#'
#' Default parameter values for \code{p_*}
#'
#' \tabular{ll}{
#' \code{p_isoreg} \tab \code{list(color = "darkgray")} \cr
#' \code{p_diag} \tab \code{list(color = "black", fill="blue", alpha = .1)} \cr
#' \code{p_ribbon} \tab \code{ list(low = "gray", high = "red", guide = "none", limits=c(0,1))}
#' }
#' @examples
#' s=.8
#' n=10000
#' x <- sort(runif(n))
#'
#' p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
#' dat <- data.frame(pr=x, y=rbinom(n,1,p(x,s)))
#'
#' cb <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method="round", digits =3)
#'
#' #simple plotting
#' plot(cb)
#' autoplot(cb)
#'
#' #customize the plot using  ggplot2::autolayer
#' autoplot(
#' cb,
#' approx.equi=NULL,
#' p_ribbon = NA
#' )+
#' ggplot2::autolayer(
#' cb,
#' p_ribbon = list(alpha = .3, fill = "gray", colour = "blue"),
#' )
#' @name plot.calibrationband
NULL




#' @rdname plot.calibrationband
#'
#' @export
autoplot.calibrationband <- function(object, ...,
                                     approx.equi=NULL,
                                     cut.bands=FALSE,
                                     p_ribbon = NULL,
                                     p_isoreg = NULL,
                                     p_diag = NULL){
  r <- object

  p <- ggplot2::ggplot()+
    autolayer.calibrationband(object=object, ...,
                              approx.equi=approx.equi,
                              cut.bands=cut.bands,
                              p_diag = p_diag,
                              p_isoreg=p_isoreg,
                              p_ribbon=p_ribbon
                              )+
    ggplot2::xlab("Predicted probability")+
    ggplot2::ylab("Calibration curve")+
    ggplot2::theme_bw()+
    ggplot2::xlim(c(0,1))+
    ggplot2::ylim(c(0,1))+
    ggplot2::coord_fixed(ratio = 1)
  p

}

#' @rdname plot.calibrationband
#'
#' @export


autolayer.calibrationband <-
  function(object, ...,
           approx.equi=NULL,
           cut.bands=FALSE,
           p_diag = NA,
           p_isoreg = NA,
           p_ribbon=NA
           ){

    r <- object

    # warnings
    if(is.vector(approx.equi)&length(approx.equi)>1){
      warning("You choose approx.equi as vector. This option is not yet available. We continue with 'approx.equi=500' instead.", call. = FALSE)
      approx.equi = 500
      }

    if({!is.null(approx.equi)&sum(approx.equi)<200}){ # non NULL and smaller than 200?
      warning("You choose approx.equi as a scalar smaller than 200. We proceed with 'approx.equi=500' instead.", call. = FALSE)
      approx.equi = NULL
    }

    # construct plot data
    if(is.null(approx.equi)){
      if(isTRUE(cut.bands)){
        p_dat <-  r$bands %>%
          dplyr::filter(
            x <= max(r$cases$x) &  x >= min(r$cases$x)
          )
      } else{
        p_dat <- r$bands
      }
    } else if(is.vector(approx.equi)&length(approx.equi)==1){
      add_points <-
        r$cal %>% dplyr::select(min_x,max_x) %>%
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_to = c(".value", "set"),
          names_pattern = "(.)(.)"
          )

      if(isTRUE(cut.bands)){
        add_points <- add_points %>%
          dplyr::filter(m >= min(r$cases$x) & m <= max(r$cases$x))
      }

      band.length <-
        c(seq(
          from=ifelse(identical(cut.bands,TRUE), min(r$cases$x),0),
          to=ifelse(identical(cut.bands,TRUE), max(r$cases$x),1),
          length.out = approx.equi
        ),
        add_points$m
        ) %>%
        sort()


      p_dat <-
        tibble::tibble(
          x = band.length,
          lwr=interpolate(x=r$bands$x, y= r$bands$lwr, xout=band.length, right = 0),
          upr=interpolate(x=r$bands$x, y= r$bands$upr, xout=band.length, right = 1)
        )
    }
    #else if(is.vector(approx.equi)&length(approx.equi)>1){
      # add_points <-
      #   r$cal %>% dplyr::select(min_x,max_x) %>%
      #   tidyr::pivot_longer(
      #     tidyselect::everything(),
      #     names_to = c(".value", "set"),
      #     names_pattern = "(.)(.)")
      #
      # band.length <-
      #   tibble::tibble(m = c(approx.equi,add_points$m)) %>%
      #   dplyr::arrange(m)
      #
      # if(isTRUE(cut.bands)){
      #   band.length <- band.length %>%
      #     dplyr::filter(m >= min(r$cases$x) & m <= max(r$cases$x))
      # }
      #
      # band.length <- band.length %>% dplyr::pull()
      #
      # p_dat <-
      #   tibble::tibble(
      #     x = band.length,
      #     lwr=interpolate(x=r$bands$x, y= r$bands$lwr, xout=band.length, right = 0),
      #     upr=interpolate(x=r$bands$x, y= r$bands$upr, xout=band.length, right = 1)
      #   )
    #}

      if(identical(cut.bands,TRUE)){
        diag_dat <-  r$cal %>%
          dplyr::select(!range)%>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            min_x = ifelse(min(r$cases$x) >= min_x & min(r$cases$x) <= max_x, min(r$cases$x), min_x),
            max_x = ifelse(max(r$cases$x) >= min_x & max(r$cases$x) <= max_x, max(r$cases$x), max_x),
            #ext = F # extended
          ) %>%
          dplyr::filter(
            max_x <= max(r$cases$x) &  min_x >= min(r$cases$x)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(min_x)
      } else {
        diag_dat <-  r$cal
      }


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
              x = .data$x, y=.data$isoy)),
            p_isoreg
          )))
    }

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

    layerlist
  }



#' @rdname plot.calibrationband
#'
#' @export

plot.calibrationband <- function(x, ...) {
  p <- autoplot(
    x,
    ...
    )
  print(p)
}
