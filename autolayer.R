autoplot.calibrationband <- function(object, ...,
                                     approx.equi=NULL,
                                     #points = 500,
                                     cut.bands=FALSE,
                                     p_ribbon = NULL,
                                     p_isoreg = NULL,
                                     p_diag = NULL){
  # return a ggplot 2 object of Isobands
  r <- object

  p <- ggplot2::ggplot()+
    autolayer.calibrationband(object=object, ...,
                              approx.equi=approx.equi,
                              #points = 500,
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


autolayer.calibrationband <-
  function(object, ...,
           approx.equi=NULL,
           cut.bands=FALSE,
           p_ribbon=NULL,
           p_isoreg = NULL,
           p_diag = NULL
           ){

  r <- object


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

    p_dat <-
      tibble(
        x = band.length %>% pull(),
        lwr=interpolate(x=r$bands$x, y= r$bands$lwr, xout=band.length, right = 0),
        upr=interpolate(x=r$bands$x, y= r$bands$upr, xout=band.length, right = 1)
      )
  }

  if(diag!="default"){
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


#
# p +
#   ggplot2::geom_segment(
#     diag_dat,
#     mapping=ggplot2::aes(
#       x=min_x,xend=max_x,
#       y=min_x, yend=max_x,
#       color=out,
#       #linetype = ext
#     ),
#     lineend="butt"
#   )+
#   ggplot2::scale_colour_gradient(
#     low = "gray", high = diag, guide = "none", limits=c(0,1)
#   )

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


#' Plot data for bands (assures correct connecting point)
#' @param bands object bands from calibration_band function
#'

p.dat <- function(bands){
  r <- bands
  x_ <- r$x
  upr <- r$upr
  lwr <- r$lwr

  m <- length(x_)
  ind_1 <- c(1, rep(2:m, each = 2))
  ind_2 <- c(rep(seq_len(m - 1), each = 2), m)
  upr <- upr[ind_1]
  lwr <- lwr[ind_2]

  x_lwr <- x_[ind_1]
  x_upr <- x_[ind_2]

  p.dat = tibble(
    x_lwr = x_lwr,
    x_upr = x_upr,
    upr = upr,
    lwr = lwr
  )

  return(p.dat)

}




#' creats ploygon for ribbon
#' @param p.dat output of function p.dat above.
#' @importFrom sp SpatialLines Lines Line  coordinates
#' @importFrom methods as

ribbon <- function(p.dat){
  # Create line segments to close gaps between the lines
  ## Note: It is necessary that both line segments include the meeting points
  close_line1 <- c(min(p.dat$lwr), min(p.dat$upr))  #c(seq(min(p.dat$lwr), min(p.dat$upr), 0.0001), min(p.dat$upr))
  close_line2 <-  c(max(p.dat$lwr), max(p.dat$upr)) #c(seq(max(p.dat$lwr), max(p.dat$upr), 0.0001), max(p.dat$upr))
  closer_lower <- cbind(rep(min(p.dat$x_lwr), length(close_line1)),
                        rep(min(p.dat$x_lwr), length(close_line1)),
                        close_line1,
                        rep(min(p.dat$lwr), length(close_line1)))
  closer_upper <- cbind(rep(max(p.dat$x_lwr), length(close_line2)),
                        rep(max(p.dat$x_upr), length(close_line2)),
                        close_line2,
                        rep(max(p.dat$lwr), length(close_line2)))
  colnames(closer_lower) <- colnames(p.dat)
  colnames(closer_upper) <- colnames(p.dat)

  # Merge closing segments with p.data frame
  p.dat <- rbind(closer_lower, p.dat, closer_upper)

  # Create spetial line segments for the lower and upper border
  line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(p.dat[, c(1,4)]), ID='a')))
  line2 <- sp::SpatialLines(list(sp::Lines(sp::Line(p.dat[, c(2,3)]), ID='b')))

  # Plot the line segments to make sure they look goods
  #plot(line1)
  #plot(line2, add = T, col = "red")

  # Transform the line segments into coordinate points
  line_pts <- methods::as(line1, "SpatialPoints")
  comparison_pts <- methods::as(line2, "SpatialPoints")
  line_coords <- sp::coordinates(line_pts)
  comparison_coords <- sp::coordinates(comparison_pts)

  # Bind the coordinate points into one p.data frame
  ## Note: It is important that the points are ordered such that the polygon can be drawn clockwise, i.e. here: line_coords is ascending while comparison_coords is descending
  coords <- data.frame(
    base::rbind(
      line_coords,
      comparison_coords[dim(comparison_coords)[1]:1, ]
    )
  )

  return(ribbon.dat = coords)

}


