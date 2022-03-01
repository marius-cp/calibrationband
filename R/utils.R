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

