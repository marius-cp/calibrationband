library(calibrationband)
library(dplyr)
s=.8
k=20
n=10000
x <- runif(n, .1, .9)
k=20
#x <- (sample(1:k, n, replace=TRUE) - 0.5)/k


p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)


isoba <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method = "round", digits = 3)

su <- summary(isoba)

autoplot(isoba)

isoba$bands

ep.iso <- plot(isoba)
autoplot(isoba,approx.equi=500)
autoplot(isoba,approx.equi=500, cut.bands = T)
autoplot(isoba,approx.equi=500, cut.bands = F)


pp <- autoplot(isoba,approx.equi=500, cut.bands = T)
pp

autoplot(isoba, approx.equi=seq(0,1,1/20), cut.bands = F )
autoplot(isoba, approx.equi=seq(0,1,1/20), cut.bands = T )


geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")

p <- p+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")
p

autoplot(isoba, approx.equi=NULL, cut.bands = F)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")


autoplot(isoba,approx.equi=500, cut.bands = F)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")

autoplot(isoba,approx.equi=10, cut.bands = T)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")

coord_cartesian(xlim = c(0, .2), ylim = c(0,.2))

nc <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method = "round", digits = 3, nc=T)
nc$bands

ep.nc <- plot(nc)
ep.nc$x_lwr
autoplot(nc,approx.equi=200, cut.bands = T)+
  geom_line(mapping=aes(x=ep.nc$x_lwr, y=ep.nc$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.nc$x_upr, y=ep.nc$upr), color = "green")




vec <- c(.1,.2,.3)
scal <- c(500)

is.vector(scal)  & length(scal)==1

is.numeric(vec) & length(vec)>1


is.null(NULL)


library(reliabilitydiag)
rr <- reliabilitydiag(x=dat$pr, y=dat$y)
autoplot(rr, colour = "black", params_ribbon = NA) +
 ggplot2::autolayer(rr, params_ribbon = list(fill = "grren", alpha = .5))


library(calibrationband)
autoplot(isoba,approx.equi=500, cut.bands = T)

autoplot(isoba,approx.equi=NULL, cut.bands = T,
         p_isoreg = NA,
         p_ribbon = NA,
         p_diag = NA
         )+
  ggplot2::autolayer(isoba,cut.bands = T,
                     p_diag = list(low = "red", high = "blue", guide = "none", limits=c(0,1)),
                     p_isoreg = list(linetype = "dashed"),
                    p_ribbon = list(alpha = .1, fill = "red", colour = "green"),
                     )

