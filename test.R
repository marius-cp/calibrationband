library(calibrationband)
library(dplyr)
s=.8
k=20
n=10000
x <- runif(n)
k=20
x <- (sample(1:k, n, replace=TRUE) - 0.5)/k


p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)


isoba <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method = "round", digits = 3)
summary(isoba)
isoba$bands

ep.iso <- plot(isoba)
autoplot(isoba, diag = "red",approx.equi=F, cut.bands = T)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")

p <- p+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")
p

autoplot(isoba, diag = "red",approx.equi=F, cut.bands = F)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")


autoplot(isoba, diag = "red",approx.equi=T, cut.bands = F, points = 20)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")

autoplot(isoba, diag = "red",approx.equi=T, cut.bands = T, points = 20)+
  geom_line(mapping=aes(x=ep.iso$x_lwr, y=ep.iso$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.iso$x_upr, y=ep.iso$upr), color = "green")
coord_cartesian(xlim = c(0, .2), ylim = c(0,.2))


xlim(0,.2)+
  ylim(0,.2)

nc <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method = "round", digits = 3, nc=T)
nc$bands

ep.nc <- plot(nc)
ep.nc$x_lwr
autoplot(nc, diag = "red", points = 100,approx.equi=T, cut.bands = T)+
  geom_line(mapping=aes(x=ep.nc$x_lwr, y=ep.nc$lwr), color = "green")+
  geom_line(mapping=aes(x=ep.nc$x_upr, y=ep.nc$upr), color = "green")+
  xlim(0,.2)+
  ylim(0,.2)+
  geom_point(aes(x=dat$pr %>% unique(), y = dat$cep %>% unique()))+
  geom_point(mapping=aes(x=nc$bands$x , y=nc$bands$lwr), color = "green")+
  geom_point(mapping=aes(x=nc$bands$x , y=nc$bands$upr), color = "green")



  geom
dat$pr %>% unique()
geom_line(mapping=aes(x=x_lwr, y=lwr), color = "red")+
  geom_line(mapping=aes(x=x_upr, y=upr), color = "red")

# correct nc

nc.cor <- nc$bands %>% filter(x %in% seq(0,1,1/1000))
x_ <- nc.cor$x
upr <- nc.cor$upr
lwr <- nc.cor$lwr

m <- length(x_)
ind_1 <- c(1, rep(2:m, each = 2))
ind_2 <- c(rep(seq_len(m - 1), each = 2), m)
upr <- upr[ind_1]
lwr <- lwr[ind_2]

x_lwr <- x_[ind_1]
x_upr <- x_[ind_2]

ggplot()+
  geom_line(mapping=aes(x=x_lwr, y=lwr), color = "green")+
  geom_line(mapping=aes(x=x_upr, y=upr), color = "green")+
  ggplot2::geom_step(mapping = ggplot2::aes(x=x_lwr,y=lwr), direction = "vh")+



