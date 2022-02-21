library(calibrationband)
library(dplyr)
s=.8
k=20
n=10000
x <- runif(n)
#x <- (sample(1:k, n, replace=TRUE) - 0.5)/k


p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)


isoba <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method = "round", digits = 3)
isoba
summary(isoba)

ggsave("/Users/mariuspuke/Downloads/calibatest.pdf", width = 5, height = 5)

autoplot(isoba, diag = "red",approx.equi=NULL)
autoplot(isoba, diag = 1, points = 700,approx.equi=1, cut.bands = F)
autoplot(isoba, diag = 1, points = 700,approx.equi=1, cut.bands = T)

ep <- plot(isoba)
ep$x_lwr
autoplot(isoba, diag = "red", points = 700,approx.equi=NULL)+
  geom_line(mapping=aes(x=ep$x_lwr, y=ep$lwr), color = "green")+
  geom_line(mapping=aes(x=ep$x_upr, y=ep$upr), color = "green")






autoplot(isoba, diag = 1, points = 700,approx.equi=1, cut.bands = F)
autoplot(isoba, diag = 1, points = 700,approx.equi=1, cut.bands = T)

plot(isoba, plot=T)

ep <- plot(isoba)
ep$x_lwr
autoplot(isoba, diag = "red", points = 700,approx.equi=NULL)+
  geom_line(mapping=aes(x=ep$x_lwr, y=ep$lwr), color = "green")+
  geom_line(mapping=aes(x=ep$x_upr, y=ep$upr), color = "green")


autoplot(isoba, diag = "red", approx.equi=1)+
  geom_line(mapping=aes(x=ep$x_lwr, y=ep$lwr), color = "green")+
  geom_line(mapping=aes(x=ep$x_upr, y=ep$upr), color = "green")+
  geom_point(mapping=aes(x=isoba$bands$x,y=isoba$bands$lwr))+
  geom_point(mapping=aes(x=isoba$bands$x,y=isoba$bands$upr))


ggplot()+
  ggplot2::geom_ribbon(isoba$bands,mapping=aes(x=x, ymin=lwr,ymax=upr),
              stat=ggalt::StatStepribbon, direction="hv")


ggplot()+
layer(
  geom = "ribbon", stat = "identity", position = "identity", data=isoba$bands,mapping=aes(x=x, ymin=lwr,ymax=upr), direction = "hv"
)

ggplot()+
geom_ribbon(data = isoba$bands,mapping = aes(x=x, ymin=lwr,ymax=upr),
                       stat = "stepribbon",  direction = "hv")
