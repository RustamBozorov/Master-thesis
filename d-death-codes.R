library(tidyverse)
#library(lubridate)
#library(skimr)
#library(ggthemes)
x <- 0:40
dee <- function(x, a,b,c, t0=0){
  y <- ifelse(x>=t0, a*exp((x-t0)/b)/(1+c*exp((x-t0)/b)),0)
}
#formulas and graphs
ggplot()+geom_line(aes(x,dee(x, 1,1,1)), size=1.2)+
  xlab("t")+ylab("D(t)")+
  ggtitle("'Default' D function with a=1, b=1, c=1 at time zero")+
  theme_bw()

ggplot()+geom_line(aes(x,dee(x, 10,1,1)), size=1.2)+
  geom_line(aes(x,dee(x, 1,1,1)), size=0.5, linetype=3)+
  xlab("t")+ylab("D(t)")+
  ggtitle("D function with a=10, b=1, c=1")+
  theme_bw()
ggplot()+geom_line(aes(x,dee(x, 1,5,1)), size=1.2)+
  geom_line(aes(x,dee(x, 1,1,1)), size=0.5, linetype=3)+
  xlab("t")+ylab("D(t)")+
  ggtitle("D function with a=1, b=5, c=1")+theme_bw()
ggplot()+geom_line(aes(x,dee(x, 1,1,0.1)), size=1.2)+
  geom_line(aes(x,dee(x, 1,1,1)), size=0.5, linetype=3)+
  xlab("t")+ylab("D(t)")+
  ggtitle("D function with a=1, b=1, c=0.1")+theme_bw()
ggplot()+geom_line(aes(x,dee(x, 1,1,1,20)), size=1.2)+
  geom_line(aes(x,dee(x, 1,1,1)), size=0.5, linetype=3)+
  xlab("t")+ylab("D(t)")+
  ggtitle("D function with a=1, b=1, c=1 at time t=20")+theme_bw()

##d prime function

dprime <- function(x, a,b,c, t0=0){
  y <- ifelse(x>=t0, a*exp((x-t0)/b)/(b*(1+c*exp((x-t0)/b))^2),0)
}
#formulas
ggplot()+geom_line(aes(x,dprime(x, 1,1,0.01)), size=1.2)+
  xlab("t")+ylab("D'(t)")+
  ggtitle("'Default' D'(t) function with a=1, b=1, c=0.01 at time zero")+
  theme_bw()

ggplot()+geom_line(aes(x,dprime(x, 10,1,0.01)), size=1.2)+
  geom_line(aes(x,dprime(x, 1,1,0.01)), size=0.5, linetype=3)+
  xlab("t")+ylab("D'(t)")+
  ggtitle("D'(t) function with a=10, b=1, c=0.01")+
  theme_bw()
ggplot()+geom_line(aes(x,dprime(x, 1,5,0.01)), size=1.2)+
  geom_line(aes(x,dprime(x, 1,1,0.01)), size=0.5, linetype=3)+
  xlab("t")+ylab("D'(t)")+
  ggtitle("D'(t) function with a=1, b=5, c=0.01")+theme_bw()
ggplot()+geom_line(aes(x,dprime(x, 1,1,0.001)), size=1.2)+
  geom_line(aes(x,dprime(x, 1,1,0.01)), size=0.5, linetype=3)+
  xlab("t")+ylab("D'(t)")+
  ggtitle("D'(t) function with a=1, b=1, c=0.001")+theme_bw()
ggplot()+geom_line(aes(x,dprime(x, 1,1,0.01,20)), size=1.2)+
  geom_line(aes(x,dprime(x, 1,1,0.01)), size=0.5, linetype=3)+
  xlab("t")+ylab("D'(t)")+
  ggtitle("D'(t) function with a=1, b=1, c=0.01 at time t=20")+theme_bw()

## double dee function

ggplot()+geom_line(aes(x,y=dee(x, 1.5,1,1) + dee(x, 1,5,0.8)), size=1.2)+
  geom_line(aes(x,dee(x, 1.5,1,1)), size=0.7, linetype=3)+
  geom_line(aes(x,dee(x, 1,5,0.8)), size=0.7, linetype=2)+
  xlab("t")+ylab("D(t)")+
  ggtitle("Dn(t) function with n=2")+
  annotate("text", x=25, y=1.6, label="D(1.5, 1, 1)")+
  annotate("text", x=25, y=1.1, label="D(1, 5, 0.8)")+
  annotate("text", x=25, y=2.9, label="D(1.5, 1, 1)+D(1, 5, 0.8)")+
  theme_bw()

  ## double dprime function
ggplot()+geom_line(aes(x,y=dprime(x, 1,4,0.005)+dprime(x, 2,2,0.008)), size=1.2)+
    geom_line(aes(x,dprime(x, 1,4,0.005)), size=0.7, linetype=3)+
  geom_line(aes(x,dprime(x,  2,2,0.008)), size=0.7, linetype=2)+
    xlab("t")+ylab("D'(t)")+
    ggtitle("D'n(t) function with n=2")+
  annotate("text", x=7.5, y=3.2, label="D'(1,4,0.005)")+
  annotate("text", x=22, y=2, label="D'(2,2,0.008)")+
  annotate("text", x=23, y=17, label="D'(1,4,0.005)+D'(2,2,0.008) ")+
    theme_bw()  
  

