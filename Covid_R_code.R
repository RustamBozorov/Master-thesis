## Loading libraries
library(tidyverse)
library(lubridate)
library(skimr)
## loading data and skim
covid <- read.csv("data28102020.csv")
skim(covid)
head(covid)
covid <- covid %>% mutate(Dat=dmy(dateRep))
spain <- covid %>% filter(geoId=="ES") %>% arrange(Dat)
spain <- spain %>% mutate(daynumber=1:nrow(spain))
skim(spain)
## initial graph daily cases
ggplot()+geom_point(data = spain,aes(x=Dat,y=cases))+
  geom_line(data = spain,aes(x=Dat,y=cases))+xlab("Date")+
  ylab("Cases")+ggtitle("Daily cases in Spain according to ECDC")+
  theme_bw()




nrow(spain %>% filter(cases==0 | cases<0))
nrow(spain %>% filter( cases<0))
## create cases regularized numbers
casesreg <- vector("numeric", length = nrow(spain))
for(i in 148:nrow(spain)){
  if(wday(spain$Dat[i])==7 && spain$cases[i]==0){
    k <- spain$cases[i-1]/spain$cases[i-2]
    casesreg[i] <-  round(spain$cases[i+2]/(k*k+k+1))
  } else if(wday(spain$Dat[i])==1 && spain$cases[i]==0){
    casesreg[i] <- round(casesreg[i-1]*(casesreg[i-2]/casesreg[i-3]))
  } else if(wday(spain$Dat[i])==2 && spain$cases[i-1]==0){
    casesreg[i] <- spain$cases[i] - casesreg[i-1]-casesreg[i-2]
  } else{
    casesreg[i] <- spain$cases[i]
  } 
}
for(i in 1:147){
  casesreg[i] <- spain$cases[i]
}
casesreg[107] <- spain$cases[107]-750
casesreg[108] <- spain$cases[108]-750
casesreg[109] <- spain$cases[109]-750
casesreg[110] <- spain$cases[110]-750
casesreg[111] <- spain$cases[111]+4*750
casesreg[144] <- round((spain$cases[144]+spain$cases[147])/2)
casesreg[147] <- spain$cases[144]+spain$cases[147]-casesreg[144]
casesreg

ggplot()+geom_point(data = spain,aes(x=Dat,y=casesreg), alpha=0.5)+
  geom_line(data = spain,aes(x=Dat,y=casesreg),alpha=0.5)+
  geom_smooth(data = spain,aes(x=Dat,y=casesreg),
              method = "gam")+
  xlab("Date")+
  ylab("Regularized cases")+ggtitle("Regularized daily cases with smooting line")+
  theme_bw()


## total cases creation


totalcases <- vector("numeric", length = nrow(spain))
for(i in 2:nrow(spain)){
  totalcases[i] <- totalcases[i-1]+casesreg[i]
}
totalcases

ggplot()+
  geom_area(data = spain,aes(x=Dat,y=totalcases),fill="gray70")+
  geom_line(data = spain,aes(x=Dat,y=totalcases))+
  xlab("Date")+  ylab("Total cases")+
  annotate(geom="text",x=spain$Dat[190], y=1098000, 
           label = "1,098,320 total cases on 26/10/2020")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

## death daily numbers

ggplot()+geom_point(data = spain,aes(x=Dat,y=deaths))+
  geom_line(data = spain,aes(x=Dat,y=deaths))+xlab("Date")+
  ylab("")+ggtitle("Daily death numbers in Spain according to ECDC")+
  theme_bw()


nrow(spain %>% filter(deaths==0 | deaths<0))
nrow(spain %>% filter( deaths<0))

## reg death creation

deathreg <- vector("numeric", length = nrow(spain))
for(i in 183:nrow(spain)){
  if(wday(spain$Dat[i])==7 && spain$deaths[i]==0){
    k <- spain$deaths[i-1]/spain$deaths[i-2]
    deathreg[i] <-  round(spain$deaths[i+2]/(k*k+k+1))
  } else if(wday(spain$Dat[i])==1 && spain$deaths[i]==0){
    deathreg[i] <- round(deathreg[i-1]*(deathreg[i-2]/deathreg[i-3]))
  } else if(wday(spain$Dat[i])==2 && spain$deaths[i-1]==0){
    deathreg[i] <- spain$deaths[i] - deathreg[i-1]-deathreg[i-2]
  } else{
    deathreg[i] <- spain$deaths[i]
  } 
}
for(i in 1:182){
  deathreg[i] <- spain$deaths[i]
}
deathreg[226] <- round((spain$deaths[226]+spain$deaths[227])/2)
deathreg[227] <- spain$deaths[226]+spain$deaths[227] - deathreg[226]
deathreg[172] <- 0
deathreg[148] <- 1
deathreg[147] <- 37
deathreg[144] <- spain$deaths[144]-494
deathreg[119] <- round((spain$deaths[119]+spain$deaths[120]+
                          spain$deaths[121])/3)
deathreg[120] <- round((spain$deaths[119]+spain$deaths[120]+
                          spain$deaths[121])/3)
deathreg[121] <- spain$deaths[119]+spain$deaths[120]+spain$deaths[121]-
  deathreg[120]-deathreg[119]


## reg death graph
ggplot()+geom_point(data = spain,aes(x=Dat,y=deathreg), alpha=0.5)+
  geom_line(data = spain,aes(x=Dat,y=deathreg),alpha=0.5)+
  geom_smooth(data = spain,aes(x=Dat,y=deathreg),
              method = "gam")+
  xlab("Date")+  ylab("Regularized death cases")+
  ggtitle("Regularized daily death cases with smooting line")+
  theme_bw()


## total death creation
totaldeath <- vector("numeric", length = nrow(spain))
for(i in 2:nrow(spain)){
  totaldeath[i] <- totaldeath[i-1]+deathreg[i]
}
totaldeath

## totaldeath graph

ggplot()+
  geom_line(data = spain,aes(x=Dat,y=totaldeath))

ggplot()+
  geom_area(data = spain,aes(x=Dat,y=totaldeath),fill="gray70")+
  geom_line(data = spain,aes(x=Dat,y=totaldeath))+
  xlab("Date")+  ylab("Total death numbers")+
  annotate(geom="text",x=spain$Dat[190], y=35000, 
           label = "35,031 total deaths on 26/10/2020")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

## d function
dee <- function(x, a,b,c, t0=0){
  y <- ifelse(x>=t0, a*exp((x-t0)/b)/(1+c*exp((x-t0)/b)),0)
}


x <- 1:nrow(spain)
abct1 <- c(2650,  11, 0.093, 75)
## abct2 <- c(111,  17.9, 0.0099, 210)

##geom_line(aes(x,dee(x, abct1[1],abct1[2],abct1[3],abct1[4])), size=0.5, linetype=2)+
##geom_line(aes(x,dee(x, abct2[1],abct2[2],abct2[3],abct2[4])), size=0.5, linetype=2)+
abct2 <- c(200, 21.5, 0.0111, 215)
ggplot()+geom_line(data = spain,
                   aes(x=daynumber,y=totaldeath),color="red")+
  geom_area(data = spain,aes(x=daynumber,y=totaldeath),fill="gray70")+
  geom_line(aes(x,y=dee(x, abct1[1],abct1[2],abct1[3],abct1[4]) +
                         dee(x, abct2[1],abct2[2],abct2[3],abct2[4])),
            size=1.2)+xlab("")+ylab("D(t)")+
  ggtitle("Total death cases lines")+
  scale_x_continuous(breaks =  NULL)+theme_bw()+
  geom_vline(xintercept = 215, linetype=2)+
  annotate(geom="text",x=140, y=30000,label = "D1(t)", size=5)+
  annotate(geom="text",x=250, y=33000, label = "D2(t)", size=5)+
  geom_text(aes(205,10000),label="01-08-2020", angle=90, size=5)

## d' lines with areas

dprime <- function(x, a,b,c, t0=0){
  y <- ifelse(x>=t0, a*exp((x-t0)/b)/(b*(1+c*exp((x-t0)/b))^2),0)
}
abct3 <- c(1950,  8.85, 0.0645, 75)
#abct3 <- c(2650,  11, 0.093, 75)
#abct4 <- c(145, 18, 0.0099, 215)
abct4 <- c(950, 42, 0.0065, 215)
ggplot()+geom_line(data = spain,
                   aes(x=daynumber,y=deathreg),color="red")+
  geom_area(data = spain,aes(x=daynumber,y=deathreg),fill="gray70")+
  geom_line(aes(x,y=dprime(x, abct3[1],abct3[2],abct3[3],abct3[4]) +
                  dprime(x, abct4[1],abct4[2],abct4[3],abct4[4])),
            size=1.2)+xlab("")+ylab("Daily death numbers")+
  ggtitle("Daily death cases lines")+
  scale_x_continuous(breaks =  NULL)+theme_bw()+
  geom_vline(xintercept = 215, linetype=2)+
  annotate(geom="text",x=55, y=330,label = "D'1(t)", size=5)+
  annotate(geom="text",x=260, y=330, label = "D'2(t)", size=5)+
  geom_text(aes(205,500),label="01-08-2020", angle=90, size=5)






