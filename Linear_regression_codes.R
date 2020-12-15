## Loading libraries
library(tidyverse)
library(lubridate)
library(skimr)
## loading data
covid <- read.csv("data28102020.csv")
covid <- covid %>% mutate(Dat=dmy(dateRep))
spain <- covid %>% filter(geoId=="ES") %>% arrange(Dat)
spain <- spain %>% mutate(daynumber=1:nrow(spain))
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
## total cases creation
totalcases <- vector("numeric", length = nrow(spain))
for(i in 2:nrow(spain)){
  totalcases[i] <- totalcases[i-1]+casesreg[i]
}
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
## total death creation
totaldeath <- vector("numeric", length = nrow(spain))
for(i in 2:nrow(spain)){
  totaldeath[i] <- totaldeath[i-1]+deathreg[i]
}
## d function
dee <- function(x, a,b,c, t0=0){
  y <- ifelse(x>=t0, a*exp((x-t0)/b)/(1+c*exp((x-t0)/b)),0)
}
x <- 1:nrow(spain)
abct1 <- c(2650,  11, 0.093, 75)
abct2 <- c(200, 21.5, 0.0111, 215)
## d' lines with areas
dprime <- function(x, a,b,c, t0=0){
  y <- ifelse(x>=t0, a*exp((x-t0)/b)/(b*(1+c*exp((x-t0)/b))^2),0)
}
abct3 <- c(1950,  8.85, 0.0645, 75)
abct4 <- c(950, 42, 0.0065, 215)
## reading oxford indeces
oxford <- read.csv("oxford_indeces.csv")
death233 <- deathreg[69:nrow(spain)]
cases233 <- casesreg[69:nrow(spain)]
oxford_full <- cbind.data.frame(oxford,death233,cases233)
library(caret)

## checking for no variance
oxford_full %>% select(C1,C2,C3,C4,C5,C6,C7,C8,
                       E1,E2,H1,H2,H3,H6) %>% nearZeroVar()
## H1 and H3 should be deleted
#----------------
#--------------------  for death numbers
library(MASS)

## death models-------glm nb-----------------------------

## economic support

nbdeath1 <- glm(death233 ~E1+E2,
                   data = oxford_full,
                   family= "poisson")

summary(nbdeath1)
confint(nbdeath1)
exp(cbind(Estimate = coef(nbdeath1), confint(nbdeath1)))

## containment all together

nbdeath2 <- glm.nb(death233~StringencyIndex,
                   data=oxford_full)
summary(nbdeath2)
confint(nbdeath2)
exp(cbind(Estimate = coef(nbdeath2), confint(nbdeath2)))

## containment

nbdeath3 <- glm.nb(death233~C1+C2+C3+C4+C5+C6+C7+C8,
                   data=oxford_full)
summary(nbdeath3)
confint(nbdeath3)
exp(cbind(Estimate = coef(nbdeath3), confint(nbdeath3)))

## containment and health  all together

nbdeath4 <- glm.nb(death233~ContainmentHealthIndex,
                   data=oxford_full)
summary(nbdeath4)
confint(nbdeath4)
exp(cbind(Estimate = coef(nbdeath4), confint(nbdeath4)))

## containment and health

nbdeath5 <- glm.nb(death233~ C1+C2+C3+C4+C5+C6+C7+C8+
                     H2+H6,
                   data=oxford_full)
summary(nbdeath5)
confint(nbdeath5)
exp(cbind(Estimate = coef(nbdeath5), confint(nbdeath5)))

## all together

nbdeath6 <- glm.nb(death233~ GovernmentResponseIndex,
                   data=oxford_full)
summary(nbdeath6)
confint(nbdeath6)
exp(cbind(Estimate = coef(nbdeath6), confint(nbdeath6)))

## all - final death model

nbdeath7 <- glm.nb(death233~ C1+C2+C3+C4+C5+C6+C7+C8+
                     E1+E2+H2+H6,
                   data=oxford_full)
summary(nbdeath7)
confint(nbdeath7)
exp(cbind(Estimate = coef(nbdeath7), confint(nbdeath7)))

## economic  index for death

nbdeath8 <- glm.nb(death233~ EconomicSupportIndex,
                   data=oxford_full)
summary(nbdeath8)
confint(nbdeath8)
exp(cbind(Estimate = coef(nbdeath8), confint(nbdeath8)))


#--------------------------------------------------------
## cases models-------glm nb-----------------------------

## economic support

nbcases1 <- glm(cases233 ~E1+E2,
                data = oxford_full,
                family= "poisson")

summary(nbcases1)
confint(nbcases1)
exp(cbind(Estimate = coef(nbcases1), confint(nbcases1)))

## containment all together

nbcases2 <- glm.nb(cases233~StringencyIndex,
                   data=oxford_full)
summary(nbcases2)
confint(nbcases2)
exp(cbind(Estimate = coef(nbcases2), confint(nbcases2)))

## containment

nbcases3 <- glm.nb(cases233~C1+C2+C3+C4+C5+C6+C7+C8,
                   data=oxford_full)
summary(nbcases3)
round(confint(nbcases3), digits=4)
round(exp(cbind(Estimate = coef(nbcases3), confint(nbcases3))),
      digits=4)

## containment and health  all together

nbcases4 <- glm.nb(cases233~ContainmentHealthIndex,
                   data=oxford_full)
summary(nbcases4)
confint(nbcases4)
round(exp(cbind(Estimate = coef(nbcases4), confint(nbcases4))), 
      digits=4)

## containment and health

nbcases5 <- glm.nb(cases233~ C1+C2+C3+C4+C5+C6+C7+C8+
                     H2+H6,
                   data=oxford_full)
summary(nbcases5)
round(confint(nbcases5), digits = 4)
round(exp(cbind(Estimate = coef(nbcases5), confint(nbcases5))),
      digits = 4)

## all together

nbcases6 <- glm.nb(cases233~ GovernmentResponseIndex,
                   data=oxford_full)
summary(nbcases6)
round(confint(nbcases6), digits=4)
exp(cbind(Estimate = coef(nbcases6), confint(nbcases6)))

## all - final cases model

nbcases7 <- glm.nb(cases233 ~ C1+C2+C3+C4+C5+C6+C7+C8+E1+E2+H2+H6,
                   data=oxford_full, maxit=100)
summary(nbcases7)
round(confint(nbcases7), digits = 4)
round(exp(cbind(Estimate = coef(nbcases7), confint(nbcases7))),
      digits = 4)

## economic  index for cases

nbcases8 <- glm.nb(cases233~ EconomicSupportIndex,
                   data=oxford_full)
summary(nbcases8)
confint(nbcases8)
exp(cbind(Estimate = coef(nbcases8), confint(nbcases8)))