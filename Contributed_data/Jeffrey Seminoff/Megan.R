rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(lubridate)
library(MASS)

#changed to read.csv as have old version of R on the computer I was using, change back to read_csv for your version
#changed to dat, as "data" is a function
dat<-read.csv("PAN.Spreadsheet.aeb.csv")

#re-organized the spreadsheet

#want a lme, with day as the random effect
library(nlme)

m1 <- lme(HV ~ Year, data=dat, random=~1|Unique.Day)

#check the model fit
plot(m1)
qqnorm(m1)

#homogeneity of variance not supported, so modelling with Day as a random effect, and the variance structure in each Year
m2 <- lme(HV ~ Year, weights=varIdent(form=~1|Year),data=dat, random=~1|Unique.Day)
#m2 is much better
anova(m1,m2)

plot(m2) 
qqnorm(m2) #to look at residuals and Q-Qplot
summary(m2) #take a snapshot for the form

#you can calculate % difference from the summary table directly, or by using predict (which is nice for complicated models so showing you here)
no.covid<-data.frame(Year=2019)
covid<-data.frame(Year=2020)

#you can calculate from the summary table and backtransform, or get predictions using predict, and I think we just need the main effect, i.e., % difference as a covid effect which can then be weighted using the df and t-statistic
no.cov.pred<-predict(m2,no.covid,level=0)
cov.pred<-predict(m2,covid,level=0)

#estimate how much % change
round(((cov.pred-no.cov.pred)/no.cov.pred)*100)
#so according to this approach, you have a 194% increase in activity during covid period



