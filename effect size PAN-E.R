######################################################################################################################
######################################################################################################################
######################################################################################################################

#### Load in data ####
data <- read.csv("data/test.data.PAN-E.csv")

#### Load in libraries ####
library(nlme)

####function for effect size
partial.r <- function (t.val, df) {
  r < -t.val / sqrt((t.val) ^ 2 + df)
  names(r) <- "effect size r"
  return(r)
  }

####using quasibinomial and quasipoisson because this provide a t-stastic as an output, as compared to a z-statistic
####so this is thus comparable across lm, glm, lme and glmmPQL

####Example 1, study that is a time-based regresson over the covid period
y = subset(data, Study_ID == "1")

######################################################################################################################
####can use with different data if we have a comparable "t" from general linear model or linear model

#Centering each invader abundance by StudyID is necessary for analysis/interpretability
#Schielzelth 2010 in Methods Ecol & Evol
#Centering predictor data (time in this case)

y$Treatment <- as.numeric(as.character(y$Treatment)) 
y$C.Treatment <- y$Treatment - mean(y$Treatment)

#m1 is a linear model of the response which is quasipoisson
#a.1 reports the t value effect size of the linear fit (and this structure can allow for covariates, if needed without modifying the below)

m1 <- glm(Response ~ C.Treatment, family = as.character(y$Model[1]), data = y)
#extract the t-value for the predictor (in this case time)

a.1 <- summary(m1)$coef[2, 3]
#compute effect size
r.1 <- partial.r(a.1, df = 1)		# using t value corresponding to coefficient for linear term in the model

# apply fisher z transformation (bounded by infinity so better for meta-analysis):
#see this transformation in Wikipedia!

es.1 <- 0.5 * log((1 + r.1) / (1 - r.1))
es.1
#END of Example 1

####Example 2, study that is a comparison of a gaussian response for a pre- and during-covid time period
y = subset(data, Study_ID == "2")
y$Treatment <- as.numeric(as.character(y$Treatment)) 
y$C.Treatment <- y$Treatment - mean(y$Treatment)
m1 <- glm(Response ~ C.Treatment, family = as.character(y$Model[1]), data = y)
a.1 <- summary(m1)$coef[2, 3]
r.1 <- partial.r(a.1, df = 1)		
es.1 <- 0.5 * log((1 + r.1) / (1 - r.1))
es.1
#END of Example 2

####Example 3, study that is a comparison of a quasipoisson response for a during- and post-covid time period
y = subset(data, Study_ID == "3")
y$Treatment <- as.numeric(as.character(y$Treatment)) 
y$C.Treatment <- y$Treatment - mean(y$Treatment)
m1 <- glm(Response ~ C.Treatment, family = as.character(y$Model[1]), data = y)
a.1 <- summary(m1)$coef[2, 3]
r.1 <- partial.r(a.1, df = 1)
es.1 <- 0.5 * log((1 + r.1) / (1 - r.1))
es.1
#END of Example 3

####Example 4, study that is a comparison of a quasipoisson response for a spatial block/treatment during the lockdown, where reference is the control, in this case "A"
y=subset(data,Study_ID=="4")
y$C.Treatment <- y$Treatment
m1 <- glm(Response ~ C.Treatment,family=as.character(y$Model[1]),data=y)
a.1 <- summary(m1)$coef[2,3]
r.1 <- partial.r(a.1, df = 1)		# using t value corresponding to coefficient for linear term in the model
es.1 <- 0.5*log((1 + r.1)/(1 - r.1))
es.1
#END of Example 4

####Example 5, study that is a comparison of a quasibinomical response for a spatial block/treatment during the lockdown, where reference is the control, in this case "A"
y=subset(data,Study_ID=="5")
y$C.Treatment <- y$Treatment
m1 <- glm(Response ~ C.Treatment,family=as.character(y$Model[1]),data=y)
a.1 <- summary(m1)$coef[2,3]
r.1 <- partial.r(a.1, df = 1)		# using t value corresponding to coefficient for linear term in the model
es.1 <- 0.5*log((1 + r.1)/(1 - r.1))
es.1
#END of Example 5

####Example 6, study that is a comparison of a guassian response for a pre- versus during-lockdown design, where each has a temporal trend 
y=subset(data,Study_ID=="6")
y$Treatment<-as.numeric(as.character(y$Treatment)) 
y$Within.Treatment.Time.Step<-as.numeric(as.character(y$Within.Treatment.Time.Step)) 
y$C.Treatment <- y$Treatment - mean(y$Treatment)
m1 <- glm(Response ~ C.Treatment*Within.Treatment.Time.Step,family=as.character(y$Model[1]),data=y)
a.1 <- summary(m1)$coef[2,3]
r.1 <- partial.r(a.1, df = 1)		# using t value corresponding to coefficient for linear term in the model
es.1 <- 0.5*log((1 + r.1)/(1 - r.1))
es.1
#END of Example 6

####Example 7, study that is a comparison of a guassian response for a pre- versus during-lockdown design, where each has a blocking factor (e.g., spatial block)
y=subset(data,Study_ID=="7")
y$Treatment<-as.numeric(as.character(y$Treatment)) 
y$C.Treatment <- y$Treatment - mean(y$Treatment)
m1 <- lme(data=y,Response ~ as.factor(C.Treatment),random=~1|Within.Treatment.Time.Step)
a.1 <-summary(m1)$tTable[2,4]
r.1 <- partial.r(a.1, df = 1)	
es.1 <- 0.5*log((1 + r.1)/(1 - r.1))
es.1
#END of Example 7

####Example 8, study that is a comparison of a guassian response where each has a temporal trend, instead of a covariate as in example 6, and thus could be included as temporal autocorrelation
y=subset(data,Study_ID=="6")
y$Treatment<-as.numeric(as.character(y$Treatment)) 
y$Within.Treatment.Time.Step<-as.numeric(as.character(y$Within.Treatment.Time.Step)) 
y$C.Treatment <- y$Treatment - mean(y$Treatment)
m1 <- gls(data=y,Response ~ as.factor(C.Treatment),corr=corARMA(form=~1|C.Treatment,q=2))
##confirm the number of q to include
##plot(ACF(m1),alpha=0.05)
a.1 <-summary(m1)$tTable[2,3]
r.1 <- partial.r(a.1, df = 1)	
es.1 <- 0.5*log((1 + r.1)/(1 - r.1))
es.1
#END of Example 8


#######################################################################################################################
# Meta-analysis 
# now for the analysis, suggest random effects model with study level random effects
# Bayesian approach allows for assessing credible intervals of probability of parameters (e.g., effect estimate of mean slope) given the dataset

library (MCMCglmm)

#uniform prior on the standard deviation of the random effects, for both residual variance (R structure), and study level variance (G structure)
#this says that we don't know what the variance is of the data - assuming a uniform distribution of variance
prior <- list(R=list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

# random effects meta-analysis with additional random effects for study (for study with multiple effects/multiple areas)
# propose mev be df
m1a <- MCMCglmm(es.1 ~ factors of interest OR 1 (global effect), random = ~ Study_ID, mev = mev, prior = prior, nitt = 110000, burnin = 10000, thin = 100, verbose = F, data = our.data)





