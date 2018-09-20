## MATH6157 Coursework 2 2016-17

## Q1.
library(MASS)

## a.
## Various GLMs
bac.glm1 <- glm(y ~ week + trt, family = binomial, data = bacteria)
bac.glm2 <- glm(y ~ ID + week, family = binomial, data = bacteria) # notice that you cannot fit ID and trt, as each subject is only given one level of trt
bac.glm3 <- glm(y ~ (ID + week)^2, family = binomial, data = bacteria)
r1 <- resid(bac.glm1, type = "pearson")
r2 <- resid(bac.glm2, type = "pearson")
r3 <- resid(bac.glm3, type = "pearson")
cor(bac.glm1$fitted, as.numeric(bacteria$y) - 1)^2 # really low! 
cor(bac.glm2$fitted, as.numeric(bacteria$y) - 1)^2 # better
cor(bac.glm3$fitted, as.numeric(bacteria$y) - 1)^2 # better again - evidence that individual intercepts and slopes might be needed for each subject?
AIC(bac.glm1)
AIC(bac.glm2)
AIC(bac.glm3) # BUT, the simplest model has smallest AIC. Evidence that our model assumptions are wrong?
par(mfrow = c(1, 3))
qqnorm(r1)
qqnorm(r2)
qqnorm(r3) # the residuals don't look normally distributed for any of the models
## Should we be surprised? Not really, when we are just looking at individual binary observations (0/1), there is no central limit theorem 
## (as we are not looking at averages or sums). The underdispersion around 0 is where we are predicting perfectly, due to the large number of 1's in the data
par(mfrow = c(1, 3)) # residuals versus week
plot(bacteria$week, r1, ylim = c(min(r1, r2, r3), max(r1, r2, r3)))
plot(bacteria$week, r2, ylim = c(min(r1, r2, r3), max(r1, r2, r3)))
plot(bacteria$week, r3, ylim = c(min(r1, r2, r3), max(r1, r2, r3)))
## these are generally looking better for the more complex models
par(mfrow = c(1, 3)) # residuals versus trt
plot(bacteria$trt, r1)
plot(bacteria$trt, r2)
plot(bacteria$trt, r3) 
## ditto the results above - the simpler models have some large(-ish) outliers, and some quite unequal variance 
par(mfrow = c(1, 3)) # fitted versus observed
plot(bacteria$y, fitted(bac.glm1))
plot(bacteria$y, fitted(bac.glm2))
plot(bacteria$y, fitted(bac.glm3))
## all models struggle to predict the zeros accurately

##b. Various GLMMs
library(lme4)
## Comparing the fits of the various GLMs, we will go directly to a model with random intercept and slope
## For this model, we have to use the Laplace approximation in glmer
bac.glmer1 <- glmer(y ~ week + trt + (week | ID), family = binomial, data = bacteria)
r1 <- resid(bac.glmer1, ty = "pearson")
par(mfrow=c(1, 3))
plot(bacteria$week, r1)
plot(bacteria$trt, r1)
plot(bacteria$y, fitted(bac.glmer1))
cor(fitted(bac.glmer1), as.numeric(bacteria$y) - 1)^2
## residuals don't look good and R2 is low, maybe we should try a simpler model

bac.glmer2 <- glmer(y ~ week + trt + (1 | ID), family = binomial, data = bacteria)
r2 <- resid(bac.glmer2, ty = "pearson")
par(mfrow=c(2, 2))
plot(bacteria$week, r2)
plot(bacteria$trt, r2)
plot(bacteria$y, fitted(bac.glmer2))
cor(fitted(bac.glmer2), as.numeric(bacteria$y) - 1)^2
qqnorm(ranef(bac.glmer2)$ID[, 1]) # not great, because of the large number (11) of subjects that were given placebo and returned "y" for each observation
## residuals look better, although not great still
## R2 is low compared to the GLM with different intercepts and slopes for each subject 
## *but* that model has **a lot** more parameters, so is bound to have higher R2 

##c. bootstrapping to perform a hypothesis test for the random effect for ID
likelihood_ratio <- NULL
attach(bacteria)
for(k in 1:100) {
  yb <- simulate(glm(y ~ week + trt, family = binomial))[[1]] # simulate data from the null fitted model
  boot_null <- glm(yb ~ week + trt, family = binomial) # refit the null model to the "new data"
  boot_alt <- glmer(yb ~ week + trt + (1 | ID), family = binomial) # fit the alternative model to the "new data"
  likelihood_ratio[k] <- as.numeric(2* (logLik(boot_alt) - logLik(boot_null))) # calculate the likelihood ratio in favour of the alternative
}
detach(bacteria)
mean(likelihood_ratio > as.numeric(2* (logLik(bac.glmer2) - logLik(bac.glm1))))
## p-value is around 0, so reject H0

##d. prediction
par(mfrow = c(1, 1))
X.new <- data.frame(week = rep(c(0, 2, 4, 6, 11), 3), trt = rep(c("placebo", "drug", "drug+"), c(5, 5, 5)), ID = rep("Z27", 15))
pr <- plogis(predict(bac.glmer2, X.new, allow.new.levels = T))
plot(X.new$week, pr, col = X.new$trt, pch = 16, xlab = "Week", ylab = "Predicted probability")
legend("topright", legend = c("placebo", "drug", "drug+"), col = 3:1, pch = 16)

#e. group across week
attach(bacteria)
IDG <- aggregate(as.numeric(y) - 1, by = list(ID), FUN = sum) # aggregating the response
trtg <- trt[match(IDG[, 1], ID)] # picking out the trt for each subject
bac.new <- data.frame(y = IDG[, 2] / summary(bacteria$ID), ID = IDG[, 1], trt = trtg) # creating a new data set
detach(bacteria)
bacg.glmer1 <- glmer(y ~ trt + (1 | ID), family = binomial, data = bac.new, weights = summary(bacteria$ID))
bacg.glmer2 <- glmer(y ~ (1 | ID), family = binomial, data = bac.new, weights = summary(bacteria$ID))
AIC(bacg.glmer1)
AIC(bacg.glmer2)
## pick the model with a trt effect
## Note - it wouldn't really be fair to look at R2 here, as the trt is confounded with ID
r1 <- resid(bacg.glmer1, ty = "pearson")
par(mfrow=c(1, 3))
plot(bac.new$trt, r1)
plot(bac.new$y, fitted(bacg.glmer1))
qqnorm(ranef(bacg.glmer1)$ID[, 1])
cor(fitted(bacg.glmer1), as.numeric(bac.new$y) - 1)^2

####################################
## Q2.
library(faraway)

## a.
attach(aatemp)
nyear <- year[-(1:3)]
ntemp <- temp[-(1:3)]
detach(aatemp)
## plot the data
par(cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
plot(nyear, ntemp, pch = 16, xlab = "Year", ylab = "Temp")

##b. fit a linear model, and plot predictions
aatemp.lm <- lm(ntemp ~ nyear)
lines(nyear, fitted(aatemp.lm))
summary(aatemp.lm) ## slope is sign. difference from zero but R2 is poor - we have restricted to a narrow class of models
cor(aatemp.lm$fitted, ntemp)^2

##c.i. loess, using default span 
aatemp.loess <- loess(ntemp ~ nyear)
lines(nyear, aatemp.loess$fitted, lty = 2) ## add to plot
cor(aatemp.loess$fitted, ntemp)^2 ## R2

#c.ii. with the crossval function from bootstrap library
library(bootstrap)
theta.fit <- function(x, y, span) loess(y ~ x, span = span) # create the two fns crossval needs - fit
theta.predict <- function(fit, x) predict(fit, x) # predict
cv.apply <- function(x) { # function to replicate crossvalidation
  cv <- NULL
  for(i in 1:R) {
    cvfit <- crossval(nyear, ntemp, theta.fit, theta.predict, span = x, ngroup = 10)$cv.fit
    cv[i] <- mean((cvfit - ntemp)^2, na.rm = T)
  }
  return(mean(cv))
}
R <- 100 # no. of replicates
aatemp.loess.cv <- sapply(seq(0.1, 1, .05), cv.apply) 
mb <- seq(.1, 1, .05)[which.min(aatemp.loess.cv)]
aatemp.loess <- loess(ntemp ~ nyear, span = mb)
lines(nyear, aatemp.loess$fitted, lty = 3)
cor(aatemp.loess$fitted, ntemp)^2 # cv has chosen a much smaller span, and hence a more complex smoother

##d. kernel smoothing, using the cv function from lectures (not included here)
library(cvTools)
aatemp.ks.cv <- cv(data.frame(nyear, ntemp), ksmoothCV, seq(1, 100, by = 1), R = 100)
mb <- aatemp.ks.cv[which.min(aatemp.ks.cv[, 2]), 1]
aatemp.ks <- ksmooth(nyear, ntemp, "normal", bandwidth = mb) 
lines(aatemp.ks, lty = 4)
cor(ksmooth(nyear, ntemp, x.points = nyear, "normal", bandwidth = mb)$y, ntemp)^2
## similar but slightly less smooth (i.e. more complicated) fit - more "wiggly", higher R2


##e. in order of complexity: linear model, Loess with default span, kernel smoother, Loess with cv span 
## as judged by R2 (lowest to highest) and plots (least to most wiggly).

#############################
## Q3.
set.seed(1550217)
library(SLHD)
library(lhs)
n <- 100

##a. 
Mmdesign <- maximinSLHD(1, n, 10)$StandDesign # generate the design
par(mfrow = c(5, 2))
for(i in 1:10) stripchart(Mmdesign[, i]) # plot the projections - all uniform

##b.
Mmdist <- min(dist(Mmdesign)) # we can use the dist function to work out inter-point distances
LHSdist <- NULL
for(i in 1:1000) LHSdist[i] <- min(dist(randomLHS(n, 10))) # LHDs using lhs package
randist <- NULL
for(i in 1:1000) randist[i] <- min(dist(matrix(runif(n * 10), ncol = 10))) # random designs
par(mfrow = c(2, 1))
hist(c(LHSdist, Mmdist), xlab = "Minimum inter-point Euclidean distance", main = "Random Latin hypercubes")
abline(v = Mmdist)
hist(c(randist, Mmdist), xlab = "Minimum inter-point Euclidean distance", main = "Random uniform points")
abline(v = Mmdist)
## plots, with the performance of the maximin design marked

##c. 
load("math6157Cwk2.RData")
y <- math6157Cwk2Data(data.frame(Mmdesign))
TrainData <- data.frame(y, Mmdesign)

##d.
names(TrainData)[1] <- "y"
for(i in 2:11) names(TrainData)[i] <- paste("x", i-1, sep="")
plot.my.data <- function(x) {
  plot(TrainData[, x], TrainData$y, ylab = "y", xlab = names(TrainData)[x])
}
par(mfrow = c(5, 2), mai = c(.5, 1, .3, .3))
invisible(lapply(2:11, plot.my.data)) # 2 marks
## Comment on variables that appear marginally important, e.g. x9 here

  
##e.  
library(mgcv)
mydata.gam <- gam(y / 10 ~ s(x1) + s(x2) + s(x3) + s(x4) + s(x5) + s(x6) + s(x7) + s(x8) + s(x9) + s(x10),  weights = rep(10, n),
                  data = TrainData, binomial(link = "logit")) # fit the logit GAM
summary(mydata.gam) 
par(mfrow = c(5, 2), mai = c(.5, 1, .3, .3))
plot(mydata.gam, residuals = T, rug = F) # effect plots
## Comment on important variables - e.g. x5, x7, x9

##f.
mydata.gam2 <- gam(y / 10 ~ s(x5) + s(x7) + s(x9),  weights = rep(10, n),
                   data = TrainData, binomial(link = "logit")) # fit a reduced gam
summary(mydata.gam2)
anova(mydata.gam2, mydata.gam, test = "Chisq") # compare to original, no evidence for extra terms

mydata.gam3 <- gam(y / 10 ~  x5 + s(x7) + s(x9),  weights = rep(10, n),
                   data = TrainData, binomial(link = "logit")) # fit a reduced gam
summary(mydata.gam3)
anova(mydata.gam3, mydata.gam2, test = "Chisq") # actually no difference between gam2 and gam3 - same model but gam3 is neater
## try different models

par(mfrow = c(3, 2), mai = c(.5, 1, .3, .3))
plot(mydata.gam3, residuals = T, rug = F, all.terms = T) # for the final selected model





