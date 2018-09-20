################################################################################
#(a) Plot these data in a suitable form and fit a simple linear regression model
bg <- read.csv("data/baby_girl_weight.csv")
plot(bg$Age, bg$Weight, 
     main="Weight of a baby girl", xlab="Age (weeks)", ylab="Weight (Kg)")
bg.lm = lm (Weight~Age, data=bg)
summary(bg.lm)
lines(bg$Age,fitted(bg.lm), col="red")
#segments(bg$Age,fitted(bg.lm), bg$Age,bg$Weight)
#plot(bg.lm$fitted.values, bg.lm$residuals)
resid(bg.lm)
plot(hist(residuals(bg.lm)), main="Histogram of residuals",
     xlab="Residuals")

#(b) Produce residual plots for the model, and hence or otherwise check the assumptions
plot(bg$Age, resid(bg.lm))
abline(0,0,lty=2)

oldp = par()$mfrow; par(mfrow=c(2,2))
plot(bg.lm); par(mfrow=oldp)

################################################################################
#(c) Fit a quadratic model to these data. Carry out the usual model checks. 
#Does this seem a reasonable model?
bg.lm2 = lm(Weight~poly(Age,2), data=bg)
plot(bg$Age, bg$Weight,
     main="Weight of a baby girl", xlab="Age (weeks)", ylab="Weight (Kg)")
lines(bg$Age,fitted(bg.lm2), col="red")

oldp = par()$mfrow; par(mfrow=c(2,2))
plot(bg.lm2); par(mfrow=oldp)
plot(hist(residuals(bg.lm2)), main="Histogram of residuals",
     xlab="Residuals")

#(d) Add a cubic term as well. Carry out the usual model checks. Do you think this is
#a reasonable model?
bg.lm3 = lm(Weight~poly(Age,3), data=bg)
plot(bg$Age, bg$Weight,
     main="Weight of a baby girl", xlab="Age (weeks)", ylab="Weight (Kg)")
lines(bg$Age,fitted(bg.lm3), col="red")

oldp = par()$mfrow; par(mfrow=c(2,2))
plot(bg.lm3); par(mfrow=oldp)
plot(hist(residuals(bg.lm3)), main="Histogram of residuals",
     xlab="Residuals")

# (f) The baby’s parents want to predict the weight of the girl at 26 weeks. Predict
# the weight for each of the three models. Find the 95% prediction interval for the
# prediction from each model, and comment briefly on how reliable this prediction
# interval is likely to be.
predict(bg.lm ,data.frame(Age=26),interval="prediction")
predict(bg.lm2,data.frame(Age=26),interval="prediction")
predict(bg.lm3,data.frame(Age=26),interval="prediction")

### Improve the model???
bg.lm.log = lm(Weight~log(Age), data=bg)
plot(bg$Age, bg$Weight,
     main="Weight of a baby girl", xlab="Age (weeks)", ylab="Weight (Kg)")
lines(bg$Age,fitted(bg.lm.log), col="red")


