library(car)

steam <- read.table("data/steam.txt", sep="\t", header = TRUE)
#steam2 = steam[steam$od!=11,] # Remove potential high leverage points # Didn't work!

summary(steam)
str(steam)
plot(steam)
scatterplotMatrix(steam)
scatterplotMatrix(steam2, diagonal='histogram')
plot(steam2$cg_od, steam2$y)

####################################################################################
# STEPWISE REGRESSION - BACKWARDS and FORWARD
model.ls <- lm(y~.+log(cd)+log(od)+log(x)+log(cg), data=steam)
step(model.ls, direction="backward") #y ~ cd + od + x.; drops cg which is important! 
step(model.ls2, direction="backward") #y ~ cg + od + x + cg:od; drops cd

# Before
model.backward <- step(model.ls2, direction="backward")
rss.backward <- sum(model.backward$resid^2)/model.backward$df.residual

# Eshasutive test:
model.ls.dumb <- lm(y ~ . +cg:od+cg:cd+od:cd+cg:cd:od,data=steam) #Same as the previous
model.backward <- step(model.ls.dumb, direction="backward") #The interaction was kept!



# So backward selection using AIC drops (cd) from the model. The final AIC is {}.
# We can now compare forward selection with AIC. Note the scope command tells you what 
# parameters we may put into our model, and we need to define this for forward 
# stepwise selection.

scope <- list(upper=~.+cg:od+cg:cd+od:cd+cg:cd:od, lower=~.) # Exhaustive
#scope <- list(upper=~.+cg:od, lower=~.) # Educated guest

model.forward <- step(lm(y ~ x, data=train), scope, direction="forward")
rss.forward <- sum(model.forward$resid^2)/model.forward$df.residual

# Does the forward stepwise regression procedure find the same model as the backwards?
# A: NO!


####################################################################################
### Ridge regression
library(MASS)

steam2 <- steam
steam2$cg_od <- steam2$cg*steam2$od

lambda.range <- seq(0,0.2,length=100) # seq(0,0.2,0.001)
#lambda.range <- seq(0,100,length=100) # seq(0,0.2,0.001)
model.ridge <- lm.ridge(y~.+cg:od, data=train, lambda=lambda.range)
#model.ridge <- lm.ridge(y~.+cg:od, data=train, lambda=lambda.range) # Seems like it didn't work

## Plot 1
plot(lambda.range, model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")
# The optimal lambda is given by
select(model.ridge)
lambda.ridge <- lambda.range[which.min(model.ridge$GCV)]

## Plot 2
# We can plot the coefficients and see how they vary as a function of lambda
colors <- rainbow(5)
#colNames <- c(colnames(steam)[-1],'cg:od')
matplot(lambda.range, coef(model.ridge)[,-1], 
        xlim=c(0, max(lambda.range) + 0.1*(range(lambda.range)[2]-range(lambda.range)[1])), 
        type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(max(lambda.range) + 0.1*(range(lambda.range)[2]-range(lambda.range)[1]),5), 
     coef(model.ridge)[length(lambda.range),-1], colnames(steam2)[-1], pos=2, col=colors)

## Plot 2(b). This is best because the betas are normalized
plot(model.ridge, main="Ridge coefficients", xlab="Lambda")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
#text(rep(0.20,4), model.ridge$coef[-1,length(lambda.range)], colNames, pos=2)
text(rep(0.007,5), model.ridge$coef[,1], colnames(steam2)[-1], pos=2)

beta.ridge <- coef(model.ridge)[which.min(model.ridge$GCV),]
#resid.ridge <- train$y - beta.ridge[1] - as.matrix(train[,2:5])%*%beta.ridge[2:5]
#resid.ridge <- train$y - as.matrix(train[,2:5])%*%beta.ridge[1:4]
resid.ridge <- steam2$y - beta.ridge[1] - as.matrix(steam2[,-1])%*%beta.ridge[-1]

# To find the degrees of freedom for this model is a little complicated, but for 
# comparison purposes, this is how we do it in R.
d <- svd(as.matrix(steam2[,2:6]))$d
df <- dim(steam2)[1] - sum(d^2/(lambda.ridge+d^2))
rss.ridge <- sum(resid.ridge^2)/df
aic.ridge <- 3*log(rss.ridge)+2*df


####################################################################################
# LASSO REGRESSION 
# In Lasso Regression, the coefficients are penalized by the L1 norm. The 
# optimal value for lambda is chosen by cross-validation.

library(lars)
y <- as.numeric(steam2[,1])
x <- as.matrix(steam2[,2:6])
x3 <- as.matrix(steam2[,c(2,4,5,6)]) #No CD

model.lasso <- lars(x3, y, type="lasso",intercept=TRUE) #normalize=FALSE
lambda.lasso <- c(model.lasso$lambda,0) 
beta <- coef(model.lasso)

plot(model.lasso) # Good plot. Better than the tutorial!

# The code below will plot the values of the coefficients for various lambda. Again the 
# optimal is higlighted by the dashed line. Can you tell why it is likely to be 
# optimal?
colors <- rainbow(5)
matplot(lambda.lasso, beta, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors)
text(rep(0, 5), beta[4,], colnames(steam2[,-1]), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

# I'll keep 3 predictors
beta.lasso <- beta[4,]
resid.lasso <- steam2$y - predict(model.lasso, as.matrix(steam2[-1]), s=4, type="fit")$fit
#TODO: s=4? A: it seems to be the index of the beta coefficients matrix
rss.lasso <- sum(resid.lasso^2)/(nrow(steam2)-sum(abs(beta.lasso)>0,1)) #TODO: n-p? p=non-cero parameters + 1 (intercept)

####################################################################################
# COMPARISON OF FITTING 
# This is straightforward now:
rss.ls
rss.backward
rss.forward
rss.ridge
rss.lasso

####################################################################################
# COMPARISON OF PREDICTION 
# We can also compare with the prediction dataset we saved from before. In this case
# y.new <- calibrate$y
# 
# pss.ls <- sum((y.new - predict(model.ls, calibrate[,2:5]))^2)
# pss.backward <- sum((y.new - predict(model.backward, calibrate[,2:5]))^2)
# pss.forward <- sum((y.new - predict(model.forward, calibrate[,2:5]))^2)
# pss.ridge <- sum((y.new - beta.ridge[1] - as.matrix(calibrate[,2:5])%*%beta.ridge[2:5])^2)
# pss.lasso <- sum((y.new - predict(model.lasso, as.matrix(calibrate[,2:5]), s=4, type="fit")$fit)^2)
# #TODO s=4?
# 
# pss.ls
# pss.backward
# pss.forward
# pss.ridge
# pss.lasso

####################################################################################
# (c) Using your preferred model, find a 95% confidence interval for a month with 20
# operating days out of 30 calendar days, in which 0.7 pounds of Crude Glycerine
# are made.
predict(model.lasso,data.frame(od=20,cd=30,cg=0.7,x=35),interval="prediction")




####################################################################################
# Some tests



# Ridge 2
require(glmnet)

set.seed(999)
cv.ridge <- cv.glmnet(x, y, alpha=0, nfolds=5, intercept=FALSE)

# Results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, s=cv.ridge$lambda.min)