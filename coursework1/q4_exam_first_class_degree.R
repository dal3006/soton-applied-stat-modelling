library(car)
exam <- read.csv("data/exams.csv", header = TRUE)
summary(exam)
str(exam)
plot(exam)
scatterplotMatrix(exam)
scatterplotMatrix(exam, diagonal='histogram')

exam$admittedFactor <- factor(exam$admitted,
                           levels=c(0,1),
                           labels=c("No","Yes"))
####################################################################################
# Logistic model
exam.glm1<-glm(admitted~exam_1+exam_2+exam_1:exam_2,
              family=binomial(link="logit"),data = exam)
exam.glm2<-glm(admitted~exam_1+exam_2,
               family=binomial(link="logit"), data = exam)

oldp = par()$mfrow; par(mfrow=c(2,2))
plot(exam.glm1); par(mfrow=oldp)

summary(exam.glm1)
anova(exam.glm2, exam.glm1, test="Chisq") #List of models from simpler to more complex

# (a) Test the significance of the model overall and of each parameter in the model
# above. Test all these using α = 0.01.


# we can perfom a chi-squared test on the difference between
# the null deviance and the model deviance, 
# taking into account the difference in the numbers of
# degrees of freedom. The easiest way to do this in R is:
with(exam.glm1, pchisq(null.deviance - deviance,
                       df.null - df.residual, lower.tail = FALSE))

# Test
anova(exam.glm1, test="Chisq")

# Significance/interpretation of the parameters
exp(coef(exam.glm2))

# Relationships
spm(exam)

# Confidence interval on the coefficients
confint(exam.glm1)


# (e) For your final model, examine the residual plots to see whether 
# the assumptions of the model were satisfied. Comment on any problems.

# Residuals
plot(fitted(exam.glm1), resid(exam.glm1), col="blue",
     main="Residuals vs Fitted values", 
     xlab="Fitted values", ylab="Residuals")
points(fitted(exam.glm2), resid(exam.glm2), col="red")
legend("topright", c("glm-1","glm-2"), col=c("blue","red"), pch=1)

plot(resid(exam.glm1, type="dev"), main="Deviance residuals",
     xlab="Index", ylab="Deviance residual")

# Using both the original and the new model, predict
# the probability of a getting an upper second degree (or higher) with marks of 62
# in both exams.
score <- 62
predict(exam.glm1, data.frame(exam_1=score, exam_2=score), type="response")
predict(exam.glm2, data.frame(exam_1=score, exam_2=score), type="response")


# Plot to try to inffer the correct answer
plot(exam$exam_1,exam$exam_2,col=c("red","blue")[exam$admitted2],
     main="First class degree based in two exams",
     xlab="Exam 1", ylab="Exam 2")
points(62,62, pch=23, bg="green")

# (c) Compare the PRESS statistics for your original and new model. Interpret these
# statistics.

X <- cbind(1,exam$exam_1,exam$exam_2) # Design matrix (1:intercept)
H <- X %*% solve(t(X) %*% X) %*% t(X)
DiagH <- diag(H)

# Model 1 (wrong)
# predictedResiduals1 <- exam.glm1$residuals*(1/(1-DiagH))
# (RSS1 <-sum(exam.glm1$residuals^2))
# (PRESS1 <- sum(predictedResiduals1^2))
# https://stevencarlislewalker.wordpress.com/2013/06/18/calculating-the-press-statistic-in-r/
predictedResiduals1 <- resid(exam.glm1)/(1-lm.influence(exam.glm1)$hat)
(RSS1 <- sum(resid(exam.glm1)^2))
(PRESS1 <- sum(predictedResiduals1^2))

# Model 2 (correct)
# predictedResiduals2 <- exam.glm2$residuals*(1/(1-DiagH))
# PRESS2 <- sum(predictedResiduals2^2)
# RSS2 <-sum(exam.glm2$residuals^2)
# These results are not what I expected!
predictedResiduals2 <- resid(exam.glm2)/(1-lm.influence(exam.glm2)$hat)
(RSS2 <- sum(resid(exam.glm2)^2))
(PRESS2 <- sum(predictedResiduals2^2))

# # There seems to be an outlayer, that is causing trouble
# exam2 = exam[!(exam$exam_1==30&exam$exam_2==95),]
# X2 <- cbind(1,exam2$exam_1,exam2$exam_2) # Design matrix (1:intercept)
# H2 <- X2 %*% solve(t(X2) %*% X2) %*% t(X2)
# DiagH2 <- diag(H2)
# exam.glm3<-glm(admitted~exam_1+exam_2,
#                family=binomial(link="logit"), 
#                data = exam2)
# predictedResiduals3 <- residuals(exam.glm3)*(1/(1-DiagH2))
# RSS3 <-sum(exam.glm3$residuals^2)
# PRESS3 <- sum(predictedResiduals3^2)

# ANOVA
anova(exam.glm2, exam.glm1, test="Chisq")
anova(exam.glm1, exam.glm2, test="Chisq")
# The nonsignificant chi-square value (p = 0.21) suggests that the reduced model with
# four predictors fits as well as the full model with nine predictors, reinforcing your
# belief that gender, children, education, and occupation don’t add significantly to the
# prediction above and beyond the other variables in the equation. Therefore, you can
# base your interpretations on the simpler model.



# (d) Use the model deviances to make an appropriate statistical test of whether the
# original or new model fits significantly better.
1-pchisq(exam.glm2$deviance-exam.glm1$deviance,
         exam.glm2$df.residual-exam.glm1$df.residual)
1-pchisq(exam.glm1$deviance-exam.glm2$deviance,
         exam.glm1$df.residual-exam.glm2$df.residual)
anova(exam.glm2, exam.glm1, test="Chisq")

# Manual check of Classification
a <- predict(exam.glm1, exam[,c(1,2)], type="response")
b <- predict(exam.glm2, exam[,c(1,2)], type="response")
classification = exam[,c(1,2,3)]
classification$model1 = factor(a > .5, levels=c(FALSE, TRUE),
                               labels=c("NO", "YES"))
classification$model2 = factor(b > .5, levels=c(FALSE, TRUE),
                               labels=c("NO", "YES"))

(logit.perf1 <- table(exam$admitted2, classification$model1,
                     dnn=c("Actual", "Predicted")))
(logit.perf2 <- table(exam$admitted2, classification$model2,
                     dnn=c("Actual", "Predicted")))


# (e) For your final model, examine the residual plots to see whether the assumptions
# of the model were satisfied. Comment on any problems.
oldp = par()$mfrow; par(mfrow=c(2,2)) 
plot(exam.glm1); par(mfrow=oldp)

plot(residuals(exam.glm1))
plot(fitted(exam.glm1), residuals(exam.glm1))
hist(residuals(exam.glm1))

plot(log(predict(exam.glm1)), residuals(exam.glm1, type="deviance"))
abline(h=0, lty=2)
qqnorm(residuals(exam.glm1, type="deviance"))
qqline(residuals(exam.glm1, type="deviance"))
