library(caret)

steam <- read.table("data/steam.txt", sep="\t", header = TRUE)

set.seed(12345)
####################################################################################
# Split the dataset in train and test 
training <- steam[1:20, ]
validation <- steam[21:25, ]

####################################################################################
# Ridge Regression
fitControl <- trainControl(method = "cv", number = 4, repeats = 10  ) 

ridgeFit <- train(y ~ cg + od + x + cg:od, 
                  data = training, 
                  method = "ridge", 
                  trControl = fitControl)
ridgeFit

# Validation
ridgePrediction <- predict(ridgeFit, newdata = validation)
postResample(pred = ridgePrediction, obs = validation$y)

####################################################################################
# Lasso Regression
lassoFit <- train(y ~ cg + od + x + cg:od, 
                  data = training, 
                  method = "lasso", 
                  trControl = fitControl)
lassoFit

# Validation
lassoPrediction <- predict(lassoFit, newdata = validation)
postResample(pred = lassoPrediction, obs = validation$y)

####################################################################################
# Linear model
lmFit <- train(y ~ cg + od + x + cg:od, 
                  data = training, 
                  method = "lm", 
                  trControl = fitControl)
lmFit

# Validation
lmPrediction <- predict(lmFit, newdata = validation)
postResample(pred = lmPrediction, obs = validation$y)


####################################################################################
# Extra

# Plots comparing performance metrics on CV
results <- resamples(list(LM=lmFit, RIDGE=ridgeFit, LASSO=lassoFit))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)


# Feature selection
# calculate correlation matrix
correlationMatrix <- cor(steam[,-1])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


# Recursive feature elimination
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(steam2[,-1], steam2[,1], sizes=c(1:5), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


####################################################################################
# PRESS statistic
# Ridge Regression
fitControl2 <- trainControl(method="cv", number=12)
ridgeFit2 <- train(y ~ cg + od + x + cg:od, 
                  data = steam, method = "ridge", trControl = fitControl2)
ridgeFit2

# Validation
ridgePrediction <- predict(ridgeFit, newdata = validation)
postResample(pred = ridgePrediction, obs = validation$y)


