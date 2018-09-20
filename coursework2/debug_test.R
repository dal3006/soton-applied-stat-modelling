# Function to calculate the p-value estimation based on a bootstraping test
# for comparing a simple linear model with a predictor and a LMM adding 
# random effects to that predictor
# Adapted from worksheet8
boostrap_test <- function(dependantVar,predictorVar,clusterVar,dataset,N){
  likelihood_ratio <- NULL
  formula_null <- paste(dependantVar,'~', predictorVar)
  formula_null_2 <- paste('y2~', predictorVar)
  formula_alt <-  paste(dependantVar, '~', predictorVar,'+(', predictorVar, '|', clusterVar, ')')
  formula_alt_2 <- paste('y2~', predictorVar,'+(', predictorVar, '|', clusterVar, ')')
  
  tmpDataset <- dataset
  
  for(k in 1:N) {
    tmpDataset$y2 <- simulate(lm(formula=formula_null, data=dataset))[[1]] # simulate data from the null fitted model
    boot_null <- lm(formula=formula_null_2, data=tmpDataset) # refit the null model to the "new data"
    boot_alt <- lmer(formula=formula_alt_2, data=tmpDataset) # fit the alternative model to the "new data"
    likelihood_ratio[k] <- as.numeric(2* (logLik(boot_alt) - logLik(boot_null))) # calculate the likelihood ratio in favour of the alternative
  }
  model_null <- lm(formula=formula_null, data=dataset)
  model_alt <- lmer(formula=formula_alt, data = dataset, REML = T)
  result <- mean(likelihood_ratio > as.numeric(2* (logLik(model_alt) - logLik(model_null)))) 
  # p-value of 0 - are observed result is extreme under the null hypothesis of no random effect, and hence we can reject the null (i.e. there is evidence to include a random effect)
  return(result)
}

testA<-boostrap_test('ynum', 'trt', 'ID', bacteria_ds, 100)
testB<-boostrap_test('ynum', 'week', 'ID', bacteria_ds, 100)
table_boostrap_test <- data.frame(randomEffect=c('trt','week'),
                                  pValue=c(testA,testB))
table_boostrap_test
