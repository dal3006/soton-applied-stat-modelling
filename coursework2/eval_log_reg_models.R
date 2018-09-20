#https://www.r-bloggers.com/evaluating-logistic-regression-models/
  
library(caret)
data(GermanCredit)

Train <- createDataPartition(GermanCredit$Class, p=0.6, list=FALSE)
training <- GermanCredit[ Train, ]
testing <- GermanCredit[ -Train, ]

mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + 
                   CreditHistory.Critical,  data=training, method="glm", family="binomial")

predict(mod_fit, newdata=testing)
predict(mod_fit, newdata=testing, type="prob")

pred = predict(mod_fit, newdata=testing)
accuracy <- table(pred, testing[,"Class"])
sum(diag(accuracy))/sum(accuracy)


