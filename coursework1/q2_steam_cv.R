library ( glmnet )

steam <- read.table("data/steam.txt", sep="\t", header = TRUE)
y <- as.numeric(train[,1])
x <- as.matrix(train[,2:5])

####################################################################################
# Ridge - CV
grid=10^ seq (-5, 4, length =100)
#ridge.mod = cv.glmnet(x,y, alpha=0, lambda=grid, nfold=5 ) #alpha=0 -> Ridge
#coef(ridge.mod)

set.seed(1)
cv.out = cv.glmnet(x,y, alpha=0, nfold=5, intercept=FALSE, lambda=grid)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

predict (cv.out, type ="coefficients", s=bestlam)
cv.out$lambda.1se

ridge.pred=predict(cv.out,s=bestlam,newx=x)
mean((ridge.pred-y)^2)
# MSE-intercept:    0.3020897
# MSE-no-intercept: 0.2882907

