# Visual representation using Dimensionality reduction
steam <- read.table("data/steam.txt", sep="\t", header = TRUE)
y <- as.numeric(train[,1])
x <- as.matrix(train[,2:5])

#PCA
pca = prcomp(x, scale = TRUE)

plot(pca$x[,1], steam$y)

plot(steam$cd, steam$od)
plot(steam$od, steam$cg)

steam2 = steam[steam$od!=11,]
plot(steam2$cd, steam2$od)
plot(steam2$od, steam2$cg)

# Check collinearity
cor(steam2[,-1])
# cg and od are correlated (0.6292512)
plot(steam$cd, steam$cg)



# New analysis ignoring high leverage points
model2.ls <- lm(y ~ ., data=steam2) # Base
model2.ls2 <- lm(y ~ . +cg:od,data=steam2) #Interaction: glycerine made and operative days
model2.ls3 <- lm(y ~ . +cg:od -cd,data=steam2) # Drop calendar days
model2.ls4 <- lm(y ~ . -cd, data=steam2) 
# Extrangely bad statistics!!!

