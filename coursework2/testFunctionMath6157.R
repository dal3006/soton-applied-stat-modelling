library(SLHD) # this package will generate LHDs
Mmdesign <- maximinSLHD(1, 100, 10) 
dfMmd <- data.frame(Mmdesign$StandDesign)
Z = data.frame(matrix(1, ncol = 10))
X <- dfMmd

if(is.data.frame(X) == F || is.data.frame(Z) == F) stop("Input must be a data frame")
if(ncol(X) != 10 || ncol(Z) != 10) stop("Input must have 10 columns")
X <- apply(X, 2, scale, center = 0, scale = 1)
#f1 <- function(x) 20 * sin(pi * x)
f2 <- function(x) exp(5 * x)
f3 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
#f4 <- function(x) 0*x
index <- sample(1:10, 4)
fx <- 10 * X[, index[1]] + f2(X[, index[2]]) + f3(X[, index[3]])
#fz <- 5 * Z[, index[1]] + f2(Z[, index[2]]) + f3(Z[, index[3]])
gx <- (fx - 7)/  3
#gz <- (fz - 7) / 3
#gz <- binomial()$linkinv(gz)
#yz <- rbinom(gz, 10, gx)
#gz <- poisson()$linkinv(gz)
#yx <- rpois(gz, gz)
#yz <- rpois(gz, gz)
g <- binomial()$linkinv(gx)
yx <- rbinom(nrow(X), 10, g)

#return(yx)

# Summarizing:
# The output is a quite hidden combination of 3 randomly chosen predictors of the input dataFrame


