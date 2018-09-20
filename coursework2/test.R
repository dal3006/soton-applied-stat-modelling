N <- 100
whiteNoise <- rnorm(N, mean = 0, sd = 0.1)
ys <- filter(whiteNoise, filter=c(0.9,0.09)) #c(0.09,0.9)
plot(whiteNoise, type='l')
lines(ys, col=2)

ys <- ys[(10*2):(N-1)]

N<-length(ys)
yt <- ys[3:N]
A <- cbind(ys[1:(N-2)],ys[2:(N-1)])
fit <- lsfit(A, yt, intercept=F)
coefficients(fit)




# Test it on a more well behaved model:
n <- 500
n.sub <- n*0.9
maxlag <- 2
x <- arima.sim(n=n,list(ar=c(0.9,0.09)),rand.gen=rnorm,sd=0.1)
x <- x[n-n.sub:1]
#acf(x,lag.max=2,type="partial")            # check!
armod <- ar(x,order.max=maxlag,method="mle")
armod       # check!




set.seed(999)
ma.sim<-arima.sim(model=list(ma=c(0.9,0.1)),n=100)
ts.plot(ma.sim)
ar(ma.sim[20:100],order.max=2,method="mle")
