library(evir)

# Filtering
# dat <- read.csv("data/cetdl1772on.csv", header = TRUE, colClasses=c("Date",NA))
# dat2 <- dat[dat$date>="1901-01-01"&dat$date<="2000-12-01",]
# write.csv(file='data/cetdl1772on_filtered.csv', x=dat2, row.names=FALSE)

daily <- read.csv("data/cetdl1772on_filtered.csv", header=TRUE, colClasses=c("Date",NA))

# Simple plots
plot(daily$date, daily$value, type="l")
plot(daily$date, daily$value, pch=16, cex = .3)
plot(daily$date, daily$value, pch='.', 
     main="Daily maximum temperature from 1901 to 2000", xlab="Date", ylab="Maximum temperature (degrees)")
hist(daily$value)

# Obtain maximum per year
yearly.max <- aggregate(x = daily$value, FUN = max,
                     by = list(year = format(daily$date, "%Y")))
plot(yearly.max, type="l", 
     main="Yearly maximum temperature from 1901 to 2000",xlab="Year",ylab="Maximum temperature (degrees)")
hist(yearly.max$x)

# Estimate parameters for Gumbel distribution
temp.gumbel<-gumbel(yearly.max$x) #, block=10
temp.gumbel$par.ests
temp.gumbel$par.ses

#Temperature will only exceed once every ten years (x = α + β log T)
b <- temp.gumbel$par.ests[1]
a <- temp.gumbel$par.ests[2]
x <- a + b * log(10)

# Appropriate plot
plot(yearly.max, type="l", 
     main="Yearly maximum temperature from 1901 to 2000",xlab="Year",ylab="Maximum temperature (degrees)")
abline(h=x, lty=2, col="red")

yearly.max[yearly.max$x>x,]
