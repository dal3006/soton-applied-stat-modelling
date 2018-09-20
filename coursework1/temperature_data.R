#################################################################################
# Daily data
#################################################################################

# Reading data
daily <- read.csv("data/cetdl1772on.csv", header = TRUE, colClasses=c("Date",NA))
# Discard records with date>=2017-03-01, because they do not contain data (99.99)
# Discard all the year 2017, because it will mess yearly aggregation
daily <- daily[daily$value<99&daily$value>-99&daily$date<"2017-01-01",]

# Aggregate maximun data (all period)
yearly.max <- aggregate(x = daily$value, FUN = max,
                        by = list(year = format(daily$date, "%Y")))
plot(yearly.max, type="l", 
     main="Yearly maximum temperature from 1901 to 2016",xlab="Year",ylab="Maximum temperature (degrees)")
abline(h=23.52506, lty=2, col="red")


# Yearly maximum for the period 2001-2016
yearly.2000 <- yearly.max[yearly.max$year>="2001"&yearly.max$year<"2017",]

plot(yearly.2000, type="l", 
     main="Yearly maximum temperature fromm 2001 to 2016",xlab="Year",ylab="Maximum temperature (degrees)")
abline(h=23.52506, lty=2, lwd=1, col="red")



#################################################################################
# Monthly data
#################################################################################

# Reading data
monthly <- read.csv("data/cetml1659on.csv", header = TRUE, colClasses=c("Date",NA))
# Discard records with date>=2017-03-01, because they do not contain data (99.99)
# Discard all the year 2017, because it will mess yearly aggregation
monthly <- monthly[monthly$value<99&monthly$value>-99&monthly$date<"2017-01-01",]

plot(monthly, type="l", 
     main="Monthly temperature from 1659 to 2016",xlab="Year",ylab="Average temperature (degrees)")

# Average aggregation
yearly.mean <- aggregate(x = monthly$value, FUN = mean,
                        by = list(year = format(monthly$date, "%Y")))
plot(yearly.mean, type="l", 
     main="Yearly average temperature from 1659 to 2016",xlab="Year",ylab="Average temperature (degrees)")


