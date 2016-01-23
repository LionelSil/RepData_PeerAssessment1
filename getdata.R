mydata <- read.csv("activity.csv", header=TRUE, sep=",")

mydata1 <- mydata[complete.cases(mydata),]

# This produces a vector with entires for each date
mydata2 <- tapply(mydata1$steps, mydata1$date, sum)

mydata2
hist(mydata2, xlab= "Steps per day", main="Steps Histogram")
summary(mydata2)


# Mean number of steps per Interval 
mydata3 <- tapply(mydata1$steps, mydata1$interval, mean)


plot(mydata3, type="l", main=" Average Steps per 5min Interval", ylab="Steps", xlab="Interval")
m1 <- max(mydata3)
m1
m2 <- which.max(mydata3)
hh <- as.integer((m2/100)
mm <- m2 - hh
hh
mm

# Now deal with missing values
misr <- complete.cases(mydata)
sum(!misr)
# Impute average no of steps per interval for NA in steps
# Add mean steps per interval as an additional column
mydata$hh <-  as.integer(mydata$interval/100)
mydata$mint <-  as.integer((mydata$interval - mydata$hh*100)/5)
mydata$int <- (mydata$hh*12) + (mydata$mint + 1) 

mydata$mst <- mydata3[mydata$int]
mydata$stepsx <- ifelse(misr, mydata$steps, mydata$mst)

# Produce histogram with steps per day after imputation
# This produces a vector with entires for each date
mydx <- tapply(mydata$stepsx, mydata$date, sum)

mydx
hist(mydx, xlab= "Steps per day", main="Steps Histogram after Imputation")
summary(mydx)
    
    
# Add weekends field to mydata
mydata$day <- weekdays(as.Date(mydata$date))
mydata$weekend <- ifelse((mydata$day=="Sunday") | (mydata$day == "Saturday"), TRUE, FALSE)

# Now

par(mfrow= c(2,1))

mydworkd<- mydata[!mydata$weekend,]
mydata3 <- tapply(mydworkd$stepsx, myworkd$interval, mean)
plot(mydata3, type="l", main=" Average Steps per 5min Interval- Weekdays", ylab="Steps", xlab="Interval")

mydwend<- mydata[mydata$weekend,]
mydata4 <- tapply(mydwend$stepsx, mydwend$interval, mean)
plot(mydata4, type="l", main=" Average Steps per 5min Interval- Weekend", ylab="Steps", xlab="Interval")
    
    