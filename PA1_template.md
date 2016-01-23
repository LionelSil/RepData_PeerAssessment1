# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Loaded the activity data from the project directory into data-frame called mydata  
Created subset mydata1 with no missing values using the following code  


```r
mydata <- read.csv("activity.csv", header=TRUE, sep=",")
mydata1 <- mydata[complete.cases(mydata),]
summary(mydata1)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

## What is mean total number of steps taken per day?
Aggregate the steps per day and plot a histogram of steps taken per day   


```r
# Derive a vector mydata2 with sum of steps for each date
mydata2 <- tapply(mydata1$steps, mydata1$date, sum)

hist(mydata2, xlab= "Steps per day", main="Daily Steps Histogram")
```

![](PA1_template_files/figure-html/Rough_Steps_Perday_Hist-1.png) 

Mean steps per day is: 1.0766189\times 10^{4}  
Median steps per day is: 10765  

## What is the average daily activity pattern?
Derive Mean steps per 5 minute interval and plot as time series 


```r
mydata3 <- tapply(mydata1$steps, mydata1$interval, mean)


plot(mydata3, type="l", main=" Average Steps per 5min Interval", ylab="Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/Mean_steps_per_interval_ts-1.png) 

```r
# m1 <- max(mydata3)
# m1
m2 <- which.max(mydata3)
hh <- as.integer((m2-1)/12)
mm <- ((m2-1) - hh*12)*5
```

It can be seen that the maximum steps per interval is 'r max(mydata3)'.
This occurs in the interval commencing at 8 hour, 35 minutes. 



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
