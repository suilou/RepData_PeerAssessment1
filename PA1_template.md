---
Title: "Reproducible Research: Project 1"
Author: "SH"
Date: "April 2016"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
rm( list = ls() )
data <- read.csv('data/activity.csv')
```

```
## Warning in file(file, "rt"): cannot open file 'data/activity.csv': No such
## file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
head(data)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

```r
str(data)
```

```
## function (..., list = character(), package = NULL, lib.loc = NULL, 
##     verbose = getOption("verbose"), envir = .GlobalEnv)
```

```r
# check missing values 
summary(data$steps)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
summary(data$interval)
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

```r
summary(data$date) 
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

## What is mean total number of steps taken per day?

### 2a. Calculate the total number of steps taken per day

```r
daySum <- aggregate( x=data$steps[!is.na(data$steps)], 
                     by=list(date=data$date[!is.na(data$steps)]), 
                     FUN=sum)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
head(daySum)
```

```
## Error in head(daySum): object 'daySum' not found
```

```r
str(daySum)
```

```
## Error in str(daySum): object 'daySum' not found
```

### 2b. Make a histogram of the total number of steps taken each day

```r
# plot on screen
hist(daySum$x, main="Distribution of Daily Total Steps",
     xlab="Number of Steps", nclass=10)
```

```
## Error in hist(daySum$x, main = "Distribution of Daily Total Steps", xlab = "Number of Steps", : object 'daySum' not found
```

```r
# save plot
png(file="Proj1_fig1.png", width=480, height=480)
hist(daySum$x, main="Distribution of Daily Total Steps",
     xlab="Number of Steps", nclass=10)
```

```
## Error in hist(daySum$x, main = "Distribution of Daily Total Steps", xlab = "Number of Steps", : object 'daySum' not found
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

```r
dev.cur()
```

![plot of chunk fig1](figure/fig1-1.png)

```
## RStudioGD 
##         2
```

### 2c. Calculate and report the mean and median of the total number of steps taken per day

```r
summary(daySum$x) 
```

```
## Error in summary(daySum$x): object 'daySum' not found
```

```r
# Mean = 9354 
# Median = 10500
dailyMeanSteps <- mean(daySum$x, rm.na=TRUE)
```

```
## Error in mean(daySum$x, rm.na = TRUE): object 'daySum' not found
```

```r
# Mean = 9354.23
dailyMedianSteps <- median(daySum$x, rm.na=TRUE)
```

```
## Error in median(daySum$x, rm.na = TRUE): unused argument (rm.na = TRUE)
```

```r
# Median = 10395
```
#### Observation: Mean and median are slightly different from "summary" output. Why?


## What is the average daily activity pattern?

### 3a. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
summary(data$interval)
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

```r
summary(data$steps)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
myMean <- aggregate( x=data$steps[!is.na(data$steps)], 
                     by=list(interval=data$interval[!is.na(data$steps)]), 
                     FUN=mean )
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
head(myMean)
```

```
## Error in head(myMean): object 'myMean' not found
```

```r
str(myMean)
```

```
## Error in str(myMean): object 'myMean' not found
```

```r
summary(myMean$x)
```

```
## Error in summary(myMean$x): object 'myMean' not found
```

#### plot

```r
# plot on screen
plot(x=myMean$interval, y=myMean$x, type="l", 
     main="Mean Number of Steps in 5-Minutes", 
     xlab = "Interval", ylab="Mean Steps")
```

```
## Error in plot(x = myMean$interval, y = myMean$x, type = "l", main = "Mean Number of Steps in 5-Minutes", : object 'myMean' not found
```

```r
# save plot
png(file="Proj1_fig2.png", width=480, height=480)
plot(x=myMean$interval, y=myMean$x, type="l", 
     main="Mean Number of Steps in 5-Minutes", 
     xlab = "Interval", ylab="Mean Steps")
```

```
## Error in plot(x = myMean$interval, y = myMean$x, type = "l", main = "Mean Number of Steps in 5-Minutes", : object 'myMean' not found
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

```r
dev.cur()
```

![plot of chunk fig2](figure/fig2-1.png)

```
## RStudioGD 
##         2
```

### 3b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
myMean$interval[myMean$x==max(myMean$x)]
```

```
## Error in eval(expr, envir, enclos): object 'myMean' not found
```

```r
# Max = 835
```

## Imputing missing values

### 4a. Calculate and report the total number of missing values in the dataset

```r
# check whether there are any dates missing (no)
length(unique(data$date)) == max(as.Date(data$date))-min(as.Date(data$date))+1
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

```r
# check whether interval has missing values (no)
summary(data$interval)
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

```r
# check whether all intervals are present (yes, there are 2304 NA values)
length(data$interval) == length(unique(data$date)) * 24 * 12
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

```r
# Therefore, number of missing rows = number of missing values in data$steps

sum(is.na(data$steps))
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
# SumMissing = 2304
```

### 4b. Devise a strategy for filling in all of the missing values in the dataset.
#### My strategy: replace missing values with mean for that day

### 4c. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dMean <- rep(daySum$x/(24*12), each=24*12)
```

```
## Error in eval(expr, envir, enclos): object 'daySum' not found
```

```r
data1 <- data
data1$steps[is.na(data$steps)] <- dMean[is.na(data$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'dMean' not found
```

```r
summary(data1$steps)
```

```
## Error in data1$steps: object of type 'closure' is not subsettable
```

### 4d. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
daySum1 <- aggregate( x=data1$steps, 
                     by=list(date=data1$date), 
                     FUN=sum )
```

```
## Error in data1$steps: object of type 'closure' is not subsettable
```

```r
head(daySum1)
```

```
## Error in head(daySum1): object 'daySum1' not found
```

```r
str(daySum1)
```

```
## Error in str(daySum1): object 'daySum1' not found
```

```r
summary(daySum1$x)
```

```
## Error in summary(daySum1$x): object 'daySum1' not found
```

```r
# plot on screen
hist(daySum1$x, main="Distribution of Daily Total Steps\n(Missing Values Imputed)",
     xlab="Number of Steps", nclass=10)
```

```
## Error in hist(daySum1$x, main = "Distribution of Daily Total Steps\n(Missing Values Imputed)", : object 'daySum1' not found
```

```r
# save plot
png(file="Proj1_fig3.png", width=480, height=480)
hist(daySum1$x, main="Distribution of Daily Total Steps\n(Missing Values Imputed)",
     xlab="Number of Steps", nclass=10)
```

```
## Error in hist(daySum1$x, main = "Distribution of Daily Total Steps\n(Missing Values Imputed)", : object 'daySum1' not found
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

```r
dev.cur()
```

![plot of chunk fig3](figure/fig3-1.png)

```
## RStudioGD 
##         2
```


### It appears that imputation of missing values with mean steps of the day has no visible effect on the distribution, nor the mean and median. This is probably due to the nature of the missing values in the dataset, which likely occur only on certain days. Note that this result is strategy-dependent.                                                         



## Are there differences in activity patterns between weekdays and weekends?

### 5a. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
tf = is.member(weekdays(as.Date(data1$date)), c("Monday","Tuesday", 
                                            "Wednesday","Thursday","Friday") )
```

```
## Error in eval(expr, envir, enclos): could not find function "is.member"
```

```r
table(tf)
```

```
## Error in table(tf): object 'tf' not found
```

```r
str(tf)
```

```
## Error in str(tf): object 'tf' not found
```

```r
sum(tf(tf==1))
```

```
## Error in eval(expr, envir, enclos): could not find function "tf"
```

```r
F <- factor(tf)
```

```
## Error in factor(tf): object 'tf' not found
```

```r
levels(F)[levels(F)=="FALSE"] <- "weekend"
levels(F)[levels(F)=="TRUE"] <- "weekday"
str(F)
```

```
##  atomic [1:1] FALSE
##  - attr(*, "levels")= chr(0)
```

```r
head(F)
```

```
## [1] FALSE
```

```r
data1$F <- F
```

```
## Error in data1$F <- F: object of type 'closure' is not subsettable
```

```r
str(data1)
```

```
## function (..., list = character(), package = NULL, lib.loc = NULL, 
##     verbose = getOption("verbose"), envir = .GlobalEnv)
```

```r
head(data1)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

### 5b. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
myMean2 <- aggregate( steps ~ interval + F, data=data1, mean, na.rm=TRUE )
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

```r
str(myMean2)
```

```
## Error in str(myMean2): object 'myMean2' not found
```

```r
head(myMean2)
```

```
## Error in head(myMean2): object 'myMean2' not found
```

```r
tapply(myMean2$steps, myMean2$F, summary)
```

```
## Error in tapply(myMean2$steps, myMean2$F, summary): object 'myMean2' not found
```

```r
# plot on screen
library(ggplot2)
ggplot(weekData, aes(interval,steps))+geom_line()+facet_grid(F ~.)+
  labs(x="Interval")+labs(y="Mean steps")+
  labs(title="Activity Patterns")+
  facet_wrap(~F, ncol=1)
```

```
## Error in ggplot(weekData, aes(interval, steps)): object 'weekData' not found
```

```r
# save plot
png(file="Proj1_fig4.png", width=480, height=480)
ggplot(weekData, aes(interval,steps))+geom_line()+facet_grid(F ~.)+
  labs(x="Interval")+labs(y="Mean steps")+
  labs(title="Activity Patterns")+
  facet_wrap(~F, ncol=1)
```

```
## Error in ggplot(weekData, aes(interval, steps)): object 'weekData' not found
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

```r
dev.cur()
```

![plot of chunk fig4](figure/fig4-1.png)

```
## RStudioGD 
##         2
```

### It appears that while the general patterns are similar for weekends and weekdays, trend for weekdays contains more fine structures than that for weekends.
