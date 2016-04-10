---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

rm( list = ls() )

data <- read.csv('data/activity.csv')

head(data)

str(data)
#### check missing values 
summary(data$steps) 

summary(data$interval) 

summary(data$date) 


## What is mean total number of steps taken per day?

### 2a. Calculate the total number of steps taken per day
daySum <- aggregate( x=data$steps[!is.na(data$steps)], 
                     by=list(date=data$date[!is.na(data$steps)]), 
                     FUN=sum)

head(daySum)

str(daySum)

### 2b. Make a histogram of the total number of steps taken each day
#### plot on screen
hist(daySum$x, main="Distribution of Daily Total Steps",
     xlab="Number of Steps", nclass=10)
#### save plot
png(file="Proj1_fig1.png", width=480, height=480)

hist(daySum$x, main="Distribution of Daily Total Steps",
     xlab="Number of Steps", nclass=10)

dev.off()

dev.cur()

### 2c. Calculate and report the mean and median of the total number of steps taken per day
summary(daySum$x) 
#### Mean = 9354 
#### Median = 10500
dailyMeanSteps <- mean(daySum$x, rm.na=TRUE)

dailyMeanSteps
#### Mean = 9354.23
dailyMedianSteps <- median(daySum$x, rm.na=TRUE)

dailyMedianSteps
#### Median = 10395

### Observation: Mean and median are slightly different from "summary" output. Why?


## What is the average daily activity pattern?

### 3a. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
summary(data$interval)

summary(data$steps)

myMean <- aggregate( x=data$steps[!is.na(data$steps)], 
                     by=list(interval=data$interval[!is.na(data$steps)]), 
                     FUN=mean )

head(myMean)

str(myMean)

summary(myMean$x)

#### plot on screen
plot(x=myMean$interval, y=myMean$x, type="l", 
     main="Mean Number of Steps in 5-Minutes", 
     xlab = "Interval", ylab="Mean Steps")
#### save plot
png(file="Proj1_fig2.png", width=480, height=480)

plot(x=myMean$interval, y=myMean$x, type="l", 
     main="Mean Number of Steps in 5-Minutes", 
     xlab = "Interval", ylab="Mean Steps")

dev.off()

dev.cur()


### 3b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
myMean$interval[myMean$x==max(myMean$x)]
#### Max = 835


## Imputing missing values

### 4a. Calculate and report the total number of missing values in the dataset

#### check whether there are any dates missing (no)
length(unique(data$date)) == max(as.Date(data$date))-min(as.Date(data$date))+1

#### check whether interval has missing values (no)
summary(data$interval) 

#### check whether all intervals are present (yes, there are 2304 NA values)
length(data$interval) == length(unique(data$date)) * 24 * 12

#### Therefore, number of missing rows = number of missing values in data$steps

sum(is.na(data$steps))
#### SumMissing = 2304


### 4b. Devise a strategy for filling in all of the missing values in the dataset.
#### My strategy: replace missing values with mean for that day

### 4c. Create a new dataset that is equal to the original dataset but with the missing data filled in.
dMean <- rep(daySum$x/(24*12), each=24*12)

data1 <- data

data1$steps[is.na(data$steps)] <- dMean[is.na(data$steps)]

summary(data1$steps)

### 4d. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
daySum1 <- aggregate( x=data1$steps, 
                     by=list(date=data1$date), 
                     FUN=sum )

head(daySum1)

str(daySum1)

summary(daySum1$x)

#### plot on screen
hist(daySum1$x, main="Distribution of Daily Total Steps\n(Missing Values Imputed)",
     xlab="Number of Steps", nclass=10)
#### save plot
png(file="Proj1_fig3.png", width=480, height=480)

hist(daySum1$x, main="Distribution of Daily Total Steps\n(Missing Values Imputed)",
     xlab="Number of Steps", nclass=10)

dev.off()

dev.cur()

### It appears that imputation of missing values with mean steps of the day has no visible effect on the distribution, nor the mean and median. This is probably due to the nature of the missing values in the dataset, which likely occur only on certain days. Note that this result is strategy-dependent.                                                         



## Are there differences in activity patterns between weekdays and weekends?

### 5a. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
tf = is.member(weekdays(as.Date(data1$date)), c("Monday","Tuesday", 
                                            "Wednesday","Thursday","Friday") )

table(tf)

str(tf)

sum(tf(tf==1))

F <- factor(tf)

levels(F)[levels(F)=="FALSE"] <- "weekend"

levels(F)[levels(F)=="TRUE"] <- "weekday"

str(F)

head(F)

data1$F <- F

str(data1)

head(data1)


### 5b. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

myMean2 <- aggregate( steps ~ interval + F, data=data1, mean, na.rm=TRUE )

str(myMean2)

head(myMean2)

tapply(myMean2$steps, myMean2$F, summary)

#### plot on screen
library(ggplot2)

ggplot(weekData, aes(interval,steps))+geom_line()+facet_grid(F ~.)+
  labs(x="Interval")+labs(y="Mean steps")+
  labs(title="Activity Patterns")+
  facet_wrap(~F, ncol=1)
#### save plot
png(file="Proj1_fig4.png", width=480, height=480)

ggplot(weekData, aes(interval,steps))+geom_line()+facet_grid(F ~.)+
  labs(x="Interval")+labs(y="Mean steps")+
  labs(title="Activity Patterns")+
  facet_wrap(~F, ncol=1)

dev.off()

dev.cur()

### It appears that while the general patterns are similar for weekends and weekdays, trend for weekdays contains more fine structures than that for weekends.
