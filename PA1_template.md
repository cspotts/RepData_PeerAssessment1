---
title: "Reproducible Research Week 2 Assignment"
author: "Courtney Potts"
date: "14th April 2019"
output: 
  html_document:
    keep_md: true
---
## Load packages

```r
# knitr::opts_chunk$set(echo = FALSE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```
## Loading the data

```r
filepath <- "C:/Users/e16005195/Documents/Coursera/Reproducible Research/repdata_data_activity/activity.csv"
activity <- read.csv(filepath, header = TRUE)
activity_narm <- activity[complete.cases(activity),]
```

## Mean total number of steps per day

```r
stepsperday <- activity_narm %>%
  group_by(date) %>%
  summarise(steps=sum(steps))
sumsteps <- sum(stepsperday$steps)
meansteps <- mean(stepsperday$steps)
mediansteps <- median(stepsperday$steps)
```
The total number of steps per day is 570608.  The mean number of steps per day is 1.0766189\times 10^{4}.  The median number of steps per day is 10765

```r
hist(stepsperday$steps, 
     xlab="Steps Per Day", 
     main="Total number of steps taken each day"
     )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Average daily pattern

```r
stepsperinterval <- activity_narm %>%
  group_by(interval) %>%
  summarise(steps=sum(steps))
maxsteps <- stepsperinterval[which.max(stepsperinterval$steps),"interval"]
```
  The 5 minute interval that contains the maximum number of steps is 835, as shown on time series plot (red)  

```r
plot(stepsperinterval, type="l")
abline(v=maxsteps, col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Imputing missing values

```r
missingvals <- sum(is.na(activity))
meanstepsperint <- activity %>%
  select(steps, interval) %>%
  group_by(interval) %>%
  mutate(meansteps = mean(steps, na.rm=TRUE))

activity.replacena <- activity
for (i in 1:nrow(activity.replacena)) {
  if (is.na(activity.replacena$steps[i])) {
    activity.replacena$steps[i] <- meanstepsperint$meansteps[i]
  } else {
    activity.replacena$steps[i] <- activity.replacena$steps[i]
  }
}

newstepsperday <- activity.replacena %>%
  group_by(date) %>%
  summarise(steps=sum(steps))
newmean <- mean(newstepsperday$steps)
newmedian <- median(newstepsperday$steps)
```
The total number of missing values in the dataset is:  2304  These missing values have been replaced with the mean number of steps for each 5 minute interval.  The mean and median total number of steps taken per day is now 1.0766189\times 10^{4} and 1.0766189\times 10^{4}, respectively. These values are still similar to the mean/median calculated when the NA values were removed at the beginning.


```r
hist(newstepsperday$steps,
     xlab = "Steps per day",
     main= "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

## Differences in activity patterns between weekdays and weekends

```r
activity.replacena$date <- as.Date(activity.replacena$date)
activity.replacena$day <- ifelse(weekdays(activity.replacena$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
perinterval <- activity.replacena %>%
  group_by(day,interval) %>%
  summarise(steps=mean(steps))

ggplot(perinterval, aes(x=interval, y=steps)) +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(.~day)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
