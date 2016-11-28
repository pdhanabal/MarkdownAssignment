---
title: "Activity Monitoring Analysis"
author: "PD"
date: "November 27, 2016"
output: html_document
---




## Data

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Preparing R Session

```r
library(knitr)
library(dplyr)
library(ggplot2)
```
## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Transform the data into a format suitable for your analysis


```r
activity <- read.csv("activity.csv",na.strings = "NA")
activity[,2]<-as.Date(activity$date)
#activity <- activity[ with (activity, { !(is.na(steps)) } ), ]
```

# What is mean total number of steps taken per day?

```r
datesum<-activity %>% group_by(date) %>% summarise(per_day=sum(steps))
```
### Histogram of the total number of steps taken each day


```r
hist(datesum$per_day)
```

![plot of chunk Plot1](figure/Plot1-1.png)
## Mean and Median

```r
summary(datesum)
```

```
##       date               per_day     
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-31   Median :10765  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:13294  
##  Max.   :2012-11-30   Max.   :21194  
##                       NA's   :8
```
## What is the average daily activity pattern?

```r
steps_by_interval <- aggregate(steps ~ interval, activity, mean)
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps_row <- which.max(steps_by_interval$steps)
steps_by_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
sum(is.na(activity))
```

```
## [1] 2304
```
### The Total No. of NA's 2304
## Imputing missing values/ Getting Data Fixed by replacing NA's with mean of the 5-min interval


```r
data_fixed <- activity
for (i in 1:nrow(data_fixed)) {
    if (is.na(data_fixed$steps[i])) {
        interval_value <- data_fixed$interval[i]
        steps_value <- steps_by_interval[
            steps_by_interval$interval == interval_value,]
        data_fixed$steps[i] <- steps_value$steps
    }
}

# calculate  total number of steps taken each day
data_fixed_steps_by_day <- aggregate(steps ~ date, data_fixed, sum)
head(data_fixed_steps_by_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```
## Histogram

```r
hist(data_fixed_steps_by_day$steps, main="Histogram of total number of steps per day (fixed)", 
     xlab="Total number of steps in a day")
```

![plot of chunk Plot2](figure/Plot2-1.png)
## Mean and Median of fixed Data

```r
mean(data_fixed_steps_by_day$steps)
```

```
## [1] 10766.19
```

```r
median(data_fixed_steps_by_day$steps)
```

```
## [1] 10766.19
```

```r
# get mean and median of data without NA's
activity <- activity[ with (activity, { !(is.na(steps)) } ), ]
datesum<-activity %>% group_by(date) %>% summarise(per_day=sum(steps))
mean(datesum$per_day)
```

```
## [1] 10766.19
```

```r
median(datesum$per_day)
```

```
## [1] 10765
```

```r
data_fixed['type_of_day'] <- weekdays(as.Date(data_fixed$date))
data_fixed$type_of_day[data_fixed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_fixed$type_of_day[data_fixed$type_of_day != "weekend"] <- "weekday"
# convert type_of_day from character to factor
data_fixed$type_of_day <- as.factor(data_fixed$type_of_day)

# calculate average steps by interval across all days
data_fixed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_fixed, mean)
```
## Panel plot for Weekdays and Weekends...

```r
# create a plot
qplot(interval, 
      steps, 
      data = data_fixed_steps_by_interval, 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
    facet_wrap(~ type_of_day, ncol = 1)
```

![plot of chunk qplot](figure/qplot-1.png)

