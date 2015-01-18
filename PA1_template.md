---
title: 'Reproducible Research: Peer Assessment 1'
output:
  word_document: default
  pdf_document: default
  html_document:
    keep_md: yes
---
# Reproducible Research: Peer Assessment 1

## A) Loading and preprocessing the data
0) First, clear any environment variables and clear the console as well.


```r
rm(list=ls()) #Clear Workspace
cat("\014") # Clear Console:
```



1) Load the data (i.e. read.csv()). Note that the data is assumed to be in a subfolder called "data".


```r
fileurl <- "data/activity.csv"
activity <- read.csv(fileurl, header = TRUE)
activity$date <- as.Date(activity$date)
```

2) Process/transform the data (if necessary) into a format suitable for your analysis. The date is loaded in as date value.

```r
activity$date <- as.Date(activity$date)
```

## B) What is mean total number of steps taken per day?
1) Make a histogram of the total number of steps taken each day.

```r
activityByDay <- aggregate(. ~ date, data = activity, FUN = sum)
hist(activityByDay$steps, ylab = "Number of Days", xlab = "Steps", main = "Total Steps per Day (no NAs)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2) Calculate and report the **mean** and **median** total number of steps taken per day.

```r
activityByDayMean <- sapply(activityByDay, mean, na.rm = TRUE)[2]
activityByDayMedian <- sapply(activityByDay, median, na.rm = TRUE)[2]
activityByDayMean
```

```
##    steps 
## 10766.19
```

```r
activityByDayMedian
```

```
## steps 
## 10765
```


## C) What is the average daily activity pattern?
1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# clean out NA's from the dataset
activityNoNA <- activity[!sapply(activity$steps, is.na), ]
# get the average number of steps taken per 5 minute interval, averaged across all days
activityFiveMinMean <- aggregate(. ~ interval, data=activityNoNA, FUN=mean)
plot(activityFiveMinMean$interval, activityFiveMinMean$steps, type = "l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps", main = "5-minute interval steps averaged across all days")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxAveSteps <- max(activityFiveMinMean$steps)
maxAveSteps
```

```
## [1] 206.1698
```

```r
activityFiveMinMean$interval[match(maxAveSteps, activityFiveMinMean$steps)]
```

```
## [1] 835
```

## D) Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
# Total NA's
summary(activity$steps)[7]
```

```
## NA's 
## 2304
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. _I decided to replace NA's with the interval mean (i.e. the mean calculated for that interval across all days)_

3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# make a copy of the activity DF
activityClean <- activity
# get the row numbers for the NA rows
badrows <- which(is.na(activityClean$steps))
# Replace NA's with the interval mean (mean calculated for that interval across all days)
activityClean[badrows, 1] <- round(activityFiveMinMean[match(activity[sapply(activity$steps, is.na), 3], activityFiveMinMean$interval), 2], 0)
```

4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activityByDayClean <- aggregate(. ~ date, data = activityClean, FUN = sum)
hist(activityByDayClean$steps, ylab = "Number of Days", xlab = "Steps", main = "Total Steps per Day (Clean data)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
activityByDayMeanClean <- sapply(activityByDayClean, mean, na.rm = TRUE)[2]
activityByDayMedianClean <- sapply(activityByDayClean, median, na.rm = TRUE)[2]
activityByDayMeanClean
```

```
##    steps 
## 10765.64
```

```r
activityByDayMedianClean
```

```
## steps 
## 10762
```
_The values differ only nominally from the estimates from the first part of the assigment. The impact of imputing missing data on the estimates of the total daily number of steps is minimal._


## E) Are there differences in activity patterns between weekdays and weekends?
0) For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityClean$day <- weekdays(activityClean$date)
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
cat <- c("Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend", "Weekend")
days <- data.frame(day, cat)
names(days) <- c("day", "cat")
activityClean$cat <- days[match(activityClean$day, days$day), 2]
```

2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
# get the average number of steps taken per 5 minute interval, averaged across all days
library(ggplot2)
averages <- aggregate(steps ~ interval + cat, data=activityClean, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(cat ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

## F) Footnote

Code to create the final HTML document:

```r
library(knitr)
setwd("C:/Users/werner/Dropbox/Programming/Coursera/DataScience_JH_specialization/5_Reproducible_Research/PeerAssessment_1/RepData_PeerAssessment1")
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")
```

