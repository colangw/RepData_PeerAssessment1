#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")


fileurl <- "data/activity.csv"
activity <- read.csv(fileurl, header = TRUE)
activity$date <- as.Date(activity$date)

# summary(activity)
# str(activity)
activityByDay <- aggregate(. ~ date, data = activity, FUN = sum)
hist(activityByDay$steps, ylab = "Number of Days", xlab = "Steps", main = "Total Steps per Day (no NAs)")

# Interesting plot but not needed for assignment
# plot(activityByDay$date, activityByDay$steps, type = "h", ylab = "Steps", xlab = "Day", main = "Total Steps by Day")

activityByDayMean <- sapply(activityByDay, mean, na.rm = TRUE)[2]
activityByDayMedian <- sapply(activityByDay, median, na.rm = TRUE)[2]
activityByDayMean
activityByDayMedian

# clean out NA's from the dataset
activityNoNA <- activity[!sapply(activity$steps, is.na), ]
# get the average number of steps taken per 5 minute interval, averaged across all days
activityFiveMinMean <- aggregate(. ~ interval, data=activityNoNA, FUN=mean)

plot(activityFiveMinMean$interval, activityFiveMinMean$steps, type = "l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps", main = "5-minute interval steps averaged across all days")

# get the max of the averaged 5-minute interval steps and its corresponding interval
maxAveSteps <- max(activityFiveMinMean$steps)
maxAveSteps
activityFiveMinMean$interval[match(maxAveSteps, activityFiveMinMean$steps)]

# Total NA's
summary(activity$steps)[7]

# make a copy of the activity DF
activityClean <- activity

# get the row numbers for the NA rows
badrows <- which(is.na(activityClean$steps))

# Replace NA's with the interval mean (mean calculated for that interval across all days)
activityClean[badrows, 1] <- round(activityFiveMinMean[match(activity[sapply(activity$steps, is.na), 3], activityFiveMinMean$interval), 2], 0)

activityByDayClean <- aggregate(. ~ date, data = activityClean, FUN = sum)
hist(activityByDayClean$steps, ylab = "Number of Days", xlab = "Steps", main = "Total Steps per Day (Clean data)")

activityByDayMeanClean <- sapply(activityByDayClean, mean, na.rm = TRUE)[2]
activityByDayMedianClean <- sapply(activityByDayClean, median, na.rm = TRUE)[2]
activityByDayMeanClean
activityByDayMedianClean

activityClean$day <- weekdays(activityClean$date)
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
cat <- c("Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend", "Weekend")
days <- data.frame(day, cat)
names(days) <- c("day", "cat")
activityClean$cat <- days[match(activityClean$day, days$day), 2]

# get the average number of steps taken per 5 minute interval, averaged across all days
library(ggplot2)
averages <- aggregate(steps ~ interval + cat, data=activityClean, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(cat ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")