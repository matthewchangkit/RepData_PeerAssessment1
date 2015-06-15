# Reading in the data set and making the necessary transformations
activity <- read.csv("Coursera/activity.csv",header=T)
head(activity)
activity$date <- as.Date(activity$date)


# Calculating the total no. of steps per day and plotting histogram
library(ggplot2)
s.p.Day <- aggregate(steps ~ date,data = activity, FUN = sum)
hist <- ggplot(s.p.Day,aes(x = s.p.Day$steps)) + geom_histogram(aes(fill=..count..))
hist + scale_fill_gradient("Count",low = "green",high = "red")

# Calculating the mean and median total no. of steps taken per day
MEAN <- mean(s.p.Day$steps)
MEDIAN <- median(s.p.Day$steps)

# Making a time series plot of the 5 min. interval and the average no. of 
# steps taken across all days
s.p.Int <- aggregate(steps ~ interval, data = activity, FUN = mean)
time <- ggplot(s.p.Int, aes(x = interval, y = steps)) 
time + geom_line(stat = "identity")

# Calculating the maximum number of steps averaged across all days on the 5 min.
# interval
s.p.Int$interval[which.max(s.p.Int$steps)]

# Calculating the total no. of missing values in dataset
length(which(is.na(activity))) 

# Testing the mean and median to choose which is better for filling in NAs
mean(na.omit(activity$steps)) #37.38
median(na.omit(activity$steps)) #0

# Filling in the missing data
n.activity <- activity
n.activity$steps[is.na(n.activity$steps)] <- mean(na.omit(activity$steps))

# Making new histogram
n.s.p.Day <- aggregate(steps ~ date, data = n.activity, FUN = sum)
hist <- ggplot(n.s.p.Day,aes(x = n.s.p.Day$steps))
hist + geom_histogram(aes(fill=..count..)) +scale_fill_gradient(
  "Count",low = "green",high = "red")

# Calculating the mean and median total no. of steps taken per day
N.MEAN <- mean(n.s.p.Day$steps)
N.MEDIAN <- median(n.s.p.Day$steps)

# Creating new factor variable
n.activity$day[weekdays(as.Date(n.activity$date)) %in% c("Saturday","sunday")] <-
  "weekend"
n.activity$day[!weekdays(as.Date(n.activity$date)) %in% c("Saturday","sunday")] <-
  "weekday"
n.activity$day <- as.factor(n.activity$day)

n.s.p.Int <- aggregate(steps ~ interval + day, data = n.activity,FUN =mean)
n.time <- ggplot(n.s.p.Int,aes(x=interval,y=steps,group=1))
n.time + geom_line() + facet_wrap(~ day, ncol=1)
