---
output:
  pdf_document: default
  html_document: default
---

Peer-graded Assignment: Course Project 1
==========================================

### Load and preprocessing the data

 1. Load the data (i.e. read.csv())
 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
if(!file.exists("~/data")){dir.create("C:/Users/wuguo/Documents/R/data")}
```

```
## Warning in dir.create("C:/Users/wuguo/Documents/R/data"): 'C:\Users\wuguo
## \Documents\R\data' already exists
```

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "C:/Users/wuguo/Documents/R/data/repdata_data_activity.zip", mode="wb")
activity <- unzip(zipfile = "C:/Users/wuguo/Documents/R/data/repdata_data_activity.zip")
data_activity <- read.csv(file = "activity.csv")
```
### What is mean total number of steps taken per day?

  1. Calculate the total number of steps taken per day
  2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
  3. Calculate and report the mean and median of the total number of steps taken per day


```r
steps_day <- aggregate(steps ~ date, data_activity, sum)
hist(steps_day$steps, main = "Total Steps Per Day", col = "red", xlab = "Number of Steps", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
step_mean <- mean(steps_day$steps)
step_mean
```

```
## [1] 10766.19
```

```r
step_median <- median(steps_day$steps)
step_median
```

```
## [1] 10765
```

### What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
2. Which 5-minute interval, on average across all the days in the dataset, ntains the maximum number of steps?


```r
step_interval <- aggregate(steps ~ interval, data_activity, mean)
plot(step_interval$interval, step_interval$steps, type="l", 
     xlab = "5-minute Interval", ylab = "Average Number of steps", 
     main = "Average Steps Number of 5-minute Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max_step <- step_interval[which.max(step_interval$steps), 1]
max_step
```

```
## [1] 835
```

### Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_NA_step <- sum(is.na(as.character(data_activity$steps)))
total_NA_step
```

```
## [1] 2304
```

```r
total_NA_data <- sum(is.na(as.character(data_activity$date)))
total_NA_data
```

```
## [1] 0
```

```r
total_NA_interval <- sum(is.na(as.character(data_activity$interval)))
total_NA_interval
```

```
## [1] 0
```

```r
data_activity$date <- as.Date(data_activity$date)
new_data <- data_activity
mean(new_data$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
new_data$steps[is.na(new_data$steps)] <- mean(new_data$steps, na.rm = TRUE)
colSums(is.na(new_data))
```

```
##    steps     date interval 
##        0        0        0
```

```r
step_eachday <- aggregate(steps ~ date, data = new_data, sum)
colnames(step_eachday) <- c("date", "steps")
hist(step_eachday$steps, main = "Total Steps Each Day", col = "red", 
     xlab = "Steps Number", ylab = "Frequency", breaks = 20)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
complete_mean <- mean(step_eachday$steps)
complete_mean
```

```
## [1] 10766.19
```

```r
complete_median <- median(step_eachday$steps)
complete_median
```

```
## [1] 10766.19
```

```r
mean_difference <- step_mean - complete_mean
mean_difference
```

```
## [1] 0
```

```r
median_difference <- step_median - complete_median
median_difference
```

```
## [1] -1.188679
```

### Are there differences in activity patterns between weekdays and weekends? 
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(ggplot2)
new_data$dayType <- ifelse(as.POSIXlt(new_data$date)$wday %in% c(0,6), "weekends","weekdays")
aggregateData <- aggregate(steps ~ interval + dayType, data=new_data, mean)
ggplot(aggregateData, aes(interval, steps)) +   geom_line(stat = "identity", aes(color = dayType)) +   facet_grid(dayType ~ .) +
  xlab("5-Minute Interval") +   ylab("Avarage Number of Steps") 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
tapply(new_data$steps, new_data$dayType, function(x){
  c(Min = min(x), Max = max(x), Mean = mean(x), Median = median(x))})
```

```
## $weekdays
##       Min       Max      Mean    Median 
##   0.00000 806.00000  35.61058   0.00000 
## 
## $weekends
##      Min      Max     Mean   Median 
##   0.0000 785.0000  42.3664   0.0000
```



