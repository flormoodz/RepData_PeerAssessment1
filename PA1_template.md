Peer-graded Assignment: Course Project 1
--------------------------------



**I. Loading and preprocessing the data**

Below are the codes for reading in the dataset and/or processing the data. The data has 17568 observations and consists of three variables: steps, date, and interval.

```r
Activity <-read.csv("activity.csv")
str(Activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```



**II. What is mean total number of steps taken per day?**

From above, it can be seen that there exists missing values, which will be ignored first in this analysis. 
  
  - Here's the code for getting the total number of steps taken per day.

```r
totalSteps<- aggregate(steps ~ date, Activity, sum)
```
  
 -  Make a histogram of the total number of steps taken each day.

```r
hist(totalSteps$steps, main="Histogram of Daily Total Number of Steps Taken", xlab="Total Steps", breaks=20)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)
  
  - Calculate and report the mean and median of the total number of steps taken per day.

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```



**III. What is the average daily activity pattern?**

  - Create a time series plot of the average number of steps taken using the codes below.

```r
averageSteps <- aggregate(steps ~ interval, Activity, mean)
plot(averageSteps$steps~averageSteps$interval, type="l", xlab="Interval (in minutes)", ylab="Daily average no. of steps", main="Average Number of Steps Taken per 5-minute Interval")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

  - The 5-minute interval that, on average, contains the maximum number of steps can be observed using these codes.

```r
maxInterval <- averageSteps$interval[which.max(averageSteps$steps)]
maxInterval
```

```
## [1] 835
```



**IV. Imputing missing values**
Note that there are a number of days/intervals where there are missing values. The presence of missing days may introduce bias into some calculations or summaries of the data. 

  - The total number of missing values in the dataset is 2304, which is approximately 13.11% of the observations.

```r
sum(is.na(Activity$steps))
```

```
## [1] 2304
```
 
  - A strategy is devised to fill in all of the missing values in the dataset. I use the mean for that 5-minute interval across days. With the help of dplyr package, the following codes are run in R. 

```r
library(dplyr)
Activity2 <- Activity %>% 
             group_by(interval) %>% 
             mutate(steps = replace(steps, is.na(steps), mean(steps,na.rm = TRUE)))
```
    Now, there are no more missing values.

```r
sum(is.na(Activity2$steps))
```

```
## [1] 0
```

  - The new histogram of the total number of steps taken each day, after missing values are imputed, can be found below.

```r
totalSteps2<- aggregate(steps ~ date, Activity2, sum)
hist(totalSteps2$steps, main="Histogram of Daily Total Number of Steps Taken with Imputed Values", xlab="Total Steps", breaks=20)
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)

  - The new mean and median are below. Notice that the mean is still the same. Meanwhile, the median moved towards the value of the mean.
  

```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```



**V. Are there differences in activity patterns between weekdays and weekends?**
  - Create first a new factor variable for weekdays and weekends using the following codes.

```r
Activity2$day <- weekdays(as.Date(Activity2$date))
Activity2$typeDay <- as.factor(ifelse(Activity2$day == "Saturday" | Activity2$day == "Sunday", "Weekend", "Weekday"))
```
  
  - Create a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends. Do not forget to take the average number of steps taken per 5-minute interval in each type of day.
  

```r
averageSteps2 <- aggregate(steps ~ interval+typeDay, Activity2, mean)

library(ggplot2)

z<- ggplot(averageSteps2, aes(interval, steps))
z+geom_line()+facet_grid(typeDay~.)+ggtitle("Average Number of Steps Taken per 5-minute Interval")+labs(x="Interval (in minutes)", y="Average no. of steps")+theme_bw(base_family="")  
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

