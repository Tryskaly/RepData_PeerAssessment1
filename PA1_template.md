Assingment 1 - Course 5
=================================


```r
rm(list=ls())
library(knitr)
library(plyr)
library(ggplot2)
library(lattice)
```

#### 1. Loading the data


#### 2. Histogram of the total number of steps taken each day
total number of steps per day

```r
total_steps <- tapply(outcome$steps,outcome$date,sum,na.rm=TRUE)
```

histogram

```r
hist(total_steps, 
    main="Histogram of total number of steps",
    xlab="Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

#### 3. Mean and median number of steps taken each day

```r
mean <- mean(total_steps, na.rm = TRUE)
median <- median(total_steps, na.rm=TRUE)
```
The mean is 9354.2295082 and the median is 1.0395 &times; 10<sup>4</sup>

#### 4. Time series plot of the average number of steps taken

average for each interval

```r
steps_interval <- tapply(outcome$steps,outcome$interval, mean,na.rm=TRUE)
rows <- unique(outcome$interval)
```
plot

```r
plot(rows,steps_interval, type = "l",
     xlab="Interval", 
     ylab="Average steps", 
     main = "Time series plot of the average number of steps taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
#### 5. The 5-minute interval that, on average, contains the maximum number of steps


```r
rows[steps_interval==max(steps_interval)]
```

```
## [1] 835
```

#### 6. Code to describe and show a strategy for imputing missing data

- Calculate and report the total number of missing values in the dataset


```r
sum(is.na(outcome))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset

Taking into account the information of the last plot, it make sence to fill the
NAs with the average of each interval point.

- Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
new_outcome <- outcome
for (i in 1:length(new_outcome$steps)){
        if (is.na(new_outcome$steps[i])==TRUE){
            new_outcome$steps[i] <- steps_interval[rows==new_outcome$interval[i]]
        } 
}
```

#### 7. Histogram of the total number of steps taken each day after missing values are imputed

total number of steps per day

```r
new_total_steps <- tapply(new_outcome$steps,new_outcome$date,sum)
```

histogram

```r
hist(new_total_steps, 
    main="Histogram of total number of steps",
    xlab="Steps")
```

![plot of chunk histogram](figure/histogram-1.png)
- Mean and median number of steps taken each day

```r
new_mean <- mean(new_total_steps, na.rm = TRUE)
new_median <- median(new_total_steps, na.rm=TRUE)
```
- The mean is 1.0766189 &times; 10<sup>4</sup> and the new_median is 1.0395 &times; 10<sup>4</sup>

#### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
new_outcome <- cbind(new_outcome,days = weekdays(new_outcome$date))
new_outcome$days <- revalue(new_outcome$days,c("lunes"="weekday","martes"="weekday",
"miércoles"="weekday","jueves"="weekday","viernes"="weekday","sábado"="weekend",
"domingo"="weekend"))                                      
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
weekday_outcome <- subset(new_outcome,new_outcome$days=="weekday")
weekday_steps <- tapply(weekday_outcome$steps,weekday_outcome$interval,sum)
weekday_steps2 <- data.frame(total=weekday_steps, interval = rows, day ="Weekday")

weekend_outcome <- subset(new_outcome,new_outcome$days=="weekend")
weekend_steps <- tapply(weekend_outcome$steps,weekend_outcome$interval,sum)
weekend_steps2 <- data.frame(total=weekend_steps, interval = rows, day="Weekend")

steps <- rbind(weekday_steps2,weekend_steps2)
```



```r
xyplot(steps$total ~ steps$interval | steps$day, layout=c(1,2),
       type="l",
       main="Time Series Plot of the Average of Total Steps (weekday vs. weekend)",
       xlab="Time intervals (in minutes)",ylab="Average of Total Steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
