# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
  DF <- read.csv("activity.csv",header=T)
  head(DF)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

First, the total number of steps per day is computed using tapply and the result is saved in a data.frame object.


```r
spd <- tapply(DF$steps,DF$date,sum)
spd_df <- data.frame(spd = spd)
```

Then, a histogram, the mean and the median of the total number of steps per day are computed.


```r
hist(spd_df$spd,col="red",breaks=10)
```

![](PA1_template_files/figure-html/mean total number-1.png) 

```r
mean(spd_df$spd,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(spd_df$spd,na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

The mean across all days of the number of steps in each 5-minute interval (ignoring the missing values) can be computed by 


```r
spi <- tapply(DF$steps,DF$interval,mean,na.rm=T)
```

The corresponding plot is


```r
intervals <- DF$interval[1:nrow(spi)]
plot(intervals,spi,type="l")
```

![](PA1_template_files/figure-html/plot mean number-1.png) 

The 5-minute interval containing the maximum number of steps can be computed by


```r
intervals[spi == max(spi)]
```

```
## [1] 835
```

## Imputing missing values

### 1 Total number of NAs in the dataset

The total number of missing values in the dataset can be computed by


```r
total_na <- sum(as.numeric(is.na(DF$steps)))
print(total_na)
```

```
## [1] 2304
```

### 2 Filling the missing values in the data set

The NAs in the dataset are replaced by the mean of that 5-minute interval across all days. 

### 3 Creating the new data set with the replaced NAs

The new dataset is called DF1. It can be created by


```r
DF1 <- DF
ind <- vector("numeric",length = total_na)

for (i in 1:total_na){
    ind[i] <- which(intervals == DF$interval[is.na(DF$steps)][i])
    DF1$steps[is.na(DF$steps)][i] <- spi[[ind[i]]]
}
head(DF)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
head(DF1)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

### 4 The histogram, mean and median of the total number of steps per day are computed by


```r
spd1 <- tapply(DF1$steps,DF1$date,sum)
spd1_df <- data.frame(spd = spd1)
hist(spd1_df$spd, col = "blue",breaks=10)
```

![](PA1_template_files/figure-html/mean total number DF1-1.png) 

```r
mean(spd1_df$spd)
```

```
## [1] 10766.19
```

```r
median(spd1_df$spd)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
