Activity Monitoring Analysys
============================

# Note

Data file must be in a working directory.

# Loading and preprocessing the data.


```r
par(mfrow = c(1, 1))

data <- read.csv("activity.csv", stringsAsFactors = F)
data$DT <- strptime(data$date, format = "%Y-%m-%d")
data$WD <- weekdays(data$DT, abbreviate = T)
dx <- data[!is.na(data$steps), ]
```


# Mean of the total number of steps taken per day


```r
dx <- data[!is.na(data$steps), ]
len <- sum(dx$steps)
d0 <- character(length = len)
k <- 1L
for (i in seq(1, nrow(dx))) {
    cc <- dx[i, ]
    if (cc$steps > 0) {
        for (j in seq(1, cc$steps)) {
            d0[k] <- cc$date
            k <- k + 1L
        }
    }
}
d0 <- strptime(d0, format = "%Y-%m-%d")
hist(d0, breaks = "days", format = "%Y-%m-%d", col = "red", xlab = "Date", ylab = "Number of Steps", 
    main = "Number of Steps per Day", freq = TRUE)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


## Mean and median steps taken per day


```r
dx <- data[!is.na(data$steps), ]
d1 <- split(dx, factor(as.Date(dx$date, format = "%Y-%m-%d")))
d2 <- data.frame()
for (c in d1) {
    d2 <- rbind(d2, data.frame(Date = c[1, "DT"], Mean = mean(c$steps), Median = median(c$steps)))
}
d2
```

```
##          Date    Mean Median
## 1  2012-10-02  0.4375      0
## 2  2012-10-03 39.4167      0
## 3  2012-10-04 42.0694      0
## 4  2012-10-05 46.1597      0
## 5  2012-10-06 53.5417      0
## 6  2012-10-07 38.2465      0
## 7  2012-10-09 44.4826      0
## 8  2012-10-10 34.3750      0
## 9  2012-10-11 35.7778      0
## 10 2012-10-12 60.3542      0
## 11 2012-10-13 43.1458      0
## 12 2012-10-14 52.4236      0
## 13 2012-10-15 35.2049      0
## 14 2012-10-16 52.3750      0
## 15 2012-10-17 46.7083      0
## 16 2012-10-18 34.9167      0
## 17 2012-10-19 41.0729      0
## 18 2012-10-20 36.0938      0
## 19 2012-10-21 30.6285      0
## 20 2012-10-22 46.7361      0
## 21 2012-10-23 30.9653      0
## 22 2012-10-24 29.0104      0
## 23 2012-10-25  8.6528      0
## 24 2012-10-26 23.5347      0
## 25 2012-10-27 35.1354      0
## 26 2012-10-28 39.7847      0
## 27 2012-10-29 17.4236      0
## 28 2012-10-30 34.0938      0
## 29 2012-10-31 53.5208      0
## 30 2012-11-02 36.8056      0
## 31 2012-11-03 36.7049      0
## 32 2012-11-05 36.2465      0
## 33 2012-11-06 28.9375      0
## 34 2012-11-07 44.7326      0
## 35 2012-11-08 11.1771      0
## 36 2012-11-11 43.7778      0
## 37 2012-11-12 37.3785      0
## 38 2012-11-13 25.4722      0
## 39 2012-11-15  0.1424      0
## 40 2012-11-16 18.8924      0
## 41 2012-11-17 49.7882      0
## 42 2012-11-18 52.4653      0
## 43 2012-11-19 30.6979      0
## 44 2012-11-20 15.5278      0
## 45 2012-11-21 44.3993      0
## 46 2012-11-22 70.9271      0
## 47 2012-11-23 73.5903      0
## 48 2012-11-24 50.2708      0
## 49 2012-11-25 41.0903      0
## 50 2012-11-26 38.7569      0
## 51 2012-11-27 47.3819      0
## 52 2012-11-28 35.3576      0
## 53 2012-11-29 24.4688      0
```


# Average daily activity pattern


```r
dx <- data[!is.na(data$steps), ]
d1 <- split(dx, factor(dx$interval))
d3 <- data.frame()
for (c in d1) {
    d3 <- rbind(d3, data.frame(Interval = c[1, "interval"], Mean = mean(c$steps)))
}

plot(d3$Interval, d3$Mean, type = "l", lwd = 3, col = "red", main = "Average Steps per 5 Minutes Interval", 
    xlab = "Interval (minutes from the start of the day)", ylab = "Average steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


## Maximum number of steps in interval


```r
d3[order(d3$Mean, decreasing = T), ][1, ]
```

```
##     Interval  Mean
## 104      835 206.2
```


# Imputing missing values

Since some values are missing, impute the values based on mean for a pair interval/day of the week for existing values.

## Missing values count


```r
sum(is.na(data))
```

```
## [1] 2304
```


## Total values count


```r
nrow(data)
```

```
## [1] 17568
```


## Results of imputing missing values


```r
dx <- data[!is.na(data$steps), ]
dx$key <- paste(dx$interval, dx$WD, sep = ";")
d6 <- split(dx, factor(dx$key))
d7 <- data.frame()
for (c in d6) {
    d7 <- rbind(d7, data.frame(key = c[1, "key"], mean = mean(c$steps)))
}

dy <- data[is.na(data$steps), ]
dy$key <- paste(dy$interval, dy$WD, sep = ";")
st <- numeric()
for (i in 1:nrow(dy)) {
    cc <- dy[i, ]
    st <- c(st, d7[d7$key == paste(cc$interval, cc$WD, sep = ";"), "mean"])
}
dy$steps <- st

data1 <- rbind(dx, dy)

# Step 6
dx <- data1
len <- sum(dx$steps)
d0 <- character(length = len)
k <- 1L
for (i in seq(1, nrow(dx))) {
    cc <- dx[i, ]
    if (cc$steps > 0) {
        for (j in seq(1, cc$steps)) {
            d0[k] <- cc$date
            k <- k + 1L
        }
    }
}
d0 <- strptime(d0, format = "%Y-%m-%d")
hist(d0, breaks = "days", format = "%Y-%m-%d", col = "red", xlab = "Date", ylab = "Number of Steps", 
    main = "Number of Steps per Day", freq = TRUE)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


Actually there are no significant difference between original and modified dataset histogram.

# Differences in activity patterns between weekdays and weekends


```r
par(mfrow = c(2, 1))

dx <- data[!is.na(data$steps), ]

d0 <- dx[(dx$WD == "Sun") | (dx$WD == "Sat"), ]

d1 <- split(d0, factor(d0$interval))
d3 <- data.frame()
for (c in d1) {
    d3 <- rbind(d3, data.frame(Interval = c[1, "interval"], Mean = mean(c$steps)))
}
plot(d3$Interval, d3$Mean, type = "l", lwd = 3, col = "blue", main = "Weekend", 
    xlab = "Interval (minutes from the start of the day)", ylab = "Number of Steps")

d0 <- dx[!((dx$WD == "Sun") | (dx$WD == "Sat")), ]
d1 <- split(d0, factor(d0$interval))
d3 <- data.frame()
for (c in d1) {
    d3 <- rbind(d3, data.frame(Interval = c[1, "interval"], Mean = mean(c$steps)))
}
plot(d3$Interval, d3$Mean, type = "l", lwd = 3, col = "blue", main = "Weekday", 
    xlab = "Interval (minutes from the start of the day)", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

