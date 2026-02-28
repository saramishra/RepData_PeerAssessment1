---
title: "Reproducible Research: Peer Graded1"
author: "Sara Mishra"
date: "2026-02-26"
output:
  html_document:
    keep_md: true
---



## Loading and Preprocessing Data


``` r
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

``` r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)

str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## Total Number of Steps Taken Per Day


``` r
steps_per_day <- data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

head(steps_per_day)
```

```
## # A tibble: 6 × 2
##   date       total_steps
##   <date>           <int>
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```


``` r
hist(steps_per_day$total_steps,
     main = "Total Steps Taken Per Day",
     xlab = "Total Steps",
     col = "lightblue",
     breaks = 15)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


``` r
mean_steps <- mean(steps_per_day$total_steps)
median_steps <- median(steps_per_day$total_steps)

mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

## Average Daily Activity Pattern


``` r
interval_avg <- data %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

head(interval_avg)
```

```
## # A tibble: 6 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1        0    1.72  
## 2        5    0.340 
## 3       10    0.132 
## 4       15    0.151 
## 5       20    0.0755
## 6       25    2.09
```


``` r
plot(interval_avg$interval,
     interval_avg$avg_steps,
     type = "l",
     xlab = "5-Minute Interval",
     ylab = "Average Steps",
     main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


``` r
max_interval <- interval_avg$interval[which.max(interval_avg$avg_steps)]
max_interval
```

```
## [1] 835
```

## Imputing Missing Values


``` r
sum(is.na(data$steps))
```

```
## [1] 2304
```


``` r
interval_means <- interval_avg

data_imputed <- data

for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_val <- data_imputed$interval[i]
    data_imputed$steps[i] <- interval_means$avg_steps[
      interval_means$interval == interval_val]
  }
}
```


``` r
steps_per_day_imp <- data_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

hist(steps_per_day_imp$total_steps,
     main = "Total Steps Per Day (Imputed Data)",
     xlab = "Total Steps",
     col = "lightgreen",
     breaks = 15)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


``` r
mean_imp <- mean(steps_per_day_imp$total_steps)
median_imp <- median(steps_per_day_imp$total_steps)

mean_imp
```

```
## [1] 10766.19
```

``` r
median_imp
```

```
## [1] 10766.19
```
## Weekday vs Weekend Analysis


``` r
data_imputed$day_type <- ifelse(
  weekdays(data_imputed$date) %in% c("Saturday", "Sunday"),
  "weekend",
  "weekday"
)

data_imputed$day_type <- as.factor(data_imputed$day_type)
```


``` r
interval_daytype <- data_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))
```

```
## `summarise()` has regrouped the output.
## ℹ Summaries were computed grouped by interval and day_type.
## ℹ Output is grouped by interval.
## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
## ℹ Use `summarise(.by = c(interval, day_type))` for per-operation grouping
##   (`?dplyr::dplyr_by`) instead.
```

``` r
head(interval_daytype)
```

```
## # A tibble: 6 × 3
## # Groups:   interval [3]
##   interval day_type avg_steps
##      <int> <fct>        <dbl>
## 1        0 weekday     2.25  
## 2        0 weekend     0.215 
## 3        5 weekday     0.445 
## 4        5 weekend     0.0425
## 5       10 weekday     0.173 
## 6       10 weekend     0.0165
```


``` r
library(lattice)

xyplot(avg_steps ~ interval | day_type,
       data = interval_daytype,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
