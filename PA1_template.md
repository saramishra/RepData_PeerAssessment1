# Reproducible Research - Peer Assessment 1

## Loading and Preprocessing the Data

```r
library(dplyr)

data <- read.csv("activity.csv")
data$date <- as.Date(data$date)

str(data)
summary(data)
```

---

## What is mean total number of steps taken per day?

```r
steps_per_day <- data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

head(steps_per_day)
```

### Histogram of Total Steps Per Day

```r
hist(steps_per_day$total_steps,
     main = "Total Steps Taken Per Day",
     xlab = "Total Steps",
     col = "lightblue",
     breaks = 15)
```

### Mean and Median

```r
mean_steps <- mean(steps_per_day$total_steps)
median_steps <- median(steps_per_day$total_steps)

mean_steps
median_steps
```

The mean total number of steps per day is approximately 10766.  
The median total number of steps per day is approximately 10765.

---

## What is the average daily activity pattern?

```r
interval_avg <- data %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

head(interval_avg)
```

### Time Series Plot

```r
plot(interval_avg$interval,
     interval_avg$avg_steps,
     type = "l",
     xlab = "5-Minute Interval",
     ylab = "Average Steps",
     main = "Average Daily Activity Pattern")
```

### Interval with Maximum Average Steps

```r
max_interval <- interval_avg$interval[which.max(interval_avg$avg_steps)]
max_interval
```

The 5-minute interval with the highest average number of steps is 835.

---

## Imputing Missing Values

### Total Missing Values

```r
sum(is.na(data$steps))
```

There are 2304 missing values in the dataset.

### Imputation Strategy

Missing values are replaced with the mean for that 5-minute interval across all days.

```r
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

---

## Histogram After Imputation

```r
steps_per_day_imp <- data_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

hist(steps_per_day_imp$total_steps,
     main = "Total Steps Per Day (Imputed Data)",
     xlab = "Total Steps",
     col = "lightgreen",
     breaks = 15)
```

### Mean and Median After Imputation

```r
mean_imp <- mean(steps_per_day_imp$total_steps)
median_imp <- median(steps_per_day_imp$total_steps)

mean_imp
median_imp
```

After imputation, both the mean and median total number of steps per day are approximately 10766.19.  
Imputing missing values reduces bias and makes the distribution more symmetric.

---

## Are there differences in activity patterns between weekdays and weekends?

```r
data_imputed$day_type <- ifelse(
  weekdays(data_imputed$date) %in% c("Saturday", "Sunday"),
  "weekend",
  "weekday"
)

data_imputed$day_type <- as.factor(data_imputed$day_type)
```

```r
interval_daytype <- data_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))

head(interval_daytype)
```

### Panel Plot

```r
library(lattice)

xyplot(avg_steps ~ interval | day_type,
       data = interval_daytype,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Average Steps")
```

The activity pattern differs slightly between weekdays and weekends. Weekdays show sharper morning peaks compared to weekends.
