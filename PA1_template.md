# Reproducible Research: Peer Assessment 1


```r
## loading used libs and setting code to globaly appear along the document
rm(list = ls())
library(ggplot2)
library(knitr)
library(chron)
opts_chunk$set(echo = TRUE)

# setting decimal number output
knit_hooks$set(inline = function(x) {
  prettyNum(round(x,2), big.mark=",")
})
```

## Loading and preprocessing the data

```r
if (!("activity.csv" %in% dir())){
  unzip("activity.zip")
}
actData <- read.csv("activity.csv",
                    header = TRUE,
                    colClasses = c("numeric", "Date", "numeric"),
                    na.strings = "NA")
```

## What is mean total number of steps taken per day?


```r
stepsByDay <- aggregate(actData$steps, by = list(actData$date), FUN = sum)
colnames(stepsByDay) <- c("Date", "Steps")

plot(stepsByDay$Date,
     stepsByDay$Steps,
     type = "h",
     lwd=8,
     main = "Total of Steps per Day",
     xlab = "Date",
     ylab = "Frequency",
     col = "orange")
```

![](PA1_template_files/figure-html/stepsPerDay-1.png)<!-- -->

```r
meanSteps <- mean(stepsByDay$Steps, na.rm=TRUE)
medianSteps <- median(stepsByDay$Steps, na.rm = TRUE)
```

The mean of the total number of steps per day is **10,766.19**.  
The median of the total number of steps per day is **10,765**.

## What is the average daily activity pattern?


```r
stepsByInterval <- aggregate(actData$steps, by = list(actData$interval), FUN = mean, na.rm = TRUE)
colnames(stepsByInterval) <- c("interval", "avgSteps")

plot(stepsByInterval$interval,
     stepsByInterval$avgSteps,
     type = "l",
     xlab = "Reading Intervals",
     ylab = "Average Number of Steps",
     main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/averagePattern-1.png)<!-- -->

```r
maxPos <- which(stepsByInterval$avgSteps == max(stepsByInterval$avgSteps))
maxAvgStepsInterval <- stepsByInterval$interval[maxPos]
```

The **835th** 5-minute interval is the interval that, on average across all the days in the dataset, contains the maximum number of steps



## Inputing missing values


The total number of missing values in the dataset is **2,304**

Missing values will be filled with the rounded value of the daily average number of steps
at the corresponding 5-minute interval.


```r
filledData <- actData

for (i in c(1:length(filledData$steps))){
  if(is.na(filledData$steps[i])){
    nRow = i %% 288
    if (nRow == 0){
      nRow <- 288
    }
    filledData$steps[i] <- round(stepsByInterval$avgSteps[nRow])
    i <- i + 1
  }
}

stepsByDayFil <- aggregate(filledData$steps, by = list(filledData$date), FUN = sum)
colnames(stepsByDayFil) <- c("Date", "Steps")

plot(stepsByDayFil$Date,
     stepsByDayFil$Steps,
     type = "h",
     lwd=8,
     main = "Total of Steps per Day after Filling Missing Values",
     xlab = "Date",
     ylab = "Frequency",
     col = "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
meanStepsFil <- mean(stepsByDayFil$Steps, na.rm=TRUE)
medianStepsFil <- median(stepsByDayFil$Steps)
```
The mean of the total number of steps per day after filling missing values is **10,765.64**.  
The median of the total number of steps per day after filling missing values is **10,762**.


## Are there differences in activity patterns between weekdays and weekends?


```r
# creating weekday | weekend factors in filled data set
filledData$fwDay <- factor(
  is.weekend(filledData$date),
  levels = c("FALSE", "TRUE"),
  labels = c("weekday", "weekend"))

# spliting filled data set by weekday | weekend factors
splitted <- split(filledData, filledData$fwDay)

# calculating average number of steps by interval in weekdays
stepsByIntervalBus <- aggregate(splitted[1]$weekday$steps, by = list(splitted[1]$weekday$interval), FUN = mean, na.rm = TRUE)
colnames(stepsByIntervalBus) <- c("interval", "avgSteps")
# creating column to mark this data set as referencing to weekdays
stepsByIntervalBus$day <- "weekday"

# calculating average number of steps by interval in weekends
stepsByIntervalEnd <- aggregate(splitted[2]$weekend$steps, by = list(splitted[2]$weekend$interval), FUN = mean, na.rm = TRUE)
colnames(stepsByIntervalEnd) <- c("interval", "avgSteps")
# creating column to mark this data set as referencing to weekends
stepsByIntervalEnd$day <- "weekend"

# merging calculated data sets again
stepsByDayWD <- do.call(rbind, list(stepsByIntervalBus, 
                  stepsByIntervalEnd))

# plotting required graph
ggplot(stepsByDayWD, aes(interval, avgSteps)) +
  geom_line() +
  facet_grid(day ~ .) +
  labs(x = "Intervals", y = "Average Steps", title = "Average Activity Pattern")
```

![](PA1_template_files/figure-html/weekDaysEndsPattern-1.png)<!-- -->

  After inputting missing values, we can see that the histogram changes dramatically in the days that there were no readings available. In the other days, which only a few readings were missing, there are only minor changes. Globally the strategy does not alter the data set significantly. A proof of it is that both average and median are almost the same as the original dataset.
