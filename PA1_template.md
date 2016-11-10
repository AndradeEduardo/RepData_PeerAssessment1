# Reproducible Research: Peer Assessment 1


```r
## loading used libs and setting code to globaly appear along the document
library(ggplot2)
library(knitr)
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
  print("descompactou")
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
     ylab = "Frequency")
```

![](PA1_template_files/figure-html/stepsPerDay-1.png)<!-- -->

```r
meanSteps <- mean(stepsByDay$Steps, na.rm=TRUE)
medianSteps <- median(stepsByDay$Steps)
```

The mean of the total number of steps per day is 10,766.19.  
The median of the total number of steps per day is 10,766.19.

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



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
