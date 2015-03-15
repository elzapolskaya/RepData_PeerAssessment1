---
title: "Peer Assessment 1"
author: "EZapolskaia"
date: "Sunday, March 15, 2015"
output: html_document
---
## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date**: The date on which the measurement was taken in YYYY-MM-DD format

**interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data
 Show any code that is needed to
 
1. Load the data (i.e. read.csv())

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
alldata<-read.csv(file = "activity.csv", header = TRUE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
alldata$interval <- sprintf("%02d:%02d", alldata$interval %/% 100, alldata$interval %% 100)
alldata$date<-as.Date(alldata$date)
na<-complete.cases(alldata)
data<-alldata[na, ]
data$date<-as.Date(data$date)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
t<-tapply(data$steps, data$date, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(t, main = "Steps per day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
tmean<-mean(t)
tmedian<- median(t)
```

The mean of the total number of steps taken per day **1.0766189 &times; 10<sup>4</sup>** and median  **10765**.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
int<-tapply(data$steps, data$interval, mean)
plot(strptime(names(int), format = "%H:%M"), 
     int,
     type = "l",
     main = "Average number of steps by Interval",
     xlab = "Interval", 
     ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxsteps<-max(int)
maxint<-names(int[which.max(int)])
```

Interval which contains the maximum number of steps **08:35**. Maximum number of steps **206.1698113**.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
comp<-sum(!complete.cases(alldata))
```

The total number of missing values in the dataset **2304**.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Each NA values - mean values for 5-minute interval. 


```r
alldata$meanind<-match(alldata$interval, names(int))
newsteps<-ifelse(is.na(alldata$steps), int[alldata$meanind], alldata$steps)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newdata<-transform(alldata, steps = newsteps)
```

4. Make a histogram of the total number of steps taken each day 


```r
t<-tapply(data$steps, data$date, sum)
t2<-tapply(newdata$steps, newdata$date, sum)
par(mfrow = c(1,1), mar = c(4, 4, 2, 1))
hist(t2, 
     main = "Steps per day",
     xlab = "Steps",
     ylim = c(0,40))
hist(t, 
     col = "green",
     main = "Steps per day",
     xlab = "Steps", 
     ylim = c(0,40),
     add=T)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

and Calculate and report the mean and median total number of steps taken per day.


```r
tmean2<-mean(t2)
tmedian2<- median(t2)
```

The mean of the total number of steps taken per day **1.0766189 &times; 10<sup>4</sup>** and median  **1.0766189 &times; 10<sup>4</sup>**.

Do these values differ from the estimates from the first part of the assignment?

Yes. The values `mean` no differ, differ from median **1.88679**.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
newdata$date<-as.Date(newdata$date)
newdata$wday <- factor(c("weekday", "weekend"))
newdata[weekdays(newdata$date) %in% c("Saturday", "Sunday"), ]$wday <- "weekend"
newdata[!(weekdays(newdata$date) %in% c("Saturday", "Sunday")), ]$wday <- "weekday" 
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
resdata<-aggregate(steps ~ interval + wday, newdata, mean) 
i <- round(seq(1, nrow(resdata) / 2, length.out=10))
resdata$interval <- as.POSIXct(strptime(resdata$interval, "%H:%M"))
xyplot(resdata$steps ~ resdata$interval|resdata$wday,
       main="Average number of steps by Interval",
       xlab="Interval", 
       ylab="Average number of steps",
       layout=c(1,2),
       type="l",
       scales = list(x = list(at = resdata[i, ]$interval, 
                             labels = format(resdata[i, ]$interval, "%H:%M"))))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
