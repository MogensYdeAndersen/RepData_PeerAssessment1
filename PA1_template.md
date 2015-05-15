---
title: "repdata_assignment1.rmd"
author: "Mogens Yde-Andersen"
date: "15. maj 2015"
output: html_document
---

###Loading and preprocessing the data

1.Load the data (i.e. read.csv())

```{r, echo=TRUE}
library(knitr)
library(data.table)
rawdata <- read.csv("./RepData_PeerAssessment1/activity.csv", header=TRUE, sep=",")
```

2.Process/transform the data (if necessary) into a format suitable for your analysis

I did not process anything at this stage.

###What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day

```{r}
sumstepsperday <- tapply(rawdata$steps, rawdata$date, FUN=sum, na.rm=TRUE)
head(sumstepsperday)
```

2.Make a histogram of the total number of steps taken each day

I think that this ggplot2 illustrates the distribution of summarized steps per day better than hist().
```{r}
library(ggplot2)
qplot(sumstepsperday, binwidth=1000, xlab="total number of steps taken each day", main="histogram created with ggplot2")
```

3.Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(sumstepsperday, na.rm=TRUE)
median(sumstepsperday, na.rm=TRUE)
```

###What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
meanstepsperinterval <- aggregate(x=list(steps=rawdata$steps), by=list(interval=rawdata$interval),
                      FUN=mean, na.rm=TRUE)
plot(meanstepsperinterval, type="l")
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
meanstepsperinterval[which.max(meanstepsperinterval$steps),]
```

###Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
nas <- is.na(rawdata$steps)
table(nas)
```

2.Devise a strategy for filling in all of the missing values in the dataset.

I replace missing values with the mean value of each 5 minute interval across all days.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
process.value <- function(steps, interval) {
    processed <- NA
    if (!is.na(steps))
        processed <- c(steps)
    else
        processed <- (meanstepsperinterval[meanstepsperinterval$interval==interval, "steps"])
    return(processed)
}
processed.data <- rawdata
processed.data$steps <- mapply(process.value, processed.data$steps, processed.data$interval)
head(processed.data)
```

4.Make a histogram of the total number of steps taken each day

```{r}
sumstepsperdayprocessed <- tapply(processed.data$steps, processed.data$date, FUN=sum, na.rm=TRUE)
head(sumstepsperdayprocessed)
```

I made the histogram wuth a ggplot2 qplot, since it shows the important details better.

```{r}
library(ggplot2)
qplot(sumstepsperdayprocessed, binwidth=1000, xlab="total number of steps taken each day", main="histogram created with ggplot2")
```

3.Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(sumstepsperdayprocessed, na.rm=TRUE)
median(sumstepsperdayprocessed, na.rm=TRUE)
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Yes, the mean og the median of the processed data deviates from the mean and the median of the raw data. In the raw data the NA's are set til the value 0, where as my strategi sets the value af the NA's to mean value of each 5 minute interval across all days. This "moves" all NA observations from the very left in a histogram rightwards and increases both mean and median.

###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

(Note that "Lørdag" and Søndag" are "Saturday" and"Sunday" in english.)

```{r}
processed.data$weekday <- weekdays(as.Date(processed.data$date, format = "%Y-%m-%d"))
processed.data$weekday.type <- factor(ifelse(processed.data$weekday=="Lørdag"|processed.data$weekday=="Søndag", "weekend", "weekday"), levels = c("weekday", "weekend"))
head(processed.data)
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```{r}
weekdaysteps <- processed.data[processed.data$weekday.type == "weekday", ]
weekendsteps <- processed.data[processed.data$weekday.type == "weekend", ]
meanweekdaysstepsperinterval <- sapply(split(weekdaysteps$steps, weekdaysteps$interval), mean)
meanweekendstepsperinterval <- sapply(split(weekendsteps$steps, weekendsteps$interval), mean)
meanweeksteps <- data.frame(rbind(meanweekdaysstepsperinterval, meanweekendstepsperinterval))
par(mfrow = c(2,1), mar = c(4,5,2,2))
plot(meanweekdaysstepsperinterval, type="l", ylim=c(0,250), xlim=c(0,300), main="Weekday", ylab="", xlab="5 minute interval")
plot(meanweekendstepsperinterval, type="l", ylim=c(0,250), xlim=c(0,300), main="Weekend", ylab="", xlab="5 minute interval")
par(mfrow=c(1,1), mar=c(3,2,2,2))
mtext(text="Mean number of steps", side=2)
```