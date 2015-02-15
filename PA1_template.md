
## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


### Loading and preprocessing the data

The following code downloads, unzips and loads the code.


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              "data/repdata_data_activity.zip",method="curl")
unzip("data/repdata_data_activity.zip",exdir = "data")

data = read.csv("data/activity.csv",header=T)
```

### What is mean total number of steps taken per day?

Ignoring the missing values in the dataset, a histogram is shown below of the total number of steps taken each day.  The mean and median are shown in the title of the graph.


```r
totStep = aggregate(steps~date,data=subset(data,!is.na(steps)),FUN=sum)

meanTotSteps = mean(totStep$steps)
medianTotSteps = median(totStep$steps)
main=sprintf("Histogram of total steps in a day (ignoring missing)\nMean = %4.2f, Median %4.2f",
             meanTotSteps,medianTotSteps)
hist(totStep$steps,main=main,xlab="Total Steps",breaks=20,xlim = c(0,25000))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### What is the average daily activity pattern?

Again, ignoring the missing values in the dataset, a time series plot is shown below averaging the number of steps in each 5-minute interval across all the days.  The maximum interval is shown in the title.


```r
meanStep = aggregate(steps~interval,data=subset(data,!is.na(steps)),FUN=mean)

intervalMax = meanStep$interval[which(meanStep$steps == max(meanStep$steps))]
main=sprintf("Time series plot of mean steps by interval (ignoring missing)\nMaximum occured at interval %d",
             intervalMax)
plot(meanStep$steps~meanStep$interval,main=main,xlab="Time Interval",ylab="Mean Steps",type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

### Imputing missing values

There are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

The tables below show the number of missing values, and that all the missing is confined to 8 days.


```r
table(is.na(data$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
missingTot = aggregate(is.na(steps)~date,data = data, FUN=sum)
names(missingTot)[2] = "missingTot"
subset(missingTot,missingTot>0)
```

```
##          date missingTot
## 1  2012-10-01        288
## 8  2012-10-08        288
## 32 2012-11-01        288
## 35 2012-11-04        288
## 40 2012-11-09        288
## 41 2012-11-10        288
## 45 2012-11-14        288
## 61 2012-11-30        288
```

Since the missingness is by day, the imputation strategy that makes the most sense is to use the mean for the interval for the non-missing days.

dataNew below is a new dataset where the missing values are filled in with the mean value for that interval.


```r
dataNew = merge(data,meanStep,by="interval",suffix = c("","_mean"))
dataNew$steps = ifelse(is.na(dataNew$steps),dataNew$steps_mean,dataNew$steps)
dataNew$steps_mean = NULL
```

Below is a set of histograms before and after imputation.  Each shows the total number of steps taken each day and the title of each plot shows the mean and median.


```r
totStepI = aggregate(steps~date,data=dataNew,FUN=sum)

meanTotStepsI = mean(totStepI$steps)
medianTotStepsI = median(totStepI$steps)

mainI=sprintf("Histogram of total steps in a day (after imputation)\nMean = %4.2f, Median %4.2f",
             meanTotStepsI,medianTotStepsI)
main=sprintf("Histogram of total steps in a day (ignoring missing)\nMean = %4.2f, Median %4.2f",
             meanTotSteps,medianTotSteps)

# I'm doing this extra work to have the 2 histograms have the same y range.
h1 = hist(totStep$steps,breaks=20,plot=FALSE)
h2 = hist(totStepI$steps,breaks=20,plot=FALSE)
maxcounts = max(c(h1$counts,h2$counts))

par(mfrow=c(2,1))
hist(totStep$steps,main=main,xlab="Total Steps",breaks=20,xlim = c(0,25000),ylim=c(0,maxcounts))
hist(totStepI$steps,main=mainI,xlab="Total Steps",breaks=20,xlim = c(0,25000),ylim=c(0,maxcounts))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

The mean doesn't change which isn't suprising giving the regularity of the missingness pattern and that we filled in the missing values with a mean.  The height of the mode (the most frequent total of days) is higher and all other values of the histogram are reduced.

### Are there differences in activity patterns between weekdays and weekends?

The code below creates a new factor variable called "daytype" which specifies whether each day is a "weekday" or a "weekend".  Then we create a set of time series plots one for weekdays and one for weekends. 


```r
dataNew$daytype = factor(ifelse(weekdays(as.Date(dataNew$date)) %in% c("Saturday","Sunday"),
                         "weekend","weekday"))

# calculate mean number of steps per interval
meanStepDT = aggregate(steps~interval+daytype,data=dataNew,FUN=mean)
par(mfrow=c(2,1))
plot(steps~interval,main="Weekday Mean steps per interval",data=subset(meanStepDT,daytype=="weekday"),
     type="l",ylim=range(meanStepDT$steps))
plot(steps~interval,main="Weekend Mean steps per interval",data=subset(meanStepDT,daytype=="weekend"),
     type="l",ylim=range(meanStepDT$steps))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Basically, the activity is more dispersed on weekends.  That is, the time doesn't matter as much on weekends.  Note that slower rise in the morning, and that relatively low peak at the mode.   Then in general more activity occurs later in the day.
