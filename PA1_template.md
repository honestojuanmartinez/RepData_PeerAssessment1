---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
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

```r
datos<-read.csv("activity.csv", stringsAsFactors=FALSE)
by_day<-group_by(datos, date)
```


## What is mean total number of steps taken per day?

```r
sum_day<-summarise(by_day, suma=sum(steps, na.rm=TRUE))
h<-hist(sum_day$suma, 
     main="Daily sum of registered steps", 
     xlab="Steps by day", 
     col="darkmagenta",
     border="black", 
     breaks=c(0, 2500,  5000, 7500, 10000, 12500, 15000,17500, 20000, 22500, 25000))
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

```r
media<-mean(sum_day$suma, na.rm=TRUE)
fmedia<-format(media, nsmall = 2, big.mark = ".")
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' and 'decimal.mark' are both '.', which could be confusing
```

```r
mediana<-median(sum_day$suma, na.rm=TRUE)
fmediana<-format(mediana, nsmall = 2, big.mark = ".")
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' and 'decimal.mark' are both '.', which could be confusing
```

```r
print(paste("Mean of the total number of steps taken by day: ",fmedia))
```

```
## [1] "Mean of the total number of steps taken by day:  9.354.23"
```

```r
print(paste("Median of the total number of steps taken by day: ", fmediana))
```

```
## [1] "Median of the total number of steps taken by day:  10.395"
```

## What is the average daily activity pattern?

```r
by_five<-group_by(datos, interval)
avg_five<-summarise(by_five, media=mean(steps, na.rm=TRUE))
plot(avg_five$interval, avg_five$media, type="l",
     main="Average number of steps every five minutes interval across all the days",
     xlab="Time interval",
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/time series-1.png)<!-- -->

```r
maximo=max(avg_five$media)
fmaximo<-format(maximo, nsmall = 2, big.mark = ".")
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' and 'decimal.mark' are both '.', which could be confusing
```

```r
maximo_interval=avg_five$interval[avg_five$media == maximo]
print(paste("The interval which contains the maximum number of steps on average is: ",toString(maximo_interval)))
```

```
## [1] "The interval which contains the maximum number of steps on average is:  835"
```

```r
print(paste("The maximum number of steps on average is: ",fmaximo))
```

```
## [1] "The maximum number of steps on average is:  206.1698"
```


## Imputing missing values

```r
ausentes<-is.na(datos$steps)
print(paste("The total number of missing data is: ",toString(sum(ausentes))))
```

```
## [1] "The total number of missing data is:  2304"
```

```r
imputed_datos<-left_join(datos, avg_five, by="interval")
imputed_datos$steps[ausentes]<-imputed_datos$media[ausentes]
imputed_datos<-imputed_datos[,1:3]
imputed_by_day<-group_by(imputed_datos, date)
imputed_sum_day<-summarise(imputed_by_day, suma=sum(steps, na.rm=TRUE))
h<-hist(imputed_sum_day$suma, 
     main="Daily sum of registered steps. Imputed Data", 
     xlab="Steps by day", 
     col="darkmagenta",
     border="black", 
     breaks=c(0, 2500,  5000, 7500, 10000, 12500, 15000,17500, 20000, 22500, 25000))
```

![](PA1_template_files/figure-html/impute-1.png)<!-- -->

```r
imputed_media<-mean(imputed_sum_day$suma, na.rm=TRUE)
fimputed_media<-format(imputed_media, nsmall = 2, big.mark = ".")
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' and 'decimal.mark' are both '.', which could be confusing
```

```r
imputed_mediana<-median(imputed_sum_day$suma, na.rm=TRUE)
fimputed_mediana<-format(imputed_mediana, nsmall = 2, big.mark = ".")
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' and 'decimal.mark' are both '.', which could be confusing
```

```r
print(paste("Mean of the total number of steps taken by day having imputed missing data: ",fimputed_media))
```

```
## [1] "Mean of the total number of steps taken by day having imputed missing data:  10.766.19"
```

```r
print(paste("Median of the total number of steps taken by day having imputed missing data: ", fimputed_mediana))
```

```
## [1] "Median of the total number of steps taken by day having imputed missing data:  10.766.19"
```

```r
print(paste("The data imputation results in the following variation of the mean of steps: ", format(media-imputed_media, nsmall=2, big.mark=",")))
```

```
## [1] "The data imputation results in the following variation of the mean of steps:  -1,411.959"
```

```r
print(paste("The data imputation results in the following variation of the median of steps: ", format(mediana-imputed_mediana, nsmall=2, big.mark=",")))
```

```
## [1] "The data imputation results in the following variation of the median of steps:  -371.1887"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
datos$dia<-weekdays(as.Date(datos$date))
datos$weekend=0
datos$weekend[datos$dia == "s�bado" | datos$dia == "domingo"] <- 1
datos$fweekend<-factor(datos$weekend, labels = c("Weekday", "Weekend"))

by_weekday<-group_by(datos, fweekend, interval )
avg_five<-summarise(by_weekday, media=mean(steps, na.rm=TRUE))
avg_weekday<-avg_five[avg_five$fweekend=="Weekday",]
avg_weekend<-avg_five[avg_five$fweekend=="Weekend",]

par(mfrow=c(2,1))
plot(avg_five$interval[avg_five$fweekend=="Weekday"], avg_five$media[avg_five$fweekend=="Weekday"], type="l",
     xlab="Weekday",
     ylab="Average number of steps")
plot(avg_five$interval[avg_five$fweekend=="Weekend"], avg_five$media[avg_five$fweekend=="Weekend"], type="l",
     xlab="Weekend",
     ylab="Average number of steps")
title("Average number of steps every five minutes interval across all the days", outer = TRUE)


library(ggplot2)
```

![](PA1_template_files/figure-html/weekend-1.png)<!-- -->

```r
library(ggpubr)
```

```
## Loading required package: magrittr
```

```r
l1<-ggplot(data=avg_weekday, aes(x=interval, y=media), xlab="Weekday", ylab="Average Steps") +geom_line(color="blue")+ ggtitle("5-minutes average number of steps across weekdays and    weekends")
l2<-ggplot(data=avg_weekend, aes(x=interval, y=media, xlab="Weekend", ylab="Average Steps")) +geom_line(color="darkred")
ggarrange(l1, l2, ncol=1, nrow=2) 
```

![](PA1_template_files/figure-html/weekend-2.png)<!-- -->

