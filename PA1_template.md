Reproducible Research: Assignment 1
===================================

Adam Suhajda, 20160902
======================

Introducing task and getting data
---------------------------------

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs) Devise a strategy for filling
in all of the missing values in the dataset. The strategy does not need
to be sophisticated. For example, you could use the mean/median for that
day, or the mean for that 5-minute interval, etc. Create a new dataset
that is equal to the original dataset but with the missing data filled
in. Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

    library(lubridate)

    ## Warning: package 'lubridate' was built under R version 3.3.1

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    setwd("C:/Users/suhaada/Desktop/R/Coursera/Reproducible res")
    dataset <- read.csv("./activity.csv")
    dataset$data <- as.Date(dataset$date, format="%Y-%m-%d")
    dataset$weekday <- wday(dataset$date, label=T,abbr=T)
    dataset$weekday <- as.factor(dataset$weekday)
    dataset$interval <- as.factor(dataset$interval)
    days <- as.factor(dataset$date)

Question 1:total number of steps taken per day
----------------------------------------------

    total <- aggregate(steps ~ date, dataset, sum)
    hist(total$steps, xlab= "Total steps per day", col="red", breaks = 10)

![alt tag](https://github.com/Suhaada/Reproducible-Research-Assignment-1/blob/master/1.PNG)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

mean of steps taken per day

    mean(total$steps)

    ## [1] 10766.19

median of steps taken per day

    median(total$steps)

    ## [1] 10765

Question 2: average of daily activity pattern
---------------------------------------------

    listaIntervalli <- split(x=dataset,f=dataset$date)
    listaIntervalli <- listaIntervalli[[1]]['interval']



    library(lattice)

    ## Warning: package 'lattice' was built under R version 3.3.1

    mediaStep <- tapply(dataset$steps, dataset$interval, mean, na.rm=T )


    xyplot(mediaStep ~ listaIntervalli,
           type="l",
           ylab="mean steps",
           xlab="intervals",
           las=2,
           par.settings=list(layout.heights=list(top.padding=3, bottom.padding=5)))

![alt tag](https://github.com/Suhaada/Reproducible-Research-Assignment-1/blob/master/2.PNG)
![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Which 5-minute interval contains the maximum number of steps?

    names(which.max(x=mediaStep))

    ## [1] "835"

Question 3: Imputing missing values
-----------------------------------

number of missing values

    average <- tapply(dataset$steps, dataset$weekday, mean, na.rm=T )
    naElem <- dataset[is.na(dataset$steps),]
    length(naElem$steps)   

    ## [1] 2304

Exchange null values to mean values

    days <- as.factor(dataset$date)
    X <- split(dataset, days)

    for(i in 1:length(X))
    {
      for(j in 1:length(X[[i]][,'steps']))
      {
        
        if(is.na(X[[i]][j,'steps']))
        {
          
          giorno <- X[[i]][j,'date']
          
          X[[i]][j,'steps'] <- average[wday(giorno)]
        }  
      }
      
    }

    # refreshing dataset environmental variable
    newdataset <- data.frame(steps={},
                             date={},
                             interval={})
    for(i in 1:length(X))
    {
      newdataset <- rbind(newdataset, data.frame(steps=X[[i]]['steps'],
                                                 date=X[[i]]['date'],
                                                 interval=X[[i]]['interval']))
      
    }

mean value rerun

    tapply(newdataset$steps, dataset$weekday, mean, na.rm=T )

    ##      Sun      Mon     Tues      Wed    Thurs      Fri      Sat 
    ## 42.63095 34.63492 31.07485 40.94010 28.51649 42.91567 43.52579

median value rerun

    tapply(newdataset$steps, dataset$weekday, median, na.rm=T )

    ##   Sun   Mon  Tues   Wed Thurs   Fri   Sat 
    ##     0     0     0     0     0     0     0

histogram

    total_new <- aggregate(steps ~ date, newdataset, sum)
    hist(total_new$steps, xlab= "Total steps per day", main="", col="blue", breaks = 10)
    
![alt tag](https://github.com/Suhaada/Reproducible-Research-Assignment-1/blob/master/3.PNG)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

Question 4: differences in activity patterns on weekdays and weekends
---------------------------------------------------------------------

    # refreshing dataset environmental variable
    newdataset['dayTypeInWeek'] = factor(sapply(newdataset$date, function(x){ if (wday(x) == 6 | wday(x) == 7) { "weekend" } else { "weekday"} }))


    avgStepdayTypeInWeek = aggregate(steps~interval + dayTypeInWeek, mean, data=newdataset)

    library(lattice)
    xyplot( steps ~ interval | dayTypeInWeek, data = avgStepdayTypeInWeek, type="l", layout=c(1,2), xlab="Interval", ylab="Number of steps")
 ![alt tag](https://github.com/Suhaada/Reproducible-Research-Assignment-1/blob/master/4.PNG)
![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
