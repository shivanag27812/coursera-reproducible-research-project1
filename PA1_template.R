## SETTING UP ENVIRONMENT

setwd("C:/Users/suhaada/Desktop/R/Coursera/Reproducible res")
library(lubridate)
dataset <- read.csv("./activity.csv")

## getting data


dataset$data <- as.Date(dataset$date, format="%Y-%m-%d")
dataset$weekday <- wday(dataset$date, label=T,abbr=T)
dataset$weekday <- as.factor(dataset$weekday)
dataset$interval <- as.factor(dataset$interval)
days <- as.factor(dataset$date)


total <- aggregate(steps ~ date, dataset, sum)
hist(total$steps, xlab= "Total steps per day", col="blue", breaks = 10)


mean(total$steps)


median(total$steps)


## Question 2:  average of daily activity pattern

listaIntervalli <- split(x=dataset,f=dataset$date)
listaIntervalli <- listaIntervalli[[1]]['interval']



library(lattice)
mediaStep <- tapply(dataset$steps, dataset$interval, mean, na.rm=T )


xyplot(mediaStep ~ listaIntervalli,
       type="l",
       ylab="mean steps",
       xlab="intervals",
       las=2,
       par.settings=list(layout.heights=list(top.padding=3, bottom.padding=5)))



names(which.max(x=mediaStep))


## Question 3: Imputing missing values

average <- tapply(dataset$steps, dataset$weekday, mean, na.rm=T )
naElem <- dataset[is.na(dataset$steps),]
length(naElem$steps)   

# Exchange null values to mean values


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


newdataset <- data.frame(steps={},
                         date={},
                         interval={})
for(i in 1:length(X))
{
  newdataset <- rbind(newdataset, data.frame(steps=X[[i]]['steps'],
                                             date=X[[i]]['date'],
                                             interval=X[[i]]['interval']))
  
}




tapply(newdataset$steps, dataset$weekday, mean, na.rm=T )




tapply(newdataset$steps, dataset$weekday, median, na.rm=T )



total_new <- aggregate(steps ~ date, newdataset, sum)
hist(total_new$steps, xlab= "Total steps per day", main="", col="blue", breaks = 10)



## Question 4:  differences in activity patterns on weekdays and weekends
newdataset['dayTypeInWeek'] = factor(sapply(newdataset$date, function(x){ if (wday(x) == 6 | wday(x) == 7) { "weekend" } else { "weekday"} }))


avgStepdayTypeInWeek = aggregate(steps~interval + dayTypeInWeek, mean, data=newdataset)

library(lattice)
xyplot( steps ~ interval | dayTypeInWeek, data = avgStepdayTypeInWeek, type="l", layout=c(1,2), xlab="Interval", ylab="Number of steps")
