#READ THE activityData
unzip(zipfile="activity.zip")
activityData <- read.csv("activity.csv")

#Make the histogram
require(ggplot2)
dailySteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
qplot(dailySteps, binwidth=1000, xlab="Number of steps", ylab="Times",fill=I("blue"), 
      col=I("red"))
mean(dailySteps, na.rm=TRUE)
median(dailySteps, na.rm=TRUE)

#Compute the means for 5-min intervals and plot the max
require(ggplot2)
meanSteps <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=meanSteps,aes(x=interval, y=steps))+geom_line(colour="#CC0000")+xlab("5min interval")+ylab("Mean steps")
meanSteps[which.max(meanSteps$steps),]

#Create imputed data histogram and means
missings<- is.na(activityData$steps)
summary(missings)
imputedActivityData <- activityData
imputedActivityData$steps[which(is.na(imputedActivityData$steps))] <- mean(activityData$steps, na.rm=TRUE)
dailySteps2 <- tapply(imputedActivityData$steps, imputedActivityData$date, FUN=sum)
qplot(dailySteps2, binwidth=1000, xlab="total number of steps taken each day")
mean(dailySteps2)
median(dailySteps2)


#Function to select weekday or weekend (days are in my own european language because of my System lang.)
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("dilluns", "dimarts", "dimecres", "dijous", "divendres"))
        return("weekday")
    else if (day %in% c("dissabte", "diumenge"))
        return("weekend")
    else{stop("BadDate")}

}
imputedActivityData$day <- sapply(as.Date(imputedActivityData$date), FUN=weekday.or.weekend)
table(imputedActivityData$day)

meanSteps3 <- aggregate(steps ~ interval + day, data=imputedActivityData, mean)
ggplot(meanSteps3, aes(interval, steps)) + geom_line(colour="#CC0000") + facet_grid(day ~ .) +
    xlab("5min interval") + ylab("Mean steps")
