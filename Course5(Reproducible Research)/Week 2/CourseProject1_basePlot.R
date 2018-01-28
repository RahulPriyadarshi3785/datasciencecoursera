setwd("C:/Users/hp/datasciencecoursera/Course5(Reproducible Research)/Week 2")


if (!file.exists("activity.zip"))
{
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
}

if(!file.exists("activity.csv")){
        files <- unzip("activity.zip")
}

library(Hmisc)
library(scales)
library(stats)

df <- read.csv('activity.csv', sep = ',', header = TRUE, na.strings = NA, stringsAsFactors = FALSE)

aggregatedTotalStepsByDay <- aggregate(steps~date, df, FUN=sum, na.rm=TRUE, na.action=na.pass)
aggregatedmeanStepsByDay <- aggregate(steps~date, df, FUN=mean, na.rm=TRUE, na.action=na.pass)
aggregatedmedianStepsByDay <- aggregate(steps~date, df, FUN=median, na.rm=TRUE, na.action=na.pass)

dev.off()
barplot(height=aggregatedTotalStepsByDay$steps, names.arg=aggregatedTotalStepsByDay$date, xlab="date", ylab=expression('steps'),main=expression('Total steps at various days'))

aggregatedTotalStepsByInterval <-  aggregate(steps~interval, df, FUN=mean, na.rm=TRUE, na.action=na.pass)

# base plot system
plot(aggregatedTotalStepsByInterval$interval, aggregatedTotalStepsByInterval$steps, type = "l", main = "average_daily_activity_pattern",  xlab = "Interval of Day", ylab = "Mean steps as per interval")

moststeps <- which.max(aggregatedTotalStepsByInterval$steps)
timeWhenmoststeps <- paste(as.character(as.integer(aggregatedTotalStepsByInterval[moststeps,'interval']/60)), as.character(aggregatedTotalStepsByInterval[moststeps,'interval'] %% 60), sep = ":")

numMissingValues <- length(which(is.na(df$steps)))

# To fill NAs we will take the mean of total dataset and put it to replace the NAs

dfImputed <- df
dfImputed$steps <- impute(df$steps, fun=mean)


aggregatedTotalStepsByDayImputed <- aggregate(steps~date, dfImputed, FUN=sum, na.rm=TRUE, na.action=na.pass)
aggregatedmeanStepsByDayImputed <- aggregate(steps~date, dfImputed, FUN=mean, na.rm=TRUE, na.action=na.pass)
aggregatedmedianStepsByDayImpute <- aggregate(steps~date, dfImputed, FUN=median, na.rm=TRUE, na.action=na.pass)


barplot(height=aggregatedTotalStepsByDayImputed$steps, names.arg=aggregatedTotalStepsByDayImputed$date, xlab="date", ylab=expression('steps'),main=expression('Total steps at various days'))


dfImputed$dayType <-  ifelse(as.POSIXlt(dfImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
aggregatedDataImputed <- aggregate(steps ~ interval + dayType, data=dfImputed, mean)

# base plot system
par(mfrow = c(2,1), mar =c(4,4,2,1), oma = c(0,0,0,0))
aggregatedTotalStepsByIntervalNWeekday <- subset(aggregatedDataImputed, dayType == "weekday")
aggregatedTotalStepsByIntervalNWeekend <- subset(aggregatedDataImputed, dayType == "weekend")
plot(aggregatedTotalStepsByIntervalNWeekday$interval, aggregatedTotalStepsByIntervalNWeekday$steps, type = "l", main = "average_daily_activity_pattern by weekdays and interval",  xlab = "Interval of Day", ylab = "Mean steps as per interval")
plot(aggregatedTotalStepsByIntervalNWeekend$interval, aggregatedTotalStepsByIntervalNWeekend$steps, type = "l", main = "average_daily_activity_pattern by weekends and interval",  xlab = "Interval of Day", ylab = "Mean steps as per interval")

