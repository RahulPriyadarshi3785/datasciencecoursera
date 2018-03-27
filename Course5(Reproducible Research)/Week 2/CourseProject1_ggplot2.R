setwd("C:/Users/hp/datasciencecoursera/Course5(Reproducible Research)/Week 2")

if (!file.exists("activity.zip"))
{
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
}

if(!file.exists("activity.csv")){
        files <- unzip("activity.zip")
}

df <- read.csv('activity.csv', sep = ',', header = TRUE, na.strings = NA, stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(stats)
library(Hmisc)
library(scales)

df1 <- tbl_df(df)

totalstepsbydate <- df1 %>% select(date, steps) %>% group_by(date) %>% summarise_if(is.numeric,sum,na.rm = TRUE)%>%ungroup()
meanstepsbydate <- df1 %>% select(date, steps) %>% group_by(date) %>% summarise_if(is.numeric,mean,na.rm = TRUE)%>%ungroup()
medianstepsbydate <- df1 %>% select(date, steps) %>% group_by(date) %>% summarise_if(is.numeric,median,na.rm = TRUE)%>%ungroup()

## ggplot2
g <- ggplot(totalstepsbydate, aes(x = date))
g + geom_bar(aes(weight = steps), stat = "count") + ylab("steps") + xlab("date") + ggtitle('Total steps at various days') 


average_daily_activity_pattern <- df1 %>% select(steps, interval) %>% group_by(interval) %>% summarise_if(is.numeric,mean,na.rm = TRUE)%>%ungroup()

## ggplot2
g <- ggplot(average_daily_activity_pattern, aes(x = interval, y = steps))
g + geom_line() + xlab("Interval of Day") + ylab("Mean steps as per interval") + ggtitle("average_daily_activity_pattern")


moststeps <- which.max(average_daily_activity_pattern$steps)
timeWhenmoststeps <- paste(as.character(as.integer(average_daily_activity_pattern[moststeps,'interval']/60)), as.character(average_daily_activity_pattern[moststeps,'interval'] %% 60), sep = ":")

numMissingValues <- length(which(is.na(df1$steps)))

# To fill NAs we will take the mean of total dataset and put it to replace the NAs

df1Imputed <- df1
df1Imputed$steps <- impute(df1$steps, fun=mean)

totalstepsbydateImputed <- df1Imputed %>% select(date, steps) %>% group_by(date) %>% summarise_if(is.numeric,sum,na.rm = TRUE)%>%ungroup()
meanstepsbydateImputed <- df1Imputed %>% select(date, steps) %>% group_by(date) %>% summarise_if(is.numeric,mean,na.rm = TRUE)%>%ungroup()
medianstepsbydateImputed <- df1Imputed %>% select(date, steps) %>% group_by(date) %>% summarise_if(is.numeric,median,na.rm = TRUE)%>%ungroup()

## ggplot2
g <- ggplot(totalstepsbydateImputed, aes(x = date))
g + geom_bar(aes(weight = steps), stat = "count") + ylab("steps") + xlab("date") + ggtitle('Total steps at various days') 


df1Imputed$dayType <-  ifelse(as.POSIXlt(df1Imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
aggregatedDataImputed <- df1Imputed %>% group_by(dayType,interval) %>% summarise_if(is.numeric,mean,na.rm = TRUE)%>%ungroup()

ggplot(aggregatedDataImputed, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(dayType ~ .) +
        xlab("Interval of Day") + 
        ylab("Mean steps as per interval") +
        ggtitle("average_daily_activity_pattern by weekdays and interval")+ 
        theme(panel.background = element_rect(fill = "white"))

# using lattice plot
xyplot(steps ~ interval | dayType,
       data = aggregatedDataImputed,
       layout = c(1, 2),
       type = "l")
