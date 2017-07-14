rm(list=ls())
library("lubridate")
library("lattice")

df <- read.csv("activity.csv",header=T,na.strings = "NA")
df1 <- df[!is.na(df$steps),]

total <- aggregate(steps ~ date, df1, sum)
hist(total$steps, breaks=20, main="Total Steps Each Day", col="red", xlab="Number of Steps")

step_mean <- round(mean(total$steps),2)
step_median <- median(total$steps)

avg_steps <- aggregate(steps ~ interval, df1, mean)
avg_steps$day <- seq.int(nrow(avg_steps))
plot(avg_steps$interval, avg_steps$steps, type="l", main="Average Steps per Day", col="green", 
     xlab="Interval", ylab="Steps")

avg_int <- aggregate(steps ~ interval, df1, mean)
max_int <- avg_int[avg_int$steps==max(avg_int$steps),"interval"]

df_new <- df
sum(is.na(df_new))
avg  <- tapply(df_new$steps, df_new$interval, mean, na.rm=T, simplify=T)
df_new$steps[is.na(df_new$steps)] <- avg[as.character(df_new$interval[is.na(df_new$steps)])]

total_new <- aggregate(steps ~ date, df_new, sum)
hist(total_new$steps, breaks=20, main="Total Steps Each Day", col="blue", xlab="Number of Steps")
new_mean <- round(mean(total_new$steps),2)
new_median <- median(total_new$steps)

weekend <- c("Saturday", "Sunday")
df_new$day = as.factor(ifelse(is.element(weekdays(as.Date(df_new$date)),weekend), "Weekend", "Weekdays"))
total_day <- aggregate(steps ~ interval + day, df_new, mean)
xyplot(total_day$steps ~ total_day$interval|total_day$day, col="green", main="Average Steps per Day by Interval",
       xlab="Interval", ylab="Steps",layout=c(2,1), type="l")
