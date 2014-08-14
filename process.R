activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)

day.steps <- tapply(activity$steps, activity$date, sum)
hist(day.steps, breaks = 10, col = "red", xlab = "steps", main = "Steps/Day")

mean(day.steps, na.rm = T)
median(day.steps, na.rm = T)

avg.steps <- tapply(activity$steps, activity$interval, mean, na.rm = T)
intervals <- as.numeric(levels(factor(activity$interval)))
plot(intervals, avg.steps, type = "l")


intervals[avg.steps == max(avg.steps)]

Nas <- sum(is.na(activity))

for (i in 1:nrow(activity)) {
	if (is.na(activity[i, "steps"])) {
		index <- activity[i, "interval"]
		activity[i, "steps"] <- avg.steps[toString(index)]
	}
}

day.steps <- tapply(activity$steps, activity$date, sum)
hist(day.steps, breaks = 10, col = "green", xlab = "steps", 
     main = "Steps/Day NA's Filled")

mean(day.steps)
median(day.steps)

days <- weekdays(activity$date)
activity$day <- ifelse(days == "Saturday" | days == "Sunday", 
			    "weekend", "weekday")
activity$day <- as.factor(activity$day)

activity.byday <- split(activity, activity$day)
mean.steps.byday <- lapply(activity.byday, function(df){
	tapply(df$steps, df$interval, mean)
})

activity.clean <- data.frame(interval = rep(intervals, 2), 
			mean.steps.byday = as.vector(unlist(mean.steps.byday)), 
			day = factor(rep(c("weekday", "weekend"), each = 288)))

library(lattice)
xyplot(mean.steps.byday ~ interval | day, data = activity.clean, layout = c(1, 2),
       type = "l", ylab = "Number of steps")