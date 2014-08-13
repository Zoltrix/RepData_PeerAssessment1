activity <- read.csv("activity.csv")

activity$date <- as.Date(activity$date)
day.steps <- tapply(activity$steps, activity$date, sum)
hist(day.steps, breaks = 10, xlab = "steps", main = "Steps/Day")

mean(day.steps, na.rm = T)
median(day.steps, na.rm = T)

avg.steps <- tapply(activity$steps, activity$interval, mean, na.rm = T)

str(avg.steps)

intervals <- as.numeric(levels(factor(activity$interval)))

plot(intervals, avg.steps, type = "l")

intervals[avg.steps == max(avg.steps)]
