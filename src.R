
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")

StepsTotal <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(StepsTotal$steps, xlab = "Total number of steps", col = "red", main = "Histogram of total steps by day")

getAvgPer5MinInterval <- function(interval.id) {
  num.steps.by.day <- with(activity, steps[interval == interval.id])
  data.frame("interval" = interval.id, "avg.num.steps" = mean(num.steps.by.day, na.rm = T))
}
ids.of.interval <- with(activity, interval[date == as.Date("2012-10-01")])
require(plyr)
res <- ldply(ids.of.interval, getAvgPer5MinInterval)
## the whole above process can be replaced by just one call to aggregate() as below
res <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(y = res$avg.num.steps, x = res$interval, type = "l", xlab = "interval", ylab = "Avg number of steps taken")

max_interval <- with(res, interval[which.max(avg.num.steps)])

impute <- function(row) {
  
  if (is.na(activity$steps[row])) {
    id <- activity$interval[row]
    with(res, avg.num.steps[interval == id])
  } else {
    activity$steps[row]
  }
}

imputed.steps <- laply(1: nrow(activity), impute)
new.activity <- data.frame("steps" = imputed.steps, "date" = activity$date, "interval" = activity$interval)
total.steps.2 <- aggregate(steps ~ date, data = new.activity, FUN = sum)

hist(total.steps.2$steps, xlab = "Total number of steps", col = "green", main = "Histogram of total steps by day")
mean(total.steps.2$steps)
median(total.steps.2$steps)

activity <- transform(activity, "weekdays" = weekdays(activity$date, abbreviate = T))
activity <- transform(activity, "is.weekend" = ifelse(activity$weekdays %in% c("Sat", "Sun"), "weekend", "weekday"))

weekday.act <- subset(activity, is.weekend == "weekday")
weekday.ds <- aggregate(steps ~ interval, data = weekday.act, FUN = mean)
with(weekday.ds, plot(y = steps, x = interval, type = "l", 
                      xlab = "interval", ylab = "Avg number of steps taken", col = "orange"))

weekend.act <- subset(activity, is.weekend == "weekend")
weekend.ds <- aggregate(steps ~ interval, data = weekend.act, FUN = mean)
with(weekend.ds, plot(y = steps, x = interval, type = "l", 
                      xlab = "interval", ylab = "Avg number of steps taken", col = "pink"))


