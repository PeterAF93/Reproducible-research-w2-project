#download data
if(!(file.exists("activity.csv"))) { 
  archiveFile <- "repdata_data_activity.zip"
  if(!file.exists(archiveFile)) {
    archiveURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url=archiveURL,destfile=archiveFile)
  }  
  unzip(archiveFile) 
}

#read data
data1 <- read.csv("activity.csv")

#1- steps taken per day
steps_for_a_day <- aggregate(steps ~ date, data1, sum)

#2- histogram for steps taken per day
hist(steps_for_a_day$steps, main = paste("Total Steps per day"), col="purple",xlab="Number of Steps")

#3- mean and median number of steps taken each day
nmean <- mean(steps_for_a_day$steps)
nmean

nmedian <- median(steps_for_a_day$steps)
nmedian


#4-average activity daily pattern
steps_interval <- aggregate(steps ~ interval, data1, mean)

#time series plot
plot(steps_interval$interval, steps_interval$steps, type="l", xlab="Steps interval", ylab="Number of Steps",main="Avg Num of Steps by Interval")

#Which 5-minute interval, on average across all the days in the dataset,
#contains the maximum number of steps?
imax <- steps_interval[which.max(steps_interval$steps),1]
imax

#Imputing missing values

#total number of missing values in the dataset 
TotalNA <- sum(!complete.cases(data1))
TotalNA

#replace missing values
StepsAvg <- aggregate(steps ~ interval, data = data1, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data1)) {
  obs <- data1[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAvg, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

#Create a new dataset with no missing vals

data1_new <- data1
data1_new$steps <- fillNA
#there's an Error in `$<-.data.frame`(`*tmp*`, steps, value = c(1.71698113207547,  : 
#replacement has 35137 rows, data has 17568


#Make a histogram of the total number of steps taken
StepsTotalnumber <- aggregate(steps ~ date, data = data1_new, sum, na.rm = TRUE)
hist(StepsTotalnumber$steps, main = paste("Total Steps daily"), col="purple", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("Red", "purple"), lwd=10)

#calculate mean
totalmean <- mean(StepsTotalnumber$steps)
totalmean

#median
totalmedian <- median(StepsTotalnumber$steps)
totalmedian


#do values differ
#mean values

meandifference <- totalmean - nmean
meandifference

#mean no difference

#median values
mediandifference <- totalmedian - nmedian
mediandifference

#there's difference in median


#Are there differences in activity patterns between weekdays and weekends?
library(lattice)
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data1 = as.factor(ifelse(is.element(weekdays(as.Date(data1$date)),weekdays), "Weekday", "Weekend"))
StepsTotalnumber <- aggregate(steps ~ steps_interval, data1, mean)
xyplot(StepsTotalnumber$steps ~ StepsTotalnumber$interval|StepsTotalnumber$steps, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
