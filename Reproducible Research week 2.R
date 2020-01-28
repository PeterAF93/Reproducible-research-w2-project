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
#after downloading data we should read it and complete the NAs
data1 <- read.csv("activity.csv")
data2 <- data1[complete.cases(data1), ]

#1- steps taken per day
steps_for_a_day <- aggregate(steps ~ date, data2, sum)

#2- histogram for steps taken per day
hist(steps_for_a_day$steps, main = paste("Total Steps per day"),xlab="Number of Steps")

#3- mean and median number of steps taken each day
nmean <- mean(steps_for_a_day$steps)
nmean

nmedian <- median(steps_for_a_day$steps)
nmedian


#4-average activity daily pattern
steps_interval <- aggregate(steps ~ interval, data2, mean)

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
for (i in 1:nrow(data1)) {
  if (is.na(data1$steps[i])) {
  value <- steps_interval$steps[which(steps_interval$steps == data1$steps[i])]
  }
}

#Create a new dataset with no missing vals

data1_new <- data1
data1_new$steps <- steps_interval$steps

#Make a histogram of the total number of steps taken
StepsTotalnumber <- aggregate(steps ~ date, data = data1_new, sum, na.rm = TRUE)
hist(StepsTotalnumber$steps, main = paste("Total Steps daily"), col="purple", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("Red", "purple"), lwd=10)

#calculate mean
totalmean <- mean(StepsTotalnumber$steps)
round(totalmean)

#median
totalmedian <- median(StepsTotalnumber$steps)
round(totalmedian)


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
data1_new$dw = as.factor(ifelse(is.element(weekdays(as.Date(data1_new$date)),weekdays), "Weekday", "Weekend"))
StepsTotalnumber1 <- aggregate(steps ~ StepsTotalnumber + dw, data1_new, mean)
xyplot(StepsTotalnumber$steps ~ StepsTotalnumber$interval|StepsTotalnumber$steps, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
