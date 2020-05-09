
# downloading and reading the data

loc <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(loc, destfile = "repores")
unzip("repores")
df <- read.csv("activity.csv")
head(df)


# histogram of steps by day
library(dplyr)
library(ggplot2)
library(ggthemes)

steps.by.day <- df %>% 
        group_by(date) %>% 
        summarize(steps = sum(steps), na.rm = T)

hist <- ggplot(steps.by.day, aes(steps)) +
    geom_histogram(fill = "skyblue2") +
    ggtitle("Histogram", "Number of Steps Taken Per Day") +
    labs(x = "Steps per Day", y = "Frequency")
    # theme_economist()

# mean and mean of steps per days
df %>% 
    group_by(date) %>% 
    summarize(Total = sum(steps),
              Average = mean(steps),
              Median = median(steps))

# time series plot

time.series <- df %>% 
        group_by(date) %>% 
        summarize(Average = mean(steps),
                  Med = median(steps))

library(lubridate)
#avg
avg <- ggplot(time.series, aes(x=ymd(date), y=Average)) +
    geom_line(color = "skyblue2", size = 1, na.rm = T) +
    ggtitle("Time Series", "Average Number of Steps Taken Per Day") +
    labs(x = "Date", y = "Average Number of Steps")

#median
med <- ggplot(time.series, aes(x=ymd(date), y=Med)) +
    geom_line(color = "skyblue2", size = 1, na.rm = T) +
    ggtitle("Time Series", "Meidan Number of Steps Taken Per Day") +
    labs(x = "Date", y = "Median Number of Steps")

ggsave(arrangeGrob(avg, med))


# max steps by interval
intrvl <- df %>% 
        group_by(interval) %>% 
        summarize(Average = mean(steps, na.rm = T))

which.max(intrvl$Average)
intrvl[104,]

ggplot(intrvl, aes(x=interval, y=Average)) +
    geom_line(color = "skyblue2", size = 1, na.rm = T) +
    ggtitle("Interval Plot", "Average Number of Steps Taken by Time Interval") +
    labs(x = "Interval", y = "Average Number of Steps") +
    annotate("text", x = 835+15, y = 206, vjust = "inward", hjust = "inward",
             label = paste0("Max point: Interval = 835, Average steps = 206"))

# missing value strategy
#   "https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/"

library(mice)
md.pattern(df) # finding the number of missing values
imputations <- mice(df, m=5, maxit = 50, method = 'pmm', seed = 500)
df_imputed <- complete(imputations, 2) # picking one dataset of out of five
str(df_imputed)

# histogram of steps by day
library(dplyr)
library(ggplot2)
library(ggthemes)

steps.by.day2 <- df_imputed %>% 
    group_by(date) %>% 
    summarize(steps = sum(steps), na.rm = T)

hist2 <- ggplot(steps.by.day2, aes(steps)) +
    geom_histogram(fill = "skyblue2") +
    ggtitle("Histogram with No Missing Values", "Number of Steps Taken Per Day") +
    labs(x = "Steps per Day", y = "Frequency")

# Number of stesps by Weekdays vs. Weekends
library(lubridate)
steps.by.weekday <- df_imputed %>%
    mutate(day.of.week = weekdays(as.Date(date))) %>% 
    mutate(weekend = ifelse(day.of.week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>% 
    group_by(weekend, interval) %>% 
    summarize(Average = mean(steps))

ggplot(steps.by.weekday, aes(x=interval, y=Average)) +
    geom_line(color = "skyblue2", size = 1, na.rm = T) +
    ggtitle("Interval Plot by Weekdays vs. Weekend", 
            "Average Number of Steps Taken by Time Interval") +
    labs(x = "Interval", y = "Average Number of Steps") +
    facet_grid(.~weekend)

dayofweek <- df_imputed %>%
    mutate(dow = weekdays(as.Date(date))) %>% 
    group_by(dow) %>% 
    summarize(Average = mean(steps))

ggplot(dayofweek, aes(x = as.factor(dow), y = Average)) +
    geom_bar(
        fill = "skyblue2", stat = "identity",
        position="dodge", width = 0.5) +
    ggtitle("Steps by Weekdays", 
            "Average Number of Steps Taken by Weekdays") +
    labs(x = "Weekdays", y = "Average Number of Steps")


