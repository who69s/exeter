getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM503/assignment/4")
load("SubstationRPD.RData")
summary(substation.data)
str(substation.data)
# there are 50 unique date values
length(unique(substation.data$Date))
# 314 substations have records everyday (50 records)
subset(count(substation.data, Substation), n == 50)

############# 1 #############
library(dplyr)
# summarise 1st interval data
summ.1 = substation.data %>%
  group_by(Substation) %>%
  summarise(mean = mean(`1`), n = n())

# visualise
summ.1$type = case_when(
    summ.1$n == 50 ~ "All data",
    summ.1$n != 50 ~ "Limited data"
  )

library(ggplot2)
ggplot(summ.1) +
  geom_histogram(aes(mean, fill = type), bins = 100, alpha = 0.5) +
  xlab("Power delivered for the 410 substations")

mean(summ.1$mean)
median(summ.1$mean)

############# 2 #############
# create base dataset
sub.mean = c()
sub.mean.all = c()
for (i in levels(factor(substation.data$Substation))) {
  temp = subset(substation.data, Substation == i)
    for (k in 3:146) {
      sub.mean = c(sub.mean, mean(temp[,k]))
      }
  sub.mean.all = rbind(sub.mean.all, c("All days", i, sub.mean))
  sub.mean = c()
}

sub.mean.all = data.frame(sub.mean.all)

# for column names
colnames(sub.mean.all) = c("Days", "Substation",
                           format(seq.POSIXt(as.POSIXct(Sys.Date()),
                                             as.POSIXct(Sys.Date()+1),
                                             by = "10 min"),
                                  "%H:%M", tz="GMT")[-145]
                           )

# visualise
library(tidyverse)
sub.mean.all.vis = sub.mean.all %>%
  pivot_longer(-c(Substation, Days), names_to = "interval", values_to = "mean") %>%
  mutate(mean = as.numeric(as.character(mean))) %>%
  # mutate(interval = times(interval)) %>%
  mutate(interval = as.POSIXct(interval, format = "%H:%M")) %>%
  ggplot() +
  geom_line(aes(x = interval, y = mean,
                colour = Substation, group = Substation)) +
  xlab("Time") +
  ylab("Average Daily Demand") +
  theme(legend.position = "none") +
  scale_x_datetime(date_labels = "%H:%M")

sub.mean.all.vis

# for overall mean
temp.all = c()
for (k in 3:146) {
  temp.all = c(temp.all, mean(substation.data[,k]))
}

mean.all = data.frame(
  cbind(format(seq.POSIXt(as.POSIXct(Sys.Date()),
                          as.POSIXct(Sys.Date()+1),
                          by = "10 min"),
               "%H:%M", tz = "GMT")[-145],
        (temp.all)))

str(mean.all)

# change data type
mean.all$X1 = as.POSIXct(mean.all$X1, format = "%H:%M")
mean.all$X2 = as.numeric(as.character(mean.all$X2))

# overlay a thick black line
sub.mean.all.vis +
  geom_line(data = data.frame(mean.all),
            aes(x = X1, y = X2), size = 1.5, alpha = 0.6)

# substations with top 4 demands
top4 = subset(sub.mean.all, as.numeric(as.character(sub.mean.all$`12:00`)) > 350)
top4$Substation

# substations with high demands at night
midnight.high = subset(sub.mean.all, as.numeric(as.character(sub.mean.all$`00:00`)) > as.numeric(as.character(sub.mean.all$`12:00`)))

############# 3 #############
library(chron)
substation.data$Days = weekdays(substation.data$Date)

# create notin function
"%notin%" = Negate("%in%")
substation.data$Days.agg = case_when(
  substation.data$Days %notin% c("Saturday", "Sunday") ~ "Weekdays",
  substation.data$Days == "Saturday" ~ "Saturday",
  substation.data$Days == "Sunday" ~ "Sunday"
)

# create base dataset
sub.mean = c()
sub.mean.split = c()
for (i in levels(factor(substation.data$Days.agg))) {
  temp1 = subset(substation.data, Days.agg == i)
  for (j in levels(factor(substation.data$Substation))) {
    temp2 = subset(temp1, Substation == j)
    for (k in 3:146) {
      sub.mean = c(sub.mean, mean(temp2[,k]))
    }
    sub.mean.split = rbind(sub.mean.split, c(i, j, sub.mean))
    sub.mean = c()
  }
}

sub.mean.split = data.frame(sub.mean.split)

# change column names
colnames(sub.mean.split) = c("Days", "Substation",
                             format(seq.POSIXt(as.POSIXct(Sys.Date()),
                                               as.POSIXct(Sys.Date()+1),
                                               by = "10 min"),
                                    "%H:%M", tz="GMT")[-145]
)

# merge with the dataset used in question 2
sub.mean.split = bind_rows(sub.mean.all, sub.mean.split)

# visualisation
sub.mean.split.vis = sub.mean.split %>%
  pivot_longer(-c(Substation, Days), names_to = "interval", values_to = "mean") %>%
  mutate(mean = as.numeric(as.character(mean))) %>%
  mutate(interval = as.POSIXct(interval, format = "%H:%M")) %>%
  ggplot() +
  geom_line(aes(x = interval, y = mean,
                colour = Substation, group = Substation)) +
  facet_grid(.~Days) + # ?facet_grid
  xlab("Time") +
  ylab("Average Daily Demand") +
  theme(legend.position = "none") +
  scale_x_datetime(date_labels = "%H:%M")

sub.mean.split.vis

# overall mean by weekdays, Saturday, Sunday
days.mean = c()
days.mean.all = c()
for (i in levels(factor(substation.data$Days.agg))) {
  temp = subset(substation.data, Days.agg == i)
  for (k in 3:146) {
    days.mean = c(days.mean, mean(temp[,k]))
  }
  days.mean.all = rbind(days.mean.all, c(i, days.mean))
  days.mean = c()
}

days.mean.all = data.frame(rbind(c("All days", temp.all), days.mean.all))

# change column names
colnames(days.mean.all) = c("Days",
                            format(seq.POSIXt(as.POSIXct(Sys.Date()),
                                              as.POSIXct(Sys.Date()+1),
                                              by = "10 min"),
                                   "%H:%M", tz="GMT")[-145])

# visualisation
days.mean.split.vis = days.mean.all %>%
  pivot_longer(-Days, names_to = "interval", values_to = "mean") %>%
  mutate(mean = as.numeric(as.character(mean))) %>%
  mutate(interval = as.POSIXct(interval, format = "%H:%M"))

# final visualisation
sub.mean.split.vis +
  geom_line(data = days.mean.split.vis,
            aes(x = interval, y = mean), size = 1, alpha = 0.6)

############# 4 #############
mean.by.date = substation.data[, -c(1, 147, 148)] %>%
  group_by(Date) %>%
  summarise_all("mean")

str(mean.by.date)

library(xtable)
xtable(head(mean.by.date)[,1:6])

############# 5 #############
# create base dataset
mean.by.date$day = day(mean.by.date$Date)
mean.by.date$month = month(mean.by.date$Date)
mean.by.date$weekdays = weekdays(mean.by.date$Date)

# pivot
mean.by.date.pivot = mean.by.date %>%
  pivot_longer(-c(Date, weekdays, day, month), names_to = "minute.int", values_to = "mean") %>%
  mutate(minute.int = as.numeric(minute.int))

# change the order of columns
if (colnames(mean.by.date.pivot)[2] == "day") {
  mean.by.date.pivot = bind_cols(mean.by.date.pivot[,c(1,4,5,6)],
                                 mean.by.date.pivot[,c(2,3)])
}

# create interval component
hms = c(0, cumsum(rep(1/144, 144))[-144])
length(hms)

# create "time" column using Date and hms
mean.by.date.pivot$time = as.integer(as.Date(mean.by.date.pivot$Date, format = "%Y-%m-%d")) + hms
head(mean.by.date.pivot)

mean.by.date.pivot$wk_num = case_when(
  mean.by.date.pivot$weekdays == "Monday" ~ 1,
  mean.by.date.pivot$weekdays == "Tuesday" ~ 2,
  mean.by.date.pivot$weekdays == "Wednesday" ~ 3,
  mean.by.date.pivot$weekdays == "Thursday" ~ 4,
  mean.by.date.pivot$weekdays == "Friday" ~ 5,
  mean.by.date.pivot$weekdays == "Saturday" ~ 6,
  mean.by.date.pivot$weekdays == "Sunday" ~ 7
)

# model 1
gam.1 <- gam(mean ~ s(time),
             data = mean.by.date.pivot, method = "REML")
summary(gam.1)
plot(gam.1) #, residuals = T)
par(mfrow = c(2,2))
gam.check(gam.1)

# model 2
gam.2 <- gam(mean ~ s(minute.int, bs = 'cc', k = 144) + s(time),
             data = mean.by.date.pivot, method = "REML")
summary(gam.2)

par(mfrow = c(1,2))
plot(gam.2)
# 3D
vis.gam(gam.2, color = "cm", theta = 45) #, se = T)
# contour
vis.gam(gam.2, color = "cm", plot.type = "contour")

par(mfrow = c(2,2))
gam.check(gam.2)

anova(gam.1, gam.2, test = "F")

gam.3 <- gam(mean ~ s(wk_num, bs = 'cc', k = 7) + s(minute.int, bs = 'cc', k = 144) + s(time),
             data = mean.by.date.pivot, method = "REML")
summary(gam.3)

par(mfrow = c(1,2))
plot(gam.3)
# 3D
vis.gam(gam.3, color = "cm", theta = 45) #, se = T)
# contour
vis.gam(gam.3, color = "cm", plot.type = "contour")

par(mfrow = c(2,2))
gam.check(gam.3)

# model selection
# install.packages("MuMIn")
library(MuMIn)
options(na.action = "na.fail")
summary(gam.3)
dredge(gam.3, rank = "AIC")

############# 6 #############
# rm(new.data)
new.data <- data.frame(matrix(c(rep(1:144, each = 8), rep(21:28,144), rep(7,1152)),
                              nrow=1152, ncol=3, byrow=FALSE))

new.data$Date <- rep(seq(as.Date("2012-07-21"),
                         as.Date("2012-07-28"), "days"), 144)

new.data = new.data[order(new.data$Date),]
names(new.data) <- c("minute.int","day","month","Date")

new.data$time = as.integer(as.Date(new.data$Date, format = "%Y-%m-%d")) + hms
new.data$weekdays = weekdays(new.data$Date)

new.data$wk_num = case_when(
  new.data$weekdays == "Monday" ~ 1,
  new.data$weekdays == "Tuesday" ~ 2,
  new.data$weekdays == "Wednesday" ~ 3,
  new.data$weekdays == "Thursday" ~ 4,
  new.data$weekdays == "Friday" ~ 5,
  new.data$weekdays == "Saturday" ~ 6,
  new.data$weekdays == "Sunday" ~ 7
)

# Make predictions
new.data$predictions <- gam.3 %>% predict(new.data)
new.data %>%
  group_by(Date) %>%
  summarise(mean = mean(predictions)) %>%
  ggplot(aes(Date, mean)) +
  geom_line() +
  geom_point() +
  ylab("Power demand (daily average)")

# Reference
mean.by.date.pivot %>%
  group_by(Date) %>%
  summarise(mean = mean(mean)) %>%
  ggplot(aes(Date, mean)) +
  geom_line() +
  geom_point() +
  ylab("Power demand (daily average) - train")
