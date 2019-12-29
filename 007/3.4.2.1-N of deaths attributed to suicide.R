getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM007")
# cd.prob = read.csv("3.4.1.1.csv")
# ncd.n = read.csv("3.4.1.2.csv")
suicide.n = read.csv("3.4.2.1.csv")
# suicide.per.100k = read.csv("3.4.2.2.csv")

str(suicide.n)

library(tidyverse)
library(data.table)
# levels of year
unique(suicide.n$TimePeriod)
# no missing GeoAreaCode
sum(count(suicide.n, GeoAreaCode)$n != 15)
# Time_Detail and TimePeriod are identical
sum(suicide.n$Time_Detail != suicide.n$TimePeriod)

# For some GeoAreas includes multiple countries (Nature == NA)
suicide.n$GeoType = case_when(
  is.na(suicide.n$Nature) == T ~ "multi",
  is.na(suicide.n$Nature) == F ~ "single"
)

# omit GeoAreas including mutiple countries
suicide.n.single = subset(suicide.n, GeoType == "single")

# time-series by sex
# n of deaths attributed to suicide has increased in some countries
ggplot(suicide.n.single) +
  geom_line(aes(TimePeriod, Value/100, colour = GeoAreaCode, group = GeoAreaCode)) +
  facet_wrap(~X.Sex., nrow = 1) +
  theme(legend.position = "none") +
  ylab("N of deaths attributed to suicide")

# function to calculate mean by year and sex
mean.by.year = function(sex, geo) {
  suicide.n.single %>%
    filter(X.Sex. == sex & GeoType == geo) %>%
    group_by(TimePeriod) %>%
    summarise(both = mean(Value))
}

# result
# mean increased from 2000 for male and decreased for female
# overall it also increased from 2000 while the 2005 marked the highest
bind_cols(mean.by.year("BOTHSEX", "single"),
          male = mean.by.year("MALE", "single")$both,
          female = mean.by.year("FEMALE", "single")$both)

# preparation to calculate differences between 2000 and 2016
suicide.n.yoy = select(suicide.n, GeoAreaName, GeoType, Nature, TimePeriod, X.Sex., Value)
str(suicide.n.yoy)

# long data to wide
temp1 = bind_cols(subset(suicide.n.yoy, TimePeriod == 2000))
temp2 = bind_cols(subset(suicide.n.yoy, TimePeriod == 2016))
suicide.n.yoy = merge(temp1, temp2, by = c("GeoType", "GeoAreaName", "Nature", "X.Sex."))
# calculate difference
suicide.n.yoy$drop.rate = suicide.n.yoy$Value.y/suicide.n.yoy$Value.x - 1
sum(is.nan(suicide.n.yoy$drop.rate))
# convert NaN to zero
suicide.n.yoy$drop.rate[is.nan(suicide.n.yoy$drop.rate)] = 0
sum(is.nan(suicide.n.yoy$drop.rate))

library(data.table)
# rename columns
setnames(suicide.n.yoy, old = c('Value.x','Value.y'), new = c("y2000", "y2016"))
# keep only "single"
suicide.n.yoy.single = subset(suicide.n.yoy, GeoType == "single")

# histogram by difference
# there are a few outliers in male
ggplot(suicide.n.yoy.single) +
  geom_histogram(aes(drop.rate), bins = 50) +
  facet_wrap(~X.Sex., nrow = 1)

N = 20
# top N (2000)
topN = function(data, sex, geo, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & TimePeriod == year) %>%
    arrange(Value) %>%
    slice(1:N)
}

# by country
bind_cols(both = topN(suicide.n, "BOTHSEX", "single", 2000)$GeoAreaName,
          female = topN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName,
          male = topN(suicide.n, "MALE", "single", 2000)$GeoAreaName)

# 8 countries exist both in female and male
intersect(topN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName,
          topN(suicide.n, "MALE", "single", 2000)$GeoAreaName)
# FEMALE only
setdiff(topN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName,
        topN(suicide.n, "MALE", "single", 2000)$GeoAreaName)
# MALE only
setdiff(topN(suicide.n, "MALE", "single", 2000)$GeoAreaName,
        topN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName)

# by nature
# top 2 are all from CA
bind_cols(both = topN(suicide.n, "BOTHSEX", "single", 2000)$Nature,
          female = topN(suicide.n, "FEMALE", "single", 2000)$Nature,
          male = topN(suicide.n, "MALE", "single", 2000)$Nature)

# top N (2016)
# by country
bind_cols(both = topN(suicide.n, "BOTHSEX", "single", 2016)$GeoAreaName,
          female = topN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName,
          male = topN(suicide.n, "MALE", "single", 2016)$GeoAreaName)

# 8 countries exist both in female and male
intersect(topN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName,
          topN(suicide.n, "MALE", "single", 2016)$GeoAreaName)
# FEMALE only
setdiff(topN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName,
        topN(suicide.n, "MALE", "single", 2016)$GeoAreaName)
# MALE only
setdiff(topN(suicide.n, "MALE", "single", 2016)$GeoAreaName,
        topN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName)

# by nature
# top 2 are all from CA
bind_cols(both = topN(suicide.n, "BOTHSEX", "single", 2016)$Nature,
          female = topN(suicide.n, "FEMALE", "single", 2016)$Nature,
          male = topN(suicide.n, "MALE", "single", 2016)$Nature)

# drop.rate top N (2000 - 2016)
topN.improve = function(data, sex) {
  data %>%
    filter(X.Sex. == sex) %>%
    arrange(drop.rate) %>%
    slice(1:N)
}

# by country
bind_cols(both = topN.improve(suicide.n.yoy.single, "BOTHSEX")$GeoAreaName,
          female = topN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName,
          male = topN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName)

# 4 countries exist both in female and male
# "Iran" "Lithuania" "Latvia" "Ukraine"
intersect(topN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName,
          topN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName)
# FEMALE only
setdiff(topN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName,
        topN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName)
# MALE only
setdiff(topN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName,
        topN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName)

# by nature
# no obs from "M" both in bothsex and male
bind_cols(both = topN.improve(suicide.n.yoy.single, "BOTHSEX")$Nature,
          female = topN.improve(suicide.n.yoy.single, "FEMALE")$Nature,
          male = topN.improve(suicide.n.yoy.single, "MALE")$Nature)

# worst N (2000)
worstN = function(data, sex, geo, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & TimePeriod == year) %>%
    arrange(desc(Value)) %>%
    slice(1:N)
}

# by country
bind_cols(both = worstN(suicide.n, "BOTHSEX", "single", 2000)$GeoAreaName,
          female = worstN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName,
          male = worstN(suicide.n, "MALE", "single", 2000)$GeoAreaName)

# 8 countries exist both in female and male
intersect(worstN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName,
          worstN(suicide.n, "MALE", "single", 2000)$GeoAreaName)
# FEMALE only
setdiff(worstN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName,
        worstN(suicide.n, "MALE", "single", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(suicide.n, "MALE", "single", 2000)$GeoAreaName,
        worstN(suicide.n, "FEMALE", "single", 2000)$GeoAreaName)

# by nature
# top 5 are all identical
bind_cols(both = worstN(suicide.n, "BOTHSEX", "single", 2000)$Nature,
          female = worstN(suicide.n, "FEMALE", "single", 2000)$Nature,
          male = worstN(suicide.n, "MALE", "single", 2000)$Nature)

# worst N (2016)
# by country
bind_cols(both = worstN(suicide.n, "BOTHSEX", "single", 2016)$GeoAreaName,
          female = worstN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName,
          male = worstN(suicide.n, "MALE", "single", 2016)$GeoAreaName)

# 8 countries exist both in female and male
intersect(worstN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName,
          worstN(suicide.n, "MALE", "single", 2016)$GeoAreaName)
# FEMALE only
setdiff(worstN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName,
        worstN(suicide.n, "MALE", "single", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(suicide.n, "MALE", "single", 2016)$GeoAreaName,
        worstN(suicide.n, "FEMALE", "single", 2016)$GeoAreaName)

# by nature
# top 5 are all identical
bind_cols(both = worstN(suicide.n, "BOTHSEX", "single", 2016)$Nature,
          female = worstN(suicide.n, "FEMALE", "single", 2016)$Nature,
          male = worstN(suicide.n, "MALE", "single", 2016)$Nature)

# drop.rate worst N (2000 - 2016)
worstN.improve = function(data, sex) {
  data %>%
    filter(X.Sex. == sex) %>%
    arrange(desc(drop.rate)) %>%
    slice(1:N)
}

# by country
bind_cols(both = worstN.improve(suicide.n.yoy.single, "BOTHSEX")$GeoAreaName,
          female = worstN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName,
          male = worstN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName)

# 3 countries exist both in female and male
# "Qatar" "Côte d'Ivoire" "Equatorial Guinea"
intersect(worstN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName,
          worstN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName)
# FEMALE only
setdiff(worstN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName,
        worstN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName)
# MALE only
setdiff(worstN.improve(suicide.n.yoy.single, "MALE")$GeoAreaName,
        worstN.improve(suicide.n.yoy.single, "FEMALE")$GeoAreaName)

# by nature
# obs are mostly from either "E" or "M"
bind_cols(both = worstN.improve(suicide.n.yoy.single, "BOTHSEX")$Nature,
          female = worstN.improve(suicide.n.yoy.single, "FEMALE")$Nature,
          male = worstN.improve(suicide.n.yoy.single, "MALE")$Nature)

# max(subset(suicide.n.yoy, X.Sex. == "BOTHSEX" & GeoType == "single")$drop.rate)
# subset(suicide.n.yoy, X.Sex. == "BOTHSEX" & GeoAreaName == "Qatar")

################### findings ###################
# n of deaths attributed to suicide has increased from 2000 for bothsex
# *increase for male and decreasee for female
# countries with less population naturally show the less number of deaths and vice verca
# hence improve rate is used to comprehend datasets
# it seems relatively difficult to observe improvement in Middle East and Africa

# the following would be required for further improvement:
# 1. overall improvement in MALE
# 2. overall improvement in Middle East and Africa