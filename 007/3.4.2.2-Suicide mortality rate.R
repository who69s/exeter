getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM007")
# cd.prob = read.csv("3.4.1.1.csv")
# ncd.n = read.csv("3.4.1.2.csv")
# suicide.n = read.csv("3.4.2.1.csv")
suicide.per.100k = read.csv("3.4.2.2.csv")

str(suicide.per.100k)

library(tidyverse)
library(data.table)
# levels of year
unique(suicide.per.100k$TimePeriod)
# no missing GeoAreaCode
sum(count(suicide.per.100k, GeoAreaCode)$n != 15)
# Time_Detail and TimePeriod are identical
sum(suicide.per.100k$Time_Detail != suicide.per.100k$TimePeriod)

# For some GeoAreas includes multiple countries (Nature == NA)
suicide.per.100k$GeoType = case_when(
  is.na(suicide.per.100k$Nature) == T ~ "multi",
  is.na(suicide.per.100k$Nature) == F ~ "single"
)

# omit GeoAreas including mutiple countries
suicide.per.100k.single = subset(suicide.per.100k, GeoType == "single")

# time-series by sex
# Suicide mortality rate has decreased in general over 16 years
ggplot(suicide.per.100k.single) +
  geom_line(aes(TimePeriod, Value, colour = GeoAreaCode, group = GeoAreaCode)) +
  facet_wrap(~X.Sex., nrow = 1) +
  theme(legend.position = "none") +
  ylab("Suicide mortality rate (per 100k)")

# function to calculate mean by year and sex
mean.by.year = function(sex, geo) {
  suicide.per.100k.single %>%
    filter(X.Sex. == sex & GeoType == geo) %>%
    group_by(TimePeriod) %>%
    summarise(both = mean(Value))
}

# result
# mean clearly decreased from 2000 for both sex
bind_cols(mean.by.year("BOTHSEX", "single"),
          male = mean.by.year("MALE", "single")$both,
          female = mean.by.year("FEMALE", "single")$both)

# preparation to calculate differences between 2000 and 2016
suicide.per.100k.yoy = select(suicide.per.100k, GeoAreaName, GeoType, Nature, TimePeriod, X.Sex., Value)
str(suicide.per.100k.yoy)

# long data to wide
temp1 = bind_cols(subset(suicide.per.100k.yoy, TimePeriod == 2000))
temp2 = bind_cols(subset(suicide.per.100k.yoy, TimePeriod == 2016))
suicide.per.100k.yoy = merge(temp1, temp2, by = c("GeoType", "GeoAreaName", "Nature", "X.Sex."))
# calculate difference
suicide.per.100k.yoy$drop.rate = suicide.per.100k.yoy$Value.y/suicide.per.100k.yoy$Value.x - 1
# confirm there is no NaN
sum(is.nan(suicide.per.100k.yoy$drop.rate))

library(data.table)
# rename columns
setnames(suicide.per.100k.yoy, old = c('Value.x','Value.y'), new = c("y2000", "y2016"))
# keep only "single"
suicide.per.100k.yoy.single = subset(suicide.per.100k.yoy, GeoType == "single")

# histogram by difference
# a few outliers aside it looks almost normal distribution
ggplot(suicide.per.100k.yoy.single) +
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

# by counrty
bind_cols(both = topN(suicide.per.100k, "BOTHSEX", "single", 2000)$GeoAreaName,
          female = topN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName,
          male = topN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName)

# 4 countries exist both in female and male
intersect(topN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName,
          topN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName)
# FEMALE only
setdiff(topN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName,
        topN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName)
# MALE only
setdiff(topN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName,
        topN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName)

# by nature
# less obs from "M"
bind_cols(both = topN(suicide.per.100k, "BOTHSEX", "single", 2000)$Nature,
          female = topN(suicide.per.100k, "FEMALE", "single", 2000)$Nature,
          male = topN(suicide.per.100k, "MALE", "single", 2000)$Nature)

# top N (2016)
# by contry
bind_cols(both = topN(suicide.per.100k, "BOTHSEX", "single", 2016)$GeoAreaName,
          female = topN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName,
          male = topN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName)

# 4 countries exist both in female and male
intersect(topN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName,
          topN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName)
# FEMALE only
setdiff(topN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName,
        topN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName)
# MALE only
setdiff(topN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName,
        topN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName)

# by nature
# less obs from "M"
bind_cols(both = topN(suicide.per.100k, "BOTHSEX", "single", 2016)$Nature,
          female = topN(suicide.per.100k, "FEMALE", "single", 2016)$Nature,
          male = topN(suicide.per.100k, "MALE", "single", 2016)$Nature)

# drop.rate top N (2000 - 2016)
topN.improve = function(data, sex) {
  data %>%
    filter(X.Sex. == sex) %>%
    arrange(drop.rate) %>%
    slice(1:N)
}

# by country
bind_cols(both = topN.improve(suicide.per.100k.yoy.single, "BOTHSEX")$GeoAreaName,
          female = topN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName,
          male = topN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName)

# 6 countries exist both in female and male
intersect(topN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName,
          topN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName)
# FEMALE only
setdiff(topN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName,
        topN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName)
# MALE only
setdiff(topN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName,
        topN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName)

# by nature
# M" "and "E" are dominant for female while top 3 are all "CA" in male
bind_cols(both = topN.improve(suicide.per.100k.yoy.single, "BOTHSEX")$Nature,
          female = topN.improve(suicide.per.100k.yoy.single, "FEMALE")$Nature,
          male = topN.improve(suicide.per.100k.yoy.single, "MALE")$Nature)

# worst N (2000)
worstN = function(data, sex, geo, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & TimePeriod == year) %>%
    arrange(desc(Value)) %>%
    slice(1:N)
}

# by country
bind_cols(both = worstN(suicide.per.100k, "BOTHSEX", "single", 2000)$GeoAreaName,
          female = worstN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName,
          male = worstN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName)

# 4 countries exist both in female and male
# "Lithuania" "Slovenia" "Russian Federation" "Hungary" 
intersect(worstN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName,
          worstN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName)
# FEMALE only
setdiff(worstN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName,
        worstN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(suicide.per.100k, "MALE", "single", 2000)$GeoAreaName,
        worstN(suicide.per.100k, "FEMALE", "single", 2000)$GeoAreaName)

# by nature
# no "M" for male
bind_cols(both = worstN(suicide.per.100k, "BOTHSEX", "single", 2000)$Nature,
          female = worstN(suicide.per.100k, "FEMALE", "single", 2000)$Nature,
          male = worstN(suicide.per.100k, "MALE", "single", 2000)$Nature)

# worst N (2016)
# by country
bind_cols(both = worstN(suicide.per.100k, "BOTHSEX", "single", 2016)$GeoAreaName,
          female = worstN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName,
          male = worstN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName)

# 3 countries exist both in female and male
# "Republic of Korea" "Guyana" "Suriname"  
intersect(worstN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName,
          worstN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName)
# FEMALE only
setdiff(worstN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName,
        worstN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(suicide.per.100k, "MALE", "single", 2016)$GeoAreaName,
        worstN(suicide.per.100k, "FEMALE", "single", 2016)$GeoAreaName)

# by nature
# no "M" for male
bind_cols(both = worstN(suicide.per.100k, "BOTHSEX", "single", 2016)$Nature,
          female = worstN(suicide.per.100k, "FEMALE", "single", 2016)$Nature,
          male = worstN(suicide.per.100k, "MALE", "single", 2016)$Nature)

# drop.rate worst 10 (2000 - 2016)
worstN.improve = function(data, sex) {
  data %>%
    filter(X.Sex. == sex) %>%
    arrange(desc(drop.rate)) %>%
    slice(1:N)
}

# by country
bind_cols(both = worstN.improve(suicide.per.100k.yoy.single, "BOTHSEX")$GeoAreaName,
          female = worstN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName,
          male = worstN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName)

# 5 countries exist both in female and male
intersect(worstN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName,
          worstN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName)
# FEMALE only
setdiff(worstN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName,
        worstN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName)
# MALE only
setdiff(worstN.improve(suicide.per.100k.yoy.single, "MALE")$GeoAreaName,
        worstN.improve(suicide.per.100k.yoy.single, "FEMALE")$GeoAreaName)


# by nature
# CA is relatively dominant
bind_cols(both = worstN.improve(suicide.per.100k.yoy.single, "BOTHSEX")$Nature,
          female = worstN.improve(suicide.per.100k.yoy.single, "FEMALE")$Nature,
          male = worstN.improve(suicide.per.100k.yoy.single, "MALE")$Nature)

# max(subset(suicide.per.100k.yoy, X.Sex. == "BOTHSEX" & GeoType == "single")$drop.rate)
# subset(suicide.per.100k.yoy, X.Sex. == "BOTHSEX" & GeoAreaName == "Cyprus")

################### findings ###################
# Suicide mortality rate has decreased from 2000 overall
# mortality rate seems relatively high in nature "CA" and "E" (for male in particular)
# the following would be required for further improvement:
# 1. overall improvement in MALE
# 2. overall improvement in "CA" and "E"
