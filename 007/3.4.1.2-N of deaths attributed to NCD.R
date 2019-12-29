getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM007")
# cd.prob = read.csv("3.4.1.1.csv")
ncd.n = read.csv("3.4.1.2.csv")
# suicide.n = read.csv("3.4.2.1.csv")
# suicide.per.100k = read.csv("3.4.2.2.csv")

str(ncd.n)

library(tidyverse)
library(data.table)
# levels of year
unique(ncd.n$TimePeriod)
# no missing GeoAreaCode
sum(count(ncd.n, GeoAreaCode)$n != 60)
# Time_Detail and TimePeriod are identical
sum(ncd.n$Time_Detail != ncd.n$TimePeriod)
# rename a column
setnames(ncd.n, old = "X.Name.of.non.communicable.disease.", new = "ncd.name")
# levels of ncd name
unique(ncd.n$ncd.name)

# For some GeoAreas includes multiple countries (Nature == NA)
ncd.n$GeoType = case_when(
  is.na(ncd.n$Nature) == T ~ "multi",
  is.na(ncd.n$Nature) == F ~ "single"
)

# function for visualisation
# time-series by sex and ncd name
viz = function(data, sex, geo) {
  ggplot(subset(data, X.Sex. == sex & GeoType == geo)) +
    geom_line(aes(TimePeriod, Value, colour = GeoAreaCode, group = GeoAreaCode)) +
    facet_wrap(~ncd.name, nrow = 1) +
    theme(legend.position = "none") +
    ylab("N of deaths (non-communicable diseases)")
}

# result
# CAR clearly caused more deaths than others
viz(ncd.n, "BOTHSEX", "single") + ggtitle("Bothsex")
viz(ncd.n, "MALE", "single") + ggtitle("Male")
viz(ncd.n, "FEMALE", "single") + ggtitle("Female")

# preparation to calculate differences between 2000 and 2016
ncd.n.yoy = select(ncd.n, GeoAreaName, GeoType, Nature, TimePeriod, X.Sex., ncd.name, Value)
str(ncd.n.yoy)

# long data to wide
temp1 = bind_cols(subset(ncd.n.yoy, TimePeriod == 2000))
temp2 = bind_cols(subset(ncd.n.yoy, TimePeriod == 2016))
ncd.n.yoy = merge(temp1, temp2, by = c("GeoType", "GeoAreaName", "Nature", "X.Sex.", "ncd.name"))
# calculate difference
ncd.n.yoy$diff.rate = ncd.n.yoy$Value.y/ncd.n.yoy$Value.x - 1

library(data.table)
# rename columns
setnames(ncd.n.yoy, old = c('Value.x','Value.y'), new = c("y2000", "y2016"))

# function for visulising histogram by difference
viz.hist = function(data, sex, geo) {
  ggplot(subset(data, X.Sex. == sex & GeoType == geo)) +
    geom_histogram(aes(diff.rate), bins = 40) +
    facet_wrap(~ncd.name, nrow = 1) +
    xlab("diff.rate (2000 to 2016)")
}

# ncd deaths increased in most countries over 16 years
viz.hist(ncd.n.yoy, "BOTHSEX", "single") + ggtitle("Bothsex")
viz.hist(ncd.n.yoy, "MALE", "single") + ggtitle("Male")
viz.hist(ncd.n.yoy, "FEMALE", "single") + ggtitle("Female")

N = 10
# top 10 (2000)
topN = function(data, sex, geo, ncd, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & ncd.name == ncd, TimePeriod == year) %>%
    arrange(Value) %>%
    slice(1:N)
}

# CAN
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAN", 2000)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "CAN", 2000)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "CAN", 2000)$GeoAreaName)

# top 10 countries are all identical 
intersect(topN(ncd.n, "FEMALE", "single", "CAN", 2000)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "CAN", 2000)$GeoAreaName)

# by nature
# 2 countries from "E", 3 from "CA" and 5 from "M"
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAN", 2000)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "CAN", 2000)$Nature,
          male = topN(ncd.n, "MALE", "single", "CAN", 2000)$Nature)

# CAR
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAR", 2000)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName)

# 8 countries exist both in female and male 
intersect(topN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName)

# by nature
# most obserations are from "M" and "E"
# more observations of CA in male than female
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAR", 2000)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "CAR", 2000)$Nature,
          male = topN(ncd.n, "MALE", "single", "CAR", 2000)$Nature)

# DIA
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "DIA", 2000)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName)

# 8 countries exist both in female and male
intersect(topN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName)

# by nature
# most obserations are from "M" and "E"
# more observations of CA in male than female
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "DIA", 2000)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "DIA", 2000)$Nature,
          male = topN(ncd.n, "MALE", "single", "DIA", 2000)$Nature)

# RES
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "RES", 2000)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(topN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName)

# by nature
# most obserations are from "E"
# 2 obs of top 3 are from CA
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "RES", 2000)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "RES", 2000)$Nature,
          male = topN(ncd.n, "MALE", "single", "RES", 2000)$Nature)


# top 10 (2016)
# CAN
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAN", 2016)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(topN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName)


# by nature
# most obserations are from "M"
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAN", 2016)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "CAN", 2016)$Nature,
          male = topN(ncd.n, "MALE", "single", "CAN", 2016)$Nature)

# CAR
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAR", 2016)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(topN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName)

# by nature
# top 3 obserations are mostly from "M"
# more observations from "CA" in male than female
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "CAR", 2016)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "CAR", 2016)$Nature,
          male = topN(ncd.n, "MALE", "single", "CAR", 2016)$Nature)

# DIA
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "DIA", 2016)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName)

# 7 countries exist both in female and male 
intersect(topN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName)

# by nature
# all the natures appear in top 3
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "DIA", 2016)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "DIA", 2016)$Nature,
          male = topN(ncd.n, "MALE", "single", "DIA", 2016)$Nature)

# RES
# by country
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "RES", 2016)$GeoAreaName,
          female = topN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName,
          male = topN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(topN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName,
          topN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName)

# FEMALE only
setdiff(topN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName,
        topN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName)
# MALE only
setdiff(topN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName,
        topN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName)

# by nature
# top 3 in bothsex are all "CA"
bind_cols(both = topN(ncd.n, "BOTHSEX", "single", "RES", 2016)$Nature,
          female = topN(ncd.n, "FEMALE", "single", "RES", 2016)$Nature,
          male = topN(ncd.n, "MALE", "single", "RES", 2016)$Nature)

# diff.rate top 10 (2000 - 2016)
topN.improve = function(data, sex, geo, ncd) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & ncd.name == ncd) %>%
    arrange(diff.rate) %>%
    slice(1:N)
}

# CAN
# by country
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAN")$GeoAreaName,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName)

# 6 countries exist both in female and male 
intersect(topN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName,
          topN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName)

# FEMALE only
setdiff(topN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName,
        topN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName)
# MALE only
setdiff(topN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName,
        topN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName)

# by nature
# "CA" is dominant both in female and male
# no obs from "M"
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAN")$Nature,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$Nature,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "CAN")$Nature)

# CAR
# by country
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAR")$GeoAreaName,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName)

# 9 countries exist both in female and male 
intersect(topN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName,
          topN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName)

# FEMALE only
setdiff(topN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName,
        topN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName)
# MALE only
setdiff(topN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName,
        topN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName)

# by nature
# "CA" is dominant both in female and male
# no obs from "E"
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAR")$Nature,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$Nature,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "CAR")$Nature)

# DIA
# by country
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "DIA")$GeoAreaName,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName)

# 5 countries exist both in female and male 
intersect(topN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName,
          topN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName)

# FEMALE only
setdiff(topN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName,
        topN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName)
# MALE only
setdiff(topN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName,
        topN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName)

# by nature
# most obs are from "CA" both in female and male while there is almost no obs from "M"
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "DIA")$Nature,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$Nature,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "DIA")$Nature)

# RES
# by country
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "RES")$GeoAreaName,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName)

# 9 countries exist both in female and male 
intersect(topN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName,
          topN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName)

# FEMALE only
setdiff(topN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName,
        topN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName)
# MALE only
setdiff(topN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName,
        topN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName)

# by nature
# most obs from "CA" both in female and male while there is almost no obs from "M"
bind_cols(both = topN.improve(ncd.n.yoy, "BOTHSEX", "single", "RES")$Nature,
          female = topN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$Nature,
          male = topN.improve(ncd.n.yoy, "MALE", "single", "RES")$Nature)

# min(subset(ncd.n.yoy, X.Sex. == "BOTHSEX" & GeoType == "single" & ncd.name == "CAN")$diff.rate)
# subset(ncd.n.yoy, X.Sex. == "BOTHSEX" & GeoAreaName == "Ukraine" & ncd.name == "CAN")

# worst 10 (2000)
worstN = function(data, sex, geo, ncd, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & ncd.name == ncd, TimePeriod == year) %>%
    arrange(desc(Value)) %>%
    slice(1:N)
}

# CAN
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAN", 2000)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "CAN", 2000)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "CAN", 2000)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "CAN", 2000)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "CAN", 2000)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "CAN", 2000)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "CAN", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "CAN", 2000)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "CAN", 2000)$GeoAreaName)

# by nature
# most obserations are from "CA" while 2 obs of top 3 from "M"
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAN", 2000)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "CAN", 2000)$Nature,
          male = worstN(ncd.n, "MALE", "single", "CAN", 2000)$Nature)

# CAR
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAR", 2000)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "CAR", 2000)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "CAR", 2000)$GeoAreaName)

# by nature
# top 3 are all from either "M" or "E" while there are also obs from "CA"
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAR", 2000)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "CAR", 2000)$Nature,
          male = worstN(ncd.n, "MALE", "single", "CAR", 2000)$Nature)

# DIA
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "DIA", 2000)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "DIA", 2000)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "DIA", 2000)$GeoAreaName)

# by nature
# top 5 obs are all from "either CA" or "M"
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "DIA", 2000)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "DIA", 2000)$Nature,
          male = worstN(ncd.n, "MALE", "single", "DIA", 2000)$Nature)

# RES
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "RES", 2000)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "RES", 2000)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "RES", 2000)$GeoAreaName)

# by nature
# less obs from "CA" compared to other worst top 10
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "RES", 2000)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "RES", 2000)$Nature,
          male = worstN(ncd.n, "MALE", "single", "RES", 2000)$Nature)

# worst 10 (2016)
# CAN
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAN", 2016)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "CAN", 2016)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "CAN", 2016)$GeoAreaName)

# by nature
# most obserations are from "CA" while 2 obs of top 3 from "M"
# not much change from 2000
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAN", 2016)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "CAN", 2016)$Nature,
          male = worstN(ncd.n, "MALE", "single", "CAN", 2016)$Nature)

# CAR
# by country
# top 5 countires are all same
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAR", 2016)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName)

# all countries are identical in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "CAR", 2016)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "CAR", 2016)$GeoAreaName)

# by nature
# most obserations are from "either CA" or "M"
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "CAR", 2016)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "CAR", 2016)$Nature,
          male = worstN(ncd.n, "MALE", "single", "CAR", 2016)$Nature)

# DIA
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "DIA", 2016)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "DIA", 2016)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "DIA", 2016)$GeoAreaName)

# by nature
# most obserations are from "either CA" or "M"
# more "CA" in male
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "DIA", 2016)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "DIA", 2016)$Nature,
          male = worstN(ncd.n, "MALE", "single", "CAN", 2016)$Nature)

# RES
# by country
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "RES", 2016)$GeoAreaName,
          female = worstN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName,
          male = worstN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName)

# 9 countries exist both in female and male 
intersect(worstN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName,
          worstN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName)

# FEMALE only
setdiff(worstN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName,
        worstN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(ncd.n, "MALE", "single", "RES", 2016)$GeoAreaName,
        worstN(ncd.n, "FEMALE", "single", "RES", 2016)$GeoAreaName)

# by nature
# no obs from "E"
bind_cols(both = worstN(ncd.n, "BOTHSEX", "single", "RES", 2016)$Nature,
          female = worstN(ncd.n, "FEMALE", "single", "RES", 2016)$Nature,
          male = worstN(ncd.n, "MALE", "single", "RES", 2016)$Nature)

# diff.rate worst 10 (2000 - 2016)
worstN.improve = function(data, sex, geo, ncd) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & ncd.name == ncd) %>%
    arrange(desc(diff.rate)) %>%
    slice(1:N)
}

# CAN
# by country
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAN")$GeoAreaName,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName)

# 5 countries exist both in female and male 
intersect(worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName,
          worstN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName)

# FEMALE only
setdiff(worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName)
# MALE only
setdiff(worstN.improve(ncd.n.yoy, "MALE", "single", "CAN")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$GeoAreaName)

# by nature
# most obs are from "E"
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAN")$Nature,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAN")$Nature,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "CAN")$Nature)

# CAR
# by country
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAR")$GeoAreaName,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName)

# 7 countries exist both in female and male 
intersect(worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName,
          worstN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName)

# FEMALE only
setdiff(worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName)
# MALE only
setdiff(worstN.improve(ncd.n.yoy, "MALE", "single", "CAR")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$GeoAreaName)

# by nature
# most obs are from either "E" or "M"
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "CAR")$Nature,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "CAR")$Nature,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "CAR")$Nature)

  # DIA
# by country
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "DIA")$GeoAreaName,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName)

# 4 countries exist both in female and male 
intersect(worstN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName,
          worstN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName)

# FEMALE only
setdiff(worstN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName)
# MALE only
setdiff(worstN.improve(ncd.n.yoy, "MALE", "single", "DIA")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$GeoAreaName)

# by nature
# most obs are from "M" in female and "CA" in male
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "DIA")$Nature,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "DIA")$Nature,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "DIA")$Nature)

# RES
# by country
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "RES")$GeoAreaName,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName)

# 7 countries exist both in female and male 
intersect(worstN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName,
          worstN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName)

# FEMALE only
setdiff(worstN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName)
# MALE only
setdiff(worstN.improve(ncd.n.yoy, "MALE", "single", "RES")$GeoAreaName,
        worstN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$GeoAreaName)

# by nature
# more obs of "CA" in female than male
bind_cols(both = worstN.improve(ncd.n.yoy, "BOTHSEX", "single", "RES")$Nature,
          female = worstN.improve(ncd.n.yoy, "FEMALE", "single", "RES")$Nature,
          male = worstN.improve(ncd.n.yoy, "MALE", "single", "RES")$Nature)

# max(subset(ncd.n.yoy, X.Sex. == "BOTHSEX" & GeoType == "single" & ncd.name == "CAN")$diff.rate)
# subset(ncd.n.yoy, X.Sex. == "BOTHSEX" & GeoAreaName == "United Arab Emirates" & ncd.name == "CAN")

################### findings ###################
# number of deaths attributed to ncd dramtically increased over 16 years
# in 2016, ncd populations are particularly high in China, India, and USA
# CAR seems most prevalent among four
# countries with less population naturally show the less number of deaths and vice verca
# hence improve rate is used to comprehend datasets
# european countries shows high improvement rate in general

# the following would be required for further improvement:
# 1. CAN: imperovement in Nature "E"
# 2. CAR: imperovement both in Nature "E" and "M"
# 3. DIA: all Natures should be equally improved
# 4. RES: all Natures should be equally improved