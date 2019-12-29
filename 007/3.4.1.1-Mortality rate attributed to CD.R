getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM007")
cd.prob = read.csv("3.4.1.1.csv")
# ncd.n = read.csv("3.4.1.2.csv")
# suicide.n = read.csv("3.4.2.1.csv")
# suicide.per.100k = read.csv("3.4.2.2.csv")

str(cd.prob)

library(tidyverse)
# levels of year
unique(cd.prob$TimePeriod)
# no missing GeoAreaCode
sum(count(cd.prob, GeoAreaName)$n != 15)
# Time_Detail and TimePeriod are identical
sum(cd.prob$Time_Detail != cd.prob$TimePeriod)

# For some GeoAreas includes multiple countries (Nature == NA)
cd.prob$GeoType = case_when(
  is.na(cd.prob$Nature) == T ~ "multi",
  is.na(cd.prob$Nature) == F ~ "single"
)

# omit GeoAreas including mutiple countries
cd.prob.single = subset(cd.prob, GeoType == "single")

# time-series by sex
# mortality rate attributed to cd has decreased over time
# clearly shows rate is higher for male
ggplot(cd.prob.single) +
  geom_line(aes(TimePeriod, Value/100, colour = GeoAreaCode, group = GeoAreaCode)) +
  facet_wrap(~X.Sex., nrow = 1) +
  theme(legend.position = "none") +
  ylab("Mortality rate (cardiovascular disease)")

# function to calculate mean by year and sex
mean.by.year = function(sex, geo) {
  cd.prob %>%
    filter(X.Sex. == sex & GeoType == geo) %>%
    group_by(TimePeriod) %>%
    summarise(both = mean(Value))
}

# result
# shows higher mean for male as line chart
bind_cols(mean.by.year("BOTHSEX", "single"),
          male = mean.by.year("MALE", "single")$both,
          female = mean.by.year("FEMALE", "single")$both)

# preparation to calculate differences between 2000 and 2016
cd.prob.yoy = select(cd.prob, GeoAreaName, GeoType, Nature, TimePeriod, X.Sex., Value)
str(cd.prob.yoy)

# long data to wide
temp1 = bind_cols(subset(cd.prob.yoy, TimePeriod == 2000))
temp2 = bind_cols(subset(cd.prob.yoy, TimePeriod == 2016))
cd.prob.yoy = merge(temp1, temp2, by = c("GeoType", "GeoAreaName", "Nature", "X.Sex."))
# calculate difference
cd.prob.yoy$drop.rate = cd.prob.yoy$Value.y/cd.prob.yoy$Value.x - 1

library(data.table)
# rename columns
setnames(cd.prob.yoy, old = c('Value.x','Value.y'), new = c("y2000", "y2016"))
# keep only "single"
cd.prob.yoy.single = subset(cd.prob.yoy, GeoType == "single")

# histogram by difference
# mortarity rate decreased in most countries over 16 years
ggplot(cd.prob.yoy.single) +
  geom_histogram(aes(drop.rate), bins = 50) +
  facet_wrap(~X.Sex., nrow = 1)

# set the number of records to view in the following sections
N = 10
# top N (2000)
topN = function(data, sex, geo, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & TimePeriod == year) %>%
    arrange(Value) %>%
    slice(1:N)
}

# by country
bind_cols(both = topN(cd.prob, "BOTHSEX", "single", 2000)$GeoAreaName,
          female = topN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName,
          male = topN(cd.prob, "MALE", "single", 2000)$GeoAreaName)

# 3 countries exist both in female and male
# "Japan" "Switzerland" "Australia"  
intersect(topN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName,
          topN(cd.prob, "MALE", "single", 2000)$GeoAreaName)
# FEMALE only
setdiff(topN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName,
        topN(cd.prob, "MALE", "single", 2000)$GeoAreaName)
# MALE only
setdiff(topN(cd.prob, "MALE", "single", 2000)$GeoAreaName,
        topN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName)

# by nature
# clealy shows that top countries exist in "CA" area in 2000
bind_cols(both = topN(cd.prob, "BOTHSEX", "single", 2000)$Nature,
          female = topN(cd.prob, "FEMALE", "single", 2000)$Nature,
          male = topN(cd.prob, "MALE", "single", 2000)$Nature)

# top N (2016)
# by country
bind_cols(both = topN(cd.prob, "BOTHSEX", "single", 2016)$GeoAreaName,
          female = topN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName,
          male = topN(cd.prob, "MALE", "single", 2016)$GeoAreaName)

# 4 countries exist both in female and male
# 3 counrties from 2000 and "Republic of Korea"
intersect(topN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName,
          topN(cd.prob, "MALE", "single", 2016)$GeoAreaName)
# FEMALE only
setdiff(topN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName,
        topN(cd.prob, "MALE", "single", 2016)$GeoAreaName)
# MALE only
setdiff(topN(cd.prob, "MALE", "single", 2016)$GeoAreaName,
        topN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName)

# by nature
# "CA" is still dominant in 2016
bind_cols(both = topN(cd.prob, "BOTHSEX", "single", 2016)$Nature,
          female = topN(cd.prob, "FEMALE", "single", 2016)$Nature,
          male = topN(cd.prob, "MALE", "single", 2016)$Nature)

# drop.rate top N (2000 - 2016)
topN.improve = function(data, sex) {
  data %>%
    filter(X.Sex. == sex) %>%
    arrange(drop.rate) %>%
    slice(1:N)
}

# by country
bind_cols(both = topN.improve(cd.prob.yoy.single, "BOTHSEX")$GeoAreaName,
          female = topN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName,
          male = topN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName)

# 6 countries exist both in female and male
# "Maldives" "Republic of Korea" "Bahrain" "Morocco" "Singapore" "Denmark"  
intersect(topN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName,
          topN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName)
# FEMALE only
setdiff(topN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName,
        topN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName)
# MALE only
setdiff(topN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName,
        topN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName)

# by nature
# most improveed in "CA" followed by "E"
bind_cols(both = topN.improve(cd.prob.yoy.single, "BOTHSEX")$Nature,
          female = topN.improve(cd.prob.yoy.single, "FEMALE")$Nature,
          male = topN.improve(cd.prob.yoy.single, "MALE")$Nature)

# worst N (2000)
worstN = function(data, sex, geo, year) {
  data %>%
    filter(X.Sex. == sex & GeoType == geo & TimePeriod == year) %>%
    arrange(desc(Value)) %>%
    slice(1:N)
}

# by country
bind_cols(both = worstN(cd.prob, "BOTHSEX", "single", 2000)$GeoAreaName,
          female = worstN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName,
          male = worstN(cd.prob, "MALE", "single", 2000)$GeoAreaName)

# 2 countries exist both in female and male
# "Mongolia" "Fiji" 
intersect(worstN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName,
          worstN(cd.prob, "MALE", "single", 2000)$GeoAreaName)
# FEMALE only
setdiff(worstN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName,
        worstN(cd.prob, "MALE", "single", 2000)$GeoAreaName)
# MALE only
setdiff(worstN(cd.prob, "MALE", "single", 2000)$GeoAreaName,
        worstN(cd.prob, "FEMALE", "single", 2000)$GeoAreaName)

# by nature
# "M" is dominant for female and "E" for male
bind_cols(both = worstN(cd.prob, "BOTHSEX", "single", 2000)$Nature,
          female = worstN(cd.prob, "FEMALE", "single", 2000)$Nature,
          male = worstN(cd.prob, "MALE", "single", 2000)$Nature)

# worst 10 (2016)
# by country
bind_cols(both = worstN(cd.prob, "BOTHSEX", "single", 2016)$GeoAreaName,
          female = worstN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName,
          male = worstN(cd.prob, "MALE", "single", 2016)$GeoAreaName)

# no country exist both in female and male in 2016
intersect(worstN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName,
          worstN(cd.prob, "MALE", "single", 2016)$GeoAreaName)
# FEMALE only
setdiff(worstN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName,
        worstN(cd.prob, "MALE", "single", 2016)$GeoAreaName)
# MALE only
setdiff(worstN(cd.prob, "MALE", "single", 2016)$GeoAreaName,
        worstN(cd.prob, "FEMALE", "single", 2016)$GeoAreaName)

# by nature
# trend in 2000 escalted: "M" and "E" are more dominant for female and male respectively
bind_cols(both = worstN(cd.prob, "BOTHSEX", "single", 2016)$Nature,
          female = worstN(cd.prob, "FEMALE", "single", 2016)$Nature,
          male = worstN(cd.prob, "MALE", "single", 2016)$Nature)

# drop.rate worst 10 (2000 - 2016)
worstN.improve = function(data, sex) {
  data %>%
    filter(X.Sex. == sex) %>%
    arrange(desc(drop.rate)) %>%
    slice(1:N)
}

# by country
bind_cols(both = worstN.improve(cd.prob.yoy.single, "BOTHSEX")$GeoAreaName,
          female = worstN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName,
          male = worstN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName)

# 3 countries exist both in female and male
# "Egypt" "Côte d'Ivoire" "Democratic People's Republic of Korea"
intersect(worstN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName,
          worstN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName)
# FEMALE only
setdiff(worstN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName,
        worstN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName)
# MALE only
setdiff(worstN.improve(cd.prob.yoy.single, "MALE")$GeoAreaName,
        worstN.improve(cd.prob.yoy.single, "FEMALE")$GeoAreaName)

# by nature
# "M" is dominant both in female and male
bind_cols(both = worstN.improve(cd.prob.yoy.single, "BOTHSEX")$Nature,
          female = worstN.improve(cd.prob.yoy.single, "FEMALE")$Nature,
          male = worstN.improve(cd.prob.yoy.single, "MALE")$Nature)

################### findings ###################
# mortarity rate of CD decreased overall in the past 16 years
# the following would be required for further improvement:
# 1. overall improvement in MALE
# 2. overall improvement in Nature "E" and "M"
# 3. in partcular, improvement in Nature "E" for male and "M" for female