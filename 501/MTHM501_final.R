getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM501/final")
ab.tokyo = read.csv("ablistings_tokyo.csv")
ab.paris = read.csv("ablistings_paris.csv")

# additional fields from full file
ab.tokyo.add = read.csv("ablistings-tokyo-full.csv") %>%
  dplyr ::select(id, host_since, property_type)
ab.paris.add = read.csv("ablistings-paris-full.csv") %>%
  dplyr ::select(id, host_since, property_type)

# merge dataset
ab.tokyo = merge(ab.tokyo, ab.tokyo.add)
ab.paris = merge(ab.paris, ab.paris.add)

str(ab.tokyo)
str(ab.paris)

# get year and month from host_since
ab.tokyo$date = as.Date(ab.tokyo$host_since, "%Y-%m-%d")
ab.tokyo$year = format(as.Date(ab.tokyo$date), "%Y")
ab.tokyo$yearmonth = format(as.Date(ab.tokyo$date), "%Y-%m")
ab.paris$date = as.Date(ab.paris$host_since, "%Y-%m-%d")
ab.paris$year = format(as.Date(ab.paris$date), "%Y")
ab.paris$yearmonth = format(as.Date(ab.paris$date), "%Y-%m")

# summary of count by year
count.tokyo = count(ab.tokyo, year)
count.tokyo$ncum = cumsum(count.tokyo$n)
count.tokyo$city = "Tokyo"
count.paris = count(ab.paris, year)
count.paris$ncum = cumsum(count.paris$n)
count.paris$city = "Paris"

# see the number of records without year
sum(is.na(ab.tokyo$year))
sum(is.na(ab.paris$year))

# union two datasets
count.tp = rbind(count.tokyo, count.paris)
# drop if year is NA
count.tp = na.omit(count.tp)

# rolling sum of the number of listings by city
library(ggplot2)
ggplot(data = count.tp) +
  geom_bar(aes(x = year, y = ncum, fill = city),
           stat = "identity", alpha = 0.5, position="dodge") +
  geom_text(aes(x = year, y = ncum, label = ncum, vjust = -0.5), family = "Segoe UI") +
  xlab("Year") +
  ylab("Rolling sum of listings")

# the number of listings by year and city
count.y = ggplot(count.tp) +
  geom_bar(aes(x = year, y = n, fill = city),
           stat = "identity", alpha = 0.5, position="dodge") +
  geom_text(aes(x = year, y = n, label = n), family = "Segoe UI") +
  xlab("Year (2008 - 2019)") +
  ylab("New listings by year")

# summary of count by year and month
count.tokyo.ym = count(ab.tokyo, yearmonth)
count.tokyo.ym$ncum = cumsum(count.tokyo.ym$n)
count.tokyo.ym$city = "Tokyo"
count.paris.ym = count(ab.paris, yearmonth)
count.paris.ym$ncum = cumsum(count.paris.ym$n)
count.paris.ym$city = "Paris"

# union two datasets
count.tp.ym = rbind(count.tokyo.ym, count.paris.ym)
# drop NA
count.tp.ym = na.omit(count.tp.ym)

# barplot of count by year and month
count.ym = ggplot(count.tp.ym) +
  geom_bar(aes(x = yearmonth, y = n, fill = city),
           stat = "identity", alpha = 0.5, position = "dodge") +
  xlab("YearMonth (082008 - 112019)") +
  ylab("New listings by month") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

library(gridExtra)
grid.arrange(count.y, count.ym, ncol = 1)

# create map using google map
# install.packages("ggmap")
library(ggmap)

# install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

# ?register_google
register_google(key = "AIzaSyAHLAfF8-JLjI1WVdJpQndu9exC_tGykGE")
has_google_key()

# see levels of neighbourhood
levels(ab.tokyo$neighbourhood)
levels(ab.paris$neighbourhood)

# confirm there is no record without longitude and latitude
sum(is.na(ab.tokyo$longitude)) + sum(is.na(ab.tokyo$latitude))
sum(is.na(ab.paris$longitude)) + sum(is.na(ab.paris$latitude))

# create map of Tokyo (including remote islands)
tokyo.map.all = get_googlemap("tokyo", zoom = 5,
                              maptype = "satellite",
                              color = "bw",
                              style = "feature:road|visibility:off") %>%
  ggmap()

# plot locations in map of Tokyo (including remote islands)
tma = tokyo.map.all +
  geom_point(data = ab.tokyo, aes(x = longitude, y = latitude, colour = neighbourhood),
             size = 1) +
  theme(legend.position = "none") +
  labs(colour = "Area") +
  xlab("") +
  ylab("") +
  ggtitle("Tokyo map - all")

# create map of Tokyo (only main island)
tokyo.map.main = get_googlemap("tokyo", zoom = 10,
                               maptype = "satellite",
                               color = "bw",
                               style = "feature:road|visibility:off") %>%
  ggmap()

# plot locations in map of Tokyo (only main island)
tmm = tokyo.map.main +
  geom_point(data = ab.tokyo, aes(x = longitude, y = latitude, colour = neighbourhood),
             size = 1) +
  theme(legend.position = "none") +
  labs(colour = "Area") +
  xlab("") +
  ylab("") +
  ggtitle("Tokyo map - main")

# combine two images
# library(gridExtra)
grid.arrange(tma, tmm, ncol=2)

# create map of paris
paris.map = get_googlemap("paris", zoom = 11,
                          maptype = "satellite",
                          color = "bw",
                          style = "feature:road|visibility:off") %>%
  ggmap()

# view map of Paris
paris.map +
  geom_point(data = ab.paris, aes(x = longitude, y = latitude, colour = neighbourhood),
             size = 1) +
  theme(legend.position = "none") +
  labs(colour = "Area") +
  xlab("") +
  ylab("") +
  ggtitle("Paris map")

# rolling mean and variance price of Tokyo by year (in GBP)
mean.tokyo = c()
var.tokyo = c()
for (i in 2010:2019) {
  temp.tokyo = subset(ab.tokyo, year <= i)
  mean.tokyo = c(mean.tokyo, round(mean(temp.tokyo$price * 0.007)))
  var.tokyo = c(var.tokyo, round(var(temp.tokyo$price * 0.007)))
}

mv.tokyo.cum = as.data.frame(cbind(2010:2019, mean.tokyo, var.tokyo, "Tokyo"))
colnames(mv.tokyo.cum) = c("year", "mean", "var", "city")

# rolling mean and variance price of Paris by year (in GBP)
mean.paris = c()
var.paris = c()
for (i in 2008:2019) {
  temp.paris = subset(ab.paris, year <= i)
  mean.paris = c(mean.paris, round(mean(temp.paris$price * 0.84)))
  var.paris = c(var.paris, round(var(temp.paris$price * 0.84)))
}

mv.paris.cum = as.data.frame(cbind(2008:2019, mean.paris, var.paris, city = "Paris"))
colnames(mv.paris.cum) = c("year", "mean", "var", "city")

# merge two datasets
mv.cum = bind_rows(mv.tokyo.cum, mv.paris.cum)
# convert data type
mv.cum$mean = as.numeric(mv.cum$mean)
mv.cum$var = as.numeric(mv.cum$var)

# rolling mean price over time by year
rm = ggplot(data = mv.cum) +
  geom_line(aes(x = year, y = mean, colour = city, group = city),
            stat = "identity", size = 1) +
  xlab("Year") +
  ylab("Mean of Price (GBP)") +
  ggtitle("Mean - cumulative")

# rolling variance price over time by year
rv = ggplot(data = mv.cum) +
  geom_line(aes(x = year, y = var, colour = city, group = city),
            stat = "identity", size = 1) +
  xlab("Year") +
  ylab("Variance of Price (GBP)") +
  ggtitle("Variance - cumulative")

# library(gridExtra)
grid.arrange(rm, rv, ncol = 1)

# list of unique host_ids in Tokyo
host.tokyo = ab.tokyo %>%
  dplyr ::select(year, host_id) %>%
  group_by_all() %>%
  count()

# drop NA
host.tokyo = na.omit(host.tokyo)
host.tokyo$city = "Tokyo"
nrow(host.tokyo)

# the number of housings owned by same host in Tokyo
host.tokyo.hist = ggplot(host.tokyo) +
  geom_histogram(binwidth = 10, aes(x = n), alpha = 0.5) +
  stat_bin(binwidth = 10, geom = "text",
           aes(x = n, label=..count..)) +
  ggtitle("Tokyo (bin = 10)")

# the number of new hosts by year in Tokyo
host.tokyo.new = ggplot(host.tokyo) +
  geom_bar(aes(year), stat = "count", alpha = 0.5) +
  geom_text(stat = 'count', aes(x = year, label = ..count..)) +
  ggtitle("New hosts - Tokyo")

# list of unique host_ids in Paris
host.paris = ab.paris %>%
  dplyr ::select(year, host_id) %>%
  group_by_all() %>%
  count()

# drop NA
host.paris = na.omit(host.paris)
host.paris$city = "Paris"
nrow(host.paris)

# the number of housings owned by same host in Tokyo
host.paris.hist = ggplot(host.paris) +
  geom_histogram(binwidth = 10, aes(n), alpha = 0.5) +
  stat_bin(binwidth = 10, geom = "text",
           aes(x = n, label=..count..)) +
  ggtitle("Paris (bin = 10)")

# the number of new hosts by year in Paris
host.paris.new = ggplot(host.paris) +
  geom_bar(aes(year), stat = "count", alpha = 0.5) +
  geom_text(stat = 'count', aes(x = year, label = ..count..)) +
  ggtitle("New hosts - Paris")

grid.arrange(host.tokyo.hist, host.paris.hist, ncol = 1)
grid.arrange(host.tokyo.new, host.paris.new, ncol = 1)

proptype.paris = count(ab.paris, property_type)
proptype.tokyo = count(ab.tokyo, property_type)

head(proptype.paris[order(-proptype.paris$n),])
head(proptype.tokyo[order(-proptype.tokyo$n),])

roomtype.paris = count(ab.paris, room_type)
roomtype.tokyo = count(ab.tokyo, room_type)

roomtype = cbind(roomtype.tokyo, roomtype.paris$n)
colnames(roomtype) = c("RoomType", "Tokyo", "Paris")
# xtable(roomtype)
