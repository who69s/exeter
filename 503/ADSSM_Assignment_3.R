getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM503/assignment")
copd.ob = read.csv("copdobserved.csv")
copd.ex = read.csv("copdexpected.csv")
library(tidyverse)

library(extrafont)
theme = theme(axis.title.x = element_text(colour = "black", size = 10, family = "Segoe UI"),
              axis.title.y = element_text(colour = "black", size = 10, family = "Segoe UI"),
              axis.text.x = element_text(size = 10, family = "Segoe UI"),
              axis.text.y = element_text(size = 10, family = "Segoe UI"),
              legend.title = element_text(size = 10, family = "Segoe UI"),
              legend.text = element_text(size = 8, family = "Segoe UI"),
              strip.text = element_text(size = 8, family = "Segoe UI"),
              plot.title = element_text(colour = "darkgrey",size = 12,family = "Segoe UI",
                                        face = "italic",hjust = 0.5))

# 1 Summarise the number of hospital admissions
summary(copd.ob)

col = c("city", 2001:2010)
colnames(copd.ex) = col
colnames(copd.ob) = col

# remove spaces in the far right
copd.ex$city = trimws(copd.ex$city, "right")
copd.ob$city = trimws(copd.ob$city, "right")

# city columns are identical between two datasets
sum(copd.ex$city != copd.ob$city)
# store nchar of cities
nch.end = nchar(as.character(copd.ex$city))
# nch.end = nchar(as.character(copd.ob$city))

# create authority code list
# find the index of the last space in each city
blank.last = c()
for (i in 1:324) {
  blank = str_locate_all(copd.ex$city, " ")[[i]][, 2]
  blank.last = c(blank.last, blank[length(blank)] + 1)
}

# add authority code to datasets
copd.ex$authority = substr(copd.ex$city, blank.last, nch.end)
copd.ob$authority = copd.ex$authority

count(copd.ex, authority)

# pivot data
pivot.ex = copd.ex[, -12] %>%
  pivot_longer(-city, names_to = "year", values_to = "n.ex")

pivot.ex = merge(pivot.ex, copd.ex[, c("city", "authority")],
                 by = "city", all = TRUE)

pivot.ob = copd.ob[, -12] %>%
  pivot_longer(-city, names_to = "year", values_to = "n.ob")

pivot.ob = merge(pivot.ob, copd.ob[, c("city", "authority")],
                 by = "city", all = TRUE)

# summarise by year
sum.ex.year = pivot.ex %>%
  group_by(year) %>%
  summarise(mean.n.ex = mean(n.ex))

sum.ob.year = pivot.ob %>%
  group_by(year) %>%
  summarise(mean.n.ob = mean(n.ob))

sum.by.year = merge(sum.ex.year, sum.ob.year,
                    by = "year", all = TRUE)

sum.by.year

# plot 1; all
library(reshape2)
v1 = melt(sum.by.year, id.vars = "year") %>%
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line(aes(group = variable, linetype = variable)) +
  scale_linetype_discrete(name = NULL, limits = c("mean.n.ob", "mean.n.ex"),
                          labels = c("Observed", "Expected")) +
  # geom_text(aes(label = round(value, 2), vjust = -.5), size = 4, family = "Segoe UI") +
  xlab("") +
  ylab("Mean") +
  theme

v1

# summarise by authority and year
sum.ex.authority.y = pivot.ex %>%
  group_by(authority, year) %>%
  summarise(mean.n.ex = mean(n.ex))

sum.ob.authority.y = pivot.ob %>%
  group_by(authority, year) %>%
  summarise(mean.n.ob = mean(n.ob))

sum.by.authority.y = merge(sum.ex.authority.y, sum.ob.authority.y,
                           by = c("authority", "year"), all = TRUE)

# summarise by authority
sum.by.authority = sum.by.authority.y %>%
  group_by(authority) %>%
  summarise(mean.n.ex = mean(mean.n.ex), mean.n.ob = mean(mean.n.ob))

sum(sum.by.authority$mean.n.ex < sum.by.authority$mean.n.ob)/nrow(sum.by.authority)
# observed numbers were more than expexted numbers in 3 areas out of 4.

# plot 2: trend by authority areas over years
v2 = ggplot(sum.by.authority.y, aes(x = year, y = mean.n.ob, colour = authority)) +
  geom_point() +
  geom_line(aes(group = authority)) +
  geom_line(aes(y = mean.n.ex, colour = authority, group = authority), linetype = "dashed") +
  xlab("") +
  ylab("Mean") +
  theme
v2

# summarise by city
sum.ex.city = pivot.ex %>%
  group_by(city) %>%
  summarise(mean.n.ex = mean(n.ex))

sum.ob.city = pivot.ob %>%
  group_by(city) %>%
  summarise(mean.n.ob = mean(n.ob))

sum.by.city = merge(sum.ex.city, sum.ob.city,
                    by = "city", all = TRUE)

sum(sum.by.city$mean.n.ex < sum.by.city$mean.n.ob)/nrow(sum.by.city)
# observed numbers were more than expexted numbers in 139 cities out of 324 areas.

# summarise by city and year
sum.by.city.y = merge(pivot.ex[, -4], pivot.ob[, c("city", "year", "n.ob")]
                      , by = c("city", "year"), all = TRUE)

# plot 3: trend by cities over years
v3 = ggplot(sum.by.city.y, aes(x = year, y = n.ob, colour = city)) +
  geom_point() +
  geom_line(aes(group = city)) +
  geom_line(aes(y = n.ex, colour = city, group = city), linetype = "dashed") +
  theme(legend.position = "none") +
  xlab("") +
  ylab("") +
  theme
v3

# while most of the data points are below 200,
# there are 8 cities that are plotted constantly above that line

# show those 8 cities
# library(dplyr)
top8 = sum.by.city %>%
  arrange(desc(mean.n.ob)) %>%
  slice(1:8)

v4 = sum.by.city.y %>%
  filter(sum.by.city.y$city %in% top8$city) %>%
  ggplot(aes(x = year, y = n.ob, colour = city)) +
  geom_point() +
  geom_line(aes(group = city)) +
  geom_line(aes(y = n.ex, colour = city, group = city), linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme(legend.position = "top") +
  theme
v4


# 2 Estimate raw and smoothed standardised morbidity ratios (SMR)
pivot.merge = merge(pivot.ob[, c("city", "year", "n.ob")],
                    pivot.ex[, c("city", "year", "n.ex")],
                    by = c("city", "year"), all = T)

pivot.merge$SMR_raw = pivot.merge$n.ob/pivot.merge$n.ex
summary(pivot.merge)

library(spData)
library(spdep)
library(rgdal)
England = readOGR(dsn = '.', layer = 'englandlocalauthority')

# Combining Raw SMRs and the shapefile
SMR <- merge(England, pivot.merge,
             by.x = 'name', by.y = 'city',
             # by = 'name',
             duplicateGeoms = T)

SMR <- st_as_sf(SMR)

# Creating map of Raw
library(RColorBrewer)
map.all = ggplot(SMR, aes(fill = SMR_raw)) +
  geom_sf(colour = NA) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', fill = 'SMR') +
  # facet_grid(~year) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdPu'),
                       breaks = c(0, 1, 2)) +
  theme

map.all

# Smoothed SMRs
# Creates the neighbourhood
W.nb <- poly2nb(England, row.names = rownames(England))
# Creates a matrix for following function call
W.mat <- nb2mat(W.nb, style = "B")

# Running smoothing model
library(MASS)
library(Rcpp)
library(CARBayes)
pivot.merge.2001 = pivot.merge %>% filter(year == 2001)
model = S.CARleroux(formula = n.ob ~ offset(log(n.ex)), # Model Formula
                    data = pivot.merge.2001, # Dataset name
                    family = "poisson", # Choosing Poisson Regression
                    W = W.mat, # Neighbourhood matrix
                    burnin = 20000, # Number of burn in samples (throw away)
                    n.sample = 100000, # Number of MCMC samples
                    thin = 10,
                    rho = 1)

# Creating a dataset with smoothed SMRs in 2001
pivot.merge.2001$FittedValue = model$fitted.values
pivot.merge.2001$SMR_smooth = pivot.merge.2001$FittedValue/pivot.merge.2001$n.ex
summary(pivot.merge.2001)

# SMR.hist = melt(pivot.merge.2001[, c(5,7)]) %>%
SMR.hist = melt(pivot.merge.2001[, c(5, 7)])
ggplot(SMR.hist, aes(x = value)) +
  geom_histogram(data = subset(SMR.hist, variable == 'SMR_raw'),
                 aes(x = value, fill = variable), alpha = 0.2,
                 binwidth = .05) +
  geom_histogram(data = subset(SMR.hist, variable == 'SMR_smooth'),
                 aes(x = value, fill = variable), alpha = 0.5,
                 binwidth = .05) +
  scale_fill_manual(values = c("red","darkgray")) +
  xlab("SMR (binwidth = 0.05)") +
  theme

# Plot of raw vs smoothed SMRs
ggplot(pivot.merge.2001, aes(SMR_raw,SMR_smooth)) +
  geom_point(colour = 'blue') +
# Adding x = y line for comparison
  geom_abline(intercept = 0,
              slope = 1,
              colour = 'red') +
  labs(x = 'Raw SMRs',
       y = 'Smooth SMRs') +
  xlim(0, 2) +
  ylim(0, 2) +
  theme

# view 10 highest SMRs in 2001
top10.2001 = pivot.merge.2001 %>%
  arrange(desc(SMR_raw)) %>%
  slice(1:10)

SMR.2001 = merge(England, pivot.merge.2001,
                 by.x = 'name', by.y = 'city',
                 # by = 'name',
                 duplicateGeoms = T)

SMR.2001 = st_as_sf(SMR.2001)

SMR.2001.raw = ggplot(SMR.2001, aes(fill = SMR_raw)) +
  geom_sf(colour = NA) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', fill = 'SMR') +
  # facet_grid(~year) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdPu'),
                       breaks = c(0, 1, 2)) +
  ggtitle("SMR raw - 2001") +
  theme

SMR.2001.smooth = ggplot(SMR.2001, aes(fill = SMR_smooth)) +
  geom_sf(colour = NA) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', fill = 'SMR') +
  # facet_grid(~year) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdPu'),
                       breaks = c(0, 1, 2)) +
  ggtitle("SMR smooth - 2001") +
  theme

library(gridExtra)
grid.arrange(SMR.2001.raw, SMR.2001.smooth, ncol = 2)

# 3 Investigate any changes in the risks of hospitalisation due to COPD in England over time
pivot.merge.temp.final = c()
for (i in 2001:2010) {
  pivot.merge.temp = pivot.merge %>% filter(year == i)
  model = S.CARleroux(formula = n.ob ~ offset(log(n.ex)), # Model Formula
                      data = pivot.merge.temp, # Dataset name
                      family = "poisson", # Choosing Poisson Regression
                      W = W.mat, # Neighbourhood matrix
                      burnin = 20000, # Number of burn in samples (throw away)
                      n.sample = 100000, # Number of MCMC samples
                      thin = 10,
                      rho = 1)
  pivot.merge.temp$FittedValue = model$fitted.values
  pivot.merge.temp$SMR_smooth = pivot.merge.temp$FittedValue/pivot.merge.temp$n.ex
  pivot.merge.temp.final = rbind(pivot.merge.temp.final, pivot.merge.temp)
}


ggplot(pivot.merge.temp.final, aes(x = year, y = SMR_smooth, colour = city)) +
  geom_point() +
  geom_line(aes(group = city)) +
  theme(legend.position = "none") +
  theme

highest.result = c()
for (i in 2001:2010) {
  highest = pivot.merge.temp.final %>%
    filter(year == i) %>%
    arrange(desc(SMR_smooth)) %>%
    slice(1)
  highest.result = rbind(highest.result, highest)
}

count(highest.result, city)

lowest.result = c()
for (i in 2001:2010) {
  lowest = pivot.merge.temp.final %>%
    filter(year == i) %>%
    arrange(SMR_smooth) %>%
    slice(1)
  lowest.result = rbind(lowest.result, lowest)
}

count(lowest.result, city)

# Creating a dataset with smoothed SMRs in 2001
pivot.merge.2001$FittedValue = model$fitted.values
pivot.merge.2001$SMR_smooth = pivot.merge.2001$FittedValue/pivot.merge.2001$n.ex


map.all + facet_wrap(~year, nrow = 2)