# The aim of this assingment is to investigate the relationship between the explanatory variables and the production of oxygen.

# read in csv
getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM503/assignment")
ox = read.csv("oxygenfull.csv")

# confirm there are no NAs
sum(is.na(ox))

# drop first column 
ox = ox[,-1]
# ox.scaled = scale(ox, center = TRUE, scale = TRUE)

# view scatter plots and correlation coefficients for each pair of variables using ggpairs
# ?ggpairs
library(extrafont)
theme = theme(axis.title.x = element_text(colour = "black", size = 10, family = "Segoe UI"),
              axis.title.y = element_text(colour = "black", size = 10, family = "Segoe UI"),
              axis.text.x = element_text(size = 10, family = "Segoe UI"),
              axis.text.y = element_text(size = 10, family = "Segoe UI"),
              legend.title = element_text(size = 10, family = "Segoe UI"),
              legend.text = element_text(size = 8, family = "Segoe UI"),
              strip.text = element_text(size = 8, family = "Segoe UI"),
              plot.title = element_text(colour = "darkgrey",size = 15,family = "Segoe UI",
                                        face = "italic",hjust = 0.5))

gg = ggpairs(ox,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"),
        axisLabels = 'show') +
  theme(panel.grid.major = element_blank()) +
  theme

# getPlot(gg, 6, 3) 

# ggpairs(ox[, c(1,2,3,5,6)],
#         lower = list(continuous = "smooth"),
#         diag = list(continuous = "barDiag"),
#         axisLabels = 'show')

# From this strong correlations can be observed between
# OXY and {CHLOR, LIGHT, SUN and SPEED}.
  
# plot OXY against each of those 4 variables
# OXY ~ CHLOR (cor: 0.321)
ggplot(ox, aes(LIGHT, SUN)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme


p.chlor = ggplot(ox, aes(OXY, CHLOR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme

# OXY ~ LIGHT (cor: 0.942)
p.light = ggplot(ox, aes(OXY, LIGHT)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme

# OXY ~ SUN (cor: 0.925)
p.sun = ggplot(ox, aes(OXY, SUN)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme

# OXY ~ SPEED (cor: -0.448)
p.speed = ggplot(ox, aes(OXY, SPEED)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme

library(Rmisc)
multiplot(p.chlor, p.light, p.sun, p.speed, cols = 2)

# corr = seq(-1, 0.9, 0.1)
# x = c()
# y = c()

# for (i in corr) {
#  x = c(x, length(which(cor(ox) > i)) - 7)
#  y = c(y, i)
# }
# yx = cbind(y, x)
# yx
# pairs(ox)

# ?cor()

# cor(ox.scaled) > 0.2
# colSums(cor(ox.scaled) > 0.2) - 1
# positive correlation (> 0.2) can be observed between OXY and {CHLOR, LIGHT, SUN} 

# cor(ox.scaled) < -0.3
# colSums(cor(ox.scaled) < -0.3)
# negative correlation (< -0.3) can be observed between OXY and SPEED

# 2
# new = substr(colnames(ox),1,3)
# old = colnames(ox)
# colnames(ox) = new

ev = colnames(ox)[-1]
mod.summary = c()

for (i in ev) {
  formula = paste("OXY ~", i)
  mod = lm(as.formula(formula), data = data.frame(ox))
  p = summary(mod)$coefficients[2,4]
  p.sig = summary(mod)$coefficients[2,4] < 0.05
  rs = summary(mod)$r.squared
  rs.a = summary(mod)$adj.r.squared
  mod.summary = rbind(mod.summary, c(p, p.sig, rs, rs.a))
}

colnames(mod.summary) = c("pvalue", "pvalue.sig", "rsquared", "adj.rsquared")
mod.summary = as_tibble(mod.summary)
mod.summary$variable.name = ev

xtable(mod.summary, type = "latex")

# Multiple Regression
# formula.m.1 <- OXY ~ 1 + SPEED + LIGHT
# mod.m.1 <- lm(formula.m.1, data = ox)
# summary(mod.m.1)$adj.r.squared
# summary(mod.m.1)$r.squared
# 
# formula.m.2 <- OXY ~ 1 + SUN + LIGHT
# mod.m.2 <- lm(formula.m.2, data = ox)
# summary(mod.m.2)$adj.r.squared
# summary(mod.m.2)$r.squared
# 
# formula.m.3 <- OXY ~ 1 + SUN + SPEED
# mod.m.3 <- lm(formula.m.3, data = ox)
# summary(mod.m.3)$adj.r.squared
# summary(mod.m.3)$r.squared
# 
# formula.m.4 <- OXY ~ 1 + SPEED + LIGHT + SUN
# mod.m.4 <- lm(formula.m.4, data = ox)
# summary(mod.m.4)$adj.r.squared
# summary(mod.m.4)$r.squared

formula.m.1 <- OXY ~ 1 + SPEED + SUN + LIGHT + CHLOR + TEMP + ALT
mod.m.1 <- lm(formula.m.1, data = ox)
summary(mod.m.1)$adj.r.squared
summary(mod.m.1)$r.squared
pv = summary(mod.m.1)$coefficients

formula.m.2 <- OXY ~ 1 + SPEED + SUN + LIGHT + CHLOR + ALT
mod.m.2 <- lm(formula.m.2, data = ox)
summary(mod.m.2)$adj.r.squared
summary(mod.m.2)$r.squared
pv = summary(mod.m.2)$coefficients

formula.m.3 <- OXY ~ 1 + SPEED + LIGHT + CHLOR + ALT
mod.m.3 <- lm(formula.m.3, data = ox)
summary(mod.m.3)$adj.r.squared
summary(mod.m.3)$r.squared
pv = summary(mod.m.3)$coefficients

formula.m.4 <- OXY ~ 1 + SPEED + LIGHT + CHLOR
mod.m.4 <- lm(formula.m.4, data = ox)
summary(mod.m.4)$adj.r.squared
summary(mod.m.4)$r.squared

formula.m.5 <- OXY ~ 1 + CHLOR + LIGHT
mod.m.5 <- lm(formula.m.5, data = ox)
summary(mod.m.5)$adj.r.squared
summary(mod.m.5)$r.squared
pv = summary(mod.m.5)$coefficients

typeof(pv)

library(xtable)
xtable(pv, type = "latex", caption = "TEMP, SUN, ALT, SPEED removed")

summary(mod.m.1)
summary(mod.m.2)
summary(mod.m.3)
summary(mod.m.4)
summary(mod.m.5)
summary(mod.m.6)

# the best model includes SPEED, LIGHT and SUN which explains the most variance of the production of oxygen
# adjusted r.squared is 0.8941195
# r.squared is 0.9050727

mod.m.0 <- lm(OXY ~ LIGHT, data = ox)
mod.m.01 <- lm(OXY ~ CHLOR, data = ox)

predict(mod.m.5,
        newdata = data.frame(CHLOR = 1, LIGHT = 1))


ggplot(data.frame(ox), aes(x = CHLOR, y = OXY)) + # ggplot with the desired data
  geom_point() +
  geom_line(aes(x = CHLOR, y = fitted(mod.m.5)), colour = 'red',) +
  geom_line(aes(x = CHLOR, y = fitted(mod.m.01)), colour = 'blue',)
# + labs(x = 'LIGHT',y = 'OXY')
