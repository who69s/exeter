getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM503/assignment")
ox = read.csv("oxygenfull.csv")

ox = ox[,-1]
ox.scaled = scale(ox, center = TRUE, scale = TRUE)

rm(x.v)
x = seq(-1, 0.9, 0.1)
x.v = c()
thres = c()

?cor()

for (i in x) {
  x.v = c(x.v, length(which(cor(ox.scaled) > i)) - 7)
  thres = c(thres, i)
}

x.v = cbind(thres, x.v)
x.v

cor(ox.scaled) > 0.2
colSums(cor(ox.scaled) > 0.2) - 1
# positive correlation (> 0.2) can be observed between OXY and {CHLOR, LIGHT, SUN} 

cor(ox.scaled) < -0.3
colSums(cor(ox.scaled) < -0.3)
# negative correlation (< -0.3) can be observed between OXY and SPEED

# confirm those correlation with visualisation
ggplot(ox) +
  geom_point(aes(OXY, CHLOR))

ggplot(ox) +
  geom_point(aes(OXY, LIGHT))

ggplot(ox) +
  geom_point(aes(OXY, SUN))

ggplot(ox) +
  geom_point(aes(OXY, SPEED))

ggpairs(ox,
        lower = list(continuous = "smooth"),
        diag = list(continuous="barDiag"),
        axisLabels = 'show')

pairs(ox)

# 2
mod1 = lm(OXY ~ LIGHT, data = data.frame(ox.scaled))
ggplot(data.frame(ox.scaled), aes(x = LIGHT, y = OXY)) + # ggplot with the desired data
  geom_point() +
  geom_line(aes(x = LIGHT, y = fitted(mod1)), colour = 'red',) #+
  # labs(x = 'LIGHT',y = 'OXY')

summary(mod1)$adj.r.squared
summary(mod1)$r.squared
# R-squared:  0.8867,	Adjusted R-squared:  0.8827 

# Multiple Regression
formula <- OXY ~ 1 + SPEED + LIGHT
# Fitting the linear model
mod2 <- lm(formula, data = ox)
summary(mod2)$adj.r.squared
summary(mod2)$r.squared
# R-squared:  0.899,	Adjusted R-squared:  0.8915

predict(mod2,
        newdata = data.frame(SPEED = 9, LIGHT = 2))

mod3 <- lm(OXY ~ CHLOR + SUN + LIGHT + SPEED, data = ox)
summary(mod3)$adj.r.squared
summary(mod3)$r.squared
