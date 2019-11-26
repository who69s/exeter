getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM501/assignment/4")

library(readxl)
schoolboys = read_xls("schoolboys.xls")
str(schoolboys)

library(tidyverse)
schoolboys = schoolboys %>%
  rename(centred.age = "Age centered about 13 years",
         height = "Height (cm)",
         season = "Season, months from start of year") %>%
  mutate(ID = as.factor(ID))

str(schoolboys)

sblm = lm(height ~ centred.age + season, data = schoolboys)
sblm = lm(height ~ centred.age, data = schoolboys)

sblm
xtable(summary(sblm)$coefficients)

library(ggplot2)
ggplot(data = sblm, aes(x = centred.age, y = height)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  xlab("Age centered about 13 years") +
  ylab("Height (cm)") +
  ggtitle("Relationship between height and age") +
  ggthemes::theme_economist()

library(lme4)
sblmer1 <- lmer(height ~ centred.age + season + (1|ID), data = schoolboys)
ggplot(data = sblmer1, aes(x = centred.age, y = height)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  xlab("Age centered about 13 years") +
  ylab("Height (cm)") +
  ggtitle("Relationship between height and age") +
  ggthemes::theme_economist()

summary(sblmer1)

sblmer1diag <- data.frame(Residuals = resid(sblmer1),
                          ID = schoolboys$ID,
                          Fitted = fitted(sblmer1))

# explore the variance components of the hierarchical model

print(VarCorr(sblmer1), comp = "Variance")
# ICC
65.5547/(65.5547+1.7228)

# ICC
performance::icc(sblmer1) #same as above

# plot of the lowest level residuals facetted by boy
ggplot(data = sblmer1diag, aes(x = Fitted, y = Residuals, col = ID)) +
  geom_point() +
  facet_wrap(~ ID) +
  ggtitle("Lowest level residuals facetting by boy ID") +
  ggthemes::theme_few()

# plot of the random effects from your hierarchical model
plot_model(sblmer1, type = "re") #Plotting random effects

# plot of the marginal effect of age on height
plot_model(sblmer1, type = "eff", terms = "centred.age") +
  geom_point(aes(x = centred.age, y = height), data = schoolboys) +
  ggthemes::theme_economist()

# hierarchical model with random intercepts and random slopes
sblmer2 = lmer(height ~ centred.age + season + (1 + centred.age|ID),
               data = schoolboys)#,
               control = lmerControl(optimizer ="Nelder_Mead"))
summary(sblmer2)
sblmer2



schoolboys.new = schoolboys
schoolboys.new$sblmer.predictions <- predict(sblmer2)
ggplot(aes(x = centred.age, y = sblmer.predictions,
           colour = ID), data = schoolboys.new) +
  geom_line(size=.3) +
  geom_point(aes(y = height)) +
  xlab("Age centered about 13 years") +
  ylab("Height (cm)") +
  ggthemes::theme_few()

sblmer2diag <- data.frame(Residuals = resid(sblmer2),
                          ID = schoolboys.new$ID,
                          Fitted = fitted(sblmer2))
ggplot(data = sblmer2diag, aes(x = Fitted, y = Residuals, col = ID)) +
  geom_point() +
  facet_wrap(~ ID) +
  ggtitle("Lowest level residuals by school") +
  ggthemes::theme_few()

# Random effects
plot_model(sblmer2, type = "re")

#Marginal effects
plot_model(sblmer2, type = "eff", terms = "centred.age") +
  geom_point(aes(x = centred.age, y = height), data = schoolboys.new)

library(gridExtra)
library(sjPlot)
library(sjmisc)

sjp.lmer(sblmer2, type = "fe.cor")
sjp.lmer(sblmer2, type = "re.qq")
p <- plot_model(jsplmer2, type = "diag")
plot_grid(p)

# cmodel comparison
anova(sblmer1,sblmer2)
