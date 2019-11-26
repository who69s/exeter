getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM502/assignment")
titan = read.csv("IDSSM_Assignment_3_titanic.csv")

x = titan$survived
mean(x)
# survival rate was 38.19%

titan = titan %>%
  mutate(survived = as.factor(survived), pclass = as.factor(pclass))

# check na in dataset
sum(is.na(titan)) # 264
sum(is.na(titan$pclass))
sum(is.na(titan$survived))
sum(is.na(titan$name))
sum(is.na(titan$gender))
sum(is.na(titan$age)) # 263
sum(is.na(titan$fare)) # 1

# plot the results
v1 = ggplot(titan, aes(x = gender, fill = survived)) +
  geom_bar() +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
v2 = ggplot(titan, aes(x = pclass, fill = survived)) +
  geom_bar() +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

library(gridExtra)
grid.arrange(v1, v2, ncol=2)
