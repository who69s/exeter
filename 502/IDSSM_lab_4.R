library(tidyverse)
set.seed(1991)


dbinom(2,15,0.2) # P(X=2), X~Binom(15,0.2)
pbinom(2,15,0.2) # P(X<=2)
rbinom(10,15,0.2) # Generates 10 samples from Binom(15,0.2)
qbinom(1/2,15,0.2) # Median 
   # Check: 
  pbinom(2:3,15,0.2)
  qbinom((1:2)/3,15,0.2)
  # Check:
  pbinom(1:4,15,0.2)
  # If 2 is not included I'm under 1/3, if it's included I'm over
  # Similarly, if 4 is not included I'm under 2/3, if it's included I'm over 2/3

  
#### Question 11 ####
# a
dpois(x = 0, lambda = 2)
# b
sum(dpois(x = 2:3, lambda = 2))
# c
sum(dpois(x = 0:5, lambda = 2))
ppois(q = 5, lambda = 2)
# d
1 - sum(dpois(x = 0:5, lambda = 2))
1 - ppois(q = 5, lambda = 2)

#### Question 12 ####
qbinom(p = 0.1, size = 10, prob = 1/3)
qbinom(p = 0.2, size = 10, prob = 1/3)
qbinom(seq(0.1, 0.9, 0.1), 10, 1/3)

n <- 10
p <- 0.3
pmf_bin_choose <- tibble(x = seq(0, n, 1))
pmf_bin_choose <- mutate(pmf_bin, y = choose(n, x) * p^x * (1-p)^(n-x))

# verification
pmf_bin <- tibble(x = seq(0, n, 1))
pmf_bin$y = c(dbinom(0:10, 10, 0.3))

ggplot(pmf_bin, aes(x, y)) +
  geom_segment(aes(xend = x, yend = 0), size = 10, lineend = "butt",
               colour="#0093AF") +
  labs(x="Number of successes", y="Probability") +
  ggtitle("Binom(10,0.3)")

# pecentile where the number of successes can be;
# 7
qbinom(p = 0.99, size = 10, prob = 1/3)
# 8
qbinom(p = 0.999, size = 10, prob = 1/3)
# 9
qbinom(p = 0.9999, size = 10, prob = 1/3)

#### Question 13 ####
# use "sample"
counter = 0
itr = 10000

for (i in 1:itr) {
  if (sum(sample(c(1,0), 100, replace = TRUE, prob = c(.05, .95))) == 5) {
    counter = counter + 1
  }
}

counter/itr

# use "rbinom"
itr = 10000
sum(rbinom(itr, 100, .05) == 5)/itr

counter/itr

# use "dbinom"
dbinom(5, 100, .05)

#### Question 14 ####
# a
# expecation
sum(c(0:50) * dbinom(0:50, 50, .2))

# variance
# E((X - E(X))^2)
ex = sum(c(0:50) * dbinom(0:50, 50, .2))
var = sum((c(0:50) - rep(ex, 51))^2 * dbinom(0:50, 50, .2))

# E(X^2) ??? E(X)^2.
ex = sum(c(0:50) * dbinom(0:50, 50, .2))
x2 = c(0:50)^2
sum(c(0:50)^2 * dbinom(0:50, 50, .2)) - ex^2

# b
x10 = rbinom(10, 50, .2)
x100 = rbinom(100, 50, .2)
x1000 = rbinom(1000, 50, .2)
x10000 = rbinom(10000, 50, .2)
x100000 = rbinom(100000, 50, .2)

xcombine = rbind(c(mean(x10), var(x10)), c(mean(x100), var(x100)),
                 c(mean(x1000), var(x1000)), c(mean(x10000), var(x10000)),
                 c(mean(x100000), var(x100000)), c(ex, var))

colnames(xcombine) = c("mean", "variance")
rownames(xcombine) = c(10, 100, 1000, 10000, 100000, "theory")

#### Question 15 ####
n = c(5, 10, 15, 20, 50, 75, 100, 1000)
np = c()
prob = c()
final = c()
counter = 1

for (i in n) {
  np = c(i, 1/i)
  prob = dbinom(1:5, i, 1/i)
  result = c(np, prob)
  final = as_tibble(rbind(final, result))
}

#rm(final.p)

colnames(final) = c("n", "p", 1:5)
final = rbind(final, c(NA, NA, dpois(1:5, 1)))

#### Question 16 ####
x = seq(0,25,.05)
cdf = cbind(pbinom(x, 11, 10/11)
      ,pbinom(x, 15, 10/15)
      ,pbinom(x, 20, 10/20)
      ,pbinom(x, 50, 10/50)
      ,pbinom(x, 100, 10/100)
      ,ppois(x, 10))

cdf.viz = as_tibble(cdf) %>%
  gather(dist.type, prob)

cdf.viz$X = x

ggplot(cdf.viz, aes(x = X, y = prob, colour = dist.type)) +
  geom_line()

#### teacher's codes for 13 & 16 START ####

#### Question 13 ####
# Estimate 1
N <- 10000 # number of repeats 
counter <- 0 # counter of successes

for(i in 1:N){
  # Number of defective items
  x <- sum(sample(c(1,0),100,replace=TRUE,prob=c(0.05,0.95))) 
  if(x==5){
    counter <- counter + 1 
    }
}
counter/N # estimated probability

# Estimate 2
N <- 10000 # number of repeats 

sum(rbinom(N,100,0.05)==5)/N

# Exact value
dbinom(5,100,0.05)

#### Question 16 ####

# Put all the data into a data frame
x=seq(0,25,0.05)
df <- data.frame(x=x,B11=pbinom(x,11,10/11),B15=pbinom(x,15,10/15),B20=pbinom(x,20,10/20),
                 B50=pbinom(x,50,10/50),B100=pbinom(x,100,10/100),P=ppois(x,10))

# Collapse the data frame
df <- df %>%
  gather(key="func",value="y",-x)

# Plot the functions
# the colour asthetics will ensure that each Binom function is treated separately,
# and that the graphs get different colours
# The geom_line function will plot the lines
ggplot(df,aes(x=x,y=y,colour=func)) +
  geom_line()

#### teacher's codes for 13 & 16 END ####
