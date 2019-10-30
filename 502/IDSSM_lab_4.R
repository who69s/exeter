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