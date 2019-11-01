# Question 6
# a
pexp(q = 5, rate = 5)
pexp(q = 5, rate = 5) - pexp(q = 3, rate = 5)
1 - pexp(q = 6, rate = 5)

# b
punif(q = 3.5, min = 2, max = 10)
punif(q = 5, min = 2, max = 10) - punif(q = 4.5, min = 2, max = 10) + punif(q = 6, min = 2, max = 10) - punif(q = 5.5, min = 2, max = 10)
punif(q = 5, min = 2, max = 10) - punif(q = 5, min = 2, max = 10)

# Question 7
# a
n = 100000
X = runif(n = n, min = 10, max = 15)

A = subset(X, X > 12 & X < 14)
B = subset(X, X > 13 & X < 15)
C = subset(X, X > 13 & X < 14)

length(A)/n * length(B)/n
length(C)/n

# A. Not independent for P(A)*P(B) is not equal to P(C)

# b
n = 10000
X = runif(n = n, min = 0, max = 1)
Y = runif(n = n, min = 0, max = 1)
XY = X*Y

A = subset(XY, Y > .5)
B = subset(XY, X > .5)
C = subset(XY, Y > .5 & X > .5)

length(A)/n * length(B)/n
length(C)/n

# A. Two events are likely to be independent.

# Question 8
# a-b
a = 1664525
c = 1013904223
m = 2^32
d = as.numeric(Sys.time())*1000 # seed
N = 10
rn = vector(length = 10) # initialising the vector

for(i in 1:N){
  d = (a * d + c) %% m
  rn[i] = d/m
}

(a * d + c)/m
d %% m
# rm(rn)
rn

# c
# create function
rand.gen <- function(N = 10){
  a = 1664525
  c = 1013904223
  m = 2^32
  d = as.numeric(Sys.time())*1000 # seed
  rn = vector(length = N) # initialising the vector
  for(i in 1:N){
    d = (a*d + c) %% m
    rn[i] = d/m
  }
  return(rn)
}

rand.gen()

# d
rand.gen(1)
rand.gen(5)
rand.gen(10)
rand.gen(20)
rand.gen(100)

# e
library(tidyverse)
X = as_tibble(-log(rand.gen(1000)))

# viz 1
hist(-log(rand.gen(1000)), breaks = 10)

# viz 2
ggplot(X, aes(x = value)) +
  geom_histogram(breaks = seq(0,7, length.out = 20))

1/mean(unlist(X)) # lambda likely to be 1