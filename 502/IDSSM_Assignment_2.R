# Question 1
# a
# E(X) = 1.6m (96s)  >> 1/lambda = 96
lambda = 1/96

pexp(30, lambda)
1 - exp(-lambda*30)

# b
# due to memoryless property
# probability that the next occurence is within 210 seconds after 30 seconds
# P(X > 30 + 210 | X > 30) = P(X > 210)
# probability that the next occurence is within 180 seconds after 30 seconds
# P(X > 30 + 180 | X > 30) = P(X > 180)
# P(180 < X < 210)
(1 - exp(-lambda*210)) - (1 - exp(-lambda*180))

# Question 2
# 2-a
e2 = data.frame(value = rexp(10000, 2))
ggplot(e2, aes(x = value)) +
  geom_histogram(breaks = seq(0,7, length.out = 20))

# e2.plot = ggplot(e2, aes(x = value))
# e2.plot + geom_histogram(bins = 100)
# e2.plot + geom_density()

# 2-b
e3 = data.frame(value = rexp(10000, 3))
ggplot(e3, aes(x = value)) +
  geom_histogram(breaks = seq(0,7, length.out = 20))

# e3.plot = ggplot(e3, aes(x = value))
# e3.plot + geom_histogram(bins = 100)
# e3.plot + geom_density()

# 2-c
e.min = data.frame(x = pmin(e2, e3))
ggplot(e.min, aes(x = value)) +
  geom_histogram(breaks = seq(0,7, length.out = 20))

# 2-d
e.min.lambda = 1/mean(e.min$value)
# A. This is an exponential distribution with lambda = 5 (2 + 3)

# Question 3
# a
randgen = function(N = 10, a = 3, m = 31, seed = 2) {
  rn.v = c()
  for (i in 1:N) {
    x = a * seed %% m
    rn = x/m
    rn.v = c(rn.v, rn)
    seed = x
  }
  return(rn.v)
  #print(rn.v)
}

rn.v10 = randgen()

# b
rn.v1000 = randgen(1000)
rn = randgen(1000, 65539, 2^13, 2019)

# c
rn.new = rn * 2 + 3
hist(rn.new, breaks = 10)
# hist(rn, breaks = 10)

# A. Uniform distribution

max(rn.new)
min(rn.new)

# parameters min = 19
# parameters max = 130873
hist(runif(1000, 19, 130873))

