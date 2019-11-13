getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM501/assignment/3")
animals = read.csv("Animals.csv")

install.packages("cclust")
library("cclust")

# rename columns
colnames(animals) = c("Weight", "Height", "Species")
# rename levels of Species
levels(animals$Species) = c("Ostrich", "Deer", "Bear", "Giant tortoise")

# Qestion 1
# create distance matrix for the full dataset
d = dist(as.matrix(animals[, -3]), method = "manhattan")
# prepare hierarchical cluster
hc = hclust(d)
hcd = as.dendrogram(hc)
# define nodePar
nodePar <- list(lab.cex = 1000, pch = c(NA, 19), 
                cex = 0.1, col = "blue")
# view dendrogram
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")

install.packages("ggdendro")
ggdendrogram(hc)
# cut the dendrogram to give 4 clusters
groups.4 <- cutree(hc,4)
# cross tabulation of 4 cluster cut vs. transmission
table(animals$Species, groups.4)

# Qestion 2
x = animals[,-3]
y = animals$Species

result = c()
for (i in 2:10) {
  kc = kmeans(x, i)
  result = rbind(result, c(i, kc$tot.withinss))
}

colnames(result) = c("nclust", "totwss")
result = data.frame(result)

# plot of within sum of squares vs. cluster size (2 to 10)
plot(result, col = "red")

# find the best number of clusters
out.spl = with(result, smooth.spline(nclust, totwss, df = 4))
with(result, predict(out.spl, x = nclust, deriv = 2))
derivative.out = with(result, predict(out.spl, x = nclust, deriv = 2))
max(derivative.out$y)

derivative.out[lapply(derivative.out, derivative.out$y) == max(derivative.out$y)]

install.packages("rlist")

library(rlist)
derivative.out$y %>%
  list.filter(derivative.out == max(derivative.out$y))

# add vertical line at the point of maximum curvature
abline(v = 4, col = "blue")
# lines(faithful$eruptions, fitted(fit), col="blue")

# cross tabulation of the animal types against the best number of clusters
maxc = 4
kc = kmeans(x, maxc)
table(animals$Species, kc$cluster)

# Question 3
install.packages("cclust")
library(cclust)

result.cc = c()
for (i in 2:10) {
  kmeans.cc = cclust(as.matrix(animals[, -3]), centers = i,
                          dist = "manhattan", method= "kmeans")
  result.cc = rbind(result.cc, c(i, sum(kmeans.cc$withinss)))
}

colnames(result.cc) = c("nclust", "totwss")
result.cc = data.frame(result.cc)

# combine two datasets
# result$package = NULL
# result.cc$package = NULL
# result.combined = rbind(result, result.cc)

# ?plot
options(scipen = 999)
plot(result$nclust, result$totwss, type="b", pch=19, col="red",
     xlab = "Number of clusters",
     ylab = "Total within sum of squares")
# add a line
lines(result.cc$nclust, result.cc$totwss, pch=18, col="blue",  type="b", lty=2)
# add a legend
legend(x = "topright", legend = c("kmeans", "cclust", "maximum curvature"),
       col = c("red", "blue", "darkgray"), lty = 1:2, cex = 0.8)
# add a vertical line
abline(v = 4, col = "darkgray")

options(scipen = 0)

?legend

view.plot<-function(){
  x<-1:10; y1=x*x; y2=2*y1
  plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y")
  lines(x, y2, pch=18, col="blue", type="b", lty=2)
}
