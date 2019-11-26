getwd()
setwd("C:/Users/whoro/Documents/01-Exeter/Term1/MTHM501/assignment/3")
animals = read.csv("Animals.csv")

# install.packages("cclust")
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
# view dendrogram using ggplot
ggdendrogram(hc, labels = FALSE) +
  ggtitle("HC Dendrogram") +
  theme(axis.text.x = element_text(size = 10, family = "Segoe UI"),
        axis.text.y = element_text(size = 10, family = "Segoe UI"),
        plot.title = element_text(size = 15, family = "Segoe UI",
                                  hjust = 0.5))

# cut the dendrogram to give 4 clusters
Cluster = cutree(hc,4)
# cross tabulation of 4 cluster cut vs. Species
ctab = table(animals$Species, Cluster)
# view the result
ctab

# Qestion 2
# Assign features (column 1 & 2) to variable x, and the Species to variable y
x = animals[,-3]
y = animals$Species

# summary of each cluster size (2 - 10) and tot.withinss
result.2 = c()
for (i in 2:10) {
  kc = kmeans(x, i)
  result.2 = rbind(result.2, c(i, kc$tot.withinss))
}

colnames(result.2) = c("nclust", "totwss")
result.2 = data.frame(result.2)

# fit a smoothed spline (with 4 degrees of freedom) to "result.2"
dfree = 4
out.spl = with(result.2, smooth.spline(nclust, totwss, df = dfree))
# calculate the second derivative of the smoothed curve
derivative.out = with(result.2, predict(out.spl, x = nclust, deriv = 2))
# find the number of clusters with the maximum second derivative
derivative.out = as.data.frame(derivative.out, derivative.out$x, derivative.out$y)
maximum.curvature = derivative.out[which.max(derivative.out$y),]$x
maximum.curvature

# plot of cluster size vs. tot.withinss
options(scipen = 999)

spline.df = as.data.frame(spline(result.2$nclust, result.2$totwss))
result.2$derivative = derivative.out$y

theme = theme(axis.title.x = element_text(colour = "black", size = 10, family = "Segoe UI"),
              axis.title.y = element_text(colour = "black", size = 10, family = "Segoe UI"),
              axis.text.x = element_text(size = 10, family = "Segoe UI"),
              axis.text.y = element_text(size = 10, family = "Segoe UI"),
              legend.title = element_text(size = 10, family = "Segoe UI"),
              legend.text = element_text(size = 8, family = "Segoe UI"),
              strip.text = element_text(size = 8, family = "Segoe UI"),
              plot.title = element_text(colour = "darkgrey",size = 15,family = "Segoe UI",
                                        face = "italic",hjust = 0.5))

ggplot(result.2, aes(x = nclust, y = totwss)) +
  geom_point(aes(colour = derivative), size = 2) +
  geom_line(data = spline.df, aes(x = x, y = y)) +
  geom_vline(xintercept = 4, linetype = "dashed",
             colour = "blue", size = 0.6) +
  xlab("Number of clusters") +
  ylab("Total within sum of squares") +
  theme

options(scipen = 0)

# cross tabulation of the animal types against the best number of clusters
kc = kmeans(x, maximum.curvature)
table(animals$Species, kc$cluster)

# Question 3
# install.packages("cclust")
library(cclust)

result.3 = c()
for (i in 2:10) {
  kmeans.3 = cclust(as.matrix(animals[, -3]), centers = i,
                    dist = "manhattan", method= "kmeans")
  result.3 = rbind(result.3, c(i, sum(kmeans.3$withinss)))
}

colnames(result.3) = c("nclust", "totwss")
result.3 = data.frame(result.3)

# plot both result.2 and result.3 (use simple plot)
options(scipen = 999)

plot(result.2$nclust, result.2$totwss,
     type="b", pch=19, col="red", frame = FALSE,
     xlab = "Number of clusters",
     ylab = "Total within sum of squares")
# add a line
lines(result.3$nclust, result.3$totwss,
      pch = 18, col = "blue", type = "b", lty = 2)
# add a vertical line
abline(v = 4, col = "darkgray")
# add a legend
legend(x = "topright", legend = c("Kmeans", "Cclust", "Point of maximum curvature"),
       col = c("red", "blue", "darkgray"), lty = 1:2, cex = 0.8)

options(scipen = 0)
