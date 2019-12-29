# install.packages("gstat")
library(gstat)
library(lattice) # this has xyplot
library(sp) #This has the meuse data
data(meuse)
str(meuse)

xyplot(y ~ x, data = meuse)
coplot(zinc ~ x | y, data = meuse)
coplot(zinc ~ y | x, data = meuse)

#Box and whisker plot of zinc
bwplot(~zinc, data = meuse)
#Histogram of zinc
histogram(~zinc, data = meuse)
#Cumulative distribution function of zinc
cdf.zinc <- ecdf(meuse$zinc)
plot(cdf.zinc, verticals = T, do.points = F)

#QQ-plot of zinc
qqnorm(meuse$zinc)
qqline(meuse$zinc)

# install.packages("geoR")
library(geoR, warn = F)
meuse.geo <- as.geodata(meuse, data.col = 6) #zinc is the 6th column
plot(meuse.geo)
points.geodata(meuse.geo)

coordinates(meuse) = ~x+y
bubble(meuse, "zinc", col="blue", main = "zinc concentrations (ppm)")
bubble(meuse, "zinc", col=c("#00ff0088"), main = "zinc concentrations (ppm)")

# model the log of zinc
# student's t with 1 and 10 degree of freedom
qqmath(~log(meuse$zinc), distribution = function(p) qt(p, df = 1),
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       },xlab="Quantiles of the t-distribution, df=1",ylab="Logged zinc concentrations")

qqmath(~log(meuse$zinc), distribution = function(p) qt(p, df = 10),
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       },xlab="Quantiles of the t-distribution, df=10",ylab="Logged zinc concentrations")

# http://www.rpubs.com/liem/63374
# explore the meuse.grid dataset
data(meuse.grid)
summary(meuse.grid)
str(meuse.grid)
class(meuse.grid)

# "SpatialPointsDataFrame"
coordinates(meuse.grid) = ~x+y
class(meuse.grid)

# See how the class of meuse.grid changes when "gridded" is used
# "SpatialPixelsDataFrame"
gridded(meuse.grid) = TRUE
class(meuse.grid)

image(meuse.grid["dist"])
title("distance to river (red = 0)")

# Now we need package gstat
library(gstat)
# Apply the inverse distance weighted interpolation
# idw = inverse distance weighted
zinc.idw = idw(zinc~1, meuse, meuse.grid)
class(zinc.idw)
# plot
spplot(zinc.idw["var1.pred"], main = "zinc inverse distance weighted interpolations")

# Variogram
vgm1 = variogram(log(zinc)~1, meuse)
# plot variogram cloud
plot(variogram(log(zinc)~1, meuse, cloud=TRUE))
# plot vgm1
plot(vgm1)

# Fit a theoretical variogram
vgm1.fit = fit.variogram(vgm1, model = vgm(1, "Sph", 900, 1))
vgm1.fit
# plot the fitted variogram and the observed variogram on the same graph
plot(vgm1, vgm1.fit)

# a mean function for the variogram
vgm2 = variogram(log(zinc)~dist, meuse)
vgm2.fit = fit.variogram(vgm2, model = vgm(1, "Exp", 300, 1))
vgm2.fit
plot(vgm2, vgm2.fit)

# Kriging
vgm1.kriged = krige(log(zinc)~1, meuse, meuse.grid, model = vgm1.fit)
spplot(vgm1.kriged["var1.pred"])

# Directional variogram
vgm3 = variogram(log(zinc)~1, meuse, alpha = c(0, 45, 90, 135))
vgm3.fit = fit.variogram(vgm3, model = vgm(.59, "Sph", 1200, .05, anis = c(45, .4)))
vgm3.fit
plot(vgm3, vgm3.fit, as.table = TRUE)

# Another directional variogram
vgm4 = variogram(log(zinc)~dist, meuse, alpha = c(0, 45, 90, 135))
vgm4.fit = fit.variogram(vgm4, model = vgm(.59, "Sph", 1200, .05, anis = c(45, .4)))
vgm4.fit
plot(vgm4, vgm4.fit, as.table = TRUE)

# Blocking Kriging
vgm5<- variogram(log(zinc)~1, meuse)
vgm5.fit <- fit.variogram(vgm5, model=vgm(psill=1, model="Sph", range=900, nugget=1))
plot(vgm5, vgm5.fit)

lzn.okriging <- krige(log(zinc)~1, meuse, meuse.grid, model = vgm5.fit)
spplot(lzn.okriging["var1.pred"], main = "ordinary kriging predictions")
spplot(lzn.okriging["var1.var"],  main = "ordinary kriging variance")

#Define a block size for block kriging 
lzn.bokriging <- krige(log(zinc)~1, meuse, meuse.grid, model = vgm5.fit, block=c(500,500))
spplot(lzn.bokriging["var1.pred"], main = "ordinary block kriging predictions")
spplot(lzn.bokriging["var1.var"],  main = "ordinary block kriging variance")

# Indicator Kriging
#Develop a variogram for indicator kriging 
vgm6 <- variogram(I(log(zinc)>6)~1, meuse)
vgm6.fit <- fit.variogram(vgm6, model=vgm(psill=0.25, model="Sph", range=900, nugget=0.1))
plot(vgm6, vgm6.fit)

logzinc.ikriging <- krige(I(log(zinc)>6.21)~1, meuse, meuse.grid, vgm6.fit)
spplot(logzinc.ikriging["var1.pred"], main = "indicator kriging predictions")
spplot(logzinc.ikriging["var1.var"],  main = "indicator kriging variance")

# Co-Kriging
# Build a gstat structure containing the two sample sets
# zinc + cadmium
zinccadmium <- gstat(NULL, id = "logzinc", form = log(zinc) ~ 1, data=meuse)
zinccadmium <- gstat(zinccadmium, id = "logcadmium", form = log(cadmium) ~ 1, data=meuse)

# Compute and display the two direct variograms and one ???cross-variogram
v.cross <- variogram(zinccadmium)
str(v.cross)
plot(v.cross, pl=T)

# Add variogram models to the gstat object and fit a them using the linear model of co-regionalisation
zinccadmium <- gstat(zinccadmium, id = "logzinc", model = vgm5.fit, fill.all=T)
zinccadmium

# use the fit.lmc method to fit all three variograms together
zinccadmium <- fit.lmc(v.cross, zinccadmium)
plot(variogram(zinccadmium), model=zinccadmium$model)

# use the predict.gstat method for co-kriging
cokrig.model <- predict(zinccadmium, meuse.grid)
str(cokrig.model)
summary(cokrig.model)
summary(cokrig.model$logcadmium.pred)
summary(cokrig.model$logcadmium.var)

# Display the predictions and their errors for log(zinc) and log(cadmium)
spplot(cokrig.model["logzinc.pred"], main = "co kriging predictions")
spplot(cokrig.model["logzinc.var"],  main = "co kriging variance")
spplot(cokrig.model["logcadmium.pred"], main = "co kriging predictions")
spplot(cokrig.model["logcadmium.var"],  main = "co kriging variance")

# Indicator Co-Kriging
# # Create a categorical variable of zinc (grouped by quartile)
quartzinc <- quantile(meuse$zinc)
quartzinc
quartzinc[2]

# create 3 separate datasets, one for each quartile.
# Example, one dataset will be 1-0 whether a zinc value is below the 25th percentile.
# Another dataset will be 1-0 whether a zinc value is below the median mark.
zinc.quart <- gstat(NULL, id = "zinc1stquart", formula = I(zinc < quartzinc[2]) ~ 1, data = meuse, 
                    nmax = 10, beta = 0.25, set = list(order = 4, zero = 1e-05))
zinc.quart <- gstat(zinc.quart, "zinc2ndquart", formula = I(zinc < quartzinc[3]) ~ 1, data = meuse,
                    nmax = 10, beta = 0.50)
zinc.quart <- gstat(zinc.quart, "zinc3rdquart", formula = I(zinc < quartzinc[4]) ~ 1, data = meuse,
                    nmax = 10, beta = 0.75)

# Create an initial semivariogram model with range equal 1200
zinc.quart <- gstat(zinc.quart, model = vgm(1, "Sph", 1000, 1) , fill.all = T)
class(zinc.quart)

# Estimate the empiricalvariogram of each indicator
zincqvag <- variogram(zinc.quart)
summary(zincqvag)
plot(zincqvag)

# fit a single semivariogram model to all these empirical varioagrams using a linear model of coregionalization
# (the fit.lmc() function)
zinc.quartfit = fit.lmc(zincqvag, zinc.quart)
plot(zincqvag, model = zinc.quartfit)
# rm(zinc.quartfit)

summary(zinc.quartfit)
zinccadmium$model
zinc.quartfit$model

# now do the co-kriging
# library(raster)
cokrig.zincquart <- predict(zinc.quartfit, meuse.grid, debug.level = 0)
str(cokrig.zincquart)
# summary(cokrig.zincquart)
# summary(cokrig.zincquart$logcadmium.pred)
# summary(cokrig.zincquart$logcadmium.var)

# plot results
spplot(cokrig.zincquart["zinc1stquart.pred"], main = "co kriging predictions")
spplot(cokrig.zincquart["zinc1stquart.var"],  main = "co kriging variance")

library(gridExtra)
grid.arrange(spplot(cokrig.zincquart["zinc1stquart.pred"], main = "co kriging predictions"),
             spplot(cokrig.zincquart["zinc2ndquart.pred"], main = "co kriging predictions"),
             ncol=2)