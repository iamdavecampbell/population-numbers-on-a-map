#rm(list=ls())
#Resource intensity

install.packages("png","calibrate","maps")

P = .2
setwd("~/Desktop")



# Get census count data and the centers of the postal code regions
PostalCentroids = read.csv(file="CA.txt",header=FALSE ,sep = "\t")
PostalCount     = read.csv(file="Postal Population.txt",header=TRUE ,sep = ",")
colnames(PostalCentroids) = c("Country","Postal","name","country.etc","prov","dunno6","dunno7","dunno8","dunno9","lat","long","measure")
PostalCentroids[,"capital"]=0
colnames(PostalCount) = c("Postal","Reserves","pop","dwellings")
head(PostalCount)
head(PostalCentroids)


PostalCenters = PostalCentroids[,c("name","country.etc","Postal","lat","long","capital")]



# Specifically extract those in the GVRD
VanPostal = PostalCenters[(PostalCenters[,"lat"] < 49.4)&
                            (PostalCenters[,"lat"] > 49.0)&
                            (PostalCenters[,"long"] > -123.5)&
                            (PostalCenters[,"long"] < -122.5),]


# now to combine the data sets and match them based on the postal code
Van.cities = merge(VanPostal, PostalCount, by = "Postal")
colnames(Van.cities)
# get rid of strangely small (and zero) values of population
Van.cities = Van.cities[Van.cities[,"pop"]>100,]



# Now let's plot the simulated resources needed and overlay it on a map png image
library(calibrate)
library(png)
ima <- readPNG("googlemap_trim.png")

#Set up the plot area in terms of Latitude and Longitude:
plot(c(-123.265,-122.735), c(49,49.329),
     type='n', main="Population per First 3 Digits of Postal Code", xlab="longitude", ylab="latitude")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
rbPal <- colorRampPalette(c('blue','red'))
textxy(Van.cities[,"long"],Van.cities[,"lat"],       rpois(n=length(Van.cities[,"pop"]),log(Van.cities[,"pop"])*P),
       cex=1.4,col=2)

