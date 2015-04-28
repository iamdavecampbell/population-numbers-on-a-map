# This was used for the SD-36 (Surrey) High School Event Gifted Program at SFU April 28, 2015
# The code enters the Statistics Canada census population counts per first three digits of postal code
# Then the code loads the centers of the postal code regions.
#
# Finally the code simulates Earthquake resource levels needed from a Poisson Distribution for the different postal code regions.
# The resources needed are assumed to be linear with the squareroot of numbers of individuals
# The logic is that people are inherrently good and will help each other and that
# resources distribution centres will be made larger and larger as more need arises
#
#
#  The resulting model for resources needed Y is
#  Y~Poisson[ P*sqrt(population in postal code region) ]
#  for some constant P defined below.
#
#  Then the resources can be allocated on a map based on making sure that there are
#  enough resource centres within a specified time (distance) of the resource need.
#  This was be approximated by placing pins on mapnificent.net/vancouver and
#  overlap can be assessed by overlaying screenshots of pin clouds in photoshop.
#
#
#  April 28, 2015
#  @iamdavecampbell
#  www.stat.sfu.ca/~dac5
#
#

install.packages("png")
install.packages("calibrate")
install.packages("maps")

P = 1
setwd("~/Desktop/population numbers on a map")



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

Nsamp = 10000
ResourceNeeds =  matrix(rpois(n=Nsamp*length(Van.cities[,"pop"]),sqrt(Van.cities[,"pop"])*P),dim(Van.cities)[1],Nsamp)


# Now let's plot the simulated resources needed and overlay it on a map png image
library(calibrate)
library(png)
ima <- readPNG("googlemap_trim.png")

#Set up the plot area in terms of Latitude and Longitude:
plot(c(-123.265,-122.735), c(49,49.329),
     type='n', main="Resources needed per First 3 Digits of Postal Code Region - Best Case", xlab="longitude", ylab="latitude")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
rbPal <- colorRampPalette(c('blue','red'))
textxy(Van.cities[,"long"],Van.cities[,"lat"],       apply(ResourceNeeds,1,min),
       cex=1.4,col=2)


plot(c(-123.265,-122.735), c(49,49.329),
type='n', main="Resources needed per First 3 Digits of Postal Code Region - Worst Case", xlab="longitude", ylab="latitude")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
rbPal <- colorRampPalette(c('blue','red'))
textxy(Van.cities[,"long"],Van.cities[,"lat"],       apply(ResourceNeeds,1,max),
cex=1.4,col=2)




plot(c(-123.265,-122.735), c(49,49.329),
type='n', main="Resources needed per First 3 Digits of Postal Code Region - Median Case", xlab="longitude", ylab="latitude")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
rbPal <- colorRampPalette(c('blue','red'))
textxy(Van.cities[,"long"],Van.cities[,"lat"],       apply(ResourceNeeds,1,median),
cex=1.4,col=2)






plot(c(-123.265,-122.735), c(49,49.329),
type='n', main="Resources needed per First 3 Digits of Postal Code Region - One Simulation", xlab="longitude", ylab="latitude")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
rbPal <- colorRampPalette(c('blue','red'))
textxy(Van.cities[,"long"],Van.cities[,"lat"],   rpois(n=length(Van.cities[,"pop"]),sqrt(Van.cities[,"pop"])*P),
cex=1.4,col=2)
