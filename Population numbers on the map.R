#rm(list=ls())
#Resource intensity

install.packages("png","calibrate","maps")

P = .2
setwd("~/Desktop")

#library(maps)
#map("world","canada",xlim=c(-125,-122),ylim=c(48,49.5))
# Vancouver is 49.2827° N, -123.1207° E
# data(canada.cities)
# BCcities = canada.cities[canada.cities[,"country.etc"]=="BC",]
# VanTowns = canada.cities[(canada.cities[,"lat"] < 49.4)&
#                            (canada.cities[,"long"] > -123.5)&
#                            (canada.cities[,"long"] < -122),]
#map.cities(VanTowns,label=FALSE)


PostalCentroids = read.csv(file="CA.txt",header=FALSE ,sep = "\t")
PostalCount     = read.csv(file="Postal Population.txt",header=TRUE ,sep = ",")

colnames(PostalCentroids) = c("Country","Postal","name","country.etc","prov","dunno6","dunno7","dunno8","dunno9","lat","long","measure")
PostalCentroids[,"capital"]=0

colnames(PostalCount) = c("Postal","Reserves","pop","dwellings")
head(PostalCount)
head(PostalCentroids)


#unique(PostalCentroids[,"dunno6"])

PostalCenters = PostalCentroids[,c("name","country.etc","Postal","lat","long","capital")]


VanPostal = PostalCenters[(PostalCenters[,"lat"] < 49.4)&
                            (PostalCenters[,"lat"] > 49.0)&
                            (PostalCenters[,"long"] > -123.5)&
                            (PostalCenters[,"long"] < -122.5),]

# now to combine the data sets and match them based on the postal code

Van.cities = merge(VanPostal, PostalCount, by = "Postal")
colnames(Van.cities)
# get rid of strangely small (and zero) values of population
Van.cities = Van.cities[Van.cities[,"pop"]>100,]

#map("world","canada",xlim=c(-126,-122),ylim=c(48,50))
#map.cities(Van.cities,label=F)




library(calibrate)
library(png)
#par(mfrow=c(2,2))
#Replace the directory and file information with your info
ima <- readPNG("googlemap_trim.png")

#Set up the plot area
plot(c(-123.265,-122.735), c(49,49.329),
     type='n', main="Population per First 3 Digits of Postal Code", xlab="longitude", ylab="latitude")
#Get the plot information so the image will
#fill the plot box, and draw it
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

rbPal <- colorRampPalette(c('blue','red'))
#points(Van.cities[,"long"],Van.cities[,"lat"],
#       #col = rbPal(10)[as.numeric(cut(log(Van.cities$pop),breaks = 10))],
#       pch='*',
#       cex=4)
textxy(Van.cities[,"long"],Van.cities[,"lat"],
       rpois(n=length(Van.cities[,"pop"]),log(Van.cities[,"pop"])*P),#round(rbinom(n=length(Van.cities[,"pop"]),size=log(Van.cities[,"pop"]),prob=P))
       cex=1.4,col=2)


# ME TODO:
# CONSOLIDATE THE POINTS?
# REMOVE SOME POINTS?
# DELETE ANMORE (7???)
#
# FIND A WAY TO OVERLAY MAPS 
# AND COUNT THE NUMBER OF 
# OVERLAYS
# HAVE EVERYONE PLACE 6 MARKERS 
# IN THE WATER WITH PARTIAL
# OVERLAPS.
# DEFINE THE TIME
# AND THE LAT/LONG FOR THOSE POINTS
# USE THEM TO COUNT
# BASED ON COLOUR. WHEN OVERLAYED
# 
####### USE GIMP SOFTWARE TO ALTER OPACITY AND STACK?


############### ACTIVITIES:
#
# STEP 1 FIND THE MINIMUM
# NUMBER OF POINTS TO 
# CATCH EVERYWHERE ON 
# THE MAP WITHIN 30 MINS
# NOTE THAT YOU CAN INPUT 
# THE LAT/LONG ON MAPNIFICENT
#
# STEP 2 LOOK AT THE VARIABILITY AND
#  SEE HOW MANY LOCATIONS YOU NEED
# FOR BEST CASE/WORST CASE
# MEDIAN CASE
# WHAT NUMBER SHOULD YOU TARGET?
#
