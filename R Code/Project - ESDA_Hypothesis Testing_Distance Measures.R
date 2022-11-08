# BT4015 Project
# Exploratory Spatial Data Analysis (ESDA), Hypothesis Testing and L&G Function
setwd("Desktop/BT4015/Project")

library(tmap)
library(sf)
library(sp)
library(plyr)
library(oldtmaptools)
library(GISTools)
library(maptools)
library(raster)
library(dplyr)

#######################################################################################
#                                Reading in Files                                     #
#######################################################################################
PlanningArea <- st_read("planning-area-census2010-shp/Planning_Area_Census2010.shp")
tm_shape(PlanningArea) + tm_polygons()

BusStops <- st_read("BusStopLocation_Aug2021/BusStop.shp") # Point Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(BusStops) + tm_dots()

MRT_Exits <- st_read("master-plan-2019-rail-station-layer/master-plan-2019-rail-station-layer.shp") # Polygon Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(MRT_Exits) + tm_polygons()

PreSchool <- st_read("pre-schools-location/pre-schools-location.shp") # Point Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(PreSchool) + tm_dots()

Schools <- st_read("Schools/schools_shapefile.shp") # Point Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(Schools) + tm_dots()

Market_Hawker <- st_read("Market_Hawker/markethawker_shapefile.shp") # Point Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(Market_Hawker) + tm_dots()

HDB <- st_read("HDB/hdb_shapefile.shp") # Point Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(HDB) + tm_dots()

Shopping <- st_read("Shopping Malls/shopping_malls_shapefile.shp") # Point Data
tm_shape(PlanningArea) + tm_polygons() + tm_shape(Shopping) + tm_dots()

#######################################################################################
#                                 Chloropeth Map                                      #
#######################################################################################
# Bus-Stops - Frequency
BusStopsAreas = st_join(PlanningArea, BusStops, by = "geometry")

detach(package:dplyr, TRUE)
StopsCount = count(BusStopsAreas, "PLN_AREA_N")
CombinedFrequency = merge(PlanningArea, StopsCount)

tmap_mode('plot')
tm_shape(CombinedFrequency) + tm_polygons("freq", title = "Number of Bus-Stops") # Number of Bus-Stops

# MRT Exits
library(dplyr)
MRT_Exits <- st_transform(MRT_Exits, crs = crs(PlanningArea))

# Need to remove duplicate MRT Exits, NULL Values and clean up data
MRT_Exits <- MRT_Exits %>% distinct(Name, .keep_all = TRUE)
MRT_Exits <- MRT_Exits[!(MRT_Exits$Name %in% c("<Null>")), ]

MRT_ExitsAreas = st_join(PlanningArea, MRT_Exits, by = "geometry")
MRT_ExitsAreas = MRT_ExitsAreas[!is.na(MRT_ExitsAreas$Name),]

detach(package:dplyr, TRUE)
MRTCount = count(MRT_ExitsAreas, "PLN_AREA_N")
MRTFrequency = merge(PlanningArea, MRTCount, all.x = TRUE)
MRTFrequency[is.na(MRTFrequency)] = 0

tmap_mode('plot')
tm_shape(MRTFrequency) + tm_polygons("freq", title = "Number of MRT Stations", breaks = c(0, 1, 5, 10, 15, 20)) # Number of MRT/LRTs

# Preschool
library(dplyr)
PreSchool <- st_transform(PreSchool, crs = crs(PlanningArea))

PreSchoolAreas = st_join(PlanningArea, PreSchool, by = "geometry")
PreSchoolAreas = PreSchoolAreas[!is.na(PreSchoolAreas$Name),]

detach(package:dplyr, TRUE)
PreSchoolCount = count(PreSchoolAreas, "PLN_AREA_N")
PreSchoolFrequency = merge(PlanningArea, PreSchoolCount, all.x = TRUE)
PreSchoolFrequency[is.na(PreSchoolFrequency)] = 0

tmap_mode('plot')
tm_shape(PreSchoolFrequency) + tm_polygons("freq", title = "Number of PreSchools") # Number of PreSchools

# Schools
Schools <- st_transform(Schools, crs = crs(PlanningArea))
SchoolAreas = st_join(PlanningArea, Schools, by = "geometry")
#SchoolAreas = na.omit(SchoolAreas) # Check again - need to omit NA in sch names only, check for bus stops too
School_Count = count(SchoolAreas, "PLN_AREA_N") # Number of Carparksin each Planning Areas
CombinedSchool = merge(PlanningArea, School_Count, all.x = TRUE)
CombinedSchool[is.na(CombinedSchool)] = 0

tmap_mode('plot')
tm_shape(CombinedSchool) + tm_polygons("freq", title = "Number of Schools") # Number of Schools

# Market_Hawker
Market_Hawker <- st_transform(Market_Hawker, crs = crs(PlanningArea))
MarketAreas = st_join(PlanningArea, Market_Hawker, by = "geometry")
MarketAreas = na.omit(MarketAreas)
Market_Hawker_Count = count(MarketAreas, "PLN_AREA_N") # Number of Market and Hawkers in each Planning Areas
CombinedMarket = merge(PlanningArea, Market_Hawker_Count, all.x = TRUE)
CombinedMarket[is.na(CombinedMarket)] = 0

tmap_mode('plot')
tm_shape(PlanningArea) + tm_polygons() + 
tm_shape(CombinedMarket) + tm_polygons("freq", title = "Number of Market and Hawker", breaks = c(0, 1, 3, 5, 7, 9, 11, 13, 14)) # Number of Market and Hawker

# Shopping
Shopping <- st_transform(Shopping, crs = crs(PlanningArea))
ShoppingAreas = st_join(PlanningArea, Shopping, by = "geometry")
ShoppingAreas = na.omit(ShoppingAreas)
Shopping_Count = count(ShoppingAreas, "PLN_AREA_N") # Number of Carparksin each Planning Areas
CombinedShopping = merge(PlanningArea, Shopping_Count, all.x = TRUE)
CombinedShopping[is.na(CombinedShopping)] = 0

tmap_mode('plot')
tm_shape(CombinedShopping) + tm_polygons("freq", title = "Number of Shopping Malls", breaks = c(0, 1, 2, 4, 6, 8, 10, 15, 20)) # Number of Shopping Malls

# Population Density 
populationSize = read.csv("estimated-resident-population-in-hdb-flats-by-town.csv", header = TRUE)
latest = aggregate(populationSize[, c('financial_year', 'population')], list(populationSize$town_or_estate), tail, 1)
latest = latest[latest$Group.1 != 'Total',]

names(latest)[names(latest) == 'Group.1'] <- 'PLN_AREA_N'
latest[,1] = toupper(latest[,1])
densityCombined = merge(PlanningArea, latest)

tmap_mode('plot')
tm_shape(PlanningArea) + tm_polygons() + 
tm_shape(densityCombined) + tm_polygons("population", title = "Population") # Population Size

densityCombined$Density = densityCombined$population/densityCombined$SHAPE_Area
tm_shape(PlanningArea) + tm_polygons() + 
  tm_shape(densityCombined) + tm_polygons("Density", title = "Density") # Density

#######################################################################################
#                                Hypothesis Testing                                   #
#######################################################################################
HDB <- st_transform(HDB, crs = crs(PlanningArea))
hdb  <- as.ppp(st_geometry(HDB))
marks(hdb) <- NULL
hdb.km <- rescale(hdb, 1000)
ann.p <- mean(nndist(hdb.km, k=1)) # Average Nearest Neighbour Distance = 0.05km

# Hypothesis: CRS
n <- 250L
ann.r <- vector(length = n)
for (i in 1:n){
  rand.p <- rpoint(n=hdb.km$n, win=PlanArea.km)
  ann.r[i] <- mean(nndist(rand.p, k=1))
}
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5), size = 0.001)

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p 

# Null Hypothesis
PPM0 <- ppm(hdb.km ~ 1)
PPM0

# Hypothesis: Bus-Stops
library(spatstat)
PlanArea  <- as.owin(PlanningArea)
PlanArea.km <- rescale(PlanArea, 1000, "km") # Rescale to metres to kilometres

busstop  <- as.ppp(st_geometry(BusStops))
marks(busstop) <- NULL
busstop <- rescale(busstop, 1000)
Window(busstop) <- PlanArea.km

library(geostatsp)
busstop.img <- as.im(busstop)

n2 <- 250L
ann.r2 <- vector(length=n2)
for (i in 1:n2){
  rand.p2 <- rpoint(n=hdb.km$n, f=busstop.img)
  ann.r2[i] <- mean(nndist(rand.p2, k=1))
}
Window(rand.p2) <- PlanArea.km
plot(rand.p2, pch=16, main=NULL, cols=rgb(0,0,0,0.5), size = 0.001)

hist(ann.r2, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r2))
abline(v=ann.p, col="blue")

N.greater2 <- sum(ann.r2 > ann.p)
p2 <- min(N.greater2 + 1, n2 + 1 - N.greater2) / (n2 +1)
p2 

# Alternative Hypothesis
PPM1 <- ppm(hdb.km ~ busstop.img)
PPM1

anova(PPM0, PPM1, test="LRT")

# Hypothesis: MRT Stations
MRT_Exits_Point <- st_cast(MRT_Exits, "MULTIPOINT")
MRT_LRT  <- as.ppp(st_geometry(MRT_Exits_Point))
marks(MRT_LRT) <- NULL
MRT_LRT <- rescale(MRT_LRT, 1000)
Window(MRT_LRT) <- PlanArea.km

library(geostatsp)
MRT_LRT.img <- as.im(MRT_LRT)

n3 <- 250L
ann.r3 <- vector(length=n3)
for (i in 1:n3){
  rand.p3 <- rpoint(n=hdb.km$n, f=MRT_LRT.img)
  ann.r3[i] <- mean(nndist(rand.p3, k=1))
}
Window(rand.p3) <- PlanArea.km
plot(rand.p3, pch=16, main=NULL, cols=rgb(0,0,0,0.5), size = 0.001)

hist(ann.r3, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r3))
abline(v=ann.p, col="blue")

N.greater3 <- sum(ann.r3 > ann.p)
p3 <- min(N.greater3 + 1, n3 + 1 - N.greater3) / (n3 +1)
p3 

PPM2 <- ppm(hdb.km ~ MRT_LRT.img)
PPM2

anova(PPM0, PPM2, test="LRT")

# Hypothesis: Market and Hawker
markethawker  <- as.ppp(st_geometry(Market_Hawker))
marks(markethawker) <- NULL
markethawker <- rescale(markethawker, 1000)
Window(markethawker) <- PlanArea.km

library(geostatsp)
markethawker.img <- as.im(markethawker)

n4 <- 250L
ann.r4 <- vector(length=n4)
for (i in 1:n4){
  rand.p4 <- rpoint(n=hdb.km$n, f=markethawker.img)
  ann.r4[i] <- mean(nndist(rand.p4, k=1))
}
Window(rand.p4) <- PlanArea.km
plot(rand.p4, pch=16, main=NULL, cols=rgb(0,0,0,0.5), size = 0.01)

hist(ann.r4, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r4))
abline(v=ann.p, col="blue")

N.greater4 <- sum(ann.r4 > ann.p)
p4 <- min(N.greater4 + 1, n4 + 1 - N.greater4) / (n4 +1)
p4 

PPM3 <- ppm(hdb.km ~ markethawker.img)
PPM3

anova(PPM0, PPM3, test="LRT")

# Hypothesis: Preschools
preschool  <- as.ppp(st_geometry(PreSchool))
marks(preschool) <- NULL
preschool <- rescale(preschool, 1000)
Window(preschool) <- PlanArea.km

library(geostatsp)
preschool.img <- as.im(preschool)

n5 <- 250L
ann.r5 <- vector(length=n5)
for (i in 1:n5){
  rand.p5 <- rpoint(n=hdb.km$n, f=preschool.img)
  ann.r5[i] <- mean(nndist(rand.p5, k=1))
}
Window(rand.p5) <- PlanArea.km
plot(rand.p5, pch=16, main=NULL, cols=rgb(0,0,0,0.5), size = 0.01)

hist(ann.r5, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r5))
abline(v=ann.p, col="blue")

N.greater5 <- sum(ann.r5 > ann.p)
p5 <- min(N.greater5 + 1, n5 + 1 - N.greater5) / (n5 +1)
p5 

PPM4 <- ppm(hdb.km ~ preschool.img)
PPM4

anova(PPM0, PPM4, test="LRT")

# Hypothesis: Schools
schools  <- as.ppp(st_geometry(Schools))
marks(schools) <- NULL
schools <- rescale(schools, 1000)
Window(schools) <- PlanArea.km

library(geostatsp)
schools.img <- as.im(schools)

n6 <- 250L
ann.r6 <- vector(length=n6)
for (i in 1:n6){
  rand.p6 <- rpoint(n=hdb.km$n, f=schools.img)
  ann.r6[i] <- mean(nndist(rand.p6, k=1))
}
Window(rand.p6) <- PlanArea.km
plot(rand.p6, pch=16, main=NULL, cols=rgb(0,0,0,0.5), size = 0.01)

hist(ann.r6, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r6))
abline(v=ann.p, col="blue")

N.greater6 <- sum(ann.r6 > ann.p)
p6 <- min(N.greater6 + 1, n6 + 1 - N.greater6) / (n6 +1)
p6 

PPM5 <- ppm(hdb.km ~ schools.img)
PPM5

anova(PPM0, PPM5, test="LRT")

# Hypothesis: Shopping
shopping  <- as.ppp(st_geometry(Shopping))
marks(shopping) <- NULL
shopping <- rescale(shopping, 1000)
Window(shopping) <- PlanArea.km

library(geostatsp)
shopping.img <- as.im(shopping)

n7 <- 250L
ann.r7 <- vector(length=n7)
for (i in 1:n7){
  rand.p7 <- rpoint(n=hdb.km$n, f=shopping.img)
  ann.r7[i] <- mean(nndist(rand.p7, k=1))
}
Window(rand.p7) <- PlanArea.km
plot(rand.p7, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r7, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r7))
abline(v=ann.p, col="blue")

N.greater7 <- sum(ann.r7 > ann.p)
p7 <- min(N.greater7 + 1, n7 + 1 - N.greater7) / (n7 +1)
p7 

PPM6 <- ppm(hdb.km ~ shopping.img)
PPM6

anova(PPM0, PPM6, test="LRT")

# Poisson Process Model - Continued
PPM7 <- ppm(hdb.km ~ busstop.img + MRT_LRT.img)
anova(PPM1,  PPM7, test = "LRT")

PPM8 <- ppm(hdb.km ~ busstop.img + MRT_LRT.img + preschool.img)
anova(PPM7, PPM8, test = "LRT")

PPM9 <- ppm(hdb.km ~ busstop.img + MRT_LRT.img + preschool.img + schools.img)
anova(PPM8, PPM9, test = "LRT")

PPM10 <- ppm(hdb.km ~ busstop.img + MRT_LRT.img + preschool.img + schools.img + markethawker.img)
anova(PPM9, PPM10, test = "LRT")

PPM11 <- ppm(hdb.km ~ busstop.img + MRT_LRT.img + preschool.img + schools.img + markethawker.img + shopping.img)
anova(PPM10, PPM11, test = "LRT")

PPM12 <- ppm(hdb.km ~ busstop.img + preschool.img + schools.img + markethawker.img + shopping.img)
anova(PPM11, PPM12, test = "LRT")

#######################################################################################
#                                L & G Function Plots                                 #
#######################################################################################
# HDB
library(spatstat)
lf.env <- envelope(hdb.km,Lest,correction="border")
plot(lf.env)

gf.env <- envelope(hdb.km,Gest,correction="border")
plot(gf.env)

# Bus Stop
library(spatstat)
lf.env <- envelope(busstop,Lest,correction="border")
plot(lf.env)

gf.env <- envelope(busstop,Gest,correction="border")
plot(gf.env)

# School
library(spatstat)
lf.env <- envelope(schools,Lest,correction="border")
plot(lf.env)

gf.env <- envelope(schools,Gest,correction="border")
plot(gf.env)

# Market & Hawker
library(spatstat)
lf.env <- envelope(markethawker,Lest,correction="border")
plot(lf.env)

gf.env <- envelope(markethawker,Gest,correction="border")
plot(gf.env)

# Shopping Malls
library(spatstat)
lf.env <- envelope(shopping,Lest,correction="border")
plot(lf.env)

gf.env <- envelope(shopping,Gest,correction="border")
# Plot it
plot(gf.env)