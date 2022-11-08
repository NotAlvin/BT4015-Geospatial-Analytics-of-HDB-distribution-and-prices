# BT4015
# Geographically Weighted Regression
# Importing Required Libraries
setwd("/Desktop/BT4015/Project")
library(rgdal)
library(maptools)
library(raster)
library(spatstat)
library(GISTools)
library(tmap)
library(sf)
library(oldtmaptools)
library(tmaptools)
library(spdep)

#####################################################################################
#                             Reading in Files                                      #
#####################################################################################

hdb <- readOGR(dsn = "HDB", layer = "hdb_shapefile")
market_hawker <- readOGR(dsn = "Market_Hawker", layer = "markethawker_shapefile")
schools <- readOGR(dsn = "Schools", layer = "schools_shapefile")
schools2 <- readOGR(dsn = "new schools", layer = "new_schools_shapefile")
singapore <- readOGR(dsn ="planning-area-census2010-shp", layer = "Planning_Area_Census2010")
singapore2 <- readOGR(dsn ="data", layer = "SGP_adm0")
busstop <- readOGR(dsn ="BusStopLocation_Aug2021", layer = "BusStop")
shopping <- readOGR(dsn ="Shopping Malls", layer = "shopping_malls_shapefile")
preschools <- readOGR(dsn ="pre-schools-location", layer = "pre-schools-location")
mrt <- readOGR(dsn ="master-plan-2019-rail-station-layer", layer = "master-plan-2019-rail-station-layer")

#Assigning same CRS and units to all datasets for consistency
P4S.latlon <- CRS("+init=epsg:3857 +units=m")
singapore <- spTransform(singapore, P4S.latlon)
singapore2 <- spTransform(singapore2, P4S.latlon)
hdb <- spTransform(hdb, P4S.latlon)
schools <- spTransform(schools, P4S.latlon)
schools2 <- spTransform(schools2, P4S.latlon)
market_hawker <- spTransform(market_hawker, P4S.latlon)
busstop <- spTransform(busstop, P4S.latlon)
preschools <- spTransform(preschools, P4S.latlon)
shopping <- spTransform(shopping, P4S.latlon)
mrt <- spTransform(mrt, P4S.latlon)

#Assigning number of each feature to each planning area
colSums(gContains(singapore, schools, byid = TRUE)) -> schools_by_area
colSums(gContains(singapore, hdb, byid = TRUE)) -> hdb_by_area
colSums(gContains(singapore, busstop, byid = TRUE)) -> busstop_by_area
colSums(gContains(singapore, shopping, byid = TRUE)) -> shopping_by_area
colSums(gContains(singapore, market_hawker, byid = TRUE)) -> market_hawker_by_area
colSums(gContains(singapore, preschools, byid = TRUE)) -> preschools_by_area
colSums(gContains(singapore, mrt, byid = TRUE)) -> mrt_by_area

#Assigning resale value of each area
singapore$resale_value = NULL
singapore$resale_value[singapore$PLN_AREA_N =="ANG MO KIO"] = 438000
singapore$resale_value[singapore$PLN_AREA_N =="BEDOK"] = 430000
singapore$resale_value[singapore$PLN_AREA_N =="BISHAN"] = 545000
singapore$resale_value[singapore$PLN_AREA_N =="BUKIT BATOK"] = 417500 
singapore$resale_value[singapore$PLN_AREA_N =="BUKIT MERAH"] = 760000
singapore$resale_value[singapore$PLN_AREA_N =="BUKIT PANJANG"] = 450000 
singapore$resale_value[singapore$PLN_AREA_N =="DOWNTOWN CORE"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="MARINA EAST"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="MARINA SOUTH"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="MUSEUM"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="NEWTON"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="ORCHARD"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="OUTRAM"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="RIVER VALLEY"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="ROCHOR"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="SINGAPORE RIVER"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="STRAITS VIEW"] = 941000
singapore$resale_value[singapore$PLN_AREA_N =="CHOA CHU KANG"] = 460000
singapore$resale_value[singapore$PLN_AREA_N =="CLEMENTI"] = 695000
singapore$resale_value[singapore$PLN_AREA_N =="GEYLANG"] = 565000 
singapore$resale_value[singapore$PLN_AREA_N =="HOUGANG"] = 435500
singapore$resale_value[singapore$PLN_AREA_N =="JURONG EAST"] = 429000 
singapore$resale_value[singapore$PLN_AREA_N =="JURONG WEST"] = 418000
singapore$resale_value[singapore$PLN_AREA_N =="KALLANG"] = 685000
singapore$resale_value[singapore$PLN_AREA_N =="PASIR RIS"] = 459000 
singapore$resale_value[singapore$PLN_AREA_N =="PUNGGOL"] = 488000
singapore$resale_value[singapore$PLN_AREA_N =="QUEENSTOWN"] = 800000 
singapore$resale_value[singapore$PLN_AREA_N =="SEMBAWANG"] = 410000
singapore$resale_value[singapore$PLN_AREA_N =="SENGKANG"] = 465000
singapore$resale_value[singapore$PLN_AREA_N =="SERANGOON"] = 465000
singapore$resale_value[singapore$PLN_AREA_N =="TAMPINES"] = 466000
singapore$resale_value[singapore$PLN_AREA_N =="TOA PAYOH"] = 655000
singapore$resale_value[singapore$PLN_AREA_N =="WOODLANDS"] = 406000
singapore$resale_value[singapore$PLN_AREA_N =="YISHUN"] = 426500
resale <- singapore$resale_value

#Indexing which areas have no associated resale value (To be dropped later)
price <- as.data.frame(na.omit(as.data.frame(singapore$resale_value)))
drop <- which(is.na(as.data.frame(singapore$resale_value)))

#Creating new SpatialPolygonDataFrames to store planning zones and how many of each feature are inside each zone
singapore_zones <- data.frame(resale, hdb_by_area,schools_by_area,busstop_by_area,shopping_by_area,market_hawker_by_area,preschools_by_area, mrt_by_area)
filtered_singapore_zones <- singapore_zones[-drop,]
singapore_polygons <- singapore@polygons
SPls <- SpatialPolygons(singapore_polygons)
filtered_singapore_polygons <- SPls[-drop,]

singapore_processed <- SpatialPolygonsDataFrame(SPls, singapore_zones)
singapore_processed_filtered <- SpatialPolygonsDataFrame(filtered_singapore_polygons, filtered_singapore_zones)

# Importing library for GWR model
library(spgwr)
# Calibrating the bandwidth for fitting the model 
bw = gwr.sel(resale ~ hdb_by_area + 
               schools_by_area + 
               busstop_by_area + 
               shopping_by_area + 
               market_hawker_by_area + 
               preschools_by_area +
               mrt_by_area, data=singapore_processed_filtered, adapt=T)

# Fitting the model
gwr.model = gwr(resale ~ hdb_by_area + 
                  schools_by_area + 
                  busstop_by_area + 
                  shopping_by_area + 
                  market_hawker_by_area + 
                  preschools_by_area + 
                  mrt_by_area, data=singapore_processed_filtered, adapt=bw)
# Model Results
gwr.model

# Calibrating the bandwidth for fitting the model 
bw2 = gwr.sel(hdb_by_area ~
               schools_by_area + 
               busstop_by_area + 
               shopping_by_area + 
               market_hawker_by_area + 
               preschools_by_area + 
               mrt_by_area, data=singapore_processed_filtered, adapt=T)

# Fitting the model
gwr2.model = gwr(hdb_by_area ~
                  schools_by_area + 
                  busstop_by_area + 
                  shopping_by_area + 
                  market_hawker_by_area + 
                  preschools_by_area + 
                  mrt_by_area , data=singapore_processed_filtered, adapt=bw2)
# Model Results
gwr2.model

# Calibrating the bandwidth for fitting the model 
bw3 = gwr.sel(hdb_by_area ~
                schools_by_area + 
                busstop_by_area + 
                shopping_by_area + 
                #market_hawker_by_area + 
                preschools_by_area + 
                mrt_by_area, data=singapore_processed_filtered, adapt=T)

# Fitting the model
gwr3.model = gwr(hdb_by_area ~
                   schools_by_area + 
                   busstop_by_area + 
                   shopping_by_area + 
                   #market_hawker_by_area + 
                   preschools_by_area + 
                   mrt_by_area , data=singapore_processed_filtered, adapt=bw2)
# Model Results
gwr3.model