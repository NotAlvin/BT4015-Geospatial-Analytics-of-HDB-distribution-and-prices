#Importing Required Libraries
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

#Plotting all points on map of Singapore to check if there are outliers
tm_shape(singapore) + tm_fill(col="grey", alpha = 0.3) + tm_borders(col = "black") +
  #tm_shape(hdb) + tm_dots(col= "blue") +
  tm_shape(schools2) + tm_dots(col = "green") +
  tm_shape(market_hawker) + tm_dots(col = "red") + 
  tm_shape(busstop) + tm_dots(col = "orange") +
  tm_shape(preschools) + tm_dots(col = "purple") + 
  tm_shape(shopping) + tm_dots(col = "blue")

#Function to choose bandwidth according to Bowman and Azzalini / Scott's rule
#Used for smooth_map function in order to generate density plots
choose_bw <- function(spdf) {
  X <- coordinates(spdf)
  sigma <- c(sd(X[,1]),sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6)
  return(sigma/1000)
}

#Plotting HDB Density plot
tmap_mode('plot')
hdb_dens <- smooth_map(hdb,cover=singapore2, bandwidth = choose_bw(hdb))
tm_shape(hdb_dens$raster) + tm_raster()

#Plotting school Density plot
school_dens <- smooth_map(schools, cover=singapore2, bandwidth = choose_bw(schools))
tm_shape(school_dens$raster) + tm_raster()  

#Plotting Pre-school Density plot
preschools_dens <- smooth_map(preschools,cover=singapore2, bandwidth = choose_bw(preschools))
tm_shape(preschools_dens$raster) + tm_raster() 

#Plotting bus stop Density plot
busstop_dens <- smooth_map(busstop,cover=singapore2, bandwidth = choose_bw(busstop))
tm_shape(busstop_dens$raster) + tm_raster() 

#Plotting shopping mall Density plot
shopping_dens <- smooth_map(shopping,cover=singapore2, bandwidth = choose_bw(shopping))
tm_shape(shopping_dens$raster) + tm_raster() 

#Plotting market and hawker Density plot
markethawker_dens <- smooth_map(market_hawker,cover=singapore2, bandwidth = choose_bw(market_hawker))
tm_shape(markethawker_dens$raster) + tm_raster()

#Loading mean yearly temperature dataset
temperature <- readOGR(dsn ="Shapefiles/Weather", layer = "weather")
temperature <- spTransform(weather, P4S.latlon)

#Plotting the individual weather stations temperature values
tm_shape(singapore2) + tm_polygons() +
  tm_shape(temperature) +
  tm_dots(col="temperatur", palette = "RdBu", stretch.palette = FALSE,
          title="Temperature (in Celcius)", size=0.3) +
  tm_text("temperatur", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

#Creating dirichlet tessellation layer for temperature values based on weather stations temperature values
th  <-  as(dirichlet(as.ppp(temperature)), "SpatialPolygons")
proj4string(th) <- proj4string(temperature)
th.z     <- over(th, temperature, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
th.clp   <- raster::intersect(singapore2,th.spdf)

#Plotting the polygons of nearest temperature values
tm_shape(th.clp) +
  tm_polygons(col="temperatur", palette="RdBu", stretch.palette =FALSE,
              title="Temperature (In Celcius)") +
  tm_legend(legend.outside=TRUE)

#Creating interpolated values for temperature layer based on weather stations temperature values
grd <- as.data.frame(spsample(temperature, "regular", n=50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE
fullgrid(grd) <- TRUE
crs(grd) <- crs(temperature)
P.idw <- gstat::idw(temperatur ~ 1, temperature, newdata=grd, idp=2.0)
r <- raster(P.idw)
r.m <- mask(r, singapore2)

#Plotting the interpolated temperature values
tm_shape(r.m) +
  tm_raster(n=10,palette = "RdBu", stretch.palette = FALSE,
            title="Temperature (In Celcius)") +
  tm_shape(temperature) + tm_dots(size=0.01) +
  tm_legend(legend.outside=TRUE)

#Labelling Zones with resale values of 4-room HDB for the 2nd quarter of 2021 
#Sourced from https://data.gov.sg/dataset/median-resale-prices-for-registered-applications-by-town-and-flat-type
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

#Assigning zone prices to central point of each zone
centroids <- coordinates(singapore)
drop <- which(is.na(as.data.frame(singapore$resale_value)))
filtered_centroids <- centroids[-drop,]
price <- as.data.frame(na.omit(as.data.frame(singapore$resale_value)))
filtered_singapore <- SpatialPointsDataFrame(filtered_centroids, price)
crs(filtered_singapore) <-  P4S.latlon

#Creating interpolated values for price layer based on zone price values
grd <- as.data.frame(spsample(filtered_singapore, "regular", n=50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE
fullgrid(grd) <- TRUE
crs(grd) <- crs(filtered_singapore)
P.idw <- gstat::idw(na.omit(singapore$resale_value) ~ 1, filtered_singapore, newdata=grd, idp=2.0)
r <- raster(P.idw)
r.m <- mask(r, singapore)

#Plotting the interpolated resale prices
tmap_mode('view')
tm_shape(r.m) +
  tm_raster(n=10,palette = "RdBu", stretch.palette = FALSE,
            title="Resale Prices ($)") +
  tm_shape(filtered_singapore) + tm_dots(size=0.01) +
  tm_legend(legend.outside=TRUE)

#Importing and processing population size in different areas data
populationSize = read.csv("estimated-resident-population-in-hdb-flats-by-town.csv", header = TRUE)
latest = aggregate(populationSize[, c('financial_year', 'population')], list(populationSize$town_or_estate), tail, 1)
latest = latest[latest$Group.1 != 'Total',]
names(latest)[names(latest) == 'Group.1'] <- 'PLN_AREA_N'
latest[,1] = toupper(latest[,1])
densityCombined = merge(singapore, latest)

#Plotting both population size and density
tmap_mode('plot')
tm_shape(singapore) + tm_polygons() + 
  tm_shape(densityCombined) + tm_polygons("population", title = "Population") # Population Size

densityCombined$Density = densityCombined$population/densityCombined$SHAPE_Area
tm_shape(singapore) + tm_polygons() + 
  tm_shape(densityCombined) + tm_polygons("Density", title = "Density") # Density

#Processing Singapore zone polygons to get neighbors list from polygon list using Queens method
singapore_q <- poly2nb(singapore, queen=FALSE)
#Processing Singapore zone neighbors list with spatial weights
singapore.lw <- nb2listw(singapore_q,style="W", zero.policy = TRUE)
#Assigning areas with no population data to 0 for the purposes of conducting Moran's test
densityCombined$Density[is.na(densityCombined$Density)] <- 0

#Conducting Moran's test for spatial autocorrelation on the 
moran <- moran.test(densityCombined$Density, listw=singapore.lw,randomisation=FALSE, zero.policy = TRUE)

#Monte Carlo simulation of Moran's I for population density
moranmc <- moran.mc(densityCombined$Density, listw=singapore.lw, zero.policy = TRUE,10000)