require(rgdal)
library(sp)
library(tmap)
library(sf)
library(rgeos)
library(raster)
library(GISTools)

#Load datasets
mrt <- st_read("master-plan-2019-rail-station-layer/master-plan-2019-rail-station-layer.shp")
zones <- st_read("planning-area-census2010-shp/Planning_Area_Census2010.shp")
bus <- st_read("BusStopLocation_Aug2021/BusStop.shp")
school <-  st_read("Schools/schools_shapefile.shp") # Point Data
market_hawker <- st_read("Market_Hawker/markethawker_shapefile.shp") # Point Data
hdb <- st_read("HDB/hdb_shapefile.shp") # Point Data
preschool <-st_read("pre-schools-location/pre-schools-location.shp") # Point Data
shopping_mall <- st_read("Shopping Malls/shopping_malls_shapefile.shp") # Point Data
qtm(shopping_mall)

#Change datum
zones <- spTransform(zones, CRS("+proj=utm +datum=WGS84 +units=m"))
mrt_sp <- as_Spatial(st_zm(mrt))
mrt_sp <- spTransform(test, CRS("+proj=utm +datum=WGS84"))
bus_utm <- spTransform(bus, CRS("+proj=utm +datum=WGS84"))
school_utm <- spTransform(school, CRS("+proj=utm +datum=WGS84"))
market_utm <- spTransform(market_hawker, CRS("+proj=utm +datum=WGS84"))
hdb_utm <- spTransform(hdb, CRS("+proj=utm +datum=WGS84"))
preschool_utm <- spTransform(preschool, CRS("+proj=utm +datum=WGS84"))
shopping_mall_utm <- spTransform(shopping_mall, CRS("+proj=utm +datum=WGS84"))

#Visualize hdb on planning areas
tm_shape(zones) + tm_borders(col='black') + tm_fill() +
  tm_shape(hdb_utm) + tm_dots()
#Add hdb.no of each zone
zones$hdb_no = poly.counts(hdb_utm,zones)
tm_shape(zones) + tm_fill(col='hdb_no') +tm_borders(col='white')

#Create buffer of 1 km for mrt
mrt_buf <- gBuffer(mrt_sp, width = 1000)
mrt_buf <- spTransform(mrt_buf, CRS("+proj=utm +datum=WGS84"))
#Plotting for mrt buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(mrt_sp) + tm_polygons() +
  tm_shape(mrt_buf) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#count of hdb that falls within mrt buffer zone
poly.counts(hdb_utm, mrt_buf)
#plot hdb within each zone that fulfill mrt buffer
hdb_within_mrt <- intersect(hdb_utm,mrt_buf)
zones$hdb_mrt_buf <- poly.counts(hdb_within_mrt,zones)
zones$hdb_mrt_fraction <- zones$hdb_mrt_buf/zones$hdb_no
tm_shape(zones) + tm_fill(col='hdb_mrt_fraction', title='Fraction of HDBs that fall within MRT buffer') +tm_borders(col='black') 

#Create buffer of 500m for bus
bus_buf <- gBuffer(bus_utm, width = 500)
bus_buf <- spTransform(mrt_buf, CRS("+proj=utm +datum=WGS84"))
bus_buf <- intersect(bus_buf,zones)
#Plotting for bus buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(bus_utm) + tm_dots() +
  tm_shape(bus_buf) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#count of hdb that falls within bus buffer zone
poly.counts(hdb_utm, bus_buf)
#plot hdb within each zone that fulfill bus buffer
hdb_within_bus <- intersect(hdb_utm,bus_buf)
zones$hdb_bus_buf <- poly.counts(hdb_within_bus,zones)
zones$hdb_bus_fraction <- zones$hdb_bus_buf/zones$hdb_no
tm_shape(zones) + tm_fill(col='hdb_bus_fraction', title='Fraction of HDBs that fall within bus buffer') +tm_borders(col='black') 

#Create buffer of 2km for school
school_buf <- gBuffer(school_utm, width = 2000)
school_buf <- spTransform(school_buf, CRS("+proj=utm +datum=WGS84"))
#Plotting for school buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(school_utm) + tm_dots() +
  tm_shape(school_buf) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#count hdb within each zone that fulfill school buffer
poly.counts(hdb_utm, school_buf)
#plot hdb within each zone that fulfill school buffer
hdb_within_school <- intersect(hdb_utm,school_buf)
zones$hdb_school_buf <- poly.counts(hdb_within_school,zones)
zones$hdb_school_fraction <- zones$hdb_school_buf/zones$hdb_no
tm_shape(zones) + tm_fill(col='hdb_school_fraction', title='Fraction of HDBs that fall within school buffer') +tm_borders(col='black') 

#Create buffer of 2km for hawker-market
market_buf <- gBuffer(market_utm, width = 2000)
market_buf <- spTransform(market_buf, CRS("+proj=utm +datum=WGS84"))
#Plotting for hawker-market buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(market_utm) + tm_dots() +
  tm_shape(market_buf) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#count hdb within each zone that fulfill hawker-market buffer
poly.counts(hdb_utm, market_buf)
#plot hdb within each zone that fulfill pre-school buffer
hdb_within_market <- intersect(hdb_utm,market_buf)
zones$hdb_market_buf <- poly.counts(hdb_within_market,zones)
zones$hdb_market_fraction <- zones$hdb_market_buf/zones$hdb_no
tm_shape(zones) + tm_fill(col='hdb_market_fraction', title='Fraction of HDBs that fall within market-hawker buffer') +tm_borders(col='black') 

#Create buffer of 2km for preschool
preschool_buf <- gBuffer(preschool_utm, width = 2000)
preschool_buf <- spTransform(preschool_buf,  CRS("+proj=utm +datum=WGS84"))
#Plotting for preschool buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(preschool_utm) + tm_dots() +
  tm_shape(preschool_buf) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#count hdb within each zone that fulfill pre-school buffer
poly.counts(hdb_utm,preschool_buf)
#plot hdb within each zone that fulfill pre-school buffer
hdb_within_preschool <- intersect(hdb_utm,preschool_buf)
zones$hdb_preschool_buf <- poly.counts(hdb_within_preschool,zones)
zones$hdb_preschool_fraction <- zones$hdb_preschool_buf/zones$hdb_no
tm_shape(zones) + tm_fill(col='hdb_preschool_fraction', title='Fraction of HDBs that fall within pre-school buffer') +tm_borders(col='black') 

#Create buffer of 5km for shopping malls
shopping_mall_buf <- gBuffer(shopping_mall_utm, width = 5000)
shopping_mall_buf <- spTransform(shopping_mall_buf,  CRS("+proj=utm +datum=WGS84"))
#Plotting for shopping malls buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(shopping_mall_utm) + tm_dots() +
  tm_shape(shopping_mall_buf) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#count hdb within each zone that fulfill shopping mall buffer
poly.counts(hdb_utm,shopping_mall_buf)
#plot hdb within each zone that fulfill shopping mall buffer
hdb_within_shopping_mall <- intersect(hdb_utm,shopping_mall_buf)
zones$hdb_shopping_mall_buf <- poly.counts(hdb_within_shopping_mall,zones)
zones$hdb_shopping_mall_fraction <- zones$hdb_shopping_mall_buf/zones$hdb_no
tm_shape(zones) + tm_fill(col='hdb_shopping_mall_fraction', title='Fraction of HDBs that fall within shopping mall buffer') +tm_borders(col='black') 

#intersection of different buffers
test1 <- intersect(mrt_buf,bus_buf)
schl_bus_mrt <- intersect(test1,school_buf)
schl_bus_mrt_market <- intersect(test1,school_buf2)
schl_bus_mrt_market_preschl <- intersect(schl_bus_mrt_market,preschool_buf)
schl_bus_mrt_market_preschl_shopping_mall <- intersect(schl_bus_mrt_market_preschl, shopping_mall_buf)
schl_bus_mrt_market1 <- intersect(zones,schl_bus_mrt_market_preschl_shopping_mall)
#plot combined buffer
tm_shape(zones) + tm_borders(col='black') + tm_shape(shopping_mall_utm) + tm_dots() +
  tm_shape(schl_bus_mrt_market1) + tm_polygons(col='orange', alpha=0.5) +
  tm_shape(hdb) + tm_dots()
#Count number of HDB that fall within combined buffer
poly.counts(hdb_utm, schl_bus_mrt_market_preschl_shopping_mall)

#Add hdb that matches criteria to each zone
hdb_meet_criteria <- intersect(hdb_utm, schl_bus_mrt_market1)
zones$hdb_no_ctieria <- poly.counts(hdb_meet_criteria,zones)
tm_shape(zones) + tm_fill(col='hdb_no_ctieria') +tm_borders(col='white')
#Find fraction of hdb that meets the criteria in a particular zone
zones$hdb_percent_critera = zones$hdb_no_ctieria/(zones$hdb_no)
tm_shape(zones) + tm_fill(col='hdb_percent_critera', title='Fraction of HDBs that fall combined buffer') +tm_borders(col='black')

#Add resale value of HDBs
zones$resale_value = NULL
zones$resale_value[zones$PLN_AREA_N =="ANG MO KIO"] = 438000
zones$resale_value[zones$PLN_AREA_N =="BEDOK"] = 430000
zones$resale_value[zones$PLN_AREA_N =="BISHAN"] = 545000
zones$resale_value[zones$PLN_AREA_N =="BUKIT BATOK"] = 417500 
zones$resale_value[zones$PLN_AREA_N =="BUKIT MERAH"] = 760000
zones$resale_value[zones$PLN_AREA_N =="BUKIT PANJANG"] = 450000 
zones$resale_value[zones$PLN_AREA_N =="DOWNTOWN CORE"] = 941000
zones$resale_value[zones$PLN_AREA_N =="MARINA EAST"] = 941000
zones$resale_value[zones$PLN_AREA_N =="MARINA SOUTH"] = 941000
zones$resale_value[zones$PLN_AREA_N =="MUSEUM"] = 941000
zones$resale_value[zones$PLN_AREA_N =="NEWTON"] = 941000
zones$resale_value[zones$PLN_AREA_N =="ORCHARD"] = 941000
zones$resale_value[zones$PLN_AREA_N =="OUTRAM"] = 941000
zones$resale_value[zones$PLN_AREA_N =="RIVER VALLEY"] = 941000
zones$resale_value[zones$PLN_AREA_N =="ROCHOR"] = 941000
zones$resale_value[zones$PLN_AREA_N =="SINGAPORE RIVER"] = 941000
zones$resale_value[zones$PLN_AREA_N =="STRAITS VIEW"] = 941000
zones$resale_value[zones$PLN_AREA_N =="CHOA CHU KANG"] = 460000
zones$resale_value[zones$PLN_AREA_N =="CLEMENTI"] = 695000
zones$resale_value[zones$PLN_AREA_N =="GEYLANG"] = 565000 
zones$resale_value[zones$PLN_AREA_N =="HOUGANG"] = 435500
zones$resale_value[zones$PLN_AREA_N =="JURONG EAST"] = 429000 
zones$resale_value[zones$PLN_AREA_N =="JURONG WEST"] = 418000
zones$resale_value[zones$PLN_AREA_N =="KALLANG"] = 685000
zones$resale_value[zones$PLN_AREA_N =="PASIR RIS"] = 459000 
zones$resale_value[zones$PLN_AREA_N =="PUNGGOL"] = 488000
zones$resale_value[zones$PLN_AREA_N =="QUEENSTOWN"] = 800000 
zones$resale_value[zones$PLN_AREA_N =="SEMBAWANG"] = 410000
zones$resale_value[zones$PLN_AREA_N =="SENGKANG"] = 465000
zones$resale_value[zones$PLN_AREA_N =="SERANGOON"] = 465000
zones$resale_value[zones$PLN_AREA_N =="TAMPINES"] = 466000
zones$resale_value[zones$PLN_AREA_N =="TOA PAYOH"] = 655000
zones$resale_value[zones$PLN_AREA_N =="WOODLANDS"] = 406000
zones$resale_value[zones$PLN_AREA_N =="YISHUN"] = 426500

#Visualize resale value by planing area
tm_shape(zones) + tm_polygons(col='resale_value') +tm_borders(col='black')
#Visualize resale value by planing area with HDB and name of planning area
tm_shape(zones) + tm_polygons(col='resale_value') +tm_borders(col='white')  +
  tm_shape(hdb) + tm_dots() +
  tm_shape(zones) +tm_text('PLN_AREA_N')

#Best value for amnesties
zones$value <- zones$hdb_percent_critera / zones$resale_value
tm_shape(zones) + tm_polygons(col='value') +tm_borders(col='white') 