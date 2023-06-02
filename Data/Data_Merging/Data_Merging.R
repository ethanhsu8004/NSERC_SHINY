
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(raster)
#Loading Data from Henry
# file <- file.choose()
# file_testing <- st_read(file)

Counties_Spatial <- st_read("../HenryData/counties.shp")
ZIP_Spatial <- st_read("../HenryData/zips.shp")
ZIP_valid <- subset(ZIP_Spatial, st_is_valid(ZIP_Spatial))


#Loading Data from Jerry
EF_vnf <- readRDS("../JerryData/ef_vnf/ef-vnf.rds")
PB_vnf <- readRDS("../JerryData/pb_vnf/pb-vnf.rds")

#duplicating column before converting
PB_vnf$longitude <- PB_vnf$lon
PB_vnf$latitude <- PB_vnf$lat

#Converting Coordinates (Long/Lat) in Jerry's Data both EF_vnf and PB_vnf
EF_vnf_coordinates <- st_as_sf(EF_vnf, coords = c("lon", "lat"), crs = 4326)
PB_vnf_coordinates <- st_as_sf(PB_vnf, coords = c("lon", "lat"), crs = 4326)
 
#Joining Counties and Jerry's Data (Permian Basin)
PB_vnf_counties_join <- st_join(Counties_Spatial, PB_vnf_coordinates)

#Joining ZIP and Jerry's Data(Permian Basin)
PB_vnf_zip_join <- st_join(ZIP_valid, PB_vnf_coordinates)


#converting it to "normal data frame" so that its possible to drop last column
PB_vnf_counties_join <- PB_vnf_counties_join %>% as.data.frame()
PB_vnf_zip_join <- PB_vnf_zip_join %>% as.data.frame()


#dropping last column
#Dropping SF objects in Counties and Zip
PB_vnf_counties_join <- PB_vnf_counties_join[,-19]
PB_vnf_zip_join <- PB_vnf_zip_join[,-18]


final_data <- cbind(PB_vnf_counties_join, PB_vnf_zip_join$latitude)













