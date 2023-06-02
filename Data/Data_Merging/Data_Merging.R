
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(raster)

Counties_Spatial <- st_read("../KenData/counties.shp")
ZIP_Spatial <- st_read("../KenData/zips.shp")
ZIP_valid <- subset(ZIP_Spatial, st_is_valid(ZIP_Spatial))


#Loading Data from Jerry
EF_vnf <- readRDS("../JerryData/ef_vnf/ef-vnf.rds")
PB_vnf <- readRDS("../JerryData/pb_vnf/pb-vnf.rds")
 

#Converting Coordinates (Long/Lat) in Jerry's Data both EF_vnf and PB_vnf
EF_vnf_coordinates <- st_as_sf(EF_vnf, coords = c("lon", "lat"), crs = 4326)
PB_vnf_coordinates <- st_as_sf(PB_vnf, coords = c("lon", "lat"), crs = 4326)

 
#Joining Counties and Jerry's Data (Permian Basin)
PB_vnf_counties_join <- st_join(PB_vnf_coordinates, Counties_Spatial)

#Joining ZIP and Jerry's Data(Permian Basin) with Counties from previous line
PB_final <- st_join(PB_vnf_coordinates, ZIP_valid)
testing <- PB_final %>%
  st_transform(crs = 4326) %>%  # Transform to WGS84 if needed
  st_coordinates() %>%
  as.data.frame() %>%
  setNames(c("long", "lat"))

#
PB_final<- PB_final %>%st_transform(crs = 4326) %>%  # Transform to WGS84 if needed
  mutate(
    long = st_coordinates(.)[, "X"],
    lat = st_coordinates(.)[, "Y"]
  )%>% as.data.frame() 

#Drop last column 
PB_final <- PB_final[, -16]








