
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

#Joining Counties and Jerry's Data(Eagle Ford)
EF_vnf_coordinates <- st_join(EF_vnf_coordinates, Counties_Spatial)

#Joining ZIP and Jerry's Data(Permian Basin) with Counties 
PB_final <- st_join(PB_vnf_coordinates, ZIP_valid)

#Joining ZIP and Jerry's Data(Permian Basin) with Counties 
EF_final <- st_join(EF_vnf_coordinates, ZIP_valid)

#Permian Basin
PB_final<- PB_final %>%st_transform(crs = 4326) %>% 
  mutate(
    long = st_coordinates(.)[, "X"],
    lat = st_coordinates(.)[, "Y"]
  )%>% as.data.frame() 

#Drop last column (EF)
PB_final <- PB_final[, -16]


#Eagle Ford
EF_final<- EF_final %>%st_transform(crs = 4326) %>% 
  mutate(
    long = st_coordinates(.)[, "X"],
    lat = st_coordinates(.)[, "Y"]
  )%>% as.data.frame() 

#Drop last column  (EF)
EF_final <- EF_final[, -21]


#Selecting only columns of interest
PB_final <- PB_final %>% dplyr::select(vnf_id, date, long, lat, zip, state, basin, county) 
EF_final <- EF_final %>% dplyr::select(vnf_id, date, county.y, zip, basin, long, lat, state.y)%>%rename(state = state.y, county = county.y)

#Removing NA
PB_final <- na.omit(PB_final)
EF_final <- na.omit(EF_final)

#Combining the Data
Final_Data <- rbind(PB_final, EF_final)

#Writing the file
write_rds(Final_Data, "VNF_data_final_2012-2022.rds")






