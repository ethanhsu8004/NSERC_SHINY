
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(raster)

Counties_Spatial <- st_read("../KenData/counties.shp")
ZIP_Spatial <- st_read("../KenData/zips.shp")
ZIP_valid <- st_make_valid(ZIP_Spatial)

# ZIP_valid <- subset(ZIP_Spatial, st_is_valid(ZIP_Spatial))


# #------------------------------------------------------IGNORE------------------------------------#
#Data did not involve cluster so ignore everything commented out.

# 
# 
#Loading Data from Jerry
# EF_vnf <- readRDS("../JerryData/ef_vnf/ef-vnf.rds")
# PB_vnf <- readRDS("../JerryData/pb_vnf/pb-vnf.rds")
# 
# 
# #Converting Coordinates (Long/Lat) in Jerry's Data both EF_vnf and PB_vnf
# EF_vnf_coordinates <- st_as_sf(EF_vnf, coords = c("lon", "lat"), crs = 4326)
# PB_vnf_coordinates <- st_as_sf(PB_vnf, coords = c("lon", "lat"), crs = 4326)
# 
# 
# #Joining Counties and Jerry's Data (Permian Basin)
# PB_vnf_counties_join <- st_join(PB_vnf_coordinates, Counties_Spatial)
# 
# #Joining Counties and Jerry's Data(Eagle Ford)
# EF_vnf_coordinates <- st_join(EF_vnf_coordinates, Counties_Spatial)
# 
# #Joining ZIP and Jerry's Data(Permian Basin) with Counties
# PB_final <- st_join(PB_vnf_coordinates, ZIP_valid)
# 
# #Joining ZIP and Jerry's Data(Permian Basin) with Counties
# EF_final <- st_join(EF_vnf_coordinates, ZIP_valid)

# #Permian Basin
# PB_final<- PB_final %>%st_transform(crs = 4326) %>% 
#   mutate(
#     long = st_coordinates(.)[, "X"],
#     lat = st_coordinates(.)[, "Y"]
#   )%>% as.data.frame() 
# 
# #Drop last column (EF)
# PB_final <- PB_final[, -16]
# 
# 
# #Eagle Ford
# EF_final<- EF_final %>%st_transform(crs = 4326) %>% 
#   mutate(
#     long = st_coordinates(.)[, "X"],
#     lat = st_coordinates(.)[, "Y"]
#   )%>% as.data.frame() 
# 
# #Drop last column  (EF)
# EF_final <- EF_final[, -21]
# 
# 
# #Filtering out temperatures
# 
# PB_final <- PB_final %>% filter(temp_bb > 1600)
# EF_final <- EF_final %>% filter(temp_bb > 1600)
# 
# #Selecting only columns of interest
# PB_final <- PB_final %>% dplyr::select(vnf_id, date, long, lat, zip, state, basin, county) 
# EF_final <- EF_final %>% dplyr::select(vnf_id, date, county.y, zip, basin, long, lat, state.y)%>%rename(state = state.y, county = county.y)
# 
# #Removing NA
# PB_final <- na.omit(PB_final)
# EF_final <- na.omit(EF_final)
# 
# 
# #Combining the Data
# Final_Data <- rbind(PB_final, EF_final)
# 
# #Writing the file
# write_rds(EF_final, "EF_final.rds")
# write_rds(PB_final, "PB_final.rds")
# write_rds(Final_Data, "VNF_data_final_2012-2022.rds")
# 

#------------------------------------------------------UPDATE------------------------------------#
#Loading Ken's Data and joining it with Data_final_2016_2022
Ken_2012_2020 <- readRDS("../KenData/vnf-2012-2020.rds")  

#filtering out everything after 2016-01-03 inclusive
Ken_2012_2016 <- Ken_2012_2020 %>% filter(!(date>= ("2016-01-03")), !(basin == "WILLISTON"))
Ken_2012_2016$lat <- Ken_2012_2016$vnf_lat
Ken_2012_2016$long <- Ken_2012_2016$vnf_lon
Ken_2012_2016_coord <- st_as_sf(Ken_2012_2016, coords = c("long", "lat"), crs = 4326)
Ken_2012_2016_zip_intersection <- st_join(Ken_2012_2016_coord, ZIP_valid) #adding the zip codes spatially joining
Ken_2012_2016_zip_intersection$basin.y <- str_to_title(Ken_2012_2016_zip_intersection$basin.x)

#Filtering out NA
Ken_2012_2016_non_na <-  Ken_2012_2016_zip_intersection %>% filter(!is.na(Ken_2012_2016_zip_intersection$stusps)) %>%as.data.frame()
#Updating State so that its based off the stusps column
Ken_2012_2016_non_na$state <- ifelse(Ken_2012_2016_non_na$stusps == "NM", "New Mexico", "Texas")
#updating column county so that it combines the county.x and stusps columns
Ken_2012_2016_non_na$county.y <- paste(str_to_title(Ken_2012_2016_non_na$county.x), toupper(Ken_2012_2016_non_na$stusps), sep =", ")


Ken_2012_2016_non_na <- Ken_2012_2016_non_na[,-ncol(Ken_2012_2016_non_na)] %>% rename(lat = "vnf_lat", long = "vnf_lon", county = "county.y", 
                                                                                      basin = "basin.x")
Ken_2012_2016_FINAL <-  Ken_2012_2016_non_na %>% dplyr::select("vnf_id", "date", "long", "lat", "zip", "state", "basin", "county")

#After receiving Jerry's Updated Data
Data_2016_2022 <- readRDS("../JerryData/flare_full.rds")
Data_2016_2022$long <- Data_2016_2022$vnf_lon
Data_2016_2022$lat <- Data_2016_2022$vnf_lat
Data_coord_2016_2022 <-  st_as_sf(Data_2016_2022, coords = c("long", "lat"), crs = 4326)
Data_join_2016_2022 <- st_join(Data_coord_2016_2022, Counties_Spatial)
Data_join_2016_2022 <- st_join(Data_join_2016_2022, ZIP_valid)


#Filtering out temperatures and noise
Data_filter_2016_2022 <- Data_join_2016_2022 %>% filter(temp_bb > 1600, cluster != 0) %>% filter(!(is.na(stusps)))
Data_filter_2016_2022$state.y<- str_to_title(Data_filter_2016_2022$state.x)
Data_filter_2016_2022$county.y <-  paste(str_to_title(Data_filter_2016_2022$county.x), toupper(Data_filter_2016_2022$stusps), sep =", ")

#Filtering out NA
# Data_final_2016_2022 <- na.omit(Data_filter_2016_2022)


Data_final_2016_2022 <- Data_filter_2016_2022 %>%st_transform(crs = 4326) %>% as.data.frame() 

Data_final_2016_2022$basin <-  Ken_2012_2016_non_na$basin.y[match(Data_final_2016_2022$county.y,Ken_2012_2016_non_na$county.y)]
Data_final_2016_2022 <- Data_final_2016_2022 %>% rename(long = "vnf_lon", lat = "vnf_lat", county = "county.y", state = "state.y")
Data_final_2016_2022 <- Data_final_2016_2022 %>% dplyr::select("vnf_id", "date", "long", "lat", "zip", "state", "basin", "county")


#Loading 2022(Sept) - 2023
Ethan_2022_2023 <- readRDS("VNF_2022-2023.rds")
Ethan_2022_2023$vnf_lat <- Ethan_2022_2023$lat
Ethan_2022_2023$vnf_lon <- Ethan_2022_2023$lon

Ethan_2022_2023_spatial <-  st_as_sf(Ethan_2022_2023, coords = c("vnf_lon", "vnf_lat"), crs = 4326)
Ethan_2022_2023_zip_intersection <- st_join(Ethan_2022_2023_spatial, ZIP_valid)
Ethan_2022_2023_non_NA <- na.omit(Ethan_2022_2023_zip_intersection) %>% as.data.frame() %>% rename(long = "lon", basin = "basin.x")
Ethan_2022_2023_FINAL <- Ethan_2022_2023_non_NA %>% dplyr::select("vnf_id", "date", "long", "lat", "zip", "state", "basin", "county")


#MERGE ALL THE ABOVE DATA INTO ONE DATAFRAME
Final_Data_2012_2023 <- rbind(Data_final_2016_2022, Ken_2012_2016_FINAL, Ethan_2022_2023_FINAL)
saveRDS(Final_Data_2012_2023, "VNF_data_final_2012-2023.rds")