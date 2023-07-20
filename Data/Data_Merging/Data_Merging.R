
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
# #Loading Ken's Data and joining it with Data_final_2016_2022
# Ken_2012_2020 <- readRDS("../KenData/vnf-2012-2020.rds")
# 
# #filtering out everything after 2016-01-03 inclusive
# Ken_2012_2016 <- Ken_2012_2020 %>% filter(!(date>= ("2016-01-03")), !(basin == "WILLISTON"))
# Ken_2012_2016$lat <- Ken_2012_2016$vnf_lat
# Ken_2012_2016$long <- Ken_2012_2016$vnf_lon
# Ken_2012_2016_coord <- st_as_sf(Ken_2012_2016, coords = c("long", "lat"), crs = 4326)
# Ken_2012_2016_zip_intersection <- st_join(Ken_2012_2016_coord, ZIP_valid) #adding the zip codes spatially joining
# Ken_2012_2016_zip_intersection$basin.y <- str_to_title(Ken_2012_2016_zip_intersection$basin.x)
# 
# #Filtering out NA
# Ken_2012_2016_non_na <-  Ken_2012_2016_zip_intersection %>% filter(!is.na(Ken_2012_2016_zip_intersection$stusps)) %>%as.data.frame()
# #Updating State so that its based off the stusps column
# Ken_2012_2016_non_na$state <- ifelse(Ken_2012_2016_non_na$stusps == "NM", "New Mexico", "Texas")
# #updating column county so that it combines the county.x and stusps columns
# Ken_2012_2016_non_na$county.y <- paste(str_to_title(Ken_2012_2016_non_na$county.x), toupper(Ken_2012_2016_non_na$stusps), sep =", ")
# 
# 
# Ken_2012_2016_non_na <- Ken_2012_2016_non_na[,-ncol(Ken_2012_2016_non_na)] %>% rename(lat = "vnf_lat", long = "vnf_lon", county = "county.y",
#                                                                                       basin = "basin.x")
# Ken_2012_2016_FINAL <-  Ken_2012_2016_non_na %>% dplyr::select("vnf_id", "date", "long", "lat", "zip", "state", "basin", "county")

#------------------------------------------------------NO NEED TO FILTER BY WELLS (SECOND UPDATE)--------------------------------#

#Flaring Data for 2012-2016 (Done by Ethan)
Flaring_2012_2016 <- readRDS("UpdatedData/Flaring_Data_2012-2016.rds")
Flaring_2012_2016 <- Flaring_2012_2016 %>% filter(temp_bb > 1600, value != 0, cloud_mask == 0)
Flaring_2012_2016_coord <- st_as_sf(Flaring_2012_2016)

Flaring_join_2012_2016_county <- st_join(Flaring_2012_2016_coord, Counties_Spatial) %>% as.data.frame()
Flaring_join_2012_2016_county_non_NA <- Flaring_join_2012_2016_county[!is.na(Flaring_join_2012_2016_county$county), ]#need to filter it separately from ZIP

Flaring_join_2012_2016_zip <- st_join(Flaring_2012_2016_coord, ZIP_valid) %>% as.data.frame()
Flaring_join_2012_2016_zip_non_NA  <- Flaring_join_2012_2016_zip[!is.na(Flaring_join_2012_2016_zip$county), ]

Flaring_2012_2016_merged <- merge(Flaring_join_2012_2016_county_non_NA ,Flaring_join_2012_2016_zip_non_NA  , by = "vnf_id", all = TRUE)

Flaring_2012_2016_merged <- Flaring_2012_2016_merged  %>% dplyr::select("vnf_id", "date.x", "lon.x", "lat.x", "zip", "state.x", "stusps", "basin" ,"county.x") #selecting relevant columns
Flaring_2012_2016_merged$state.x <- str_to_title(Flaring_2012_2016_merged$state.x) #formating to only make first letter capitalized
Flaring_2012_2016_merged$county <- paste(str_to_title(Flaring_2012_2016_merged$county.x), toupper(Flaring_2012_2016_merged$stusps), sep =", ") #creating new column for county based off other columns
Flaring_2012_2016_FINAL <- Flaring_2012_2016_merged %>% dplyr::select(-"county.x", -"stusps") %>% rename(date = "date.x", state = "state.x", long = "lon.x", lat = "lat.x")


#After receiving Jerry's Updated Data (ONLY PB)
Data_2016_2022_PB <- readRDS("../JerryData/flare_full.rds")
Data_2016_2022_PB $long <- Data_2016_2022_PB $vnf_lon
Data_2016_2022_PB $lat <- Data_2016_2022_PB $vnf_lat
Data_coord_PB_2016_2022 <-  st_as_sf(Data_2016_2022_PB , coords = c("long", "lat"), crs = 4326)
Data_join_PB_2016_2022 <- st_join(Data_coord_PB_2016_2022, Counties_Spatial)
Data_join_PB_2016_2022 <- st_join(Data_join_PB_2016_2022, ZIP_valid)
#Filtering out temperatures and noise
Data_filter_PB_2016_2022 <- Data_join_PB_2016_2022%>% filter(temp_bb > 1600, cluster != 0) %>% filter(!(is.na(stusps)))
Data_filter_PB_2016_2022$state.y<- str_to_title(Data_filter_PB_2016_2022$state.x)
Data_filter_PB_2016_2022$county.y <-  paste(str_to_title(Data_filter_PB_2016_2022$county.x), toupper(Data_filter_PB_2016_2022$stusps), sep =", ")

#Filtering out NA
# Data_final_2016_2022 <- na.omit(Data_filter_2016_2022)

Data_final_PB_2016_2022 <- Data_filter_PB_2016_2022 %>%st_transform(crs = 4326) %>% as.data.frame()
Data_final_PB_2016_2022$basin <- "Permian"

Data_final_PB_2016_2022 <- Data_final_PB_2016_2022 %>% rename(long = "vnf_lon", lat = "vnf_lat", county = "county.y", state = "state.y")
Data_final_PB_2016_2022 <- Data_final_PB_2016_2022 %>% dplyr::select("vnf_id", "date", "long", "lat", "zip", "state", "basin", "county")


#------------------------------------------------------------------(EAGLEFORD 2016-2022)---------------------------------#
#EagleFord 2016-2022 Data
EagleFord_2016_2022 <- readRDS("UpdatedData/EF_2016_2022_w_cluster.rds")
EagleFord_2016_2022 <- EagleFord_2016_2022 %>% filter(value != 0, cloud_mask == 0, temp_bb > 1600)

EagleFord_2016_2022_coord <- st_as_sf(EagleFord_2016_2022)

EagleFord_2016_2022_county <- st_join(EagleFord_2016_2022_coord, Counties_Spatial) %>% as.data.frame()
EagleFord_2016_2022_county_non_NA <- EagleFord_2016_2022_county[!is.na(EagleFord_2016_2022_county$county), ]

EagleFord_2016_2022_zip <- st_join(EagleFord_2016_2022_coord, ZIP_valid) %>% as.data.frame()
EagleFord_2016_2022_zip_non_NA <- EagleFord_2016_2022_zip[!is.na(EagleFord_2016_2022_zip$county), ]

EagleFord_2016_2022_merged <- merge(EagleFord_2016_2022_county_non_NA ,EagleFord_2016_2022_zip_non_NA  , by = "vnf_id", all = TRUE)
EagleFord_2016_2022_merged <- EagleFord_2016_2022_merged %>% dplyr::select("vnf_id", "date.x", "lon.x", "lat.x", "zip", "state.x", "stusps", "basin" ,"county.x") 
EagleFord_2016_2022_merged$state.x <- str_to_title(EagleFord_2016_2022_merged$state.x) 
EagleFord_2016_2022_merged$county <- paste(str_to_title(EagleFord_2016_2022_merged$county.x), toupper(EagleFord_2016_2022_merged$stusps), sep =", ")
EagleFord_2016_2022_FINAL <- EagleFord_2016_2022_merged %>% dplyr::select(-"county.x", -"stusps") %>% rename(date = "date.x", state = "state.x", long = "lon.x", lat = "lat.x")

#------------------------------------------------------------------(2022-2023)---------------------------------#
#Loading 2022(Sept) - 2023
Ethan_2022_2023 <- readRDS("VNF_2022-2023.rds")
Ethan_2022_2023$vnf_lat <- Ethan_2022_2023$lat
Ethan_2022_2023$vnf_lon <- Ethan_2022_2023$lon

Ethan_2022_2023_spatial <-  st_as_sf(Ethan_2022_2023, coords = c("vnf_lon", "vnf_lat"), crs = 4326)

# Ethan_2022_2023_county <- st_join(Ethan_2022_2023_spatial, Counties_Spatial)
# Ethan_2022_2023_county_non_NA <- Ethan_2022_2023_county [!is.na(Ethan_2022_2023_county $county), ] #same as zip not like previous years. 

Ethan_2022_2023_zip_intersection <- st_join(Ethan_2022_2023_spatial, ZIP_valid) 
Ethan_2022_2023_non_NA <- na.omit(Ethan_2022_2023_zip_intersection) %>% as.data.frame() %>% rename(long = "lon", basin = "basin.x")
Ethan_2022_2023_FINAL <- Ethan_2022_2023_non_NA %>% dplyr::select("vnf_id", "date", "long", "lat", "zip", "state", "basin", "county")



#MERGE ALL THE ABOVE DATA INTO ONE DATAFRAME (THE DATA BELOW IS FOR THE SHINY APPLICATION)
Final_Data_2012_2023 <- rbind(Flaring_2012_2016_FINAL, Data_final_PB_2016_2022, EagleFord_2016_2022_FINAL, Ethan_2022_2023_FINAL) 
saveRDS(Final_Data_2012_2023, "VNF_data_final_2012-2023.rds")


#saving into R shiny app folder
file <- getwd()
file <- dirname(dirname(file))
name <- "ShinyApp/data/VNF_data_final_2012-2023.rds"
filepath <- file.path(file, name)
saveRDS(Final_Data_2012_2023, filepath)




