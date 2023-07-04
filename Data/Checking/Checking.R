#Loading Data

library(dplyr)
library(dbscan)
pkgs = c('data.table', 'dbscan', 'sf', 'tidyverse')
for(p in pkgs) require(p, character.only = TRUE)
rm(p, pkgs)


PB_data <- readRDS("pb-vnf.rds")
EF_data <- readRDS("ef-vnf.rds")

PB_wells <- readRDS("wells/cleaned_wells_pb.rds") 
EF_wells <- readRDS("Wells/cleaned_wells_ef.rds")

#Filtering for Permian Basin
PB_data <- PB_data %>% filter(temp_bb > 1600, cloud_mask == 0)
PB_data <- PB_data %>% select(-temp_bkg, -cloud_mask, -file_date)
PB_data$basin <- "Permian"


#Filtering for Western Gulf Basin
EF_data <- EF_data %>%  filter(temp_bb > 1600, cloud_mask == 0)
EF_data <- EF_data %>% select(-temp_bkg, -cloud_mask, -file_date)
EF_data$basin <- "Western Gulf"

# x <- file.choose()
# yy <- readRDS(x)
#Combining both
Data <- rbind(PB_data, EF_data) #Joining the two datasets
Data <- Data %>% filter(date >= "2016-01-03")

Data$uog_lat <- Data$lat
Data$uog_lon <- Data$lon

Data <- st_as_sf(Data, coords = c("uog_lon", "uog_lat"), crs = 4326)


#Merging the wells data
Wells_Merged <- rbind(PB_wells, EF_wells)
Wells_Merged$lat <- Wells_Merged$uog_lat
Wells_Merged$long <- Wells_Merged$uog_lon
Wells_Merged <- st_as_sf(Wells_Merged, coords = c("uog_lon", "uog_lat"), crs = 4326)


Data_with_wells_distance <- st_join(Data, Wells_Merged, join = st_is_within_distance, dist = 1e3) 
Data_with_wells_distance <- Data_with_wells_distance[!duplicated(Data_with_wells_distance$vnf_id), ]
# Data_with_wells_distance <- Data_with_wells_distance %>% distinct(vnf_id, .keep_all = TRUE)

#Getting rid of flaring not near wells
Data_with_wells_distance <- Data_with_wells_distance %>% filter(!drill_type == "NA")
Data_with_wells_distance <- as.data.frame(Data_with_wells_distance)
Data_with_wells_distance <- Data_with_wells_distance[1:(length(Data_with_wells_distance)-11)]
Data_with_wells_distance <-Data_with_wells_distance %>% rename(lat = "lat.x") 


Data.clust.list <- Data_with_wells_distance %>% mutate(year = year(date)) 
Data.clust.list <- split(Data.clust.list,list(Data.clust.list$basin, Data.clust.list$year))

minpts.grid = 3:8 #the minpts used in HDBSCAN from 3-8 inclusive

#Applying HDBSCAN
for(c in 1:length(Data.clust.list)){
  if(nrow(Data.clust.list[[c]]) > 40e3){
    Data.clust.list = c(
      Data.clust.list,
      Data.clust.list[[c]] %>% filter(month(date) > 6) %>% list()
    )
    Data.clust.list[[c]] = Data.clust.list[[c]] %>%
      filter(month(date) <= 6)
  }
}

Data.clust.list <- Data.clust.list[1:(length(Data.clust.list)-12)]
for(c in 1:length(Data.clust.list)){
  
  curr.vnf.year.minpts = NA
  curr.vnf.year.noises = NA
  curr.vnf.year.clusts = NA
  
  for(m in 1:length(minpts.grid)){
    curr.hdbscan = hdbscan(
      x = select(Data.clust.list[[c]], lon, lat),
      minPts = minpts.grid[m]
    )
    
    gc(); gc(reset = TRUE)
    
    if(is.na(curr.vnf.year.noises)) {
      curr.vnf.year.minpts = minpts.grid[m]
      curr.vnf.year.noises = sum(curr.hdbscan$cluster == 0)
      curr.vnf.year.clusts = ifelse(curr.hdbscan$cluster > 0, "Yes", "No")
      curr.vnf.value = curr.hdbscan$cluster
    } else if(sum(curr.hdbscan$cluster == 0) < curr.vnf.year.noises) {
      curr.vnf.year.minpts = minpts.grid[m]
      curr.vnf.year.noises = sum(curr.hdbscan$cluster == 0)
      curr.vnf.year.clusts = ifelse(curr.hdbscan$cluster > 0, "Yes", "No")
      curr.vnf.value = curr.hdbscan$cluster
    }}
  
  Data.clust.list[[c]] = data.table(
    vnf_id = Data.clust.list[[c]]$vnf_id,
    clustered = curr.vnf.year.clusts, value = curr.vnf.value)
  rm(list = ls(pattern = "curr")); gc(); gc(reset = TRUE)
  
  
}
xxx <- file.choose()
zzz <- readRDS(xxx)
#merges the list of objects
Output_Data <- Data.clust.list[[1]]
for (i in 2:length(Data.clust.list)){
  Output_Data <- merge(Output_Data, Data.clust.list[[i]], all = TRUE)
}