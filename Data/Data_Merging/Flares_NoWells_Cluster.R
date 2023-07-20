library(dplyr)
library(dbscan)
pkgs = c('data.table', 'dbscan', 'sf', 'tidyverse')
for(p in pkgs) require(p, character.only = TRUE)
rm(p, pkgs)


EF_raw <- readRDS("../JerryData/ef_vnf/ef-vnf.rds")
PB_raw <- readRDS("../JerryData/pb_vnf/pb-vnf.rds")

#Data from 2012-2016
EF_filtered_2012_2016 <- EF_raw %>% filter(date < "2016-01-03", cloud_mask == 0, temp_bb > 1600)
EF_filtered_2012_2016$basin <- "Western Gulf"
EF_filtered_2012_2016 <- distinct(EF_filtered_2012_2016 , vnf_id, .keep_all = TRUE) #need to use distinct to get rid of duplicates 

PB_filtered_2012_2016 <- PB_raw %>% filter(date < "2016-01-03", cloud_mask == 0, temp_bb > 1600)
PB_filtered_2012_2016$basin <- "Permian"
PB_filtered_2012_2016 <- distinct(PB_filtered_2012_2016 , vnf_id, .keep_all = TRUE) #need to use distinct to get rid of duplicates 

Merged_2012_2016 <- rbind(EF_filtered_2012_2016, PB_filtered_2012_2016)
Merged_2012_2016$uog_lat <- Merged_2012_2016$lat
Merged_2012_2016$uog_lon <- Merged_2012_2016$lon
Merged_2012_2016 <- st_as_sf(Merged_2012_2016, coords = c("uog_lon", "uog_lat"), crs = 4326) %>%as.data.frame()
minpts.grid = 3:8 #the minpts used in HDBSCAN from 3-8 inclusive

#splitting it by year and basin
Merged_2012_2016.clust.list <- Merged_2012_2016 %>% mutate(year = year(date)) %>%as.data.frame()
Merged_2012_2016.clust.list <- split(Merged_2012_2016.clust.list, list(Merged_2012_2016.clust.list$basin, Merged_2012_2016.clust.list$year))


#HDBSCAN FOR 2012-2016
for(c in 1:length(Merged_2012_2016.clust.list)){
  if(nrow(Merged_2012_2016.clust.list[[c]]) > 40e3){
    Merged_2012_2016.clust.list = c(
      Merged_2012_2016.clust.list,
      Merged_2012_2016.clust.list[[c]] %>% filter(month(date) > 6) %>% list()
    )
    Merged_2012_2016.clust.list[[c]] = Merged_2012_2016.clust.list[[c]] %>%
      filter(month(date) <= 6)
  }
}

for(c in 1:length(Merged_2012_2016.clust.list)){
  
  curr.vnf.year.minpts = NA
  curr.vnf.year.noises = NA
  curr.vnf.year.clusts = NA

  for(m in 1:length(minpts.grid)){
    print(m)
    curr.hdbscan = hdbscan(
      x = dplyr::select(Merged_2012_2016.clust.list[[c]], lon, lat),
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
    }
  }

  Merged_2012_2016.clust.list[[c]] = data.table(
    vnf_id = Merged_2012_2016.clust.list[[c]]$vnf_id,
    clustered = curr.vnf.year.clusts, value = curr.vnf.value)
  rm(list = ls(pattern = "curr")); gc(); gc(reset = TRUE)


}

Output_Data_2012_2016 <- Merged_2012_2016.clust.list[[1]]
for (i in 2:length(Merged_2012_2016.clust.list)){
  Output_Data_2012_2016 <- merge(Output_Data_2012_2016, Merged_2012_2016.clust.list[[i]], all = TRUE)
}

Output_Data_2012_2016_FINAL <- merge(Merged_2012_2016,Output_Data_2012_2016,  by = "vnf_id")

#-------------------------------------EAGLE FORD---------------------------------------------------------#


#EAGLEFORD HDBSCAN for 2016-2022 (very small number of observations)
EF_2016_2022 <- EF_raw %>% filter(date >= "2016-01-03") %>% filter(cloud_mask == 0, temp_bb > 1600)
EF_2016_2022$basin <- "Western Gulf"
EF_2016_2022$uog_lat <- EF_2016_2022$lat
EF_2016_2022$uog_lon<- EF_2016_2022$lon

EF_2016_2022 <- st_as_sf(EF_2016_2022, coords = c("uog_lon", "uog_lat"), crs = 4326) %>% as.data.frame()
EF_2016_2022 <- distinct(EF_2016_2022, vnf_id, .keep_all = TRUE)
EF_2016_2022.clust.list <- EF_2016_2022 %>% mutate(year = year(date)) %>%as.data.frame()
EF_2016_2022.clust.list <- split(EF_2016_2022.clust.list, list(EF_2016_2022.clust.list$year)) #don't need the other line since there is only the Western Gulf Basin

#Applying HDBSCAN
for(c in 1:length(EF_2016_2022.clust.list)){
  
  curr.vnf.year.minpts = NA
  curr.vnf.year.noises = NA
  curr.vnf.year.clusts = NA
  
  for(m in 1:length(minpts.grid)){
    print(m)
    curr.hdbscan = hdbscan(
      x = dplyr::select(EF_2016_2022.clust.list[[c]], lon, lat),
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
    }
  }
  
  EF_2016_2022.clust.list[[c]] = data.table(
    vnf_id = EF_2016_2022.clust.list[[c]]$vnf_id,
    clustered = curr.vnf.year.clusts, value = curr.vnf.value)
  rm(list = ls(pattern = "curr")); gc(); gc(reset = TRUE)
  
  
}


DATA_2016_2022_EF <- EF_2016_2022.clust.list[[1]]
for (i in 2:length(EF_2016_2022.clust.list)){
  DATA_2016_2022_EF <- merge(DATA_2016_2022_EF, EF_2016_2022.clust.list[[i]], all = TRUE)
}

DATA_2016_2022_EF_FINAL <- merge(EF_2016_2022, DATA_2016_2022_EF , by = "vnf_id")

#Saving Data
saveRDS(Output_Data_2012_2016_FINAL, "UpdatedData/Flaring_Data_2012-2016.rds")
saveRDS(DATA_2016_2022_EF_FINAL, "UpdatedData/EF_2016_2022_w_cluster.rds")

