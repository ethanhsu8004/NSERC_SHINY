#Extracting from EagleFord
library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(utils)
library(dbscan)
pkgs = c('data.table', 'R.utils', 'sf', 'tidyverse')
for(p in pkgs) require(p, character.only = TRUE)
rm(p, pkgs)


code.dir = paste0(getwd())
data.dir = paste0(getwd(), '/data/')



#EAGLE FORD!
ef_bbox = st_read("../JerryData/shapes/ShalePlays_US_EIA_Dec2021.shp") %>% 
  filter((Basin %in% c("Western Gulf"))) %>%
  st_bbox()
lon_bounds = ef_bbox[c(1,3)]; lat_bounds = ef_bbox[c(2,4)]

#This is just general guidelines from https://eogdata.mines.edu/products/register/ (bottom of page)
params <- list(
  client_id = 'eogdata_oidc',
  client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
  username = "ethan.hsu@mail.utoronto.ca",
  password = "NSERC_stuff",
  grant_type = 'password'
)
token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
response <- POST(token_url, body = params, encode = "form")
access_token_list <- fromJSON(content(response,as="text",encoding="UTF-8"))
access_token <- access_token_list$access_token

data_url <- 'https://eogdata.mines.edu/eog/EOG_sensitive_contents'
auth <- paste('Bearer', access_token)

output_file <- basename(data_url)
download.file(data_url,output_file,mode = "wb", headers = list(Authorization = auth))


#link to where the files are
vnfv30.url.pfx = c(
  "https://eogdata.mines.edu/wwwdata/viirs_products/vnf/v30//VNF_npp_d",
  "_noaa_v30-ez.csv.gz")


#columns that we want
vnf.cols = c('date_mscan', 'lon_gmtco', 'lat_gmtco', 'temp_bb', 'temp_bkg',
             'esf_bb', 'rhi', 'rh', 'area_pixel', 'area_bb', 'cloud_mask')

#end date currently
end_date = as.Date("2023-06-26")

vnf.data.exist = file.exists(paste0(code.dir, "ef_vnf/ef-vnf.rds")) #change this line

if(vnf.data.exist){
  vnf = readRDS(paste0(data.dir, "ef_vnf/ef-vnf.rds"))
  
  # start new update from date after last date in existing data
  start_date = max(vnf$file_date) + 1
  
} else {
  vnf = data.table(
    vnf_id = character(),
    date = as.Date(character()),
    lon = numeric(),
    lat = numeric(),
    temp_bb = integer(),
    temp_bkg = integer(),
    esf_bb = numeric(),
    rhi = numeric(),
    rh = numeric(),
    area_pixel = numeric(),
    area_bb = numeric(),
    cloud_mask = integer(),
    file_date = as.Date(character())
  )
  # start new update from date after last date in existing data
  start_date = as.Date("2022-09-01")
  
}

vnf.dates = seq(start_date, end_date, "days")
for (date in 1:length(vnf.dates)){
  url.name = paste0(vnfv30.url.pfx[1], 
                    gsub('-', '', vnf.dates[date]), vnfv30.url.pfx[2])
  gz.name = paste0(data.dir, '', basename(url.name))
  csv.name = gsub(".gz", "", gz.name)
  
  tryCatch({
    
    cat("[", as.character(vnf.dates[date]),'] download. ', sep='')
    download.file(url.name, gz.name, 
                  mode = 'wb', quiet = TRUE,
                  headers = list(Authorization = auth))
    R.utils::gunzip(gz.name, overwrite = TRUE)
    vnf.temp = fread(csv.name) %>% 
      rename_all(tolower) %>% # rename all columns to lowercase for convenience
      select(all_of(vnf.cols))
    
    is.na(vnf.temp) <- vnf.temp == 999999
    
    vnf.temp = vnf.temp %>% filter(!is.na(temp_bb)) %>%  mutate(vnf_id = paste0('VNF', gsub('-','',vnf.dates[date]), sprintf('%06d', 1:nrow(.))),
                                                                date = as.Date(substr(date_mscan, 1, 10), format = "%Y/%m/%d"),
                                                                file_date = vnf.dates[date]) %>% select(vnf_id, date, lon = lon_gmtco, lat = lat_gmtco,
                                                                                                        everything(), -date_mscan) %>% 
      # filter by lat & lon bounds (bbox)
      filter(lon >= lon_bounds[1], lon <= lon_bounds[2],
             lat >= lat_bounds[1], lat <= lat_bounds[2])
    
    vnf = rbindlist(
      list(vnf,
           vnf.temp))
  }, error = function(e){
    print(e)})
  
  file.remove(csv.name)
}



file <- saveRDS(vnf, file = "ef_vnf/ef_vnf.rds")







