# Loading Packages
library(data.table)
library(leaflet)
library(reactable)
library(sf)
library(shiny)
library(tidyverse)
library(stringr)

# Loading Data
Data <- readRDS("data/VNF_data_final_2012-2022.rds")

Counties <- st_read("data/counties.shp")
ZIP <- st_read("data/zips.shp")

#Creating Counties
permian.counties <- Data %>% filter(basin == "Permian") %>% 
  pull(county) %>% unique() %>% sort() %>% as.list()

western_gulf.counties <- Data %>% filter(basin == "Western Gulf")%>% 
  pull(county) %>% unique() %>% sort() %>% as.list()

#Creating ZIP CODES
permian.ZipCodes <-
  Data %>% filter(basin == "Permian") %>% pull(zip) %>% unique() %>% sort() %>% as.list()

western_gulf.ZipCodes <-
  Data %>% filter(basin == "Western Gulf") %>% pull(zip) %>% unique()  %>%
  sort() %>% as.list()




#Server
server <- function(input, output, session) {
  #Logic for checkboxes (COUNTY)
  observeEvent({input$checkboxes}, {
    #Having no options selected
    if(is.null(input$checkboxes) | is.null(input$checkboxes2)){
      updateSelectizeInput(session, "County", selected = "Pick An Option Above",
                           choice = "Pick An Option Above")}
    
    #Having both options selected
    if ("Permian Basin" %in% input$checkboxes & "Western Gulf Basin" %in% input$checkboxes) {
      updateSelectizeInput(session, "County", choices = list(`Permian Basin` = permian.counties, `Western Gulf Basin` = western_gulf.counties), selected = "Andrews, TX")
    }
    
    #Having only the Permian Basin Selected
    else if ("Permian Basin" %in% input$checkboxes | "Permian Basin" %in% input$checkboxes2){
      updateSelectizeInput(session, "County", choices = list(`Permian Basin` = permian.counties), selected = "Andrews, TX")
      updateSelectizeInput(session, "County2", choices = list(`Permian Basin` = permian.counties), selected = "Andrews, TX")
    }
    
    #Having only the Western Gulf Basin selected
    else if ("Western Gulf Basin" %in% input$checkboxes){
      updateSelectizeInput(session, "County", choices = list(`Western Gulf Basin` = western_gulf.counties), selected = "Atascosa, TX")
    }
    
  }, ignoreNULL = FALSE) #since by default this does not register NULL. 
  
  
  #Logic for checkboxes for Zip Codes
  observeEvent({input$checkboxes2}, {
    print('test')
    #Having no options selected
    if(is.null(input$checkboxes2)){
      updateSelectizeInput(session, "Zip", selected = "Pick An Option Above",
                           choice = "Pick An Option Above")}
    
    #Having both options selected
    if ("Permian Basin" %in% input$checkboxes2 &
        "Western Gulf Basin" %in% input$checkboxes2) {
      updateSelectizeInput(
        session,
        "Zip",
        choices = list(`Permian Basin` = permian.ZipCodes, `Western Gulf Basin` = western_gulf.ZipCodes),
        selected = min(unlist(permian.ZipCodes), unlist(western_gulf.ZipCodes))
      )
    }
    
    #Having only the Permian Basin Selected
    else if ("Permian Basin" %in% input$checkboxes2) {
      updateSelectizeInput(
        session,
        "Zip",
        choices = list(`Permian Basin` = permian.ZipCodes),
        selected = min(unlist(permian.ZipCodes))
      )
    }
    
    #Having only the Western Gulf Basin selected
    else if ("Western Gulf Basin" %in% input$checkboxes2) {
      updateSelectizeInput(
        session,
        "Zip",
        choices = list(`Western Gulf Basin` = western_gulf.ZipCodes),
        selected = min(unlist(western_gulf.ZipCodes))
      )
    }
    
  }, ignoreNULL = FALSE) #since by default this does not register NULL.
  
  
  #render the default shiny map
  output$shiny_map = renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(noWarp = TRUE)) %>%
      flyToBounds(lng1 = -127.8, lat1 = 23.8,
                  lng2 = -64.8, lat2 = 49.8)
    # %>% addLayersControl(overlayGroups =  "Show Wells",options = layersControlOptions(collapse = FALSE, unselected = TRUE)) 
    # %>% hideGroup("Show Wells")
  })
  
  output$shiny_map_zip = renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(noWarp = TRUE)) %>%
      flyToBounds(lng1 = -127.8, lat1 = 23.8,
                  lng2 = -64.8, lat2 = 49.8)
  })
  

  
  #data after filtering for time (County)
  data_filtered_time_county = reactive({
    Data %>% filter(county %in% input$County) %>% filter(date >= input$county_date_range[1],
                                                         date <= input$county_date_range[2])
  })
  

  #data after filtering for time (ZIP)
  data_filtered_time_zip = reactive({
    Data %>% filter(zip %in% input$Zip) %>% 
      filter(date >= input$zip_date_range[1], date <= input$zip_date_range[2])
  })
  
  
  #data after choosing certain counties
  current_county = reactive({
    Counties %>% filter(county %in% toupper(str_sub(input$County, end = -5))) #had to convert to uppercase
  })
  
  #data after choosing certain zip codes
  current_zip = reactive({
    ZIP %>% filter(zip %in% input$Zip)
  })
  
  
  #output for barplot
  output$county_bar_plot = renderPlot(
    if (is.null(input$checkboxes)){
      plot.new()
      text(0.5, 0.5, "Please select a Basin", cex = 2)
    }
    
    else{
      data <- data_filtered_time_county() %>% group_by(date) %>% summarize(n = n())
      print(data)
      ggplot(data = data, aes(date)) + geom_histogram()
      
    }
    
  )
  
  output$zip_bar_plot = renderPlot(
    if(is.null(input$checkboxes2)){
      plot.new()
      text(0.5, 0.5, "Please select a Basin", cex = 2)
      
    }
    
    else{
      data <- data_filtered_time_zip() %>% group_by(date) %>% summarize(n = n())
      
      ggplot(data = data, aes(date)) + geom_histogram()
      
    }
    
  )
  
  #logic for the submit button (County)
  observeEvent(input$submitbutton,{
    data_filtered_with_time <- data_filtered_time_county()
    curr_county = current_county()
    cty_bbox = curr_county %>% st_bbox() %>% as.list() #converts to spatial box
    lon_min = min(data_filtered_with_time$long, cty_bbox$xmin)  
    lat_min = min(data_filtered_with_time$lat, cty_bbox$ymin)
    lon_max = max(data_filtered_with_time$long, cty_bbox$xmax)
    lat_max = max(data_filtered_with_time$lat, cty_bbox$ymax)

    #generates map
    leafletProxy("shiny_map", data = data_filtered_with_time) %>%
      clearShapes() %>% clearMarkerClusters()%>%
    addPolygons(data = curr_county,
                weight = 1, opacity = 1) %>%
    addCircleMarkers(
      lng = ~long, lat = ~lat,
      color = "#d62728", radius = 5,
      fillOpacity = 0.8, stroke = FALSE,
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE
      )) %>%
    flyToBounds(lng1 = lon_min, lat1 = lat_min,
                lng2 = lon_max, lat2 = lat_max)
  })
  #Submit Button (ZIP)
  observeEvent(input$submit_button_zip, {
    data_filtered_with_time <- data_filtered_time_zip()
    curr_zip <- current_zip()
    
    cty_bbox = curr_zip %>% st_bbox() %>% as.list() #converts to spatial box
    lon_min = min(data_filtered_with_time$long, cty_bbox$xmin)  
    lat_min = min(data_filtered_with_time$lat, cty_bbox$ymin)
    lon_max = max(data_filtered_with_time$long, cty_bbox$xmax)
    lat_max = max(data_filtered_with_time$lat, cty_bbox$ymax)
    
    leafletProxy("shiny_map_zip", data = data_filtered_with_time) %>%
      clearShapes() %>% clearMarkerClusters()%>%
      addPolygons(data = curr_zip,
                  weight = 1, opacity = 1) %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        color = "#d62728", radius = 5,
        fillOpacity = 0.8, stroke = FALSE,
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          spiderfyOnMaxZoom = TRUE
        )) %>%
      flyToBounds(lng1 = lon_min, lat1 = lat_min,
                  lng2 = lon_max, lat2 = lat_max)
  })
}








