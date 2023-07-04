# Loading Packages
library(data.table)
library(leaflet)
library(reactable)
library(sf)
library(shiny)
library(tidyverse)
library(stringr)
library(plotly)
# Loading Data
Data <- readRDS("data/VNF_data_final_2012-2023.rds")
rownames(Data) = seq(length=nrow(Data))#this is used to reset the row numbers so that the table in ZIP
                                         # does not show the row number (no other way to fix)

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
    Data %>%  
      filter(date >= input$zip_date_range[1], date <= input$zip_date_range[2])%>%
      st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
      st_transform(crs = 32613) %>%
      st_join( #joins based off the buffer region
        filter(ZIP, zip %in% input$Zip) %>%
          st_transform(crs = 32613) %>%
          st_buffer(dist = input$zip_distance * 1e3) %>%
          select(zip, geometry),
        join = st_intersects, left = FALSE) %>%
      st_drop_geometry() %>% data.table()
    
  })
  
  
  #data after choosing certain counties
  current_county = reactive({
    Counties %>% filter(county %in% toupper(str_sub(input$County, end = -5))) #had to convert to uppercase
  })
  
  #data after choosing certain zip codes
  current_zip = reactive({
    ZIP %>% filter(zip %in% input$Zip)
  })
  
  
  #county_table
  output$county_table = renderTable({
    data_filtered_time_county() %>% 
      group_by(County = county) %>% 
      summarize(Flares = n(), .groups = "drop")
  })
  
  #zip_table
  output$zip_table = renderTable({
    data_filtered_time_zip() %>% 
      group_by(County = county) %>% 
      summarize(Flares = n(), .groups = "drop")
  })
  
  #reference tab for Zip Code (NEED TO FIX IT SO THAT )
  output$zips_tab = renderReactable({
    z =  Data%>% mutate(county = substr(county, 1, nchar(county) - 4)) %>%   #updated the county column and get rid of duplicates
      select(zip, basin, state, county) %>% distinct()
    

    reactable(data = z, searchable = TRUE,
              height = 500,
              highlight = TRUE)

  })
  
  


  output$county_bar_plot_non_error = renderPlotly({
   if(input$county_date_range[2] - input$county_date_range[1] < 181){
            data = data_filtered_time_county() %>%
              group_by(date) %>%
              summarize(n = n(), .groups = "drop") %>%
              mutate(time = as.Date(cut(date, "week")))

            v<- ggplot() +
              geom_bar(data = data, aes(x = time, weight = n), fill = "firebrick") +
              scale_x_date(labels = scales::date_format("%b"),
                           breaks = "2 month", expand = c(0.01, 0.01)) +
              scale_y_continuous(expand = c(0.01, 0.01)) +
              labs(x = NULL, y = NULL, title = "Weekly # of UOG flares") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5))
            ggplotly(v, tooltip = c("text", "weight", "y", "n"))
          }
          else if(input$county_date_range[2] - input$county_date_range[1]  < 732) {
            data <- data_filtered_time_county() %>% mutate(date = as.Date(date)) %>% mutate(year_month = format(date, format ="%Y-%m"))       #grouping by months
            data <- data %>% group_by(year_month) %>% summarise(n = n())        #finding the count based off the months
            data <- data %>% mutate(year_month = as.Date(paste(year_month, "01", sep = "-"), format = "%Y-%m-%d"))       #converting back to date object

            v <- ggplot() +
              geom_bar(data = data, aes(x = year_month, weight = n, text = paste('<br>Count: ', n)), fill = "firebrick") +
              scale_x_date(labels = scales::date_format("%b-%Y"),
                           breaks = "3 month", expand = c(0.01, 0.01)) +
              scale_y_continuous(expand = c(0.01, 0.01)) +
            labs(x = NULL, y = NULL, title = "Monthly # of UOG flares") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))

            ggplotly(v, tooltip = "text")}
        else{

          data <- data_filtered_time_county() %>% mutate(as.Date(date)) %>% mutate(year = format(date, format = "%Y"))
          data <- data %>% group_by(year) %>% summarise(n = n())
          data <- data %>% mutate(year = as.Date(paste(year, "1", "1", sep = "-"), format = "%Y-%m-%d"))

          v <- ggplot() +
            geom_bar(data = data, aes(x = year, weight = n, text = paste('<br>Count: ', n)), fill = "firebrick") +
            scale_x_date(labels = scales::date_format("%Y"),
                         breaks = "1 year", expand = c(0.01, 0.01)) +
            scale_y_continuous(expand = c(0.01, 0.01)) +
            labs(x = NULL, y = NULL, title = "Annual # of UOG flares") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(v, tooltip = "text")}}
  )

  
#logic for not having a basin selected
output$county_bar_plot = renderUI(
  if (is.null(input$checkboxes)){
    print("Please select a Basin")
  }
  else{
    plotlyOutput("county_bar_plot_non_error")
  }
)
  

#render the table for when we have a basin selected
output$zip_bar_plot_non_error = renderPlotly(
  {if(input$zip_date_range[2] - input$zip_date_range[1] < 181){
      data = data_filtered_time_zip() %>%
        group_by(date) %>%
        summarize(n = n(), .groups = "drop") %>%
        mutate(time = as.Date(cut(date, "week")))
      
      
      v <- ggplot() +
        geom_bar(data = data, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%b"),
                     breaks = "2 month", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Weekly # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(v)
    }
    
    else if(input$zip_date_range[2] - input$zip_date_range[1]  < 732) {
      
      data <- data_filtered_time_zip() %>% mutate(date = as.Date(date)) %>% mutate(year_month = format(date, format ="%Y-%m"))       #grouping by months
      data <- data %>% group_by(year_month) %>% summarise(n = n())        #finding the count based off the months
      data <- data %>% mutate(year_month = as.Date(paste(year_month, "01", sep = "-"), format = "%Y-%m-%d"))       #converting back to date object
      v<-  ggplot() +
        geom_bar(data = data, aes(x = year_month, weight = n, text = paste('<br>Count: ', n)), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%b-%Y"),
                     breaks = "3 month", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Monthly # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(v, tooltip = "text")
    }
    
    else{
      data <- data_filtered_time_zip() %>% mutate(as.Date(date)) %>% mutate(year = format(date, format = "%Y"))
      data <- data %>% group_by(year) %>% summarise(n = n())
      data <- data %>% mutate(year = as.Date(paste(year, "1", "1", sep = "-"), format = "%Y-%m-%d"))
      v <- ggplot() + geom_bar(data = data, aes(x = year, weight = n, text = paste('<br>Count: ', n)), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%Y"),
                     breaks = "1 year", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Annual # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(v, tooltip = "text")
    }}
  
)
output$zip_bar_plot = renderUI(
  if (is.null(input$checkboxes2)){
    print("Please select a Basin")
  }
  else{
    plotlyOutput("zip_bar_plot_non_error")
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
    print(nrow(data_filtered_with_time))
    curr_zip <- current_zip()
  
    
    cty_bbox = curr_zip %>% st_bbox() %>% as.list() #converts to spatial box
    lon_min = min(data_filtered_with_time$long, cty_bbox$xmin)  
    lat_min = min(data_filtered_with_time$lat, cty_bbox$ymin)
    lon_max = max(data_filtered_with_time$long, cty_bbox$xmax)
    lat_max = max(data_filtered_with_time$lat, cty_bbox$ymax)
    
    
    curr_zip_range = current_zip() %>%
      st_transform(crs = 32613) %>%
      st_buffer(dist = input$zip_distance * 1e3) %>%
      st_transform(crs = 4326)
    
    
    leafletProxy("shiny_map_zip", data = data_filtered_time_zip()) %>%
      clearShapes() %>% clearMarkerClusters()%>%
      addPolygons(data = curr_zip,
                  weight = 1, opacity = 1) %>%
      addPolygons(data = curr_zip_range, opacity = .1, fillOpacity = .1,
                  weight = 1, color = "red") %>%
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








