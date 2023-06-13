# Loading Packages
library(data.table)
library(leaflet)
library(reactable)
library(sf)
library(shiny)
library(tidyverse)
library(plotly)
library(rsconnect)
# Loading Data
Data <- readRDS("data/VNF_data_final_2012-2022.rds")

#Sorting Basin Data (Counties)
permian.counties <- Data %>% filter(basin == "Permian") %>% 
  pull(county) %>% unique() %>% sort() %>% as.list()

western_gulf.counties <- Data %>% filter(basin == "Western Gulf")%>% 
  pull(county) %>% unique() %>% sort() %>% as.list()

#Sorting Basin Data(ZIP CODES)
permian.ZipCodes <-
  Data %>% filter(basin == "Permian") %>% pull(zip) %>% unique() %>% sort() %>% as.list()

western_gulf.ZipCodes <-
  Data %>% filter(basin == "Western Gulf") %>% pull(zip) %>% unique()  %>%
  sort() %>% as.list()

#ui
ui <- navbarPage(
  tags$head(includeCSS("styles.css")),
  title = "UOG Flares",
  tabsetPanel(
  tabPanel(
    title = "County View",
    #county view
    sidebarLayout(
      position = "left",
      sidebarPanel(
        checkboxGroupInput(
          "checkboxes",
          label = "Select Option(s) Below",
          choices = c("Permian Basin", "Western Gulf Basin")
        ),
        selectizeInput(
          inputId = 'County',
          label = "Choose one or more Counties",
          choices = list(`permian` = permian.counties, `western` = western_gulf.counties),
          multiple = TRUE
        ),
        dateRangeInput(
          inputId = "county_date_range",
          label = "Date range",
          start = "
2012 - 03 - 01",
          end = "2014 - 12 - 31"
        ),
        actionButton('submitbutton', "Find UOG Flares", class = "county_submit_button")
      ),
      #output
      mainPanel(tabsetPanel(tabPanel(
        "Map", leafletOutput("shiny_map", width = "100%", height = "600")
      ), tabPanel("Summary",tableOutput("county_table"), uiOutput("county_bar_plot"))))
    ),
  ),
  
  #ZIP view tab
  tabPanel(
    "ZIP View",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        checkboxGroupInput(
          "checkboxes2",
          label = "Select Option(s) Below",
          choices = c("Permian Basin", "Western Gulf Basin")
        ),
        selectizeInput(
          inputId = 'Zip',
          label = "Choose one or more Zip Codes",
          choices = list(`permian` = permian.ZipCodes, `western` = western_gulf.ZipCodes),
          multiple = TRUE
        ),
        dateRangeInput(
          inputId = "zip_date_range",
          label = "Date range",
          start = "
2012 - 03 - 01",
          end = "2014 - 12 - 31"
        ),
        sliderInput(inputId = "zip_distance", label = "Distance (km)", min = 0, max = 25,
                    value = 0, step = 1), 
        
        actionButton('submit_button_zip', "Find UOG Flares", class = "zip_submit_button")
      ),
      mainPanel(tabsetPanel(tabPanel(
        "Map", leafletOutput("shiny_map_zip", width = "100%", height = "600")
      ), tabPanel("Summary",tableOutput("zip_table"), uiOutput("zip_bar_plot")), tabPanel("Reference",  width = 12, align = "center",
                                                                                              h4("Table of ZIP codes within study area"),
                                                                                              reactableOutput("zips_tab")))
    
 )))
))


