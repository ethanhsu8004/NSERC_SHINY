# Loading Packages
library(data.table)
library(leaflet)
library(reactable)
library(sf)
library(shiny)
library(tidyverse)
library(plotly)

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
  title = "UOG flares",
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
          end = "2012 - 05 - 31"
        ),
        actionButton('submitbutton', "Find UOG Flares")
      ),
      #output
      mainPanel(tabsetPanel(tabPanel(
        "Map", leafletOutput("shiny_map", width = "100%", height = "600")
      ), tabPanel("Summary",tableOutput("county_table"), plotlyOutput("county_bar_plot"))))
    )
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
          end = "2012 - 12 - 31"
        ),
        sliderInput(inputId = "zip_distance", label = "Distance (km)", min = 1, max = 100,
                    value = 10, step = 1), 
        
        actionButton('submit_button_zip', "Find UOG Flares")
      ),
      mainPanel(tabsetPanel(tabPanel(
        "Map", leafletOutput("shiny_map_zip", width = "100%", height = "600")
      ), tabPanel("Summary",tableOutput("zip_table"), plotlyOutput("zip_bar_plot")), tabPanel("Reference",  width = 12, align = "center",
                                                                                              h4("Table of ZIP codes within study area"),
                                                                                              reactableOutput("zips_tab")))
    
 )))
))


