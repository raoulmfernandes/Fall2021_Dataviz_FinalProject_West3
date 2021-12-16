
library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(tidyverse)

# Set working directory
# setwd("/Users/raoul.fernandes/Downloads/DataViz/--Data-Viz-2021-Fall-master/Final Project Class/Dataviz_FinalProect")

districts <- st_read("City_Council_Districts.shp")
districts.data <- districts %>%
    st_set_geometry(NULL)

load("SBmatrix.rdata")



# Loading the parks and locations csv file
park.loc <- read_csv("Parks_Locations_and_Features.csv")
public.facilities <- read_csv("Public_Facilities.csv")

# Converting the csv file into a spatial object
park.loc.spatial <- park.loc %>%
    st_as_sf(coords = c("Lon", "Lat")) %>%
    st_set_crs(value = 4326)
public.facilities.spatial <- public.facilities %>%
    st_as_sf(coords = c("Lon", "Lat")) %>%
    st_set_crs(value = 4326)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Finding the best Neighborhoods and Districts in South Bend, IN"),
    
    # Sidebar with a slider input for number of bins
    sidebarPanel(
        
        selectInput(
            inputId = "Park_Type",
            label = "Park Type",
            choices = park.loc.spatial$Park_Type,
            selected = 1),
        
        sliderInput(
            inputId = "Distance",
            label = "Distance between City Parks and Public Services",
            min = 1,
            max = 8047,
            value = 8047)),#end sidebarPanel
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
            
            tabPanel(title = "Distance between Parks and Facilities",
                     leafletOutput(outputId = "map"))
        ),#end tabsetPanel
        width = 150,
        height = 100
    )#end mainPanel
    
)#end fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
    park.loc.subset <- reactive({
        park.loc.spatial %>%
            filter(Park_Type == input$Park_Type)
    })
    
    distance.subset <- reactive({
        print(park.loc.subset()$Park_Name)
        fac.list <- SBmatrix %>%
            filter(Distance <= input$Distance) %>%
            filter(Park_Name %in% park.loc.subset()$Park_Name)
        print(fac.list$Public_Facilities_Name)
        public.facilities.spatial %>%
            filter(POPL_NAME %in% fac.list$Public_Facilities_Name)
        
    })
    
    output$map <- renderLeaflet({
        pal = colorFactor(palette = c("red", "yellow","blue"),domain = c("FIRE STATION", "LIBRARY", "POLICE STATION"))
        
        leaflet()  %>%
            addTiles()  %>%
            addMarkers(data = park.loc.subset(), popup = ~Park_Name) %>%
            addCircleMarkers(data = distance.subset(), fillColor = ~pal(POPL_TYPE)) %>%
            addLegend("bottomright", pal = pal, values = c("Fire Station", "Library", "Police Station"),
                      title = "Type of Public Facilities",
                      opacity = 1)
    }) #end outputmap
} #end server

# Run the application
shinyApp(ui = ui, server = server)