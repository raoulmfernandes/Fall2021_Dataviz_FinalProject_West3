#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(sf)
library(ggmap)
library(tmap)
library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)
library(tidyverse)

sf_use_s2(FALSE)
setwd("~/Documents/ND Data Science/Data Viz/Final Project/City Council")
council <- sf::st_read("City_Council_Districts.shp", stringsAsFactors = FALSE)  
public <- readr::read_csv("Public_Facilities.csv")
parks <- readr::read_csv("Parks_Locations_and_Features.csv")

load("SBmatrix.rdata")


names(dm) <- public$POPL_NAME
dm <- cbind(Park_Name = rownames(dm), dm)
rownames(dm) <- NULL

SBmatrix <- dm %>% pivot_longer(-"Park_Name", 
                                names_to = "Public_Facilities_Name", 
                                values_to = "Distance")
SBmatrix$Distance <- as.numeric(SBmatrix$Distance)


points_park <- parks %>% st_as_sf(coords = c("Lon","Lat"), crs = 4326) 
council%>%mutate(count = lengths(st_intersects(.,points_park)))

# Converting the csv file into a spatial object
park.loc.spatial <- parks %>%
    st_as_sf(coords = c("Lon", "Lat")) %>%
    st_set_crs(value = 4326)
public.facilities.spatial <- public %>%
    st_as_sf(coords = c("Lon", "Lat")) %>%
    st_set_crs(value = 4326)

#Prepare Data for Pivot
parks_pivot <- parks
colnames(parks_pivot) <- paste("Feat", colnames(parks_pivot), sep = "_")
names(parks_pivot)[1] <- 'Park_Name'
names(parks_pivot)[2] <- 'Park_Type'
names(parks_pivot)[3] <- 'Zip_Code'
names(parks_pivot)[48] <- 'Address'
names(parks_pivot)[49] <- 'Lat'
names(parks_pivot)[50] <- 'Lon'

parks_pivot <- parks_pivot %>%
    pivot_longer(
        cols = starts_with("Feat"), 
        names_to = "feature", 
        names_prefix = "Feat_",
        values_to = "count",
        values_drop_na = TRUE,
    )

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("South Bend Parks and Public Facilities"),
    
    navbarPage("South Bend",
               
               #TAB 1
               tabPanel("Park Locations and Features",
                        # Sidebar layout with a input and output definitions ----
                        sidebarLayout(
                            sidebarPanel(
                                
                                # Input: Selector for choosing dataset ----
                                selectInput(inputId = "parkfacs", label = "Select Type of Park", choices = unique(parks$Park_Type)),
                                
                                radioButtons(inputId = "FeatureName",
                                             label = "Select Feature(s):",
                                             choices = unique(parks_pivot$feature),
                                             selected = "Playground__Local"),
                                
                                sliderInput(inputId = "FeatureCount",
                                            label = "Count of Selected Features",
                                            min = 0,
                                            max = 20,
                                            value = c(0,3),
                                            width = "220px")    
                                
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                                
                                # Output: Map ----
                                leafletOutput(outputId = "map"),
                                
                                # Output: Table ----
                                dataTableOutput(outputId = "pubTbl")
                                
                            )
                        )
               ),
               
               #TAB 2
               tabPanel("Public Facilities",
                        # Sidebar layout with a input and output definitions ----
                        sidebarLayout(
                            sidebarPanel(
                                
                                # Input: Selector for choosing dataset ----
                                selectInput(inputId = "pubfacs", label = "Select Type of Public Facility", choices = unique(public$POPL_TYPE)),
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                                
                                # Output: Map ----
                                leafletOutput(outputId = "map_pf"),
                                
                                # Output: Table ----
                                dataTableOutput(outputId = "pubTbl_pf")
                                
                            )
                        )
               ),
               
               #TAB 3
               tabPanel("Finding the best Neighborhoods and Districts in South Bend, IN",
                        
                        # Sidebar layout with a input and output definitions ----
                        sidebarLayout(
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
                                    value = 8047)),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                                tabsetPanel(
                                    
                                    tabPanel(title = "Distance between Parks and Facilities",
                                             leafletOutput(outputId = "map3"))
                                ),#end tabsetPanel
                                width = 150,
                                height = 100
                            )#end mainPanel
                        )
               )
    )
)

# Define server logic to summarize and view selected dataset ----

server <- function(input, output) {
    
    parks.subset <- reactive({
        parks_pivot %>% filter(Park_Type == input$parkfacs) %>%
            filter(feature == input$FeatureName) %>%
            filter(count >= input$FeatureCount[1],
                   count <=input$FeatureCount[2]) 
    })
    
    parks_subset <- reactive({
        parks.subset() %>% st_as_sf(coords = c("Lon","Lat"), crs = 4326) 
    })
    
    
    map_data <- reactive({
        council%>%mutate(count = lengths(st_intersects(.,parks_subset())))
    })
    
    output$pubTbl <- renderDataTable({
        datatable(parks.subset())
        
    })
    
    
    output$map <- renderLeaflet({
        mypalette <- colorNumeric("RdYlGn",domain = map_data()$count)
        
        labels <- sprintf(
            "<strong>Park Name: %s</strong><br/>Park Type: %s",
            parks_subset()$Park_Name, parks_subset()$Park_Type
        ) %>% lapply(htmltools::HTML)
        
        map_data() %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(fillColor = ~mypalette(count),label=~paste("District:", Dist, "|", "Councilperson:", Council_Me)) %>%
            addCircleMarkers(data = parks_subset(), label = labels)
    })
    
    #Public Facilities
    
    public.subset <- reactive({
        public %>% filter(POPL_TYPE == input$pubfacs) 
    })
    
    public_points_subset <- reactive({
        public.subset() %>% st_as_sf(coords = c("Lon","Lat"), crs = 4326) 
    })
    
    map_data_pf <- reactive({
        council%>%mutate(count = lengths(st_intersects(.,public_points_subset())))
    })
    
    output$pubTbl_pf <- renderDataTable({
        datatable(public.subset())
        
    })
    
    
    output$map_pf <- renderLeaflet({
        mypalette <- colorNumeric("RdYlGn",domain = map_data_pf()$count)
        
        labels <- sprintf(
            "<strong>Facility Name: %s</strong><br/>Facility Type: %s",
            public_points_subset()$POPL_NAME, public_points_subset()$POPL_TYPE
        ) %>% lapply(htmltools::HTML)
        
        map_data_pf() %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(fillColor = ~mypalette(count),label=~paste("District:", Dist, "|", "Councilperson:", Council_Me, "|", "Public Facilities:", count)) %>%
            addCircleMarkers(data = public_points_subset(), label = labels)
    })
    
    #Parks and Public Facilities
    
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
    
    output$map3 <- renderLeaflet({
        pal = colorFactor(palette = c("red", "yellow","blue"),domain = c("FIRE STATION", "LIBRARY", "POLICE STATION"))
        
        leaflet()  %>%
            addTiles()  %>%
            addMarkers(data = park.loc.subset(), popup = ~Park_Name) %>%
            addCircleMarkers(data = distance.subset(), fillColor = ~pal(POPL_TYPE)) %>%
            addLegend("bottomright", pal = pal, values = c("Fire Station", "Library", "Police Station"),
                      title = "Type of Public Facilities",
                      opacity = 1)
    }) #end outputmap
    
}

shinyApp(ui = ui, server = server)