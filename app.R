#TEST H5N1 MAMMAL MAP#
#Last updated: 2/26/2025
#Script function: read in, merge, and organize data, prepare data for mapping, create interactive map in leaflet/shiny

#packages#
#install.packages("spdep")
#install.packages("maptools")
#install.packages("rtools")
#install.packages("spatialreg")
#install.packages("dpylr")
#install.packages("leaflet")

#libraries#
library(spdep)
#library(maptools)
library(data.table)
library(dplyr)
library(leaflet)

#READ IN, MERGE, AND ORGANIZE DATA#

#Read in mammal detection csv
require(data.table)

mammal <- fread("data/mammal.csv", stringsAsFactors = F, data.table = F)
#summary(mammal)
#head(mammal)


#read in the county shapefile
require(sf)
co_shapefile <- st_read("shapefiles/cb_2018_us_county_500k.shp")
#summary(co_shapefile)
#head(co_shapefile)
#names(co_shapefile)

#Add state number as a new column in "mammal" where "state" matches in both files
#read in file with State FP codes
state_fp <- fread("data/us-state-codes_ncei-to-fips.csv", stringsAsFactors = F, data.table = F)
#summary(state_fp)
#head(state_fp)

#ensure single digit FIPS codes match co_shapefile FIPS code
state_fp[state_fp$FIPS_code == 1, "FIPS_code"] <- "01"
state_fp[state_fp$FIPS_code == 4, "FIPS_code"] <- "04"
state_fp[state_fp$FIPS_code == 5, "FIPS_code"] <- "05"
state_fp[state_fp$FIPS_code == 6, "FIPS_code"] <- "06"
state_fp[state_fp$FIPS_code == 8, "FIPS_code"] <- "08"
state_fp[state_fp$FIPS_code == 9, "FIPS_code"] <- "09"

mammal$state_fp <- state_fp$FIPS_code[match(mammal$State, state_fp$state_name)]

#add unique GEOID in a new column in "mammal" where "state_fp" and "County" matches in both datasets
#Rename county 'name' column to match 
co_shapefile <- co_shapefile %>% rename(County = NAME)
#names(co_shapefile)

mammal$GEOID <- co_shapefile$GEOID[match(
  paste(mammal$state_fp, mammal$County, sep = "_"), 
  paste(co_shapefile$STATEFP, co_shapefile$County, sep = "_")
)]


#bind data together in new data file

mammal_shp <- left_join(co_shapefile, mammal, by = "GEOID", relationship = "many-to-many")
#summary(mammal_shp)
#head(mammal_shp)

#check CRS
#st_crs(mammal_shp) #CRS is 4269, need 4326 for leaflet

#create centroids for each county using CRS 4236
mammal_shp$centroids <- st_transform(mammal_shp, 4326) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()

#Extract latitude and longitude
mammal_shp$lat <- st_coordinates(mammal_shp$centroids)[,2]  # Extract latitude (Y)
mammal_shp$lng <- st_coordinates(mammal_shp$centroids)[,1]  # Extract longitude (X)

#Mutate date format for subsetting and mapping
mammal_shp <- mammal_shp %>%
  mutate(`Date Collected` = as.Date(`Date Collected`, format = "%m/%d/%Y")) #mutate date collected format
mammal_shp <- mammal_shp %>%
  mutate(`Date Detected` = as.Date(`Date Detected`, format = "%m/%d/%Y")) #mutate date detected format

#Create subset of data where date collected is Jan 1 2024 or sooner, and there is data in the species column
mammal_subset <- mammal_shp %>%
  filter(`Date Collected` >= as.Date("2024-01-01") & !is.na(Species))


#create colored markers
mammal_subset$markerColor <- case_when(
    mammal_subset$Species %in% c("Virginia opossum", "American mink", "American marten:", "North American river otter", "Fisher", "Ermine", 
                                 "Striped skunk", "Raccoon", "Skunk (unidentified)") ~ "green",
    mammal_subset$Species %in% c("Black rat", "Eastern gray squirrel", "House mouse", "Deer mouse", "Prairie vole", "Desert cottontail", "Abert's Squirrel") ~ "orange" , 
    mammal_subset$Species %in% c("Domestic cat") ~ "purple",
    mammal_subset$Species %in% c("Bobcat", "Mountain lion", "Serval", "Tiger", "African lion", "Bengal tiger", "Savannah cat", "Canada lynx", "Hybrid tiger (Panthera)", "Geoffroy's cat", "Eurasian lynx", "Amur Leopard") ~ "yellow",
    mammal_subset$Species %in% c("Harbor seal", "Bottlenose dolphin", "Grey seal") ~ "lightblue",
    mammal_subset$Species %in% c("Red fox", "Fox sp.", "Coyote") ~ "lightred",
    mammal_subset$Species %in% c("Polar bear", "American black bear", "Grizzly bear", "Kodiak bear") ~ "brown",
  TRUE ~ "gray"  # Default color
)

colored_markers <- awesomeIcons(
  icon = "info-sign", 
  iconColor = "white",
  markerColor = mammal_subset$markerColor 
)

#Create map using leaflet

mammalmap <- leaflet(data=mammal_subset) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  setView(lng =-98.58, lat =39.83 , zoom = 4) %>%
  addAwesomeMarkers(lng = ~lng, lat = ~lat, icon = colored_markers,
             popup = paste("County:", mammal_subset$County.x, "<br>",
                           "Species:", mammal_subset$Species, "<br>",
                           "HPAI Strain:", mammal_subset$`HPAI Strain`, "<br>",
                           "Date Collected:", mammal_subset$`Date Collected`, "<br>",
                           "Date Detected:", mammal_subset$`Date Detected`),
                           clusterOptions = markerClusterOptions())
 

mammalmap



#Create interactive dashboard in Shiny
#create a dashboard with shiny
#install.packages("shiny")
#install.packages("zoo")
#.rs.restartR()
require(shiny)
#require(zoo)

#create shiny app
#2 components: UI (user interface) and a server- will create server later
#shinyApp(ui = fluidPage(titlePanel("H5N1 Detections in Mammals, January 2024 - Present"),
#                        sliderInput("time_slider", 
#                                    "Select Date:", 
#                                    min = as.Date("2024-01-01"), 
#                                    max = Sys.Date(), 
#                                    value = Sys.Date(),
#                                    timeFormat = "%m-%Y"), 
#                        mainPanel(textOutput("MainPanel"))),
#         server = function(input, output){
#           output$MainPanel = renderText(input$slider)
#        }
#)


#have to close dashboard panel before moving on
#install.packages("bit64") #package allows you to manage data with long characters- takes a while to install (don't install unless needed)
#call out just the dataframe within the spatial object and create new object
mammal_df <- as.data.frame(mammal_subset)

#build out user interface and input/output function
ui = fluidPage(titlePanel("Detections of H5N1 in Mammals, January 2024 - Present"),
               #sliderInput("time_slider", 
                           #"Select Date:", 
                           #min = min(filtered_data$`Date Collected`), 
                           #max = max(filtered_data$`Date Detected`), 
                           #value = max(filtered_data$`Date Detected`),
                           #timeFormat = "%m-%Y"), 
               leafletOutput("mammalmap"))


#specify output function
#add background image
#set the view so the lat long is the center, and the zoom is zoom at first appearance (lower number further away)
#create data popups
#create data clustering
server = function(input, output){
  
  output$mammalmap <- renderLeaflet({
    leaflet(mammal_subset) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng =-98.58, lat =39.83 , zoom = 4)
    
  })
  
  observe({
    
    leafletProxy(mapId = "mammalmap", data = mammal_subset) %>% 
      clearMarkers() %>%
      addAwesomeMarkers(lng = ~lng, lat = ~lat, icon = colored_markers,
                        popup = paste("County:", mammal_subset$County.x, "<br>",
                                      "Species:", mammal_subset$Species, "<br>",
                                      "HPAI Strain:", mammal_subset$`HPAI Strain`, "<br>",
                                      "Date Collected:", mammal_subset$`Date Collected`, "<br>",
                                      "Date Detected:", mammal_subset$`Date Detected`),
                        clusterOptions = markerClusterOptions())
  })
}


shinyApp(ui, server)



