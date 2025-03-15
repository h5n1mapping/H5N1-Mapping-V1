#TEST H5N1 MAMMAL AND WILD BIRD MAP#
#Last updated: 2/26/2025
#Script function: read in, merge, and organize data, prepare data for mapping, create interactive map in leaflet/shiny

#packages#
install.packages("rtools")
install.packages("dpylr")
install.packages("leaflet")
install.packages("shiny")
install.packages("shinydashboard")

#libraries#
library(data.table)
library(dplyr)
library(leaflet)

#READ IN, MERGE, AND ORGANIZE DATA#

#Read in mammal detection csv
require(data.table)

mammal <- fread("data/mammal.csv", stringsAsFactors = F, data.table = F)
#summary(mammal)
#head(mammal)
wild_bird <-fread("data/bird_wild.csv", stringsAsFactors = F, data.table =F)
#summary(wild_bird)
#head(wild_bird)


#read in the county shapefile
require(sf)
co_shapefile <- st_read("shapefiles/cb_2018_us_county_500k.shp")
#summary(co_shapefile)
#head(co_shapefile)
#names(co_shapefile)

#Add state number as a new column in "mammal" and "wild_bird" where "state" matches in both files
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
wild_bird$state_fp <- state_fp$FIPS_code[match(wild_bird$State, state_fp$state_name)]

#add unique GEOID in a new column in "mammal" and "wild bird" where "state_fp" and "County" matches in both datasets
#Rename county 'name' column to match 
co_shapefile <- co_shapefile %>% rename(County = NAME)
#names(co_shapefile)

mammal$GEOID <- co_shapefile$GEOID[match(
  paste(mammal$state_fp, mammal$County, sep = "_"), 
  paste(co_shapefile$STATEFP, co_shapefile$County, sep = "_")
)]

wild_bird$GEOID <- co_shapefile$GEOID[match(
  paste(wild_bird$state_fp, wild_bird$County, sep = "_"), 
  paste(co_shapefile$STATEFP, co_shapefile$County, sep = "_")
)]

wild_bird <- wild_bird %>% rename(Bird_Species = `Bird Species`)
wild_bird <- wild_bird %>% rename(WOAH_Classification = `WOAH Classification`)
wild_bird <- wild_bird %>% rename(Sampling_Method = `Sampling Method`)
wild_bird <- wild_bird %>% rename(Submitting_Agency = `Submitting Agency`)
wild_bird <- wild_bird %>% rename(Date_Collected = `Collection Date`)
mammal <- mammal %>% rename(Date_Collected = `Date Collected`)

wild_bird$Species <- NA
mammal$Bird_Species <- NA 
mammal$WOAH_Classification <- NA 
mammal$Sampling_Method <- NA 
mammal$Submitting_Agency <- NA 
combined_mammal_bird <- rbind(mammal, wild_bird)


#bind data together in new data file

combined_shp <- left_join(co_shapefile, combined_mammal_bird, by = "GEOID", relationship = "many-to-many")
#summary(mammal_shp)
head(combined_shp)


#Mutate date format for subsetting and mapping
combined_shp <- combined_shp %>%
  mutate(`Date_Collected` = as.Date(`Date_Collected`, format = "%m/%d/%Y")) #mutate date collected format
combined_shp <- combined_shp %>%
  mutate(`Date Detected` = as.Date(`Date Detected`, format = "%m/%d/%Y")) #mutate date detected format

#Create subset of data where date collected is Jan 1 2024 or sooner
combined_subset <- combined_shp %>%
  filter(`Date_Collected` >= as.Date("2024-01-01"))

#check CRS
#st_crs(mammal_shp) #CRS is 4269, need 4326 for leaflet

#create centroids for each county using CRS 4236
combined_subset$centroids <- st_transform(combined_subset, 4326) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()

#Extract latitude and longitude
combined_subset$lat <- st_coordinates(combined_subset$centroids)[,2]  # Extract latitude (Y)
combined_subset$lng <- st_coordinates(combined_subset$centroids)[,1]  # Extract longitude (X)


cat_data <- combined_subset %>% filter(Species == "Domestic cat")
mammal_data <- combined_subset %>% filter(!is.na(Species) & Species != "Domestic cat")
bird_data <- combined_subset %>% filter(!is.na(Bird_Species))


#create colored markers
mammal_data$markerColor <- case_when(
  mammal_data$Species %in% c("Virginia opossum", "American mink", "American marten:", "North American river otter", "Fisher", "Ermine", 
                             "Striped skunk", "Raccoon", "Skunk (unidentified)") ~ "green",
  mammal_data$Species %in% c("Black rat", "Eastern gray squirrel", "House mouse", "Deer mouse", "Prairie vole", "Desert cottontail", "Abert's Squirrel") ~ "orange" , 
  mammal_data$Species %in% c("Bobcat", "Mountain lion", "Serval", "Tiger", "African lion", "Bengal tiger", "Savannah cat", "Canada lynx", "Hybrid tiger (Panthera)", "Geoffroy's cat", "Eurasian lynx", "Amur Leopard") ~ "red",
  mammal_data$Species %in% c("Harbor seal", "Bottlenose dolphin", "Grey seal") ~ "lightblue",
  mammal_data$Species %in% c("Red fox", "Fox sp.", "Coyote") ~ "lightred",
  mammal_data$Species %in% c("Polar bear", "American black bear", "Grizzly bear", "Kodiak bear") ~ "brown",
  TRUE ~ "gray"  # Default color
)

cat_data$markerColor <- case_when(
  cat_data$Species %in% c("Domestic cat") ~ "purple",
  TRUE ~ "gray"  # Default color
)

bird_data$markerColor <- case_when(
  bird_data$WOAH_Classification %in% c("Captive wild bird") ~ "orange",
  bird_data$WOAH_Classification %in% c("Wild bird") ~ "yellow",
  TRUE ~ "gray"  # Default color
)


colored_markers_mammal <- awesomeIcons(
  icon = "exclamation-sign", 
  iconColor = "white",
  markerColor = mammal_data$markerColor 
)

colored_markers_cat <- awesomeIcons(
  icon = "paw", 
  library = "fa",
  iconColor = "white",
  markerColor = cat_data$markerColor 
)

colored_markers_bird <- awesomeIcons(
  icon = "exclamation-sign", 
  iconColor = "white",
  markerColor = bird_data$markerColor 
)

#Create map using leaflet

mammalbirdmap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  setView(lng =-98.58, lat =39.83 , zoom = 4) %>%
  addAwesomeMarkers(data = mammal_data,
                    lng = ~lng, 
                    lat = ~lat,
                    group = "Wild Mammals",
                    icon = colored_markers_mammal,
                    popup = paste("County:", mammal_data$County.x, "<br>",
                                  "Species:", mammal_data$Species, "<br>",
                                  "HPAI Strain:", mammal_data$`HPAI Strain`, "<br>",
                                  "Date Collected:", mammal_data$`Date_Collected`, "<br>",
                                  "Date Detected:", mammal_data$`Date Detected`),
                    clusterOptions = markerClusterOptions()) %>%
  addAwesomeMarkers(data = bird_data,
                    lng = ~lng, 
                    lat = ~lat,
                    group = "Wild Birds",
                    icon = colored_markers_bird,
                    popup = paste("County:", bird_data$County.x, "<br>",
                                  "Species:", bird_data$Bird_Species, "<br>",
                                  "HPAI Strain:", bird_data$`HPAI Strain`, "<br>",
                                  "Date Collected:", bird_data$`Date_Collected`, "<br>",
                                  "Date Detected:", bird_data$`Date Detected`),
                    clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    overlayGroups = c("Wild Mammals", "Wild Birds"),
    options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
  )

mammalbirdmap


catmap<- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
  addAwesomeMarkers(data = cat_data,
                    lng = ~lng, 
                    lat = ~lat,
                    group = "Cats",
                    icon = colored_markers_cat,
                    popup = paste("County:", cat_data$County.x, "<br>",
                                  "Species:", cat_data$Species, "<br>",
                                  "HPAI Strain:", cat_data$`HPAI Strain`, "<br>",
                                  "Date Collected:", cat_data$`Date_Collected`, "<br>",
                                  "Date Detected:", cat_data$`Date Detected`))

catmap




#####CREATE DASHBOARD#####

require(shiny)
require(shinydashboard)

#Dashboard outputs
server <- function(input, output, session) {
  
  # First Leaflet Map
  output$mammalbirdmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
      addAwesomeMarkers(data = mammal_data,
                        lng = ~lng, 
                        lat = ~lat,
                        group = "Wild Mammals",
                        icon = colored_markers_mammal,
                        popup = paste("County:", mammal_data$County.x, "<br>",
                                      "Species:", mammal_data$Species, "<br>",
                                      "HPAI Strain:", mammal_data$`HPAI Strain`, "<br>",
                                      "Date Collected:", mammal_data$`Date_Collected`, "<br>",
                                      "Date Detected:", mammal_data$`Date Detected`),
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(data = bird_data,
                        lng = ~lng, 
                        lat = ~lat,
                        group = "Wild Birds",
                        icon = colored_markers_bird,
                        popup = paste("County:", bird_data$County.x, "<br>",
                                      "Species:", bird_data$Bird_Species, "<br>",
                                      "HPAI Strain:", bird_data$`HPAI Strain`, "<br>",
                                      "Date Collected:", bird_data$`Date_Collected`, "<br>",
                                      "Date Detected:", bird_data$`Date Detected`),
                        clusterOptions = markerClusterOptions()) %>%
      addLayersControl(
        overlayGroups = c("Wild Mammals", "Wild Birds"),
        options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
      )
  })
  
  # Second Leaflet Map
  output$catmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
      addAwesomeMarkers(data = cat_data,
                        lng = ~lng, 
                        lat = ~lat,
                        group = "Cats",
                        icon = colored_markers_cat,
                        popup = paste("County:", cat_data$County.x, "<br>",
                                      "Species:", cat_data$Species, "<br>",
                                      "HPAI Strain:", cat_data$`HPAI Strain`, "<br>",
                                      "Date Collected:", cat_data$`Date_Collected`, "<br>",
                                      "Date Detected:", cat_data$`Date Detected`))
  })
}


header <- dashboardHeader(
  title = "H5N1 Mapping Project"
)


body <- dashboardBody(
  fluidRow(
    box(title = "Where has H5N1 been Detected in Wild Birds and Mammals?", solidHeader = TRUE,
        leafletOutput("mammalbirdmap")),
    box(title = "Where has H5N1 been Detected in Cats?", solidHeader = TRUE,
        leafletOutput("catmap"))
  ))



ui <- dashboardPage(
  header,
  dashboardSidebar(),
  body
)


shinyApp(ui, server)


