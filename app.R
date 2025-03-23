#TEST H5N1 MAMMAL AND WILD BIRD MAP#
#Last updated: 3/19/2025
#Script function: read in, merge, and organize data, prepare data for mapping, create interactive map in leaflet/shiny

#packages#
#install.packages(data.table)
#install.packages("dpylr")
#install.packages("leaflet")
#install.packages("shiny")
#install.packages("shinydashboard)

#libraries#
library(data.table)
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)

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

#rename columns in wild_bird and mammal to prepare for data binding
wild_bird <- wild_bird %>% rename(Bird_Species = `Bird Species`)
wild_bird <- wild_bird %>% rename(WOAH_Classification = `WOAH Classification`)
wild_bird <- wild_bird %>% rename(Sampling_Method = `Sampling Method`)
wild_bird <- wild_bird %>% rename(Submitting_Agency = `Submitting Agency`)
wild_bird <- wild_bird %>% rename(Date_Collected = `Collection Date`)
mammal <- mammal %>% rename(Date_Collected = `Date Collected`)

#create blank columns in wild_bird and mammal to prepare for data binding
wild_bird$Species <- NA
mammal$Bird_Species <- NA 
mammal$WOAH_Classification <- NA 
mammal$Sampling_Method <- NA 
mammal$Submitting_Agency <- NA 

#bind data together in a new file
combined_mammal_bird <- rbind(mammal, wild_bird)


#add county-level geography by joining combined_mammal_bird with county shapefile
combined_shp <- left_join(co_shapefile, combined_mammal_bird, by = "GEOID", relationship = "many-to-many")
#summary(mammal_shp)
head(combined_shp)


#Mutate date format for subsetting and mapping
combined_shp <- combined_shp %>%
  mutate(`Date_Collected` = as.Date(`Date_Collected`, format = "%m/%d/%Y")) #mutate date collected format
combined_shp <- combined_shp %>%
  mutate(`Date Detected` = as.Date(`Date Detected`, format = "%m/%d/%Y")) #mutate date detected format


#check CRS
#st_crs(mammal_shp) #CRS is 4269, need 4326 for leaflet

#create centroids for each county using CRS 4236
combined_shp$centroids <- st_transform(combined_shp, 4326) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()

#Extract latitude and longitude
combined_shp$lat <- st_coordinates(combined_shp$centroids)[,2]  # Extract latitude (Y)
combined_shp$lng <- st_coordinates(combined_shp$centroids)[,1]  # Extract longitude (X)

#add colors to markers

combined_shp$markerColor <- case_when(
  combined_shp$Species %in% c("Virginia opossum", "American mink", "American marten", "North American river otter", "Fisher", "Ermine", 
                              "Striped skunk", "Raccoon", "Skunk (unidentified)") ~ "green",
  combined_shp$Species %in% c("Black rat", "Eastern gray squirrel", "House mouse", "Deer mouse", "Prairie vole", "Desert cottontail", "Albert's Squirrel", "Abert's Squirrel", "Abert's squirrel") ~ "orange" , 
  combined_shp$Species %in% c("Bobcat", "Mountain lion", "Serval", "Tiger", "African lion", "Bengal tiger", "Savannah cat", "Canada lynx", "Hybrid tiger (Panthera)", "Geoffroy's cat", "Eurasian lynx", "Amur Leopard") ~ "red",
  combined_shp$Species %in% c("Harbor seal", "Bottlenose dolphin", "Grey seal") ~ "lightblue",
  combined_shp$Species %in% c("Red Fox", "Red fox", "Fox sp.", "Coyote") ~ "pink",
  combined_shp$Species %in% c("Polar bear", "American black bear", "Grizzly bear", "Kodiak bear") ~ "brown",
  combined_shp$Species %in% c("Domestic cat") ~ "purple",
  combined_shp$WOAH_Classification %in% c("Captive wild bird", "Wild bird") ~ "purple",
  TRUE ~ "gray"  # Default color
)


cat_data <- combined_shp %>% filter(Species == "Domestic cat")

#create bird_data and mammal_data for separate map layer
bird_data <- combined_shp %>% filter(!is.na(Bird_Species))
mammal_data <- combined_shp %>% filter(!is.na(Species) & Species != "Domestic cat")



colored_markers_mammal<- awesomeIcons(
  icon = "exclamation-sign", 
  iconColor = "white",
  markerColor = ~markerColor 
)


colored_markers_cat <- awesomeIcons(
  icon = "paw", 
  library = "fa",
  iconColor = "white",
  markerColor = cat_data$markerColor 
)

#Create legend names and colors for mammal_bird
category_colors <- c(
  "Other Mammal" = "green",
  "Rodent" = "orange",
  "Big Cat" = "red",
  "Water Mammal" = "lightblue",
  "Canid" = "pink", 
  "Bear" = "brown",
  "Bird" = "purple"
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
                    icon = awesomeIcons(
                      icon = "exclamation-sign",
                      iconColor = "white",
                      markerColor = "purple"),
                    popup = paste("County:", bird_data$County.x, "<br>",
                                  "Species:", bird_data$Bird_Species, "<br>",
                                  "HPAI Strain:", bird_data$`HPAI Strain`, "<br>",
                                  "Date Collected:", bird_data$`Date_Collected`, "<br>",
                                  "Date Detected:", bird_data$`Date Detected`),
                    clusterOptions = markerClusterOptions()) %>%
  addLegend(
    position = "bottomright",
    title = "Animal Types",
    colors = unname(category_colors), # Manually define colors
    labels = names(category_colors), # Labels for legend
    opacity = 1) %>%
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

#Dashboard outputs
server <- function(input, output, session){
  output$dynamic_map <- renderLeaflet({
    if (input$animal_tabs == "wild_animals"){
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
                          icon = awesomeIcons(
                            icon = "exclamation-sign",
                            iconColor = "white",
                            markerColor = "purple"),
                          popup = paste("County:", bird_data$County.x, "<br>",
                                        "Species:", bird_data$Bird_Species, "<br>",
                                        "HPAI Strain:", bird_data$`HPAI Strain`, "<br>",
                                        "Date Collected:", bird_data$`Date_Collected`, "<br>",
                                        "Date Detected:", bird_data$`Date Detected`),
                          clusterOptions = markerClusterOptions()) %>%
        addLegend(
          position = "bottomright",
          title = "Animal Types",
          colors = unname(category_colors), # Manually define colors
          labels = names(category_colors), # Labels for legend
          opacity = 1) %>%
        addLayersControl(
          overlayGroups = c("Wild Mammals", "Wild Birds"),
          options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
        )
    } else if (input$animal_tabs == "Pets") {
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
    } else {
      NULL
    }
  })
  output$wild_birds_value <- renderValueBox({
    valueBox(
      paste0(nrow(bird_data)), "Detections in Wild Birds", icon = icon("list"),
      color = "blue", width = 12
    )
  })
  
  output$wild_mammals_value <- renderValueBox({
    valueBox(
      paste0(nrow(mammal_data)), "Detections in Wild Mammals", icon = icon("list"),
      color = "blue", width = 12
    )
  })
  
  output$pets_value <- renderValueBox({
    valueBox(
      paste0(nrow(cat_data)), "Detections in Pets", icon = icon("list"),
      color = "blue", width = 12
    )
  })
}


header <- dashboardHeader(
  title = span("H5N1 Mapping Project", style = "white-space: nowrap; overflow: visible; font-size: 18px;")
)

dashboardSidebar<- dashboardSidebar(
  sidebarMenu(
    menuItem(HTML("&nbsp;&nbsp;H5N1 Data and Maps"), tabName = "h5n1_data", icon = icon("map")),
    menuItem(HTML("&nbsp;&nbsp;Information and FAQs"), tabName = "faqs", icon = icon("circle-question")),
    menuItem(HTML("&nbsp;&nbsp;H5N1 News"), tabName = "news", icon = icon("newspaper")),
    menuItem(HTML("&nbsp;&nbsp;About"), tabName = "about", icon = icon("circle-info"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "h5n1_data",
            fluidRow(
              column(12,
                     tabBox(id = "animal_tabs", width = NULL,
                            tabPanel("Wild Animals", value = "wild_animals"),
                            tabPanel("Pets", value = "Pets")
                     )
              )
            ),
            fluidRow(
              column(8,
                     leafletOutput("dynamic_map", height = "500px")
              ),
              column(4,
                     conditionalPanel(
                       condition = "input.animal_tabs == 'wild_animals'",
                       fluidRow(
                         column(12, valueBoxOutput("wild_birds_value", width = 12)),
                         column(12, valueBoxOutput("wild_mammals_value", width = 12))
                       )
                     ),
                     conditionalPanel(
                       condition = "input.animal_tabs == 'Pets'",
                       column(12, valueBoxOutput("pets_value", width = 12))
                     )
              )
            )
    ),
    
    tabItem(tabName = "faqs"),
    
    tabItem(tabName = "news"),
    
    tabItem(tabName = "about")
  )
)



ui <- dashboardPage(
  header,
  dashboardSidebar,
  body
)


shinyApp(ui, server)