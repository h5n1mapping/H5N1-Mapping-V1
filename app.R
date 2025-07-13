#DASHBOARD SCRIPT
#Last updated: 4/22/2025
#Script function: read in, merge, and organize data, prepare data for mapping, create interactive map in leaflet/shiny

#packages#
#install.packages("pacman")
#install.packages(data.table)
#install.packages("dpylr")
#install.packages("leaflet")
#install.packages("leaflet.extras")
#install.packages("shiny")
#install.packages("shinydashboard)
#install.packages("sf")
#install.packages("janitor")
#install.packages("plotly")
#install.packages("lubridate")
#install.packages("zoo")
#install.packages("tidyverse")


#libraries#
pacman::p_load(
  dplyr,
  data.table,
  sf,
  janitor,
  shiny,
  shinydashboard,
  plotly,
  lubridate,
  zoo,
  tidyverse,
  leaflet,
  leaflet.extras
)

#READ IN, MERGE, AND ORGANIZE MAMMAL AND WILD BIRD DETECTION DATA#

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
state_fp[state_fp$FIPS_code == 2, "FIPS_code"] <- "02"
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
category_colors1 <- c(
  "Other Mammal" = "green",
  "Rodent" = "orange",
  "Big Cat" = "red",
  "Water Mammal" = "lightblue",
  "Canid" = "pink", 
  "Bear" = "brown",
  "Bird" = "purple"
)


#Create map using leaflet

#mammalbirdmap <- leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron)%>%
#  setView(lng =-98.58, lat =39.83 , zoom = 4) %>%
#  addAwesomeMarkers(data = mammal_data,
#                    lng = ~lng, 
#                    lat = ~lat,
#                    group = "Wild Mammals",
#                    icon = colored_markers_mammal,
#                    popup = paste("County:", mammal_data$County.x, "<br>",
#                                  "Species:", mammal_data$Species, "<br>",
#                                  "HPAI Strain:", mammal_data$`HPAI Strain`, "<br>",
#                                  "Date Collected:", mammal_data$`Date_Collected`, "<br>",
#                                  "Date Detected:", mammal_data$`Date Detected`),
#                    clusterOptions = markerClusterOptions()) %>%
#  addAwesomeMarkers(data = bird_data,
#                    lng = ~lng, 
#                    lat = ~lat,
#                    group = "Wild Birds",
#                    icon = awesomeIcons(
#                      icon = "exclamation-sign",
#                      iconColor = "white",
#                      markerColor = "purple"),
#                    popup = paste("County:", bird_data$County.x, "<br>",
#                                  "Species:", bird_data$Bird_Species, "<br>",
#                                  "HPAI Strain:", bird_data$`HPAI Strain`, "<br>",
#                                  "Date Collected:", bird_data$`Date_Collected`, "<br>",
#                                  "Date Detected:", bird_data$`Date Detected`),
#                    clusterOptions = markerClusterOptions()) %>%
#  addLegend(
#    position = "bottomright",
#    title = "Animal Types",
#    colors = unname(category_colors), # Manually define colors
#    labels = names(category_colors), # Labels for legend
#    opacity = 1) %>%
#  addLayersControl(
#    overlayGroups = c("Wild Mammals", "Wild Birds"),
#    options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
#  )

#mammalbirdmap


#catmap<- leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron)%>%
#  setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
#  addAwesomeMarkers(data = cat_data,
#                    lng = ~lng, 
#                    lat = ~lat,
#                    group = "Cats",
#                    icon = colored_markers_cat,
#                    popup = paste("County:", cat_data$County.x, "<br>",
#                                  "Species:", cat_data$Species, "<br>",
#                                  "HPAI Strain:", cat_data$`HPAI Strain`, "<br>",
#                                  "Date Collected:", cat_data$`Date_Collected`, "<br>",
#                                  "Date Detected:", cat_data$`Date Detected`))

#catmap

#________________________________HUMAN CASES______________________________________

state_shapefile <- st_read("shapefiles/mapbase.shp")

state_shapefile <- st_transform(state_shapefile, crs = 4326) # crs may need to be changed

names(state_shapefile)

cases <- fread("data/cases.csv")

map <- merge(state_shapefile, cases, by.x = "NAME", by.y = "State")

# LAYERED MAP

# renaming

map <- map %>%
  rename("Total" = "State Total")

map <- map %>%
  rename("Dairy" = "Dairy Herds")

map <- map %>%
  rename("Poultry" = "Poultry Farms and Culling Operations")

map <- map %>%
  rename("Other" = "Other Animal Exposure")

map <- map %>%
  rename("Unknown" = "Exposure Source Unknown")


#colorcoding

map$Total <- as.numeric(as.character(map$Total))


human_col <- colorNumeric("Reds", 
                          domain = map$Total,
                          reverse = FALSE)

deaths_frame <- data.frame(
  name = c("Death1"),
  lat = c(30.5191),
  lng = c(-91.5209),
  info = c("Death"),
  date = c("January 6, 2025")
)

#basemap w location of interest

#human_map <- leaflet(options = leafletOptions(minZoom = 3)) %>% 
#  addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB.PositronNoLabels") %>%
#  setView(lng = -98.58, lat =39.83, zoom = 3) %>%
#  addPolygons(data = map,
#              color = "black",
#              weight = 1,
#              fillColor = ~human_col(Total),
#              fillOpacity = 0.7,
#              label = ~NAME,
#              group = "Human Cases",
#              highlightOptions = highlightOptions(weight = 3, color = "red", fillOpacity = 0.8),
#              popup = ~paste("<div style='font-size:14px;'><b>Total Human Cases:</b><span style='color:red;'>", Total, "</span><br>",
#                             "<b>Case # by Exposure Source:</b><br>",
#                             "Dairy:", Dairy, "<br>",
#                             "Poultry:", Poultry, "<br>",
#                             "Other or Unknown:", Other + Unknown, "</div>")) %>%
#  addLegend(pal = human_col,
#            values = map$Total,
#            title = "<b>Number of Human Cases</br>",
#            position = "bottomright") %>%
#  addScaleBar(position = "bottomleft") %>%
#  addSearchOSM() %>%
#  addCircleMarkers(data = deaths_frame,
#                   lng = ~lng,
#                   lat = ~lat,
#                   color = "red",
#                   radius = 5,
#                   fillOpacity = .8,
#                   popup = ~paste("<div style='text-align: center;'><b>", info, "</b><br>", date, "</div>"))

#human_map 

#________________________________POULTRY______________________________________
###############
###load data###
###############

#load poultry data
poultry <- read.csv(here::here("poultry", "data", "poultry.csv")) %>% janitor::clean_names()

#load shapefile
#same shapefile as used in mammal data cleaning
#eventually may be able to combine in one cleaning file
co_shapefile_pou <- read_sf(here::here("shapefiles", "cb_2018_us_county_500k.shp"))

#load FIPS id crosswalk
state_fp_pou <- read.csv(here::here("poultry", "data", "state_fips_master.csv"))

poultry <- poultry %>%
  mutate(
    outbreak_date = mdy(outbreak_date),
    outbreak_my = as.yearmon(outbreak_date),
    # outbreak_my_char = format(outbreak_date, "%b %Y"),
    # outbreak_date_2 = my(outbreak_my_char)
    county = toupper(county)
  )

#change FIPS code in state FP crosswalk to match county shapefile
state_fp_pou <- state_fp_pou %>%
  mutate(FIPS_code = case_when(
    fips == 1 ~ "01",
    fips == 2 ~ "02",
    fips == 4 ~ "04",
    fips == 5 ~ "05",
    fips == 6 ~ "06",
    fips == 8 ~ "08",
    fips == 9 ~ "09",
    TRUE ~ as.character(fips)
  ))


#merge county shapefile with state FP crosswalk by FIPS code
#this will put state name and county geometry in the same file
co_shapefile_pou1 <- co_shapefile_pou %>%
  left_join(state_fp_pou, by = c("STATEFP" = "FIPS_code")) %>%
  filter(!is.na(state_name)) %>% #remove territories and islands
  mutate(NAME = toupper(NAME)) %>%
  mutate(county_state = paste0(NAME,"_", state_name)) #join state and county name

#when both city and county are present, keep county
co_shapefile_pou2 <- co_shapefile_pou1 %>%
  group_by(county_state) %>%
  slice_min(LSAD) %>%
  ungroup() 

#update names of hypenanted counties or counties with spaces to match in shapefile and county outbreak data
co_shapefile_pou3 <- co_shapefile_pou2 %>%
  mutate(NAME = str_replace(NAME, "-", " ")) %>%
  mutate(NAME = case_when(
    GEOID == 51650 ~ "HAMPTON CITY",
    GEOID == 51810 ~ "VIRGINIA BEACH CITY",
    GEOID == 51510 ~ "ALEXANDRIA CITY",
    GEOID == 55109 ~ "SAINT CROIX",
    GEOID == 12027 ~ "DE SOTO",
    TRUE ~ NAME
  ))

poultry <- poultry %>%
  mutate(county = str_replace(county, "-", " ")) %>%
  filter(state != "Puerto Rico") #remove non-state data (for now, at least)

#merge shapefile with poultry data to add geometry
poultry_shp <- poultry %>%
  left_join(co_shapefile_pou3, by = c("county" = "NAME", "state" = "state_name")) %>%
  st_as_sf() %>% #change to sf object
  st_transform(4326) #change coordinate reference system to 4326 for leaflet

if (sum(is.na(poultry_shp$STATEFP)) != 0) {
  print("County names do not match")
  stop()
}

poultry_state_sum <- poultry_shp %>%
  group_by(state, outbreak_my) %>%
  summarize(num = n()) %>%
  mutate(mon_year = as.Date(outbreak_my)) %>%
  as.data.frame()

#Create dataframe with missing month rows filled in for each state
all_combinations <- expand.grid(
  state = unique(poultry_shp$state),
  outbreak_my = unique(poultry_shp$outbreak_my)
)

# Join with existing data
df_complete <- all_combinations %>%
  left_join(poultry_state_sum, by = c("state", "outbreak_my"))

df_complete <- df_complete %>%
  mutate(num = ifelse(is.na(num), 0, num))

#ensure dates match
df_complete <- df_complete %>%
  mutate(mon_year = as.Date(outbreak_my))



#map


#create centroids for each county using CRS 4236
poultry_shp$centroids <- poultry_shp %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()

#Extract latitude and longitude
poultry_shp$lat <- st_coordinates(poultry_shp$centroids)[,2]  # Extract latitude (Y)
poultry_shp$lng <- st_coordinates(poultry_shp$centroids)[,1]  # Extract longitude (X)



#add year only and marker color
poultry_shp <- poultry_shp %>%
  mutate(year = year(outbreak_my),
         markerColor = case_when(
           year == 2022 ~ "lightblue",
           year == 2023 ~ "lightgreen",
           year == 2024 ~ "purple",
           year == 2025 ~ "orange"
         ))


#Create legend names and colors for poultry
category_colors2 <- c(
  "2022" = "lightblue",
  "2023" = "lightgreen",
  "2024" = "purple",
  "2025" = "orange"
)

# poultry_map <- leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron)%>%
#   setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
#   addAwesomeMarkers(data = poultry_shp,
#                     lng = ~lng, 
#                     lat = ~lat,
#                     icon = colored_markers_poultry,
#                     clusterOptions = markerClusterOptions()) %>%
#   addLegend(
#     position = "bottomright",
#     title = "Year",
#     colors = unname(category_colors), # Manually define colors
#     labels = names(category_colors), # Labels for legend
#     opacity = 1) %>%
#   addLayersControl(
#     options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
#   )
# 
# 
# poultry_map

#####CREATE DASHBOARD#####

#Dashboard outputs
server <- function(input, output, session){
  
  output$dynamic_map <- renderLeaflet({
    if (input$animal_tabs == "Humans") {
      
      
      leaflet(options = leafletOptions(minZoom = 3)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB.PositronNoLabels") %>%
        setView(lng = -98.58, lat =39.83, zoom = 3) %>%
        addPolygons(data = map,
                    color = "black",
                    weight = 1,
                    fillColor = ~human_col(Total),
                    fillOpacity = 0.7,
                    label = ~NAME,
                    group = "Human Cases",
                    highlightOptions = highlightOptions(weight = 3, color = "red", fillOpacity = 0.8),
                    popup = ~paste("<div style='font-size:14px;'><b>Total Human Cases:</b><span style='color:red;'>", Total, "</span><br>",
                                   "<b>Case # by Exposure Source:</b><br>",
                                   "Dairy:", Dairy, "<br>",
                                   "Poultry:", Poultry, "<br>",
                                   "Other or Unknown:", Other + Unknown, "</div>")) %>%
        addLegend(pal = human_col,
                  values = map$Total,
                  title = "<b>Number of Human Cases</br>",
                  position = "bottomright") %>%
        addScaleBar(position = "bottomleft") %>%
        addCircleMarkers(data = deaths_frame,
                         lng = ~lng,
                         lat = ~lat,
                         color = "red",
                         radius = 5,
                         fillOpacity = .8,
                         popup = ~paste("<div style='text-align: center;'><b>", info, "</b><br>", date, "</div>"))
    } else if (input$animal_tabs == "Pets") {
      
      cat_data_filt <- cat_data %>% filter((`Date Detected` >= input$date_min) & (`Date Detected` <= input$date_max))
      
      
      leaflet(options = leafletOptions(minZoom = 3)) %>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
        addAwesomeMarkers(data = cat_data_filt,
                          lng = ~lng, 
                          lat = ~lat,
                          group = "Cats",
                          icon = colored_markers_cat,
                          popup = paste("County:", cat_data_filt$County.x, "<br>",
                                        "Species:", cat_data_filt$Species, "<br>",
                                        "HPAI Strain:", cat_data_filt$`HPAI Strain`, "<br>",
                                        "Date Collected:", cat_data_filt$`Date_Collected`, "<br>",
                                        "Date Detected:", cat_data_filt$`Date Detected`))
    } else if (input$animal_tabs == "Livestock") {
      leaflet(options = leafletOptions(minZoom = 3)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB.PositronNoLabels") %>%
        setView(lng = -98.58, lat =39.83, zoom = 3)
    } else if (input$animal_tabs == "wild_animals"){
      
      mammal_data <- mammal_data %>% filter((`Date Detected` >= input$date_min) & (`Date Detected` <= input$date_max))
      bird_data <- bird_data %>% filter((`Date Detected` >= input$date_min) & (`Date Detected` <= input$date_max))
      
      
      leaflet(options = leafletOptions(minZoom = 3)) %>%
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
          colors = unname(category_colors1), # Manually define colors
          labels = names(category_colors1), # Labels for legend
          opacity = 1) %>%
        addLayersControl(
          overlayGroups = c("Wild Mammals", "Wild Birds"),
          options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
        )
    } else if (input$animal_tabs == "poultry") {
      filtered_data <- poultry_shp %>% filter((outbreak_date >= input$date_min) & (outbreak_date <= input$date_max))
      
      colored_markers_poultry <- awesomeIcons(
        icon = "fa-solid fa-feather", 
        library = "fa",
        iconColor = "white",
        markerColor = filtered_data$markerColor
      )
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        setView(lng =-98.58, lat =39.83 , zoom = 3) %>%
        addAwesomeMarkers(data = filtered_data,
                          lng = ~lng, 
                          lat = ~lat,
                          icon = colored_markers_poultry,
                          popup = paste("County:", str_to_title(filtered_data$county), "<br>",
                                        "State:", filtered_data$state, "<br>",
                                        "Outbreak Date:", filtered_data$outbreak_date, "<br>",
                                        "Flock Type:", filtered_data$flock_type, "<br>",
                                        "Flock Size:", filtered_data$flock_size, "<br>",
                                        "Testing:", filtered_data$year),
                          clusterOptions = markerClusterOptions()) %>%
        addLegend(
          position = "bottomright",
          title = "Year",
          colors = unname(category_colors2), # Manually define colors
          labels = names(category_colors2), # Labels for legend
          opacity = 1) %>%
        addLayersControl(
          options = layersControlOptions(collapsed = FALSE)  # Keeps control panel open
        )
      
    } else {
      NULL
    }
  })
  output$wild_birds_value <- renderValueBox({
    
    bird_data <- bird_data %>% filter((`Date Detected` >= input$date_min) & (`Date Detected` <= input$date_max))
    
    valueBox(
      paste0(nrow(bird_data)), "Detections in Wild Birds", icon = icon("list"),
      color = "blue", width = 12
    )
  })
  
  output$wild_mammals_value <- renderValueBox({
    
    mammal_data <- mammal_data %>% filter((`Date Detected` >= input$date_min) & (`Date Detected` <= input$date_max))
    
    valueBox(
      paste0(nrow(mammal_data)), "Detections in Wild Mammals", icon = icon("list"),
      color = "blue", width = 12
    )
  })
  
  output$pets_value <- renderValueBox({
    
    cat_data_filt <- cat_data %>% filter((`Date Detected` >= input$date_min) & (`Date Detected` <= input$date_max))
    
    valueBox(
      paste0(nrow(cat_data_filt)), "Detections in Pets", icon = icon("list"),
      color = "blue", width = 12
    )
  })
  
  output$poultry_graph <- renderPlotly({
    state_data <- df_complete %>% filter(state == input$state)
    y_max <- max(state_data$num)
   
    
    if (y_max <=5) {
      y_options = list(title = "Number of Outbreaks",
                       tickvals = seq(from=0,to=y_max))
    }
    
    
    if (y_max >5) {
      y_options = list(title = "Number of Outbreaks")
    }
      
      plot_ly(
        data = state_data,
        x = ~mon_year,
        y = ~num,
        type = "bar",
        hovertemplate = paste("Month: %{x}","<br>Number of Outbreaks: %{y}<extra></extra>")
      ) %>%
        layout(yaxis = y_options,
               xaxis = list(title = ""),
               title = "\nH5N1 Poulty Outbreaks by State")
      
    
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
  fluidRow(column(2,
                  # sliderInput(
                  #  "year_slide",
                  #  "Select Date Range:",
                  #  min = as.Date("2022-02-08"),
                  #  max = as.Date(Sys.Date()),
                  #  value = c(as.Date("2022-02-08"), as.Date(Sys.Date())),
                  #  ticks = TRUE,
                  #  timeFormat = "%D",
                  #  dragRange = FALSE
                  # ))
                  dateInput(
                    "date_min",
                    "Select Start Date",
                    value = "2022-02-08",
                    min = "2022-02-08",
                    max = Sys.Date(),
                    format = "mm/dd/yyyy"
                  )),
           column(2,
                  dateInput(
                    "date_max",
                    "Select End Date",
                    value = Sys.Date(),
                    min = "2022-02-08",
                    max = Sys.Date(),
                    format = "mm/dd/yyyy"
                  ))
  ),
  tabItems(
    tabItem(tabName = "h5n1_data",
            fluidRow(
              column(12,
                     tabBox(id = "animal_tabs", width = NULL,
                            tabPanel("Humans", value = "Humans"),
                            tabPanel("Pets", value = "Pets"),
                            tabPanel("Livestock", value = "Livestock"),
                            tabPanel("Wild Animals", value = "wild_animals"),
                            tabPanel("Poultry", value = "poultry")
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
            ),
            div(style = "margin-top: 50px"),
            fluidRow(
              conditionalPanel(
                condition = "input.animal_tabs == 'poultry'",
                column(2,
                       selectInput("state", "Select State", choices = unique(poultry_state_sum$state))),
                column(6,
                       plotlyOutput("poultry_graph", height = "500px")
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


