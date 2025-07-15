# load packages

pacman::p_load(
  dplyr, data.table, sf, janitor, lubridate, zoo, stringr, rmapshaper, shiny, leaflet,
  bslib, fontawesome, googlesheets4
)

# loading data + shapefile

mammal <- fread("data/mammal.csv")
wild_bird <- fread("data/bird_wild.csv")
state_fp <- fread("data/us-state-codes_ncei-to-fips.csv")
co_shapefile <- st_read("shapefiles/cb_2018_us_county_500k.shp")

# normalizing fps
state_fp <- state_fp |>
  mutate(FIPS_code = str_pad(as.character(FIPS_code), width = 2, pad = "0"))

mammal$state_fp <- state_fp$FIPS_code[match(mammal$State, state_fp$state_name)]
wild_bird$state_fp <- state_fp$FIPS_code[match(wild_bird$State, state_fp$state_name)]
co_shapefile <- co_shapefile |> rename(County = NAME)

mammal$GEOID <- co_shapefile$GEOID[match(paste(mammal$state_fp, mammal$County, sep = "_"),
                                         paste(co_shapefile$STATEFP, co_shapefile$County, sep = "_"))]

wild_bird$GEOID <- co_shapefile$GEOID[match(paste(wild_bird$state_fp, wild_bird$County, sep = "_"),
                                            paste(co_shapefile$STATEFP, co_shapefile$County, sep = "_"))]

# merging columns

wild_bird <- wild_bird |> rename(
  Bird_Species = `Bird Species`,
  WOAH_Classification = `WOAH Classification`,
  Sampling_Method = `Sampling Method`,
  Submitting_Agency = `Submitting Agency`,
  Date_Collected = `Collection Date`
)
mammal <- mammal |> rename(Date_Collected = `Date Collected`)

# missing columns

wild_bird$Species <- NA
mammal$Bird_Species <- NA 
mammal$WOAH_Classification <- NA 
mammal$Sampling_Method <- NA 
mammal$Submitting_Agency <- NA 

# combining files

combined <- rbind(mammal, wild_bird)

combined <- left_join(co_shapefile, combined, by = "GEOID", relationship = "many-to-many") |>
  mutate(
    Date_Collected = as.Date(Date_Collected, format = "%m/%d/%Y"),
    `Date Detected` = as.Date(`Date Detected`, format = "%m/%d/%Y")
  ) |>
  st_transform(4326) |>
  mutate(centroids = st_centroid(geometry),
         lat = st_coordinates(centroids)[, 2],
         lng = st_coordinates(centroids)[, 1])

# color assignment

combined$markerColor <- case_when(
  combined$Species %in% c("Virginia opossum", "American mink", "Ermine", "Raccoon") ~ "green",
  combined$Species %in% c("House mouse", "Deer mouse") ~ "orange",
  combined$Species %in% c("Tiger", "Bobcat") ~ "red",
  combined$Species %in% c("Harbor seal") ~ "lightblue",
  combined$Species %in% c("Coyote", "Red fox") ~ "pink",
  combined$Species %in% c("American black bear") ~ "brown",
  combined$Species %in% c("Domestic cat") ~ "purple",
  combined$WOAH_Classification %in% c("Captive wild bird", "Wild bird") ~ "purple",
  TRUE ~ "gray"
)

# splitting data for map

cat_data <- combined |> filter(Species == "Domestic cat")
bird_data <- combined |> filter(!is.na(Bird_Species))
mammal_data <- combined |> filter(!is.na(Species) & Species != "Domestic cat")

# poultry
poultry <- read.csv("poultry/data/poultry.csv") |>
  clean_names() |>
  mutate(
    outbreak_date = mdy(outbreak_date),
    outbreak_my = as.yearmon(outbreak_date),
    county = toupper(county) |> str_trim() |> str_replace_all(" COUNTY$", "")
  ) |>
  filter(state != "Puerto Rico")

state_fp_pou <- read.csv("poultry/data/state_fips_master.csv") |>
  mutate(FIPS_code = str_pad(as.character(fips), 2, pad = "0"))

base_county <- read_sf("shapefiles/cb_2018_us_county_500k.shp") |>
  mutate(county = toupper(NAME) |> str_trim() %>% str_replace_all(" COUNTY$", "")) |>
  group_by(STATEFP, county) |>
  slice(1) |>
  ungroup()

poultry <- poultry |>
  left_join(state_fp_pou, by = c("state" = "state_name")) |>
  mutate(FIPS_code = str_pad(FIPS_code, width = 2, pad = "0"))

poultry_shp <- poultry |>
  left_join(
    base_county |> select(GEOID, county, STATEFP, geometry),
    by = c("county" = "county", "FIPS_code" = "STATEFP")
  ) %>%
  filter(!is.na(GEOID)) |>
  st_as_sf() |>
  st_transform(4326) 
  mutate(
    outbreak_date = as.Date(outbreak_my),
    year = year(outbreak_date),
    markerColor = case_when(
      year == 2022 ~ "#90D5FF",
      year == 2023 ~ "#90EE90",
      year == 2024 ~ "#BF77F6",
      year == 2025 ~ "#FFCCCB",
      TRUE ~ "#D3D3D3"
    ),
    geometry_centroid = st_centroid(geometry),
    lng = st_coordinates(geometry_centroid)[,1],
    lat = st_coordinates(geometry_centroid)[,2]
  )

poultry_shp$outbreak_date <- as.Date(poultry_shp$outbreak_date)

# combining again

all_combinations <- expand.grid(
  state = unique(poultry_shp$state),
  outbreak_my = unique(poultry_shp$outbreak_my)
)

# human

cases <- fread("data/cases.csv")
state_shp <- read_sf("shapefiles/mapbase.shp") |>
  st_transform(4326) |>
  ms_simplify(keep = 0.05)

human_map <- merge(state_shp, cases, by.x = "NAME", by.y = "State") |>
  rename(
    Total = `State Total`,
    Dairy = `Dairy Herds`,
    Poultry = `Poultry Farms and Culling Operations`,
    Other = `Other Animal Exposure`,
    Unknown = `Exposure Source Unknown`
  )

# livestock

livestock <- fread("data/livestock.csv", data.table = FALSE)

livestock_state <- livestock |>
  group_by(State) |>
  summarise(Count_Cases = n()) |>
  ungroup()

livestock_state$Count_Cases <- as.numeric(as.character(livestock_state$Count_Cases))

livestock_map <- merge(state_shp, livestock_state, by.x = "NAME", by.y = "State")

# save for next file

saveRDS(combined, "data/combined_shp.rds")
saveRDS(cat_data, "data/cat_data.rds")
saveRDS(bird_data, "data/bird_data.rds")
saveRDS(mammal_data, "data/mammal_data.rds")
saveRDS(poultry_shp, "data/poultry_shp.rds")
saveRDS(human_map, "data/human_cases_map.rds")
saveRDS(livestock_map, "data/livestock_map.rds")
