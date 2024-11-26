## ---------------------------
##
## ScaleMaterials Mining Database
##
## Author: Melinda Wong
##
## Date Created: 11-14-2024
##
## LPPS 4735: Experiential Social Entrepreneurship
##
## ---------------------------

## ---------------------------
## Load Data & Libraries

library(dplyr)
library(stringr)
library(sf)
library(leaflet)

wv_mines <- read.csv("data/West Virginia Mines.csv")
va_mines <- read.csv("data/va_mines.csv")
va_ownership <- read.csv("data/va_mines_ownership.csv")

## ---------------------------
## Clean West Virginia Data

# Converting WV data into a mappable format
wv_mines_map <- st_as_sf(wv_mines, coords = c("x", "y"), crs = 3857) %>% 
  st_transform(crs = 4326)
wv_mines_map$longitude <- st_coordinates(wv_mines_map)[, 1]
wv_mines_map$latitude <- st_coordinates(wv_mines_map)[, 2]

# Selecting Columns, Cleaning Names, Recoding Data
wv_mines_cleaned <- wv_mines_map %>% 
  select(c("PA_NAME", "PU_NAME", "COUNTY", "WATERSHED", "MINE_TYPE", "ORE_TYPES", 
           "OWNER_PRIVATE", "OWNER_STATE", "OWNER_NATIONAL", "DATE_PREPARED", "PROB_TY_NAME", 
           "PROGRAM", "UNFD_METERS", "UNFD_COST", "FUND_METERS", "FUND_COST", "COMP_METERS",
           "COMP_COST", "TOTAL_UNITS", "longitude", "latitude", "Owner Name", "Additional Ownership Notes", 
           "Property Assessment Report Link")) %>% 
  rename(problem_area = "PA_NAME", 
         planning_unit = "PU_NAME", 
         county = "COUNTY", 
         watershed = "WATERSHED", 
         mine_type = "MINE_TYPE", 
         ore_type = "ORE_TYPES", 
         owner_private = "OWNER_PRIVATE", 
         owner_state = "OWNER_STATE", 
         owner_national = "OWNER_NATIONAL", 
         date_prepared = "DATE_PREPARED", 
         problem_type = "PROB_TY_NAME",
         program = "PROGRAM", 
         unfunded_meters = "UNFD_METERS", 
         unfunded_cost = "UNFD_COST", 
         funded_meters = "FUND_METERS",
         funded_cost = "FUND_COST",
         completed_meters = "COMP_METERS",
         completed_costs = "COMP_COST", 
         total_units = "TOTAL_UNITS") %>% 
  # Recode unclear variables
  mutate(mine_type = recode(mine_type, "U" = "Underground", "B" = "Both", "S" = "Surface", "P" = "Processing")) %>% 
  mutate(ore_type = recode(ore_type, "GCZSL" = "Gold, Copper, Zinc, Silver, and Lead", "GS" = "Gold and Silver", "1" = "Coal", "1Y" = "Coal")) %>% 
  mutate(program = recode(program, 
                          "SGA" = "Pre-SMCRA Coal State/Tribe",
                          "BIL" = "Bipartisan Infrastructure Law", 
                          "AMA" = "State AMD Set-Aside Program",
                          "SEA" = "State Emergency Program",
                          "WCA" = "Watershed Cooperative Agreement Funding",
                          "CLA" = "Appalachian Clean Streams Initiative Program",
                          "AFS" = "Alternate Funding Source(s)")) %>%
  # Make county names have uniform format
  mutate(county = ifelse(str_detect(county, "County$"), county, paste(county, "County"))) %>%
  # Format dates to allow for compatability with inputs
  mutate(date_prepared = format(as.Date(date_prepared, format = "%m/%d/%Y"), "%Y-%m-%d")) %>% 
  # Change case of text for readability
  mutate(problem_area = str_to_title(problem_area)) %>% 
  mutate(planning_unit = str_to_title(planning_unit)) %>% 
  mutate(county = str_to_title(county)) %>% 
  mutate(watershed = str_to_title(watershed)) %>% 
  mutate(`Owner Name` = str_to_title(`Owner Name`))

## ---------------------------
## Cleaning Virginia Data

# Adding ownership data from reduced data set
virginia_mines <- left_join(va_mines, va_ownership, by = 'OBJECTID') %>% 
  select(-c('OBJECTID'))

# Transforming data into mappable format
virginia_mines_map <- st_as_sf(virginia_mines, coords = c("x", "y"), crs = 3857)
virginia_mines_map$longitude <- st_coordinates(virginia_mines_map)[, 1]
virginia_mines_map$latitude <- st_coordinates(virginia_mines_map)[, 2]

# Cleaning Names and Recoding Data
virginia_mines_cleaned <- virginia_mines_map %>%
  # Format dates to allow for compatability with inputs
  mutate(InspectDate = format(as.Date(InspectDate, format = "%m/%d/%Y"), "%Y-%m-%d")) %>% 
  mutate(RecordUpdatedDate = format(as.Date(RecordUpdatedDate, format = "%m/%d/%Y"), "%Y-%m-%d")) %>% 
  # Change case of text for readability
  mutate(Mineral = str_to_title(Mineral)) %>%
  # Add County to county name
  mutate(County = paste(County, "County"))


## ---------------------------
## Export Data
st_write(wv_mines_cleaned, "data/wv_mines_cleaned.gpkg", append = F)
st_write(virginia_mines_cleaned, "data/va_mines_cleaned.gpkg", append = F)
