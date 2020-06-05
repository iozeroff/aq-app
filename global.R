library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(MazamaLocationUtils)
library(AirSensor)
library(sf)
library(maps)
library(htmltools)
library(dplyr)
library(ggplot2)
library(formattable)
library(stringr)
library(shinybusy)
library(shinycssloaders)
library(tibble)

# TODO: Need to figure out what needs to be included for deployment from ophealthyair.
# Package not yet compiled.
devtools::load_all("C://Users/iozeroff/Data-Science/ophealthyair")
# Pull Request to add to AirSensor pending.
# TODO: Move these files into app.
source("C://Users/iozeroff/Data-Science/R-Projects/AirSensor/R/pat_outlierPlot.R")
source("C://Users/iozeroff/Data-Science/R-Projects/AirSensor/R/utils-outliers.R")
source("C://Users/iozeroff/Data-Science/R-Projects/useful/model-equation.R")
# # Initiate Async
# plan(multisession)
# Load Mazama Spatial Data.
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
setSpatialDataDir("data/spatial")
loadSpatialData("NaturalEarthAdm1")
loadSpatialData("EEZCountries")

# Set GG Plotting Theme
ggthemr::ggthemr(palette = "fresh")


# Prepare introText in HTML Formatting.
introText <- readLines('intro.txt') %>%  
  stringr::str_split(pattern = '\\n') %>% 
  # wrap a paragraph tag around each element in the list
  purrr::map(.f = p) %>% 
  purrr::reduce(.f = stringr::str_c, sep = " ")


# Set Limit to Number of Sensor that can be loaded at the same time.
sensor_limit <- 5
pas_retries <- 7
pas_countries <- c(
  "US",
  "IN",
  "LK"
)

# Download New Purple Air Synoptic Data:
# TODO: Fix pas_load timezone issue.
pas <- pas_createNew(
  countryCodes = pas_countries,
  lookbackDays = pas_retries
)

# TODO: Find efficient way to load all sensors.
# pas %<-% pas_createNew(
#     countryCodes = pas_countries,
#     lookbackDays = pas_retries
# )
          

# Currently Ingesting New taking about 1-minute.
# microbenchmark({
# pas <- pas_createNew(countryCodes = c("US"))},
# times = 5)

# Define Options for Choice Arguments 
calibration_opts <- list.files(path = "data/calibration-models") %>%
  stringr::str_remove(
    pattern = stringr::fixed(".rds", ignore_case = TRUE)
  ) %>%
  # Replace all dashes w spaces.
  stringr::str_replace_all("-", " ") %>% 
  stringr::str_to_title()

# AQI options
aqi_country_opts <- load_aqi_info() %>% 
  names() %>% 
  countrycode::countrycode(origin = "iso2c",
                           destination = "country.name")

# Download Options
temp_opts <- c("fahrenheit", "celsius")
column_opts <- c(
  "Date" = "date",
  "Time" = "time",
  "Particulate Matter 2.5" = "pm25",
  "Temperature" = "temperature",
  "Humidity" = "humidity",
  "Day of the Week" = "weekday",
  "AQI Category" = "aqi_category"
  )

# Columns to Include in Metadata Download Sheet
meta_columns <- c(
  "longitude",
  "latitude",
  "countryCode",       
  "stateCode",
  "timezone",
  "sensorManufacturer",
  "PurpleAirQC_algorithm"
)