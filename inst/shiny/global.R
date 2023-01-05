library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)
library(shinyURL)
library(rclipboard)
library(tidyr)

Sys.setenv("GCS_DEFAULT_BUCKET"=Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public"))
library(googleCloudStorageR)
library(creapower)

#####################
# Global variables
#####################
frequency <- c(
  # "Hourly"="hour",
  "Daily"="day",
  "Weekly"="week",
  "Monthly"="month",
  "Yearly"="year")

plot_types <- c("Area" = "area",
                "Area (share)" = "area_pct",
                "Lines" = "lines",
                "Bars" = "bar")

presets <- c(
  "Custom" = "custom",
  "Wind & solar contribution" = "windsolarmonth")

preset_params <- list(
  "windsolarmonth" = list(
    "frequency"="month",
    "sources"=c("Wind","Solar","Renewables"), #Renewable and Wind|Solar are exclusive
    "plot_type"="bar"
    # "date_from"="2016-01-01",
    # "date_to"=lubridate::today()
  )
)

# get data from api for last 5 years
current_year <- lubridate::year(lubridate::today())
downloaded_years <- (current_year - 4):current_year
api_data <- get_generation_api(date_from = sprintf("%s-01-01", current_year - 4)) %>%
  filter(!is.na(country)) # database has empty country and/or region

data_sources <- available_data_sources()
iso2s <- api_data %>% distinct(country) %>%
  mutate(iso2 = countrycode::countrycode(country, origin = 'country.name', 
                                         destination = 'iso2c', custom_match = c("Kosovo"="XK")))

sources <- names(palette_power())
countries <- c(iso2s$iso2) %>% `names<-`(iso2s$country)
countries['European Union'] <- 'EU'