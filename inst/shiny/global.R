library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)
library(shinyURL)
library(rclipboard)

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

# Download cache for latest year
# lapply(data_sources, function(ds) creapower::data.download_cache(ds, lubridate::year(lubridate::today()), freq="day"))

# TODO remove this function, move to creapower 
get_generation <- function(date_from,
                           date_to=lubridate::today() + 1,
                           # data_source=available_data_sources(),
                           country=NULL,
                           freq='daily'){
  api_url <- 'https://api.energyandcleanair.org/power/generation' # ?date_from={date_from}&date_to={date_to}&format=csv'
  api_data <- httr::content(httr::GET(api_url, query = list(date_from = date_from, date_to = date_to, 
                                                            format = 'csv', country = country,
                                                            frequency = freq)))
  
  return(api_data)
}

# get data from api for last 5 years
current_year <- lubridate::year(lubridate::today())
api_data <- get_generation(date_from = sprintf("%s-01-01", current_year - 4)) # TODO change to creapower 

data_sources <- available_data_sources()
iso2s <- api_data %>% distinct(country) %>%
  mutate(iso2 = countrycode::countrycode(avail_countries, origin = 'country.name', 
                                         destination = 'iso2c', custom_match = c("Kosovo"="XK")))
sources <- names(palette_power())
countries <- c(iso2s$iso2) %>% `names<-`(iso2s$country)
countries['European Union'] <- 'EU'