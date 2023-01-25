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
                "Lines (yearly)" = "lines_yearly",
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

min_years <- c( # for dynamic date_from
  "entsoe"=2015,
  "EIA"=2018,
  "posoco_in"=2017,
  "BMRS"=2015,
  "japan"=2016,
  "EPPO"=2015,
  "southkorea_kr"=2016,
  "southafrica_za"=2018,
  "TEIAS"=2016,
  "iemop_ph"=2022,
  "newmweb_au"=2022,
  "nldc_vn"=2020,
  "wind_cn"=2014,
  "crea"=2022
)

# for gas tab
data_types <- c(
  "Fossil Gas"="fossil_gas",
  "Fossil Gas (Temperature Corrected)"="fossil_gas_temperature_corrected",
  "Electricity (Temperature Corrected)"="electricity_temperature_corrected")

# get power data from api for last 4 years
print("Getting power data")
current_year <- lubridate::year(lubridate::today())
downloaded_years <- (current_year - 3):current_year
api_data <- get_generation_api(date_from = sprintf("%s-01-01", current_year - 3), freq = 'daily,monthly') %>%
  filter(!is.na(country)) # database has empty country and/or region

data_sources <- available_data_sources()
iso2s <- api_data %>% distinct(country) %>%
  mutate(iso2 = countrycode::countrycode(country, origin = 'country.name',
                                         destination = 'iso2c', custom_match = c("Kosovo"="XK")))

sources <- names(palette_power())
countries <- c(iso2s$iso2) %>% `names<-`(iso2s$country)
countries['European Union'] <- 'EU'

# get gas demand data from api for last 2 years
gas_api_data <- get_gas_api(date_from = sprintf("%s-01-01", current_year - 1))

downloaded_years_gas <- (current_year - 1):current_year
iso2s_gas <- gas_api_data %>% distinct(region_id, country) %>%
  replace_na(list(country = 'European Union'))
countries_gas <- c(iso2s_gas$region_id) %>% `names<-`(iso2s_gas$country)
