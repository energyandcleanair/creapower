library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)

Sys.setenv("GCS_DEFAULT_BUCKET"=Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public"))
library(googleCloudStorageR)
library(creapower)

#####################
# Global variables
#####################
data_sources <- available_data_sources()
iso2s <- available_iso2s()
sources <- names(palette_power())
countries <- c(iso2s$iso2) %>% `names<-`(iso2s$region)
frequency <- c("Hourly"="hour", "Daily"="day", "Weekly"="week", "Monthly"="month")

plot_types <- c("Lines" = "lines",
                "Area" = "area",
                "Area (share)" = "area_pct",
                "Monthly" = "monthly_bar")

# Download cache for latest year
lapply(data_sources, function(ds) creapower::data.download_cache(ds, lubridate::year(lubridate::today())))
