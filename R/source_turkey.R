turkey.iso2s <- function(){
  'TR'
}

#' Collect generation data from Turkis Electricity Transmission Corporation (TEIAS)
#'
#' @param date_from 
#'
#' @return
#' @export
#'
#' @examples

turkey.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSinHKRdPIEm3PJjt_23QMJLA4z8DK0WhG4G3WacOBLKG4JtLMfzoYD58KFHTt2xw/pub?gid=2034580308&single=true&output=csv'
  
  data <- read.csv(url)
  
  data %>% rename(Coal = Total, Oil = Total.1, Gas = Natural.Gas, Waste = Renew.and.Wastes,
                  Hydro = HYDRO, Geothermal = GEOTHERMAL, Wind = WIND, Solar = SOLAR) %>%
    mutate(date = strptime(paste("01", Month, Year), "%d %b %Y")) %>%
    select(date, Hydro, Coal, Oil, Gas, Waste, Geothermal, Wind, Solar) %>%
    pivot_longer(-date, names_to = 'source', values_to = 'output_gwh') %>%
    mutate(duration_hours = lubridate::days_in_month(date)*24, 
           output_mw = output_gwh*1000 / duration_hours,
           iso2 = 'TR', region = 'Turkey', data_source = 'turkey') %>%
    filter(date >= date_from,
           date <= date_to) %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours) 
}