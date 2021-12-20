safrica.iso2s <- function(){
  'ZA'
}

#' Collect generation data from Eskom
#'
#' @param date_from 
#'
#' @return
#' @export
#'
#' @examples

safrica.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSinHKRdPIEm3PJjt_23QMJLA4z8DK0WhG4G3WacOBLKG4JtLMfzoYD58KFHTt2xw/pub?gid=2040621015&single=true&output=csv'
  
  data <- read.csv(url)
  
  data %>% mutate(date = lubridate::mdy_hm(data$DateTime, tz = 'Africa/Johannesburg'),
                  Solar = PV + CSP,
                  Hydro = Hydro + Pumped.Water) %>%
    select(date, Hydro, Nuclear, Thermal, Wind, Solar) %>%
    pivot_longer(-date, names_to = 'source', values_to = 'output_mw') %>%
    mutate(duration_hours = 1, 
           iso2 = 'ZA', region = 'South Africa', data_source = 'south africa') %>%
    filter(date >= date_from,
           date <= date_to) %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours) 
}