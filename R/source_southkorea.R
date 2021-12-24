southkorea.iso2s <- function(){
  'KR'
}

#' Collect generation data from Korea Electric Power Corporation (KEPCO)
#'
#' @param date_from 
#'
#' @return
#' @export
#'
#' @examples

southkorea.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSinHKRdPIEm3PJjt_23QMJLA4z8DK0WhG4G3WacOBLKG4JtLMfzoYD58KFHTt2xw/pub?gid=2036731479&single=true&output=csv'
  
  data <- read.csv(url)
  
  data %>% rename(Oil = Diesel, Renewables = Renewable, 'Fossil Gas' = Gas) %>%
    mutate(Coal = Hard.coal + Bituminous.coal, 
           date = strptime(paste("01", Month, Year), "%d %b %Y")) %>%
    select(date, Hydro, Coal, Oil, 'Fossil Gas', Composite, Nuclear, Renewables) %>%
    pivot_longer(-date, names_to = 'source', values_to = 'output_gwh') %>%
    mutate(duration_hours = lubridate::days_in_month(date)*24, 
           output_mw = output_gwh*1000 / duration_hours,
           iso2 = 'KR', region = 'South Korea', data_source = 'south korea') %>%
    filter(date >= date_from, 
           date <= date_to) %>% 
    select(iso2, region, data_source, date, source, output_mw, duration_hours) 
}

# skorea.read_spreadsheet <- function(){
#   url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSinHKRdPIEm3PJjt_23QMJLA4z8DK0WhG4G3WacOBLKG4JtLMfzoYD58KFHTt2xw/pub?gid=2036731479&single=true&output=csv'
#   
#   data <- read.csv(url)
#   
#   data %>% rename(Oil = Diesel, Renewables = Renewable) %>%
#     mutate(Coal = Hard.coal + Bituminous.coal, 
#            date = strptime(paste("01", Month, Year), "%d %b %Y")) %>%
#     select(date, Hydro, Coal, Oil, Gas, Composite, Nuclear, Renewables) %>%
#     pivot_longer(-date, names_to = 'source', values_to = 'output_gwh') %>%
#     mutate(duration_hours = lubridate::days_in_month(date)*24, 
#            output_mw = output_gwh*1000 / duration_hours,
#            iso2 = 'KR', region = 'South Korea', data_source = 'south korea') %>%
#     filter(date >= date_from, 
#            date <= date_to) %>% 
#     select(iso2, region, data_source, date, source, output_mw, duration_hours) -> test
#     
#   
# }