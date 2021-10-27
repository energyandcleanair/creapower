library(readxl)

thailand.iso2s <- function(){
  'TH'
}

#' Collect generation data from Thailand's Energy Policy and Planning Office (EPPO)
#'
#' @param date_from 
#' @param date_to 
#'
#' @return
#' @export
#'
#' @examples

thailand.collect_generation <- function(date_from){
  url <- 'http://www.eppo.go.th/epposite/images/Energy-Statistics/energyinformation/Energy_Statistics/Electricity/T05_02_02-1.xls'
  download.file(url, 'thailand.xls', mode='wb')
  gen <- read_excel('thailand.xls', skip=5)
  months <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
  names(gen) <- c('Date','Hydro','Fuel Oil','Coal','Gas','Diesel','Imported','Renewables','Total')
  gen %>% select(!Total) %>%
    filter(Date %in% months) %>% 
    mutate(Year = rep(1986:(year(today())-1), each=12)) %>%
    pivot_longer(cols = c(-Date, -Year), names_to='Source', values_to='Output_GWh') %>%
    mutate(iso2='TH', region='Thailand', data_source='thailand', Date=paste(Date, Year), 
           across(Output_GWh, as.numeric)) %>%
  select(-Year) %>%
  filter(lubridate::my(Date) >= lubridate::ymd(date_from))
}

