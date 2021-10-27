thailand.iso2s <- function(){
  'TH'
}

#' Collect generation data from Thailand's Energy Policy and Planning Office (EPPO)
#'
#' @param date_from 
#'
#' @return
#' @export
#'
#' @examples

thailand.collect_generation <- function(date_from){
  url <- 'http://www.eppo.go.th/epposite/images/Energy-Statistics/energyinformation/Energy_Statistics/Electricity/T05_02_02-1.xls'
  # download.file(url, 'thailand.xls', mode='wb')
  gen <- rio::import(file = url)
  months <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
  names(gen) <- c('date','hydro','fuel oil','coal','gas','diesel','imported','renewables','total')
  gen %>% select(!total) %>%
    filter(date %in% months) %>% 
    mutate(year = rep(1986:(year(today())-1), each=12)) %>%
    pivot_longer(cols = c(-date, -year), names_to='source', values_to='output_gwh') %>%
    mutate(iso2='TH', region='Thailand', data_source='thailand', date=paste(date, year), 
           across(output_gwh, as.numeric)) %>%
  select(-year) %>%
  filter(lubridate::my(date) >= lubridate::ymd(date_from))
}

