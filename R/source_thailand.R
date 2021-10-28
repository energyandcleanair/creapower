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

thailand.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1, iso2s=NULL){
  url <- 'http://www.eppo.go.th/epposite/images/Energy-Statistics/energyinformation/Energy_Statistics/Electricity/T05_02_02-1.xls'
  # download.file(url, 'thailand.xls', mode='wb')
  gen <- rio::import(file = url)
  
  months <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
  names(gen) <- c('date','Hydro','Fuel oil','Coal','Fossil Gas','Diesel','Imported','Renewables','Total')
  
  gen %>%
    filter(date %in% months) %>% 
    mutate(year = rep(1986:(lubridate::year(lubridate::today())-1), each=12)) %>%
    pivot_longer(cols = c(-date, -year), names_to='source', values_to='output_gwh') %>%
    mutate(iso2='TH', region='Thailand', data_source='thailand', date=paste(date, year), 
           across(output_gwh, as.numeric),
           date=strptime(paste("01",date), "%d %b %Y"),
           duration_hours=lubridate::days_in_month(date)*24,
           output_mw=output_gwh*1000/duration_hours) %>%
    filter(date >= date_from,
         date <= date_to,
         !source %in% c("Total", "Imported")) %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours)
}

