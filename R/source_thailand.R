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
  
  # Historical
  url_hist <- 'http://www.eppo.go.th/epposite/images/Energy-Statistics/energyinformation/Energy_Statistics/Electricity/T05_02_02-1.xls'
  
  # Current year
  url_current <- 'http://www.eppo.go.th/epposite/images/Energy-Statistics/energyinformation/Energy_Statistics/Electricity/T05_02_02.xls'
  
  gen_hist <- rio::import(file = url_hist)
  gen_current <- rio::import(file = url_current)
  
  months <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
  names(gen_hist) <- c('date','Hydro','Fuel oil','Coal','Fossil Gas','Diesel','Imported','Renewables','Total')
  names(gen_current) <- c('year', 'date','Hydro','Fuel oil','Coal','Fossil Gas','Diesel','Imported','Renewables','Total')
  
  
  bind_rows(
    tibble(gen_current) %>%
      tidyr::fill(year, .direction="down") %>%
      filter(date %in% months) %>%
      filter(Total>1000), # Exclude the fuel share(%) table
    
    tibble(gen_hist) %>%
      mutate(year=ifelse(grepl("19|20", date) & is.na(Total), date, NA)) %>%
      tidyr::fill(year, .direction="down") %>%
      filter(date %in% months)
    ) %>%
    pivot_longer(cols = c(-date, -year), names_to='source', values_to='output_gwh') %>%
    distinct(year, date, source, .keep_all=T) %>%
    mutate(output_gwh=tidyr::replace_na(output_gwh, 0)) %>%
    mutate(iso2='TH', region='Thailand', data_source='thailand',
           date=paste(date, year), 
           across(output_gwh, as.numeric),
           date=strptime(paste("01",date), "%d %b %Y"),
           duration_hours=lubridate::days_in_month(date)*24,
           output_mw=output_gwh*1000/duration_hours) %>%
    filter(date >= date_from,
         date <= date_to,
         !source %in% c("Total", "Imported")) %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours)
}

