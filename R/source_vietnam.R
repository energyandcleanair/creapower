vietnam.iso2s <- function(){
  'VN'
}

#' Collect generation data from Vietnam 
#'
#' @param date_from 
#' @param date_to 
#'
#' @return
#' @export
#'
#' @examples

vietnam.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC"), iso2s=NULL){
  dates <- seq(lubridate::date(date_from), lubridate::date(date_to), by='1 day')
  
  lapply(dates, function(date){
    url <- sprintf('https://www.nldc.evn.vn/Chart24hHandle.ashx?d=%s&isChangeDate=0', 
                   format(date, '%d/%m/%Y'))
    d <- NULL
    attempt <- 1
    while(is.null(d) && attempt <=3){
      try(
        d <- jsonlite::fromJSON(url)
      )
    }
    gen <- data.frame(d$data)[1,] %>% 
      pivot_longer(1:48, names_to = 'remove', values_to = 'output_mw') %>% 
      select(!remove) %>%
      mutate(date = seq(lubridate::ymd_hms(paste(date, '00:00:00'), tz='Asia/Saigon'), 
                        lubridate::ymd_hms(paste(date, '23:30:00'), tz='Asia/Saigon'), 
                        by = '30 min'),
             iso2 = 'VN', region = 'Vietnam', data_source = 'vietnam', source = 'All',
             duration_hours = 0.5)
  }) %>%
    do.call(bind_rows, .) %>%
    filter(date >= date_from, date < date_to) %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours)
}