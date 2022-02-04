philippines.iso2s <- function(){
  'PH'
}

#' Collect generation data from Independent Electricity Market Operator of the Philippines (IEMOP)
#'
#' @param date_from 
#'
#' @return
#' @export
#'
#' @examples

philippines.collect_generation <- function(date_from, date_to = lubridate::today(tzone = "Asia/Manila")-1){
  yesterday_from <- date_from-1
  dir.create(file.path(tempdir(), '1'))
  dir.create(file.path(tempdir(), '2'))
  temp_out1 <- file.path(tempdir(), '1')
  temp_out2 <- file.path(tempdir(), '2')
  
  url <- 'https://www.iemop.ph/market-data/dipc-energy-results-raw/?post=5754&sort=&page=1&start={date1}%2023:00&end={date2}%2023:00'
  url <- glue::glue(url, date1 = yesterday_from, date2 = date_to)
  download.file(url, file.path(temp_out1, 'data.zip'), mode = 'wb')
  
  dl_zip <- list.files(temp_out1, pattern = '\\.zip$')
  sapply(dl_zip, function(file){
    unzip(file.path(temp_out1, file), exdir = temp_out2)
  })
  
  filelist <- list.files(temp_out2, pattern = '\\.zip$')
  sapply(filelist, function(file){
    unzip(file.path(temp_out2, file), exdir = temp_out2)
  })
  
  csvlist <- list.files(temp_out2, pattern = '\\.csv$')
  
  ph_pp <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ10_73VJzWqfccZ5iPJCcjsLWmXgzP2AFsDyn-7scLz5kCpeDxuXtuE8heRg4QwXcI7nXxxcOF65Mm/pub?output=csv') %>%
    select(RESOURCE.NAME, Source) %>%
    mutate(Source = replace(Source, Source == 'Diesel', 'Oil'), 
           Source = replace(Source, Source == 'Gas', 'Fossil Gas'))
  
  ph_data <- lapply(csvlist, function(file){
    read.csv(file.path(temp_out2, file))
  }) %>% bind_rows() %>%
    filter(TIME_INTERVAL != 'EOF', SCHED_MW >= 0) %>%
    mutate(date = dplyr::case_when(nchar(TIME_INTERVAL)==10 ~ paste0(TIME_INTERVAL, ' 12:00:00 AM'), 
                                   TRUE ~ TIME_INTERVAL)) %>%
    mutate(date = as.POSIXct(lubridate::mdy_hms(date))) %>%
    mutate(date_short = lubridate::date(date), hour = lubridate::hour(date)) %>%
    select(date, date_short, hour, RESOURCE_NAME, SCHED_MW)
  
  ph_data <- ph_data %>% 
    group_by(date_short, hour, RESOURCE_NAME) %>%
    summarise(output_mw = sum(SCHED_MW)/12) %>%
    mutate(date = lubridate::ymd_h(paste(date_short, hour), tz = 'Asia/Manila')) %>%
    left_join(ph_pp, by = c('RESOURCE_NAME' = 'RESOURCE.NAME'))
  
  ph_data %>% rename(source = Source) %>%
    group_by(date, source) %>%
    summarise(output_mw = sum(output_mw)) %>%
    ungroup() %>%
    mutate(duration_hours = 1, iso2 = 'PH', region = 'Philippines', data_source = 'philippines') %>%
    filter(date >= lubridate::date(date_from), date < date_to+1, !is.na(source)) %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours)
  
  # data <- ph_data %>% left_join(ph_pp, by = c('RESOURCE_NAME' = 'RESOURCE.NAME'))
  
  # data %>% rename(Oil = Diesel, Renewables = Renewable, 'Fossil Gas' = Gas) %>%
  #   mutate(Coal = Hard.coal + Bituminous.coal, 
  #          date = strptime(paste("01", Month, Year), "%d %b %Y")) %>%
  #   select(date, Hydro, Coal, Oil, 'Fossil Gas', Composite, Nuclear, Renewables) %>%
  #   pivot_longer(-date, names_to = 'source', values_to = 'output_gwh') %>%
  #   mutate(duration_hours = lubridate::days_in_month(date)*24, 
  #          output_mw = output_gwh*1000 / duration_hours,
  #          iso2 = 'KR', region = 'South Korea', data_source = 'south korea') %>%
  #   filter(date >= date_from, 
  #          date < date_to) %>% 
  #   select(iso2, region, data_source, date, source, output_mw, duration_hours) 
}