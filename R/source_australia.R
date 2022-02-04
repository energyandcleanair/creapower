australia.iso2s <- function(){
  'AU'
}

#' Collect generation data from Australian Energy Market Operator (AEMO)
#'
#' @param date_from 
#'
#' @return
#' @export
#'
#' @examples

australia.collect_generation <- function(date_from, date_to = lubridate::today(tz = 'Australia/Canberra')-2){
  td <- tempdir()
  dir.create(file.path(td, 'extracted1'))
  dir.create(file.path(td, 'extracted2'))
  exdir1 <- file.path(td, 'extracted1')
  exdir2 <- file.path(td, 'extracted2')
  
  homogenise_source <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQj2gsn-Pds_6ErCLLdxDxVV-kc2YCxTX634alBZ0cufi38r7gU75wXMgRm0uzIP8S4-0H137SB37X_/pub?output=csv')
  participant_url <- 'https://www.aemo.com.au/-/media/Files/Electricity/NEM/Participant_Information/NEM-Registration-and-Exemption-List.xls'
  participant <- rio::import(participant_url, which = 'Generators and Scheduled Loads') %>%
    left_join(homogenise_source, by=c('Fuel Source - Descriptor' = 'Old')) %>%
    select(DUID, source)
  
  # conversion <- data.frame(old = unique(gen$`Fuel Source - Descriptor`),
  #                new = c('Storage', 'Solar', 'Waste', 'Oil', 'Fossil Gas', 'Wind', 'Fossil Gas', 'Hydro',
  #                        'Coal', 'Fossil Gas', 'Waste', 'Fossil Gas', 'Oil', 'Coal', 'Solar', 'Waste', 'Fossil Gas'))
  
  archive_urls <- rvest::read_html('https://nemweb.com.au/Reports/Archive/Dispatch_SCADA/') %>%
    rvest::html_elements('a') %>% 
    rvest::html_attr('href') %>%
    purrr::keep(substr(., 33, 38) == 'PUBLIC')
  # current_urls <- read_html('http://www.nemweb.com.au/REPORTS/CURRENT/Dispatch_SCADA/') %>%
  #   html_element('a') %>%
  #   html_attr('href') %>%
  #   keep(substr(., 33, 38) == 'PUBLIC')
  archive_urls <- archive_urls[lubridate::ymd(stringr::str_sub(archive_urls, -12, -5)) >= lubridate::date(date_from) &
                                 lubridate::ymd(stringr::str_sub(archive_urls, -12, -5)) <= date_to]
  
  sapply(archive_urls, function(u){
    tf <- tempfile(fileext = '.zip')
    download.file(sprintf('http://www.nemweb.com.au%s', u), tf)
    unzip(tf, exdir = exdir1, overwrite = TRUE)
  })
  
  zips <- list.files(exdir1, full.names = TRUE)
  sapply(zips, function(z){
    unzip(z, exdir = exdir2, overwrite = TRUE)
  })
  
  data_files <- list.files(exdir2, full.names = TRUE)
  
  lapply(data_files, function(d){
    rio::import(d) %>%
      filter(SCADAVALUE > 0) %>%
      select(SETTLEMENTDATE, DUID, SCADAVALUE)
  }) %>% bind_rows() %>% 
    left_join(participant) %>%
    filter(!source %in% c('', 'Storage') & !is.na(source)) %>%
    mutate(date = as.POSIXct(lubridate::ymd_hms(SETTLEMENTDATE))) %>%
    mutate(date_short = lubridate::date(date), hour = lubridate::hour(date)) %>%
    group_by(date_short, hour, source) %>%
    summarise(output_mw = mean(SCADAVALUE)) %>%
    ungroup() %>%
    mutate(date = lubridate::ymd_h(paste(date_short, hour), tz = 'Australia/Melbourne'),
           duration_hours = 1, iso2 = 'AU', region = 'Australia', 
           data_source = 'australia') %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours)
  
}

################################################################################
# getting fuel type and facility code
# source is only the first fuel type
# LIQUID FUEL counted as oil

# years <- c(2014:2022)
# url <- 'https://data.wa.aemo.com.au/datafiles/commissioning-test/commissioning-test-summary-{year}.csv'
# gen_url <- 'https://data.wa.aemo.com.au/datafiles/facility-scada/facility-scada-{year}-{month}.csv'
# 
# participant <- lapply(years, function(y){
#   download_url <- glue::glue(url, year = y)
#   read.csv(download_url)
# }) %>% bind_rows() %>%
#   add_count(Facility.Code, Fuel.Type.1) %>%
#   group_by(Facility.Code) %>%
#   summarise(Primary = Fuel.Type.1[n == max(n)][1]) %>%
#   select(Facility.Code, Primary) %>%
#   ungroup() %>%
#   left_join(homogenise_source, by = c('Primary' = 'Old'))
# 
# generation <- read.csv('https://data.wa.aemo.com.au/datafiles/facility-scada/facility-scada-2008-12.csv')
