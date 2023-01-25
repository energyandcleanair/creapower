carbonmonitor.get_power <- function(){
  url <- 'https://datas.carbonmonitor.org/API/energy_global/China;India;US;EU27%20&%20UK;UK;France;Germany;Italy;Spain;Russia;Japan;Brazil;Australia;South%20Africa;Chile;Mexico?evolution=true&withDates=true&years=2023;2022;2021;2020;2019;2018'
  data <- jsonlite::fromJSON(url)$datas
  as.data.frame(data) %>%
    `names<-`(c('region', 'date', 'source','value_gwh','timestamp')) %>%
    tibble()%>%
    mutate(value_gwh=as.numeric(value_gwh),
           date=date(strptime(date, '%d/%m/%Y'))) %>%
    select(-timestamp) %>%
    mutate(data_source='CarbonMonitor') %>%
    arrange(desc(date), region, source)
}


carbonmonitor.get_co2 <- function(){
  url <- 'https://datas.carbonmonitor.org/API/carbon_eu/EU27%20&%20UK;Austria;Belgium;Bulgaria;Croatia;Cyprus;Czech%20Republic;Denmark;Estonia;Finland;France;Germany;Greece;Hungary;Ireland;Italy;Latvia;Lithuania;Luxembourg;Malta;Netherlands;Poland;Portugal;Romania;Slovakia;Slovenia;Spain;Sweden;United%20Kingdom?evolution=true&withDates=true'
  data <- jsonlite::fromJSON(url)$datas
  as.data.frame(data) %>%
    `names<-`(c('region', 'date', 'sector','value_mtco2','timestamp')) %>%
    tibble()%>%
    mutate(value_mtco2=as.numeric(value_mtco2),
           date=date(strptime(date, '%d/%m/%Y'))) %>%
    select(-timestamp) %>%
    mutate(data_source='CarbonMonitor') %>%
    arrange(desc(date), region, sector)
}



