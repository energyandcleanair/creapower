japan.iso2s <- function(){
  "JP"
}

#' Collect POSOCO data from great Robbie Andrew's work
#'
#' @param date_from 
#' @param date_to 
#'
#' @return
#' @export
#'
#' @examples
japan.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  
  years <- seq(lubridate::year(date_from), lubridate::year(date_to))
  
  lapply(years, function(year){
    url <- sprintf("https://www.renewable-ei.org/en/statistics/electricity/data/%d/power-data.json",year)
    d <- jsonlite::fromJSON(url)
    gen <- data.frame(d$japan) %>% tibble()
    dates <- data.frame(d$epochs)[,1] %>% 
      as.POSIXct(origin="1970-01-01", tz="Asia/Tokyo")
    gen$date <- dates
    
    gen %>%
      mutate(Hydro=hydropower+pumping_down+pumping_up) %>%
      select(date,
             Hydro,
             Nuclear=nuclear,
             Thermal=thermal,
             Geothermal=geothermal,
             Biomass=bioenergy,
             Solar=solar,
             Wind=wind) %>%
      tidyr::pivot_longer(cols=c(-date), values_to="output_mw", names_to="source") %>%
      mutate(
        iso2="JP",
        region="Japan",
        data_source="japan"
      )
  }) %>%
    do.call(bind_rows, .) %>%
    filter(date>=date_from,
           date<=date_to) %>%
    select(iso2, region, data_source, date, source, output_mw) %>%
    ungroup() %>%
    tidyr::complete(nesting(iso2, region, data_source, source), date,
                    fill=list(output_mw=0)) %>%
    mutate(duration_hours=1)
}