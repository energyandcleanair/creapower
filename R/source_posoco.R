posoco.iso2s <- function(){
  "IN"
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
posoco.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  
  d <- read.csv("https://robbieandrew.github.io/india/data/POSOCO_data.csv")
  d$date <- lubridate::ymd(d$yyyymmdd)
  
  d %>%
    select_if(grepl("date|India.", names(.))) %>%
    select(-c("India..HydroGen")) %>% #We use India..Hydro column instead
    mutate_if(is.numeric, tidyr::replace_na, replace=0) %>%
    mutate(India..CoalTotal=ifelse(India..Coal>0, India..Coal+India..Lignite, India..Thermal)) %>%
    mutate(India..OtherRenewables=ifelse(India..RES>0,India..RES-India..WindGen-India..SolarGen,0)) %>%
    # Other is ~ 0
    # mutate(India..Other=ifelse(India..Total>0,
    #                            India..Total - 
    #                              India..CoalTotal -India..Gas -India..Nuclear -India..Hydro -
    #                              India..WindGen -India..SolarGen -India..OtherRenewables,
    #                            0)) %>%
    filter(India..Total>0) %>%
    select(date,
           Coal=India..CoalTotal,
           `Fossil Gas`=India..Gas,
           Nuclear=India..Nuclear,
           Hydro=India..Hydro,
           Wind=India..WindGen,
           Solar=India..SolarGen,
           `Other Renewables`=India..OtherRenewables
           ) %>%
    tidyr::pivot_longer(cols=-date,
                        names_to="source",
                        names_prefix="India..",
                        values_to="output_gwh") %>%
    mutate(output_mw=output_gwh * 1000 / 24,
           iso2="IN",
           region="India",
           data_source="posoco") %>%
    filter(date>=date_from,
           date<=date_to) %>%
    select(iso2, region, data_source, date, source, output_mw) %>%
    ungroup() %>%
    tidyr::complete(nesting(iso2, region, data_source, source), date,
                    fill=list(output_mw=0))
}

