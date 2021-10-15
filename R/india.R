india.get_generation <- function(date_from=lubridate::floor_date(lubridate::today(),"year"),
                                 date_to=lubridate::today()){
  
  f <- file.path("cache/india/", sprintf("gen_IN_POSOCO.RDS",year))
  india.update_generation()

  readRDS(f) %>%
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
           data_source="POSOCO") %>%
    filter(date>=date_from,
           date<=date_to) %>%
    select(iso2, region, data_source, date, source, output_mw)
}


india.update_generation <- function(n_days_refresh=3){
    
    dir.create("cache/india", showWarnings = F, recursive = T)
    f <- file.path("cache/india/", sprintf("gen_IN_POSOCO.RDS",year))
    
    if(file.exists(f)){
      d <- readRDS(f)
      # Update only if required (i.e. only if within year and older than n days)
      need_update <- max(d$date) < lubridate::today() - n_days_refresh
    }
    
    if(!file.exists(f) || need_update){
      d <- read.csv("https://robbieandrew.github.io/india/data/POSOCO_data.csv")
      d$date <- lubridate::ymd(d$yyyymmdd)
      saveRDS(d, f)  
    }
}
