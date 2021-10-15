eia.init <- function(){
  readRenviron(".env")
  readRenviron(".Renviron")
  eia::eia_set_key(Sys.getenv("EIA_KEY"))
}

eia.get_generation <- function(date_from=lubridate::floor_date(lubridate::today(),"year"),
                               date_to=lubridate::today()){
  
  eia.init()
  years <- seq(lubridate::year(date_from), lubridate::year(date_to))
  
  # Update cache from EIA
  lapply(years, eia.update_generation)
  
  # Return requested generation
  lapply(years, function(year){
    readRDS(sprintf("cache/eia/gen_%d.RDS", year))
  }) %>%
    do.call(bind_rows, .) %>%
    filter(date>=as.Date(date_from),
           date<=as.Date(date_to))
}



eia.update_generation <- function(year){
  
  dir.create("cache/eia", showWarnings = F, recursive = T)
  f <- file.path("cache/eia/", sprintf("gen_%d.RDS",year))
  
  
  series <- list(
    Coal="EBA.US48-ALL.NG.COL.H",
    `Fossil Gas`="EBA.US48-ALL.NG.NG.H",
    Nuclear="EBA.US48-ALL.NG.NUC.H",
    Oil="EBA.US48-ALL.NG.OIL.H",
    Other="EBA.US48-ALL.NG.OTH.H",
    Solar="EBA.US48-ALL.NG.SUN.H",
    Wind="EBA.US48-ALL.NG.WND.H",
    Hydro="EBA.US48-ALL.NG.WAT.H"
  )
  
  
  if(file.exists(f)){
    d <- readRDS(f)
    last_update <- max(eia::eia_series_updates(id=series)$updated) %>%
      lubridate::as_datetime()
    need_update <- (nrow(d)==0) || 
      ((last_update > max(d$date) + lubridate::hours(24)) & (lubridate::year(last_update)==lubridate::year(max(d$date))))
  }else{
    need_update <- T
  }
  
  if(need_update){
    d <- lapply(names(series), function(source){
      tryCatch({
        eia::eia_series(series[[source]],
                        cache=F,
                        n=365*24,
                        start=year,
                        end=year) %>%
          select(unit=units, data) %>%
          tidyr::unnest(data) %>%
          filter(unit=="megawatthours") %>%
          mutate(source=!!source,
                 iso2="US",
                 region="United States",
                 data_source="eia"
          ) %>%
          select(iso2, region, data_source, date, source, output_mw)
      }, error=function(e){return(tibble())})
    }) %>% do.call(bind_rows, .)
    
    saveRDS(d, f)
  }
}
  


    