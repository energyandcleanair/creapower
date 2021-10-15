entso.init <- function(){
  readRenviron(".env")
  
}


entso.get_generation <- function(
  iso2s=NULL,
  date_from=lubridate::floor_date(lubridate::today(),"year"),
  date_to=lubridate::today()){
  
  if(is.null(iso2s)){
    iso2s <-  entsoeapi::en_eic() %>%
      filter(AreaTypeCode=="CTY") %>%
      pull(MapCode) %>%
      unique()
  }
  
  years <- seq(lubridate::year(date_from), lubridate::year(date_to))
  
  # Update cache from EIA
  pbapply::pblapply(iso2s, function(iso2){
    print(iso2)
    lapply(years, function(year){
      print(year)
      entso.update_generation(iso2=iso2, year=year)
      })
  })
  
  # Return requested generation
  lapply(iso2s, function(iso2){
    lapply(years, function(year){
      readRDS(sprintf("cache/entso/gen_%s_%d.RDS", iso2, year)) %>%
        filter(quantity_Measure_Unit.name=="MAW") %>%
        mutate(iso2=iso2,
               region=countrycode::countrycode(iso2, "iso2c", "country.name"),
               data_source="entso") %>%
        rename(
          date=dt,
          output_mw=quantity,
          codes=MktPSRType
        ) %>%
        left_join(
          entsoeapi::en_generation_codes() %>%
            select(codes, source=meaning)
        ) %>%
        select(iso2, region, data_source, date, source, output_mw)
    }) %>%
      do.call(bind_rows, .) %>%
      filter(date>=as.Date(date_from),
             date<=as.Date(date_to))
  }) %>%
    do.call(bind_rows, .)
}



entso.update_generation <- function(iso2, year){
  
  dir.create("cache/entso", showWarnings = F, recursive = T)
  f <- file.path("cache/entso/", sprintf("gen_%s_%d.RDS",iso2,year))
  
  eic <- entsoeapi::en_eic() %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(MapCode==iso2) %>%
    pull(AreaCode)
  
  
  get_gen_safe <- function(eic, date_from, date_to){
    tryCatch({
      entsoeapi::en_generation_agg_gen_per_type(eic=eic,
                                              period_start=date_from,
                                              period_end=date_to,
                                              security_token=Sys.getenv("ENTSO_TOKEN"))
    }, error=function(e){return(tibble())})
  }
  
  
  if(file.exists(f)){
    d <- readRDS(f)
    d <- bind_rows(
      get_gen_safe(eic, max(d$dt), as.POSIXct(paste0(year,"-12-31"), tz="UTC")),
      d) %>%
      distinct()
  }else{
    d <- get_gen_safe(eic, as.POSIXct(paste0(year,"-01-01"), tz="UTC"), as.POSIXct(paste0(year,"-12-31"), tz="UTC"))
  }

  saveRDS(d, f)
  
}







entso.get_capacity <- function(
  iso2s=NULL,
  years=lubridate::year(lubridate::today())){
  
  if(is.null(iso2s)){
    iso2s <-  entsoeapi::en_eic() %>%
      filter(AreaTypeCode=="CTY") %>%
      pull(MapCode) %>%
      unique()
  }
  
  
  # Update cache from EIA
  pbapply::pblapply(iso2s, function(iso2){
    lapply(years, function(year){entso.update_capacity(iso2=iso2, year=year)})
  })
  
  # Return requested generation
  lapply(iso2s, function(iso2){
    lapply(years, function(year){
      readRDS(sprintf("cache/entso/cap_%s_%d.RDS", iso2, year)) %>%
        filter(quantity_Measure_Unit.name=="MAW") %>%
        mutate(iso2=iso2,
               region=countrycode::countrycode(iso2, "iso2c", "country.name")) %>%
        rename(
          date=dt,
          output_mw=quantity,
          codes=MktPSRType
        ) %>%
        left_join(
          entsoeapi::en_generation_codes() %>%
            select(codes, source=meaning)
        ) %>%
        select(iso2, region, source, date, output_mw)
    }) %>%
      do.call(bind_rows, .) %>%
      filter(date>=as.Date(date_from),
             date<=as.Date(date_to))
  }) %>%
    do.call(bind_rows, .)
}




entso.update_capacity <- function(iso2, year, n_days_refresh=7){
  
  dir.create("cache/entso", showWarnings = F, recursive = T)
  f <- file.path("cache/entso/", sprintf("cap_%s_%d.RDS",iso2,year))
  
  eic <- entsoeapi::en_eic() %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(MapCode==iso2) %>%
    pull(AreaCode)
  
  if(file.exists(f)){
    d <- readRDS(f)
    # Update only if required (i.e. only if within year and older than n days)
    need_update <- is.na(d) ||
      (unique(d$dt_created) < unique(d$end)) &
      (unique(d$dt_created) < lubridate::today() - n_days_refresh)
  }
  
  if(!file.exists(f) || need_update){
    d <- tryCatch({
      entsoeapi::en_generation_inst_gen_cap_agg(eic=eic,
                                                year=year,
                                                security_token=Sys.getenv("ENTSO_TOKEN"))
    }, error=function(e){
      warning("Failed getting capacity for ", iso2, ": ", e)
      return(NA)})
      
    saveRDS(d, f)  
  }
}