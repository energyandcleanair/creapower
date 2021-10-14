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
    lapply(years, function(year){entso.update_generation(iso2=iso2, year=year)})
  })
  
  # Return requested generation
  lapply(iso2s, function(iso2){
    lapply(years, function(year){
      readRDS(sprintf("cache/entso/gen_%s_%d.RDS", iso2, year)) %>%
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


entso.update_generation <- function(iso2, year){
  
  dir.create("cache/entso", showWarnings = F, recursive = T)
  f <- file.path("cache/entso/", sprintf("gen_%s_%d.RDS",iso2,year))
  
  eic <- entsoeapi::en_eic() %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(MapCode==iso2) %>%
    pull(AreaCode)
  
  if(file.exists(f)){
    d <- readRDS(f)
    d <- bind_rows(
      entsoeapi::en_generation_agg_gen_per_type(eic=eic,
                                                period_start=max(d$dt),
                                                period_end=as.POSIXct(paste0(year,"-12-31"), tz="UTC"),
                                                security_token=Sys.getenv("ENTSO_TOKEN")),
      d) %>%
      distinct()
  }else{
    d <- entsoeapi::en_generation_agg_gen_per_type(eic=eic,
                                                   period_start=as.POSIXct(paste0(year,"-01-01"), tz="UTC"),
                                                   period_end=as.POSIXct(paste0(year,"-12-31"), tz="UTC"),
                                                   security_token=Sys.getenv("ENTSO_TOKEN"))
  }

  saveRDS(d, f)
  
}