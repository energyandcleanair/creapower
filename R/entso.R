entso.get_entso_token <- function(){
  suppressWarnings(readRenviron(".env"))
  suppressWarnings(readRenviron(".Renviron"))
  entso_token = Sys.getenv("ENTSO_TOKEN")

  if(entso_token==""){
    stop("Could not find ENTSO_TOKEN in your environment.")
  }
  
  return(d)
}



#' Function to collect generation data directly from ENTSO
#'
#' @param iso2 
#' @param date_from 
#' @param date_to 
#'
#' @return
#' @export
#'
#' @examples
entso.collect_generation <- function(date_from, date_to, iso2s=NULL){
  
  eics <- entsoeapi::en_eic() %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(is.null(iso2s) || (MapCode %in% iso2s)) %>%
    distinct(eic=AreaCode, iso2=MapCode) %>%
    filter(iso2!="GB")
  
  # Sanity check
  if(!1==eics %>% group_by(iso2) %>% summarise(n=n()) %>% pull(n) %>% max()){
    stop("More than one eic per country. Aggregation will lead to double counting")
  }
  
  
  get_gen_safe <- function(eic, date_from, date_to){
    tryCatch({
      entsoeapi::en_generation_agg_gen_per_type(eic=eic,
                                                period_start=as.POSIXct(date_from),
                                                period_end=as.POSIXct(date_to),
                                                security_token=Sys.getenv("ENTSO_TOKEN"))
    }, error=function(e){return(tibble::tibble())})
  }
  
  d <- pbapply::pblapply(unique(eics$eic), get_gen_safe, date_from=date_from, date_to=date_to) %>%
    do.call(bind_rows, .)
  
  d <- d %>%
    rename(eic=inBiddingZone_Domain.mRID,
           unit=quantity_Measure_Unit.name,
           codes=MktPSRType,
           output_mw=quantity,
           date=dt) %>%
    filter(unit=="MAW") %>%
    left_join(eics)  %>%
    select(iso2, unit, date, codes, output_mw) %>%
    mutate(region=countrycode::countrycode(iso2, "iso2c", "country.name"),
           data_source="entso") %>%
    left_join(
      entsoeapi::en_generation_codes() %>%
        select(codes, source=meaning)
    ) %>%
    select(iso2, region, data_source, date, source, output_mw) %>%
    mutate(output_mw=tidyr::replace_na(output_mw, 0)) %>%
    mutate(date=lubridate::floor_date(date, "hours")) %>%
    group_by(iso2, region, data_source, date, source) %>%
    summarise_at("output_mw", mean, na.rm=T)
  
  # Complete hours
  d <- d %>%
    ungroup() %>%
    tidyr::complete(nesting(iso2, region, data_source, source), date,
                    fill=list(output_mw=0))
  
  # Add Europe
  eu_iso2s <- countrycode::codelist %>% filter(!is.na(eu28)) %>% pull(iso2c)
  d <- bind_rows(d,
            d %>%
              filter(iso2 %in% eu_iso2s) %>%
              filter(iso2 != "GB") %>% # GB not part of EU, and GB data stopped in July
              group_by(data_source, source, date) %>%
              summarise_at("output_mw", sum, na.rm=T) %>%
              mutate(iso2="EU", region="European Union")
  )
  
  
  return(d)
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



#' Update local and GCS cache of ENTSO generation data
#'
#' @param iso2 
#' @param year 
#' @param download_from_gcs 
#' @param upload_to_gcs 
#' @param cache_folder 
#'
#' @return
#' @export
#'
#' @examples
entso.update_generation <- function(iso2,
                                    year,
                                    download_from_gcs=T,
                                    upload_to_gcs=T,
                                    cache_folder="cache"){
  
  dir.create(file.path(cache_colder, "entso"), showWarnings = F, recursive = T)
  file_base <- sprintf("entso/gen_%s_%d.RDS",iso2,year)
  file_cache <- file.path(cache_folder, file_base)
  
  
  if(download_from_gcs){
    gcs.download(source_path=file_base,
                 dest_path=file_cache)
  }
  
  
  if(file.exists(file_cache)){
    d_cache <- readRDS(file_cache)
    date_from <- max(d_cache$dt)-lubridate::days(2)
  }else{
    d_cache <- tibble::tibble()
    date_from <- as.POSIXct(paste0(year,"-01-01"), tz="UTC")
  }
  
  d_new <- entso.collect_generation(iso2=iso2,
                        date_from=date_from,
                        date_to=as.POSIXct(paste0(year,"-12-31"), tz="UTC"))
      
  d <- data.combine_generation_cache_and_new(d_cache, d_new)
  saveRDS(d, file_cache)
  
  if(upload_to_gcs){
    gcs.upload(source_path=file_cache,
               dest_path=file_base)
  }
  return(d)
}


# Capacity ----------------------------------------------------------------

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