entso.get_entso_token <- function(){
  suppressWarnings(readRenviron(".env"))
  suppressWarnings(readRenviron(".Renviron"))
  entso_token = Sys.getenv("ENTSO_TOKEN")

  if(entso_token==""){
    stop("Could not find ENTSO_TOKEN in your environment.")
  }
  
  return(entso_token)
}


entso.iso2s <- function(){
  entsoeapi::en_eic() %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(MapCode!="GB") %>%
    pull(MapCode) %>%
    c("EU")
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
entso.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1, iso2s=NULL){
  
  print(paste0("Collecting ENTSO: ", date_from, " to ", date_to))
  eics <- entsoeapi::en_eic() %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(is.null(iso2s) | (MapCode %in% iso2s)) %>%
    distinct(eic=AreaCode, iso2=MapCode) %>%
    filter(iso2!="GB")
  
  # Sanity check
  if(!1==eics %>% group_by(iso2) %>% summarise(n=n()) %>% pull(n) %>% max()){
    stop("More than one eic per country. Aggregation will lead to double counting")
  }
  
  
  get_gen_safe <- function(eic, date_from, date_to){
    tryCatch({
      print(eic)
      dates <- seq.Date(as.Date(date_from), as.Date(date_to), "day")
      n_chunk <- 30 # We split per days
      gen <- split(dates, seq(1, length(dates)) %/% n_chunk) %>%
        pbapply::pblapply(
          function(dates){
            tryCatch({
              entsoeapi::en_generation_agg_gen_per_type(eic=eic,
                                                        period_start=as.POSIXct(min(dates)),
                                                        period_end=as.POSIXct(max(dates)),
                                                        security_token=entso.get_entso_token())    
            }, error=function(e){return(tibble())}) # Error happens when no selected date has data
          }
        ) %>% do.call(bind_rows, .)
      print(sprintf("%d unique dates found", length(unique(gen$dt))))
      return(gen)
    }, error=function(e){
      warning("Failed to collect generation: ", eic, " :", e)
      return(tibble::tibble())})
  }
  
  d <- pbapply::pblapply(unique(eics$eic), get_gen_safe, date_from=date_from, date_to=date_to) %>%
    do.call(bind_rows, .) %>%
    filter(!is.na(inBiddingZone_Domain.mRID))
  
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
  if(is.null(iso2s)){
    eu_iso2s <- countrycode::codelist %>% filter(!is.na(eu28)) %>% pull(iso2c)
    d <- bind_rows(d,
                   d %>%
                     filter(iso2 %in% eu_iso2s) %>%
                     filter(iso2 != "GB") %>% # GB not part of EU, and GB data stopped in July
                     group_by(data_source, source, date) %>%
                     summarise_at("output_mw", sum, na.rm=T) %>%
                     mutate(iso2="EU", region="European Union")
    )
  }
  
  # Set duration_hours
  d <- d %>%
    mutate(duration_hours=1)
  
  return(d)
}



# Capacity ----------------------------------------------------------------

entso.get_capacity <- function(
  iso2=NULL,
  years=lubridate::year(lubridate::today())){
  
  if(is.null(iso2)){
    iso2 <-  entsoeapi::en_eic() %>%
      filter(AreaTypeCode=="CTY") %>%
      pull(MapCode) %>%
      unique()
  }
  
  
  # Update cache from ENTSO
  pbapply::pblapply(iso2, function(iso2){
    lapply(years, function(year){entso.update_capacity(iso2=iso2, year=year)})
  })
  
  # Return requested generation
  lapply(iso2, function(iso2){
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