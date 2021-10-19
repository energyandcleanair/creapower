data.available_data_sources <- function(){
  c("entso", "eia", "posoco")
}

data.source_homogenising_greps <- function(){
  list(
    "Coal"= "Coal",
    "Wind"= "Wind",
    "Hydro"= "Hydro",
    "Solar"= "Solar",
    "Nuclear"= "Nuclear",
    "Fossil Gas"= "Fossil Gas|Natural Gas",
    "Oil"= "Oil",
    "Other Renewables"= "Geothermal|Renewable|Marine|Biomass",
    "Other" = "Peat|^Other$|Waste"
  )
}

data.combine_generation_cache_and_new <- function(d_cache, d_new){
  if(is.null(d_cache) || nrow(d_cache)==0){
    return(d_new)
  }else{
    bind_rows(
      d_cache %>% filter(date < min(d_new$date)),
      d_new
    )  
  }
}


data.collect_generation <- function(data_source,
                                    date_from,
                                    date_to=lubridate::today(tzone="UTC") + 2, ...){
  collect_fn <- get(sprintf("%s.collect_generation", data_source))
  return(collect_fn(date_from=date_from, date_to=date_to, ...))
}


data.update_generation <- function(data_source,
                                   year,
                                   download_from_gcs=T,
                                   upload_to_gcs=T,
                                   cache_folder="cache"){
  
  dir.create(file.path(cache_folder, data_source), showWarnings = F, recursive = T)
  file_base <- sprintf("%s/gen_%d.RDS", data_source, year)
  file_cache <- file.path(cache_folder, file_base)
  
  gcs.download(source_path=file_base, dest_path=file_cache)
  
  if(file.exists(file_cache) && file.size(file_cache) > 100){ # case when gcs.download file failed
    d_cache <- readRDS(file_cache)
    date_from <- max(d_cache$date)-lubridate::days(2)
  }else{
    d_cache <- tibble::tibble()
    date_from <- as.POSIXct(paste0(year,"-01-01"), tz="UTC")
  }
  
  date_to <- as.POSIXct(paste0(year,"-12-31"), tz="UTC")
  
  
  d_new <- data.collect_generation(data_source=data_source,
                                   date_from=date_from, date_to=date_to)
  
  d <- data.combine_generation_cache_and_new(d_cache, d_new)
  saveRDS(d, file_cache)
  
  gcs.upload(source_path=file_cache, dest_path=file_base)
  
  return(d)
}


#' Download
#'
#' @param data_source 
#' @param year 
#' @param force if F, then bucket file will only be downloaded if more recent than local one
#'
#' @return file path of cache file (whether it has been downloaded or not)
#' @export
#'
#' @examples
data.download_cache <- function(data_source, year, force=F, cache_folder="cache"){
  
  dir.create(file.path(cache_folder, data_source), showWarnings = F, recursive = T)
  file_base <- file.path(data_source, sprintf("gen_%d.RDS", year))
  file_cache <- file.path(cache_folder, file_base)
  
  download <- force || !file.exists(file_cache) || (gcs.modification_date(file_base) > file.info(file_cache)$mtime)
  
  if(download){
    message("Downloading cache: ", file_base)
    gcs.download(source_path=file_base, dest_path=file_cache)
  }
  
  return(file_cache)
}


#' Group various winds together, hydro together, coal together etc
#'
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
data.homogenise_generation <- function(gen){
  
  source_greps <- data.source_homogenising_greps()
  
  d <- lapply(names(source_greps),function(s){
    gen %>% filter(grepl(source_greps[[s]], source, ignore.case = T)) %>%
      group_by(iso2, region, date, data_source) %>%
      summarise_at(c('output_mw'), sum, na.rm=T) %>%
      mutate(source = s)
  }) %>%
    do.call(bind_rows, .)
  
  return(d)
}

#' Getting generation data from various data sources.
#'
#' @param date_from 
#' @param date_to 
#' @param data_sources one or several of available data sources (see \link(data.available_data_sources)). By default, all available data sources are considered.
#' @param iso2s if NULL, all iso2s are considered.
#' @param homogenise whether to homogenise sources of power or keep original source classification.
#'
#' @return tibble of power generation data
#' @export
#'
#' @examples
data.get_generation <- function(date_from, date_to=lubridate::today() + 1,
                                data_sources=data.available_data_sources(),
                                iso2s=NULL, homogenise=T){
  
  years <- seq(lubridate::year(date_from), lubridate::year(date_to))
  
  d <- lapply(data_sources, function(data_source){
    lapply(years, function(year){
      data.download_cache(data_source=data_source, year=year, force=F) %>%
        readRDS()
    })
  }) %>%
    do.call(bind_rows, .)
  
  if(!is.null(iso2s)){
    d <- d %>%
      filter(iso2 %in% iso2s)
  }
  
  if(homogenise){
    d <- data.homogenise_generation(d)
  }
  
  return(d)
}


  # iso2s=NULL,
  # date_from=lubridate::floor_date(lubridate::today(),"year"),
  # date_to=lubridate::today()){
  # 
  # if(is.null(iso2s)){
  #   iso2s <-  entsoeapi::en_eic() %>%
  #     filter(AreaTypeCode=="CTY") %>%
  #     pull(MapCode) %>%
  #     unique()
  # }
  # 
  # years <- seq(lubridate::year(date_from), lubridate::year(date_to))
  # 
  # # Update cache from EIA
  # pbapply::pblapply(iso2s, function(iso2){
  #   print(iso2)
  #   lapply(years, function(year){
  #     print(year)
  #     entso.update_generation(iso2=iso2, year=year)
  #   })
  # })
  # 
  # # Return requested generation
  # lapply(iso2s, function(iso2){
  #   lapply(years, function(year){
  #     readRDS(sprintf("cache/entso/gen_%s_%d.RDS", iso2, year)) %>%
  #       filter(quantity_Measure_Unit.name=="MAW") %>%
  #       mutate(iso2=iso2,
  #              region=countrycode::countrycode(iso2, "iso2c", "country.name"),
  #              data_source="entso") %>%
  #       rename(
  #         date=dt,
  #         output_mw=quantity,
  #         codes=MktPSRType
  #       ) %>%
  #       left_join(
  #         entsoeapi::en_generation_codes() %>%
  #           select(codes, source=meaning)
  #       ) %>%
  #       select(iso2, region, data_source, date, source, output_mw)
  #   }) %>%
  #     do.call(bind_rows, .) %>%
  #     filter(date>=as.Date(date_from),
  #            date<=as.Date(date_to))
  # }) %>%
  #   do.call(bind_rows, .)


