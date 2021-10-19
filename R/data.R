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
  
  collect_fn <- get(sprintf("%s.collect_generation", data_source))
  
  d_new <- collect_fn(date_from=date_from, date_to=date_to)
  
  d <- data.combine_generation_cache_and_new(d_cache, d_new)
  saveRDS(d, file_cache)
  
  gcs.upload(source_path=file_cache, dest_path=file_base)
  
  return(d)
}




data.get_generation <- function(){
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
}

