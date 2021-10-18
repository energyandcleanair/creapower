data.combine_generation_cache_and_new <- function(d_cache, d_new){
  if(is.null(d_cache) || nrow(d_cache)==0){
    return(d_new)
  }else{
    bind_rows(
      d_cache %>% filter(dt < min(d_new$dt)),
      d_new
    )  
  }
}





data.update_generation <- function(data_source,
                                   year,
                                   download_from_gcs=T,
                                   upload_to_gcs=T,
                                   cache_folder="cache"){
  
  dir.create(file.path(cache_colder, data_source), showWarnings = F, recursive = T)
  file_base <- sprintf("%s/gen_%d.RDS", data_source, year)
  file_cache <- file.path(cache_folder, file_base)
    
  if(download_from_gcs){
    gcs.download(source_path=file_base,
                 dest_path=file_cache)
  }
    
    
  if(file.exists(file_cache)){
    d_cache <- readRDS(file_cache)
    date_from <- max(d_cache$date)-lubridate::days(2)
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