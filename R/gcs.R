gcs.auth <- function(force_service_account=F){
  
  # To avoid interactive prompt
  # which blocks execution on ComputeEngine and AppEngine
  options(httr_oauth_cache=T)
  
  if(force_service_account){
    googleAuthR::gar_deauth()
  }
  
  if(!googleAuthR::gar_has_token()){
    suppressWarnings(readRenviron(".env"))
    suppressWarnings(readRenviron(".Renviron"))
    
    # First try to see if we're on a Compute Engine instance
    googleAuthR::gar_gce_auth()
    
    if (!googleAuthR::gar_has_token()){
      # Use USER specific credentials if set
      if(Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")!="" & !force_service_account){
        message("Using local user rights for GCS: ", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
        googleCloudStorageR::gcs_auth(json_file=Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))  
      }
    }
  }
}

gcs.modification_date <- function(source_path){
  gcs.auth()
  tryCatch({
    m <- googleCloudStorageR::gcs_get_object(file.path("creapower/cache", source_path),
                                        meta=T)
    return(lubridate::as_datetime(m$updated))
  }, error=function(e){
    warning("File does not exist: ", source_path)
    return(lubridate::date("0000-01-01"))
  })
}


gcs.download <- function(source_path, dest_path, only_if_modified_since=T, overwrite=T){
  # gcs.auth()
  tryCatch({
    # googleCloudStorageR::gcs_get_object(file.path("creapower/cache", source_path),
    #                                     saveToDisk=dest_path,
    #                                     overwrite=overwrite)
    
    url <- sprintf("https://storage.googleapis.com/crea-public/creapower/cache/%s", source_path)
    config <- list()
    if(only_if_modified_since && file.exists(dest_path)){
      config <- httr::add_headers(`If-Modified-Since`=httr::http_date(file.info(dest_path)$mtime))
    }
    
    r <- httr::GET(url=url, config=config)
    if(r$status_code==304){
      message("Cache file already up to date. No need to update ", source_path)
    }else{
      bin <- httr::content(r, "raw")
      writeBin(bin, dest_path)
    }
  }, error=function(e){
    warning("Failed to download ", source_path, ": ", e)
    return(F)
  })
  
  # Now we're using public bucket
  
}


gcs.upload<- function(source_path, dest_path, overwrite=T){
  gcs.auth()
  tryCatch({
    googleCloudStorageR::gcs_upload(file=source_path,
                                    name=file.path("creapower/cache", dest_path),
                                    upload_type="simple",
                                    predefinedAcl="default")
  }, error=function(e){
    warning("Failed to upload ", source_path, ": ", e)
    return(F)
  })
}