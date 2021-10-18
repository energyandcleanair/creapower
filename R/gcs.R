gcs.auth <- function(force_service_account=F){
  if(!googleAuthR::gar_has_token()){
    suppressWarnings(readRenviron(".env"))
    suppressWarnings(readRenviron(".Renviron"))
    
    # Use USER specific credentials if set
    if(Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")!="" & !force_service_account){
      message("Using local user rights for GCS: ", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
      googleCloudStorageR::gcs_auth(json_file=Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))  
    }
    
    # Otherwise, use service account created for creapower
    googleCloudStorageR::gcs_auth(json_file=file.path(pkgload::inst("creapower"),"keys/creapower_key.json"))
    
    
    # googleAuthR::gar_set_client(json= file.path(pkgload::inst("creapower"),"creapower_oauth.json"))
    # googleAuthR::gar_auth(scopes = c("https://www.googleapis.com/auth/devstorage.full_control",
    #                                  "https://www.googleapis.com/auth/cloud-platform"))  
  }
}


gcs.download <- function(source_path, dest_path, overwrite=T){
  googleCloudStorageR::gcs_get_object(file.path("power/creapower/cache", source_path),
                                      saveToDisk=dest_path,
                                      overwrite=overwrite)
}


gcs.upload<- function(source_path, dest_path, overwrite=T){
  googleCloudStorageR::gcs_upload(file=source_path,
                                  name=file.path("power/creapower/cache", dest_path),
                                  predefinedAcl="default")
}