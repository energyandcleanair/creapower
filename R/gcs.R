gcs.auth <- function(){
  if(!googleAuthR::gar_has_token()){
    googleAuthR::gar_set_client(json= file.path(pkgload::inst("creapower"),"creapower_oauth.json"))
    googleAuthR::gar_auth(scopes = c("https://www.googleapis.com/auth/devstorage.full_control",
                                     "https://www.googleapis.com/auth/cloud-platform"))  
  }
}