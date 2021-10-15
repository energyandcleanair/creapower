data.download_cache_file <- function(file){
  
  dir.create("cache/entso", showWarnings = F, recursive = T)
  googleCloudStorageR::gcs_get_object("power/creapower/cache/entso/gen_AT_2021.RDS", saveToDisk="cache/entso/gen_AT_2021.RDS", overwrite = T, parseObject = F)
  
  googleCloudStorageR::gcs_get_object("power/creapower/cache/entso/gen_AT_2021.RDS",
                                      saveToDisk = "cache/entso/gen_AT_2021.RDS",
                                      parseObject = F,
                                      overwrite = T)
  
}