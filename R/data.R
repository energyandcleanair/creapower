available_data_sources <- function(){
  c("entso", "eia", "posoco", "bmrs")
}


data.source_homogenising_greps <- function(){
  list(
    "Coal"= "Coal",
    "Fossil Gas"= "Fossil Gas|Natural Gas",
    # "Oil"= "Oil",
    "Nuclear"= "Nuclear",
    "Hydro"= "Hydro",
    "Wind"= "Wind",
    "Solar"= "Solar",
    # "Other Renewables"= "Geothermal|Renewable|Marine|Biomass",
    # "Other" = "Peat|^Other$|Waste"
    "Other"= "Geothermal|Renewable|Marine|Biomass|Peat|^Other$|Waste|Oil"
  )
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
  
  download <- force || !file.exists(file_cache)
    # (gcs.modification_date(file_base) > file.info(file_cache)$mtime) # gcs.modification_date doesn't work for unauthenticated
  
  if(download){
    message("Downloading cache: ", file_base)
    gcs.download(source_path=file_base, dest_path=file_cache, only_if_modified_since = T)
  }
  
  return(file_cache)
}

