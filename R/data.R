available_data_sources <- function(){
  c("japan", "entso", "eia", "posoco", "bmrs")
}


data_source_reference <- function(data_source){
  list(
    "entso"="ENTSO-E Transparency Platform",
    "eia"="EIA",
    "posoco"="POSOCO (from robbieandrew.github.io/india)",
    "bmrs"="BMRS",
    "japan"="Renewable Energy Institute (www.renewable-ei.org)")[[data_source]]
}


available_iso2s <- function(){
  lapply(available_data_sources(), function(ds){
    get(sprintf("%s.iso2s",ds))() %>%
      tibble(iso2=.) %>%
      mutate(data_source=ds,
             region=countrycode::countrycode(iso2, "iso2c", "country.name",
                                             custom_match =c("EU"="European Union")))
  }) %>%
    do.call(bind_rows, .)
}


data.source_homogenising_greps <- function(){
  list(
    "Thermal"= "^Thermal$",
    "Coal"= "Coal|Lignite",
    "Fossil Gas"= "Fossil Gas|Natural Gas",
    # "Oil"= "Oil",
    "Nuclear"= "Nuclear",
    "Hydro"= "Hydro",
    "Wind"= "Wind",
    "Solar"= "Solar",
    # "Other Renewables"= "Geothermal|Renewable|Marine|Biomass",
    # "Other" = "Peat|^Other$|Waste"
    "Other"= "Geothermal|Renewable|Marine|Biomass|Peat|^Other$|Waste|Oil|Diesel"
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
data.download_cache <- function(data_source, year, force=F, cache_folder="cache", freq=NULL){
  
  dir.create(file.path(cache_folder, data_source), showWarnings = F, recursive = T)
  
  if(is.null(freq) || grepl("hour", freq, ignore.case = T)){
    file_base <- file.path(data_source, sprintf("gen_%d.RDS", year))
    file_cache <- file.path(cache_folder, file_base)  
  }else{
    file_base <- file.path(data_source, sprintf("gen_daily_%d.RDS", year))
    file_cache <- file.path(cache_folder, file_base)  
  }
  
  download <- force || !file.exists(file_cache)
    # (gcs.modification_date(file_base) > file.info(file_cache)$mtime) # gcs.modification_date doesn't work for unauthenticated
  
  if(download){
    message("Downloading cache: ", file_base)
    gcs.download(source_path=file_base, dest_path=file_cache, only_if_modified_since = T)
  }
  
  return(file_cache)
}

