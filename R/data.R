available_data_sources <- function(){
  # c("japan", "eia", "posoco", "bmrs", "thailand", "wind", "turkey", 
  #   "southafrica", "southkorea", "entso", "philippines", "australia")
  c('posoco_in', 'japan', 'nemweb_au', 'iemop_ph', 'EIA', 
    'southkorea_kr', 'nldc_vn', 'BMRS', 'EPPO', 'southafrica_za',
    'wind_cn', 'entsoe', 'TEIAS')
}


data_source_reference <- function(data_source){
  list(
    "entsoe"="ENTSO-E Transparency Platform",
    "EIA"="US Energy Information Administration",
    "posoco_in"="POSOCO (from robbieandrew.github.io/india)",
    "BMRS"="UK National Load Dispatch Centre of Vietnam",
    "japan"="Renewable Energy Institute (www.renewable-ei.org)",
    "EPPO"="Thailand Energy Policy and Planning Office (EPPO)",
    "southkorea_kr"="Korea Electric Power Corporation (KEPCO)",
    "southafrica_za"="ESKOM",
    "TEIAS"="Turkish Electricity Transmission Corporation (TEIAS)",
    "iemop_ph"="Independent Electricity Market Operator of the Philippines (IEMOP)",
    "newmweb_au"="Australian Energy Market Operator (AEMO)",
    "nldc_vn"="National Load Dispatch Centre of Vietnam (NLDC)",
    "wind_cn"="Wind",
    "crea"="Centre for Research on Energy and Clean Air")[[data_source]]
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
    "Renewables" = "^Renewables$|^Renewable$",
    # "Other Renewables"= "Geothermal|Renewable|Marine|Biomass",
    # "Other" = "Peat|^Other$|Waste"
    "Other"= "Geothermal|Other Renewable|Marine|Biomass|Peat|^Other$|Waste|Oil|Diesel"
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

  message("Checking online cache version: ", file_base)
  gcs.download(source_path=file_base, dest_path=file_cache, only_if_modified_since = force)

  return(file_cache)
}

