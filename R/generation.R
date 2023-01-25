#' Getting generation data from various data sources.
#'
#' @param date_from 
#' @param date_to 
#' @param data_source one or several of available data sources (see \link(data.available_data_sources)). By default, all available data sources are considered.
#' @param iso2 one or several iso2 codes. If NULL, all iso2s are considered.
#' @param homogenise whether to homogenise sources of power or keep original source classification.
#' @param freq rounding date at this frequency: can be second, minute, hour, day, week, month, bimonth, quarter, season, halfyear or year
#'
#' @return tibble of power generation data
#' @export
#'
#' @examples
get_generation <- function(date_from,
                           date_to=lubridate::today() + 1,
                           data_source=available_data_sources(),
                           iso2=NULL,
                           homogenise=T,
                           freq=NULL){
  
  # Only keeping relevant data sources
  if(!is.null(iso2)){
    data_source <- data_source[unlist(lapply(data_source, function(ds){any(iso2 %in% get(sprintf("%s.iso2s", ds))())}))]
  }
  
  years <- seq(lubridate::year(date_from), lubridate::year(date_to))

  d <- lapply(data_source, function(ds){
    lapply(years, function(year){
      f <- data.download_cache(data_source=ds, year=year, force=F, freq=freq)
      if(file.size(f)>300){
        return(readRDS(f))
      }else{
        return(tibble())
      }
    })
  }) %>%
    do.call(bind_rows, .)
  
  if(nrow(d)==0) return(tibble())
  
  if(!is.null(iso2)){
    d <- d %>%
      filter(iso2 %in% !!iso2)
  }
  
  d <- d %>%
    filter(date >= date_from,
           date <= date_to)
  
  if(homogenise){
    d <- homogenise_generation(d)
  }
  
  if(!is.null(freq)){
    d <- d %>%
      mutate(date=lubridate::floor_date(date, unit=freq)) %>%
      group_by(across(c(-output_mw))) %>%
      summarise_at("output_mw", mean) %>%
      ungroup()
    # Note: taking the mean is ok since we've ensured generation data is hourly-complete
    # If not, we would have had to account for potentially missing data
  }
  
  return(d)
}



combine_generation_cache_and_new <- function(d_cache, d_new){
  if(is.null(d_cache) || nrow(d_cache)==0){
    return(d_new)
  }else{
    bind_rows(
      d_cache %>% filter(date < min(d_new$date)),
      d_new
    )  
  }
}


collect_generation <- function(data_source,
                                    date_from,
                                    date_to=lubridate::today(tzone="UTC") + 2, ...){
  collect_fn <- get(sprintf("%s.collect_generation", data_source))
  return(collect_fn(date_from=date_from, date_to=date_to, ...))
}

data_source_iso2s <- function(data_source){
  get(sprintf("%s.iso2s", data_source))()
}


update_generation <- function(data_source,
                              year=lubridate::year(lubridate::today()),
                              use_cache=T,
                              download_from_gcs=T,
                              upload_to_gcs=T,
                              upload_to_db=F,
                              cache_folder="cache"){
  
  
  if(length(data_source)!=1){
    stop("update_generation only supports one data_source at a time")
  }
  
  #TODO deal with risk that end of year isn't updated by cron jobs
  dir.create(file.path(cache_folder, data_source), showWarnings = F, recursive = T)
  file_base <- sprintf("%s/gen_%d.RDS", data_source, year)
  file_cache <- file.path(cache_folder, file_base)
  
  # Creating a daily version for faster loading in online platform
  file_base_daily <- sprintf("%s/gen_daily_%d.RDS", data_source, year)
  file_cache_daily <- file.path(cache_folder, file_base_daily)
  
  if(use_cache & download_from_gcs){
    gcs.download(source_path=file_base, dest_path=file_cache)  
  }
  
  d_cache <- tibble::tibble()
  date_from <- as.POSIXct(paste0(year-1,"-12-30"), tz="UTC")
  
  if(use_cache && file.exists(file_cache) && file.size(file_cache) > 300){ # case when gcs.download file failed
    tryCatch({
      d_cache <- readRDS(file_cache)
      date_from <- max(d_cache$date)-lubridate::days(2)  
    }, error=function(e){})
  }
  
  date_to <- min(as.POSIXct(paste0(year+1,"-01-02"), tz="UTC"),
                 as.POSIXct(lubridate::today() + 2, tz="UTC"))
  
  d_new <- collect_generation(data_source=data_source,
                              date_from=date_from,
                              date_to=date_to) %>%
    filter(lubridate::year(date)==year)
  
  d <- combine_generation_cache_and_new(d_cache, d_new)
  saveRDS(d, file_cache)
  
  if(upload_to_gcs){
    gcs.upload(source_path=file_cache, dest_path=file_base)
  }
  
  # Updating daily version as well
  d_daily <- d %>%
    mutate(date=lubridate::floor_date(date, unit="day")) %>%
    group_by(across(c(-output_mw))) %>%
    summarise_at("output_mw", mean) %>%
    ungroup()
  saveRDS(d_daily, file_cache_daily)
  
  if(upload_to_gcs){
    gcs.upload(source_path=file_cache_daily, dest_path=file_base_daily)
  }
  
  if(upload_to_db){
    db.upload(d_daily)
  }
  
  return(d)
}


#' Group various winds together, hydro together, coal together etc
#'
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
homogenise_generation <- function(gen, do_factor=T){
  
  source_greps <- data.source_homogenising_greps()
  
  d <- lapply(names(source_greps),function(s){
    gen %>% filter(grepl(source_greps[[s]], source, ignore.case = T)) %>%
      group_by(across(c(-output_mw))) %>%
      summarise_at(c('output_mw'), sum, na.rm=T) %>%
      mutate(source = s)
  }) %>%
    do.call(bind_rows, .)
  
  if(do_factor){
    d$source <- factor(d$source, levels=names(source_greps))
  }
  
  return(d)
}
