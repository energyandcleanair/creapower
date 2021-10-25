bmrs.iso2s <- function(){
  "GB"
}

bmrs.get_bmrs_key <- function(){
  suppressWarnings(readRenviron(".env"))
  suppressWarnings(readRenviron(".Renviron"))
  bmrs_key = Sys.getenv("BMRS_KEY")
  
  if(bmrs_key==""){
    stop("Could not find BMRS_KEY in your environment.")
  }
  
  return(bmrs_key)
}


bmrs.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  
  dates <- seq.Date(as.Date(date_from), as.Date(date_to), by="day")
  
  gen <- pbapply::pblapply(dates, function(date){
    url <- sprintf("https://api.bmreports.com/BMRS/B1620/v1?APIKey=%s&SettlementDate=%s&Period=*&ServiceType=csv",
                   bmrs.get_bmrs_key(), date)
    tryCatch({
        suppressWarnings(
          read_csv(url, skip=4, col_types = cols()) %>%
            select(source=`Power System Resource  Type`,
                   Quantity,
                   period=`Settlement Period`
            ) %>%
            mutate(date=!!date+lubridate::minutes(30)*(period-1)) %>%
            group_by(date=lubridate::floor_date(date,"hour"), source) %>%
            summarise(output_mw=mean(Quantity, na.rm=T)) %>%
            ungroup() %>%
            mutate(iso2="GB",
                   region="Great Britain",
                   data_source="bmrs",
                   duration_hours=1) %>%
            select(iso2, region, date, source, output_mw, data_source),
          classes=c("messages","warning"))
      }, error=function(e){tibble()})
    }) %>%
    do.call(bind_rows, .)
  
  gen <- gen %>%
    ungroup() %>%
    tidyr::complete(nesting(iso2, region, data_source, source), date,
                    fill=list(output_mw=0, duration_hours=1))
  
  return(gen)
}