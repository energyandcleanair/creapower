eia.init <- function(){
  suppressWarnings(readRenviron(".env"))
  suppressWarnings(readRenviron(".Renviron"))
  eia::eia_set_key(Sys.getenv("EIA_KEY"))
}

eia.iso2s <- function(){
  "US"
}

eia.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  
  eia.init()
  
  series <- list(
    Coal="EBA.US48-ALL.NG.COL.H",
    `Fossil Gas`="EBA.US48-ALL.NG.NG.H",
    Nuclear="EBA.US48-ALL.NG.NUC.H",
    Oil="EBA.US48-ALL.NG.OIL.H",
    Other="EBA.US48-ALL.NG.OTH.H",
    Solar="EBA.US48-ALL.NG.SUN.H",
    Wind="EBA.US48-ALL.NG.WND.H",
    Hydro="EBA.US48-ALL.NG.WAT.H"
  )
  
  d <- pbapply::pblapply(names(series), function(source){
    tryCatch({
      eia::eia_series(series[[source]],
                      cache=F,
                      start=strftime(date_from,"%Y%m%d"),
                      end=strftime(date_to,"%Y%m%d")) %>%
        select(unit=units, data) %>%
        tidyr::unnest(data) %>%
        filter(unit=="megawatthours") %>%
        mutate(source=!!source,
               iso2="US",
               region="United States",
               data_source="eia",
               duration_hours=1
        ) %>%
        select(iso2, region, data_source, date, source, output_mw=value, duration_hours) %>%
        ungroup() %>%
        tidyr::complete(nesting(iso2, region, data_source, source), date,
                        fill=list(output_mw=0, duration_hours=1))
    }, error=function(e){return(tibble())})
  }) %>% do.call(bind_rows, .)

  return(d)    
}
  


    