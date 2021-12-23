wind.iso2s <- function(){
  "CN"
}


wind.collect_generation <- function(date_from, date_to=lubridate::today(tzone="UTC")+1){
  
  gen <- wind.read_spreadsheet() %>%
    wind.unYTD() %>%
    ungroup()
  
  gen %>%
    mutate(data_source="wind",
           iso2="CN",
           duration_hours=lubridate::days_in_month(date)*24,
           output_mw=value/duration_hours) %>%
    select(iso2, data_source, date, source, output_mw, duration_hours) %>%
    # Sum provinces
    group_by(across(-c(output_mw))) %>%
    summarise_at("output_mw", sum, na.rm=T) %>%
    mutate(region="China") %>%
    select(iso2, region, data_source, date, source, output_mw, duration_hours) %>%
    filter(date >= date_from,
           date <= date_to) %>%
    ungroup() %>%
    mutate(source=gsub(" Power Generating Capacity","",source),
           source=gsub(" Power","",source))
}



wind.read_spreadsheet <- function(){
  
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTC2CayhIlcbomItzug-RI1FewmC80klaN4l9MToZ5cq0k4FrnoZx1T_kEepBiWTJh5tvhwfjS09BBL/pub?gid=638862476&single=true&output=csv"
  
  colNames <- c('var', 'source', 'province')
  columnFilter <- c('^Output')
  
  data <- read_csv(url, col_names = F, col_types = readr::cols())
  data[1,] %>% unlist -> h
  
  if(any(grepl('YTD', h))) colNames %<>% c('type') %>% unique
  if(any(grepl('YoY', h))) colNames %<>% c('YoY') %>% unique
  
  d <- data[3:nrow(data),]
  
  if(!is.null(columnFilter)) {
    ind <- c(1, grep(columnFilter, h))
    h[ind] -> h
    d[, ind] -> d
    
    # if(read_vardata) vardata[, ind] -> vardata
  }
  
  ncols <- h[-1] %>% sapply(function(x) gregexpr(":",x) %>% unlist %>% length) %>% max %>% magrittr::add(1)
  h[-1] %>% tibble(col=.) %>% separate(col, paste0("V", 1:ncols), ":") -> md
  md %>% lapply(function(x) x %>% gsub("^ ", "", .) %>% gsub(" $", "", .)) -> md[,]
  names(md)[1:length(colNames)] <- colNames
  
  names(d) <- c('date', make.names(h[-1]))
  md$col <- names(d[-1])
  
  d$date <- as.Date(paste0(d$date,"-01"), format="%Y-%m-%d")
  dups <- d %>% names %>% duplicated %>% which
  if(length(dups)>0) d[, -dups] -> d
  
  d %<>% subset(!is.na(date)) %>%
    gather(col, value, -date) %>%
    left_join(md) %>% dplyr::select(-col) %>% 
    mutate(value=as.numeric(gsub(",","",value))*10,
           unit="mwh")
  
  if(all(c('type', 'YoY') %in% names(d))) {
    d %<>% mutate(YoY = ifelse(type=='YoY', 'YoY', YoY),
                  type = ifelse(type=='YoY', '', type)) 
  }
  
  return(d %>% select(date, source, province, type, var, value, unit))
}
  

wind.addmonths <- function(df) {
  alldates <- seq.Date(df$date[1]+1, max(df$date)+1, by='month')-1
  data.frame(date=alldates) %>%
    tidyr::crossing(df %>% distinct(source, province, type, var, unit)) %>%
    left_join(df) %>%
    mutate(duration_hours=lubridate::days_in_month(date) * 24)
}

wind.unYTD <- function(df) {
  
  wind.addmonths(df) -> df
  
  if(!is.null(df$type)) {
    if(class(df$type) %in% c('character', 'factor') && all(df$type!='YTD'))  do.ytd=F
    if(class(df$type) == 'logical' && all(!df$type)) do.ytd=F
    
    if(length(unique(df$type))>1) stop('passed data of mixed YTD/non-YTD type')
  }
  
  
  #fix NAs in February
  df <- df %>%
    group_by(var, source, type) %>%
    arrange(province, date) %>%
    group_modify(function(df, keys){
      ind <- which(month(df$date)==3 & is.na(dplyr::lag(df$value, 1)))
      if(length(ind)>0) {
        df$value[ind-1] <- df$value[ind]*(mean(df$value[month(df$date)==2], na.rm=T) /
                                            mean(df$value[month(df$date)==3], na.rm=T))
      }
      return(df)
    }) %>%
    ungroup()
      
  
  df <- df %>%
    group_by(var, source, province, type, year=lubridate::year(date)) %>%
    arrange(date) %>%
    group_modify(function(df, keys){
    
      # print(keys)
      df$prevdate <- dplyr::lag(df$date, 1)
      df$prevval <- dplyr::lag(df$value, 1)
      df$value1m <- df$value - df$prevval
      df$value1m[month(df$date)==1] <- df$value[month(df$date)==1]
  
          
      ind <- which(month(df$date)==2 & is.na(df$prevval) & !is.na(df$value))
      
      if(length(ind)==1){
        # Interpolate 1-2-3
        # value is in MWh. We convert to MW because of different hours in Jan and February
        # Feb is probably NA because its value1m is na
        
        #                   JAN     FEB     MAR
        # MW                a       b       c
        # MWh               d       e       f
        # Cumulated MWh     d       h       i
        i <- df$value[ind+1]
        h <- df$value[ind]
        
        dh1 <- as.numeric(df$duration_hours[ind-1])
        dh2 <- as.numeric(df$duration_hours[ind])
        dh3 <- as.numeric(df$duration_hours[ind+1])
        
        # Equation
        # g^2 * h * dh3 + g * ((h-i)*dh2) + (h-i)*dh1 = 0
        gs <- polyroot(cbind((h-i)*dh1, ((h-i)*dh2), h*dh3)) %>% Re()
        
        if(all(is.na(gs))){
          df$value1m[ind-1] <- NA
          df$value1m[ind] <- NA
        }else{
          g <- max(gs)
          
          # a*dh1 + a*g*dh2=h
          a = h/(dh1+dh2*g)
          b = a * g
          
          assertthat::are_equal(a*dh1 + b*dh2, h)
          
          df$value1m[ind-1] <- a * dh1
          df$value1m[ind] <- b * dh2  
        }
      }
      
      return(df)
    })
  
  df %>%
    ungroup() %>%
    dplyr::select(-tidyr::starts_with("prev")) %>%
    filter(var=="Output") %>%
    select(source, province, date, value=value1m, unit)
}