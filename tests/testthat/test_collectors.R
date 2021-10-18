library(testthat)


test_that("Power generation collection works", {

  data_sources <- c("entso")
  
  for(data_source in data_sources){
    collect_fn <- get(sprintf("%s.collect_generation", data_source))
    d <- collect_fn(date_from=lubridate::today()-2,
                    date_to=lubridate::today())
    
    # Shouldn't be grouped
    expect_false(is.grouped_df(d))
    
    # Test columns returned 
    expect_equal(sort(names(d)),
                 sort(c("iso2","region","data_source","date","source","output_mw")))
    
    # Test no NA data
    expect_true(!any(is.na(d$output_mw)))
    
    
    # Hourly complete
    d.complete <- d %>%
      tidyr::complete(nesting(iso2, region, data_source, source), date,
                      fill=list(output_mw=0))
    expect_equal(nrow(d.complete), nrow(d))
    
  }
  
  
})


plot_rasters <- function(rs,
                         add_adm=T,
                         adm_level=2){

  if(is.list(rs)){
    rs <- raster::stack(rs)
  }

  grid <- rs[[1]] %>% raster::raster()

  adm <- creahelpers::get_adm(adm_level, res="low") %>%
    creahelpers::cropProj(grid)


  pl <- rasterVis::levelplot(rs) +
    latticeExtra::layer(sp::sp.lines(adm, lwd=2, col='darkgray'),
          data=list(adm=adm))

  print(pl)
  return(pl)
}
