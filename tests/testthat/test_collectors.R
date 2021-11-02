library(testthat)


test_that("Power generation collection works", {

  data_sources <- available_data_sources()
  
  for(data_source in data_sources){
    print(data_source)
    date_from <- lubridate::today(tzone="UTC")-7
    d <- collect_generation(data_source=data_source, date_from=date_from)
    while(nrow(d)==0){
      date_from <- date_from - 31
      d <- collect_generation(data_source=data_source, date_from=date_from)
    }
    
    # ISO2s are covered
    expect_true(all(unique(d$iso2) %in% data_source_iso2s(data_source=data_source)))
    
    # Shouldn't be grouped
    expect_false(is.grouped_df(d))
    
    # Dates should match
    expect_true(min(d$date) >= date_from)
    
    # data_source should match
    expect_true(unique(d$data_source) == data_source)
    
    # Test columns returned 
    expect_equal(sort(names(d)),
                 sort(c("iso2","region","data_source","date","source","output_mw","duration_hours")))
    
    # Test no NA data
    expect_true(!any(is.na(d$output_mw)))
    
    
    # Power generation sources are covered, not overlapping and homogenise properly
    source_greps <- data.source_homogenising_greps()
    d_homogenised <- homogenise_generation(d)
    expect_true(all(unique(d_homogenised$source) %in% names(source_greps)))
    
    # Either Thermal or Coal and/or Fossil Gas and/or Oil
    expect_true(
      xor(any(grepl("Thermal", unique(d_homogenised$source), ignore.case = T)),
        any(grepl("Coal|Gas|Oil", unique(d_homogenised$source), ignore.case = T))
        ))

    # Either Renewables or Wind & Solar separately
    expect_true(
      xor(any(grepl("^Renewables$|^Renewable$", unique(d_homogenised$source), ignore.case = T)),
          any(grepl("Wind|Solar", unique(d_homogenised$source), ignore.case = T))
      ))
    
    compare <- d %>%
      group_by(iso2, region, data_source, date) %>%
      summarise(output_mw1=sum(output_mw)) %>%
      left_join(
        d_homogenised %>%
          group_by(iso2, region, data_source, date) %>%
          summarise(output_mw2=sum(output_mw))
      )
    
    # No double counting
    expect_true(all(compare$output_mw1==compare$output_mw2))
    expect_true(nrow(compare)>0)
    
    
    # Hourly complete
    d.complete <- d %>%
      tidyr::complete(nesting(iso2, region, data_source, source), date,
                      fill=list(output_mw=0))
    
    expect_equal(nrow(d.complete), nrow(d))
  }
})

