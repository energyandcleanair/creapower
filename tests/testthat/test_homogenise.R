library(testthat)



test_that("Source homogenisation doesn't lead to double counting", {
  
  data_sources <- data.available_data_sources()
  
  date_from <- lubridate::today() - 3
  
  for(data_source in data_sources){
    print(data_source)
    gen <- data.get_generation(data_source=data_source, date_from=date_from)
    
    gen_agg <- data.homogenise_generation(gen)
    
    compare <- gen %>%
      group_by(iso2, region, data_source, date) %>%
      summarise(output_mw1=sum(output_mw)) %>%
      left_join(
        gen_agg %>%
          group_by(iso2, region, data_source, date) %>%
          summarise(output_mw2=sum(output_mw))
      )
    
    # No double counting
    expect_true(all(compare$output_mw1==compare$output_mw2))
    
    # Only homogenised sources remaining
    expect_true(all(unique(gen_agg$source) %in% 
                      names(data.source_homogenising_greps())))
    
  }
})
    