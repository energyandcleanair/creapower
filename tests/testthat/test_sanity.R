library(testthat)


test_that("Power generation data makes sense", {
  
  gen_raw <- get_generation("entso", date_from="2020-01-01", date_to="2020-12-31")
  
  gen <- gen_raw %>%
    filter(!is.na(iso2)) %>%
    group_by(iso2, source, year=lubridate::year(date)) %>%
    mutate(output_mwh=output_mw * duration_hours) %>%
    summarise(output_twh=sum(output_mwh)/1e6)
  

  # Comparing with EMBER data
  download.file("https://ember-climate.org/wp-content/uploads/2021/01/Data-file-Europe-Power-Sector-2020.xlsx",
                "cache/ember.xlsx")
  
  ember <- readxl::read_xlsx("cache/ember.xlsx", sheet="Data") %>%
    select(year=Year,
           country_name=Area,
           source=Variable,
           output_twh=`Generation (TWh)`) %>%
    mutate(source=recode(source,
                         "Gas"="Fossil Gas",
                         "Hard Coal"="Coal",
                         "Lignite"="Coal")) %>%
    group_by(across(c(-output_twh))) %>%
    summarise_at("output_twh", sum) %>%
    mutate(iso2=countrycode(country_name, "country.name", "iso2c"))
  
  
  compare <- gen %>%
    inner_join(ember,
               by=c("year", "iso2", "source"),
               suffix=c("_crea","_ember")) %>%
    mutate(diff_abs = abs(output_twh_crea-output_twh_ember),
           diff_rel = diff_abs/output_twh_ember,
           ok=(diff_rel<0.25) | (diff_abs<1))
  
  # Not OK at the moment. EMBER data is different from (better than) ENTSO
  
  
})