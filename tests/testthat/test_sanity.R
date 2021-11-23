library(testthat)


test_that("[ENTSO] Power generation data makes sense", {
  
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



test_that("[Others] Power generation data makes sense", {
  
  
  # Thailand
  gen_thai <- get_generation("thailand", date_from="2020-01-01", date_to="2020-12-31")
  
  
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

test_that("[CHINA] Power generation data makes sense", {
  
  gen_raw <- get_generation("wind", date_from="2018-01-01", date_to="2021-12-31")
  
  
  
  gen <- gen_raw %>%
    filter(!is.na(iso2)) %>%
    group_by(iso2, source, year=lubridate::year(date), month=lubridate::month(date)) %>%
    mutate(output_mwh=output_mw * duration_hours) %>%
    summarise(output_twh=sum(output_mwh)/1e6)
  
  
  ggplot(gen %>%   filter(source %in% c("Solar","Wind"))) +
    geom_bar(stat="identity", aes(month, output_twh, fill=factor(year)), position="dodge") +
    facet_wrap(~source)
  
  # Comparing with CEC data
  # Specifically, power generation from hydro, thermal, nuclear, grid-connected wind and solar sources
  # reached 1,360TWh, 5,170TWh, 366.2TWh, 466.5TWh and 261.1TWh
  # https://english.cec.org.cn/detail/index.html?3-1128#:~:text=In%202020%2C%20China%20power%20generation,16.6%25%20year%20on%20year%20respectively.
  cec2020 <- list(
    Hydro=1360,
    Thermal=5170,
    Nuclear=366.2,
    Wind=466.5,
    Solar=261.1)

  compare2020 <- gen %>% 
    filter(year==2020) %>%
    group_by(iso2, source, year) %>%
    summarise(output_twh=sum(output_twh)) %>%
    mutate(output_twh_cec=recode(source, !!!cec2020),
           diff_rel = (output_twh_cec-output_twh)/output_twh_cec)

  
  # Comparing on first two months
  wind1to2 <- tibble(
    Wind=c(63.81,
           65.32,
           65.94,
           115.19),
    Solar=c(13.11752,
            14.63,
            17.62,
            23.99
            ),
    data_source="Wind",
    month="1-2",
    year=c(2018,2019,2020,2021)
  ) %>%
    tidyr::pivot_longer(cols=c(Wind,Solar), names_to="source", values_to="output_twh")
  
  nbsc1to2 <- tibble(
    Wind=c(914.5, 593.6, 571.7,	568.1)/10,
    Solar=c(246.7,	179.2,	147.5,	131.4)/10,
    data_source="NBSC",
    month="1-2",
    year=c(2021,2020,2019,2018)
  ) %>%
    tidyr::pivot_longer(cols=c(Wind,Solar), names_to="source", values_to="output_twh")
  
  unique(gen_raw$source)
  
  gen1to2 <- gen_raw %>%
    mutate(year=lubridate::year(date),
           month=lubridate::month(date),
           data_source="CREA",
           output_twh=output_mw*duration_hours/1e6) %>%
    filter(source %in% c("Wind", "Solar"),
           month <= 2) %>%
    group_by(data_source, source, year) %>%
    summarise(output_twh=sum(output_twh)) %>%
    mutate(month="1-2")
  
  
  # Comparing on first three months
  wind1to3 <- tibble(
    Wind=c(9808000e7/1e12, 10412000e7/1e12, 11537000e7/1e12, 17373000/1000/100),
    Solar=c(1983296e7/1e12, 2417241e7/1e12, 3049154e7/1e12, 3768260e7/1e12),
    data_source="Wind",
    month="1-3",
    year=c(2018,2019,2020,2021)
  ) %>%
    tidyr::pivot_longer(cols=c(Wind,Solar), names_to="source", values_to="output_twh")
  
  nbsc1to3 <- tibble(
    Wind=c(87.16,  91.55, 102.24, 140.06),
    Solar=c(19.88, 24.4, 31.01, 38.77),
    data_source="NBSC",
    month="1-3",
    year=c(2018,2019,2020,2021)
  ) %>%
    tidyr::pivot_longer(cols=c(Wind,Solar), names_to="source", values_to="output_twh")
  
  unique(gen_raw$source)
  
  gen1to3 <- gen_raw %>%
    mutate(year=lubridate::year(date),
           month=lubridate::month(date),
           data_source="CREA",
           output_twh=output_mw*duration_hours/1e6) %>%
    filter(source %in% c("Wind", "Solar"),
           month <= 3) %>%
    group_by(data_source, source, year) %>%
    summarise(output_twh=sum(output_twh)) %>%
    mutate(month="1-3")
  
  compare <- bind_rows(
    wind1to2,
    nbsc1to2,
    gen1to2,
    wind1to3,
    nbsc1to3,
    gen1to3
  )
  
  
  compare_wide <- compare %>%
    tidyr::pivot_wider(names_from=data_source, values_from="output_twh") %>%
    mutate(err=CREA-Wind)
  
  expect_lt(max(abs(compare_wide$err)), 1e-10)
  
  compare %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(year,output_twh,fill=data_source),
             position="dodge") +
    facet_wrap(paste0("Months ", month)~source, scales="free_y") +
    rcrea::theme_crea() +
    rcrea::scale_fill_crea_d(name="Data source") +
    labs(subtitle="China cumulated power generation",
         y="TWh",
         x=NULL)
  
})
  