library(tidyverse)
library(creapower)



seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="entso")

seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="eia")

seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="posoco")

seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="bmrs")
