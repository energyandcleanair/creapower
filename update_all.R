library(tidyverse)
library(creapower)


gcs.auth()
seq(2016, 2021) %>% lapply(creapower::data.update_generation, data_source="entso")
