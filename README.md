
# creapower

<!-- badges: start -->
<!-- badges: end -->

`creapower` is a R package dedicated to **retrieving power generation data** from various sources, namely:
- EIA
- ENTSO-E
- POSOCO (through [Robbie Andrew's](https://robbieandrew.github.io/india/) repository)


Data is regularly updated and cached on CREA Google Bucket. Hence, this package does not require user to own api keys or security tokens for any of the various sources.

## Installation

You can install the released version of creapower from [CRAN](https://CRAN.R-project.org) with:

``` r
library(remotes)
remotes::install_github("energyandcleanair/creapower")
```

## Example


``` r
library(creapower)

# Get Germany generation data with homogenised sources (Coal, Hydro, Wind, Solar etc.)
creapower::get_generation(date_from="2021-01-01", iso2="DE")

# Get all available countries generation data with homogenised sources (Coal, Hydro, Wind, Solar etc.)
creapower::get_generation(date_from="2021-01-01")

# Get generation data with original sources (Coal, Hydro pump, Wind offshore, Wind onshore etc.)
creapower::get_generation(date_from="2021-01-01", iso2=c("EU","JP"), homogenise=F)

# Get generation data from specific data source(s)
creapower::get_generation(date_from="2021-01-01", iso2="DE", data_source="entso")

# Get generation data from specific data source(s)
creapower::get_generation(date_from="2021-01-01", data_source=c("entso","eia"))

# Available data sources
creapower::available_data_sources()

```

