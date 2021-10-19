
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

This is a basic example which shows you how to solve a common problem:

``` r
library(creapower)

# Get generation data with homogenised sources (Coal, Hydro, Wind, Solar etc.)
creapower::data.get_generation(date_from="2021-01-01", iso2s="DE")

# Get generation data with original sources (Coal, Hydro pump, Wind offshore, Wind onshore etc.)
creapower::data.get_generation(date_from="2021-01-01", iso2s="DE", homogenise=F)

# Get generation data from specific data sources
creapower::data.get_generation(date_from="2021-01-01", iso2s="DE", data_sources="entso")

```

