
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nppr

<img src="https://raw.githubusercontent.com/chaoxv/figures/main/nppr.png" height="200" align="right" /></a>

<!-- badges: end -->

Tools for ocean productivity data downloading, reading, processing and
visualization. All data are available from
<http://sites.science.oregonstate.edu/ocean.productivity/index.php>.

## :writing_hand: Authors

Chao Xu

Xiamen University

## Installation

You can install the development version of nppr like so:

``` r
# install.packages("remotes")
remotes::install_github("chaoxv/nppr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(nppr)
```

1.  Download ocean productivity data

``` r
# Load supporting packages
library(RCurl)
library(XML)
library(R.utils)
library(tidyverse)
library(lubridate)

# Create an empty folder to save your ocean produtivity files.
yourfolder <- paste0(getwd(), '/', 'vgpm') 
dir.create(yourfolder)

# Dowmload the VGPM data. If it is not defined, the default  grid size is 'low', time span is 'monthly', satellite is 'MODIS'.
get_npp_vgpm(file.path = yourfolder,
             grid.size = 'low',
             time.span = 'monthly',
             satellite = 'MODIS',
             mindate = '2016-01-15', 
             maxdate = '2016-03-15') 
```

2.  Read hdf file

``` r
# Use the hdf file you have downloaded yet.
yourfile <- paste0(yourfolder, '/201601.vgpm')
read_hdf(file.path = yourfile)    
```

3.  Match the ocean productivity data by your longitude, latitude and
    date.

``` r
# Extract a single value
match_sig(file.path = yourfolder,
          lon = 120, lat = 20, date = '2016-03-01')

# Extract multiple values
mydat <- data.frame(lon = c(120, 112, 116), 
                    lat = c(17, 15, 18),
                    date = c('2016-03-04', '2016-03-07', '2016-02-04'))

match_df(mydat, file.path = yourfolder)  
```

4.  Ocean productivity data visualization.

``` r
# Load supporting packages

library(viridis)
#> 载入需要的程辑包：viridisLite
library(raster)
#> 载入需要的程辑包：sp
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
data(nppdata)

  ggplot()+
  geom_oce(nppdata, aes(x = lon, y = lat, fill = npp), 
           lonlim = c(100, 120), latlim = c(7, 25))+
  scale_fill_viridis(option = "D", 
                     direction = -1,
                     breaks = seq(50, 1050, 100),
                     limits = c(50, 1050))+
  labs(x = 'Longitude', y = 'Latitude',
  fill = expression(NPP*~'('*mg~C~m^-2~d^-1*')'))
#> Scale on map varies by more than 10%, scale bar may be inaccurate
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## :sparkling_heart: Contributing

We welcome any contributions! By participating in this project you agree
to abide by the terms outlined in the [Contributor Code of
Conduct](CONDUCT.md).
