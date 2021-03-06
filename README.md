[![CircleCI](https://circleci.com/gh/hrvg/regionaldrivers.svg?style=svg)](https://circleci.com/gh/hrvg/regionaldrivers)

# `regionaldrivers`

## Purpose

Code supporting statistical analysis of factors driving classifications.

## How to install

```
# Install development version from GitHub
devtools::install_github("hrvg/regionaldrivers")
```

It is also likely you'll need to install the latest `terra` or `raster` version which has dependencies on GDAL (>= 3.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 6.0.0).

On Linux, you can do:

```
sudo add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable
sudo apt-get -q update
sudo apt-get -y install libgdal-dev libgeos-dev libproj-dev 
```

The `terra` [github page](https://github.com/rspatial/terra) has some insights for other operating systems.