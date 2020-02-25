#
# Australia dead fine fuel moisture content and fire history
# 1980 - 2018
# Version 2.1
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(sp)
library(raster) # rasterFromXYZ
library(lubridate) # ymd()
library(RColorBrewer) # brewer.pal
library(cowplot)
library(ggpubr) # ggarrange
library(vroom)
# mapping
library(leaflet)
library(leaflet.extras)
library(tmap)
library(tmaptools)
library(mapdeck)
library(geoshaper)

### Load study region FMC data
## 1-32 FMC thresholds
# filter(between(long, -130, -110), between(lat, 32, 56))
# filter(between(long, 133, 155), between(lat, -45, -25))
fmc_prop <- vroom('./input/demo/demo_fmc1-32.csv.gz') %>%
  filter(year >= 1980)
## Regression tree-based outputs
fmc_rt_prop <- vroom('./input/demo/demo_fmc-rt-prop.csv.gz') %>%
  filter(year >= 1980)
fmc_rt_thresh <- vroom('./input/demo/demo_fmc-rt-thresh.csv.gz') %>%
  mutate(year = 2020)
  
## Summary statistics
# TODO: NSW
# TODO: WNA
## Ecoregion data
ecoID <- vroom('./input/demo/demo_cov_ecoID.csv.gz')
## Elevation data
elev <- vroom('./input/demo/demo_cov_elev.csv.gz')
## ERA-5 climate & vegetation data
temp <- vroom('./input/demo/demo_cov_t2m.csv.gz') %>%
  mutate(mat = mat - 273.15,
         mhmt = mhmt - 273.15,
         mcmt = mcmt - 273.15,
         mdst = mdst - 273.15)
precip <- vroom('./input/demo/demo_cov_tp.csv.gz') %>%
  mutate(tp = tp * 1000,
         tpds = tpds * 1000)
veg <- vroom('./input/demo/demo_cov_veg.csv.gz') %>%
  mutate(slt = as.factor(round(slt)),
         tvh = as.factor(round(tvh)),
         tvl = as.factor(round(tvl)))
## MODIS hotspot data
modis <- vroom('./input/demo/demo_modis.csv.gz') %>%
  rename(burned_area = burnedArea) %>%
  replace_na( list( burned_area = 0,
                    frp_n = 0,
                    frp_sum = 0,
                    frp_mean = 0))
## clone lat-long & year data for interactions
latlong <- elev %>% 
  dplyr::select(lat, long) %>%
  distinct() %>%
  mutate(lat_int = lat,
         long_int = long)
yearx <- tibble(year = c(1979:2020),
                year_int = c(1979:2020))

### Color palettes
pal_climate <- brewer.pal(11, 'RdBu')
pal_neuromancer <- c('#fd34b2','#1f0f47', '#51e6ff')
pal_greeneyes <- c('#fd34b2', '#1f0f47', '#1bf913')
