# getting the growing degree days from ja-ma

library(raster)
library(tidyverse)
library(sf)

tmmn_path <- "/home/a/data/gridmet/tmmn"

m <- c(0, 273.15, 0,  273.15, 999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

x <- raster::stack(file.path(tmmn_path, "tmmn_2016.nc"))[[1:90]] %>%
  raster::reclassify(rclmat) %>%
  raster::calc(sum)

x13 <- raster::stack(file.path(tmmn_path, "tmmn_2013.nc"))[[1:90]] %>%
  raster::reclassify(rclmat) %>%
  raster::calc(sum)

gm_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

gdds <- read.csv("data/locations.csv") %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs = gm_crs) %>%
  dplyr::select(Site_number = "Site.number") %>%
  mutate(gdd13 = raster::extract(x13,.),
         gdd16 = raster::extract(x,.)) %>%
  st_set_geometry(NULL) %>%
  gather(key = Year, value = gdds,-Site_number) %>%
  mutate(Year = str_replace(Year, "gdd16","2016"),
         Year = str_replace(Year, "gdd13","2013"),
         Site_number = as.character(Site_number));gdds

write_csv(gdds, "data/growing_degree_days.csv")
