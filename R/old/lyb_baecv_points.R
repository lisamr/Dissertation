# setup ========================================================================
libs <- c("tidyverse", "sf", "raster")
lapply(libs, library, character.only = TRUE)

# data import, projections defined =============================================
csv_file <- "/home/a/projects/Jones_Study/data/locations.csv"
latlong <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

# created by welty et al 2017
# download_from https://www.sciencebase.gov/catalog/item/59667039e4b0d1f9f05cf2ff
gb_fires <- st_read("/home/a/data/fire/wf1870_2015/Wildfires_1870_2015_Great_Basin.shp")

# i made this from the usgs baecv data (takes a long time)
baecv_lyb <- raster("/home/a/data/fire/baecv/lyb_usa_baecv_1984_2015.tif")
baecv_crs <- crs(baecv_lyb, asText = TRUE)

# extract last year burned =====================================================
dd <-read_csv(csv_file)%>%
  st_as_sf(coords=c("Longitude","Latitude"), crs = latlong) %>%
  st_transform(st_crs(baecv_crs)) %>%
  dplyr::select(Site_number = "Site number", lyb_est = "Latest Year Burned", Site.type = "Site type")%>%
  mutate(lyb_baecv = raster::extract(baecv_lyb, .)) %>%
  left_join(read_csv(csv_file)%>%
              st_as_sf(coords=c("Longitude","Latitude"), crs = latlong) %>%
              dplyr::select(Site_number = "Site number")%>%
              st_transform(st_crs(gb_fires))%>%
              st_intersection(gb_fires) %>%
              st_set_geometry(NULL) %>%
              arrange(desc(Fire_Year)) %>%
              group_by(Site_number) %>%
              summarise(lyb_gb = max(Fire_Year),
                        fire_name = first(Fire_Name)) %>%
              ungroup(), 
            by = "Site_number") %>%
  st_write("data/last_year_burned.gpkg", delete_dsn = TRUE)


