# Snap FIRMS fire data to CA raster grid, keeping all grid points which have had a fire in 2000-2021.
rm(list = ls())

library(terra)
library(tidyverse)
library(sf) 
library(raster)

#Dir = "wildfire_mitigation/raw_data/"
#outDir = "wildfire_mitigation/processed_data/"
Dir = "..data/raw_data"
outDir = "..data/processed_data"

# CA Bound without tigris:
#https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2020.html#list-tab-BG8ZITUQ783GX73G14
# The file URL is:
# "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_500k.zip"
obj = st_read(file.path(Dir, "cb_2020_us_state_500k", "cb_2020_us_state_500k.shp"))
CA_bound = subset(obj, NAME == "California")
CA_bound = st_transform(CA_bound, crs = 4326)

# active fire data downloaded from FIRMS (https://firms.modaps.eosdis.nasa.gov/active_fire/) 
Fire_ca_rough <- st_read(dsn = file.path(Dir, "DL_FIRE_M-C61_300726", "fire_archive_M-C61_300726.shp")) %>% 
  st_set_crs(4326) %>% 
  subset(ACQ_DATE < "2022-01-01")

Fire_poi <- st_intersects(st_as_sf(CA_bound$geometry), st_as_sf(Fire_ca_rough$geometry))
Fire_ca <- Fire_ca_rough[Fire_poi[[1]], ]

# Geographic coordinate system file downloaded from GPW (https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11)
str_name <- file.path(Dir, "gpw-v4-population-count-rev11_2020_30_sec_tif", "gpw_v4_population_count_rev11_2020_30_sec.tif") 
imported_raster <- raster(str_name) %>% setExtent(extent(c(-124.5, -114, 32, 42.1)), keepres=TRUE)
raster_pts = SpatialPoints(imported_raster)
gpw_grid <- data.frame(raster_pts@coords)
colnames(gpw_grid) <- c("LONGITUDE", "LATITUDE")
gpw_grid <- st_as_sf((gpw_grid),
                     coords = c("LONGITUDE", "LATITUDE"),
                     crs = 4326,
                     remove=FALSE)
gpw_grid_ca_inds = st_intersects(st_as_sf(CA_bound$geometry), st_as_sf(gpw_grid$geometry))
gpw_grid_ca = gpw_grid[gpw_grid_ca_inds[[1]], ]

# save the standardized coordinate system 
saveRDS(gpw_grid_ca, file = file.path(outDir, "gpw_grid_ca.RDS"))
rm(raster_pts)
rm(gpw_grid)

# overlay FRIMS with the standardized geographic coordinate system
FIRMS_ca_gridded_full = st_join(Fire_ca, gpw_grid_ca, join=st_nearest_feature, suffix=c("_ignore", ""))
FIRMS_ca_gridded_full$year = as.numeric(format(FIRMS_ca_gridded_full$ACQ_DATE,'%Y'))
FIRMS_ca_gridded = FIRMS_ca_gridded_full[, c("BRIGHTNESS", "FRP", "year", "LATITUDE", "LONGITUDE")]

# Group by space and time (year), slow command
FIRMS_ca_grouped = FIRMS_ca_gridded %>% 
  group_by(LATITUDE, LONGITUDE, year) %>% 
  summarize(avg_BRIGHTNESS = mean(BRIGHTNESS), 
            max_FRP = max(FRP),
            avg_FRP = mean(FRP)) %>% 
  ungroup()

FIRMS_ca_grouped = st_drop_geometry(FIRMS_ca_grouped)
FIRMS_ca_grouped <- st_as_sf(FIRMS_ca_grouped,
                             coords = c("LONGITUDE", "LATITUDE"),
                             crs = 4326,
                             remove = FALSE)

saveRDS(FIRMS_ca_grouped, file = file.path(outDir, "FIRMS.RDS"))
