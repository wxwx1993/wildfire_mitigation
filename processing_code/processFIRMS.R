# Snap FIRMS fire data to CA raster grid, keeping all grid points which had a fire in the 20 year period.
rm(list = ls())

library(terra)
library(tidyverse)
# library(data.table)
library(sf) 
# library(parallel)
# library(R.utils)
# library(lwgeom)
# library(RCurl)
library(raster)
library(tigris)

CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
CA_bound = st_transform(CA_bound, crs = 4326)

Dir = "wildfire_mitigation/raw_data/"
outDir = "wildfire_mitigation/processed_data/"
#Dir = "~Dropbox"
# poi <- st_as_sf((lat_lon_ca2),
#                 coords = c("LONGITUDE", "LATITUDE"),
#                 crs = 4326)
# poi_inter <- st_intersects(CA_bound, poi)
# poi_sf <- poi[poi_inter[[1]],]
# poi_sf <- st_buffer(poi_sf, dist = 1000 / 2)
Fire_ca_rough <- st_read(dsn = paste0(Dir, "FIRMS_MODIS_CA_to2022/fire_archive_M-C61_262030.shp")) %>% 
  st_set_crs(4326) 
# %>%
#   subset(LONGITUDE >= -124.5 & LONGITUDE <= -114 & LATITUDE >= 32, LATITUDE <= 42.1)
# Fire_ca_rough_points <- Fire_ca_rough$geometry
Fire_poi <- st_intersects(st_as_sf(CA_bound$geometry), st_as_sf(Fire_ca_rough$geometry))
Fire_ca <- Fire_ca_rough[Fire_poi[[1]], ]

plot(CA_bound$geometry)
plot(Fire_ca$geometry, pch=16,
     col=rgb(0,100,0,50,maxColorValue=255), add=TRUE)

str_name<-paste0(Dir, "gpw-v4-population-count-rev11_2020_30_sec_tif/gpw_v4_population_count_rev11_2020_30_sec.tif") 
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
plot(gpw_grid_ca$geometry, pch=16, cex=0.1,
     col=rgb(0,100,0,50,maxColorValue=255))
saveRDS(gpw_grid_ca, file = paste0(outDir, "gpw_grid_ca.RDS"))
rm(raster_pts)
rm(gpw_grid)

FIRMS_ca_gridded_full = st_join(Fire_ca, gpw_grid_ca, join=st_nearest_feature, suffix=c("_ignore", ""))
FIRMS_ca_gridded_full$year = as.numeric(format(FIRMS_ca_gridded_full$ACQ_DATE,'%Y'))
FIRMS_ca_gridded_full$month = as.numeric(format(FIRMS_ca_gridded_full$ACQ_DATE,'%m'))

head(FIRMS_ca_gridded_full)

FIRMS_ca_gridded = FIRMS_ca_gridded_full[, c("BRIGHTNESS", "FRP", "BRIGHT_T31","year", "month", "CONFIDENCE", "LATITUDE", "LONGITUDE")]

# Group by space and time (month), slow command
FIRMS_ca_grouped = FIRMS_ca_gridded %>% 
  group_by(LATITUDE, LONGITUDE, year, month) %>% 
  summarize(avg_BRIGHTNESS = mean(BRIGHTNESS), 
            avg_FRP = mean(FRP),
            avg_BRIGHT_T31 = mean(BRIGHT_T31),
            avg_CONFIDENCE = mean(CONFIDENCE)) %>% 
  ungroup()

FIRMS_ca_grouped = st_drop_geometry(FIRMS_ca_grouped)
FIRMS_ca_grouped <- st_as_sf(FIRMS_ca_grouped,
                             coords = c("LONGITUDE", "LATITUDE"),
                             crs = 4326,
                             remove = FALSE)

saveRDS(FIRMS_ca_grouped, file = paste0(outDir, "FIRMS.RDS"))
