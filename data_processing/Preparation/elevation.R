library(terra)#
library(tidyverse)
library(data.table)
library(sf) 
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)
library(raster)
library(tigris)
library(fstcore)
library(ncdf4) # package for netcdf manipulation

Dir = "../data/raw_data/"
outDir = "../data/processed_data/"

obj = st_read(file.path(Dir, "cb_2020_us_state_500k", "cb_2020_us_state_500k.shp"))
CA_bound = subset(obj, NAME == "California")
CA_bound = st_transform(CA_bound, crs = 4326)

elev <- raster(file.path(Dir, "elevation_1KMmd_GMTEDmd.tif"))

elev_ca <- crop(elev, extent(CA_bound))
#elev_ca <- mask(elev_ca, CA_bound)

fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))

#elev_grid_ca <- raster::extract(elev_ca, fveg_grid_ca_poly, na.rm = TRUE)

elev_grid_ca_poly <- raster::extract(elev_ca, fveg_grid_ca_poly, 
                             fun = mean, na.rm = TRUE)

fveg_elev_grid_ca_poly <- cbind(fveg_grid_ca_poly, elev_grid_ca_poly)
colnames(fveg_elev_grid_ca_poly)[3:4] <- c("fveg", "elev")

saveRDS(fveg_elev_grid_ca_poly, file = file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))
