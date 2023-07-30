library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)
library(raster)
library(tigris)
library("fst")
library(ncdf4) # package for netcdf manipulation
library(velox)

Dir = "../data/raw_data/"
outDir = "../data/processed_data/"

#CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
#CA_bound = st_transform(CA_bound, crs = 4326)
#firms <- readRDS(paste0(outDir, "FIRMS.RDS"))
#TODO:  1) Don't use tigris to get california census gridm, get it from census.cov
#https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2020.html#list-tab-BG8ZITUQ783GX73G14
# The file URL is:
# "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_500k.zip"
# (Say "Retrieved date XXX")
gpw_grid_ca <- readRDS(paste0(Dir, "gpw_grid_ca.RDS"))

var_climate <- c("minat","maxat","prcp","wvp","swe")
parameters <- expand.grid(2000:2020, var_climate)

gridclimate_mon <- data.frame(matrix(NA,nrow = nrow(gpw_grid_ca), ncol = 0))
gridclimate_mon$LATITUDE = gpw_grid_ca$LATITUDE
gridclimate_mon$LONGITUDE = gpw_grid_ca$LONGITUDE


for (par in 1:nrow(parameters)) {
  
  gridclimate_n_list <- lapply(1:12, function(months) {
    stack <- stack(paste0(Dir, parameters[par, 2], "_", as.numeric(parameters[par,1]),
                          ".nc"),varname =paste0("Band",months))
    layer_climate <- mean(stack[[1]], na.rm = T)
    layer_climate <- data.frame(rasterToPoints((layer_climate)))
    colnames(layer_climate)[1:2] <- c("LONGITUDE", "LATITUDE")
    layer_climate <- st_as_sf(layer_climate,
                              coords = c("LONGITUDE", "LATITUDE"),
                              crs = 4326,
                              remove = FALSE)
    layer_climate_gridded = st_drop_geometry(st_join(gpw_grid_ca, layer_climate, 
                                                     join=st_nearest_feature,
                                                     suffix=c("","_ignore"))[-c(3:4)])
    return(layer_climate_gridded[, 3])
  })
  gridclimate_n_df <- data.frame(do.call(cbind, gridclimate_n_list))
  rm(gridclimate_n_list)
  colnames(gridclimate_n_df) <- paste0(parameters[par,2], "_", parameters[par,1], "_", 1:12)
  gridclimate_mon <- cbind(gridclimate_mon, gridclimate_n_df)
}

write_fst(gridclimate_mon, path = paste0(outDir, "gridClimate_mon2.fst"))


