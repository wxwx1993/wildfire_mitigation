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

Dir = "wildfire_mitigation/raw_data/"
outDir = "wildfire_mitigation/processed_data/"
resultDir = "wildfire_mitigation/outputs/"

#CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
#CA_bound = st_transform(CA_bound, crs = 4326)
#firms <- readRDS(paste0(outDir, "FIRMS.RDS"))
gpw_grid_ca <- readRDS(paste0(outDir, "gpw_grid_ca.RDS"))

var_met <- c("sph", "vpd", "pr", "rmin", "rmax", "srad", "tmmn", "tmmx", "vs", "th", "pdsi", "pet", "etr",
             "erc", "bi", "fm100", "fm1000")
parameters <- expand.grid(2000:2021, var_met)
gridmet_mon <- data.frame(matrix(NA,nrow = nrow(gpw_grid_ca), ncol = 0))
gridmet_mon$LATITUDE = gpw_grid_ca$LATITUDE
gridmet_mon$LONGITUDE = gpw_grid_ca$LONGITUDE

for (par in 1:nrow(parameters)) {
stack_met <- stack(paste0(Dir, "Gridmet/", parameters[par, 2], "_", as.numeric(parameters[par, 1]), ".nc")) %>% crop(extent(c(-124.5, -114, 32, 42.5)), keepres=TRUE)
time <- seq(as.Date(paste0(as.numeric(parameters[par, 1]), "-01-01")), as.Date(paste0(as.numeric(parameters[par, 1]), "-12-31")), 1)
gridmet_n_list <- mclapply(1:12, function(months, par) {
  index <- which(year(time) == as.numeric(parameters[par, 1]) & month(time) == months)
  layer_met <- mean(stack_met[[index]], na.rm = T)
  layer_met <- data.frame(rasterToPoints((layer_met)))
  colnames(layer_met)[1:2] <- c("LONGITUDE", "LATITUDE")
    layer_met <- st_as_sf(layer_met,
                      coords = c("LONGITUDE", "LATITUDE"),
                      crs = 4326,
                      remove = FALSE)
  layer_met_gridded = st_drop_geometry(st_join(gpw_grid_ca, layer_met, join=st_nearest_feature, suffix=c("", "_ignore"))[-c(3:4)]) 
  return(layer_met_gridded[, 3])
 }, par = par, mc.cores = 12)
gridmet_n_df <- data.frame(do.call(cbind, gridmet_n_list))
rm(gridmet_n_list)
colnames(gridmet_n_df) <- paste0(parameters[par,2], "_", parameters[par,1], "_", 1:12)
gridmet_mon <- cbind(gridmet_mon, gridmet_n_df)
}

write_fst(gridmet_mon, path = paste0(outDir, "gridmet_mon.fst"))

# if select 2000-2010 monthly data
gridmet_mon2010 <- gridmet_mon[, c(1,2,as.numeric(outer(c(3:(2 + 12*11)), seq(0, 15, 1) * (22*12), FUN = "+")))]



