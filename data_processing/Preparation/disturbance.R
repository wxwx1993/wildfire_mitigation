library("dataverse")
library("tibble") # to see dataframes in tidyverse-form
library(dplyr)
library(sf) 
library(parallel)
library(raster)
library(fst)


Dir = "../data/raw_data/"
outDir = "../data/processed_data/"

# Import the data contains the pixels of our interest in California.
fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))
#st_transform(gpw_grid_ca[1:2,], crs = crs(fveg))

# slow!! run via HPC
# Import all disturbance data (they are in 5000 * 5000 meters tiles, all combines to California) 
# (See: https://www.usgs.gov/landsat-missions/landsat-shapefiles-and-kml-files)
f <- list.files(file.path(Dir, "disturbance"),
                pattern = "\\.tif",
                full.names = TRUE)

# combine all 5000 * 5000 meters tiles into a whole California map) 
# Different band means different years (band = 16 is Year 2000)
disturbance.list <- lapply(1:length(f), function(i) raster(f[[i]], band = 16))
# project the map from meter coordinate reference system (CRS) to longitude-latitude system
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
st_crs(fveg_grid_ca_poly) <- llprj

start <- Sys.time()
fveg_grid_ca_tile.list <- mclapply(1:33, function(i) {
  fveg_grid_ca_meter <- st_transform(fveg_grid_ca_poly, crs = crs(disturbance.list[[i]]))
  fveg_grid_ca_tile <- st_crop(fveg_grid_ca_meter, extent(disturbance.list[[i]]))
  disturbance.fire <- raster::extract(disturbance.list[[i]], fveg_grid_ca_tile, na.rm = TRUE)
  #print( Sys.time() - start)
  fveg_grid_ca_tile$fire = sapply(disturbance.fire, function(list) {sum(list == 1, na.rm = T)/length(list)})
  fveg_grid_ca_tile$timber = sapply(disturbance.fire, function(list) {sum(list == 2, na.rm = T)/length(list)})
  fveg_grid_ca_tile$drought = sapply(disturbance.fire, function(list) {sum(list == 3, na.rm = T)/length(list)})
  fveg_grid_ca_tile$greening = sapply(disturbance.fire, function(list) {sum(list == 4, na.rm = T)/length(list)})
  fveg_grid_ca_tile$browning = sapply(disturbance.fire, function(list) {sum(list == 5, na.rm = T)/length(list)})
  #print( Sys.time() - start )
  st_geometry(fveg_grid_ca_tile) <- NULL
  return(fveg_grid_ca_tile[,c(1:2,4:8)])
}, mc.cores = 12)
print( Sys.time() - start )

# after run through HPC
disturbance_summary.list <- mclapply(2000:2020, function(year) {
  f <- list.files(file.path(outDir, "disturbance", year),
                  pattern = "\\.rds",
                  full.names = TRUE)
  disturbance.list <- lapply(f, readRDS)
  disturbance <- bind_rows(disturbance.list)
  df_joined <- fveg_grid_ca_poly[,c("LONGITUDE", "LATITUDE")] %>% 
    left_join(disturbance, by = c("LONGITUDE", "LATITUDE")) %>%
    group_by(LONGITUDE, LATITUDE) %>%
    summarise_at(.var = c("fire", "timber", "drought", "greening","browning"), list(max = ~max(., na.rm = TRUE)))
  df_joined <- st_drop_geometry(df_joined)
}, mc.cores = 11)

disturbance_summary <- Reduce(function(x, y) merge(x, y, by =c("LONGITUDE", "LATITUDE"), all = TRUE), 
                              disturbance_summary.list)
colnames(disturbance_summary)[3:ncol(disturbance_summary)] <- do.call(paste0, 
                                                                    expand.grid(c("fire_disturb_", "timber_", "drought_", "greening_","browning_"), 
                                                                                2000:2020))
sapply(disturbance_summary, function(x) sum(is.infinite(x)))
disturbance_summary[] <- lapply(disturbance_summary, function(x) replace(x, is.infinite(x) & x < 0, 0))

# disturbance data ready for analysis
write_fst(disturbance_summary, path = file.path(outDir, "disturbance.fst"))
