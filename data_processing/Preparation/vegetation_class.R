library(sf)   
library(dplyr)
library(raster)
library(rgdal)
library(dplyr)
library("parallel")

Dir = "wildfire_mitigation/raw_data"
outDir = "wildfire_mitigation/processed_data"

gpw_grid_ca <- readRDS(file.path(outDir, "gpw_grid_ca.RDS"))

gpw_grid_ca_poly <- gpw_grid_ca
int_lon <- min(abs(diff(sort(unique(gpw_grid_ca$LONGITUDE)))/2))
int_lat <- min(abs(diff(sort(unique(gpw_grid_ca$LATITUDE)))/2))
st_geometry(gpw_grid_ca_poly) <-  st_as_sfc(do.call(rbind, mclapply(1:nrow(gpw_grid_ca_poly), function(row) {
  st_as_sfc(st_bbox(c(xmin = gpw_grid_ca_poly$LONGITUDE[row] - int_lon,
                      xmax = gpw_grid_ca_poly$LONGITUDE[row] + int_lon,
                      ymin = gpw_grid_ca_poly$LATITUDE[row] - int_lat,
                      ymax = gpw_grid_ca_poly$LATITUDE[row] + int_lat),
                    crs = st_crs(4326))
  )}, mc.cores = 10)))

# CAL FIRE
fveg <- raster(file.path(Dir, "vegetation", "arc_fveg15_1.tif/fveg15_1.tif"))
fveg_attributes <- fveg@data@attributes[[1]]

# reproject to Lat/Lon
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
projected_fveg <- projectRaster(fveg, crs = llprj, method = "ngb")
values(projected_fveg) <- fveg_attributes$WHR13NUM[match(getValues(projected_fveg), 
                                                         fveg_attributes$ID)]

fveg_poly <- extract(projected_fveg, gpw_grid_ca_poly, 
                     fun = modal, na.rm = TRUE)
fveg_grid_ca_poly <- cbind(gpw_grid_ca_poly, fveg_poly)

fveg_grid_ca <- cbind(gpw_grid_ca, fveg_grid_ca$fveg_poly)

saveRDS(fveg_grid_ca_poly, file = file.path(outDir, "fveg_grid_ca_poly.RDS"))
saveRDS(fveg_grid_ca, file = file.path(outDir, "fveg_grid_ca.RDS"))

## Visual check
# st_crs(gpw_grid_ca) <- 4326
# st_area(st_transform(gpw_grid_ca[1:2,], crs = crs(fveg)))
# plot(fveg_grid_ca[,3:4])

