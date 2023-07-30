library("dataverse")
library("tibble") # to see dataframes in tidyverse-form
library(dplyr)
library(sf) 
library(parallel)
library(raster)
library(fst)

Dir = "../data/raw_data/"
outDir = "../data/processed_data/"

# download fractional vegetation
fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))

vegetation_file <- get_dataset("doi:10.7910/DVN/KMBYYM", server = "dataverse.harvard.edu")

vegetation_file_name <- vegetation_file$files$label[grepl(paste(2000:2020, collapse = "|"), vegetation_file$files$label)]

for (index in 1:length(vegetation_file_name)) {
veg_f <- get_file_by_name(filename = vegetation_file_name[index],
                       dataset = "10.7910/DVN/KMBYYM",
                       server = "dataverse.harvard.edu",
                       format = "original",
                       original = TRUE)
writeBin(veg_f, file.path(Dir, "fractional_vegetation", vegetation_file_name[index]))
}

# slow!! run via HPC
f <- list.files(file.path(Dir, "fractional_vegetation", 2000),
                pattern = "\\.tif",
                full.names = TRUE)

# combine all 5000 * 5000 meters tiles into a whole California map) 
fractional_veg.list <- lapply(1:length(f), function(i) raster(f[[i]], band = 1))

llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
st_crs(fveg_grid_ca_poly) <- llprj
fveg_grid_ca_meter <- st_transform(fveg_grid_ca_poly, crs = crs(fractional_veg.list[[1]]))
fveg_grid_ca_tile <- st_crop(fveg_grid_ca_meter, extent(fractional_veg.list[[1]]))

fveg_grid_ca_tile$tree_cover <- raster::extract(fractional_veg.list[[1]], fveg_grid_ca_tile, fun = mean, na.rm = TRUE)

# load data from HPC
tree_cover_summary.list <- mclapply(2000:2020, function(year) {
  f <- list.files(file.path(outDir, "tree_cover", year),
                  pattern = "\\.rds",
                  full.names = TRUE)
  tree_cover.list <- lapply(f, readRDS)
  tree_cover <- bind_rows(tree_cover.list)
  tree_cover$tree_cover[is.na(tree_cover$tree_cover)] <- 0
  df_joined <- fveg_grid_ca_poly[,c("LONGITUDE", "LATITUDE")] %>% 
    left_join(tree_cover, by = c("LONGITUDE", "LATITUDE")) %>%
    group_by(LONGITUDE, LATITUDE) %>%
    slice_max(tree_cover, n=1) %>%
    distinct()
  df_joined = st_drop_geometry(df_joined)
}, mc.cores = 8)

tree_cover_summary <- Reduce(function(x, y) merge(x, y, by =c("LONGITUDE", "LATITUDE"), all = TRUE), 
                             tree_cover_summary.list)
colnames(tree_cover_summary)[3:ncol(tree_cover_summary)] <- paste("tree_cover", 2000:2020, sep = "_")

tree_cover_summary[] <- lapply(tree_cover_summary, function(x) replace(x, is.na(x), 0))

# tree cover data ready for analysis
write_fst(tree_cover_summary, path = file.path(outDir, "tree_cover.fst"))

## Visual check
# df_joined = st_drop_geometry(df_joined)
# df_joined <- df_joined %>%
#   group_by(LONGITUDE, LATITUDE) %>%
#   summarise(tree_cover = max(tree_cover, na.rm = TRUE))
# 
# df_joined = st_drop_geometry(df_joined)
# df_joined <- st_as_sf(df_joined,
#                              coords = c("LONGITUDE", "LATITUDE"),
#                              crs = 4326,
#                              remove = FALSE)
# plot(df_joined["tree_cover"])
# 
# ggplot(data = df_joined) +
#   geom_sf(aes(fill = tree_cover)) +
#   scale_fill_gradient(low = "blue", high = "red") +
#   theme_minimal()



