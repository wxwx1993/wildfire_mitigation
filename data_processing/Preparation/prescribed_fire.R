library(sf)   
library(dplyr)
library(raster)
library(ggplot2)
library(grid) 
library(pBrackets) 
library(gridExtra)
library(Hmisc)

Dir = "../data/raw_data/"
outDir = "../data/processed_data/"
resultDir = "../data/outputs/"


FIRMS_ca_grouped = readRDS(file.path(outDir, "FIRMS.RDS"))
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
st_crs(FIRMS_ca_grouped) <- llprj


# Prescribed Fire Burns - California [ds397]
ds397 <- st_read(file.path(Dir, "prescribed_fire", "ds397/ds397.gdb"))
ds397 <- subset(ds397 , YEAR_ >= 2000)

st_geometry(ds397) <- "geometry"
#ds1327 <- st_read("ds1327/ds1327.gdb")
#oregon_layers <- st_layers(dsn = "ds1327/ds1327.gdb")
ds397_transformed <- st_transform(ds397, crs = st_crs(FIRMS_ca_grouped))
ds397_transformed <- ds397_transformed[-which(st_geometry_type(ds397_transformed) == "MULTISURFACE"),]
ds397_transformed = st_make_valid(ds397_transformed)

ds397_intersects.list <- lapply(2000:2021, function(year_c) {
  #year_c = 2000
  res <- st_intersects(subset(ds397_transformed, YEAR_ == year_c), subset(FIRMS_ca_grouped, year == year_c),
                       sparse = TRUE)
  FIRMS_ca_prescribed <- subset(FIRMS_ca_grouped, year == year_c)[unlist(res), ]
  return(FIRMS_ca_prescribed)
})

ds397_ca_prescribed = do.call("rbind", ds397_intersects.list)

# federal Prescribed Fire Burns - California [Clark Knight FACET]
facts_haz_all <- st_read(file.path(Dir, "prescribed_fire", "knight/facts_haz_all/facts_haz_all.shp"))

head(facts_haz_all)        # View the first few rows of the attribute table
summary(facts_haz_all)     # Summarize the attribute table
#plot(facts_haz_all) 

facts_haz_fire <- subset(facts_haz_all, disttype %in% c("Broadcast Burn",
                                                        "Fire Use",
                                                        "Jackpot Burn",
                                                        "Machine Pile Burn"))

facts_haz_fire$year = format(as.Date(facts_haz_fire$newdate, format="%Y-%m-%d"),"%Y")
facts_haz_fire$year[is.na(facts_haz_fire$year)] <- format(as.Date(facts_haz_fire$olddate[is.na(facts_haz_fire$year)], format="%Y-%m-%d"),"%Y")

facts_haz_fire_transformed <- st_transform(facts_haz_fire, crs = st_crs(FIRMS_ca_grouped))

facts_intersects.list <- lapply(2000:2021, function(year_c) {
  #year_c = 2000
  res <- st_intersects(subset(facts_haz_fire_transformed, year == year_c), subset(FIRMS_ca_grouped, year == year_c),
                       sparse = TRUE)
  FIRMS_ca_prescribed <- subset(FIRMS_ca_grouped, year == year_c)[unlist(res), ]
  return(FIRMS_ca_prescribed)
})

facts_ca_prescribed = do.call("rbind", facts_intersects.list)

# identify prescribed fires
FIRMS_ca_prescribed <- ds397_ca_prescribed %>%
  bind_rows(facts_ca_prescribed) %>%
  distinct()

saveRDS(FIRMS_ca_prescribed, file = file.path(outDir, "FIRMS_ca_prescribed.RDS"))

FIRMS_ca_prescribed <- st_drop_geometry(FIRMS_ca_prescribed)

# identify prescribed fires
FIRMS_ca_wild <- FIRMS_ca_grouped %>% 
  anti_join(FIRMS_ca_prescribed)

saveRDS(FIRMS_ca_wild, file = file.path(outDir, "FIRMS_ca_wild.RDS"))

FIRMS_ca_wild <- readRDS(file.path(outDir, "FIRMS_ca_wild.RDS"))
FIRMS_ca_prescribed <- readRDS(file.path(outDir, "FIRMS_ca_prescribed.RDS"))
FIRMS_ca_prescribed <- st_drop_geometry(FIRMS_ca_prescribed)
FIRMS_ca_wild <- st_drop_geometry(FIRMS_ca_wild)

# Create a dataframe for fire intensity comparison
FIRMS_ca_prescribed$prescribed = 1
FIRMS_ca_wild$prescribed = 0
df <- rbind(FIRMS_ca_wild, FIRMS_ca_prescribed)
df <- subset(df, max_FRP < 100)
  
# Create a density plot
ggplot(df, aes(x = log(max_FRP), fill = as.factor(prescribed))) +
  geom_density(alpha = 0.5) +
  #geom_density(aes(x = y), fill = "red", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Values", y = "Density", title = "Density Plot")

fveg_elev_grid_ca_poly <- readRDS(file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))

df <- merge(df, fveg_elev_grid_ca_poly, by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)

st_geometry(fveg_elev_grid_ca_poly) <- NULL
if (area == "forestland") {
  fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
    filter(fveg %in% c(31, 51))
} else if (area == "woodland") {
  fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
    filter(fveg %in% c(32, 52))
} else if (area == "conifer") {
  fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
    filter(fveg %in% c(31, 32))
} else if (area == "hardwood") {
  fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
    filter(fveg %in% c(51, 52))
} else if (area == "shrub") {
  fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
    filter(fveg %in% c(70))
}

res <- list()
k = 1  
for (area in c( "conifer", "hardwood")) {
  if (area == "forest") {
    fveg2 = c(31, 51)
  } else if (area == "conifer") {
    fveg2 = c(31, 32)
  } else if (area == "hardwood") {
    fveg2 = c(51, 52)
  } else if (area == "shrub") {
    fveg2 = c(70)
  }
res[[k]] <- ggplot(subset(df, fveg %in% fveg2), aes(x = log(max_FRP), fill = as.factor(prescribed))) +
  geom_density(alpha = 0.5) +
  scale_fill_discrete(name = "Prescribed Fire") +
  #geom_density(aes(x = y), fill = "red", alpha = 0.5) +
  theme(plot.margin = unit(c(0.2, 1, 0.2, 0.2), "lines"),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        text = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="bottom",
        legend.box="vertical",
        axis.text.y = element_text(angle = 30, hjust = 1)) +
  labs(x = "log(max FRP)", y = "Percentage", title = "Fire Intensity") +
  ggtitle(paste0("Fire Intensity, ", capitalize(area)))
  k <- k+1
}

res_combined <- grid.arrange(res[[1]], res[[2]], 
                             nrow = 1)

ggsave(file.path(resDir, "results", paste0("prescribed_intensity.jpeg")), 
       res_combined, 
       width = 10 / 1.6*2,
       height = 8.5 / 1.6,
       units = "in")

