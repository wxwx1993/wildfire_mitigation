library(sf)   
library(dplyr)
library(raster)
library(rgdal)
library(dplyr)
library("parallel")
library(xtable)

Dir = "../data/raw_data/"
outDir = "../data/processed_data/"

obj = st_read(file.path(Dir, "cb_2020_us_state_500k", "cb_2020_us_state_500k.shp"))
CA_bound = subset(obj, NAME == "California")

fveg_elev_grid_ca_poly <- readRDS(file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))

#long-term burn monitoring assessments (https://www.mtbs.gov/)
mtbs.list <- mclapply(2001:2020, function(fire_year) {
#long-term burn monitoring assessments (https://www.mtbs.gov/)
  mtbs_CA <- raster(file.path(Dir, 
                           "fires",
                           paste0("mtbs_CA_", fire_year),
                           paste0("mtbs_CA_", fire_year, ".tif")))
  
  return(table(getValues(mtbs_CA))*0.0009)
}, mc.cores = 10)

mtbs.df <- t(sapply(mtbs.list, function(list) {
  aa <- list[c("1","2","3","4")]
  aa["1"] <- aa["1"] + aa["2"]
  aa["2"] <- aa["3"]
  aa["3"] <- aa["4"]
  return(aa)}
  ))

write.csv(format(mtbs.df[,c("1","2","3")], digits=1, nsmall=0), file = "mtbs.csv")

llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
projected_mtbs.list <- mclapply(2001:2020, function(fire_year) {
  mtbs_CA <- raster(file.path(Dir, 
                              "fires",
                              paste0("mtbs_CA_", fire_year),
                              paste0("mtbs_CA_", fire_year, ".tif")))
  projected_mtbs_CA <- projectRaster(mtbs_CA, crs = llprj, method = "ngb")
  mtbs_grid_ca_poly <- raster::extract(projected_mtbs_CA, fveg_elev_grid_ca_poly, fun = modal, na.rm = TRUE)
  
  return(mtbs_grid_ca_poly)
}, mc.cores = 10)

project_mtbs.df <- t(sapply(projected_mtbs.list, function(list) {
  aa <- list[c("1","2","3","4")]
  aa["1"] <- aa["1"] + aa["2"]
  aa["2"] <- aa["3"]
  aa["3"] <- aa["4"]
  return(aa*0.6390217)}
))

# rapid burn severity estimates (https://burnseverity.cr.usgs.gov/products/ravg)
ravg_2020_ba7 <- raster(file.path(Dir, 
                            "fires",
                            "ravg_2020_ba7.tif"))

ravg_2020_ba7_ca <- crop(ravg_2020_ba7, CA_bound)
ravg_2020_ba7_ca <- mask(ravg_2020_ba7_ca, CA_bound)

ravg_2020_ba7_class <- table(getValues(ravg_2020_ba7_ca))*0.0009
print(ravg_2020_ba7_class)


cbi4.list <- mclapply(2012:2020, function(fire_year) {

    ravg_2020_cbi4 <- raster(file.path(Dir, 
                                       "fires/cbi4",
                                       paste0("ravg_", fire_year, "_cbi4.tif")))
    CA_bound = st_transform(CA_bound, crs = crs(ravg_2020_cbi4))
    ravg_2020_cbi4_ca <- crop(ravg_2020_cbi4, CA_bound)
    ravg_2020_cbi4_ca <- mask(ravg_2020_cbi4_ca, CA_bound)
    
    return(table(getValues(ravg_2020_cbi4_ca))*0.0009)
}, mc.cores = 9)

cbi4.df <- t(sapply(cbi4.list, function(list) {
  aa <- list[c("1","2","3","4")]
  aa["1"] <- aa["1"] + aa["2"]
  aa["2"] <- aa["3"]
  aa["3"] <- aa["4"]
  return(aa)}
))

write.csv(format(cbi4.df[,c("1","2","3")], digits=1, nsmall=0), file = "cbi4.csv")

cc5.list <- mclapply(2012:2020, function(fire_year) {
  
  ravg_2020_cc5 <- raster(file.path(Dir, 
                                    "fires/cc5",
                                    paste0("ravg_", fire_year, "_cc5.tif")))
  CA_bound = st_transform(CA_bound, crs = crs(ravg_2020_cc5))
  
  ravg_2020_cc5_ca <- crop(ravg_2020_cc5, extent(CA_bound))
  ravg_2020_cc5_ca <- mask(ravg_2020_cc5_ca, CA_bound)

  return(table(getValues(ravg_2020_cc5_ca))*0.0009)
}, mc.cores = 9)

cc5.df <- t(sapply(cc5.list, function(list) {
  aa <- list[c("1","2","3","4")]
  aa["1"] <- aa["1"] + aa["2"]
  aa["2"] <- aa["3"]
  aa["3"] <- aa["4"]
  return(aa)}
))

write.csv(format(cbi4.df[,c("1","2","3")], digits=1, nsmall=0), file = "cbi4.csv")

# our fire intnesity data FRP
FIRMS_ca_grouped = readRDS(file.path(outDir, "FIRMS.RDS"))
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
st_geometry(FIRMS_ca_grouped) <- NULL
fire.df <- FIRMS_ca_grouped
fire.index = unique(fire.df[c("unit")])

fire.df_year <- fire.df %>% 
  group_by(LATITUDE, LONGITUDE, year) %>% 
  summarise(max_FRP = max(max_FRP))

# classify the fire types by max FRP for each years
# based on systems proposed in https://www.sciencedirect.com/science/article/abs/pii/S003442570800062X
fire.df_year$class <- 0 
fire.df_year[fire.df_year$max_FRP == 0,]$class <- 0
fire.df_year[0 < fire.df_year$max_FRP & fire.df_year$max_FRP < 100,]$class <- 1
fire.df_year[100 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 500,]$class <- 2
fire.df_year[500 <= fire.df_year$max_FRP,]$class <- 3
fire.df_year$unit <- paste0(fire.df_year$LATITUDE, fire.df_year$LONGITUDE)


st_geometry(fveg_elev_grid_ca_poly) <- NULL
st_geometry(fire.df_year) <- NULL

mtbs_grid_ca_poly <- read.fst(file.path(outDir, "mtbs_fire.fst"),
                              from = 1,
                              to = NULL,
)
fveg_elev_grid_ca_poly <- cbind(fveg_elev_grid_ca_poly, mtbs_grid_ca_poly)

frp_mtbs_table <- lapply(2001:2020, function(years) {
fire.df_year2 <- merge(fveg_elev_grid_ca_poly[,c(1:2,(years - 1993))], subset(fire.df_year, year == years), by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)
fire.df_year2 <- fire.df_year2 %>% 
  filter(fire.df_year2[, 3] > 0 | class >0)
table = table((fire.df_year2$class), (fire.df_year2$fire_))
colnames(table) <- paste("mtbs", colnames(table), sep = "_")
rownames(table) <- paste("frp", rownames(table), sep = "_")
return(table)
})
names(frp_mtbs_table) <- 2001:2020
write_rds(frp_mtbs_table, file = file.path(outDir, "frp_mtbs_table.rds"))
frp_mtbs_table <- readRDS(file.path(outDir, "frp_mtbs_table.rds"))

## create fire intensity-severity summary table
frp_mtbs_table2 = lapply(frp_mtbs_table, function(table) {
  if (nrow(table) == 4) {table <- table[-1, ]}
  else {table}
  c(t(table)*0.6390217)
})
table <- do.call("rbind", frp_mtbs_table2)
rownames(table) <- rep(2001:2020, each = 1)                      
latex_table <- xtable(table, digits = 0)
# Print the LaTeX code for the table
print(latex_table, type = "latex", include.rownames = TRUE)


df = sapply(2001:2020, function(treated.year) {
  table(subset(fire.df_year, year == treated.year)$class)
})

frp <- t(sapply(df, function(list) list[c("1","2","3")]*0.6390217))
write.csv(format(frp, digits=1, nsmall=0), file = "frp.csv")

## create fire intensity-severity cross-walk table
# Assuming df is your data frame
df1 <- read.csv("frp.csv")
df2 <- read.csv("mtbs.csv")
df3 <- read.csv("cbi4.csv")

df0 <- data.frame(matrix(, nrow = 11, ncol = 5))
colnames(df0) <- colnames(df3)
df3 <- rbind(df0, df3)
df <- cbind(df1, df2)
df <- cbind(df, df3)

# Convert the data frame to a LaTeX table
latex_table <- xtable(df)

# Print the LaTeX code for the table
print(latex_table, type = "latex", include.rownames = FALSE)



