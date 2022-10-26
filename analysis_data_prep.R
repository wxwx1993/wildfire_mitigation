print(Sys.time())
rm(list = ls())
library(MatchIt)
library(fst)
library("sf")
library("tidyverse")
library("RCAL")
library("CVXR")
library("tigris")

#source("/cbps/cbps_ATT.R")
outDir = "wildfire_mitigation/processed_data/"

df2 = read.fst(file.path(outDir, "gridClimate_mon.fst"),
              from = 1,
              to = NULL,
)
#year up to
treated.year = 2017

#col.idx = c(1,2, as.numeric(outer(c(3:(2 + 12*11)), seq(0, 15, 1) * (22*12), FUN = "+")))
#df = df[col.idx]
col.idx2 = c(1,2, as.numeric(outer(c(3:(2 + 12*(treated.year-2000))), seq(0, 4, 1) * (21*12), FUN = "+")))
col.idx3 = c(1,2, as.numeric(outer(c(3:(2 + 12*(treated.year-2000))), seq(0, 2, 1) * (22*12), FUN = "+")))
col.idx4 = c(1,2, as.numeric(outer(c(3:(2 + (treated.year-2000))), seq(0, 7, 1) * (20), FUN = "+")))


df2 = df2[col.idx2]

landcover <- readRDS(paste0(outDir, "LandCover.RDS"))
landcover <- st_as_sf(landcover,
                      coords = c("LONGITUDE", "LATITUDE"),
                      crs = 4326,
                      remove = FALSE)
st_geometry(landcover) <- NULL
landcover <- landcover[col.idx4]
df = cbind(df2, landcover[,3:ncol(landcover)])

fire_brightness_frp = read.fst(file.path(outDir, "fire_brightness_frp.fst"),
               from = 1,
               to = NULL,
)

fire_brightness_frp = fire_brightness_frp[col.idx3]

df = cbind(df, fire_brightness_frp[,3:ncol(fire_brightness_frp)])

unit = paste0(df$LATITUDE, df$LONGITUDE)
df$unit = unit

#treated.year = 2012
FIRMS_ca_grouped = readRDS(file.path(outDir, "FIRMS.RDS"))
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
st_geometry(FIRMS_ca_grouped) <- NULL
fire.df <- subset(FIRMS_ca_grouped, year == treated.year)
fire.df = unique(fire.df[c("unit")])

hist.fire.df <- subset(FIRMS_ca_grouped, year <= treated.year - 1)
hist.fire.df$has.fire = 1
hist.fire.df <- hist.fire.df %>%
  group_by(unit) %>%
  summarise(num.fire = sum(has.fire))
  
df <- merge(df, hist.fire.df, by = "unit", all.x = TRUE)
df$num.fire[is.na(df$num.fire)] <- 0
# treated = all units which had fire in treated.year
df$treated = 0
df[df$unit %in% fire.df$unit, "treated"] = 1
# Keep only covariates and treatment

saveRDS(df, file = paste0("analysis_treated", treated.year, "_all.RDS"))
