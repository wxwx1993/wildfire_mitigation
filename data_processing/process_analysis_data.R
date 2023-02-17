print(Sys.time())
rm(list = ls())
library(MatchIt)
library(fst)
library("sf")
library("tidyverse")
library("RCAL")
library("CVXR")
library("tigris")
library("mltools")
library("data.table")


outDir = "../data/processed_data"

#year up to
treated.year = 2008
area = "grasslands"

#col.idx = c(1,2, as.numeric(outer(c(3:(2 + 12*11)), seq(0, 15, 1) * (22*12), FUN = "+")))
#df = df[col.idx]
col.idx2 = c(1,2, as.numeric(outer(c(3:(2 + 12*(treated.year-2000))), seq(0, 4, 1) * (21*12), FUN = "+")))
col.idx3 = c(1,2, as.numeric(outer(c(3:(2 + 12*(treated.year-2000))), seq(0, 2, 1) * (22*12), FUN = "+")))
col.idx4 = c(1,2, as.numeric(outer(c(3:(2 + (treated.year-2000))), seq(3, 3, 1) * (20), FUN = "+")))

landcover <- readRDS(file.path(outDir, "LandCover.RDS"))
landcover <- st_as_sf(landcover,
                      coords = c("LONGITUDE", "LATITUDE"),
                      crs = 4326,
                      remove = FALSE)
st_geometry(landcover) <- NULL
landcover <- landcover[col.idx4]
landcover <- landcover %>%
#  filter_at(c(c(3:(2 + (treated.year-2000)))), all_vars(. <= 5))
#  filter_at(c(c(3:(2 + (treated.year-2000)))), all_vars(. >= 6 & . <= 7))
#  filter_at(c(c(3:(2 + (treated.year-2000)))), all_vars(. >= 8 & . <= 9))
  filter_at(c(c(3:(2 + (treated.year-2000)))), all_vars(. == 10))
landcover[,c(3:(2 + (treated.year-2000)))] <- lapply(landcover[,c(3:(2 + (treated.year-2000)))] , factor)
landcover <- cbind(landcover[,c(1:2)], one_hot(as.data.table(landcover[,c(3:(2 + (treated.year-2000)))])))
#df = cbind(df2, landcover[,3:ncol(landcover)])
landcover$unit = paste0(landcover$LATITUDE, landcover$LONGITUDE)
landcover$LATITUDE <- NULL
landcover$LONGITUDE <- NULL

df = read.fst(file.path(outDir, "gridClimate_mon.fst"),
               from = 1,
               to = NULL,
)
df = df[col.idx2]

fire_brightness_frp = read.fst(file.path(outDir, "fire_brightness_frp.fst"),
                               from = 1,
                               to = NULL,
)

fire_brightness_frp = fire_brightness_frp[col.idx3]

df = cbind(df, fire_brightness_frp[,3:ncol(fire_brightness_frp)])

unit = paste0(df$LATITUDE, df$LONGITUDE)
df$unit = unit

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
df <- merge(landcover, df, by = "unit", all.x = TRUE)

saveRDS(df, file = paste0("analysis_treated", treated.year, "_", area, ".RDS"))


