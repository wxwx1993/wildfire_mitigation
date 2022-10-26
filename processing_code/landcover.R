library(terra)#
library(tidyverse)
library(data.table)
library(sf) 
library(parallel)
library(raster)
library(tigris)
library("fst")
#Dir="C:/Users/Pan Dulce/Downloads/supporting files/"
#outDir = "C:/Users/Pan Dulce/Downloads/gpw-v4-population-count-rev11_2020_30_sec_asc/gpw-v4-population-count-rev11_2020_30_sec_asc/processed_data/"

Dir = "wildfire_mitigation/raw_data/"
outDir = "wildfire_mitigation/processed_data/"
resultDir = "wildfire_mitigation/outputs/"

gpw_grid_ca <- readRDS(paste0(outDir, "gpw_grid_ca.RDS"))

landcover <- readRDS(paste0(outDir, "LandCover.RDS"))
landcover <- st_as_sf(landcover,
                     coords = c("LONGITUDE", "LATITUDE"),
                     crs = 4326,
                     remove = FALSE)
plot(landcover[,c(3,163)])
plot(landcover[,c( (3 + 2*20),163)])



# dir = "~/Dropbox/gpw-v4-population-count-rev11_2020_30_sec_asc/processed_data"

df = read.fst(file.path(outDir, "gridmet_mon.fst"),
              from = 1,
              to = NULL,
)
#year up to 2010
col.idx = c(1,2, as.numeric(outer(c(3:(2 + 12*11)), seq(0, 15, 1) * (22*12), FUN = "+")))
df = df[col.idx]

unit = paste0(df$LATITUDE, df$LONGITUDE)
df$unit = unit
df$treated = 0

treated.year = 2011
fire.df = readRDS(file.path(outDir, "FIRMS.RDS"))
fire.df$unit = paste0(fire.df$LATITUDE, fire.df$LONGITUDE)
fire.df = subset(fire.df, year %in% treated.year)
fire.df = unique(fire.df[c("unit")])

# treated = all units which had fire in treated.year
df[df$unit %in% fire.df$unit, "treated"] = 1
# Keep only covariates and treatment
df$unit = NULL
df$LATITUDE = NULL
df$LONGITUDE = NULL

df = cbind(df, landcover[,3:162])

#Dummy data
# n = 100000
# p = 20
# X = round(matrix(rnorm(n*p), n, p), 2)
# num.treated = 200
# treated = rep(0, n)
# treated[sample(n, num.treated)] = 1
# data = data.frame(X, W)


# With covariate balancing propensity score = CBPS MatchIt
# balances with some cbps moment condition, then uses the propensity
# scores implied by the solution to this to match on.
system.time(m <- matchit(treated ~ ., data = df, 
                         # distance = "cbps" # crashed
                         distance = "glm"
))
# summary(m)
# plot(m, type = "jitter", interactive = FALSE)

# Units with  m$weights equal to 1 are the matched sample.
matches = m$weights == 1
df.matches = df[matches, ]
df.matches$unit = unit[matches]
m$df.matches = df.matches # store the trt/control covariate matrix x1, x2, ...., treated, unit

saveRDS(m, file = file.path(dir, "MatchIt.RDS"))
saveRDS(m$df.matches, file = file.path(dir, "df.matches.RDS"))
print(Sys.time())

matches <- readRDS(paste0(outDir, "df.matches.RDS"))

Matchit <- readRDS(paste0(outDir, "MatchIt.RDS"))


