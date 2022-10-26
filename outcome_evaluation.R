print(Sys.time())
rm(list = ls())
#library(MatchIt)
#library(fst)
library("sf")
library("tidyverse")
#library("RCAL")
library("CVXR")
#library("tigris")
setwd("/zfs/gsb/intermediate-yens/wuxiao/wildfire")
treated.year = 2012
df = readRDS(paste0("analysis_treated", treated.year, "_all.RDS"))
res = readRDS(paste0("res_histfire", treated.year, ".RDS"))

# Outcome Analysis
df_weight <- df
df_weight$weight <- df_weight$treated *res$weights.1 + (1-df_weight$treated)*res$weights.0

#dir = "~/Dropbox/gpw-v4-population-count-rev11_2020_30_sec_asc/processed_data"
gpw_grid_ca = readRDS(file.path("gpw_grid_ca.RDS"))
st_geometry(gpw_grid_ca) <- NULL
FIRMS_ca_grouped = readRDS(file.path("FIRMS.RDS"))
# Create a unit identifier
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
gpw_grid_ca$unit = paste0(gpw_grid_ca$LATITUDE, gpw_grid_ca$LONGITUDE)

df_weight <- cbind(gpw_grid_ca[,1:2], df_weight)
#df_weight$unit = paste0(df_weight$LATITUDE, df_weight$LONGITUDE)

df_weight2 <- st_as_sf(df_weight[,c("LATITUDE", "LONGITUDE","weight")],
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs = 4326,
                       remove = FALSE)
df_weight2$logwt <- log(df_weight2$weight)
#df_weight2$loglogwt <- log(df_weight2$logwt - min(df_weight2$logwt))
df_weight2$logwt[df_weight2$logwt < quantile(df_weight2$logwt,0.25)] <- quantile(df_weight2$logwt,0.25)

#CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
#CA_bound = st_transform(CA_bound, crs = 4326)
#pdf(paste0("logweight_histfire", treated.year, ".pdf"))
#plot(CA_bound$geometry)
#plot(subset(df_weight2, weight != 1 & logwt > quantile(df_weight2$logwt,0.75))[c("logwt","geometry")], pch=16, cex = 0.05, add = TRUE)
#plot(subset(df_weight2, weight == 1)[c("logwt","geometry")], pch=16, cex = 0.1, col = "black", add = TRUE)
#dev.off()


control = df_weight$unit[df_weight$treated == 0]
treated = df_weight$unit[df_weight$treated == 1]

FIRMS_ca_grouped <- merge(FIRMS_ca_grouped, 
                          df_weight[,c("LATITUDE", "LONGITUDE", "weight")],
                          by = c("LATITUDE", "LONGITUDE"),
                          all.x = TRUE)
FIRMS_ca_grouped = FIRMS_ca_grouped[FIRMS_ca_grouped$unit %in% c(control, treated), ]
FIRMS_ca_grouped$treated = 0
FIRMS_ca_grouped[FIRMS_ca_grouped$unit %in% treated, ]$treated = 1

# Create a date range panel
start.year = min(FIRMS_ca_grouped$year)
end.year = max(FIRMS_ca_grouped$year)
start.month = min(subset(FIRMS_ca_grouped, year == start.year)$month)
end.month = max(subset(FIRMS_ca_grouped, year == end.year)$month)

start.date = as.Date(paste0(start.year, "-", start.month, "-01"))
end.date = as.Date(paste0(end.year, "-", end.month, "-01"))
date.range = seq(start.date, end.date, "months")
df.date.panel = data.frame(year = as.numeric(format(date.range, "%Y")), month = as.numeric(format(date.range, "%m")), date.obj = date.range)

# Create unit-date full panel
ix.date = rep(1:nrow(df.date.panel), length(unique(FIRMS_ca_grouped$unit)))
ix.unit = gl(length(unique(FIRMS_ca_grouped$unit)), nrow(df.date.panel))
df.panel = cbind(df.date.panel[ix.date, ], unit  = unique(FIRMS_ca_grouped$unit)[ix.unit])

# high FRP
high_frp = 0.9
FIRMS_ca_grouped$has.fire = 1
FIRMS_ca_grouped$has.hifire = 0
FIRMS_ca_grouped[FIRMS_ca_grouped$avg_FRP >= quantile(FIRMS_ca_grouped$avg_FRP, high_frp),]$has.hifire = 1

df_final = merge(df.panel,
                 FIRMS_ca_grouped[, c("year", "month", "unit", "has.fire", "avg_BRIGHTNESS", "avg_FRP", "treated", "has.hifire")],
                 by = c("year", "month", "unit"),
                 all.x = TRUE)
df_final$has.fire[is.na(df_final$has.fire)] = 0
df_final$treated[df_final$unit %in% control] = 0
df_final$treated[df_final$unit %in% treated] = 1
df_final <- merge(df_final, df_weight[,c("unit","weight")], by = c("unit"), all.x = TRUE)

#(sum(df_weight$weight))^2/sum(df_weight$weight^2)

df_final$weight.fire = df_final$has.fire * df_final$weight
df_final$weight.hifire = df_final$has.hifire * df_final$weight

df_final$weight.bright = df_final$avg_BRIGHTNESS * df_final$weight
df_final$weight.frp = df_final$avg_FRP * df_final$weight


df.freq = df_final %>%
  group_by(date.obj, treated) %>%
  summarise(sum.fire = sum(weight.fire),
            sum.hifire = sum(weight.hifire, na.rm = TRUE),
            fire.frac = sum.fire/sum(df_weight$treated),
            hifire.frac = sum.hifire/sum(df_weight$treated),
            bright.mean = sum(weight.bright, na.rm = T)/sum(df_weight$treated),
            frp.mean = sum(weight.frp, na.rm = T)/sum(df_weight$treated))

df.freq$year = format(df.freq$date.obj, format = "%Y")

df.freq.year = df.freq %>%
  group_by(year, treated) %>%
  summarise(fire.frac = sum(fire.frac),
            hifire.frac = sum(hifire.frac),
            bright.mean = sum(bright.mean),
            frp.mean = sum(frp.mean))

pdf(paste0("fire.frac.month", treated.year, ".pdf"))
with(subset(df.freq, treated == 0), {
  plot(date.obj, fire.frac, type = "l", ylim = c(0, max(df.freq$fire.frac)*0.4))
})
with(subset(df.freq, treated == 1), {
  lines(date.obj, fire.frac, type = "l", col = "red")
  abline(v = "2011-01-01", col = "blue")
})

abline(v = as.Date("2008-01-01"), col = "blue")
abline(v = as.Date("2009-01-01"), col = "blue")
legend(x = "left", legend = c("control", "treated"), col = c("black", "red"), pch = 1)

# plot(df.freq$date.obj, df.freq$has.fire, type = "l")
dev.off()

pdf(paste0("fire.frac.year", treated.year,".pdf"))
with(subset(df.freq.year, treated == 0), {
  plot(year, fire.frac, type = "l", ylim = c(0, max(df.freq$fire.frac)*0.6))
})
with(subset(df.freq.year, treated == 1), {
  lines(year, fire.frac, type = "l", col = "red")
  abline(v = "2011-01-01", col = "blue")
})

#abline(v = as.Date("2017-01-01"), col = "blue")
#abline(v = as.Date("2017-12-01"), col = "blue")
legend(x = "left", legend = c("control", "treated"), col = c("black", "red"), pch = 1)

# plot(df.freq$date.obj, df.freq$has.fire, type = "l")
dev.off()
  

pdf(paste0("fire.frac.month", treated.year,".hifire",high_frp, ".pdf"))
with(subset(df.freq, treated == 0), {
  plot(date.obj, hifire.frac, type = "l", ylim = c(0, max(df.freq$hifire.frac)*0.4))
})
with(subset(df.freq, treated == 1), {
  lines(date.obj, hifire.frac, type = "l", col = "red")
  abline(v = "2011-01-01", col = "blue")
})

abline(v = as.Date("2008-01-01"), col = "blue")
abline(v = as.Date("2009-01-01"), col = "blue")
legend(x = "left", legend = c("control", "treated"), col = c("black", "red"), pch = 1)

# plot(df.freq$date.obj, df.freq$has.fire, type = "l")
dev.off()

pdf(paste0("fire.frac.year", treated.year,".hifire",high_frp, ".pdf"))
with(subset(df.freq.year, treated == 0), {
  plot(year, hifire.frac, type = "l", ylim = c(0, max(df.freq$hifire.frac)))
})
with(subset(df.freq.year, treated == 1), {
  lines(year, hifire.frac, type = "l", col = "red")
  abline(v = "2011-01-01", col = "blue")
})

abline(v = as.Date("2017-01-01"), col = "blue")
abline(v = as.Date("2017-12-01"), col = "blue")
legend(x = "left", legend = c("control", "treated"), col = c("black", "red"), pch = 1)

# plot(df.freq$date.obj, df.freq$has.fire, type = "l")
dev.off()

plot(subset(df.freq, treated == 1 & date.obj >= "2012-01-01")$date.obj,
     subset(df.freq, treated == 1 & date.obj >= "2012-01-01")$frp.mean - subset(df.freq, treated == 0& date.obj >= "2012-01-01")$frp.mean,
     type = "l")

plot(subset(df.freq, treated == 1 & date.obj < "2011-01-01")$date.obj,
     subset(df.freq, treated == 1 & date.obj < "2011-01-01")$frp.mean - subset(df.freq, treated == 0& date.obj < "2011-01-01")$frp.mean,
     type = "l")

plot(subset(df.freq, treated == 1)$date.obj,
     subset(df.freq, treated == 1)$fire.frac - subset(df.freq, treated == 0)$fire.frac,
     type = "l")

plot(subset(df.freq, treated == 1 & date.obj >= "2012-01-01")$date.obj,
     subset(df.freq, treated == 1 & date.obj >= "2012-01-01")$fire.frac - subset(df.freq, treated == 0& date.obj >= "2012-01-01")$fire.frac,
     type = "l")

plot(subset(df.freq, treated == 1 & date.obj < "2011-01-01")$date.obj,
     subset(df.freq, treated == 1 & date.obj < "2011-01-01")$fire.frac - subset(df.freq, treated == 0& date.obj < "2011-01-01")$fire.frac,
     type = "l")


plot(subset(df.freq, treated == 1 & date.obj >= "2013-01-01")$date.obj,
     subset(df.freq, treated == 1 & date.obj >= "2013-01-01")$hifire.frac - subset(df.freq, treated == 0& date.obj >= "2013-01-01")$hifire.frac,
     type = "l")

plot(subset(df.freq, treated == 1 & date.obj < "2012-01-01")$date.obj,
     subset(df.freq, treated == 1 & date.obj < "2012-01-01")$hifire.frac - subset(df.freq, treated == 0& date.obj < "2012-01-01")$hifire.frac,
     type = "l")

