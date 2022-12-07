library("sf")
source("cbps_ATT.R")

Dir = "../data/processed_data"
outDir = "../data/intermediate_res"

parameters =  expand.grid(c(2008:2020), c("forests", "savannas"))

for (year_area in 1:nrow(parameters)) {
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])

df = readRDS(paste0(Dir, "/analysis/analysis_treated", treated.year, "_", area, ".RDS"))

FIRMS_ca_grouped = readRDS(Dir, "/FIRMS_gridded.RDS")
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
st_geometry(FIRMS_ca_grouped) <- NULL
low_intensity <- 0.9

fire.df <- subset(FIRMS_ca_grouped, year == treated.year
                  & avg_FRP <= quantile(FIRMS_ca_grouped$avg_FRP, low_intensity))
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

W = df$treated
X = df
X$unit = NULL
X$LATITUDE = NULL
X$LONGITUDE = NULL
X$treated = NULL
X$num.fire = NULL

X.mean <- colMeans(X)
X.sd <- apply(X, 2, sd)
X.sd[X.sd == 0] <- 1 # in case Xj is constant.
X.scl <- scale(X, center = X.mean, scale = X.sd)


res_regu.list <- lapply(1:8, function(n) {

system.time(res <- cbps_att(as.matrix(X.scl),
                            W,
                            theta.init = rep(0, ncol(X)+1),
                            control = list(trace=10, maxit = 5000),
                            rhos = rep(10^{n-7}, ncol(X))))
  return(res)
})

converge_set = (sapply(res_regu.list, function(res) res$convergence))
res = res_regu.list[[min(which(converge_set == 0))]]
rho = 10^{min(which(converge_set == 0)) - 7}
saveRDS(res, paste0(outDir, "/weights/res_histfire", treated.year, "_", area, rho ,".RDS"))
}
