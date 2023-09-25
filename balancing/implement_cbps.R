## apply synthetic control approch via covariate balancing (Imai 2014, Zhao 2019, Tan 2020)
## calculate ATT weights for each units (the weights corresponding to each focal year and land type)
library("sf")

source("cbps_ATT_regu.R")

outDir = "../data/processed_data/"

parameters =  expand.grid(c(2005:2020), c("conifer","hardwood"))

for (year_area in 1:nrow(parameters)) {
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])
  
  df = readRDS(paste0(outDir, "/rev_analysis_low/analysis_treated", treated.year, "_", area, ".RDS"))

  W = df$treated

## Only keep covariates that aim to balance
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
    system.time(res <- cbps_att_regu(as.matrix(X.scl),
                                     W,
                                     theta.init = rep(0, ncol(X)+1),
                                     #method = "Nelder-Mead",
                                     control = list(trace=10, maxit = 5000),
                                     lambda = rep(10^{n-7}, ncol(X))))
    return(res)
    })
  converge_set = (sapply(res_regu.list, function(res) res$convergence))
  res = res_regu.list[[min(which(converge_set == 0))]]
  rho = 10^{min(which(converge_set == 0)) - 7}
  saveRDS(res, paste0(outDir, "/rev_res_low/res_histfire", treated.year, "_", area, rho ,".RDS"))
  gc()
}
