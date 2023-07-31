print(Sys.time())
rm(list = ls())
library(fst)
library("sf")
library("tidyverse")
library("mltools")
library("data.table")
library(Hmisc)
library(sp)
library("raster")


outDir = "../data/processed_data/"
resultDir = "../data/outputs/"

# Figure 3 covariate balance check

parameters <- expand.grid(c(2008:2020), c("conifer", "hardwood"))
covariates <- c("minat_", "maxat_", "prcp_", "swe_", "wvp_", "fire_", "avg_BRIGHTNESS_", "max_FRP_", 
                "fire_disturb_", "timber_", "drought_", "greening_", "browning_", "tree_cover_")
# year_area = 1                          
for (year_area in 1:nrow(parameters)) {
  #year_area <- 1
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])
  df = readRDS(file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
  
  weights = readRDS(list.files(file.path(outDir, "rev_res_low"),
                               pattern = paste0(treated.year, "_", area),
                               full.names = TRUE))
  df_weight <- df
  df_weight$weight <- df_weight$treated *weights$weights.1 + (1-df_weight$treated)*weights$weights.0
  
  post_bal <- NULL
  pre_bal <- NULL
  ncov = length(covariates)
  for (i in 1:(ncov)) {
    covariate = covariates[i]
  post_bal[i] <- (mean(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))  - 
    sum(rowMeans(subset(df_weight, treated == 0)[,grep(covariate, colnames(df))])* 
        subset(df_weight, treated == 0)$weight)/sum(subset(df_weight, treated == 0)$weight))/
    sd(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))
  
  pre_bal[i] <- (mean(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))  - 
    mean(rowMeans(subset(df_weight, treated == 0)[,grep(covariate, colnames(df))])))/
    sd(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))
  }
  
  post_bal <- replace(post_bal, is.infinite(post_bal), 0)
  pre_bal <- replace(pre_bal, is.infinite(pre_bal), 0)
  
  
  order <- order((pre_bal))
  balance <- data.frame(matrix(NA, nrow = ncov * 2, ncol = 0))
  balance$Covariates <- rep(1:ncov, 2)
  balance$SMD <- (c(sort((pre_bal)), post_bal[order]))
  balance$covariates_name <- rep(covariates[order], 2)
  balance$Scenarios <- c(rep("Unweighted: Orginal Data", ncov), rep("Weighted: Synthetic Control", ncov))
  balance$Scenarios <- factor(balance$Scenarios, levels = c("Unweighted: Orginal Data", "Weighted: Synthetic Control"))
  if (area == "forestland") {area = "forest"}
  balance_p <- ggplot(balance, aes(x=SMD, y=Covariates, colour=Scenarios)) + 
    scale_y_discrete(limit = 1:ncov,
                     labels = c("Min air temperature", "Max air temperature", "Precipitation", "Snow water equivalent",
                                "Water vapor pressure", "Fire frequency", "Avg fire brightness", "Max fire radiative power",
                                "Disturbance: fire", "Disturbance: timber harvest", "Disturbance: drought", "Disturbance: greening", "Disturbance: browning", "Vegetation: tree cover",
                                "Topography: elevation")[order]) + 
    scale_color_manual(breaks = c("Unweighted: Orginal Data", "Weighted: Synthetic Control"),
                       values=c("red", "blue")) + 
    geom_point() +
    geom_path() +
    theme_bw() +
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
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(paste0(treated.year, ", ", capitalize(area))) + 
    xlab("Standardized Mean Differences") +
    xlim(min(balance$SMD) - 0.01, max(balance$SMD) + 0.01)
  ggsave(file.path(resDir, "balance", paste0("Covariate_Balance" , treated.year, "across",  area, ".jpeg")), 
         balance_p, 
         width = 8.5 / 1.6,
         height = 11 / 1.6,
         units = "in")
}

## Figure 3 California maps for exposed and control regions

parameters <- expand.grid(c(2008:2020), c("conifer", "hardwood"))
for (year_area in 1:nrow(parameters)) {
  #year_area <- 1
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])

  df = readRDS(file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
  weights = readRDS(list.files(file.path(outDir, "rev_res_low"),
                               pattern = paste0(treated.year, "_", area),
                               full.names = TRUE))
  df_weight <- df
  df_weight$weight <- df_weight$treated *weights$weights.1 + (1-df_weight$treated)*weights$weights.0
  
  df_weight2 <- st_as_sf(df_weight[,c("LATITUDE", "LONGITUDE","weight")],
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = 4326,
                         remove = FALSE)
  df_weight2$logwt <- log(df_weight2$weight)
  df_weight2$logwt[df_weight2$logwt < quantile(df_weight2$logwt,0.1, na.rm = T)] <- quantile(df_weight2$logwt,0.1, na.rm = T)
  
  CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
  CA_bound = st_transform(CA_bound, crs = 4326)
  df_weight2$transparency <- (df_weight2$logwt- min(df_weight2$logwt))/(max(df_weight2$logwt)-min(df_weight2$logwt)) + 10^{-1}
  
  jpeg(file.path(resDir, "maps", paste0("logweight_histfire", treated.year, "_", area, "..jpeg")),
       width = 8.5*300, height = 11*300, quality = 100, res = 300)
  par(mar=c(0.1,1,2,1))
  if (area == "forestland") {area = "forest"}
  plot(CA_bound$geometry, main = paste0(treated.year, ", ", capitalize(area)), cex.main= 3)
  control = subset(df_weight2, weight != 1)[c("logwt","geometry","transparency")]
  plot(subset(df_weight2, weight != 1)[c("logwt","geometry")], pch=16, cex = 0.15, col = alpha("blue", control$transparency), add = TRUE)
  plot(subset(df_weight2, weight == 1)[c("logwt","geometry")], pch=16, cex = 0.2, col = "red", add = TRUE)
  legend("bottomleft", legend = c("Exposed", "Control"), fill = c("red", "blue"), cex=2)
  dev.off()
}

# Table 1 for fire frequency

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

fveg_elev_grid_ca_poly <- readRDS(file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))

st_geometry(fveg_elev_grid_ca_poly) <- NULL


results <- merge(fire.df_year, fveg_elev_grid_ca_poly, by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)

results = results %>%
  filter(class > 0)

df.list = sapply(2001:2020, function(treated.year) {
  c(table(subset(results, year == treated.year & fveg %in% c(31, 32))$class),
    table(subset(results, year == treated.year & fveg %in% c(51, 52))$class))
  })

df.list[[11]] <- append(df.list[[11]], 0)
df.list <- lapply(df.list, function(list) {
  round(list*0.6390217, digits = 0)
})
df <- do.call(rbind, df.list)

write.csv(df, "fire_freq.csv", row.names = F)

### Table for covariate balance of indivudal covariates

parameters <- expand.grid(c(2008:2020), c("conifer", "hardwood"))
balance_summary.df <- data.frame(area = rep(NA, nrow(parameters)),
                                 year = rep(NA, nrow(parameters)),
                                 pre = rep(NA, nrow(parameters)),
                                 post = rep(NA, nrow(parameters)))

for (year_area in 1:nrow(parameters)) {
  
treated.year = as.numeric(parameters[year_area, 1])
area = as.character(parameters[year_area, 2])
df = readRDS(file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))

weights = readRDS(list.files(file.path(outDir, "rev_res_low"),
                             pattern = paste0(treated.year, "_", area),
                             full.names = TRUE))
df_weight <- df
df_weight$weight <- df_weight$treated *weights$weights.1 + (1-df_weight$treated)*weights$weights.0

df_weight$unit <- NULL
df_weight$LATITUDE <- NULL
df_weight$LONGITUDE <- NULL
df_weight$num.fire <- NULL

df_weight <- df_weight[, -grep("V1_", colnames(df_weight))]
post_bal <- NULL
pre_bal <- NULL
ncov = ncol(df_weight)
bal.list <- mclapply(1:(ncol(df_weight) - 2), function(i) {
  #covariate = covariates[i]
  c((mean(subset(df_weight, treated == 1)[,i])  - 
       sum((subset(df_weight, treated == 0)[,i])* 
             subset(df_weight, treated == 0)$weight)/sum(subset(df_weight, treated == 0)$weight))/
      sd(subset(df_weight, treated == 1)[,i]),
    (mean(subset(df_weight, treated == 1)[,i])  - 
       mean(subset(df_weight, treated == 0)[,i]))/
      sd(subset(df_weight, treated == 1)[,i]))
}, mc.cores = 12)
bal = do.call("rbind", bal.list)

balance_summary.df$area[year_area] = area
balance_summary.df$year[year_area] = treated.year
balance_summary.df$pre[year_area] = paste0(sum(bal[,2] > 0.1, na.rm = T), " (", round(mean(bal[,2] > 0.1, na.rm = T)*100, digits=2), ")")
balance_summary.df$post[year_area] = paste0(sum(bal[,1] > 0.1, na.rm = T), " (", round(mean(bal[,1] > 0.1, na.rm = T)*100, digits=2), ")")
}
write.csv(balance_summary.df, "balance_summary_frp.csv")

## Figure for E-values

res <- list()
k = 1  
for (outcome in c("fire_all", "fire_90", "fire_95")) {
  for (biome in c("conifer", "hardwood")) {
    data.raw = Reduce(rbind, lapply(1:9, function(ll) {
      XX = read.csv(file.path(outDir, "rev_result_low",
                              paste0(biome, "_lag", ll, ".csv")))
      XX$lag = ll
      XX
    }))
    
    names(data.raw)[1] = "year"
    data.raw$end.year = data.raw$year + data.raw$lag
    
    data.reg = data.raw[c("lag", "year", "end.year")]
    data.reg = rbind(data.reg, data.reg)
    data.reg$treat = c(rep(0, nrow(data.raw)), rep(1, nrow(data.raw)))
    data.reg$fire_all = c(data.raw$fire.0, data.raw$fire.1) * data.raw$pixels_burn
    data.reg$fire_90 = c(data.raw$hifire90.0, data.raw$hifire90.1) * data.raw$pixels_burn
    data.reg$fire_95 = c(data.raw$hifire95.0, data.raw$hifire95.1) * data.raw$pixels_burn
    
    all.end.years = 2009:2021
    all.lags = 1:9
    
    jackfun = function(end.years) {
      #regform = paste0(outcome, " ~ factor(end.year) * factor(lag) + treat + treat:poly(lag, 3)")
      #reg.jack = glm(formula(regform),
      #            family = quasipoisson,
      #            data = subset(data.reg, end.year %in% end.years))
      regform = paste0(outcome, " ~ factor(end.year) * factor(lag) + treat + treat:poly(lag, 1)")
      #regform = paste0(outcome, " ~ poly(end.year,1) * poly(lag, 1) + treat + treat:poly(lag, 1)")
      reg.jack = glm(formula(regform),
                     family = quasipoisson,
                     data = subset(data.reg, end.year %in% end.years))
      
      Xpred0 = data.frame(end.year=end.years[1], lag=all.lags, lag2=all.lags^2, treat=0)
      Xpred1 = data.frame(end.year=end.years[1], lag=all.lags, lag2=all.lags^2, treat=1)
      
      yy0 = predict(reg.jack, Xpred0)
      yy1 = predict(reg.jack, Xpred1)
      
      yy1 - yy0
    }
    
    full.reg = jackfun(all.end.years)
    
    jackreps = t(sapply(1:length(all.end.years), function(ii) { jackfun(all.end.years[-ii]) }))
    colnames(jackreps) = all.lags
    
    jackvar = apply(jackreps, 2, function(xx) { var(xx) * (length(xx) - 1)^2 / length(xx) })
    jackse = sqrt(jackvar)
    
    rat = exp(full.reg)
    ub.rat = exp(full.reg + 1.96 * jackse)
    lb.rat = exp(full.reg - 1.96 * jackse)
    results <-  data.frame(year = 1:length(rat), rate = rat, lower = lb.rat, upper = ub.rat, "land_type" = rep(capitalize(biome), length(rat)))
    
    if (outcome == "fire_all") {fire_type <- "all fires"} else 
      if (outcome == "fire_90") {fire_type <- "class 2-5 fires"} else 
        if (outcome == "fire_95") {fire_type <- "class 3-5 fires"} 
    if (biome == "forestland") {biome = "forest"}
    
    results_eval <- cbind(results[,c(1:2,5)], t(sapply(1:nrow(results), function(i) {
      evalues.RR(est = results[i,2], lo = results[i,3], hi = results[i,4])[2,]})))
    
    results_eval2 = results_eval[,c(1,6)]
    colnames(results_eval2) <- colnames(results_eval[,c(1,4)])
    results_eval2 = rbind(results_eval2, results_eval[,c(1,4)])
    results_eval2$Types <- c(rep("95% CI upper limit", nrow(results_eval)),rep("Main Effect", nrow(results_eval)))
    results_eval2$point[is.na(results_eval2$point)] <- 1
    
res[[k]] =   ggplot(data = results_eval2, aes(x = year, y = point, color = Types)) +

  geom_line(aes(x= year, y= point, color = Types), linetype="dashed" , lwd=2) +
  geom_hline(yintercept=1) +
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  labs(color = "E-value types") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position="bottom",
        text = element_text(size=24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24)) +
  ggtitle(paste0("Effect on ", fire_type, ", ", capitalize(biome))) + 
  xlab("Years since fire") +
  ylab("E-values") +
  coord_cartesian(ylim = c(0.9, 5))

  k = k + 1
  }

}

res_combined <- grid.arrange(res[[1]], res[[2]], res[[3]], res[[4]], 
                             res[[5]], res[[6]],
                             nrow = 3)

ggsave(file.path(resDir, "results", paste0("evalue_combined_rev_linear.jpeg")), 
       res_combined, 
       width = 14 / 1.6*2,
       height = 8.5 / 1.6*3,
       units = "in")

