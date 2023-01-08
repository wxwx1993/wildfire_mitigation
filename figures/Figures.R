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

outDir <- "wildfire_mitigation/processed_data"
resDir  <- "wildfire_mitigation/outputs"

#year up to 
parameters <- expand.grid(c(2008:2020), c("forests", "savannas"))
covariates <- c("minat_", "maxat_", "prcp_", "swe_", "wvp_", "fire_", "avg_BRIGHTNESS_", "max_FRP_")
# year_area = 1                          
for (year_area in 1:nrow(parameters)) {
  #year_area <- 1
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])
  df = readRDS(file.path(outDir, "analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
  
  weights = readRDS(list.files(file.path(outDir, "res_low"),
                               pattern = paste0(treated.year, "_", area),
                               full.names = TRUE))
  df_weight <- df
  df_weight$weight <- df_weight$treated *weights$weights.1 + (1-df_weight$treated)*weights$weights.0
  
  post_bal <- NULL
  pre_bal <- NULL
  ncov = length(covariates)
  for (i in 1:ncov) {
    covariate = covariates[i]
  post_bal[i] <- (mean(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))  - 
    sum(rowMeans(subset(df_weight, treated == 0)[,grep(covariate, colnames(df))])* 
        subset(df_weight, treated == 0)$weight)/sum(subset(df_weight, treated == 0)$weight))/
    sd(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))
  
  pre_bal[i] <- (mean(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))  - 
    mean(rowMeans(subset(df_weight, treated == 0)[,grep(covariate, colnames(df))])))/
    sd(rowMeans(subset(df_weight, treated == 1)[,grep(covariate, colnames(df))]))
  }
  order <- order((pre_bal))
  balance <- data.frame(matrix(NA, nrow = ncov * 2, ncol = 0))
  balance$Covariates <- rep(1:ncov, 2)
  balance$SMD <- (c(sort((pre_bal)), post_bal[order]))
  balance$covariates_name <- rep(covariates[order], 2)
  balance$Scenarios <- c(rep("Unweighted: Orginal Data", ncov), rep("Weighted: Synthetic Control", ncov))
  balance$Scenarios <- factor(balance$Scenarios, levels = c("Unweighted: Orginal Data", "Weighted: Synthetic Control"))
  
  balance_p <- ggplot(balance, aes(x=SMD, y=Covariates, colour=Scenarios)) + 
    scale_y_discrete(limit = 1:ncov,
                     labels = c("Min air temperature", "Max air temperature", "Precipitation", "Snow water equivalent",
                                "Water vapor pressure", "Fire frequency", "Avg fire brightness", "Max fire radiative power")[order]) + 
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
  
  
  df.freq.year <- readRDS(file.path(outDir, "result_low", paste0("df.freq.year", treated.year, "_", area, ".RDS")))
  df.freq.year$Assignment <- "Exposed"
  df.freq.year$Assignment[df.freq.year$treated == 0] <- "Unexposed"
  
  #pdf(file.path(resDir, "historical", paste0("Fire_Frequency_" , treated.year, "across",  area, ".pdf")), width = 11 / 1.6, height = 8.5 / 1.6)
#  historical <- ggplot(df.freq.year, aes(x = year, y = fire.frac, colour = Assignment, type = "line")) + 
#    geom_point() +
#    geom_path() +
#    scale_color_manual(breaks = c("Exposed", "Unexposed"),
#                         values=c("red", "blue")) + 
#    geom_rect(aes(xmin = treated.year-0.35, 
#                  xmax = treated.year + 0.35, 
#                  ymin = +0.0002, 
#                  ymax = max(subset(df.freq.year, year != treated.year)$fire.frac + 0.002),
#                  fill = "red")) +
#    guides(fill="none") +
#    theme_bw() +
#    theme(plot.margin = unit(c(0.1, 1, 0.1, 0.1), "lines"),
#          plot.title = element_text(hjust = 0.5, size = 12),
#          legend.title = element_text(size = 12),
#          legend.position = c(0.2, 0.8),
#          axis.text.y = element_text(angle = 45, hjust = 1)) +
#    ggtitle(paste0("Annual Fire Frequency, Exposed vs Unexposed, ", capitalize(area))) + 
#    xlab("Year") +
#    ylab("Historical Fire Frequency by Year") +
#    ylim(0.0002, max(subset(df.freq.year, year != treated.year)$fire.frac + 0.002))
#  #dev.off()
#  ggsave(file.path(resDir, "historical", paste0("Fire_Frequency_" , treated.year, "across",  area, ".jpeg")), 
#         historical, 
#         width = 11 / 1.6,
#         height = 8.5 / 1.6,
#         units = "in")
  
}

## maps
parameters <- expand.grid(c(2008:2020), c("forests", "savannas"))
for (year_area in 1:nrow(parameters)) {
  #year_area <- 1
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])

  df = readRDS(file.path(outDir, "analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
  weights = readRDS(list.files(file.path(outDir, "res_low"),
                               pattern = paste0(treated.year, "_", area),
                               full.names = TRUE))
  df_weight <- df
  df_weight$weight <- df_weight$treated *weights$weights.1 + (1-df_weight$treated)*weights$weights.0
  
  df_weight2 <- st_as_sf(df_weight[,c("LATITUDE", "LONGITUDE","weight")],
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = 4326,
                         remove = FALSE)
  df_weight2$logwt <- log(df_weight2$weight)
  #df_weight2$loglogwt <- log(df_weight2$logwt - min(df_weight2$logwt))
  df_weight2$logwt[df_weight2$logwt < quantile(df_weight2$logwt,0.25, na.rm = T)] <- quantile(df_weight2$logwt,0.25, na.rm = T)
  
  CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
  CA_bound = st_transform(CA_bound, crs = 4326)
  df_weight2$transparency <- (df_weight2$logwt- min(df_weight2$logwt))/(max(df_weight2$logwt)-min(df_weight2$logwt))
  
  jpeg(file.path(resDir, "maps", paste0("logweight_histfire", treated.year, "_", area, "..jpeg")),
       width = 8.5*300, height = 11*300, quality = 100, res = 300)
  par(mar=c(0.1,1,2,1))
  plot(CA_bound$geometry, main = paste0(treated.year, ", ", capitalize(area)), cex.main= 3)
  control = subset(df_weight2, weight != 1)[c("logwt","geometry","transparency")]
  #plot(df_weight2[c("logwt","geometry")], pch=16, cex = 0.5, col = alpha("green", 0.01), add = TRUE)
  plot(subset(df_weight2, weight != 1)[c("logwt","geometry")], pch=16, cex = 0.3, col = alpha("blue", control$transparency), add = TRUE)
  plot(subset(df_weight2, weight == 1)[c("logwt","geometry")], pch=16, cex = 0.6, col = "red", add = TRUE)
  legend("right", legend = c("Treated", "Control"), fill = c("red", "blue"), , cex=2)
  dev.off()
}


# Table 1
FIRMS_ca_grouped = readRDS(file.path(outDir, "FIRMS.RDS"))
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
st_geometry(FIRMS_ca_grouped) <- NULL
fire.df <- FIRMS_ca_grouped
fire.index = unique(fire.df[c("unit")])

fire.df_year <- fire.df %>% 
  group_by(LATITUDE, LONGITUDE, year) %>% 
  summarize(max_FRP = max(max_FRP))

# classify the fire types by max FRP for each years
# based on systems proposed in https://www.sciencedirect.com/science/article/abs/pii/S003442570800062X
fire.df_year$class <- 0 
fire.df_year[fire.df_year$max_FRP == 0,]$class <- 0
fire.df_year[0 < fire.df_year$max_FRP & fire.df_year$max_FRP < 100,]$class <- 1
fire.df_year[100 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 500,]$class <- 2
fire.df_year[500 <= fire.df_year$max_FRP,]$class <- 3
fire.df_year$unit <- paste0(fire.df_year$LATITUDE, fire.df_year$LONGITUDE)

col.idx4 <- c(1, 2, as.numeric(outer(c(3:(2 + (treated.year - 2000))), seq(3, 3, 1) * (20), FUN = "+")))
landcover <- readRDS(file.path(outDir, "LandCover.RDS"))
landcover <- st_as_sf(landcover,
                      coords = c("LONGITUDE", "LATITUDE"),
                      crs = 4326,
                      remove = FALSE)
st_geometry(landcover) <- NULL
landcover <- landcover[col.idx4]
landcover$unit <- paste0(landcover$LATITUDE, landcover$LONGITUDE)

landcover$LATITUDE <- NULL
landcover$LONGITUDE <- NULL

results <- merge(fire.df_year, landcover, by = "unit", all.x = TRUE)

df = sapply(2001:2020, function(treated.year) {
  c(table(subset(results, year == treated.year & results[, 6+ treated.year-2000] <= 5)$class),
    table(subset(results, year == treated.year & results[, 6+ treated.year-2000] <= 9 & results[, 6+ treated.year-2000] >= 8)$class),
    table(subset(results, year == treated.year & results[, 6+ treated.year-2000] == 10)$class))
})

write.csv(do.call(rbind, df), "fire_freq.csv", row.names = F)



