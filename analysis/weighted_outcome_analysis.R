# create weighted synthetic controls using covariate balance weights.

print(Sys.time())
rm(list = ls())
library("sf")
library("tidyverse")
library("parallel")
library("bootstrap")
library(grid) 
library(pBrackets) 
library(gridExtra)

outDir = "../data/processed_data/"

for (start_year in c(2006, 2008, 2010)) {
# By lagged
parameters <- expand.grid(c("conifer", "hardwood"), c(1:9))
for (year_area in 1:nrow(parameters)) {
  area <- as.character(parameters[year_area, 1])
  lagged <- as.numeric(parameters[year_area, 2])
  #year_area = 1
  rate <- lapply(c(start_year:2020), function(treated.year) {
    
    # access both covariates data and weights set
    df <- readRDS(file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
    res <- readRDS(list.files(file.path(outDir, "rev_res_low"),
                              pattern = paste0(treated.year, "_", area),
                              full.names = TRUE))
    
    # outcome analysis, weights by covariate balancing weights to create synthetic controls
    df_weight <- df
    df_weight$weight <- df_weight$treated *res$weights.1 + (1-df_weight$treated)*res$weights.0
    
    FIRMS_ca_grouped <- readRDS(file.path(outDir, "FIRMS.RDS"))
    FIRMS_ca_grouped$unit <- paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
    
    control <- df_weight$unit[df_weight$treated == 0]
    treated <- df_weight$unit[df_weight$treated == 1]
    
    FIRMS_ca_grouped <- merge(FIRMS_ca_grouped, 
                              df_weight[,c("LATITUDE", "LONGITUDE", "weight")],
                              by = c("LATITUDE", "LONGITUDE"),
                              all.x = TRUE)
    FIRMS_ca_grouped <- FIRMS_ca_grouped[FIRMS_ca_grouped$unit %in% c(control, treated), ]
    FIRMS_ca_grouped$treated = 0
    FIRMS_ca_grouped[FIRMS_ca_grouped$unit %in% treated, ]$treated = 1
    
    # Create a date range panel
    start.year = min(FIRMS_ca_grouped$year)
    end.year = max(FIRMS_ca_grouped$year)
    #start.month = min(subset(FIRMS_ca_grouped, year == start.year)$month)
    #end.month = max(subset(FIRMS_ca_grouped, year == end.year)$month)
    
    start.date = as.Date(paste0(start.year, "-01-01"))
    end.date = as.Date(paste0(end.year, "-12-01"))
    date.range = seq(start.date, end.date, "years")
    df.date.panel = data.frame(year = as.numeric(format(date.range, "%Y")), date.obj = date.range)
    
    # Create unit-date full panel (unit * year)
    ix.date = rep(1:nrow(df.date.panel), length(unique(FIRMS_ca_grouped$unit)))
    ix.unit = gl(length(unique(FIRMS_ca_grouped$unit)), nrow(df.date.panel))
    df.panel = cbind(df.date.panel[ix.date, ], unit  = unique(FIRMS_ca_grouped$unit)[ix.unit])
    
    # define outcomes as high intensity fire with high max FRP
    FIRMS_ca_grouped$has.fire <- 1
    FIRMS_ca_grouped$has.hifire95 <- 0
    FIRMS_ca_grouped$has.hifire90 <- 0
    FIRMS_ca_grouped[FIRMS_ca_grouped$max_FRP >= 1000, ]$has.hifire95 <- 1
    FIRMS_ca_grouped[FIRMS_ca_grouped$max_FRP >= 500, ]$has.hifire90 <- 1
    
    df_final <- merge(df.panel,
                      FIRMS_ca_grouped[, c("year", "unit", "has.fire", "avg_BRIGHTNESS", 
                                           "max_FRP", "treated", "has.hifire95", "has.hifire90")],
                      by = c("year", "unit"),
                      all.x = TRUE)
    df_final$has.fire[is.na(df_final$has.fire)]  <- 0
    df_final$treated[df_final$unit %in% control]  <- 0
    df_final$treated[df_final$unit %in% treated]  <- 1
    df_final <- merge(df_final, df_weight[, c("unit", "weight")], by = c("unit"), all.x = TRUE)

    df_final$weight.fire <- df_final$has.fire * df_final$weight
    df_final$weight.hifire95 <- df_final$has.hifire95 * df_final$weight
    df_final$weight.hifire90 <- df_final$has.hifire90 * df_final$weight
    
    df.freq.year <- df_final %>%
      group_by(year, treated) %>%
      summarise(sum.fire = sum(weight.fire, na.rm = TRUE),
                sum.hifire95 = sum(weight.hifire95, na.rm = TRUE),
                sum.hifire90 = sum(weight.hifire90, na.rm = TRUE),
                fire.frac = sum.fire/sum(df_weight$treated),
                hifire95.frac = sum.hifire95/sum(df_weight$treated),
                hifire90.frac = sum.hifire90/sum(df_weight$treated))
    
    #saveRDS(df.freq.year, file = file.path(outDir, "rev_result_low", start_year, paste0("df.freq.year", treated.year, "_", area, ".RDS")))
  
      ratio.fire.1 <- subset(df.freq.year, treated == 1 & year == treated.year + lagged)$fire.frac
      ratio.fire.0 <- subset(df.freq.year, treated == 0 & year == treated.year + lagged)$fire.frac
      ratio.hifire95.1 <- subset(df.freq.year, treated == 1 & year == treated.year + lagged)$hifire95.frac
      ratio.hifire95.0 <- subset(df.freq.year, treated == 0 & year == treated.year + lagged)$hifire95.frac
      ratio.hifire90.1 <- subset(df.freq.year, treated == 1 & year == treated.year + lagged)$hifire90.frac
      ratio.hifire90.0 <- subset(df.freq.year, treated == 0 & year == treated.year + lagged)$hifire90.frac
  
    return(c(ratio.fire.1, ratio.fire.0,
             ratio.hifire95.1, ratio.hifire95.0,
             ratio.hifire90.1,ratio.hifire90.0))
  })
  rate.df <- do.call(rbind, rate)
  colnames(rate.df) <- c("fire.1", "fire.0", "hifire95.1", "hifire95.0", "hifire90.1", "hifire90.0")
  saveRDS(rate.df, file = file.path(outDir, "rev_result_low",start_year, paste0(area , "_t", lagged, ".RDS")))
}

# save as CSV file for better disbute
parameters = expand.grid(c("conifer", "hardwood"), as.character(seq(1,9,1)))
for (index in 1:nrow(parameters)) {
  rate <- data.frame(readRDS(file.path(outDir, "rev_result_low", start_year, 
                                       paste0(parameters[index,1], "_t", parameters[index,2], ".RDS"))))
  rownames(rate) <- start_year:(start_year + nrow(rate) -1)
  
  rate$pixels_burn <- sapply(start_year:(start_year + nrow(rate) -1), function(treated.year) {
    df <- readRDS(file.path(outDir, "rev_analysis_low", 
                            paste0("analysis_treated", treated.year, "_", parameters[index,1], ".RDS")))
    return(sum(df$treated))
  })

  write.csv(rate, file = file.path(outDir, "rev_result_low", start_year,
                                   paste0(parameters[index,1], "_lag", parameters[index,2], ".csv")))
  }

}