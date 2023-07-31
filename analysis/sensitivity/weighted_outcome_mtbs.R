# Sensitivity analysis that uses fire severity rather than fire intensity as the fire classification scheme
# create weighted synthetic controls using covariate balance weights.

print(Sys.time())
rm(list = ls())
library("sf")
library("fst")
library("tidyverse")
library("parallel")
library("bootstrap")
library(grid) 
library(pBrackets) 
library(gridExtra)

outDir = "../data/processed_data/"

# By lagged
parameters <- expand.grid(c("conifer", "hardwood"), c(1:9))
for (year_area in 1:nrow(parameters)) {
  area <- as.character(parameters[year_area, 1])
  lagged <- as.numeric(parameters[year_area, 2])
  #year_area = 1
  rate <- lapply(c(2008:2020), function(treated.year) {
    
    # access both covariates data and weights set
    df <- readRDS(file.path(outDir, "mtbs_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
    res <- readRDS(list.files(file.path(outDir, "mtbs_res_low"),
                              pattern = paste0(treated.year, "_", area),
                              full.names = TRUE))
    
    # outcome analysis, weights by covariate balancing weights to create synthetic controls
    df_weight <- df
    df_weight$weight <- df_weight$treated *res$weights.1 + (1-df_weight$treated)*res$weights.0
    
    # input fires defined by mtbs
    fire_mtbs <- read.fst(file.path(outDir, "mtbs_fire.fst"),
                          from = 1,
                          to = NULL,
    )
    colnames(fire_mtbs)[3:ncol(fire_mtbs)] <- gsub("fire_", "", names(fire_mtbs[3:ncol(fire_mtbs)]))
    years = gsub("fire_", "", names(fire_mtbs[3:ncol(fire_mtbs)]))

    fire_mtbs$unit <- paste0(fire_mtbs$LATITUDE, fire_mtbs$LONGITUDE)
    
    control <- df_weight$unit[df_weight$treated == 0]
    treated <- df_weight$unit[df_weight$treated == 1]
    
    
    fire_mtbs <- merge(fire_mtbs, 
                       df_weight[,c("LATITUDE", "LONGITUDE", "weight")],
                       by = c("LATITUDE", "LONGITUDE"),
                       all.x = TRUE)
    fire_mtbs <- fire_mtbs[fire_mtbs$unit %in% c(control, treated), ]
    fire_mtbs$treated = 0
    fire_mtbs[fire_mtbs$unit %in% treated, ]$treated = 1
    
    #define three fire classes (all fires, moderate severity (90), high serverity (95))
    fire_mtbs_fire = fire_mtbs
    fire_mtbs_hifire90 = fire_mtbs
    fire_mtbs_hifire95 = fire_mtbs
    for(i in years){
      fire_mtbs_fire[fire_mtbs_fire[, i] >= 1, i] <- 1
      fire_mtbs_fire[, i] <- fire_mtbs_fire[, i] * fire_mtbs_fire$weight
      
      fire_mtbs_hifire90[fire_mtbs_hifire90[, i] < 2, i] <- 0
      fire_mtbs_hifire90[fire_mtbs_hifire90[, i] >= 2, i] <- 1
      fire_mtbs_hifire90[, i] <- fire_mtbs_hifire90[, i] * fire_mtbs_hifire90$weight
      
      fire_mtbs_hifire95[fire_mtbs_hifire95[, i] < 3, i] <- 0
      fire_mtbs_hifire95[fire_mtbs_hifire95[, i] >= 3, i] <- 1
      fire_mtbs_hifire95[, i] <- fire_mtbs_hifire95[, i] * fire_mtbs_hifire95$weight
    }
    
    mtbs.freq.year = fire_mtbs_fire %>% 
      group_by(treated) %>%
      summarise_at(.vars = c(years), .funs = sum, na.rm = TRUE) %>%
      pivot_longer(
        cols = -treated, # Assuming 'treated' is the id column, change if necessary
        names_to = "year", 
        values_to = "sum.fire",
        names_transform = list(year = as.integer) # Transform year from character to integer
      )
    
    mtbs_hifire95.freq.year = fire_mtbs_hifire95 %>% 
      group_by(treated) %>%
      summarise_at(.vars = c(years), .funs = sum, na.rm = TRUE) %>%
      pivot_longer(
        cols = -treated, # Assuming 'treated' is the id column, change if necessary
        names_to = "year", 
        values_to = "sum.hifire95",
        names_transform = list(year = as.integer) # Transform year from character to integer
      )
    
    mtbs_hifire90.freq.year = fire_mtbs_hifire90 %>% 
      group_by(treated) %>%
      summarise_at(.vars = c(years), .funs = sum, na.rm = TRUE) %>%
      pivot_longer(
        cols = -treated, # Assuming 'treated' is the id column, change if necessary
        names_to = "year", 
        values_to = "sum.hifire90",
        names_transform = list(year = as.integer) # Transform year from character to integer
      )
    
    mtbs.freq.year$sum.hifire95 <- mtbs_hifire95.freq.year$sum.hifire95
    mtbs.freq.year$sum.hifire90 <- mtbs_hifire90.freq.year$sum.hifire90
    mtbs.freq.year$fire.frac = mtbs.freq.year$sum.fire/sum(df_weight$treated)
    mtbs.freq.year$hifire95.frac = mtbs.freq.year$sum.hifire95/sum(df_weight$treated)
    mtbs.freq.year$hifire90.frac = mtbs.freq.year$sum.hifire90/sum(df_weight$treated)
    
    saveRDS(mtbs.freq.year, file = file.path(outDir, "mtbs_result_low", paste0("df.freq.year", treated.year, "_", area, ".RDS")))
    
    ratio.fire.1 <- subset(mtbs.freq.year, treated == 1 & year == treated.year + lagged)$fire.frac
    ratio.fire.0 <- subset(mtbs.freq.year, treated == 0 & year == treated.year + lagged)$fire.frac
    ratio.hifire95.1 <- subset(mtbs.freq.year, treated == 1 & year == treated.year + lagged)$hifire95.frac
    ratio.hifire95.0 <- subset(mtbs.freq.year, treated == 0 & year == treated.year + lagged)$hifire95.frac
    ratio.hifire90.1 <- subset(mtbs.freq.year, treated == 1 & year == treated.year + lagged)$hifire90.frac
    ratio.hifire90.0 <- subset(mtbs.freq.year, treated == 0 & year == treated.year + lagged)$hifire90.frac
    
    return(c(ratio.fire.1, ratio.fire.0,
             ratio.hifire95.1, ratio.hifire95.0,
             ratio.hifire90.1,ratio.hifire90.0))
  })
  rate.df <- do.call(rbind, rate)
  colnames(rate.df) <- c("fire.1", "fire.0", "hifire95.1", "hifire95.0", "hifire90.1", "hifire90.0")
  saveRDS(rate.df, file = file.path(outDir, "mtbs_result_low", paste0(area , "_t", lagged, ".RDS")))
}

# save as CSV file for better disbute
parameters = expand.grid(c("conifer", "hardwood"), as.character(seq(1,9,1)))
for (index in 1:nrow(parameters)) {
  rate <- data.frame(readRDS(file.path(outDir, "mtbs_result_low", 
                                       paste0(parameters[index,1], "_t", parameters[index,2], ".RDS"))))
  rownames(rate) <- 2008:(2008 + nrow(rate) -1)
  
  rate$pixels_burn <- sapply(2008:(2008 + nrow(rate) -1), function(treated.year) {
    df <- readRDS(file.path(outDir, "mtbs_analysis_low", 
                            paste0("analysis_treated", treated.year, "_", parameters[index,1], ".RDS")))
    return(sum(df$treated))
  })
  
  write.csv(rate, file = file.path(outDir, "mtbs_result_low",
                                   paste0(parameters[index,1], "_lag", parameters[index,2], ".csv")))
}