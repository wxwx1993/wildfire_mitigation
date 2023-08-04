## process data for the covariate balance synthetic control analysis
## combine active fire, historic trajectories on fire behaviors, topography, meteorological,
## disturbance, and vegetation data into single df
## the df is per exposure year (2008-2020) per land type ("conifer", "hardwood")
print(Sys.time())
rm(list = ls())
library("sf")
library("tidyverse")
library("mltools")
library("data.table")
library("fst")

outDir = "../data/processed_data/"

#year up to 2020
parameters <- expand.grid(c(2005:2020), c("conifer", "hardwood"))
# year_area = 1                          
for (year_area in 1:nrow(parameters)) {
  treated.year <- as.numeric(parameters[year_area, 1])
  area <- as.character(parameters[year_area, 2])
  
  # choose only pre-exposure data (i.e., for exposure year XXXX, only covariates before that year will be chosen)
  col.idx2 <- c(1, 2, as.numeric(outer(c(3:(2 + 12 * (treated.year - 2000))), seq(0, 4, 1) * (21*12), FUN = "+")))
  col.idx3 <- c(1, 2, as.numeric(outer(c(3:(2 + (treated.year - 2000))), seq(0, 2, 1) * (22), FUN = "+")))
  col.idx4 <- c(1, 2, seq(3, (2 + (treated.year - 2000) * 5), 1))
  col.idx5 <- c(1, 2, seq(3, (2 + (treated.year - 2000)), 1))
  
  # Import vegetation class and topography of each locations
  fveg_elev_grid_ca_poly <- readRDS(file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))

  st_geometry(fveg_elev_grid_ca_poly) <- NULL
  if (area == "conifer") {
    fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
      filter(fveg %in% c(31, 32))
  } else if (area == "hardwood") {
    fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>%
      filter(fveg %in% c(51, 52))
  }
  fveg_elev_grid_ca_poly$fveg <- as.factor(fveg_elev_grid_ca_poly$fveg)
  fveg_elev_grid_ca_poly <- cbind(fveg_elev_grid_ca_poly[,c("LONGITUDE", "LATITUDE", "elev")], one_hot(as.data.table(fveg_elev_grid_ca_poly$fveg)))
  fveg_elev_grid_ca_poly$unit <- paste0(fveg_elev_grid_ca_poly$LATITUDE, fveg_elev_grid_ca_poly$LONGITUDE)
  fveg_elev_grid_ca_poly$LATITUDE <- NULL
  fveg_elev_grid_ca_poly$LONGITUDE <- NULL
  

  # Import pre-exposure monthly meteorological covariates
  df <- read.fst(file.path(outDir, "gridClimate_mon2.fst"),
                 from = 1,
                 to = NULL,
  )
  df <- df[col.idx2]

  # Import pre-exposure annual disturbance covariates
  disturbance <- read.fst(file.path(outDir, "disturbance.fst"),
                          from = 1,
                          to = NULL,
  )
  disturbance <- disturbance[col.idx4]
  
  df <- merge(df, disturbance, by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)
  
  # Import pre-exposure annual tree cover covariates
  tree_cover <- read.fst(file.path(outDir, "tree_cover.fst"),
                          from = 1,
                          to = NULL,
  )
  tree_cover <- tree_cover[col.idx5]
  
  df <- merge(df, tree_cover, by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)
  
  # Import pre-exposure annual fire behaviors (frequency and intensity) covariates
  fire_brightness_frp <- read.fst(file.path(outDir, "fire_brightness_frp.fst"),
                                  from = 1,
                                  to = NULL,
  )
  fire_brightness_frp <- fire_brightness_frp[col.idx3]

  df <- merge(df, fire_brightness_frp, by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)
  
  unit <- paste0(df$LATITUDE, df$LONGITUDE)
  df$unit <- unit

  # Create exposed and unexposed units based on fire intensity (FRP) at focal years
  FIRMS_ca_grouped = readRDS(file.path(outDir, "FIRMS.RDS"))
  FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
  st_geometry(FIRMS_ca_grouped) <- NULL
  fire.df <- subset(FIRMS_ca_grouped, year == treated.year)
  fire.index = unique(fire.df[c("unit")])

  hist.fire.df <- subset(FIRMS_ca_grouped, year <= treated.year - 1)
  hist.fire.df$has.fire = 1
  hist.fire.df <- hist.fire.df %>%
    group_by(unit) %>%
    summarise(num.fire = sum(has.fire))
  
  # Keep only covariates and treatment
  df <- merge(fveg_elev_grid_ca_poly, df, by = "unit", all.x = TRUE)

  df <- merge(df, hist.fire.df, by = "unit", all.x = TRUE)
  df$num.fire[is.na(df$num.fire)] <- 0
  # treated = all units which had fire in treated.year
  df$treated = 0
  df[df$unit %in% fire.index$unit, "treated"] = 1
  
  fire.df_year <- fire.df %>% 
    group_by(LATITUDE, LONGITUDE, year) %>% 
    summarise(max_FRP = max(max_FRP))
  
  # classify the fire types by max FRP for each focal years
  # based on systems proposed in https://www.sciencedirect.com/science/article/abs/pii/S003442570800062X
  fire.df_year$class <- 0 
  fire.df_year[fire.df_year$max_FRP == 0,]$class <- 0
  fire.df_year[0 < fire.df_year$max_FRP & fire.df_year$max_FRP < 100,]$class <- 1
  fire.df_year[100 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 500,]$class <- 2
  fire.df_year[500 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 1000,]$class <- 3
  fire.df_year[1000 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 1500,]$class <- 4
  fire.df_year[fire.df_year$max_FRP >= 1500,]$class <- 5
  
  saveRDS(fire.df_year, file = paste0(outDir, "/rev_analysis_low/fire_class/fire.df", treated.year, "_", area, ".RDS"))
  
  # df for the covariate balance analysis, only keep low intensity fire class 1 as the exposure
  fire.df$has.hifire <- 0
  fire.df[fire.df$max_FRP >= 100,]$has.hifire <- 1
  df <- subset(df, !(unit %in% subset(fire.df, has.hifire == 1)$unit))
  
  saveRDS(df, file = file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
}
