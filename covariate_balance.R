library("CVXR")
library("glmnet")
library("sf")
library("tidyverse")
library("tigris")
setwd("~/zfs/gsb/intermediate-yens/wuxiao")
source("cbps_ATT.R")

treated.year = 2017

df = readRDS(paste0("analysis_treated", treated.year, "_all.RDS"))

FIRMS_ca_grouped = readRDS("FIRMS.RDS")
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
st_geometry(FIRMS_ca_grouped) <- NULL
#low_intensity <- 0.9

fire.df <- subset(FIRMS_ca_grouped, year == treated.year) 
                  #& avg_FRP <= quantile(FIRMS_ca_grouped$avg_FRP, low_intensity))
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

#histfire <- X[,821:1216]

#histfire_year <- sapply(1:(3*11), function(i) {
#  return(rowMeans(histfire[,1:12 + (i-1)*12]))
#})
#X_new <- cbind(X[,1:820], histfire_year)

system.time(res <- cbps_att(as.matrix(X),
                            W, 
                            #method = "CG",
                            control = list(trace=10, maxit = 5000)))
saveRDS(res, paste0("res_histfire", treated.year, ".RDS"))
plot((res$balance.condition[,1] - res$balance.condition[,2]), 
     main = "Balance Conditions",
     ylab = "Differences")
plot((res$balance.condition[,1] - res$balance.condition[,2])/res$balance.condition[,2], 
     main = "Balance Conditions",
     ylab = "Percentage Differences")

#W = df$treated
#X = as.matrix(df[,1:820])

# ATT balance constraint is:
# 1/n1 \sum_{Wi = 0} e(x)/(1-e(x)) Xi = 1/n1 \sum_{Wi=1} Xi,
# which gives loss function
# (1 - W)exp(theta * X) - W * theta * X
