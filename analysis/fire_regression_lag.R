library(grid) 
library(pBrackets) 
library(gridExtra)
library(Hmisc)
library(ggplot2)

rm(list = ls())

outDir <- "../data/intermediate_res"
resDir = "../data/outputs"

# biome = "forests"
# outcome = "fire_all"

res <- list()
k = 1  
for (outcome in c("fire_all", "fire_90", "fire_95")) {
#  results.list <- list()
  for (biome in c("forests", "savannas")) {
data.raw = Reduce(rbind, lapply(1:9, function(ll) {
  XX = read.csv(file.path(outDir, "result_low",
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
  regform = paste0(outcome, " ~ factor(end.year) * factor(lag) + treat + treat:poly(lag, 2)")
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
#results.list[[biome]] <- data.frame(year = 1:length(rat), rate = rat, lower = lb.rat, upper = ub.rat, "land type" = rep(capitalize(biome), length(rat)))
results <-  data.frame(year = 1:length(rat), rate = rat, lower = lb.rat, upper = ub.rat, "land_type" = rep(capitalize(biome), length(rat)))

if (outcome == "fire_all") {fire_type <- "all fires"} else 
  if (outcome == "fire_90") {fire_type <- "class 2-5 fires"} else 
    if (outcome == "fire_95") {fire_type <- "class 3-5 fires"} 

res[[k]] =   ggplot(data = results, aes(x = year, y = rate, color = land_type)) +
  #geom_point(data = subset(DR_all_nosmooth,band=="DR100"),aes(x=a.vals.nosmooth,y=dose_matching.response.nonsmooth), lwd=0.8) +
  geom_ribbon(data = results, aes(ymin = lower, ymax = upper), fill = "grey70", alpha=0.3) +
  geom_line(aes(x= year, y= rate, color = land_type), lwd=1.2) +
  geom_line(aes(x= year, y= lower, color = land_type), linetype="dashed" , lwd=1.2) +
  geom_line(aes(x= year, y= upper, color = land_type), linetype="dashed" , lwd=1.2) +
  geom_hline(yintercept=1) +
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  #ylim(1.5*min(Sim2$lower) - 0.5*max(Sim2$upper), 1.5*max(Sim2$upper)- 0.5*min(Sim2$lower)) +
  #geom_text(x = Sim2$logOR[1], y = 1.5*max(Sim2$upper)- 0.5*min(Sim2$lower), label="a", size = 16, color = "black") + 
  #scale_color_manual(values=c("red", "blue")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none",
        text = element_text(size=18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  #guides( linetype = FALSE) +
  ggtitle(paste0("Effect on ", fire_type, ", ", capitalize(biome))) + 
  xlab("Years since fire") +
  ylab("Relative reduction in fire frequency")

k = k + 1
  }
  
}

res_combined <- grid.arrange(res[[1]], res[[2]], res[[3]],
                             res[[4]], res[[5]], res[[6]],
                             nrow = 3)

ggsave(file.path(resDir, "results", paste0("res_combined.jpeg")), 
       res_combined, 
       width = 11 / 1.6*2,
       height = 8.5 / 1.6*3,
       units = "in")
