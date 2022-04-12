###############################################################################
# Project: Plotting results of subgroup analysis                              #
# Author: Yaguang Wei                                                         #
###############################################################################

################## 0. Setup ##################
rm(list=ls())
gc()

require(metafor)
require(magrittr)
library(cowplot)
require(gtable)
require(gridExtra)
require(grid)
require(plotly)
library(berryFunctions)

dir_data <- '~/data/'
dir_save <- '~/save/'



################## 1. Format separate dataframes for PM, O3, NO2 ##################
uniam_main_results_lag06 <- readRDS(paste0(dir_data,'dat_main.rds'))
uniam_results_sub <- readRDS(paste0(dir_data,'dat_sub.rds'))

options(na.action = "na.pass")  # to not ignore NAs during plotting

space <- '        '

### subset pm25
uniam_results_sub_pm25 <- uniam_results_sub[uniam_results_sub$exp=='pm25',]

# test for effect modifications
uniam_results_sub_pm25$em_test <- NA
uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='PopDengreater25','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='PopDengreater25','coef']-uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='PopDenless25','coef'])/sqrt(uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='PopDengreater25','se']^2+uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='PopDenless25','se']^2))))
uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='BMIless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='BMIless75','coef']-uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='BMIgreater75','coef'])/sqrt(uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='BMIless75','se']^2+uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='BMIgreater75','se']^2))))
uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='smokerateless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='smokerateless75','coef']-uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='smokerategreater75','coef'])/sqrt(uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='smokerateless75','se']^2+uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='smokerategreater75','se']^2))))
uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='nearest_hospless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='nearest_hospless75','coef']-uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='nearest_hospgreater75','coef'])/sqrt(uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='nearest_hospless75','se']^2+uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='nearest_hospgreater75','se']^2))))
uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='ADI_NATRANK_avgless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='ADI_NATRANK_avgless75','coef']-uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='ADI_NATRANK_avggreater75','coef'])/sqrt(uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='ADI_NATRANK_avgless75','se']^2+uniam_results_sub_pm25[uniam_results_sub_pm25$cat=='ADI_NATRANK_avggreater75','se']^2))))

uniam_results_sub_pm25$em_p <- ifelse(is.na(uniam_results_sub_pm25$em_test),'Reference',
                                      ifelse(uniam_results_sub_pm25$em_test<0.01,'<0.01',format(round(uniam_results_sub_pm25$em_test,2),nsmall=2)))

# format
uniam_results_sub_pm25$cat <- c(paste0(space,'Low'),
                                paste0(space,'Medium to High'),
                                paste0(space,'High'),
                                paste0(space,'Medium to low'),
                                paste0(space,'High'),
                                paste0(space,'Medium to low'),
                                paste0(space,'High'),
                                paste0(space,'Medium to low'),
                                paste0(space,'High'),
                                paste0(space,'Medium to low'))

uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 1, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 4, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 4, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 8, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 8, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 12, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 12, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 16, new = NA)
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 16, new = NA)
uniam_results_sub_pm25 <- rbind(uniam_results_sub_pm25,uniam_main_results_lag06[uniam_main_results_lag06$exp=='pm25',])
uniam_results_sub_pm25 <- insertRows(uniam_results_sub_pm25, 20, new = NA)


### subset o3
uniam_results_sub_o3 <- uniam_results_sub[uniam_results_sub$exp=='o3summer',]

# test for effect modifications
uniam_results_sub_o3$em_test <- NA
uniam_results_sub_o3[uniam_results_sub_o3$cat=='PopDengreater25','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_o3[uniam_results_sub_o3$cat=='PopDengreater25','coef']-uniam_results_sub_o3[uniam_results_sub_o3$cat=='PopDenless25','coef'])/sqrt(uniam_results_sub_o3[uniam_results_sub_o3$cat=='PopDengreater25','se']^2+uniam_results_sub_o3[uniam_results_sub_o3$cat=='PopDenless25','se']^2))))
uniam_results_sub_o3[uniam_results_sub_o3$cat=='BMIless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_o3[uniam_results_sub_o3$cat=='BMIless75','coef']-uniam_results_sub_o3[uniam_results_sub_o3$cat=='BMIgreater75','coef'])/sqrt(uniam_results_sub_o3[uniam_results_sub_o3$cat=='BMIless75','se']^2+uniam_results_sub_o3[uniam_results_sub_o3$cat=='BMIgreater75','se']^2))))
uniam_results_sub_o3[uniam_results_sub_o3$cat=='smokerateless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_o3[uniam_results_sub_o3$cat=='smokerateless75','coef']-uniam_results_sub_o3[uniam_results_sub_o3$cat=='smokerategreater75','coef'])/sqrt(uniam_results_sub_o3[uniam_results_sub_o3$cat=='smokerateless75','se']^2+uniam_results_sub_o3[uniam_results_sub_o3$cat=='smokerategreater75','se']^2))))
uniam_results_sub_o3[uniam_results_sub_o3$cat=='nearest_hospless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_o3[uniam_results_sub_o3$cat=='nearest_hospless75','coef']-uniam_results_sub_o3[uniam_results_sub_o3$cat=='nearest_hospgreater75','coef'])/sqrt(uniam_results_sub_o3[uniam_results_sub_o3$cat=='nearest_hospless75','se']^2+uniam_results_sub_o3[uniam_results_sub_o3$cat=='nearest_hospgreater75','se']^2))))
uniam_results_sub_o3[uniam_results_sub_o3$cat=='ADI_NATRANK_avgless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_o3[uniam_results_sub_o3$cat=='ADI_NATRANK_avgless75','coef']-uniam_results_sub_o3[uniam_results_sub_o3$cat=='ADI_NATRANK_avggreater75','coef'])/sqrt(uniam_results_sub_o3[uniam_results_sub_o3$cat=='ADI_NATRANK_avgless75','se']^2+uniam_results_sub_o3[uniam_results_sub_o3$cat=='ADI_NATRANK_avggreater75','se']^2))))

uniam_results_sub_o3$em_p <- ifelse(is.na(uniam_results_sub_o3$em_test),'Reference',
                                    ifelse(uniam_results_sub_o3$em_test<0.01,'<0.01',format(round(uniam_results_sub_o3$em_test,2),nsmall=2)))

# format
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 1, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 4, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 4, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 8, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 8, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 12, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 12, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 16, new = NA)
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 16, new = NA)
uniam_results_sub_o3 <- rbind(uniam_results_sub_o3,uniam_main_results_lag06[uniam_main_results_lag06$exp=='o3summer',])
uniam_results_sub_o3 <- insertRows(uniam_results_sub_o3, 20, new = NA)


### subset no2
uniam_results_sub_no2 <- uniam_results_sub[uniam_results_sub$exp=='no2',]

# test for effect modifications
uniam_results_sub_no2$em_test <- NA
uniam_results_sub_no2[uniam_results_sub_no2$cat=='PopDengreater25','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_no2[uniam_results_sub_no2$cat=='PopDengreater25','coef']-uniam_results_sub_no2[uniam_results_sub_no2$cat=='PopDenless25','coef'])/sqrt(uniam_results_sub_no2[uniam_results_sub_no2$cat=='PopDengreater25','se']^2+uniam_results_sub_no2[uniam_results_sub_no2$cat=='PopDenless25','se']^2))))
uniam_results_sub_no2[uniam_results_sub_no2$cat=='BMIless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_no2[uniam_results_sub_no2$cat=='BMIless75','coef']-uniam_results_sub_no2[uniam_results_sub_no2$cat=='BMIgreater75','coef'])/sqrt(uniam_results_sub_no2[uniam_results_sub_no2$cat=='BMIless75','se']^2+uniam_results_sub_no2[uniam_results_sub_no2$cat=='BMIgreater75','se']^2))))
uniam_results_sub_no2[uniam_results_sub_no2$cat=='smokerateless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_no2[uniam_results_sub_no2$cat=='smokerateless75','coef']-uniam_results_sub_no2[uniam_results_sub_no2$cat=='smokerategreater75','coef'])/sqrt(uniam_results_sub_no2[uniam_results_sub_no2$cat=='smokerateless75','se']^2+uniam_results_sub_no2[uniam_results_sub_no2$cat=='smokerategreater75','se']^2))))
uniam_results_sub_no2[uniam_results_sub_no2$cat=='nearest_hospless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_no2[uniam_results_sub_no2$cat=='nearest_hospless75','coef']-uniam_results_sub_no2[uniam_results_sub_no2$cat=='nearest_hospgreater75','coef'])/sqrt(uniam_results_sub_no2[uniam_results_sub_no2$cat=='nearest_hospless75','se']^2+uniam_results_sub_no2[uniam_results_sub_no2$cat=='nearest_hospgreater75','se']^2))))
uniam_results_sub_no2[uniam_results_sub_no2$cat=='ADI_NATRANK_avgless75','em_test'] <- 2*(1-pnorm(abs((uniam_results_sub_no2[uniam_results_sub_no2$cat=='ADI_NATRANK_avgless75','coef']-uniam_results_sub_no2[uniam_results_sub_no2$cat=='ADI_NATRANK_avggreater75','coef'])/sqrt(uniam_results_sub_no2[uniam_results_sub_no2$cat=='ADI_NATRANK_avgless75','se']^2+uniam_results_sub_no2[uniam_results_sub_no2$cat=='ADI_NATRANK_avggreater75','se']^2))))

uniam_results_sub_no2$em_p <- ifelse(is.na(uniam_results_sub_no2$em_test),'Reference',
                                     ifelse(uniam_results_sub_no2$em_test<0.01,'<0.01',format(round(uniam_results_sub_no2$em_test,2),nsmall=2)))

# format
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 1, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 4, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 4, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 8, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 8, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 12, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 12, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 16, new = NA)
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 16, new = NA)
uniam_results_sub_no2 <- rbind(uniam_results_sub_no2,uniam_main_results_lag06[uniam_main_results_lag06$exp=='no2',])
uniam_results_sub_no2 <- insertRows(uniam_results_sub_no2, 20, new = NA)



################## 2. Plot ##################
pdf(paste0(dir_save,'Fig.pdf'), height = 7.5, width = 17)

layout(matrix(c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3),byrow = TRUE,nrow = 1))

op = par(cex=0.8, font=1)
forest(# Plot data
  x = uniam_results_sub_pm25$rr,
  ci.lb = uniam_results_sub_pm25$rr_ci_lower,
  ci.ub = uniam_results_sub_pm25$rr_ci_upper,
  annotate = F,
  # Add lag names
  slab = uniam_results_sub_pm25$cat,
  # x-label
  xlab = "",
  xlim = c(-0.8,0.78),
  alim = c(0,0.61),
  at = seq(0,0.6,0.2),
  ilab = cbind(uniam_results_sub_pm25$ci,uniam_results_sub_pm25$em_p),
  ilab.xpos= c(-0.3,0.78),
  ilab.pos = c(4,2),
  psize = c(rep(1.2,20),3.2), 
  pch = c(rep(16,20),18),
  cex.lab = 0.5)
par(font = 2)
text(-0.8, dim(uniam_results_sub_pm25)[1]+2, pos = 4, cex = 0.95, "Community-level")
text(-0.8, dim(uniam_results_sub_pm25)[1]+1.5, pos = 4, cex = 0.95, "characteristic")
text(-0.14, dim(uniam_results_sub_pm25)[1]+2, pos = 4, cex = 0.95, expression(paste(bold("Percent increase in risk of asthma hospitalization"))))
text(-0.14, dim(uniam_results_sub_pm25)[1]+1.5, pos = 4, cex = 0.95, expression(paste(bold(" for each 1 "),bold("\u03bc"),bold("g/m")^bold("3"),bold(" increase in PM")[bold(2.5)],bold(' at lag 0-6 day'))))
text(0.65, dim(uniam_results_sub_pm25)[1]+1.5, pos = 4, cex = 0.95, "p-value")

### add text for the subgroups
par(font=2)
text(-0.8, c(21,17,13,9,5,1), pos=4, cex = 0.95, c("Population density","Average BMI","Percent of ever smokers",
                                                   "Distance to the nearest hospital",
                                                   "Neighborhood disadvantage level","Main analysis"))
par(mar=(c(5+0.1,0,4+0.1,2+0.1)), font = 1)
forest(# Plot data
  x = uniam_results_sub_o3$rr,
  ci.lb = uniam_results_sub_o3$rr_ci_lower,
  ci.ub = uniam_results_sub_o3$rr_ci_upper,
  annotate = F,
  slab = NA,
  ilab = cbind(uniam_results_sub_o3$ci,uniam_results_sub_o3$em_p),
  ilab.xpos = c(-0.22,0.6),
  ilab.pos = 2,
  xlab = "",
  xlim = c(-0.6,0.6),
  alim = c(-0.2,0.4),
  at = seq(-0.2,0.4,0.2),
  psize = c(rep(1.2,20),3.2), 
  pch = c(rep(16,20),18),
  cex.lab = 0.5)
par(font = 2)
text(-0.43, dim(uniam_results_sub_o3)[1]+2, pos = 4, cex = 0.95, expression(paste(bold("Percent increase in risk of asthma hospitalization"))))
text(-0.43, dim(uniam_results_sub_o3)[1]+1.5, pos = 4, cex = 0.95, expression(paste(bold("     for each 1 ppb increase in O")[bold(3)],bold(' at lag 0-6 day'))))
text(0.44, dim(uniam_results_sub_pm25)[1]+1.5, pos = 4, cex = 0.95, "p-value")

par(mar=(c(5+0.1,0,4+0.1,2+0.1)), font = 1)
forest(# Plot data
  x = uniam_results_sub_no2$rr,
  ci.lb = uniam_results_sub_no2$rr_ci_lower,
  ci.ub = uniam_results_sub_no2$rr_ci_upper,
  annotate = F,
  slab = NA,
  ilab = cbind(uniam_results_sub_no2$ci,uniam_results_sub_no2$em_p),
  ilab.xpos = c(-0.03,0.8),
  ilab.pos = 2,
  xlab = "",
  xlim = c(-0.4,0.8),
  alim = c(0,0.61),
  at = seq(0,0.61,0.2),
  psize = c(rep(1.2,20),3.2), 
  pch = c(rep(16,20),18),
  cex.lab = 0.5)
par(font = 2)
text(-0.25, dim(uniam_results_sub_no2)[1]+2, pos = 4, cex = 0.95, expression(paste(bold("Percent increase in risk of asthma hospitalization"))))
text(-0.25, dim(uniam_results_sub_no2)[1]+1.5, pos = 4, cex = 0.95, expression(paste(bold("    for each 1 ppb increase in NO")[bold(2)],bold(' at lag 0-6 day'))))
text(0.64, dim(uniam_results_sub_pm25)[1]+1.5, pos = 4, cex = 0.95, "p-value")

par(op)
dev.off()

