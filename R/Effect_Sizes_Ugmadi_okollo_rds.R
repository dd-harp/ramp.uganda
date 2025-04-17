## -----------------------------------------------------------------------------
library(dplyr)
library(ramp.xds)
library(ramp.work)
library(ramp.control)
library(ramp.library)
library(ramp.uganda)
#attach(yaml::read_yaml("../my_paths.yaml"))



#tprdata <- read.csv(file='C:/Users/Admin/Documents/ramp.uganda/data-raw/pfpr_ts_district_from_hf_pixel_agg.csv')
tprdata <- read.csv(file='C:/Users/doree/Documents/Github/ramp.uganda/data-raw/pfpr_ts_district_from_hf_pixel_agg.csv')
sorted_data <- tprdata[order(tprdata$district_name,tprdata$period),] #arrange the data in chronical order of period and district name
base_data <-sorted_data[sorted_data$period >= 201506 & sorted_data$period <= 202211, ]
id_namef= sorted_data$district_name
id_name = base_data$district_name
dist_name = unique(base_data$district_name)
dist_namef= unique(sorted_data$district_name)

## ----get_one_district--------------------------------------------------------------------------------------------------
get_one_district_base = function(i){
  # Pull PfPR values
  pfpr <- as.vector(base_data %>% filter(id_name == dist_name[i]) %>% select(pfpr))$pfpr
    # Pull and convert month year to date
  t_start =2015
  period <- as.vector(base_data %>% filter(id_name == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15 # time in days
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}

get_one_district = function(i){
  # Pull PfPR values
  pfpr <- as.vector(sorted_data %>% filter(id_namef == dist_name[i]) %>% select(pfpr))$pfpr
  # Pull and convert month year to date
  t_start =2015
  period <- as.vector(sorted_data %>% filter(id_namef == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15 # time in days
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}

t_start =2015
year1 <- as.numeric(substr(202211, 1, 4))  # Extract first 4 characters
month1 <- as.numeric(substr(202211, 5, 6))  # Extract last 2 characters
fac_time_month1 = (year1+month1/12) # TIME IN months
itn_starttime = (fac_time_month1-t_start)*365 -15 # time in days


#-------------------------------------full model time fit -------------------------------------------------------------
data_list = list()
for(i in 92)
  data_list[[i]] <- get_one_district(i)
 #-------------------------------- base model time fit----------------------------------------------------------------- 
 data_list_base <- list()
for(i in 92)
  data_list_base[[i]] <- get_one_district_base(i)

#--------------------------------------------------------------------------------------------------------
fit_one_district_base = function(i, model){
    with(data_list_base[[i]],
         return(pr2eir_history(pr, times, model))
)}
#----------------------pre control baseline---------------------------------------------------  \
fitted_models=list() 
  
# Lo <- list(
#     Lambda = 50,
#     season_par = makepar_F_sin(),
#     trend_par = makepar_F_spline(data_list_base[[92]]$times, data_list_base[[92]]$times*0+1)
#   ) 
Xo = list(xi=1/365, rho=0.2)
model <- xds_setup_cohort(Xname= "SIP", Xopts = Xo, F_season = make_function(makepar_F_sin()))

  
directory <- paste("C:/Users/doree/Box/RAMP", "/models", sep = "") 
eir_files <- list.files(path = directory, pattern = "*_eir_sis.rds", full.names = TRUE)
  
## ----fit_all_district--------------------------------------------------------------------------------------------------
fitted_models <- list()
fitted_mod <- list()
# Loop through each RDS file and load it into the list
for (i in 92) {
    fitted_models[[i]] <- fit_one_district_base(i, model)
    fitted_mod[[i]] <- readRDS(eir_files[i])
  }
  
  
## -----------Set up plotting profile---------------------------------------------------------------------------------
FirstYear = 2015.5
profileL = function(i, fitted_models, data_list_base,fitted_mod, data_list){
  #FirstYear = 2016
  
  mod <- fitted_models[[92]]
  mod_full <- fitted_mod[[92]]
  data <- data_list_base[[92]]
  data_full <- data_list[[92]]

  tt <- seq(-90, ceiling(max(data$times)/365)*365-90 , length.out=200)
  tt_full<- seq(-90, ceiling(max(data_full$times)/365)*365-90 , length.out=200)
  tt_foc <- seq(-90, ceiling(max(data$times)/365)*365-90+5*365 , length.out=200)
  
  mod <- xds_solve_cohort(mod, times=tt)
  get_XH(mod) -> vals
  mod_full <- xds_solve_cohort(mod_full, times=tt_full)
  get_XH(mod_full) -> vals_full
  
  baselinevals <- vector("list", 10)
  for (j in 1:10) {
    mod1 <- forecast_spline_EIR(mod, 5, x_last = 0)
    mod1 <- xds_solve_cohort(mod1, times = tt_foc)
    vals1 <- get_XH(mod1)
    baselinevals[[j]] <- vals1$true_pr
    
    # with(vals1, plot(mod1$outputs$time / 365 + FirstYear, baselinevals[[j]], ylab = "PR", lwd = 2,
    #                  xlab = "Time", main = data$name, type = "l", ylim = c(0, 1), col = "magenta"))
    # with(vals, lines(mod$outputs$time / 365 + FirstYear, true_pr, col = "darkblue"))
    # with(vals_full, lines(mod_full$outputs$time / 365 + FirstYear, true_pr, col = "black"))
    # with(data_full, lines(times / 365 + FirstYear, pr, pch = 19, type = "o", col = "red"))
   }
  # baselinevals is a list of numeric vectors of equal length
  median_list <- apply(do.call(rbind, baselinevals), 2, median)
  mean_list <- Reduce("+", baselinevals) / length(baselinevals)
  max_list <- apply(do.call(rbind, baselinevals), 2, max)
  
  closest_index <- which.min(abs(vals_full$time - itn_starttime))
  
  effectsize_median <- ((median_list[closest_index:length(vals_full$time)] - vals_full$true_pr[closest_index:length(vals_full$time)])/median_list[closest_index:length(vals_full$time)])*100
  effectsize_mean <- ((mean_list[closest_index:length(vals_full$time)] - vals_full$true_pr[closest_index:length(vals_full$time)])/mean_list[closest_index:length(vals_full$time)])*100
  effectsize_max <- ((max_list[closest_index:length(vals_full$time)] - vals_full$true_pr[closest_index:length(vals_full$time)])/max_list[closest_index:length(vals_full$time)])*100
  
  #par(mfrow = c(2, 1))
  with(vals_full, plot(time/365+FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1),col="black")) #model fit for full data
  lines(vals1$time/365 + FirstYear, max_list, lwd = 2, col="purple")# forcat after 2018_dec
  lines(vals1$time/365 + FirstYear, mean_list, lwd = 2, col="red")# forcat after 2018_dec
  lines(vals1$time/365 + FirstYear, median_list, lwd = 2, col="green")# forcat after 2018_dec
  #with(vals, lines(time/365+FirstYear, true_pr, lwd=2, col="darkblue"))# model fit from 2015-2018 dec
  with(data_full, lines(data_full$times/365+FirstYear, pr, pch=19,type = "o", col="darkblue")) #full data plot
  
  #inset=-0.2 
  #par(xpd = TRUE, mar = c(5, 4, 4, 2))  # allow plotting outside
  legend("topleft",              # Position of the legend
         legend = c("fitted model", "max baseline","mean baseline","median baseline","pfpr data"), # Names for the groups
         col = c("black", "purple","red","green","darkblue"),
         lwd=2, pch=c(NA,NA,NA,NA,19), horiz = FALSE, bty="n")      # Colors corresponding to the groups
  
  plot(FirstYear + (vals_full$time[closest_index:length(vals_full$time)])/365,effectsize_median, ylab = "Effect sze", lwd=2, xlab = "Time", type = "l",col="green" )
  lines(FirstYear + (vals_full$time[closest_index:length(vals_full$time)])/365,effectsize_mean, lwd=2, col="red")
  lines(FirstYear + (vals_full$time[closest_index:length(vals_full$time)])/365,effectsize_max, lwd=2, col ="purple")
  
  legend("bottomright",              # Position of the legend
         legend = c(
           paste0("median_effs: ", sprintf("%.2f%%", median(effectsize_median))),
           paste0("mean_effs: ", sprintf("%.2f%%", mean(effectsize_median))),
           paste0("max_effs: ", sprintf("%.2f%%", max(effectsize_median)))
         ),
         col = c("green", "red", "purple"),
         lwd = 2,
         horiz = FALSE,
         bty = "n")
  
  
  # Symbols for the groups
  # return(list(
  #   name = data$name,
  #   baseline = baselinevals,
  #   fit_vals = vals,
  #   full_fit_vals = vals_full,
  #   forecast_vals = vals1
  # ))
  
}

pdf("C:/Users/doree/Documents/Github/Doreen/ramp_uganda_files/Madi_Okollo_effectsize.pdf")
for(i in 92)
  profileL(i, fitted_models, data_list_base, fitted_mod, data_list)
dev.off(dev.cur())


