library(dplyr)
library(ramp.xds)
library(ramp.library)
library(ramp.work)

#This returns  the i^th data set as a list:
#-------------------------------------------------------------------------------------------------------------------
tprdata <- read.csv(file='C:/Users/doree/Documents/Github/ramp.uganda/data/pfpr_ts_district_from_hf_pixel_agg.csv')
sorted_data <- tprdata[order(tprdata$district_name,tprdata$period),] #arrange the data in chronical order of period and district name
id_name = sorted_data$district_name
dist_name = unique(sorted_data$district_name)


## ----get_one_district--------------------------------------------------------------------------------------------------
get_one_district = function(i){
  # Pull PfPR values
  pfpr <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(pfpr))$pfpr
  # Pull and convert month year to date
  t_start =2015
  period <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15 # time in days
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}

#----------------------------------------------------------------------------------------------------
data_list = list()
for(i in 1:146)
  data_list[[i]] <- get_one_district(i)

## ----fit_all_district--------------------------------------------------------------------------------------------------
directory <- "C:/Users/doree/Box/Models"
# List all RDS files in the directory
rds_files <- list.files(path = directory, pattern = "\\.rds$", full.names = TRUE)
eir_files <- grep("_eir_sis\\.rds$", rds_files,value=TRUE)
# Initialize an empty list to store the contents
fitted_mod <- list()
# Loop through each RDS file and load it into the list
for (i in 1:146) {
  fitted_mod[[i]] <- readRDS(eir_files[i])
}

## -----------Set up plotting profile---------------------------------------------------------------------------------
FirstYear = 2015
profile = function(i, model_list, data_list){
  mod <- model_list[[i]]
  data <- data_list[[i]]
  tt <- seq(-90, ceiling(max(data$times)/365)*365-90+5*365 , length.out=200)
  mod <- xds_solve_cohort(mod, times=tt)
  get_XH(mod) -> vals
  mod1a <- forecast_spline_EIR(mod, 5,x_last=1)
  mod1a <- xds_solve_cohort(mod1a, times=tt)
 clrs <- viridis::turbo(146)
 get_XH(mod1a) -> vals_forecast
 with(vals_forecast, plot(time/365 + FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1), col=clrs[i]))
 with(data, lines(times/365+FirstYear, pr, pch=19,type = "o", col="darkblue"))
}

pdf("C:/Users/doree/Documents/Github/Doreen/ramp_uganda_files/dist_all_forecast.pdf")
#par(mfrow = c(1,2))
for(i in 1:146)
  profile(i, fitted_mod, data_list)
dev.off(dev.cur())
