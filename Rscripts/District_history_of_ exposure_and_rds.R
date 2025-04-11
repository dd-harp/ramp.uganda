## ----message = FALSE, warning = FALSE----------------------------------------------------------------------------------
library(dplyr)
library(ramp.xds)
library(ramp.library)
library(ramp.work)


## ----read TPR data-----------------------------------------------------------------------------------------------------
source("my_ramp_uganda.R")
data_file <- paste(ramp_uganda_local, "data/pfpr_ts_district_from_hf_pixel_agg.csv", sep="")
tprdata <- read.csv(file=data_file)
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
## ----data_list---------------------------------------------------------------------------------------------------------
data_list = list()
for(i in 1:146)
  data_list[[i]] <- get_one_district(i)

## ----fit_all_district--------------------------------------------------------------------------------------------------
# List all RDS files in the directory
mod_dir <- paste(Box_RAMP_local, "Models/", sep="")
rds_files <- list.files(path = mod_dir, pattern = "\\.rds$", full.names = TRUE)
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
  tt <- seq(-90, ceiling(max(data$times)/365)*365-90, length.out=200)
  mod <- xds_solve_cohort(mod, times=tt)
  get_XH(mod) -> vals
  pars <- get_Xpars(mod, 1)

  # -------------Compute R_0, ioD, peak and annual EIR ________________________________________________________________________
  alp<- 0.1
  R0 <-(vals$eir*pars$b*(1+pars$c*vals$true_pr)*(1+ alp))/(pars$r*vals$true_pr)
  mean_R0 <- mean(R0)
  ioD <- compute_IoD_F(mod$EIRpar$F_season)
  phase <- mod$EIRpar$season_par$phase
  ave_annual_eir <- mean(vals$eir * 365)
  start_date <- as.Date("2021-01-01")  # Define the start date as begining of year
  date <- start_date + phase  # Add phase
  month_peak <- format(date,"%m-%d")

 # ____________________Plot PR, EIR, trend and seasonal outputs_______________________________________________________
  with(vals, plot(time/365+FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1), col="darkred"))
  with(data, lines(times/365+FirstYear, pr, type="o"))
  trend = mod$EIRpar$F_trend(tt)
  season = mod$EIRpar$F_season(tt)
  EIR = mod$F_eir(tt, 0)
  plot(tt/365+FirstYear, EIR, type = "l", main = paste("EIR, peak day = ",month_peak),lwd=2, xlab = "Time", ylim = range(0, EIR), ylab ="daily EIR", col = "darkblue")
  plot(tt/365+FirstYear, trend, type = "l", main = "Interannual Trend, Seasonality", lwd=2,xlab = "Time", ylim = range(0, trend,      season), ylab ="Trend, Seasonality", col = "darkblue")
   lines(tt/365+FirstYear, season)

   # ----------plot residuals--------------------------------------------------------------------------------------------
   mod1 <- xds_solve_cohort(mod, times=data$times)
   get_XH(mod1)$true_pr -> ppr
   resid = data$pr - ppr
   plot(data$times/365+FirstYear, resid, xlab = "Time", main = "Residual Errors", type = "b", ylim = c(-0.25,.25))
   segments(FirstYear,0, max(tt/365+FirstYear),0)

   # #___________________________plot R0___________________________________________________________________________________
   # plot(tt/365+FirstYear, R0, type = "l", main = "Adjusted Reproductive Number", lwd=2,xlab = "Time", ylim = range(0, R0), ylab ="R_c", col = "darkgreen")
   #  segments(FirstYear-1,1, max(tt/365+FirstYear),1,col = "red", lwd=2)

    return(list(
      name = data$name,
      ave_annual_eir = ave_annual_eir,
      phas_peak = phase,
      ioD = ioD,
      mean_R0 = mean_R0)
    )

}

## ----------------------------------------------------------------------------------------------------------------------
#pdf("../Images/all_district.pdf")

source("R/vector_control.R")
names_file <- "data/district_directories.csv"
district_names <- read.csv(names_file, header=TRUE)
names_file <- "data/city_directories.csv"
city_names <- read.csv(names_file, header=TRUE)
district_names <- rbind(district_names, city_names)
uga_irs <- read.csv("data/uga_irs_fmt.csv", header=T)

malaria_data <- list()
for (i in 1:146) {
  location <- fitted_mod[[i]]$location
  ix = which(district_names$dir == location)
  dist_name <- district_names[ix,1]
  image_file <- paste(intel_local, location, "/", fitted_mod[[i]]$location, "_eir.png", sep="")
  png(image_file, width=1040,height=720)
  par(mfrow = c(2,2))
  Est_data <- profile(i, fitted_mod, data_list)
  add_irs_history(dist_name, uga_irs)
  dev.off(dev.cur())
  malaria_data[[i]] <- Est_data
}
#malaria_dat_df <- do.call(rbind, malaria_data)
#write.csv(malaria_dat_df, "../data/all_malaria_data_R0.csv", row.names = FALSE)
#dev.off(dev.cur())

