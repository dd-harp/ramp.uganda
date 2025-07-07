## ----message = FALSE, warning = FALSE----------------------------------------------------------------------------------
library(dplyr)
library(ramp.xds)
library(ramp.library)
library(ramp.work)


library(dplyr)

## ----read TPR data-----------------------------------------------------------------------------------------------------
tprdata <- read.csv(file='C:/Users/Admin/Documents/ramp.uganda/data-raw/pfpr_ts_district_from_hf_pixel_agg.csv')
sorted_data <- tprdata[order(tprdata$district_name,tprdata$period),] #arrange the data in chronical order of period and district name
id_name = sorted_data$district_name
dist_name = unique(sorted_data$district_name)

## ----get_one_district--------------------------------------------------------------------------------------------------
get_one_district = function(i){
  # Pull PfPR values
  pfpr <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(pfpr))$pfpr
  # Pull and convert month year to date
  t_start =2016
  period <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15 # time in days
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}
## ----data_list---------------------------------------------------------------------------------------------------------
data_list = list()
for(i in 10)
  data_list[[i]] <- get_one_district(i)

## ----fit_one_district--------------------------------------------------------------------------------------------------
fit_one_district = function(i, model){
  with(data_list[[i]],
    return(pr2eir_history(pr, times, model))
)}
## ----fit all-----------------------------------------------------------------------------------------------------------
fitted_models=list()
Xo = list(xi=1/365, rho=0.2)
Lo = list()
Lo$Lambda = 1000
Lo$Lt = function(t, pars){(1.01+sin(2*pi*t/365))^2}
model <- xds_setup_cohort(Xname= "SIP",Xopts = Xo)
tt <- seq(0, ceiling(max(data_list[[10]]$times) / 365) * 365 + (5 * 365), length.out = 200)
model<- xds_solve_cohort(model, times=tt)
xds_plot_PR(model)
for(i in 10)
  fitted_models[[i]] = fit_one_district(i, model)

vals <- get_XH(fitted_models[[10]])
FirstYear<- 2015
with(vals, plot(time/365+FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1), col="darkred"))
with(data_list[[10]], lines(times/365+FirstYear, pr, pch=16, type="o"))

## -----------Set up plotting profile---------------------------------------------------------------------------------
FirstYear = 2015
profile = function(i, model_list, data_list){
  mod <- fitted_models[[10]]#model_list[[i]]
  data <- data_list[[10]]
  tt <- seq(-90, ceiling(max(data$times)/365)*365-90, length.out=200)
  mod <- xds_solve_cohort(mod, times=tt)
  get_XH(mod) -> vals
  pars <- get_Xpars(mod, 1)

  # add itn
  library(ramp.control)
  cover_par <- list(trend_par = makepar_F_sharkfin())

  itn_mod <- setup_bednets(mod,
                           coverage_name = "func",
                           coverage_opts = cover_par,
                           effectsizes_name = "lemenach")

  itn_mod <- xds_solve_cohort(itn_mod, times=tt)
  vals_itn <- get_XH(itn_mod)
 # ____________________Plot PR, EIR, trend and seasonal outputs_______________________________________________________
  with(vals, plot(time/365+FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1), col="darkred"))
  with(vals_itn, lines(time/365+ FirstYear, true_pr, col= "blue"))
  with(data, lines(times/365+FirstYear, pr, pch=16, type="o"))


}

## ----------------------------------------------------------------------------------------------------------------------
for (i in 10) {
  profile(i, fitted_models, data_list)
  }


