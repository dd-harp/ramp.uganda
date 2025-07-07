library(dplyr)
library(ramp.xds)
library(ramp.library)
library(ramp.work)

#This returns  the i^th data set as a list:
tprdata <- read.csv(file='C:/Users/Admin/Box/RAMP/data/pfpr_estimates/monthly/pfpr_ts_facility.csv')
sorted_data <- tprdata[order(tprdata$district_name,tprdata$period),] #arrange the data in chronical order of period and district name
id_name = sorted_data$district_name
dist_name = unique(sorted_data$district_name)

get_one_district = function(i){

  # Pull PfPR values
  pfpr <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(pfpr))$pfpr

  # Pull and convert month year to jdate
  t_start =2016
  period <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}


fit_one_district = function(i, model){
  with(data_list[[i]],
       return(pr_ts2Lambda_history(pr, times, model))
  )}

Xo = list(xi=1/365, rho=0.2)
data_list = list()
fitted_models = list()
solv_fitted = list()
xh = list()

for(i in 1){
  data_list[[1]] <- get_one_district(1)
  time <- data_list[[1]]$times
  pr <-  data_list[[1]]$pr
  dt = data.frame(time,pr)

  Fpw = function(x){1 + 8*x^2/(1+x^2)}
  Lopts <- list(Fpw)
  mod  <- xds_setup(MYZname = "SI",Lname = "basicL", Xname= "SIPmav3")


  mod <- xds_solve(mod,200)
  mod1 <- with(dt, pr2Lambda_history(pr, time, mod, twice=TRUE))
  Fx <- mod1$EIRpar$F_trend
  with(dt, integrate(Fx,min(tt), max(tt))$val/(max(tt)-min(tt)))
  mod2 <- with(dt, pr2eir_history(pr, time, mod, twice=TRUE))
  xh[[i]] <- get_XH(mod2, i=1)


  par(mfrow = c(3, 1),  # Arrange plots in a 2x2 grid
      mar = c(2, 2, 2, 2),  # Reduce margins for individual plots
      oma = c(1, 1, 1, 1))
  #par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
  plot(t_start+(data_list[[i]]$times/365), data_list[[i]]$pr, type = "o", lwd=2, col = i,ylim = c(0,1), pch = 16, main = paste(data_list[[i]]$name), xlab = "Time", ylab = "PR")
  with(xh[[i]], lines(t_start+(time/365), true_pr,lwd=2, col = "darkred"))


  #plot EIR
  plot(t_start+ (xh[[i]]$time/365), 365*xh[[i]]$eir, type = "l",lwd=2, col = "red",xlab = "Time ", ylab = " Annual EIR")

  # # Compute R0
  pars <- get_Xpars(mod2, 1)
  R0 <- (xh[[i]]$eir*pars$b)*(1-Xo$rho)/(pars$r+Xo$xi)
  #   #plot R0
  plot(t_start+ (xh[[i]]$time/365), R0, type = "l",lwd=2, col = "blue",  xlab= "Time", ylab = expression(R_0))

  #mtext("Time (years)", side = 1, outer = TRUE, line = 1)
}

dev.off()
