
profile_eir_i = function(i, model_list, data_list){
  mod <- model_list[[i]]
  data <- data_list[[i]]
  profile_eir(mod, data)
}

profile_eir = function(model, data){

  tt <- seq(-90, ceiling(max(data$times)/365)*365-90, length.out=200)
  model <- xds_solve_cohort(model, times=tt)

  get_XH(model) -> vals
  pars <- get_Xpars(model, 1)

  # -------------Compute R_0, ioD, peak and annual EIR ________________________________________________________________________
  alp<- 0.1
  R0 <-(vals$eir*pars$b*(1+pars$c*vals$true_pr)*(1+ alp))/(pars$r*vals$true_pr)
  mean_R0 <- mean(R0)
  ioD <- compute_IoD_F(model$EIRpar$F_season)
  phase <- model$EIRpar$season_par$phase
  ave_annual_eir <- mean(vals$eir * 365)
  start_date <- as.Date("2021-01-01")  # Define the start date as begining of year
  date <- start_date + phase  # Add phase
  month_peak <- format(date,"%m-%d")

  # ____________________Plot PR, EIR, trend and seasonal outputs_______________________________________________________
  with(vals, plot(time/365+FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1), col="darkred"))
  with(data, lines(times/365+FirstYear, pr, type="o"))
  trend = model$EIRpar$F_trend(tt)
  season = model$EIRpar$F_season(tt)
  EIR = model$F_eir(tt, 0)
  plot(tt/365+FirstYear, EIR, type = "l", main = paste("EIR, peak day = ",month_peak),lwd=2, xlab = "Time", ylim = range(0, EIR), ylab ="daily EIR", col = "darkblue")
  plot(tt/365+FirstYear, trend, type = "l", main = "Interannual Trend, Seasonality", lwd=2,xlab = "Time", ylim = range(0, trend,      season), ylab ="Trend, Seasonality", col = "darkblue")
  lines(tt/365+FirstYear, season)

  # ----------plot residuals--------------------------------------------------------------------------------------------
  model1 <- xds_solve_cohort(model, times=data$times)
  get_XH(model1)$true_pr -> ppr
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
