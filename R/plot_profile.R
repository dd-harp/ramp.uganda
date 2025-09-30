
#' Profile a Fitted Model from a List
#'
#' @param i index
#' @param model_list a list of models, as xds objects
#' @param data_list a list of data sets
#' @param Yr0 a reference year
#'
#' @returns invisible()
#' @export
profile_plot_i = function(i, model_list, data_list, Yr0=2015){
  mod <- model_list[[i]]
  data <- data_list[[i]]
  profile_plot(mod, data)
}

#' Profile a Fitted Model
#'
#' @param model a model; an xds object
#' @param data a data set
#' @param Yr0 a reference year
#'
#' @importFrom graphics par
#'
#' @returns invisible()
#' @export
profile_plot = function(model, data, Yr0=2015){

  tt <- seq(-90, ceiling(max(data$jdate)/365)*365-90, by=10)
  model <- ramp.xds::xds_solve_cohort(model, times=tt)

  par(mfrow = c(2,2))

  profile_pr(model, data, Yr0)
  profile_eir(model, Yr0)
  profile_residual(model, data, Yr0)
  profile_temporal(model, Yr0)

  return(invisible())
}

#' Plot Predicted and Observed PR
#'
#' @param model a model; an xds object
#' @param data a data set
#' @param Yr0 a reference year
#' @param y1 scale y limits to 1
#'
#' @returns invisible()
#' @export
profile_pr = function(model, data, Yr0=2015, y1=FALSE){

  if(y1 == FALSE) ylm = c(0, 1.5*max(data$pfpr))
  if(y1 == TRUE) ylm = c(0,1)
  dname <- data$district_name[1]
  with(get_XH_out(model),
       plot(model$outputs$time/365+Yr0, true_pr, ylim = ylm,
            ylab = "PR", lwd=2, xlab = "Time",
            main = dname, type = "l", col="darkred"))

  with(data, lines(jdate/365+Yr0, pfpr, type="o"))

  add_irs_history(dname,Yr0=Yr0)
  add_itn_history(dname,Yr0=Yr0)

  return(invisible())
}

#' Plot Predicted and Observed PR
#'
#' @param model a model; an xds object
#' @param Yr0 a reference year
#'
#' @importFrom graphics plot
#'
#' @returns invisible()
#' @export
profile_eir = function(model, Yr0=2015){

  tt <- model$outputs$time
  EIR = model$F_eir(tt, 0)
  phase <- model$EIRpar$season_par$phase

  month <- get_month(as.numeric(format(as.Date("2015-01-01") + phase, "%m")))
  day <- format(as.Date("2015-01-01") + phase, "%d")

  plot(tt/365+Yr0, EIR, type = "l", ylim = range(0, EIR),
       main = paste("EIR peak =", month, day), lwd=2,
       xlab = "Time",  ylab ="daily EIR", col = "darkblue")

  dname <- model$placename
  add_irs_history(dname, ymax = max(EIR), Yr0=Yr0)
  add_itn_history(dname, ymax = max(EIR), Yr0=Yr0)

  return(invisible())
}

#' Plot Seasonal & Interannual Patterns
#'
#' @param model a model; an xds object
#' @param Yr0 a reference year
#'
#' @importFrom graphics plot lines
#'
#' @returns invisible()
#' @export
profile_temporal = function(model, Yr0=2015){
  tt <- model$outputs$time
  trend = model$EIRpar$F_trend(tt)
  season = model$EIRpar$F_season(tt)
  plot(tt/365+Yr0, trend, type = "l", ylim = range(0, trend, season),
       main = "Interannual Trend, Seasonality",
       lwd=2, xlab = "Time",  ylab ="Trend, Seasonality", col = "darkblue")
  lines(tt/365+Yr0, season)

  return(invisible())
}

#' Plot the residuals
#'
#' @param model a model; an xds object
#' @param data a data set
#' @param Yr0 a reference year
#'
#' @importFrom graphics plot segments
#'
#' @returns invisible()
#' @export
profile_residual = function(model, data, Yr0=2015){

  model1 <- ramp.xds::xds_solve_cohort(model, times=data$jdate)
  resid = data$pfpr - get_XH_out(model1)$true_pr

  plot(data$jdate/365+Yr0, resid,
       type = "b", ylim = c(-0.25,.25),
       xlab = "Time", main = "Residual Errors")

  segments(Yr0, 0, max(data$jdate/365+Yr0),0)

  return(invisible())
}

