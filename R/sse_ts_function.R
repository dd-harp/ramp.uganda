#' @title Given data, compute GoF for a seasonal function
#' @description For a time series c(`times`,`data`),
#' and a model, compute the sum of squared errors
#' @param data the PR observed
#' @param times the times of the observations
#' @param func the model
#' @return a list with the mean peak and the values
#' @export
sse_ts_function <- function(ts_data, times, func){
  pr <- func(times)
  return(sum((ts_data - pr)^2))
}

#' @title Make a function of time
#' @description Return a function with the given
#' average, with seasonality and interannual variation
#' @param avg the average
#' @param Spar parameters for the seasonal component
#' @param Tpar parameters for the interannual component
#' @return a function
#' @export
funky <- function(avg, Spar, Tpar){
  F_S <- ramp.xds::make_function(Spar)
  F_T <- ramp.xds::make_function(Tpar)
  return(function(t){avg*F_S(t)*F_T(t)})
}

#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak `dog`
#' @param data the PR observed
#' @param model an `xds` model
#' @param Spar parameters for `makepar_F_sin`
#' @return an `xds` object
#' @export
fit_ts_trend <- function(ts_data, times, Spar){

  tt = seq(0, ceiling(max(times)/365))*365

  F_eval = function(X, avg, times, ts_data, Spar){
    avg <- mean(ts_data)
    Tpar <- makepar_F_spline(tt, X)
    func <- funky(avg, Spar, Tpar)
    sse_ts_function(ts_data, times, func)
  }

  inits = c(rep(1, length(tt)))

  pars <- stats::optim(inits, F_eval, ts_data=ts_data, times=times, Spar=Spar)$par

  Tpar = makepar_F_spline(tt=tt, yy=pars)
  pars <- list(avg=mean(ts_data), Spar=Spar, Tpar=Tpar, frame = "func")
  class(pars$frame) <- "func"
  return(pars)
}

#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak `dog`
#' @param data the PR observed
#' @param model an `xds` model
#' @param Tpar parameters for the interannual pattern
#' @return an `xds` object
#' @export
fit_ts_season <- function(ts_data, times, Tpar){

  pwc <- function(x){
    10*(1-exp(x)/(1+exp(x)))+0.5
  }
  tt = seq(0, ceiling(max(times)/365))*365

  F_eval = function(X, avg, times, ts_data, Tpar){
    avg <- mean(ts_data)
    Spar <- makepar_F_sin(phase = X[1], bottom = X[2]^2, pw = pwc(X[3]))
    func <- funky(avg, Spar, Tpar)
    sse_ts_function(ts_data, times, func)
  }


  inits = c(1, 0.6, 0.3)

  pars <- stats::optim(inits, F_eval, ts_data=ts_data, times=times, Tpar=Tpar)$par
  Spar = makepar_F_sin(phase = pars[1], bottom = pars[2]^2, pw = pwc(pars[3]))
  pars <- list(avg=mean(ts_data), Spar=Spar, Tpar=Tpar, frame = "func")
  class(pars$frame) <- "func"
  return(pars)
}

#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak `dog`
#' @param data the PR observed
#' @param model an `xds` model
#' @param Fpar parameters from `makepar_F_sin`
#' @return an `xds` object
#' @export
fit_ts_function <- function(ts_data, times){

  pwc <- function(x){
    10*(1-exp(x)/(1+exp(x)))+0.5
  }
  tt = seq(0, ceiling(max(times)/365))*365

  F_all = function(X, avg, times, ts_data){
    avg <- mean(ts_data)
    Spar <- makepar_F_sin(phase = X[1], bottom = X[2]^2, pw = pwc(X[3]))
    Tpar <- makepar_F_spline(tt, X[-c(1:3)])
    func <- funky(avg, Spar, Tpar)
    sse_ts_function(ts_data, times, func)
  }

  F_trend = function(X, avg, times, ts_data){
    avg <- mean(ts_data)
    Spar <- makepar_F_sin(phase = X[1], bottom = X[2]^2, pw = pwc(X[3]))
    Tpar <- makepar_F_spline(tt, X[-c(1:3)])
    func <- funky(avg, Spar, Tpar)
    sse_ts_function(ts_data, times, func)
  }


  inits = c(1, 0.6, 0.3, rep(1, length(tt)))
  pars <- stats::optim(inits, F_eval, ts_data=ts_data, times=times, method = "SANN")$par

  pars <- stats::optim(inits, F_eval, ts_data=ts_data, times=times)$par

  Spar = makepar_F_sin(phase = pars[1], bottom = pars[2]^2, pw = pwc(pars[3]))
  Tpar = makepar_F_spline(tt=tt, yy=pars[-c(1:3)])
  return(list(avg=mean(ts_data), Spar=Spar, Tpar=Tpar))
}
