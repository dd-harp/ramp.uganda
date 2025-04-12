
#' Compute Various Summary Statistics
#'
#' @param model a fitted model, with
#' @param data the
#'
#' @returns a [list] of summary statistics
#' @export
#summarize_mod_fit = function(model, data){
#  pars <- get_Xpars(model, 1)
#
#  # -------------Compute R_0, ioD, peak and annual EIR ________________________________________________________________________
#  alp<- 0.1
#  R0 <-(vals$eir*pars$b*(1+pars$c*vals$true_pr)*(1+ alp))/(pars$r*vals$true_pr)
#  mean_R0 <- mean(R0)
#  ioD <- compute_IoD_F(model$EIRpar$F_season)
#  phase <- model$EIRpar$season_par$phase
#  ave_annual_eir <- mean(vals$eir * 365)
#  start_date <- as.Date("2021-01-01")  # Define the start date as begining of year
#  date <- start_date + phase  # Add phase
#  month_peak <- format(date,"%m-%d")
#
#  return(list(
#    name = data$name,
#    ave_annual_eir = ave_annual_eir,
#    phas_peak = phase,
#    ioD = ioD,
#    mean_R0 = mean_R0)
#  )
#}
