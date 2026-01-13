#' Setup Fitting for a Uganda District
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param ix the index of a district
#' @param N the number of spline interpolation points
#' @param tmin censor data before tmin
#' @param tmax censor data after tmax
#'
#'
#' @returns a **`ramp.xds`** model object
#' @export
setup_ug_fitting = function(xds_obj, ix, N=c(), tmin=0, tmax=Inf){
  prts  <- get_district_pfpr_i(ix)
  dname <- unique(prts$district_name)
  dir_name <- unique(prts$dir_name)
  xds_obj$location <- dname
  xds_obj$saveto   <- dir_name


  ix = with(prts, which(jdate<tmax & jdate>tmin))
  xds_obj <- setup_fitting(xds_obj, prts$pfpr[ix], prts$jdate[ix], N=N)

  irs_ix <- which(uga_irs$location == dname)

  if(length(irs_ix)>0){
    irs_start <- as.vector(uga_irs[irs_ix,]$jdate)
    irs_round <- uga_irs[irs_ix,]$round
    xds_obj <- setup_irs_events(xds_obj, irs_start, irs_round)
  }

  bednet_jdates <- sort(get_itn_jdates(dname))

  xds_obj <- setup_bednet_events(xds_obj, bednet_jdates)

  return(xds_obj)
}

#' Setup Bednet Evaluation
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param round the round to evaluate
#' @param N the number of spline points
#' @param buf the buffer around the event
#'
#' @returns a **`ramp.xds`** model object
#' @export
setup_bednet_eval = function(xds_obj, round, N=2, buf=1000){
  jd <- xds_obj$events_obj$bednet$start_day[round]
  ix = with(xds_obj$data_obj, which(jdates<jd+buf & jdates>jd-buf))
  jday <- xds_obj$data_obj$jdates[ix]
  pr <- xds_obj$data_obj$pfpr[ix]
  xds_obj <- setup_fitting(xds_obj, pr, jday, N=N)
  return(xds_obj)
}



#' Fit the Realized History
#'
#' @param ix an index for a district
#' @param base_model a **`ramp.xds`** model object with scaling
#' @param model_name a string, the model name
#'
#' @returns the filename
#' @export
realized_history_ix = function(ix, base_model, model_name = "sip_eir"){
  dist_mod <- setup_ug_fitting(base_model, ix, 9)
  print(paste("Realized History: ", dist_mod$location, " (", ix, ")"), sep="")
  filename = paste(dist_mod$saveto, "-", model_name, "-history.rds", sep="")
  if(file.exists(filename)){
    print(paste(filename, " Exists"))
  } else {
    print(filename)
    file.create(filename)
    Nrounds <- dist_mod$events_obj$bednet$N
    dist_mod$events_obj$bednet$event_length = rep(100, Nrounds)
    dist_mod <- change_bednet_shock_multiround(dist_mod, rep(0, Nrounds))
    dist_mod <- pr2history_xm(dist_mod)
    filename = paste(dist_mod$saveto, "-", model_name, "-history.rds", sep="")
    dist_mod$filename = filename
    saveRDS(dist_mod, filename)
  }
  return(filename)
}

#' Evaluate the Bed Net Rounds
#'
#' @param ix an index for a district
#' @param model_name a string, the model name
#'
#' @returns a string
#' @export
short_fit_bednet_rounds = function(ix, model_name = "sip_eir"){
  prts  <- get_district_pfpr_i(ix)
  saveto <- unique(prts$dir_name)
  filename = paste("./outputs/", saveto, "-", model_name, "-history.rds", sep="")
  print(filename)
  stopifnot(file.exists(filename))
  real_hist <- readRDS(filename)
  Nrounds = real_hist$events_obj$bednet$N
  for(round in 2:Nrounds){
    print(c(round=round))
    filename = paste(saveto, "-", model_name, "-round", round, "hist.rds", sep="")
    if(file.exists(filename)){
      print("file existst")
    } else {
      print(filename)
      file.create(filename)
      dist_round_hist <- setup_bednet_eval(real_hist, round=round, N=3, buf=1000)
      dist_round_hist <- fit_trend(dist_round_hist)
      dist_round_hist <- fit_season(dist_round_hist)
      dist_round_hist <- norm_trend(dist_round_hist)
      dist_round_hist <- fit_trend(dist_round_hist)
      saveRDS(dist_round_hist, filename)
    }

    Xinit = rep(0, Nrounds)
    Xinit[round] = 0.1
  }
  return(filename)
}

#' Evaluate the Bed Net Rounds
#'
#' @param ix an index for a district
#' @param model_name a string, the model name
#'
#' @returns a string
#' @export
eval_bednet_rounds = function(ix, model_name = "sip_eir"){
  prts  <- get_district_pfpr_i(ix)
  saveto <- unique(prts$dir_name)
  filename = paste("./outputs/", saveto, "-", model_name, "-history.rds", sep="")
  print(filename)
  file.exists(filename)
  real_hist <- readRDS(filename)
  Nrounds = real_hist$events_obj$bednet$N
  for(round in 2:Nrounds){
    filename = paste(saveto, "-", model_name, "-round", round, "hist.rds", sep="")
    dist_round_hist = readRDS(filename)
    print(c(round=round))

    Xinit = rep(0, Nrounds)
    Xinit[round] = 0.1

    #print("dist_shock_3")
    ## Leave the Middle, Fit Shocks
    filename = paste(saveto, "-", model_name, "-round", round, "shock3.rds", sep="")
    if(file.exists(filename)){
      print("file exists")
    }else{
      print(filename)
      file.create(filename)
      dist_shock_3 <- change_bednet_shock_multiround(dist_round_hist, Xinit)
      dist_shock_3 <- fit_bednet_shock(dist_shock_3, list(bednet_ix=round))
      dist_shock_3 <- fit_trend(dist_shock_3)
      dist_shock_3 <- fit_bednet_shock(dist_shock_3, list(bednet_ix=round))
      dist_shock_3 <- fit_trend(dist_shock_3)
      saveRDS(dist_shock_3, filename)
    }
    #print("dist_shock_2")
    ## Remove the Middle, Fit ShocksA
    filename = paste(saveto, "-", model_name, "-round", round, "shock2.rds", sep="")
    if(file.exists(filename)){
      print("file exists")
    }else{
      print(filename)
      file.create(filename)
      dist_shock_2 <- change_bednet_shock_multiround(dist_round_hist, Xinit)
      dist_shock_2 <- rm_ix_fit_spline_ty(2, dist_shock_2)
      dist_shock_2 <- fit_bednet_shock(dist_shock_2, list(bednet_ix=round))
      dist_shock_2 <- fit_trend(dist_shock_2)
      dist_shock_2 <- fit_bednet_shock(dist_shock_2, list(bednet_ix=round))
      dist_shock_2 <- fit_trend(dist_shock_2)
      saveRDS(dist_shock_2, filename)
    }}
  return(filename)
}

#' Evaluate the Bed Net Rounds
#'
#' @param ix an index for a district
#' @param model_name a string, the model name
#'
#' @returns a string
#' @export
re_eval_bednet_rounds = function(ix, model_name = "sip_eir"){
  prts  <- get_district_pfpr_i(ix)
  saveto <- unique(prts$dir_name)
  for(round in 2:3){

    Xinit = rep(0, 4)
    Xinit[round] = 0.1

    #print("dist_shock_3")
    ## Leave the Middle, Fit Shocks
    filename = paste("./outputs/", saveto, "-", model_name, "-round", round, "shock3.rds", sep="")
    print(filename)
    dist_shock_3 <- readRDS(filename)
    dist_shock_3 <- fit_bednet_shock(dist_shock_3, list(bednet_ix=round))
    dist_shock_3 <- fit_trend(dist_shock_3)
    dist_shock_3 <- norm_trend(dist_shock_3)
    dist_shock_3 <- fit_season(dist_shock_3)
    dist_shock_3 <- fit_bednet_shock(dist_shock_3, list(bednet_ix=round))
    dist_shock_3 <- fit_trend(dist_shock_3)
    saveRDS(dist_shock_3, filename)

    #print("dist_shock_2")
    ## Remove the Middle, Fit ShocksA
    filename = paste("./outputs/", saveto, "-", model_name, "-round", round, "shock2.rds", sep="")
    dist_shock_2 <- readRDS(filename)
    dist_shock_2 <- rm_ix_fit_spline_ty(2, dist_shock_2)
    dist_shock_2 <- fit_bednet_shock(dist_shock_2, list(bednet_ix=round))
    dist_shock_2 <- fit_trend(dist_shock_2)
    dist_shock_2 <- norm_trend(dist_shock_2)
    dist_shock_2 <- fit_season(dist_shock_2)
    dist_shock_2 <- fit_bednet_shock(dist_shock_2, list(bednet_ix=round))
    dist_shock_2 <- fit_trend(dist_shock_2)
    saveRDS(dist_shock_2, filename)
  }
  return(filename)
}
