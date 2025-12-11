#' Setup Fitting for a Uganda District
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param ix the index of a district
#' @param N the number of spline interpolation points
#' @param tmin censor data before tmin
#' @param tmax censor data after tmax
#'
#' @returns a **`ramp.xds`** model object
#' @export
setup_ug_fitting = function(xds_obj, ix, N=c(), tmin=0, tmax=Inf){
  prts  <- get_district_pfpr_i(ix)
  dname <- unique(prts$district_name)
  xds_obj$location <- dname


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
