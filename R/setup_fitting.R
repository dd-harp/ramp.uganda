#' Setup Fitting for a Uganda District
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param ix the index of a district
#' @param N the number of spline interpolation points
#'
#' @returns a **`ramp.xds`** model object
#' @export
setup_ug_fitting = function(xds_obj, ix, N=c()){
  prts  <- get_district_pfpr_i(ix)
  dname <- unique(prts$district_name)

  xds_obj$location <- dname

  xds_obj <- setup_fitting(xds_obj, prts$pfpr, prts$jdate, N=N)

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
