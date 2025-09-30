
#' Get a District PfPR, by Name
#'
#' @param district a district name
#'
#' @returns a data frame
#' @export
get_district_pfpr = function(district){
  ix <- which(ramp.uganda::pfpr_by_district$district_name == district)
  tmp <- ramp.uganda::pfpr_by_district[ix,]
  ot <- order(tmp$jdate)
  return(tmp[ot,])
}

#' Get a District PfPR, by Index
#'
#' @param i district index
#'
#' @returns a data frame
#' @export
get_district_pfpr_i = function(i){
  this_one <- ramp.uganda::district_dir[i,]
  district <- this_one$district_name
  dir_name <- this_one$dir
  prts <- get_district_pfpr(district)
  prts$dir_name <- dir_name
  return(prts)
}

#' Get a District PfPR, by Index
#'
#' @param i district index
#'
#' @returns a data frame
#' @export
get_one_district = function(i){
  get_district_pfpr_i(i)
}
