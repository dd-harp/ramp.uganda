
#' Get a District PfPR, by Name
#'
#' @param district a district name
#'
#' @returns a data frame
#' @export
get_district_pfpr = function(district){
  ix <- which(pfpr_by_district$district_name == district)
  tmp <- pfpr_by_district[ix,]
  ot <- order(tmp$jdate)
  return(tmp[ot,])
}

#' Get a District PfPR, by Index
#'
#' @param district a district name
#'
#' @returns a data frame
#' @export
get_district_pfpr_i = function(i){
  district <- district_dir[i,1]
  return(get_district_pfpr(district))
}
