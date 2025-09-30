

#' Make IRS Rounds Setup Object for a District
#'
#' @param dname the district name
#'
#' @returns an IRS multi-round setup object
#' @export
make_irs_history = function(dname){
  t_init = get_irs_jdates(dname)
  if(length(t_init) == 0){
    return(list())
  } else {
    ix = which(ramp.uganda::uga_irs$district == dname)
    here = uga_irs[ix,]
    type = here$round
    dta <- data.frame(t_init, type)
    dta$event = "irs"
    dta$coverage = 0
    ot <- order(t_init)
    dta <- dta[ot, ]
    dta$round = 1:length(t_init)
    return(dta)
}}


#' Make Bed Net Rounds Setup Object for a District
#'
#' @param dname the district name
#'
#' @returns a bednet multi-round setup object
#' @export
make_bednet_history = function(dname){
  t_init = get_itn_jdates(dname)
  if(length(t_init) == 0){
    return(list())
  } else {
    ix = which(ramp.uganda::uga_itn$district == dname)
    here = uga_itn[ix,]
    type = "pbo"
    dta <- data.frame(t_init, type)
    dta$event = "bednet"
    dta$coverage = 0
    ot <- order(t_init)
    dta <- dta[ot, ]
    dta$round = 1:length(t_init)
    return(dta)
}}
