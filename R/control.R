#' Make IRS Rounds Setup Object for a District
#'
#' @param dname the district name
#' @param t_end a cutoff for late events
#'
#' @returns an IRS multi-round setup object
#' @export
make_irs_rounds = function(dname, t_end=3352){
  ix = which(ramp.uganda::uga_irs$district == dname)
  if(length(ix)==0)
    return(list(dname = dname, t_init = c(), coverage = c(), type = c()))

  t_init = get_irs_jdates(dname)
  ix_in = which(t_init  < t_end & t_init >0)

  if(length(ix_in)==0)
    return(list(dname = dname, t_init = c(), coverage = c(), type = c()))

  d_irs <- ramp.uganda::uga_irs[ix,]
  nRounds <- length(ix)

  rounds <- list(
    district <- dname,
    t_init = t_init,
    coverage = rep(0, length(ix)),
    type = d_irs$round)

  rounds$t_init = rounds$t_init[ix_in]
  rounds$coverage = rounds$coverage[ix_in]
  rounds$type = rounds$type[ix_in]

  return(rounds)
}

#' Make Bed Net Rounds Setup Object for a District
#'
#' @param dname the district name
#' @param t_end a cutoff for late events
#'
#' @returns a bednet multi-round setup object
#' @export
make_bednet_rounds = function(dname, t_end=3352){
  ix = which(ramp.uganda::uga_itn$district == dname)
  if(length(ix)==0)
    return(list(dname = dname, t_init = c(), coverage = c(), type = c()))

  t_init = get_itn_jdates(dname)
  ix_in = which(t_init  < t_end & t_init >0)

  if(length(ix_in)==0)
    return(list(dname = dname, t_init = c(), coverage = c(), type = c()))

  d_bednets <- ramp.uganda::uga_itn[ix,]
  nRounds <- length(ix)

  rounds <- list(
    district <- dname,
    t_init = t_init,
    coverage = rep(0, length(ix)),
    type = rep("pbo", nRounds))

  rounds$t_init = rounds$t_init[ix_in]
  rounds$coverage = rounds$coverage[ix_in]
  rounds$type = rounds$type[ix_in]

  return(rounds)
}
