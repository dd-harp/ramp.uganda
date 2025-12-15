
#' Get Neighboring Districts
#'
#' @param ix the index of a district
#'
#' @returns a list of neighbors and their indices
#' @export
get_neighbors = function(ix){
  obj = list()
  obj$this = list(ix=ix, name=district_dir[ix,]$district_name)
  nbrs <- which(district_adjacency[ix,] == TRUE)
  obj$ix_neighbors = nbrs
  obj$neighbors = district_dir[nbrs,]$district_name
  return(obj)
}
