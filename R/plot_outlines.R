

#' Plot all regions and shade a focal region
#' @note `uga_region_shp` is imported from `ramptools`
#' @param region the region name
#'
#' @returns invisible()
#' @export
shade_region = function(region){
  which(uga_region_shp$name == region) ->ix
  clrs = rep("white", 15)
  clrs[ix] <- grey(0.5)
  plot(uga_region_shp$geometry, col = clrs)
  return(invisible())
}

#' Title
#'
#' @param region
#' @param names
#' @param xshift district specific x values to shift the label
#' @param yshift district specific y values to shift the label
#'
#' @returns invisible()
#' @export
#'
#' @examples
label_region = function(region, names, xshift=c(), yshift=c()){
  ix = which(uga_district_shp$name %in% names)
  plot(uga_district_shp$geometry[ix])
  lix = length(ix)
  if(length(xshift)==0) xshift = rep(0, lix)
  if(length(yshift)==0) yshift = rep(0, lix)
  for(i in 1:lix){
    j = ix[i]
    xy = st_centroid(uga_district_shp$geometry[j])
    xy = xy[[1]][c(1,2)]
    text(xy[1]+xshift[i], xy[2]+yshift[i], uga_district_shp$name[j], cex=.5)
  }
  return(invisible())
}


#' Plot a region outline
#'
#' @param region the region name
#'
#' @returns invisible()
#' @export
#'
#' @examples
outline_region = function(region){
  which(uga_region_shp$name == region) ->ix
  plot(uga_region_shp$geometry[ix], lwd=2, add=TRUE)
  return(invisible())
}

#' Plot a district in red within its region
#'
#' @param district the district name
#'
#' @returns invisible()
#' @export
#'
#' @examples
shade_district = function(district){
  region = unique(loc_table[district_name == district]$region)
  clrs = rep("white", 146)
  other = unique(loc_table[region_name == region]$district)
  ix = which(uga_district_shp$name %in% other)
  clrs[ix] <- grey(0.5)
  which(uga_district_shp$name == district) ->ix
  clrs[ix] <- "red"
  mtl = paste(district, "in", region)
  plot(uga_district_shp$geometry, col = clrs, main=mtl)
  outline_region(region)
  return(invisible())
}
