
#' Plot Uganda Shading a Region
#'
#' @param region the region name
#'
#' @returns invisible()
#' @export
shade_region = function(region){
  which(ramptools::uga_region_shp$name == region) ->ix
  clrs = rep("white", 15)
  clrs[ix] <- "#808080"
  plot(ramptools::uga_region_shp$geometry, col = clrs)
  return(invisible())
}

#' Plot a Region with District Labels
#'
#' @param region the region name
#' @param xshift district specific x values to shift the label
#' @param yshift district specific y values to shift the label
#'
#' @returns invisible()
#' @export
label_region = function(region, xshift=c(), yshift=c()){
  ix = which(ramptools::uga_district_shp$name == region)
  plot(ramptools::uga_district_shp$geometry[ix])
  lix = length(ix)
  if(length(xshift)==0) xshift = rep(0, lix)
  if(length(yshift)==0) yshift = rep(0, lix)
  for(i in 1:lix){
    j = ix[i]
    xy = sf::st_centroid(ramptools::uga_district_shp$geometry[j])
    xy = xy[[1]][c(1,2)]
    text(xy[1]+xshift[i], xy[2]+yshift[i], ramptools::uga_district_shp$name[j], cex=.5)
  }
  return(invisible())
}


#' Plot a region outline
#'
#' @param region the region name
#' @param add replot if add=FALSE
#'
#' @returns invisible()
#' @export
outline_region = function(region, add=TRUE){
  which(ramptools::uga_region_shp$name == region) ->ix
  plot(ramptools::uga_region_shp$geometry[ix], lwd=2, add=add)
  return(invisible())
}

#' Plot a district in red within its region
#'
#' @param district the district name
#'
#' @returns invisible()
#' @export
shade_district = function(district){
  ix =  which(ramptools::loc_table$district_name == district)
  region =  unique(ramptools::loc_table$region_name[ix])
  clrs = rep("white", 146)
  ix = which(ramptools::loc_table$region_name == region)
  other = unique(ramptools::loc_table$district[ix])
  ix = which(ramptools::uga_district_shp$name %in% other)
  clrs[ix] <- "#808080"
  which(ramptools::uga_district_shp$name == district) ->ix
  clrs[ix] <- "red"
  mtl = paste(district, "in", region)
  plot(ramptools::uga_district_shp$geometry, col = clrs, main=mtl)
  outline_region(region)
  return(invisible())
}
