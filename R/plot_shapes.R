
#' Standardized Plot for Uganda Districts
#'
#' @param clrs The color(s) for the district
#' @param ttl The Plot Title
#'
#' @returns invisible()
#' @export
uga_districts_plot = function(clrs = "#E6E6E6", ttl="Default Uganda by District"){
  plot(ramptools::uga_district_shp$geometry, col = clrs, main = ttl)
  plot(uga_water_shp$geometry, col = "skyblue", border="skyblue", add=TRUE)
  plot(ramptools::uga_district_shp$geometry, add=TRUE)
  plot(ramptools::uga_region_shp$geometry, add=TRUE, lwd=3)
  return(invisible())
}

#' Standardized Plot for Uganda Regions
#'
#' @param clrs The color(s) for the district
#' @param ttl The Plot Title
#'
#' @returns invisible()
#' @export
uga_regions_plot= function(clrs = "#E6E6E6", ttl="Default Uganda by District"){
  plot(ramptools::uga_region_shp$geometry, col=clrs, lwd=3)
  plot(uga_water_shp$geometry, col = "skyblue", border="skyblue", add=TRUE)
  plot(ramptools::uga_district_shp$geometry, add=TRUE)
  return(invisible())
}

#' Plot Uganda Shading a Region
#'
#' @param region the region name
#'
#' @returns invisible()
#' @export
shade_region = function(region, clrs = "#CCCCCC"){
  which(ramptools::uga_region_shp$name == region)->ix
  plot(ramptools::uga_region_shp$geometry[ix], col = clrs, add=TRUE)
  return(invisible())
}

#' Plot a district in red within its region
#'
#' @param district the district name
#'
#' @returns invisible()
#' @export
make_district_icon = function(district){
  ix = which(district_dir$district_name == district)
  region = district_dir$in_region[ix]
  ttl <- paste(district, "in", region)
  plot(ramptools::uga_district_shp$geometry, col = "#F2F2F2", main = ttl)
  shade_region(region, "#808080")
  plot(uga_water_shp$geometry, col = "skyblue", border="skyblue", add=TRUE)
  plot(ramptools::uga_district_shp$geometry, add=TRUE)
  plot(ramptools::uga_region_shp$geometry, add=TRUE, lwd=3)
  shade_district(district, "darkred")
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
#' @param lwd the line width
#' @param clr the border color
#' @param add replot if add=FALSE
#'
#' @returns invisible()
#' @export
outline_region = function(region, lwd=2, clr = "black", add=TRUE){
  which(ramptools::uga_region_shp$name == region) ->ix
  plot(ramptools::uga_region_shp$geometry[ix], border=clr, bg=NULL, lwd=lwd, add=add)
  return(invisible())
}

#' Plot a district in red within its region
#'
#' @param district the district name
#'
#' @returns invisible()
#' @export
shade_district = function(district, clr = "darkred"){
  ix =  which(ramptools::uga_district_shp$name == district)
  plot(ramptools::uga_district_shp$geometry[ix], col=clr, add=TRUE)
  return(invisible())
}
