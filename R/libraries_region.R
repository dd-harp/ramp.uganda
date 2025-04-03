suppressMessages(library(ramptools))
suppressMessages(library(sf))

shade_region = function(region, names){
  which(uga_region_shp$name == region) ->ix
  clrs = rep("white", 15)
  clrs[ix] <- grey(0.5)
  plot(uga_region_shp$geometry, col = clrs)
}


label_region = function(region, names, xshift, yshift){
  ix = which(uga_district_shp$name %in% names)
  plot(uga_district_shp$geometry[ix])
  for(i in 1:length(ix)){
    j = ix[i]
    xy = st_centroid(uga_district_shp$geometry[j])
    xy = xy[[1]][c(1,2)]
    text(xy[1]+xshift[i], xy[2]+yshift[i], uga_district_shp$name[j], cex=.5)
  }
}
