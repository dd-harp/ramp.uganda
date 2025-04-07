outline_region = function(region){
  which(uga_region_shp$name == region) ->ix
  plot(uga_region_shp$geometry[ix], lwd=2, add=TRUE)
}

shade_district = function(district, names){
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
}
