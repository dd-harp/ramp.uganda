district_dir <- read.csv("data-raw/district_directories.csv", header=TRUE)
library(ramptools)
library(sf)
x = rep(0, 146)
y = rep(0, 146)
short_name = district_dir$district_name
Snake_Name = district_dir$district_name

for(i in 1:146){
  j = which(uga_district_shp$name == district_dir$district_name[i])
  xy = st_centroid(uga_district_shp$geometry[j])
  print(c(i=i, j=j))
  x[i] = xy[[1]][1]
  y[i] = xy[[1]][2]
}

for(i in 1:146){
  stem = strsplit(district_dir$district_name[i], " ")
  if(length(stem[[1]]) == 2)
    Snake_Name[i] <- paste(stem[[1]][1], stem[[1]][2], sep="_")
  if(length(stem[[1]]) == 3)
    Snake_Name[i] <- paste(stem[[1]][1], stem[[1]][2], stem[[1]][3], sep="_")
  short_name[i] = stem[[1]][1]
}

district_dir$short = short_name
district_dir$x = x
district_dir$y = y
district_dir$lab_x = x
district_dir$lab_y = y
district_dir$alt_name = Snake_Name

usethis::use_data(district_dir, overwrite=TRUE)
