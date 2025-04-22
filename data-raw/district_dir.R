district_dir <- read.csv("data-raw/district_directories.csv", header=TRUE)
district_dir$city=0
city_dir$city=1
district_dir <- rbind(district_dir, city_dir)
usethis::use_data(district_dir, overwrite=TRUE)
