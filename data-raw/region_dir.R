region_dir <- read.csv("data-raw/region_directories.csv", header=TRUE)
usethis::use_data(region_dir, overwrite=TRUE)
