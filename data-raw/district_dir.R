district_dir <- read.csv("data-raw/district_directories.csv", header=TRUE)
usethis::use_data(district_dir, overwrite=TRUE)
