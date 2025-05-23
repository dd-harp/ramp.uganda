start = as.Date("2015-01-01")
pfpr_by_district <- read.csv("data-raw/pfpr_ts_district_from_hf_pixel_agg.csv", header=TRUE)
pfpr_by_district$date <- as.Date(pfpr_by_district$date)
pfpr_by_district$jdate <- julian(pfpr_by_district$date, origin = start)
pfpr_by_district <- data.frame(pfpr_by_district)
# Fix this in production: for now this omits two
# clear outliers
ix = c(3452, 7062)
pfpr_by_district <- pfpr_by_district[-ix,]
usethis::use_data(pfpr_by_district, overwrite=TRUE)
