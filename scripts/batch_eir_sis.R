# Rscript batch_eir i

library(ramp.xds)
library(ramp.work)


Save_Models_To <- "/Users/smitdave/Library/CloudStorage/Box-Box/RAMP/Models/"
data_assets_location <- "/Users/smitdave/git/ramp.uganda/data/"

names_file <- paste(data_assets_location, "district_directories.csv", sep="")
district_names <- read.csv(names_file, header=TRUE)
names_file <- paste(data_assets_location, "city_directories.csv", sep="")
city_names <- read.csv(names_file, header=TRUE)
district_names <- rbind(district_names, city_names)

#Set a Start Date
start_date = as.Date("2015-01-01")

data_file <- paste(data_assets_location, "pfpr_ts_district_from_hf_pixel_agg.csv", sep="")
pfpr_by_district <- read.csv(data_file, header=TRUE)
pfpr_by_district$date <- as.Date(pfpr_by_district$date)
pfpr_by_district$jdate <- julian(pfpr_by_district$date, origin = start_date)

get_pfpr_ts = function(district, data){
  ix <- which(data$district_name == district)
  tmp <- data[ix,]
  ot <- order(tmp$jdate)
  return(tmp[ot,])
}

model <- xds_setup_cohort(model_name = "sis")
model <- xds_solve_cohort(model, 3650, 3650)
model <- last_to_inits(model)

args <- commandArgs(trailingOnly=TRUE)
run_ix = c(args[1]:args[2])
for(dist_ix in run_ix){
  district <- district_names[dist_ix,1]
  model$location <- district_names[dist_ix,2]
  prts <- get_pfpr_ts(district, pfpr_by_district)
  fit_model <- pr2eir_history(prts$pfpr, prts$jdate, model)
  fit_model <- xds_solve_cohort(fit_model, times=seq(0, max(prts$jdate), by = 10))
  filename <- paste(Save_Models_To, fit_model$location, "_eir_", fit_model$model_name, ".rds", sep="")
  saveRDS(fit_model, filename)
}

