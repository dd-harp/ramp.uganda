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

model <- xds_setup(model_name = "sis")
model <- setup_travel_eir(model, travelEIR = 1/36500)
model$Lpar[[1]]$Lambda <- 250
model <- xds_solve(model, 3650, 3650)
model <- last_to_inits(model)
model$Lpar[[1]]$season_par <- makepar_F_sin()
model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
model$Lpar[[1]]$trend_par <- makepar_F_spline(c(0:9)*365, rep(1,10))
model$Lpar[[1]]$F_trend <- make_function(model$Lpar[[1]]$trend_par)

args <- commandArgs(trailingOnly=TRUE)
run_ix = c(args[1]:args[2])
for(dist_ix in run_ix){
  district <- district_names[dist_ix,1]
  model$location <- district_names[dist_ix,2]
  prts <- get_pfpr_ts(district, pfpr_by_district)
  fit_model <- pr2Lambda_history(prts$pfpr, prts$jdate, model)
  fit_model <- xds_solve(fit_model, times=seq(0, max(prts$jdate), by = 10))
  filename <- paste(Save_Models_To, fit_model$location, "_Lambda_", fit_model$model_name, ".rds", sep="")
  saveRDS(fit_model, filename)
}

