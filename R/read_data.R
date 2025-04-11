
get_pfpr_from_tpr = function(pfpr_ts_file){
  filename = paste("data/", pfpr_ts_file, sep="")
  pfpr_from_tpr <<- read.csv(filename, header=TRUE)
}
