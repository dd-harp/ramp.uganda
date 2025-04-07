
plot_irs_history = function(district_name, start=2012, end=2026, add=FALSE, clr = viridis::turbo(7)[2]){
  origin = as.Date(paste(start,"-01-01", sep =""))
  last = as.Date(paste(end, "-01-01", sep ="")) - 1
  years <- c(start:end)
  yrs <- 365*(c(start:end)-start)

  uga_irs <- read.csv("data/uga_irs_fmt.csv", header=T)
  ix = which(uga_irs$location == district_name)
  here_irs <- uga_irs[ix,]
  here_irs$spray_start <- as.Date(here_irs$spray_start)
  here_irs$spray_end <- as.Date(here_irs$spray_end)

  tt <- c(0,as.numeric(last-origin))
  mtl = paste(district_name, "Vector Control")
  if(add==FALSE){
    plot(tt, c(-1,3), type ="n", xlab = "Date", xaxt="n", yaxt = "n", ylab = "", main = mtl)
    lines(tt, 0*tt)
  }
  axis(1, yrs, years)
  irs_dates <- as.numeric(here_irs$spray_start-origin)
  text(irs_dates, irs_dates*0+0.95, here_irs$formulation, srt=90, adj=0, col = grey(0.5))
  text(irs_dates, irs_dates*0+0.2, "IRS: ", srt=90, adj=0, col = clr)
  points(irs_dates, irs_dates*0, pch = 15, col = clr)
}

get_irs_dates = function(district_name, start=2012){
  origin = as.Date(paste(start,"-01-01", sep =""))
  uga_irs <- read.csv("data/uga_irs_fmt.csv", header=T)
  ix = which(uga_irs$location == district_name)
  here_irs <- uga_irs[ix,]
  here_irs$spray_start <- as.Date(here_irs$spray_start)
  irs_dates <- as.numeric(here_irs$spray_start-origin)
  return(irs_dates)
}

plot_itn_history = function(district_name, start=2012, end=2026, add=FALSE, clr = viridis::turbo(7)[6]){
  origin = as.Date(paste(start,"-01-01", sep =""))
  last = as.Date(paste(end, "-01-01", sep ="")) - 1
  years <- c(start:end)
  yrs <- 365*(c(start:end)-start)

  uga_itn <- read.csv("data/uga_itn_fmt.csv", header=T)
  ix = which(uga_itn$district_name == district_name)
  here_itn <- uga_itn[ix,]
  here_itn$distribution_date <- as.Date(here_itn$mean_date)
  itn_dates <- as.numeric(here_itn$distribution_date-origin)

  tt <- c(0,as.numeric(last-origin))
  mtl = paste(district_name, "Vector Control")
  if(add==FALSE){
    plot(tt, c(-0.5,2), type ="n", xlab = "Date", xaxt="n", xaxt = "n", ylab = "", yaxt="n", main = mtl)
    lines(tt, 0*tt)
    axis(1, yrs, years)
    text(itn_dates, itn_dates*0 + 0.2, "ITN ", srt=90, adj=0, col = clr)
    points(itn_dates, itn_dates*0, pch = 19, col = clr)
  } else {
    text(itn_dates, itn_dates*0-0.2, "ITN ", srt=90, adj=1, col = clr)
    points(itn_dates, itn_dates*0, pch = 19, col = clr)
  }
}
