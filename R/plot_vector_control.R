
#' Create a Plot of IRS spray Dates for a District
#'
#' @param district_name a district name
#' @param start the first year to plot
#' @param end the last year to plot
#' @param add if TRUE, add to an existing plot
#' @param clr a color for the labels
#' @importFrom graphics plot lines text points axis
#'
#' @returns invisible()
#' @export
plot_irs_history = function(district_name, start=2012, end=2026, add=FALSE, clr = "#4686FBFF"){
  origin = as.Date(paste(start,"-01-01", sep =""))
  last = as.Date(paste(end, "-01-01", sep ="")) - 1
  years <- c(start:end)
  yrs <- 365*(c(start:end)-start)

  ix = which(ramp.uganda::uga_irs$location == district_name)
  here_irs <- ramp.uganda::uga_irs[ix,]
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
  text(irs_dates, irs_dates*0+0.95, here_irs$formulation, srt=90, adj=0, col = "#808080")
  text(irs_dates, irs_dates*0+0.2, "IRS: ", srt=90, adj=0, col = clr)
  points(irs_dates, irs_dates*0, pch = 15, col = clr)
  return(invisible())
}

#' Add IRS History as Vertical Lines
#'
#' @param district_name a district name
#' @param ymax a scaling factor
#' @param clr a color for the labels
#' @param Yr0 the start year
#' @importFrom graphics segments text
#'
#' @returns invisible()
#' @export
add_irs_history = function(district_name, ymax=1, clr="#4686FBFF", Yr0=2015){
  origin = as.Date(paste(Yr0,"-01-01", sep =""))

  jd <- get_irs_jdates(district_name, Yr0)

  if(length(jd)>0){
    ix = which(ramp.uganda::uga_irs$location == district_name)
    formula <- ramp.uganda::uga_irs[ix,]$formulation

    jd = jd/365 + Yr0

    for(i in 1:length(jd)){
      points(jd[i], 0, pch = 15, col=clr)
      segments(jd[i], 0, jd[i], .4*ymax, col=clr)
      text(jd[i], ymax, formula[i], srt=90, adj=0, pos=2, col = clr, cex=0.8)
    }
  }
}

#' Add itn History as Vertical Lines
#'
#' @param district_name a district name
#' @param ymax a district name
#' @param clr a color for the labels
#' @param Yr0 the start year
#' @importFrom graphics segments text
#'
#' @returns invisible()
#' @export
add_itn_history = function(district_name, ymax=1, clr = "#E4460AFF", Yr0=2015){
  origin = as.Date(paste(Yr0,"-01-01", sep =""))

  jd <- get_itn_jdates(district_name, Yr0)

  if(length(jd)>0){
    ix = which(ramp.uganda::uga_itn$location == district_name)

    jd = jd/365 + Yr0

    for(i in 1:length(jd)){
      points(jd[i], 0, pch = 21, col=clr)
      segments(jd[i], 0, jd[i], .5*ymax, col=clr)
    }
  }
}

#' Get Julian dates for IRS spray rounds
#'
#' @param district_name  a district name
#' @param Yr0 the start year
#'
#' @returns a vector
#' @export
get_irs_jdates = function(district_name, Yr0=2015){
  origin = as.Date(paste(Yr0,"-01-01", sep =""))
  ix = which(ramp.uganda::uga_irs$location == district_name)
  here_irs <- ramp.uganda::uga_irs[ix,]
  here_irs$spray_start <- as.Date(here_irs$spray_start)
  irs_dates <- as.numeric(here_irs$spray_start-origin)
  return(irs_dates)
}

#' Get Julian dates for ITN spray rounds
#'
#' @param district_name  a district name
#' @param Yr0 the start year
#'
#' @returns a vector
#' @export
get_itn_jdates = function(district_name, Yr0=2015){
  origin = as.Date(paste(Yr0,"-01-01", sep =""))
  ix = which(ramp.uganda::uga_itn$district == district_name)
  jd <- ramp.uganda::uga_itn[ix,]$distribution_date
  for(i in 1:length(jd)) jd[i] <- paste(jd[i], "-15", sep="")
  jd <- as.Date(jd)
  itn_dates <- as.numeric(jd-origin)
  return(itn_dates)
}

#' Create a Plot of ITN Mass Distributions for a District
#'
#' @param district_name a district name
#' @param start the first year to plot
#' @param end the last year to plot
#' @param add if TRUE, add to an existing plot
#' @param clr a color for the labels
#' @importFrom graphics plot lines text points axis
#'
#' @returns invisible()
#' @export
plot_itn_history = function(district_name, start=2012, end=2026, add=FALSE, clr = "#E4460AFF"){
  origin = as.Date(paste(start,"-01-01", sep =""))
  last = as.Date(paste(end, "-01-01", sep ="")) - 1
  years <- c(start:end)
  yrs <- 365*(c(start:end)-start)

  ix = which(ramp.uganda::uga_itn$district_name == district_name)
  here_itn <- ramp.uganda::uga_itn[ix,]
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
