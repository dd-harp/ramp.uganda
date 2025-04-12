
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
#' @param clr a color for the labels
#' @param Yr0 the start year
#' @importFrom graphics segments text
#'
#' @returns invisible()
#' @export
add_irs_history = function(district_name, clr = "#4686FBFF", Yr0=2015){
  origin = as.Date(paste(Yr0,"-01-01", sep =""))
  jd <- get_irs_jdates(district_name, ramp.uganda::uga_irs)

  if(length(jd)>0){
    ix = which(ramp.uganda::uga_irs$location == district_name)
    formula <- ramp.uganda::uga_irs[ix,]$formulation

    for(i in 1:length(jd)){
      segments(jd[i], 0.5, jd[i], 1)
      text(jd[i], 0, formula[i], srt=90, adj=0, col = clr)
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
