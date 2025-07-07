library(ramp.xds)
library(dplyr)
library(ramp.uganda)
library(ramp.work)
library(ramp.control)

fit_mod <- readRDS("fit_mod.rds")

# The first three spline knots and nodes
rbind(fit_mod$Lpar[[1]]$trend_par$tt[2:5],
round(1000*fit_mod$Lpar[[1]]$trend_par$yy[2:5])/1000)

# we want to replace knot 3, we get the mean of the preceeding two values
mean(fit_mod$Lpar[[1]]$trend_par$yy[c(2,3)])
fit_mod$Lpar[[1]]$trend_par$yy[4]

# After 6 years, there is a significant linear increase in malaria prevalence
knots <- fit_mod$Lpar[[1]]$trend_par$yy[2:7]
tm <- c(1:6)
plot(tm, knots)
llm <- lm(knots~tm)
abline(llm)
summary(llm)

# using the llm model to project
projected <- .802930 + .0372*c(7:9)
fit_mod1 <- fit_mod
fit_mod1$Lpar[[1]]$trend_par$yy[8:10] <- projected
fit_mod1$Lpar[[1]]$F_trend <-
  make_function(fit_mod1$Lpar[[1]]$trend_par)
fit_mod1 <- xds_solve(fit_mod1, 2550, 10)

fit_mod2 <- fit_mod
fit_mod2$Lpar[[1]]$trend_par$yy[8:10] <-
  mean(fit_mod$Lpar[[1]]$trend_par$yy[2:7])
fit_mod2$Lpar[[1]]$F_trend <-
  make_function(fit_mod2$Lpar[[1]]$trend_par)
fit_mod2 <- xds_solve(fit_mod2, 2550, 10)

xds_plot_PR(fit_mod1, clrs = "darkred")
xds_plot_PR(fit_mod2, clrs = "darkblue", add=TRUE)
xds_plot_PR(fit_mod, add=TRUE)
segments(2175, 0, 2175, 1)
segments(2540, 0, 2540, 1, lty = 2)

fit_mod1$Lpar[[1]]$trend_par$yy[8]/
  fit_mod$Lpar[[1]]$trend_par$yy[8]



fit_mod_focast <- last_to_inits(fit_mod)
fit_mod_focast$nHostSpecies <- 1
fit_mod_focast$nVectorSpecies <- 1
fit_mod_focast <- setup_other_variables(fit_mod_focast)
fit_mod_focast <- forecast_spline_Lambda(fit_mod_focast, 5, x_last = 1)
tt <- seq(0, ceiling(max(data$jdate) / 365) * 365 + (5 * 365), length.out = 200)

#fit_mod_focast <- xds_solve(fit_mod_focast, Tmax = max(tt))
fit_mod_focast <- xds_solve(fit_mod_focast, 2550,10)
xds_plot_PR(fit_mod_focast, clrs = "darkred")
xds_plot_PR(fit_mod1, clrs = "purple4", add=TRUE)
xds_plot_PR(fit_mod2, clrs = "darkblue", add=TRUE)
xds_plot_PR(fit_mod, add=TRUE)

vals_for <- get_XH(fit_mod_focast)
time_years <- mod_for_cast$outputs$time / 365 + FirstYear
t <- time_years

