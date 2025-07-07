library(ramp.xds)
library(ramp.control)
library(ramp.work)
library(ramp.library)
model <- xds_setup(MYZname = "SI")
model <- xds_solve(model, 3650, 3650)
model <- last_to_inits(model)
cover <- makepar_F_sharkfin()
coverp <- make_function(cover)


tm <- seq(0, 500, by = 5)
plot(tm, coverp(tm), type = "l",
     xlab = "t - Time (in days)",
     ylab = expression(F(t)))
segments(100, 0, 100, 1, lty=2)
segments(280, 0, 280, 1, lty=2)
segments(100, .5, 280, .5, lty=2)

cover_par <- make_function(makepar_F_sharkfin())
itn_mod <- setup_bednets(model,
                         coverage_name = "func",
                         coverage_opts = cover_par,
                         effectsizes_name = "lemenach")
tm = seq(0, 5*365, by = 5)
cv <- itn_mod$bednets$coverage$F_trend(tm)
plot(tm, cv, type = "l")
model <- xds_solve(model, 5*365, 10)
model$nHostSpecies <-1
model$nVectorSpecies <- 1
model$nOtherVariables <- 1
itn_mod <- xds_solve(itn_mod, 5*365, 10)
xds_plot_aEIR(model)
xds_plot_aEIR(itn_mod, clrs = "red", add=TRUE)
fitted_mod[[i]]$nHostSpecies <- 1
fitted_mod[[i]]$nVectorSpecies <- 1
