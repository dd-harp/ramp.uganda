library(ramp.uganda)
prts <- get_district_pfpr("Terego District")
with(prts, {
  plot(jdate, pfpr, ylim = c(0,0.65), main = "Terego District", xlab = "Julian Date (day 1 is Jan 1, 2015)", ylab = "PR")
  lines(jdate, pfpr)
})

library(ramp.work)

fit_model <- pr2eir_history(prts$pfpr, prts$jdate, model)
fit_model <- xds_solve_cohort(fit_model,
                              times=seq(0, max(prts$jdate), by = 10))
saveRDS(fit_model, "fit_model.rds")
profile_pr(fit_model, prts)
mod <- xds_setup(Xname = "SIS", MYZname = "SI",
                 Lopts = list(
                   season_par = makepar_F_sin(),
                   trend_par = makepar_F_spline(c(0:9)*365, rep(1,10))
                 ))
mod <- xds_solve(mod, 3650, 3650)
mod <- last_to_inits(mod)
saveRDS(mod, "mod.rds")
fit_mod <- pr2Lambda_history(prts$pfpr, prts$jdate, mod)
fit_mod <- xds_solve(fit_mod, times=seq(0, 3210, by = 10))
fit_mod <- last_to_inits(fit_mod)
fit_mod <- xds_solve(fit_mod, times=seq(0, max(prts$jdate), by = 10))
saveRDS(fit_mod, "fit_mod.rds")
profile_pr(fit_mod, prts)
cf_baseline <- forecast_spline_Lambda(fit_mod, 5, x_last=3)
cf_baseline <- xds_solve(cf_baseline, times=seq(0, 5000, by = 10))
xds_plot_PR(cf_baseline, clrs = "skyblue")
xds_plot_PR(fit_mod, add=T)

mda_pars <- makepar_F_sharkfin(D=3840, L=40, dk = 1, uk=1, mx=80)
mda <- make_function(mda_pars)
mda_model <- cf_baseline
mda_model$Xpar[[1]]$F_treat <- mda

mda_model <- xds_solve(mda_model, times=seq(0, 4400, by = 10))
xds_plot_PR(mda_model)

