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
cf_baseline <- xds_solve(cf_baseline,  max(prts$jdate)+(5*365))
xds_plot_PR(cf_baseline, clrs = "skyblue")
xds_plot_PR(fit_mod, add=T)

library(ramp.control)
# Parameters
itn_type_eff <- 0.9                          # Maximum efficacy of ITN
effect_itn_start <- 0.3 * itn_type_eff       # Effect at deployment
effect_itn_end <- 0.01                       # Effect after full decay
decay_start <- (2023 - 2015) * 365           # Day ITN is deployed
decay_end <- (2026 - 2015) * 365             # Day ITN effect is negligible
t0 <- (decay_start + decay_end) / 2          # Midpoint of decay
k <- 0.01                                     # Steepness of decay

# Smooth logistic decay function
itn_pars <- function(t, e0 = effect_itn_start, e1 = effect_itn_end,
                     k_eff = k, t0_eff = t0) {
  e1 + (e0 - e1) / (1 + exp(k_eff * (t - t0_eff)))
}


#itn_pars <- makepar_F_sharkfin(D=3840, L=40, dk = 1, uk=1, mx=80)
#itn_pars <- makepar_F_sharkfin()
#itn_pars1 <- makepar_F_sigmoid(k=7)
cover_par <- list(trend_par = itn_pars)
itn_mod <- setup_bednets(cf_baseline,
                         coverage_name = "func",
                         coverage_opts = cover_par,
                         effectsizes_name = "lemenach")

FirstYear <- 2015
time_years <- cf_baseline$outputs$time/365 + FirstYear
itn_mod <- xds_solve(itn_mod, max(prts$jdate)+(5*365))
vals_cf <- get_XH(cf_baseline)
vals_itn <- get_XH(itn_mod)
df <- data.frame(
  Year = time_years,
  Original_PR = vals_cf$true_pr,
  itn_PR = round(vals_itn$true_pr, 4)
  #itn_PR_2026 = round(pr_itn_2026, 4)
)
df <- df[df$Year >= 2020,]
#df_first <- df[df$Year >= 2023 & df$Year <= 2026, ]
#df_sec   <- df[df$Year >= 2025.8, ]

# === Plot for each ITN type ===
plot(df$Year, df$Original_PR, type = "l", lwd = 2, col = "blue",
     ylim = c(0, 1),
     ylab = "Parasite Rate (PR)", xlab = "Year",
     main = paste(unique(prts$district_name), "-", "cov=", 0.3*100,"%"))

lines(df$Year, df$itn_PR, col = "darkred", lwd = 2)
#lines(df$Year, df$itn_PR_2026, col = "green", lwd = 2)
with(prts, lines(jdate / 365 + FirstYear, pfpr, pch = 16, type = "o", col = "darkblue"))

mda_model <- xds_solve(itn_mod, times=seq(0, 4400, by = 10))
xds_plot_PR(mda_model,clrs = "blue")
xds_plot_PR(cf_baseline, clrs = "skyblue",add = T)
xds_plot_PR(fit_mod, add=T)

