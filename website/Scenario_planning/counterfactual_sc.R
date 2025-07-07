library(ramp.xds)
library(ramp.uganda)
fit_mod <- readRDS("fit_mod.rds")
rbind(fit_mod$Lpar[[1]]$trend_par$tt[2:5],
round(1000*fit_mod$Lpar[[1]]$trend_par$yy[2:5])/1000)
mean(fit_mod$Lpar[[1]]$trend_par$yy[c(2,3)])
fit_mod$Lpar[[1]]$trend_par$yy[4]
knots <- fit_mod$Lpar[[1]]$trend_par$yy[2:7]
tm <- c(1:6)
plot(tm, knots)
llm <- lm(knots~tm)
abline(llm)
summary(llm)
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
