## ----message = FALSE, warning = FALSE--------------------------------------------------------------
library(dplyr)
library(ramp.xds)
library(ramp.library)
library(ramp.work)

library(igraph)
library(tidygraph)
library(ggplot2)
library(ggraph)
## --------------------------------------------------------------------------------------------------
library(knitr)

# Convert Rmd to R
#purl("Dave_history_of_ exposure_all_dist_ro_core1.Rmd", output = "Dave_history_of_ exposure_all_dist_ro_core1.R")



## ----read pfpr data---------------------------------------------------------------------------------
tprdata <- read.csv(file='C:/Users/Admin/Documents/Doreen/district_level_pfpr_time_series.csv')
#pfpr <- tprdata$pfpr_pred
#agg = tprdata%>%group_by(district_name,  year,month)%>%summarize(pfpr.mean = mean(pfpr),.groups = 'drop')
id_name = tprdata$district_name
dist_name = unique(tprdata$district_name)


#-----------------------------------------------------------------------------------------------------------------------------------
get_one_district = function(i){

  # Pull PfPR values
  pfpr <- as.vector(tprdata %>% filter(id_name == dist_name[i]) %>% select(pfpr_pred))$pfpr_pred

  # Pull and convert month year to jdate
  t_start =2016
  period <- as.vector(tprdata %>% filter(id_name == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}
## ----data_list-------------------------------------------------------------------------------------
data_list = list()
for(i in 1)
  data_list[[i]] <- get_one_district(i)

## ----fit_one_district------------------------------------------------------------------------------
fit_one_district = function(i, model){
  with(data_list[[i]],
    return(pr2eir_history(pr, times, model))
)}

## ----eval=F----------------------------------------------------------------------------------------
fitted_models=list()


## ----fit all---------------------------------------------------------------------------------------
Xo = list(xi=1/365, rho=0.2)


model <- xds_setup_cohort(Xname= "SIP")
for(i in 1)
  fitted_models[[i]] = fit_one_district(i, model)



## --------------------------------------------------------------------------------------------------
FirstYear = 2016

profile = function(i, model_list, data_list){
  mod <- fitted_models[[1]]#model_list[[i]]
  data <- data_list[[1]]
  tt <- seq(-90, ceiling(max(data$times)/365)*365-90+(365*5), length.out=200)
  #tt <- seq(-90, ceiling(max(data$times)/365)*365-90, length.out=200)
  #mod <- xds_solve_cohort(mod, times=tt)
  get_XH(mod) -> vals
  pars <- get_Xpars(mod, 1)
  mod$nHostSpecies <- 1
  mod$nVectorSpecies <- 1
  mod <- setup_other_variables(mod)
  # === Decay and PR calculation ===
  #coverage <- 0.3
  itn_type_eff <- 0.9
  effect_itn_start <- 0.3*itn_type_eff
  effect_itn_end <- 0.01
  decay_start <- 2023
  decay_end <- 2026
  t0 <- (decay_start + decay_end) / 2
  t01 <- (decay_start + 3 + decay_end + 3) / 2
  k <- 1.5

  cov_opts <- list(
    mean = 0.5,
    F_season = function(t)
    {ifelse(t < 0, 0, (sin(2*pi*(t-365/4) / 365) + 1))}
  )
  library(ramp.control)
  mod <- last_to_inits(mod)
  itn_mod <- setup_bednets(mod,
                           coverage_name = "func", coverage_opts = cov_opts,
                           effectsizes_name = "lemenach")

  xds_solve_cohort(itn_mod,tt) -> itn_mod

  vals_cov <- get_XH(itn_mod)


    # Compute R0
  R0 <- mean((vals$eir * pars$b) * (1 - Xo$rho) / (pars$r + Xo$xi))
  ioD <- compute_IoD_F(mod$EIRpar$F_season)
  phase <- mod$EIRpar$season_par$phase
  Eir_tot <- vals$eir
  ave_annual_eir <- mean(vals$eir * 365)
  start_date <- as.Date("2020-01-01")  # Define the start date
  date <- start_date + phase  # Add 256 days
  #date <-  format(as.Date(FirstYear+phase), "%B %Y")

  with(vals, plot(time/365+FirstYear, true_pr, ylab = "PR", lwd=2, xlab = "Time", main = data$name, type = "l", ylim = c(0,1), col="darkred"))
  with(data, lines(times/365+FirstYear, pr, type="o"))
  trend = mod$EIRpar$F_trend(tt)
  season = mod$EIRpar$F_season(tt)
   EIR = mod$F_eir(tt, 0)
   plot(tt/365+FirstYear, EIR, type = "l", main = paste("EIR, peak day = ",phase),lwd=2, xlab = "Time", ylim = range(0, EIR), ylab ="daily EIR", col = "darkblue")
  #
   mod1 <- xds_solve_cohort(mod, times=data$times)

   get_XH(mod1)$true_pr -> ppr
   resid = data$pr - ppr
   plot(data$times/365+FirstYear, resid, xlab = "Time", main = "Residual Errors", type = "b", ylim = c(-0.25,.25))
   segments(FirstYear,0, max(tt/365+FirstYear),0)

   plot(tt/365+FirstYear, trend, type = "l", main = "Interannual Trend, Seasonality", lwd=2,xlab = "Time", ylim = range(0, trend, season), ylab ="Trend, Seasonality", col = "darkblue")
   lines(tt/365+FirstYear, season)
   return(list(
     Eir_tot = Eir_tot, time = vals$time, name = data$name
     )
   )
  }

#}

## --------------------------------------------------------------------------------------------------
#Dist_connect <- read.csv(file='C:/Users/Admin/Documents/Doreen/Adjacency_output.csv')
Dist_connect <- read.csv(file='C:/Users/Admin/Documents/Doreen/Adjacency_output.csv')
Dist_connect <- Dist_connect[, -c(1,2)]
matrix_adj <- as.matrix(Dist_connect)
rownames(matrix_adj) <- names(Dist_connect)
colnames(matrix_adj) <- names(Dist_connect)
# Reorder rows and columns alphabetically
Dist_sorted<- order(rownames(matrix_adj))
matrix_adj<- matrix_adj[Dist_sorted, Dist_sorted]


# Save the sorted data (optional)
write.csv(matrix_adj, "C:/Users/Admin/Documents/Doreen/new_data/Dist_sorted.csv")

# Convert adjacency matrix to an igraph object
network_graph <- graph_from_adjacency_matrix(matrix_adj, mode = "directed")

# Convert graph to a tidygraph object
graph_tbl <- as_tbl_graph(network_graph)

# Plot using ggraph

pdf("C:/Users/Admin/Documents/Doreen/new_data/district_connectivity_network.pdf", width = 8, height = 6)

ggraph(graph_tbl, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.09), edge_width = 0.3, color = "darkred", show.legend = FALSE) +
  geom_node_point(size = 2.5, color = "blue") +
  geom_node_text(aes(label = name), size = 1.8, hjust = 0, nudge_x = 0.1,repel = TRUE) +
  theme_void() +
  ggtitle("District Connectivity Network")

dev.off()
## ____________________________________________________________________________________________________________________
Eir_dat <- list()
for (i in 1:146) {
  eird <- profile(i, fitted_models, data_list)
  Eir_dat[[i]] <- eird
  }
## Example
sample_adj <- matrix_adj[1:146,1:146]
# Replace ones with 1/sum(row)
psi_adj<- t(apply(sample_adj, 1, function(row) {
  row_sum <- sum(row)  # Calculate the sum of the row
  row[row == 1] <- 1 / row_sum  # Replace ones with 1/sum(row)
  return(row)
}))
delta <- 0.05
t_trav_diag <- diag(delta, 146)
inv_diag <- solve(diag((1-delta),146))
tot_Eir <- sapply(as.matrix(Eir_dat), function(x) x$Eir_tot)
time <- sapply(as.matrix(Eir_dat), function(x) x$time)
name <- sapply(as.matrix(Eir_dat), function(x) x$name)
E_travel <- list()
E_total <- list()
E_local <- list()
for (i in (1:length(time[,1]))){
E_travel[[i]] <- t_trav_diag%*%psi_adj%*% tot_Eir[i,]
E_total[[i]] <- tot_Eir[i,]}

E_total <- do.call(rbind, E_total)
E_travel1 <- do.call(rbind,E_travel)
E_travel_matrix <- t(matrix(E_travel1,146,200))

for (i in (1:length(time[,1]))){
E_local[[i]] <- inv_diag %*% (E_total[i,] -(E_travel_matrix[i,] %*% E_total[i,]))}

E_local1 <- do.call(rbind,E_local)
E_local_matrix <- t(matrix(E_local1,146,200))


#Eir_loc_t <- solve((t_trav_diag %*% psi_adj) + diag((1-delta),5))%*%tot_Eir
##b  plot local eir
#pdf("loc_eir_ts.pdf", width = 9, height = 7)
pdf("C:/Users/Admin/Documents/Doreen/new_data/eir_ts.pdf", width = 9, height = 7)
for (i in 1:146){
 plot(time[,1]/365+FirstYear, E_local_matrix[,i], type = "l", lwd=2, xlab = "Time", main = name[i], ylim = range(0, E_local_matrix[,i]+E_total[,i]), ylab ="daily EIR", col = "darkblue")
  lines(time[,1]/365+FirstYear, E_total[,i], type = "l", lwd=2,col="red")}
dev.off()

