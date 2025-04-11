## ----message = FALSE, warning = FALSE--------------------------------------------------------------
library(dplyr)
library(ramp.xds)
library(ramp.library)
library(ramp.work)

library(igraph)
library(tidygraph)
library(ggplot2)
library(ggraph)

tprdata <- read.csv(file='C:/Users/Admin/Documents/ramp.uganda/data/pfpr_ts_district_from_hf_pixel_agg.csv')
sorted_data <- tprdata[order(tprdata$district_name,tprdata$period),] #arrange the data in chronical order of period and district name
id_name = sorted_data$district_name
dist_name = unique(sorted_data$district_name)

## ----get_one_district--------------------------------------------------------------------------------------------------
get_one_district = function(i){
  # Pull PfPR values
  pfpr <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(pfpr))$pfpr
  # Pull and convert month year to date
  t_start =2016
  period <- as.vector(sorted_data %>% filter(id_name == dist_name[i]) %>% select(period))$period
  year <- as.numeric(substr(period, 1, 4))  # Extract first 4 characters
  month <- as.numeric(substr(period, 5, 6))  # Extract last 2 characters
  fac_time_month = (year+month/12) # TIME IN months
  fac_time_day = (fac_time_month-t_start)*365 -15 # time in days
  list(pr = pfpr, times = fac_time_day, name = dist_name[i])
}

## ----data_list-------------------------------------------------------------------------------------
data_list = list()
for(i in 1:146)
  data_list[[i]] <- get_one_district(i)


## ----fit_all_district--------------------------------------------------------------------------------------------------
directory <- "C:/Users/Admin/Box/Models"
# List all RDS files in the directory
rds_files <- list.files(path = directory, pattern = "\\.rds$", full.names = TRUE)
eir_files <- grep("_eir_sis\\.rds$", rds_files,value=TRUE)
# Initialize an empty list to store the contents
fitted_mod <- list()
# Loop through each RDS file and load it into the list
for (i in 1:146) {
  fitted_mod[[i]] <- readRDS(eir_files[i])
}


## --------------------------------------------------------------------------------------------------
FirstYear = 2016

profile = function(i, model_list, data_list){
  mod <- model_list[[i]]
  data <- data_list[[i]]
  #tt <- seq(-90, ceiling(max(data$times)/365)*365-90+(365*5), length.out=200)
  tt <- seq(-90, ceiling(max(data$times)/365)*365-90, length.out=200)
  mod <- xds_solve_cohort(mod, times=tt)
  get_XH(mod) -> vals
  pars <- get_Xpars(mod, 1)

#------ District total EIR______________________________________
  R0 <- mean((vals$eir * pars$b) * (1 - Xo$rho) / (pars$r + Xo$xi))
  ioD <- compute_IoD_F(mod$EIRpar$F_season)
  phase <- mod$EIRpar$season_par$phase
  Eir_tot <- vals$eir
  ave_annual_eir <- mean(vals$eir * 365)
 return(list(
     Eir_tot = Eir_tot, time = vals$time, name = data$name
     )
   )
}

## --------------------------------------------------------------------------------------------------
matrix_adj<- as.matrix(read.csv(file='C:/Users/Admin/Documents/ramp.uganda/data/District_sorted_Adjaceny_matrix.csv'))[, -c(1)]
# Convert adjacency matrix to an igraph object
network_graph <- graph_from_adjacency_matrix(matrix_adj, mode = "directed")
# Convert graph to a tidygraph object
graph_tbl <- as_tbl_graph(network_graph)
# Plot using ggraph
pdf("../Images/district_connectivity_network.pdf", width = 8, height = 6)
ggraph(graph_tbl, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.09), edge_width = 0.3, color = "darkred", show.legend = FALSE) +
  geom_node_point(size = 2.5, color = "blue") +
  geom_node_text(aes(label = name), size = 1.8, hjust = 0, nudge_x = 0.1,repel = TRUE) +
  theme_void() +
  ggtitle("District Connectivity Network")
dev.off()

## _________________Compute local EIR___________________________________________________________________________________________________
# Exract the EIR total
Eir_dat <- list()
for (i in 1:146) {
  eird <- profile(i, fitted_mod, data_list)
  Eir_dat[[i]] <- eird
  }
matrix_adj <- apply(matrix_adj, c(1, 2), as.numeric)
# Adjust psi_adj
psi_adj <- t(apply(matrix_adj, 1, function(row) {
  row_sum <- sum(row)  # Calculate the sum of the row
  if (row_sum > 0) {  # Avoid division by zero
    row[row == 1] <- 1 / row_sum  # Replace ones with 1 / sum(row)
  }
  return(row)
}))

# ----------------Compute the travel and local eir--------------------------------------------------------------------------
delta <- 0.05 # proportion of time spent travel
t_trav_diag <- diag(delta, 146) # diag matrix of time spent travel
inv_diag <- solve(diag((1-delta),146)) #inverse of diag matrix of local time
tot_Eir <- sapply(as.matrix(Eir_dat), function(x) x$Eir_tot) #Extract total EIR from data
time <- sapply(as.matrix(Eir_dat), function(x) x$time) #Extract time
name <- sapply(as.matrix(Eir_dat), function(x) x$name) #Extract district name
E_travel <- list()
E_total <- list()
E_local <- list()
for (i in (1:length(time[,1]))){
E_travel[[i]] <- t_trav_diag%*%psi_adj%*% tot_Eir[i,]
E_total[[i]] <- tot_Eir[i,]}

E_total <- do.call(rbind, E_total)
E_travel <- do.call(rbind,E_travel)
E_travel_matrix <- t(matrix(E_travel,146,200))

for (i in (1:length(time[,1]))){
E_local[[i]] <- inv_diag %*% (E_total[i,] -(E_travel_matrix[i,] %*% E_total[i,]))}

E_local1 <- do.call(rbind,E_local)
E_local_matrix <- t(matrix(E_local1,146,200))

#----------------plot eir local and total------------------------------------------------------------------------------
pdf("../Images/eir_ts.pdf", width = 9, height = 7)
for (i in 1:146){
 plot(time[,1]/365+FirstYear, E_local_matrix[,i], type = "l", lwd=2, xlab = "Time", main = name[i], ylim = range(0, E_local_matrix[,i]+E_total[,i]), ylab ="daily EIR", col = "darkblue")
  lines(time[,1]/365+FirstYear, E_total[,i], type = "l", lwd=2,col="red")}
dev.off()

