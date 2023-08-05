library(this.path)
library(lubridate)
library(ggplot2)
library(gghighlight)
library(magrittr)
library(sf)
library(data.table)
library(ggpmisc)
library(ggplot2)
library(patchwork)
library(latex2exp)
library(checkmate)
#data.table::update.dev.pkg()
#===============================================================================
path_r_folder <- this.dir()
path_project_root <- dirname(path_r_folder)
path_data_folder <- file.path(path_project_root, "data")
theme_set(theme_bw())
theme_update(text = element_text(family = "Bookman"))
#===============================================================================
data <- readRDS(file.path(path_data_folder, "data.RDS"))
geom_mapping <- readRDS(file.path(path_data_folder, "geom_mapping.RDS"))
# Kalenderwoche 10/2020 bis 20/2020
wave_1 <- seq(ymd("2020-01-01") + weeks(10),
               ymd("2020-01-01") + weeks(20),
               by = "days")
#  Kalenderwoche 40/2020 bis 8/2021
wave_2 <- seq(ymd("2020-01-01") + weeks(40),
               ymd("2021-01-01") + weeks(8),
               by = "days")
#===============================================================================
library(igraph)
library(spdep)
## TODO: name_rki instead of rownumber
ladjacency <- data[year(date) == 2022, .(name_rki = unique(name_rki))][geom_mapping, on = "name_rki", nomatch = 0]

ladjacency <- ladjacency %>% st_as_sf() %>% spdep::poly2nb(row.names = "name_rki")

graph <- igraph::graph_from_adj_list(ladjacency, mode = "all")

lweights <- nb2listw(ladjacency, style = "W", zero.policy = T)
#===============================================================================
## TODO: select two cols from dtbl and transform into named vector
library(graphseg)
library(Matrix)
lambda <- 10 ^ seq(-3, 3, length = 50)  # define sequence of penalties
source(file.path(path_r_folder,"graphseg_spatial.R"))

data_2022 <- data[ , .(yearly_cases = sum(cases_tot, na.rm = TRUE),
                       mean_pop_year = mean(pop_base, na.rm = TRUE)),
     by = .(year = format(date, "%Y"),
            name_rki)][year == "2022"]

data_2021 <- data[ , .(yearly_cases = sum(cases_tot, na.rm = TRUE),
                       mean_pop_year = mean(pop_base, na.rm = TRUE)),
                   by = .(year = format(date, "%Y"),
                          name_rki)][year == "2021"]

data_2020 <- data[ , .(yearly_cases = sum(cases_tot, na.rm = TRUE),
                       mean_pop_year = mean(pop_base, na.rm = TRUE)),
                   by = .(year = format(date, "%Y"),
                          name_rki)][year == "2020"]
data_total <- data[ , .(cummulative_cases = sum(cases_tot, na.rm = TRUE),
                        mean_pop = mean(pop_base, na.rm = TRUE)),
                    by = .(name_rki)]

data_wave_1 <- data[date %in% wave_1, .(cummulative_cases = sum(cases_tot,na.rm = TRUE),
                                        mean_pop = mean(pop_base, na.rm = TRUE)),
                     by = .(name_rki)]

data_wave_2 <- data[date %in% wave_2, .(cummulative_cases = sum(cases_tot, na.rm = TRUE),
                                        mean_pop = mean(pop_base, na.rm = TRUE)),
                     by = .(name_rki)]
#===============================================================================
signal_2022 <- setNames(data_2022$yearly_cases / data_2022$mean_pop_year, data_2022$name_rki)
signal_2021 <- setNames(data_2021$yearly_cases / data_2021$mean_pop_year, data_2021$name_rki)
signal_2020 <- setNames(data_2020$yearly_cases / data_2020$mean_pop_year, data_2020$name_rki)
signal_total <- data_total$cummulative_cases / data_total$mean_pop
signal_wave_1 <- data_wave_1$cummulative_cases / data_wave_1$mean_pop
signal_wave_2 <- data_wave_2$cummulative_cases / data_wave_2$mean_pop


result_graphseg_2020 <- agraph(gamma = signal_2020, graph = graph, lambda)
result_graphseg_2021 <- agraph(gamma = signal_2021, graph = graph, lambda)
result_graphseg_2022 <- agraph(gamma = signal_2022, graph = graph, lambda)
result_graphseg_total <- agraph(gamma = signal_total, graph = graph, lambda)
result_graphseg_wave_1 <- agraph(gamma = signal_wave_1, graph = graph, lambda)
result_graphseg_wave_2 <- agraph(gamma = signal_wave_2, graph = graph, lambda)

# BIC
result_2020_bic <- result_graphseg_2020$result[which.min(result_graphseg_2020$bic), ]
result_2021_bic <- result_graphseg_2021$result[which.min(result_graphseg_2021$bic), ]
result_2022_bic <- result_graphseg_2022$result[which.min(result_graphseg_2022$bic), ]
result_total_bic <- result_graphseg_total$result[which.min(result_graphseg_total$bic), ]
result_wave_1_bic <-result_graphseg_wave_1$result[which.min(result_graphseg_wave_1$bic), ]
result_wave_2_bic <-result_graphseg_wave_2$result[which.min(result_graphseg_wave_2$bic), ]

# AIC
result_2020_aic <- result_graphseg_2020$result[which.min(result_graphseg_2020$aic), ]
result_2021_aic <- result_graphseg_2021$result[which.min(result_graphseg_2021$aic), ]
result_2022_aic <- result_graphseg_2022$result[which.min(result_graphseg_2022$aic), ]
result_total_aic <- result_graphseg_total$result[which.min(result_graphseg_total$aic), ]
result_wave_1_aic <-result_graphseg_wave_1$result[which.min(result_graphseg_wave_1$aic), ]
result_wave_2_aic <-result_graphseg_wave_2$result[which.min(result_graphseg_wave_2$aic), ]

# GCV
result_2020_gcv <- result_graphseg_2020$result[which.min(result_graphseg_2020$gcv), ]
result_2021_gcv <- result_graphseg_2021$result[which.min(result_graphseg_2021$gcv), ]
result_2022_gcv <- result_graphseg_2022$result[which.min(result_graphseg_2022$gcv), ]
result_total_gcv <- result_graphseg_total$result[which.min(result_graphseg_total$gcv), ]
result_wave_1_gcv <-result_graphseg_wave_1$result[which.min(result_graphseg_wave_1$gcv), ]
result_wave_2_gcv <-result_graphseg_wave_2$result[which.min(result_graphseg_wave_2$gcv), ]

#===============================================================================
source(file.path(path_r_folder, "functions.R"))
# 2022
clusters_2022_aic <- get_cluster_table(result_graphseg_2022, ic = "aic",
                                       ic_value = min(result_graphseg_2022$aic),
                                       mapping = geom_mapping$name_rki)
clusters_2022_bic <- get_cluster_table(result_graphseg_2022, ic = "bic",
                                       ic_value = min(result_graphseg_2022$bic),
                                       mapping = geom_mapping$name_rki)
clusters_2022_gcv <- get_cluster_table(result_graphseg_2022, ic = "gcv",
                                       ic_value = min(result_graphseg_2022$gcv),
                                       mapping = geom_mapping$name_rki)

# 2021
clusters_2021_aic <- get_cluster_table(result_graphseg_2021, ic = "aic",
                                       ic_value = min(result_graphseg_2021$aic),
                                       mapping = geom_mapping$name_rki)
clusters_2021_bic <- get_cluster_table(result_graphseg_2021, ic = "bic",
                                       ic_value = min(result_graphseg_2021$bic),
                                       mapping = geom_mapping$name_rki)
clusters_2021_gcv <- get_cluster_table(result_graphseg_2021, ic = "gcv",
                                       ic_value = min(result_graphseg_2021$gcv),
                                       mapping = geom_mapping$name_rki)

# 2020
clusters_2020_aic <- get_cluster_table(result_graphseg_2020, ic = "aic",
                                       ic_value = min(result_graphseg_2020$aic),
                                       mapping = geom_mapping$name_rki)
clusters_2020_bic <- get_cluster_table(result_graphseg_2020, ic = "bic",
                                       ic_value = min(result_graphseg_2020$bic),
                                       mapping = geom_mapping$name_rki)
clusters_2020_gcv <- get_cluster_table(result_graphseg_2020, ic = "gcv",
                                       ic_value = min(result_graphseg_2020$gcv),
                                       mapping = geom_mapping$name_rki)
#===============================================================================