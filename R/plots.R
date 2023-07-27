#===============================================================================
path_r_folder <- this.dir()
path_project_root <- dirname(path_r_folder)
path_data_folder <- file.path(path_project_root, "data")
path_assets_folder <- file.path(path_project_root, "assets")
theme_set(theme_bw())
theme_update(text = element_text(family = "Bookman"))
source(file.path(path_r_folder,"functions.R"))
#===============================================================================
data <- readRDS(file.path(path_data_folder, "data.RDS"))
data[, signal := ifelse(data$cases_tot == 0, 0, (data$cases_tot / data$pop_base) * 1000)]
geom_mapping <- readRDS(file.path(path_data_folder, "geom_mapping.RDS"))
land <- "Nordrhein-Westfalen"
#===============================================================================
# plot germany
data[, .(name_rki = unique(name_rki), bundesland)][geom_mapping, on = "name_rki", nomatch =0] %>% 
  unique(by = "name_rki") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf() +
  ggtitle("NUTS-3 districs Germany")
gg_save("germany_nuts_3")

# plot monthly mean
# TODO: top n arrow/label
#data[bundesland == land, .(monthly_cases = sum(cases_tot, na.rm = TRUE, mean_pop = mean(pop_base, na.rm = TRUE)), date),
#     by = .(month = format(date, "%m"), name_rki)] %>% 
#  unique(by = c("month", "name_rki")) %>% 
#  ggplot(aes(x = month, y = mean_monthly_occupancy, group = name_rki, color = name_rki)) +
#  geom_point() +
#  geom_line()

#data[bundesland == land, .(mean_monthly_occupancy = mean(occupancy, na.rm = TRUE), date),
#     by = .(month = format(date, "%m"), year = format(date, "%Y"), name_rki)] %>% 
#  unique(by = c("month", "year", "name_rki")) %>% 
#  ggplot(aes(x = mean_monthy_occupancy))
  

# plot spatial signal monthly
## TODO: one col to one month only
data[, .(singal_monthly = (sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE)),
                           year = format(date, "%Y")), by = .(name_rki, month = format(date, "%Y-%m"))] %>% 
  unique(by = c("name_rki", "month")) %>% 
  merge(geom_mapping, by = "name_rki", all = FALSE) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = singal_monthly)) +
  guides(fill=guide_legend(title="Signal")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Spatial Signal over time") +
  facet_wrap(~month)
gg_save("monthly-signal")

tmp <- data[, .(singal_monthly = (sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE)),
         year = format(date, "%Y")), by = .(name_rki, month = format(date, "%Y-%m"))] %>% 
  unique(by = c("name_rki", "month")) %>% 
  merge(geom_mapping, by = "name_rki", all = FALSE)
tmp[month == "2021-05"] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = singal_monthly)) +
  guides(fill=guide_legend(title="Signal")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Example data Germany") +
  theme(legend.position = "none")
gg_save("spatial-signal-example")


# plot spatial signal yearly
data[, .(signal_year= (sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE))),
     by = .(name_rki, year = format(date, "%Y"))] %>% 
  unique(by = c("name_rki", "year")) %>% 
  merge(geom_mapping, by = "name_rki", all = FALSE) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = signal_year)) +
  guides(fill=guide_legend(title="Signal")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Spatial Signal over time") +
  facet_wrap(~year)
gg_save("yearly_signal")


#===============================================================================
ppath_2020 <- penalize_plot(result_graphseg_2020, lambda = lambda,
              district_names = data_2020$name_rki,
              y_lab = "Signal", gg_title = "2020")
gg_save("penalization-path-2020")

ppath_2021 <- penalize_plot(result_graphseg_2021, lambda = lambda,
              district_names = data_2021$name_rki,
              y_lab = "Signal", gg_title = "2021")
gg_save("penalization-path-2021")

ppath_2022 <- penalize_plot(result_graphseg_2022, lambda = lambda,
              district_names = data_2022$name_rki,
              y_lab = "Signal", gg_title = "2022")
gg_save("penalization-path-2022")
ppath_2022_ic<- penalize_plot(result_graphseg_2022, lambda = lambda,
                            district_names = data_2022$name_rki,
                            y_lab = "Signal", gg_title = "Regularization path 2022",
                            plot_min_ic = TRUE)
gg_save("penalization-path-2022-ic")

ppaths <- ppath_2020 + (ppath_2021 + ylab("")) + (ppath_2022+ ylab("")) + plot_annotation(
  title = "Regularization paths for each year"
)
gg_save("penalization-paths")

#===============================================================================
## 2020
segmentation_2020_min <- comparison_plot(geom_mapping, 
                       list(
                         aic = result_2020_aic,
                         bic = result_2020_bic,
                         gcv = result_2020_gcv
                       ),
                       gg_title = "Piecewise constant signal 2020")
gg_save("segmentation-2020-min")

#segmentation_2020_cmin <-comparison_plot(geom_mapping, 
#                list(
#                  aic = get_condition_min_result(result_graphseg_2020, "aic"),
#                  bic = get_condition_min_result(result_graphseg_2020, "bic"),
#                  gcv = get_condition_min_result(result_graphseg_2020, "gcv")
#                ),
#                gg_title = "Piecewise constant signal 2020 (conditional minimum)")
#gg_save("segmentation-2020-cmin")

#data_2020[, .(signal = yearly_cases / mean_pop_year, name_rki)] %>% 
#  hist_plot(signal_col = "signal",
#            gg_title = "Histogram spatial signal 2020")
#gg_save("hist-signal-2020")

## 2021
segmentation_2021_min <- comparison_plot(geom_mapping, 
                       list(
                         aic = result_2021_aic,
                         bic = result_2021_bic,
                         gcv = result_2021_gcv
                       ),
                       gg_title = "Piecewise constant signal 2021")
gg_save("segmentation-2021-min")

#segmentation_2021_cmin <- comparison_plot(geom_mapping, 
#                list(
#                  aic = get_condition_min_result(result_graphseg_2021, "aic"),
#                  bic = get_condition_min_result(result_graphseg_2021, "bic"),
#                  gcv = get_condition_min_result(result_graphseg_2021, "gcv")
#                ),
#                gg_title = "Piecewise constant signal 2021 (conditional minimum)")
#gg_save("segmentation-2022-cmin")

#data_2021[, .(signal = yearly_cases / mean_pop_year, name_rki)] %>% 
#  hist_plot(signal_col = "signal",
#            gg_title = "Histogram spatial signal 2021")
#gg_save("hist-signal-2021")

## 2022
segmentation_2022_min <- comparison_plot(geom_mapping, 
                       list(
                         aic = result_2022_aic,
                         bic = result_2022_bic,
                         gcv = result_2022_gcv
                       ),
                       gg_title = "Piecewise constant signal 2022")
gg_save("segmentation-2022-min")

#segmentation_2022_cmin <- comparison_plot(geom_mapping, 
#                list(
#                  aic = get_condition_min_result(result_graphseg_2022, "aic"),
#                  bic = get_condition_min_result(result_graphseg_2022, "bic"),
#                  gcv = get_condition_min_result(result_graphseg_2022, "gcv")
#                ),
#                gg_title = "Piecewise constant signal 2022 (conditional minimum)")
#gg_save("segmentation-2022-cmin")

#data_2022[, .(signal = yearly_cases / mean_pop_year, name_rki)] %>% 
#  hist_plot(signal_col = "signal",
#            gg_title = "Histogram spatial signal 2022")
#gg_save("hist-signal-2022")


## total
comparison_plot(geom_mapping,
                list(
                  aic = result_total_aic,
                  bic = result_total_bic,
                  gcv = result_total_gcv
                ),
                gg_title = "Piecewise constant signal aggregated")
gg_save("constant-signals-aggregated")

data_2020[, .(signal = yearly_cases / mean_pop_year, name_rki)][
  geom_mapping, on = "name_rki", nomatch = NULL] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_histogram(aes(x = signal)) +
  ggtitle("Histogram spatial signal in 2020") +
  ylab("")
gg_save("hist-signal-2020")

data_2021[, .(signal = yearly_cases / mean_pop_year, name_rki)][
  geom_mapping, on = "name_rki", nomatch = NULL] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_histogram(aes(x = signal)) +
  ggtitle("Histogram spatial signal in 2021") +
  ylab("")
gg_save("hist-signal-2021")

#===============================================================================
# Welle 1
penalize_plot(result_graphseg_wave_2, lambda = lambda,
                            district_names = data_2020$name_rki,
                            y_lab = "Signal", gg_title = "Regularization paths \n First wave (10/2020 - 20/2020)")
gg_save("penalization-path-wave-1")

segmentation_wave_1 <- comparison_plot(geom_mapping, 
                                        list(aic = result_wave_1_aic,
                                             bic = result_wave_1_bic,
                                             gcv = result_wave_1_gcv),
                                        gg_title = "Piecewise constant signal \n First wave (10/2020 - 20/2020)")
gg_save("constant-signal-wave-1")

# Welle 2
penalize_plot(result_graphseg_wave_2, lambda = lambda,
              district_names = data_2020$name_rki,
              y_lab = "Signal", gg_title = "Regularization paths \n First wave (10/2020 - 20/2020)")

segmentation_wave_2 <- comparison_plot(geom_mapping,
                                        list(aic = result_wave_2_aic,
                                             bic = result_wave_2_bic,
                                             gcv = result_wave_2_gcv),
                                        gg_title = "Piecewise constant signal \n Sencond wave (40/2020 - 8/2021)")
gg_save("constant-signal-wave-2")
#===============================================================================
signal_by_year_map <- data[, .(signal_year = sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE)),
                           by = .(name_rki, year = format(date, "%Y"))] %>% 
  unique(by = c("name_rki", "year")) %>% 
  merge(geom_mapping, by = "name_rki", all = FALSE) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = signal_year)) +
  guides(fill=guide_legend(title="#cases / pop.")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  facet_wrap(~year) +
  theme(panel.spacing = unit(3, "lines"))

signal_by_year_hist <- data[, .(signal_year = sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE)),
                            by = .(name_rki, year = format(date, "%Y"))] %>% 
  unique(by = c("name_rki", "year")) %>% 
  ggplot() +
  geom_histogram(aes(x = signal_year)) +
  facet_wrap(~year) +
  ylab("") +
  xlab("#cases / pop.")

p1 <- signal_by_year_map / signal_by_year_hist 
p1 + plot_layout(widths = c(5, 2), heights = unit(c(6, 2), c('cm', 'null'))) +
  plot_annotation(title = "Aggregated corona cases by year (adjusted by population)")
gg_save("signal-description-1")
#===============================================================================
p2 <- (ppath_2022_ic + ggtitle("")) + (segmentation_2022_min + ggtitle(""))
p2 & plot_annotation(title = "Regularization path and result 2022")
gg_save("path-result-2022")

p3.1 <- piecewise_constant_signal_plot(geom_mapping, result = result_2022_aic, as_factor = TRUE)
p3.2 <- piecewise_constant_signal_plot(geom_mapping, result = result_2022_aic, as_factor = FALSE) + guides(fill=guide_legend("Signal"))
p3.1 + p3.2 & plot_annotation(title = paste0("Result for year 2022 (AIC) \n ", length(unique(result_2022_aic)), "Cluster(s)"))
gg_save("result-2022-factornumeric")


title_1 <- paste0("Segmentation 2022 (AIC) \n", length(unique(result_2022_aic)), " Cluster(s)")
title_2 <- paste0("Segmentation 2022 (BIC) \n", length(unique(result_2022_bic)), " Cluster(s)")
p3.3 <- piecewise_constant_signal_plot(geom_mapping, result = result_2022_bic, as_factor = TRUE)
p4 <- ((p3.1 + ggtitle(title_1)) + (p3.3 + ggtitle(title_2))) + ppath_2022_ic
gg_save("result-2022-regpath")

