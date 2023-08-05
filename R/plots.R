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
#===============================================================================
# plot germany
data[, .(name_rki = unique(name_rki), bundesland)][geom_mapping, on = "name_rki", nomatch =0] %>% 
  unique(by = "name_rki") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf() +
  ggtitle("NUTS-3 districs Germany")
gg_save("germany_nuts_3")

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
              y_lab = "Spatial Signal", gg_title = "Regularization path 2020")
gg_save("penalization-path-2020")

ppath_2021 <- penalize_plot(result_graphseg_2021, lambda = lambda,
              district_names = data_2021$name_rki,
              y_lab = "Spatial Signal", gg_title = "Regularization path 2021")
gg_save("penalization-path-2021")

ppath_2022 <- penalize_plot(result_graphseg_2022, lambda = lambda,
              district_names = data_2022$name_rki,
              y_lab = "Spatial Signal", gg_title = "Regularization path 2022")
gg_save("penalization-path-2022")
ppath_2022_ic<- penalize_plot(result_graphseg_2022, lambda = lambda,
                            district_names = data_2022$name_rki,
                            y_lab = "Signal", gg_title = "",
                            plot_min_ic = TRUE)
gg_save("penalization-path-2022-ic")



ppath_2020 <- ppath_2020 + labs(caption = element_text("(a)")) + theme(plot.caption = element_text(hjust = 0.5))
ppath_2021 <- ppath_2021 + labs(caption = element_text("(b)")) + theme(plot.caption = element_text(hjust = 0.5))
ppath_2022 <- ppath_2022 + labs(caption = element_text("(c)")) + theme(plot.caption = element_text(hjust = 0.5))


ppaths <- ppath_2020  + ppath_2021 + ppath_2022
gg_save("penalization-paths")

ppath_total <- penalize_plot(result_graphseg_total, lambda = lambda,
                             district_names = data_2020$name_rki,
                             y_lab = "Signal", gg_title = "")
gg_save("penalization-path-total")

#===============================================================================
## 2020
segmentation_2020_min <- comparison_plot(geom_mapping, 
                       list(
                         aic = result_2020_aic,
                         bic = result_2020_bic,
                         gcv = result_2020_gcv
                       ),
                       gg_title = "Segmentation 2020")
gg_save("segmentation-2020-min")


## 2021
segmentation_2021_min <- comparison_plot(geom_mapping, 
                       list(
                         aic = result_2021_aic,
                         bic = result_2021_bic,
                         gcv = result_2021_gcv
                       ),
                       gg_title = "Segmentation 2021")
gg_save("segmentation-2021-min")

## 2022
segmentation_2022_min <- comparison_plot(geom_mapping, 
                       list(
                         aic = result_2022_aic,
                         bic = result_2022_bic,
                         gcv = result_2022_gcv
                       ),
                       gg_title = "Segmentation 2022")
gg_save("segmentation-2022-min")

## total
segmentation_total_min <- comparison_plot(geom_mapping,
                list(
                  aic = result_total_aic,
                  bic = result_total_bic,
                  gcv = result_total_gcv
                ),
                gg_title = "")
gg_save("segmentation-total-min")


#+ theme(plot.title = element_text(hjust = 0.5)))
(segmentation_2020_min + ggtitle("") + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))) /
  (segmentation_2021_min + ggtitle("") + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))) / 
  (segmentation_2022_min + ggtitle("") + theme(plot.margin=grid::unit(c(0,0,0,0), "mm")))
gg_save("segmentations-all-years")

(segmentation_2020_min + ggtitle("")) %>%  add_custom_caption("(a)")

#===============================================================================
hist_2020_plot <- data_2020[, .(signal = yearly_cases / mean_pop_year, name_rki)][
  geom_mapping, on = "name_rki", nomatch = NULL] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_histogram(aes(x = signal)) +
  ggtitle("Histogram spatial signal in 2020") +
  ylab("")
gg_save("hist-signal-2020")

hist_2021_plot <- data_2021[, .(signal = yearly_cases / mean_pop_year, name_rki)][
  geom_mapping, on = "name_rki", nomatch = NULL] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_histogram(aes(x = signal)) +
  ggtitle("Histogram spatial signal in 2021") +
  ylab("")
gg_save("hist-signal-2021")

hist_2022_plot <- data_2022[, .(signal = yearly_cases / mean_pop_year, name_rki)][
  geom_mapping, on = "name_rki", nomatch = NULL] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_histogram(aes(x = signal)) +
  ggtitle("Histogram spatial signal in 2022") +
  ylab("")
gg_save("hist-signal-2022")
#===============================================================================
descriptive_signal(signal_2020, 2020, gg_title = "")
gg_save("descriptive-signal-2020")

descriptive_signal(signal_2021, 2021, "")
gg_save("descriptive-signal-2021")

descriptive_signal(signal_2022, 2022, "")
gg_save("descriptive-signal-2022")
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
cluster_size_ic_plot(result_graphseg_2020, "aic", lambda)
gg_save("cluster_plot-aic-2020")
cluster_size_ic_plot(result_graphseg_2020, "bic", lambda)
gg_save("cluster_plot-bic-2020")
cluster_size_ic_plot(result_graphseg_2020, "gcv", lambda)
gg_save("cluster_plot-gcv-2020")

cluster_size_ic_plot(result_graphseg_2021, "aic", lambda)
gg_save("cluster_plot-aic-2021")
cluster_size_ic_plot(result_graphseg_2021, "bic", lambda)
gg_save("cluster_plot-bic-2021")
cluster_size_ic_plot(result_graphseg_2021, "gcv", lambda)
gg_save("cluster_plot-gcv-2021")

cluster_size_ic_plot(result_graphseg_2022, "aic", lambda)
gg_save("cluster_plot-aic-2022")
cluster_size_ic_plot(result_graphseg_2022, "bic", lambda)
gg_save("cluster_plot-bic-2022")
cluster_size_ic_plot(result_graphseg_2022, "gcv", lambda)
gg_save("cluster_plot-gcv-2022")

cluster_size_ic_plot(result_graphseg_total, "aic", lambda)
gg_save("cluster_plot-aic-total")
cluster_size_ic_plot(result_graphseg_total, "bic", lambda)
gg_save("cluster_plot-bic-total")
cluster_size_ic_plot(result_graphseg_total, "gcv", lambda)
gg_save("cluster_plot-gcv-total")
#===============================================================================
hist_result_2022 <- histogram_plot(unique(result_2022_aic), "AIC") %>% add_custom_caption("(a)") +
  histogram_plot(unique(result_2022_bic), "BIC") %>% add_custom_caption("(b)") +
  histogram_plot(unique(result_2022_gcv), "GCV") %>% add_custom_caption("(c)")
gg_save("underlying_signal-2022")

hist_result_2021 <- histogram_plot(unique(result_2021_aic), "AIC") %>% add_custom_caption("(a)") +
  histogram_plot(unique(result_2021_bic), "BIC") %>% add_custom_caption("(b)") +
  histogram_plot(unique(result_2021_gcv), "GCV") %>% add_custom_caption("(c)")
gg_save("underlying_signal-2021")

hist_result_2020 <- histogram_plot(unique(result_2020_aic), "AIC") %>% add_custom_caption("(a)") +
  histogram_plot(unique(result_2020_bic), "BIC") %>% add_custom_caption("(b)") +
  histogram_plot(unique(result_2020_gcv), "GCV") %>% add_custom_caption("(c)")
gg_save("underlying_signal-2020")