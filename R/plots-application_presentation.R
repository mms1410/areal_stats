signal_by_year_map <- data[, .(signal_year = sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE)),
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
  #theme(legend.position = "none") +
  facet_wrap(~year) +
  theme(panel.spacing = unit(3, "lines"))

signal_by_year_hist <- data[, .(signal_year = sum(cases_tot, na.rm = TRUE) / mean(pop_base, na.rm = TRUE)),
                            by = .(name_rki, year = format(date, "%Y"))] %>% 
  unique(by = c("name_rki", "year")) %>% 
  ggplot() +
  geom_histogram(aes(x = signal_year)) +
  facet_wrap(~year) +
  ylab("") +
  xlab("signal")

p1 <- signal_by_year_map / signal_by_year_hist 
p1 + plot_layout(widths = c(5, 2), heights = unit(c(6, 2), c('cm', 'null')))
gg_save("")

