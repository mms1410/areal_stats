library(this.path)
library(ggplot2)
library(magrittr)
library(sf)
library(data.table)
#data.table::update.dev.pkg()
#===============================================================================
path_r_folder <- this.dir()
path_project_root <- dirname(r_folder)
path_data_folder <- file.path(project_root, "data")
theme_set(theme_bw())
theme_update(text = element_text(family = "Bookman"))
#===============================================================================
data <- readRDS(file.path(data_folder, "data.RDS"))
geom_mapping <- readRDS(file.path(path_data_folder, "geom_mapping.RDS"))
land <- "Nordrhein-Westfalen"
#===============================================================================
# plot bundesland
## TODO: centroid coords label by 'kreise'
data[bundesland == land , .(name_rki = unique(name_rki), bundesland)][geom_mapping, on = "name_rki", nomatch =0][ ,
      .(name_rki, geometry,centroid = lapply(geometry, st_centroid))][,
      .(name_rki,
        geometry,
        coords = lapply(centroid, st_coordinates))] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = name_rki))

# plot monthly mean
# TODO: top n arrow/label
data[bundesland == land, .(mean_monthly_occupancy = mean(occupancy, na.rm = TRUE), date), by = .(format(date, "%m"), name_rki)]%>% 
  ggplot(aes(x = format(date, "%m"), y = mean_monthly_occupancy, group = name_rki, color = name_rki)) +
  geom_point() +
  geom_line()

# plot spatial data monthly
## TODO: one col to one month only
data[bundesland == land, .(monthly_cases = sum(cases_tot, na.rm = TRUE), year = format(date, "%Y")), by = .(name_rki, month = format(date, "%Y-%m"))][
  geom_mapping, on = "name_rki", nomatch = 0] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = monthly_cases)) +
  facet_wrap(~month)

# plot data yearly
data[bundesland == land, .(yearly_cases = sum(cases_tot, na.rm = TRUE)), by = .(name_rki, year = format(date, "%Y"))][
  geom_mapping, on = "name_rki", nomatch = 0] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = yearly_cases)) +
  facet_wrap(~year)
#===============================================================================
library(igraph)
library(spdep)
## TODO: name_rki instead of rownumber
adjacency <- data[bundesland == land, .(name_rki = unique(name_rki))][geom_mapping, on = "name_rki", nomatch = 0]
adjacency <- adjacency %>% st_as_sf() %>% poly2nb()
graph <- igraph::graph_from_adj_list(adjacency)

library(graphseg)
gamma <- data[bundesland == land & (year(date) == 2022),
              .(yearly_cases = sum(cases_tot, na.rm = TRUE)),
              by = .(name_rki, year = format(date, "%Y"))][ , yearly_cases]

graphseg::agraph(gamma = gamma, graph = graph)

traceback()
