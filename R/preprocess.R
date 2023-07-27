library(this.path)
library(sf)
library(data.table)
#data.table::update.dev.pkg()
#===============================================================================
path_r_folder <- this.dir()
path_project_root <- dirname(path_r_folder)
path_data_folder <- file.path(path_project_root, "data")
#===============================================================================
data <- readRDS(file.path(path_data_folder, "final_data.RDS"))
class(data) <- c("data.table", "data.frame", "sf")
## remove districts with no connection ("SK Coburg" & "SK Weiden i.d.OPf.")
data <- data[!(name_rki %in% c("SK Coburg", "SK Weiden i.d.OPf."))]
geom_mapping <- data[, .(name_rki = unique(name_rki))]
geom_mapping <- geom_mapping[data[, .(name_rki, geometry)], on = "name_rki", nomatch = NULL]
geom_mapping <- unique(geom_mapping, by = "name_rki")

saveRDS(geom_mapping, file = file.path(path_data_folder, "geom_mapping.RDS"))

data[, geometry := NULL]
saveRDS(data, file = file.path(path_data_folder, "data.RDS"))
