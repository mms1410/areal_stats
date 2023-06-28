dtbl <- data[ , .(name_rki = unique(name_rki), geometry = unique(geometry))][ ,
 .(name_rki, geometry,centroid = lapply(geometry, st_centroid))][,
 .(name_rki,
 geometry,
 x_coord = lapply(centroid, function(x){st_coordinates(x)[[1]]}),
 y_coord = lapply(centroid, function(x){unlist(st_coordinates(x)[[2]])}))]

   