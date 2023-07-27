# Bundeslaender
#[1] Schleswig-Holstein     Hamburg                Niedersachsen          Bremen                 Nordrhein-Westfalen   
#[6] Hessen                 Rheinland-Pfalz        Baden-Württemberg      Bayern                 Saarland              
#[11] Berlin                 Brandenburg            Mecklenburg-Vorpommern Sachsen                Sachsen-Anhalt        
#[16] Thüringen      
   
# No Link
# SK Weiden i.d.OPf. SK Coburg


# nothing printed in console (== no return value ?)
clusters <- get_cluster_table(result_graphseg_2022, ic = "aic",
                              ic_value = min(result_graphseg_2022$aic),
                              mapping = geom_mapping$name_rki)

clusters

f <- function(value) {
  length(unlist(value[1]))
}
lapply(clusters$members, f)

clusters[, lapply(members, function(x)length(unlist(x)))]
