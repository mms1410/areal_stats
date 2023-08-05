# Bundeslaender
#[1] Schleswig-Holstein     Hamburg                Niedersachsen          Bremen                 Nordrhein-Westfalen   
#[6] Hessen                 Rheinland-Pfalz        Baden-Württemberg      Bayern                 Saarland              
#[11] Berlin                 Brandenburg            Mecklenburg-Vorpommern Sachsen                Sachsen-Anhalt        
#[16] Thüringen      
   
# No Link
# SK Weiden i.d.OPf. SK Coburg


clusters_2020_gcv[, unlist(n_cluster)] %>% table()

cluster_table <- function(clusters) {
  #'
  #'
  #'
  clusters[, members := paste0(members, sep = ", ")]
}


get_cluster_table_smry <- function(cluster_table, year, ic) {
  #'
  #'
  #'
  #'
  q = quantile(cluster_table[, unlist(n_cluster)])
  cbind(
    year = year,
    ic = ic,
    zones = nrow(cluster_table),
    median_areas_per_zone = cluster_table[, unlist(n_cluster)] %>% median() %>% round(2),
    avg_areas_per_zone = cluster_table[, unlist(n_cluster)] %>% mean() %>% round(2)
  )
}

library(xtable)

data.table(rbind(
  get_cluster_table_smry(get_cluster_table(result_graphseg_2020, ic = "aic"), "2020", "AIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2020, ic = "bic"), "2020", "BIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2020, ic = "gcv"), "2020", "GCV"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2021, ic = "aic"), "2021", "AIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2021, ic = "bic"), "2021", "BIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2021, ic = "gcv"), "2021", "GCV"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2022, ic = "aic"), "2022", "AIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2022, ic = "bic"), "2022", "BIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_2022, ic = "gcv"), "2022", "GCV")
)) %>% xtable()

data.table(rbind(
  get_cluster_table_smry(get_cluster_table(result_graphseg_total, ic = "aic"), "", "AIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_total, ic = "bic"), "", "BIC"),
  get_cluster_table_smry(get_cluster_table(result_graphseg_total, ic = "gcv"), "", "GCV")
)) %>% xtable()




plot_cluser <- function(cluster, signal, gg_title = "", y_lab = "", x_lab = "Signal") {
  #'
  #' Plot segmentation
  #'
  #'
  assertDataTable(cluster)
  assert(all(c("cluster_name", "signal", "members", "n_cluster") %in% colnames(cluster)))
  assertNumeric(signal)
  assertNamed(signal)
  
  districts <- unlist(cluster[,members])
  assert(all(districts %in% names(signal)))
  
  if( gg_title == "") {
    gg_title <- paste0("Histogramm ", cluster[, cluster_name], "\n", unlist(cluster[, n_cluster]), " districts")
  }
  
  dtbl <- data.table(cbind(district = districts, signal = signal[districts])) 
  
  ggplot(dtbl) +
    geom_boxplot(aes(y = as.numeric(signal))) +
    theme(axis.text.x = element_blank()) +
    geom_hline(aes(yintercept = as.numeric(cluster[, signal])), color = "red", linetype = "dashed") +
    ylab("") +
    xlab("") +
    ggtitle(gg_title)
  
}


get_signal_smry <- function(signal){
  #'
  #'
  #'
  
  cbind(min = min(signal),
        q2 = summary(signal)[2],
        median = summary(signal)[3],
        mean = summary(signal)[4],
        q3 = summary(signal)[5],
        max = max(signal),
        sd = sd(signal),
        v = sd(signal) / mean(signal))
}

data.table(rbind(
  get_signal_smry(signal_2020),
  get_signal_smry(signal_2021),
  get_signal_smry(signal_2022)
)) %>%  round(3) %>% xtable()



get_signal_smry(signal_total) %>% round(3) %>% xtable()


library(ggpmisc) 

plot_table <- function(signal) {
  #'
  #'
  #'
  #'
  tbl <- result_2021_aic %>% unique() %>% round(2) %>% table() %>% data.frame()
  colnames(tbl) <- c("Underlying Signal", "Frequency")
  ggplot() +
    annotate(geom = "table",
             x = 1, y = 1, size = 9.5,
             label = list(tbl)) +
    theme_void()
}

stack_cluster_tables <- function(result_graphseg, lambda) {
  #'
  #' Stack 
  #'
  #'
  assertList(result_graphseg)
  assertNumeric(lambda)
  assert(length(lambda) == nrow(result_graphseg$result))
  
  aic_table <- get_ic_cluster_table(result_graphseg, "aic", lambda)
  bic_table <- get_ic_cluster_table(result_graphseg, "bic", lambda)
  gcv_table <- get_ic_cluster_table(result_graphseg, "gcv", lambda)
  
  dtbl <- (rbindlist(list(
    aic_table[, group := "aic"],
    bic_table[, group := "bic"],
    gcv_table[, group := "gcv"]
  )))
  dtbl
}

print(get_moran_table(signals, signal_names, lweights) %>% xtable(), math.style.exponents = TRUE)
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



pdf(file.path(path_assets_folder, "plots", "moran-scatter.pdf"))
par(mfrow = c(1,3))
moran.plot(x = signal_2020, listw = lweights, ylab = "Spatial Signal", xlab = "Spatial Signal", main = "2020")
moran.plot(x = signal_2021, listw = lweights, ylab = "Spatial Signal", xlab = "Spatial Signal", main = "2021")
moran.plot(x = signal_2022, listw = lweights, ylab = "Spatial Signal", xlab = "Spatial Signal", main = "2022")
knitr::plot_crop(file.path(path_assets_folder,"plots" ,"moran-scatter.pdf"))

