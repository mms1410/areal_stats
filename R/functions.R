
#===============================================================================
# Graphics
add_custom_caption <- function(gg_object, gg_caption, h_just = 0.5) {
  #'
  #' Add caption to ggplot object
  #'
  #' @param gg_object: ggplot object
  #' @param gg_caption: sting with caption
  #' @param h_just: numeric value for horizontal adjustment
  #' 
  assert("ggplot" %in% class(gg_object))
  assertString(gg_caption)
  assertNumeric(h_just)
  
  gg_object +
    labs(caption = element_text(gg_caption))+ theme(plot.caption = element_text(hjust = h_just))  
}

gg_save <- function(filename, save_folder = file.path("assets", "plots"), device = "pdf", crop = TRUE) {
  #'
  #' Save last invoked ggplot object.
  #'
  #' @param filename: string with filename (no pdf ending)
  #' @param save_folder: string of folder location
  #' @device: "pdf"
  #'
  assertString(path_project_root)
  assertString(filename)
  
  path_save_folder <- file.path(path_project_root, save_folder)
  if(!dir.exists(path_save_folder)) {
    dir.create(path_save_folder, recursive = TRUE)
  }
  
  filename <- paste0(filename, ".pdf")
  ggsave(filename = file.path(path_save_folder,filename), device = device)
  if (crop) {
    knitr::plot_crop(file.path(path_save_folder,filename))
  }
}

penalize_plot <- function(result_graphseg, lambda, district_names,
                          y_lab = "",
                          x_lab =TeX(r"($log_{10}\lambda$)"),
                          gg_title = "",
                          plot_min_ic = FALSE) {
  #'
  #' Create plot of regularization path
  #'
  #' @param result_graphseg: list containing results from agraph
  #' @param lambda: numeric vector of penalization parameters
  #' @param district_names: character vector of district names
  #' @param y_lab: string for y-axis title
  #' @param x_lab: string for x-axis title
  #' @param gg_title: string for plot title
  #'
  assertList(result_graphseg)
  assertNumeric(lambda)
  assert(length(district_names) == dim(result_graphseg$result)[2])
  assert(length(lambda) == dim(result_graphseg$result)[1])
  
  
  results_dtbl <- as.data.frame(result_graphseg$result)
  
  results_dtbl <- as.data.table(results_dtbl)
  colnames(results_dtbl) <- district_names
  results_dtbl$lambda <- lambda
  
  min_aic_lambda <- lambda[which.min(result_graphseg$aic)]
  min_bic_lambda <- lambda[which.min(result_graphseg$bic)]
  
  results_dtbl <- melt(results_dtbl, id.vars = "lambda")
  
  p <- ggplot() +
    geom_line(data = results_dtbl, aes(lambda, value, group = variable, color = variable), show.legend = FALSE) +
    scale_x_log10() +
    labs(title = gg_title, x = x_lab, y = y_lab)
  
  if (plot_min_ic) {
    p <- p + 
      geom_vline(xintercept = min_aic_lambda, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = min_bic_lambda, color = "red", linetype = "dashed") +
      geom_text(aes(x=min_bic_lambda, label= "min BIC", y= quantile(log10(lambda), 0.85)), colour="red", angle=0, hjust = -0.2) +
      geom_text(aes(x=min_aic_lambda, label= "min AIC", y= quantile(log10(lambda), 0.75)), colour="blue", angle=0, hjust = -0.2)
  }
  p
}

piecewise_constant_signal_plot <- function(geom_mapping, result, gg_title = "",
                                           legend_title = "",
                                           as_factor = FALSE,
                                           plot_ticks = FALSE) {
  #'
  #'
  #'  @param geom_mapping:
  #'  @param result:
  #'  @gg_title:
  #'  @legend_title:
  #'
  #'
  assertDataTable(geom_mapping)
  assertNumeric(result)
  assertString(gg_title)
  assertString(legend_title)
  assert(length(result) == nrow(geom_mapping))
  
  if (as_factor) {
    result <- as.factor(result)
  }
  
  p <- geom_mapping[, .(name_rki, geometry, result)] %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(aes(fill = result)) +
    ggtitle(gg_title) +
    guides(fill=guide_legend(title=legend_title))
  
  if (as_factor) {
    p <- p + theme(legend.position = "none")
  }
  if (!plot_ticks) {
    p <- p + theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())
  }
  p
}

comparison_plot <- function(geom_mapping, results_list, gg_title = "",
                            as_factor = TRUE, legend_title = "Signal",
                            plot_legend = FALSE, plot_ticks = TRUE) {
  #'
  #' @param geom_mapping: data.table with mapping of geom to rki_name
  #' @param results_list:
  #' @param gg_title:
  #' @param legend_title:
  #' @param plot_legend:
  #'
  #'
  assertDataTable(geom_mapping)
  assertList(results_list)
  assertString(gg_title)
  assertLogical(as_factor)
  assertString(legend_title)
  assertLogical(plot_legend)
  assertLogical(plot_ticks)
  
  results <- do.call(cbind, results_list)
  assert(nrow(results) == nrow(geom_mapping))
  
  dtbl <-cbind(geom_mapping, results)
  if (as_factor) {
    dtbl[, `:=`(aic = as.factor(aic), bic = as.factor(bic), gcv = as.factor(gcv))]
  }
  
  n_clusters <- c(aic = paste0("AIC \n ", results_list$aic %>% unique %>% length(), " Cluster(s)"),
                  bic = paste0("BIC \n ", results_list$bic %>% unique %>% length(), " Cluster(s)"),
                  gcv = paste0("GCV \n", results_list$gcv %>% unique %>% length(), " Cluster(s)")
                  )
  
  new_name <- colnames(dtbl)
  new_name[which(new_name == "aic")] <- n_clusters["aic"]
  new_name[which(new_name == "bic")] <- n_clusters["bic"]
  new_name[which(new_name == "gcv")] <- n_clusters["gcv"]
  colnames(dtbl) <- new_name
  
  p <- melt(dtbl, id.vars = c("name_rki", "geometry")) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(aes(fill = value)) +
    facet_wrap(~variable) +
    ggtitle(gg_title) +
    guides(fill=guide_legend(title=legend_title))
  
  if (!plot_legend & !plot_ticks) {
    p <- p + theme(legend.position = "none",
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())
  }
  if (!plot_legend & plot_ticks) {
    p <- p + theme(legend.position = "none")
  }
  if (plot_legend & !plot_ticks) {
    p <- p + theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())
  }
  p
}

hist_plot <- function(dtbl, signal_col, gg_title = "", y_lab = "", x_lab = "signal") {
  #'
  #' Plot histogram.
  #' 
  #' @param dtbl: data.table containing necessary information
  #' @param signal_col: string of column name for signal column
  #' @param gg_title: string for plot title
  #' @param y_lab: string for y-lab title
  #' @param x_lab: string for y-lab title
  #'
  assertDataTable(dtbl)
  assert(signal_col %in% colnames(dtbl))
  assertString(gg_title)
  assertString(y_lab)
  assertString(x_lab)
  
  dtbl %>% 
    ggplot() +
    geom_histogram(aes(x = dtbl[[signal_col]])) +
    ggtitle(gg_title) +
    ylab(y_lab) +result_graphseg_wave_1
    xlab(x_lab)
}

get_ic_lambda <- function(ic_value, result_graphseg, ic, lambda) {
  #'
  #' Return corresponding lambda value for given value of information criterion.
  #'
  #' @param ic_value: numeric value of some ic value
  #' @param result_graphseg: list of results from agraph
  #' @param ic: string of either "aic", "bic" or "gcv"
  #' @param lambda: numeric vector of penalization parameters
  #'
  assertNumeric(ic_value)
  assertList(result_graphseg)
  assertChoice(ic, c("aic", "bic", "gcv"))
  assertNumeric(lambda)
  
  idx <- which(result_graphseg$aic == ic_value)
  lambda[idx]
}

get_cluster_table <- function(result_graphseg, ic, ic_value = NULL, mapping = NULL) {
  #'
  #' Create a datatable with clustering results
  #'
  #' @param result_graphseg: list of results from agraph
  #' @param ic: string of either "aic", "bic" or "gcv"
  #' @param ic_value: numeric value of some ic value
  #' @param mapping: character vector where the ith element contain the name for the ith signal
  #' 
  assertList(result_graphseg)
  assertChoice(ic, c("aic", "bic", "gcv"))
  if (is.null(ic_value)) {
    ic_value <- min(result_graphseg[[ic]])
  }
  assertNumeric(ic_value)
  assert(ic_value %in% result_graphseg[[ic]])
  
  idx <- which(result_graphseg[[ic]] == ic_value)
  
  piecewise_constant_signals <- result_graphseg$result[idx, ]
  piecewise_constant_signals <- unique(piecewise_constant_signals)
  
  set_members <- function(signal) {
    members <- which(result_graphseg$result[idx, ] == signal)
    if (!is.null(mapping)) {
      members <- mapping[members]
    }
    members
  }
  
  result <- as.data.table(cbind(
    cluster_name = paste0("Cluster_", seq_along(piecewise_constant_signals)),
    signal = as.numeric(unique(piecewise_constant_signals))))
  
  result[, members := lapply(signal, set_members)]
  result[, n_cluster := lapply(members, function(x)length(unlist(x)))]
  
  result[]
}

get_moran_table <- function(signals, signal_names, lweights, digits = 3) {
  #'
  #' Create datatable summarizeng results from moran.test()
  #'
  #' @param signals: list of underlying signals
  #' @param signal_names: character vector of names for each element in signals
  #' @param lweights: listw object of adjacency weights
  #' @param digits: integer for rounding numbers
  #'
  assertList(signals)
  assertCharacter(signal_names)
  assert("listw" %in% class(lweights))
  assert(length(signal_names) == length(signals))
  
  moran_table <- data.table(name = character(0),
                            moran_i = numeric(0),
                            moran_i_std = numeric(0),
                            p_val = numeric(0),
                            expectation = numeric(0),
                            variance = numeric(0))
  for (idx in seq(signals)) {
    moran_smry <- spdep::moran.test(signals[[idx]], lweights)
    moran_table <- rbind(moran_table, 
                         data.table(name = signal_names[idx],
                                    moran_i = round(moran_smry[[3]][1], digits),
                                    moran_i_std = round(moran_smry[[1]], digits),
                                    p_val = moran_smry[[2]],
                                    expectation = round(moran_smry[[3]][2], digits),
                                    variance = round(moran_smry[[3]][3], digits)
                         ))
    
  }
  moran_table[]
}

get_n_clusters <- function(result_graphseg, ic, ic_value){
  #'
  #' Create vector containing #members of each cluster
  #'
  #' @param result_graphseg: list of agraph result
  #' @param ic: string, either "aic", "bic" or "gcv"
  #' @param ic_value: numeric value of one ic value
  #' 
  assertList(result_graphseg)
  assertChoice(ic, c("aic", "bic", "gcv"))
  assertNumeric(ic_value)
  assert(ic_value %in% result_graphseg[[ic]])
  
  unlist(get_cluster_table(result_graphseg, ic, ic_value)[, n_cluster])
}
 
get_ic_cluster_table <- function(result_graphseg, ic, lambda) {
  #'
  #' Create datatable describing results of graph segmentation.
  #'
  #' @param result_graphseg: list of agraph result
  #' @param ic: string, either "aic", "bic" or "gcv"
  #' @param lambda: numeric vector of penalzation parameters
  #'
  #'
  assertChoice(ic, c("aic", "bic", "gcv"))
  assertList(result_graphseg)
  assertNumeric(lambda)
  
  result <- data.table()
  for (idx in seq(nrow(result_graphseg$result))) {
    
    clusters <- get_n_clusters(result_graphseg, ic, result_graphseg[[ic]][idx])
    n_clusters <- length(clusters)
    mean_size <- mean(clusters)
    median_size <- median(clusters)
    q1 <- unname(quantile(clusters, 0.25))
    q3 <- unname(quantile(clusters, 0.75))
    
    result <- rbindlist(list(
      result,
      as.data.table(cbind(
        ic_value = result_graphseg[[ic]][idx],
        n_clusters = n_clusters,
        mean_size = mean_size,
        median_size = median_size,
        q1 = q1,
        q3 = q3,
        lambda = lambda[idx]
      ))
    ))
  }
  result
}

cluster_size_ic_plot <- function(result_graphseg, ic, lambda) {
  #'
  #' Plot 4 subplots of graph segmentation.
  #'
  #' @param result_graphseg: list of results from agraph
  #' @param ic: string "aic", "bic" or "gcv"
  #' @param lambda: numeric vector of penalization parameters
  #'
  assertList(result_graphseg)
  assertChoice(ic, c("aic", "bic", "gcv"))
  assertNumeric("lambda")
  
  ic_vals <- result_graphseg[[ic]]
  n_clusters <- numeric(0)
  avg_districts <- numeric(0)
  median_districts <- numeric(0)
  
  for(idx in seq(ic_vals)) {
    cluster_table <- get_cluster_table(result_graphseg,
                                       ic = ic,
                                       ic_value = ic_vals[idx])
    n_cluster <- nrow(cluster_table)
    avg_district <- cluster_table[, unlist(n_cluster)] %>%  mean()
    median_district <- cluster_table[, unlist(n_cluster)] %>%  median()
    
    n_clusters <- append(n_clusters, n_cluster)
    avg_districts <- append(avg_districts, avg_district)
    median_districts <- append(median_districts, median_district)
  }
  n_clusters
  avg_districts
  median_districts
  
  
  ic_plot <- ggplot() +
    geom_line(aes(x = lambda, y = ic_vals)) +
    scale_x_log10() +
    labs(title = toupper(ic),
         x = TeX(r"($log_{10}\lambda$)"),
         y = "",
         caption = "(a)") +
    theme(plot.caption = element_text(hjust = 0.5))
  
  n_cluster_plot <- ggplot() +
    geom_line(aes(x = lambda, y = n_clusters)) +
    scale_x_log10() +
    labs(title = "Number of areas",
         x = TeX(r"($log_{10}\lambda$)"),
         y = "",
         caption = "(b)") +
    theme(plot.caption = element_text(hjust = 0.5))
  
  avg_districts_plot <- ggplot() +
    geom_line(aes(x = lambda, y = avg_districts)) +
    scale_x_log10() +
    labs(title = "Average number of districts p. area",
         x = TeX(r"($log_{10}\lambda$)"),
         y = "",
         caption = "(c)") +
    theme(plot.caption = element_text(hjust = 0.5))
  
  median_districts_plot <- ggplot() +
    geom_line(aes(x = lambda, y = median_districts)) +
    scale_x_log10() +
    labs(title = "Median number of districts p. area",
         x = TeX(r"($log_{10}\lambda$)"),
         y = "",
         caption = "(d)") +
    theme(plot.caption = element_text(hjust = 0.5))
  
  
  (ic_plot + avg_districts_plot) / (n_cluster_plot + median_districts_plot)
  
}

signal_boxplot <- function(signal, gg_title = NULL) {
  #'
  #' Plot boxplot.
  #'
  #' @param signal: numeric vector of underlying signal
  #' @param gg_title: string for plot title.
  #' 
  assertNumeric(signal)
  assertString(gg_title)
  
  if (is.null(gg_title)) {
    gg_title = ""
  }
  ggplot() +
    geom_boxplot(aes(y = signal)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ylab("Spatial Signal") +
    ggtitle(gg_title)
}

table_plot <- function(signal, size = 3.5) {
  #'
  #' Plot frequency table.
  #'
  #' @param signal: numeric vector of underlying signal
  #' @param size: numeric scaling factor for freq. table size
  #'
  assertNumeric(signal)
  assertNumber(size)
  
  tbl <- signal %>% unique() %>% round(2) %>% table() %>% data.frame()
  colnames(tbl) <- c("Underlying Signal", "Frequency")
  ggplot() +
    annotate(geom = "table",
             x = 1, y = 1, size = size,
             label = list(tbl)) +
    theme_void()
}

histogram_plot <- function(signal, gg_title = NULL, size = 3.5, threshold = 10) {
  #'
  #' Plot histogram if number of obs > threshold, else plot freq. table.
  #'
  #' @param signal: numeric vector of underlying signal
  #' @param gg_title: string with plot title
  #' @param size: scaling factor of freq. table
  #' @param threshold: numeric number for histogram vs. freq. plot decision
  #' 
  assertNumeric(signal)
  assertString(gg_title)
  assertNumber(size)
  assertInt(threshold)
  
  signal <- unique(signal)
  if (is.null(gg_title)) {
    gg_title = ""
  }
  if (length(signal) < threshold) {
    p <- table_plot(unique(signal), size)
  } else {
    p <- ggplot() +
      geom_histogram(aes(x = signal)) +
      xlab("Underlying Signal") 
  }
  p + ggtitle(gg_title)
}

descriptive_signal <- function(signal, year, gg_title = NULL) {
  #'
  #' Plot histogram and boxplot next to each other.
  #' 
  #' @param signal: numeric vector of underlying signal
  #' @param year: numeric number for year
  #' @param gg_title: string for title
  #'
  assertNumeric(signal)
  assertNumber(year)
  assertString(gg_title)
  
  hist_plot <- ggplot() +
    geom_histogram(aes(x = signal)) +
    xlab("Spatial Signal")
  
  box_plot <- ggplot() +
    geom_boxplot(aes(y = signal)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ylab("Spatial Signal")
  
  if (is.null(gg_title)) {
    gg_title = paste0("Histogram and Boxplot of Spatial Signal for Year ", year)
  }
  hist_plot + box_plot & 
    plot_annotation(title = gg_title)
}