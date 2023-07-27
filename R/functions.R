library(ggplot2)
library(patchwork)
library(latex2exp)
library(checkmate)
#===============================================================================
# Graphics
gg_save <- function(filename, save_folder = file.path("assets", "plots"), device = "pdf") {
  #'
  #' Save last invoked ggplot object.
  #'
  #' @param filename:
  #' @param save_folder:
  #' @device:
  #'
  assertString(path_project_root)
  assertString(filename)
  
  path_save_folder <- file.path(path_project_root, save_folder)
  if(!dir.exists(path_save_folder)) {
    dir.create(path_save_folder, recursive = TRUE)
  }
  
  filename <- paste0(filename, ".pdf")
  ggsave(filename = file.path(path_save_folder,filename), device = device)
}

penalize_plot <- function(result_graphseg, lambda, district_names,
                          y_lab = "",
                          x_lab =TeX(r"($log_{10}\lambda$)"),
                          gg_title = "",
                          plot_min_ic = FALSE) {
  #'
  #' Create (gg-)plot of penalization-path.
  #'
  #' @param result_graphseg:
  #' @param lambda:
  #' @param district_names:
  #' @param y_lab:
  #' @param x_lab:
  #' @param gg_title:
  #'
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
  #' @param geom_mapping:
  #' @param results_list:
  #' @param gg_title:
  #' @param legend_title:
  #' @param plot_legend:
  #'
  #'
  assertDataTable(geom_mapping)
  assertList(results_list)
  
  
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
  #' @param dtbl:
  #' @param signal_col:
  #' @param gg_title:
  #' @param y_lab:
  #' @param x_lab:
  #'
  assert(signal_col %in% colnames(dtbl))
  
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
  #' @param ic_value:
  #' @param result_graphseg:
  #' @param ic:
  #' @param lambda:
  #'
  assertNumeric(ic_value)
  assertList(result_graphseg)
  assertChoice(ic, c("aic", "bic", "gcv"))
  
  idx <- which(result_graphseg$aic == ic_value)
  lambda[idx]
}

get_cluster_table <- function(result_graphseg, ic, ic_value, mapping = NULL) {
  #'
  #' 
  #'
  #' @param result_graphseg:
  #' @param ic:
  #' @param ic_value:
  #' @param mapping:
  #' 
  assertList(result_graphseg)
  assertChoice(ic, c("aic", "bic", "gcv"))
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
    list(members = members)
  }
  
  result <- as.data.table(cbind(
    cluster_name = paste0("Cluster_", seq_along(piecewise_constant_signals)),
    signal = as.numeric(unique(piecewise_constant_signals))))
  
  result[, members := lapply(signal, set_members)]
  result[, n_cluster := lapply(members, function(x)length(unlist(x)))]
  
  result[]
}

get_n_clusters <- function(result_graphseg, ic, ic_value){
  #'
  #' Create vector containing #members of each cluster
  #'
  #' @param result_graphseg:
  #' @param ic:
  #' @param ic_value:
  #' 
  unlist(get_cluster_table(result_graphseg, ic, ic_value)[, n_cluster])
}
 
get_ic_cluster_table <- function(result_graphseg, ic, lambda) {
  #'
  #'
  #' @param result_graphseg:
  #' @param ic:
  #' @param lambda:
  #'
  #'
  assertChoice(ic, c("aic", "bic", "gcv"))
  assertList(result_graphseg)
  
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

stack_cluster_tables <- function(result_graphseg, lambda) {
  #'
  #'
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

plot_lambda_vs_ic <- function(cluster_table, gg_title = "",
                            x_lab =TeX(r"($log_{10}\lambda$)"),
                            y_lab = "", group_selection = NULL) {
  #'
  #'
  #' @param result_graphseg:
  #' @param gg_title:
  #' @param x_lab:
  #' @param y_lab:
  #'
  
  assertDataTable(cluster_table)
  assert(all(c("ic_value", "n_clusters", "mean_size", "median_size", "q1", "q3", "lambda", "group") %in% colnames(cluster_table)))
  
  ggplot(cluster_table) +
    geom_line(aes(x = lambda, y = ic_value, group = group, color = group)) +
    scale_x_log10() +
    xlab(x_lab)
}
