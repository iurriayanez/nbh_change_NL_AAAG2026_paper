#------------------------------------------------------------------------------#
# Name: 2_1_nbh_cluster_model_selection.R
# Goal: Cluster model selection: estimation and plot indices
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_for_clustering_metaggl_all.rds
# Output databases: data/nbh/data_scaled_[std_method]_for_clustering_metaggl_all.rds
#                   data/nbh/data_pca_[std_method]_for_clustering_metaggl_all.rds
#                   data/nbh/data_clust_assign_[std_method]_orig_pca_mbkmeans_k_7_10_metaggl_all.rds
#                   data/nbh/data_clust_assign_[std_method]_orig_somkmeans_k[nclust]_metaggl_all.rds
#------------------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# 0. Setup ----
#-----------------------------------------------------------------------#
source(file.path(path_code, "functions.R"), echo = F)
#-----------------------------------------------------------------------#
## 0.1 Libraries ----
#-----------------------------------------------------------------------#

  library(cluster)
  library(ClusterR)
  library(factoextra)
  library(mclust)
  library(microbenchmark)
  library(ggplot2)
  library(dplyr)
  library(data.table)
  library(readr)

#-----------------------------------------------------------------------#
## 0.2 Seed ----
#-----------------------------------------------------------------------#
set.seed(1409)

#-----------------------------------------------------------------------#
# 1. Import data set ----
#-----------------------------------------------------------------------#
rds_file <- paste0("data_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df <- read_rds(file.path(path_nbh,rds_file))

#-----------------------------------------------------------------------#
# 2. Pre-processing ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 2.1 Scaling ----
#-----------------------------------------------------------------------#
cols_scaling <- c(
  # Ethnic background
  c(grep("^PERC_ETHNL", names(df), value = TRUE)),
  c(grep("^PERC_ETHEUR", names(df), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_3_TO_6", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df), value = TRUE)),
  c(grep("^PERC_HH_MIDINC", names(df), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df), value = TRUE)),
  c(grep("^PERC_HH_PRIVRENTAL", names(df), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df), value = TRUE)) 
)

df_scale = copy(df)
if (std_method == "year_std") {
  # By year at national level 
  df_scale[, (cols_scaling) := lapply(.SD, function(x) ((x-mean(x))/(sd(x)))), by = YEAR, .SDcols = cols_scaling ]
} else if (std_method == "full_std") {
  # Full sample national level 
  df_scale[, (cols_scaling) := lapply(.SD, function(x) ((x-mean(x))/(sd(x)))), .SDcols = cols_scaling ]
} else if (std_method == "urbreg_year_std") {
  # Full sample national level 
  df_scale[, (cols_scaling) := lapply(.SD, function(x) ((x-mean(x))/(sd(x)))), by = .(URBREG, YEAR), .SDcols = cols_scaling ]
} else {
  stop("Define standardisation level for z-score (std_method)")
}

#-----------------------------------------------------------------------#
## 2.2 Select cluster variables ---- 
#-----------------------------------------------------------------------#

clust_cols <- c(
  # Mig. background
  c(grep("^PERC_ETHEUR", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_scale), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_scale), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_scale), value = TRUE)),
  # Tenure
  c(grep("^PERC_HH_HOMEOWNER", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_scale), value = TRUE))
)

df_clust <- df_scale[, clust_cols, with = F]

#-----------------------------------------------------------------------#
## 2.3 Save data used in clustering ----
#-----------------------------------------------------------------------#

save_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
write_rds(df_scale, file = file.path(path_nbh,save_file))

#-----------------------------------------------------------------------#
# 3. PCA + KMeans ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 3.1 PCA ----
#-----------------------------------------------------------------------#

res.pca <- FactoMineR::PCA(df_clust, scale.unit = FALSE, graph = FALSE, ncp = 30)
pca_dim <- length(res.pca$eig[res.pca$eig[,1]>1,1])
res.pca <- FactoMineR::PCA(df_clust, scale.unit = FALSE, graph = FALSE, ncp = pca_dim)
df_pca = as.data.frame(res.pca$ind$coord)

# Save data used in clustering (PCA)
save_file <- paste0("data_pca_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
write_rds(df_pca, file = file.path(path_nbh,save_file))

# Plot loadings 
list_cols_num <- list(
  # Ethnic background
  c(grep("^PERC_ETHEUR", names(df_clust), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_clust), value = TRUE)),
  # Age groups
  c(grep("^PERC_AGE_GROUP_1", names(df_clust), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_clust), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_clust), value = TRUE)),
  # Socioeconomic variables
  c(grep("^PERC_HH_LOWINC", names(df_clust), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_clust), value = TRUE)),
  # Tenure
  c(grep("^PERC_HH_HOMEOWNER", names(df_clust), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_clust), value = TRUE))
)

cols_names <- c(
  # Ethnic background
  'PERC_ETHEUR',
  'PERC_ETHNONEUR',
  # Age groups
  'PERC_AGE_GROUP_1',
  'PERC_AGE_GROUP_2',
  'PERC_AGE_GROUP_7',
  # Household income
  'PERC_HH_LOWINC',
  'PERC_HH_HIGHINC',
  # Tenure
  'PERC_HH_HOMEOWNER',
  'PERC_HH_SUBRENTAL'
)

# Wide to long
df_pca_coord_long <- pca_data_long(res.pca, list_cols_num, cols_names)

# Plot each PCA dimension
for (i in 1:max(df_pca_coord_long$dim)) {
  df_labels <- df_pca_coord_long %>%
    filter(dim == i & SCALE == 6) %>%
    group_by(variable) %>%
    slice(which.max(variable))
  
  # Plot
  plot_loadings <- ggplot(
    df_pca_coord_long[dim == i,],
    aes(
      x = SCALE, 
      y = value, 
      color = variable
    )
  ) +
    geom_line(size= 0.7)
  
  # Labels
  plot_loadings <- plot_loadings +
    ggrepel::geom_text_repel(
      data = df_labels,
      aes(label = variable),
      nudge_x = 1,
      direction = 'y',
      hjust = 0,
      segment.color = NA
    ) +
    scale_x_continuous(breaks = 0:6, labels = scale_labs_save) +
    ylab("Loading") +
    scale_color_manual(
      values = c(colors_dim_b,
                 colors_dim_p,
                 colors_dim_o,
                 colors_dim_g)
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = 'black')
    )
  
  # Save
  ggsave(
    file.path(path_results, "figures", "PCA", paste0("PCA_", std_method, "_metaggl_", met_aggl_labs_save,"_loadings_dim", i, ".jpg")),
    plot = plot_loadings,
    width = 10,
    height = 6,
    units = "in"
  )
}

rm(res.pca)
gc()

#-----------------------------------------------------------------------#
## 3.2 Cluster with Mini Batch ----
#-----------------------------------------------------------------------#

# List to store results
pca_minibkmeans <- list()

for (k in 2:30) {
  # Model
  time_minibatchkmeans <- system.time({
    batch_size <- min(100000, nrow(df_pca))
    minibatch_kmeans_result <- MiniBatchKmeans(df_pca, cluster = k, batch_size = batch_size, num_init = 100, max_iters = 1000000)
    minibatch_kmeans_clusters <- predict_MBatchKMeans(df_pca, minibatch_kmeans_result$centroids)
  })["elapsed"]
  print(paste("PCA_MiniBatchKMeans", k, "done. Time to fit the model:",time_minibatchkmeans, "seconds."))
  
  #Metrics
  time_metric <- system.time({
    batch_kmeans_metric <- clust_metrics(df_pca, minibatch_kmeans_clusters, sample_size = 10000, label = "MiniBatchKMeans")
    pca_minibkmeans[[as.character(k)]] <- batch_kmeans_metric
    # Save centroids for Clustergram
    pca_minibkmeans[[as.character(k)]][["centroids"]] <- minibatch_kmeans_result$centroids
  })["elapsed"]
  print(paste("PCA_MiniBatchKMeans", k, "metrics done. Time:",time_metric, "seconds."))
  
  # Save results
  write_rds(pca_minibkmeans, file.path(path_nbh, paste0("clust_metrics_", std_method, "_pca_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")), compress = "gz"); gc()
}
  
#-----------------------------------------------------------------------#
## 3.3 Metrics ----
#-----------------------------------------------------------------------#
pca_minibkmeans <- read_rds(file.path(path_nbh, paste0("clust_metrics_", std_method, "_pca_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")))
  
# Silhouette
sorted_metric <- sort(sapply(pca_minibkmeans, function(x) x$Silhouette), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Silhouette Score: %.4f\n", name, sorted_metric[name]))
}
  
# CH
sorted_metric <- sort(sapply(pca_minibkmeans, function(x) x$Calinski_Harabasz), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Calinski Harabasz: %.4f\n", name, sorted_metric[name]))
}
  
# DB
sorted_metric <- sort(sapply(pca_minibkmeans, function(x) x$Davies_Bouldin), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Davies Bouldin: %.4f\n", name, sorted_metric[name]))
}

# Plot
pca_minikmeans_sil_plot <- plot_clust_metric(pca_minibkmeans, "Silhouette", "Silhouette")
pca_minikmeans_ch_plot <- plot_clust_metric(pca_minibkmeans, "Calinski_Harabasz", "Calinski Harabasz")
pca_minikmeans_db_plot <- plot_clust_metric(pca_minibkmeans, "Davies_Bouldin", "Davies Bouldin")

png(
  file.path(path_results, 
            "figures", 
            "cluster_metrics", 
            paste0(
              "cluster_metrics_", 
              std_method, 
              "_pca_mbkmeans_",
              met_aggl_labs_save,
              ".png"
            )
  ),
  width = 5000,
  height = 1000,
  res = 300
)

gridExtra::grid.arrange(grobs = list(pca_minikmeans_sil_plot,
                                     pca_minikmeans_ch_plot,
                                     pca_minikmeans_db_plot),
                        nrow = 3)

dev.off()

#-----------------------------------------------------------------------#
## 3.4 Plot clustergram ----
#-----------------------------------------------------------------------#

cluster_info <- list()
for(k in 2:30) {
  cluster_info[[as.character(k)]]$centers <- pca_minibkmeans[[as.character(k)]]$centroids
  cluster_info[[as.character(k)]]$clusters <- pca_minibkmeans[[as.character(k)]]$cluster_assign
}

for (factor_sel in 1:2) {
  png(file.path(path_results, "figures", "clustergram", paste0("clustergram_", std_method, "_PCA_minibkmeans_PCA", factor_sel, "_", met_aggl_labs_save,".png")), 1500, 900, pointsize = 15)
  clustergram(
    Data = df_pca, 
    cluster_info = cluster_info,
    k.range = 2:20,
    pca.weight = F,
    col.select = factor_sel,
    ylab = paste0("Mean of the clusters (PCA Factor: ", factor_sel, ")")
  )
  dev.off()
}


#-----------------------------------------------------------------------#
## 3.5 Estimate potentially optimal cluster number ----
#-----------------------------------------------------------------------#

# range of optimal number of clusters
init_k <- 7
end_k <- 10

# reload data (in case rows change order)
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file.path(path_nbh,rds_file))
df_clust <- df_scale[, cols_clust, with = F]

rds_file <- paste0("data_pca_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_pca <- read_rds(file.path(path_nbh,rds_file))

# estimation 
op_pca_minibkmeans <- list()
for (k in init_k:end_k) {
  
  # Model
  time_minibatchkmeans <- system.time({
    batch_size <- min(100000, nrow(df_pca))
    minibatch_kmeans_result <- MiniBatchKmeans(df_pca, cluster = k, batch_size = batch_size, num_init = 1000, max_iters = 1000000)
    minibatch_kmeans_clusters <- predict_MBatchKMeans(df_pca, minibatch_kmeans_result$centroids)
  })["elapsed"]
  print(paste("PCA_MiniBatchKMeans", k, "done. Time to fit the model:",time_minibatchkmeans, "seconds."))
  
  #Metrics
  time_metric <- system.time({
    batch_kmeans_metric <- clust_metrics(df_pca, minibatch_kmeans_clusters, sample_size = 10000, label = "MiniBatchKMeans")
    op_pca_minibkmeans[[as.character(k)]] <- batch_kmeans_metric
    # Save centroids for Clustergram
    op_pca_minibkmeans[[as.character(k)]][["centroids"]] <- minibatch_kmeans_result$centroids
  })["elapsed"]
  print(paste("PCA_MiniBatchKMeans", k, "metrics done. Time:",time_metric, "seconds."))
  
  # Save results
  write_rds(op_pca_minibkmeans, file.path(path_nbh, paste0("clust_metrics_", std_method, "_optimal_pca_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")), compress = "gz")
}

# Save data set
cols_id <- c(paste0('VIERKANT', grid_size,'M'), 'YEAR')
df_save_clust <- df_scale[, cols_id, with = F]

for (k in init_k:end_k) {
  col_name <- paste0("CLUSTERk",k)
  df_save_clust[, col_name] <- as.factor(op_pca_minibkmeans[[as.character(k)]]$cluster_assign)
}

save_file <- paste0("data_clust_assign_", std_method, "_orig_pca_mbkmeans_k_",init_k,"_", end_k,"_metaggl_", met_aggl_labs_save, ".rds")
write_rds(df_save_clust, file = file.path(path_nbh,save_file))
rm(df_save_clust)
gc()

#-----------------------------------------------------------------------#
# 4. Mini Batch KMeans ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 4.1 Cluster ---- 
#-----------------------------------------------------------------------#

minibkmeans <- list()      
for (k in 2:30) {

  # Model
  time_minibatchkmeans <- system.time({
    batch_size <- min(100000, nrow(df_clust))
    minibatch_kmeans_result <- MiniBatchKmeans(df_clust, cluster = k, batch_size = batch_size, num_init = 100, max_iters = 1000000)
    minibatch_kmeans_clusters <- predict_MBatchKMeans(df_clust, minibatch_kmeans_result$centroids)
  })["elapsed"]
  print(paste("MiniBatchKMeans", k, "done. Time to fit the model:",time_minibatchkmeans, "seconds."))

  #Metrics
  time_metric <- system.time({
    batch_kmeans_metric <- clust_metrics(df_clust, minibatch_kmeans_clusters, sample_size = 10000, label = "MiniBatchKMeans")
    minibkmeans[[as.character(k)]] <- batch_kmeans_metric
    # Save centroids for Clustergram
    minibkmeans[[as.character(k)]][["centroids"]] <- minibatch_kmeans_result$centroids
  })["elapsed"]
  print(paste("MiniBatchKMeans", k, "metrics done. Time:",time_metric, "seconds."))

  # Save results
  write_rds(minibkmeans, file.path(path_nbh, paste0("clust_metrics_", std_method, "_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")), compress = "gz"); gc()
}

#-----------------------------------------------------------------------#
## 4.2 Metrics  ----
#-----------------------------------------------------------------------#

minibkmeans <- read_rds(file.path(path_nbh, paste0("clust_metrics_", std_method, "_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")))
      
# Silhouette
sorted_metric <- sort(sapply(minibkmeans, function(x) x$Silhouette), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Silhouette Score: %.4f\n", name, sorted_metric[name]))
}

# CH
sorted_metric <- sort(sapply(minibkmeans, function(x) x$Calinski_Harabasz), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Calinski Harabasz: %.4f\n", name, sorted_metric[name]))
}

# DB
sorted_metric <- sort(sapply(minibkmeans, function(x) x$Davies_Bouldin), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Davies Bouldin: %.4f\n", name, sorted_metric[name]))
}

# Plot
minikmeans_sil_plot <- plot_clust_metric(minibkmeans, "Silhouette", "Silhouette")
minikmeans_ch_plot <- plot_clust_metric(minibkmeans, "Calinski_Harabasz", "Calinski Harabasz")
minikmeans_db_plot <- plot_clust_metric(minibkmeans, "Davies_Bouldin", "Davies Bouldin")

png(
  file.path(path_results, 
            "figures", 
            "cluster_metrics", 
            paste0(
              "cluster_metrics_", 
              std_method, 
              "_mbkmeans_",
              met_aggl_labs_save,
              ".png"
            )
  ),
  width = 5000,
  height = 1000,
  res = 300
)

gridExtra::grid.arrange(grobs = list(minikmeans_sil_plot,
                                     minikmeans_ch_plot,
                                     minikmeans_db_plot),
                        nrow = 3)

dev.off()

#-----------------------------------------------------------------------#
## 4.3 Plot clustergram ----
#-----------------------------------------------------------------------#

cluster_info <- list()
for(k in 2:30) {
  cluster_info[[as.character(k)]]$centers <- minibkmeans[[as.character(k)]]$centroids
  cluster_info[[as.character(k)]]$clusters <- minibkmeans[[as.character(k)]]$cluster_assign
}

for (factor_sel in 1:2) {
  png(file.path(path_results, "figures", "clustergram", paste0("clustergram_", std_method, "_minibkmeans_PCA", factor_sel, "_", met_aggl_labs_save,".png")),1500,900,pointsize = 15)
  clustergram(
    Data = df_clust, 
    cluster_info = cluster_info,
    k.range = 2:20, 
    pca.loading = factor_sel,
    ylab = paste0("PCA weighted mean of the clusters (PCA Factor: ", factor_sel, ")")
  )
  dev.off()
}

#-----------------------------------------------------------------------#
## 4.4 Plot indices of PCA+KMeans and KMeans together ----
#-----------------------------------------------------------------------#

# Set same min- max
set_min_max <- function(p, metric_name) {
  p + ylim(
    min(
      sapply(
        minibkmeans, function(x) x[[metric_name]]
      ),
      sapply(
        pca_minibkmeans, function(x) x[[metric_name]]
      )
    ),
    max(
      sapply(
        minibkmeans, function(x) x[[metric_name]]
      ),
      sapply(
        pca_minibkmeans, function(x) x[[metric_name]]
      )
    )
  )
}

minikmeans_sil_plot <- set_min_max(minikmeans_sil_plot, "Silhouette") 
minikmeans_ch_plot <- set_min_max(minikmeans_ch_plot, "Calinski_Harabasz") 
minikmeans_db_plot <- set_min_max(minikmeans_db_plot, "Davies_Bouldin") 
pca_minikmeans_sil_plot <- set_min_max(pca_minikmeans_sil_plot, "Silhouette") 
pca_minikmeans_ch_plot <- set_min_max(pca_minikmeans_ch_plot, "Calinski_Harabasz") 
pca_minikmeans_db_plot <- set_min_max(pca_minikmeans_db_plot, "Davies_Bouldin") 

# Set titles and subfigures      
title1 <- grid::textGrob("PCA + Mini Batch K-Means", gp = grid::gpar(fonsize = 16,
                                                                     fontface = "bold"))
col1 <- gridExtra::arrangeGrob(pca_minikmeans_sil_plot,
                               pca_minikmeans_db_plot,
                               pca_minikmeans_ch_plot, ncol = 1)
title2 <-  grid::textGrob("Mini Batch K-Means", gp = grid::gpar(fonsize = 16,
                                                                fontface = "bold"))
col2 <- gridExtra::arrangeGrob(minikmeans_sil_plot,
                               minikmeans_db_plot,
                               minikmeans_ch_plot, ncol = 1) 

png(file.path(path_results, "figures", "cluster_metrics", paste0("cluster_metrics_", std_method, "_pca_mbkmeans_mbkmeans_", met_aggl_labs_save,".png")),1500,900,pointsize = 15)
gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    title1, title2, ncol = 2
  ),
  gridExtra::arrangeGrob(
    col1, col2, ncol = 2
  ),
  nrow = 2, 
  heights = c(0.1,1)
)
dev.off()

#-----------------------------------------------------------------------#
## 4.5 Estimate potentially optimal cluster number ----
#-----------------------------------------------------------------------#

# range of optimal number of clusters
init_k <- 7
end_k <- 10

# reload data (in case rows change order)
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file.path(path_nbh,rds_file))
df_clust <- df_scale[, cols_clust, with = F]

# estimation 
op_minibkmeans <- list()

for (k in init_k:end_k) {
  
  # Model
  time_minibatchkmeans <- system.time({
    batch_size <- min(100000, nrow(df_clust))
    minibatch_kmeans_result <- MiniBatchKmeans(df_clust, cluster = k, batch_size = batch_size, num_init = 1000, max_iters = 1000000)
    minibatch_kmeans_clusters <- predict_MBatchKMeans(df_clust, minibatch_kmeans_result$centroids)
  })["elapsed"]
  print(paste("MiniBatchKMeans", k, "done. Time to fit the model:",time_minibatchkmeans, "seconds."))
  
  #Metrics
  time_metric <- system.time({
    batch_kmeans_metric <- clust_metrics(df_clust, minibatch_kmeans_clusters, sample_size = 10000, label = "MiniBatchKMeans")
    op_minibkmeans[[as.character(k)]] <- batch_kmeans_metric
    # Save centroids for Clustergram
    op_minibkmeans[[as.character(k)]][["centroids"]] <- minibatch_kmeans_result$centroids
  })["elapsed"]
  print(paste("MiniBatchKMeans", k, "metrics done. Time:",time_metric, "seconds."))
  
  # Save results
  write_rds(op_minibkmeans, file.path(path_nbh, paste0("clust_metrics_", std_method, "_optimal_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")), compress = "gz")
}

# Save data set
cols_id <- c(
  paste0('VIERKANT', grid_size,'M'),
  'YEAR'
)
df_save_clust <- df_scale[, cols_id, with = F]

for (k in init_k:end_k) {
  col_name <- paste0("CLUSTERk",k)
  df_save_clust[, col_name] <- as.factor(op_minibkmeans[[as.character(k)]]$cluster_assign)
}

save_file <- paste0("data_clust_assign_", std_method, "_orig_mbkmeans_k_",init_k,"_", end_k,"_metaggl_", met_aggl_labs_save, ".rds")
write_rds(df_save_clust, file = file.path(path_nbh,save_file))
rm(df_save_clust)
gc()


#-----------------------------------------------------------------------#
# 5. SOM ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 5.1 Import clustering data ----
#-----------------------------------------------------------------------#
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file = file.path(path_nbh,rds_file))

clust_cols <- c(
  # Mig. background
  c(grep("^PERC_ETHEUR", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_scale), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_scale), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_scale), value = TRUE)),
  # Tenure
  c(grep("^PERC_HH_HOMEOWNER", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_scale), value = TRUE))
)
df_clust <- df_scale[, clust_cols, with = FALSE]

#-----------------------------------------------------------------------#
## 5.2 Set parameters for hypertuning ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
### 5.2.1 Grid size ----
#-----------------------------------------------------------------------#

grid_side1 <- ceiling(sqrt(sqrt(nrow(df_clust))*5))
grid_side2 <- ceiling(grid_side1 * 1.25)
grid_side3 <- ceiling(grid_side1 / 2)

#-----------------------------------------------------------------------#
### 5.2.2 Distance functions ----
#-----------------------------------------------------------------------#
distances <- c("euclidean", "sumofsquares","manhattan")

#-----------------------------------------------------------------------#
## 5.3 Hypertuning distance functions ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
### 5.3.1 Calculate SOM and metrics ----
#-----------------------------------------------------------------------#

som_res = list()

for (grid_side in c(grid_side1)) {
  
  grid = kohonen::somgrid(xdim = grid_side, ydim = grid_side, "hexagonal", neighbourhood.fct = "gaussian")
  
  for (dist in distances) {
    
    # Fit Model
    time_som <- system.time({
      res.som = kohonen::som(as.matrix(df_clust), grid, rlen = 50, mode = "pbatch", dist.fcts = dist)
    })["elapsed"]
    print(paste("SOM",grid_side, dist, "done. Time to fit the model:",time_som, "seconds."))
    
    # Quality metrics
    time_qual_som <- system.time({
      res.som.qual <- aweSOM::somQuality(res.som,as.matrix(df_clust))
    })["elapsed"] 
    print(paste("Qual. SOM",grid_side, dist, "done. Time to fit the model:",time_qual_som, "seconds."))
    
    # Save in list
    som_res[[paste("SOM",grid_side, dist, sep = "_")]] <- res.som
    som_res[[paste("SOM_qual",grid_side, dist, sep = "_")]] <- res.som.qual
    
    # Save results as R object in local
    write_rds(som_res, file.path(path_nbh, paste0("SOM_metrics_dist_fcts_", std_method, "_metaggl_", met_aggl_labs_save, "_gridside_", grid_side1, ".rds")), compress = "gz")
  }
}

#-----------------------------------------------------------------------#
### 5.3.2 Evaluate metrics ----
#-----------------------------------------------------------------------#
som_res_dist <- read_rds(file.path(path_nbh,  paste0("SOM_metrics_dist_fcts_", std_method, "_metaggl_", met_aggl_labs_save, "_gridside_", grid_side1, ".rds")))

# Quant error
sorted_metric <- sort(sapply(som_res_dist[startsWith(names(som_res_dist), "SOM_qual_")], function(x) x$err.quant), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Quant. Error: %.4f\n", name, sorted_metric[name]))
}

# Topo error
sorted_metric <- sort(sapply(som_res_dist[startsWith(names(som_res_dist), "SOM_qual_")], function(x) x$err.topo), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Topo. Error: %.4f\n", name, sorted_metric[name]))
}

# Explained variance
sorted_metric <- sort(sapply(som_res_dist[startsWith(names(som_res_dist), "SOM_qual_")], function(x) x$err.varratio), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Expl. Variance: %.4f\n", name, sorted_metric[name]))
}

# Prefered distance function: euclidean (slightly better to sum of squares and more widely used)

#-----------------------------------------------------------------------#
## 5.4 Hypertuning grid side ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
### 5.4.1 Calculate SOM and metrics ----
#-----------------------------------------------------------------------#

som_res = list()
# Grid side 1 already in previous iteration

for (grid_side in c(grid_side2, grid_side3)) {
  
  grid = kohonen::somgrid(xdim = grid_side, ydim = grid_side, "hexagonal", neighbourhood.fct = "gaussian")
  
  for (dist in c("euclidean")) {
    # Fit Model
    time_som <- system.time({
      res.som = kohonen::som(as.matrix(df_clust), grid, rlen = 50, mode = "pbatch", dist.fcts = dist)
    })["elapsed"]
    print(paste("SOM",grid_side, dist, "done. Time to fit the model:",time_som, "seconds."))
    
    # Quality metrics
    time_qual_som <- system.time({
      res.som.qual <- aweSOM::somQuality(res.som,as.matrix(df_clust))
    })["elapsed"] 
    print(paste("Qual. SOM",grid_side, dist, "done. Time to fit the model:",time_qual_som, "seconds."))
    
    # Save in list
    som_res[[paste("SOM",grid_side, dist, sep = "_")]] <- res.som
    som_res[[paste("SOM_qual",grid_side, dist, sep = "_")]] <- res.som.qual
    
    # Save results as R object in local
    write_rds(som_res, file.path(path_nbh, paste0("SOM_metrics_gridsides_", grid_side2, "_", grid_side3, "_", std_method, "_metaggl_", met_aggl_labs_save, ".rds")), compress = "gz")
  }
}

#-----------------------------------------------------------------------#
### 5.4.2 Evaluate metrics ----
#-----------------------------------------------------------------------#

som_res_grid <- read_rds(file.path(path_nbh, paste0("SOM_metrics_gridsides_", grid_side2, "_", grid_side3, "_", std_method, "_metaggl_", met_aggl_labs_save, ".rds")))

# Quant error
sorted_metric <- sort(sapply(som_res_grid[startsWith(names(som_res_grid), "SOM_qual_")], function(x) x$err.quant), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Quant. Error: %.4f\n", name, sorted_metric[name]))
}

# Topo error
sorted_metric <- sort(sapply(som_res_grid[startsWith(names(som_res_grid), "SOM_qual_")], function(x) x$err.topo), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Topo. Error: %.4f\n", name, sorted_metric[name]))
}

# Explained variance
sorted_metric <- sort(sapply(som_res_grid[startsWith(names(som_res_grid), "SOM_qual_")], function(x) x$err.varratio), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Expl. Variance: %.4f\n", name, sorted_metric[name]))
}

# Prefered grid side: ceiling(sqrt(sqrt(nrow(df_clust))*5)) (73)
# as the gains in quality of a bigger grid are small relative to the significant increase in computing time

#-----------------------------------------------------------------------#
## 5.5 Clustering on SOM ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
### 5.5.1 Final SOM ---- 
#-----------------------------------------------------------------------#

# Import data
rm(res.som)
gc()
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file = file.path(path_nbh,rds_file))
df_clust <- df_scale[, clust_cols, with = FALSE]

# Run SOM
grid = kohonen::somgrid(xdim = grid_side1, ydim = grid_side1, "hexagonal", neighbourhood.fct = "gaussian")
res.som = kohonen::som(as.matrix(df_clust), grid, rlen = 150, mode = "pbatch", dist.fcts = "euclidean")
# Save final SOM 
write_rds(res.som, file.path(path_nbh, paste0("final_SOM_gridsize_", grid_side1, "_dist_euclidean_", std_method, "_metaggl_", met_aggl_labs_save, ".rds")), compress = "gz")

#-----------------------------------------------------------------------#
### 5.5.2 Load results and extract code vectors ----
#-----------------------------------------------------------------------#

res.som <- read_rds(file.path(path_nbh, paste0("final_SOM_gridsize_", grid_side1, "_dist_euclidean_", std_method, "_metaggl_", met_aggl_labs_save, ".rds")))
codes_vectors <- res.som$codes[[1]]
codes_dist = kohonen::object.distances(res.som, type = "codes")
wards.dist <- hclust(codes_dist, method = "ward.D2")

#-----------------------------------------------------------------------#
### 5.5.3 K-Means clustering on code vectors ----
#-----------------------------------------------------------------------#

somkmeans <- list()      
for (k in 2:30) {
  
  # Model
  time_somkmeans <- system.time({
    batch_size <- min(100000, nrow(codes_vectors))
    som_kmeans_result <- kmeans(codes_vectors, centers = k, nstart = 1000, iter.max = 1000000)
  })["elapsed"]
  print(paste("SOMKMeans", k, "done. Time to fit the model:",time_somkmeans, "seconds."))
  
  #Metrics
  time_metric <- system.time({
    batch_som_kmeans_metric <- clust_metrics(codes_vectors, som_kmeans_result, sample_size = 10000, label = "SOMKMeans")
    somkmeans[[as.character(k)]] <- batch_som_kmeans_metric
    # Save centroids for Clustergram
    somkmeans[[as.character(k)]][["centroids"]] <- som_kmeans_result$centers
  })["elapsed"]
  print(paste("SOMKMeans", k, "metrics done. Time:",time_metric, "seconds."))
  
  # Save results
  write_rds(somkmeans, file.path(path_nbh, paste0("clust_metrics_", std_method, "_optimal_somkmeans_metaggl_",met_aggl_labs_save, ".rds")), compress = "gz")
  gc()
}

#-----------------------------------------------------------------------#
## 5.6 Metrics  ----
#-----------------------------------------------------------------------#

somkmeans <- read_rds(file.path(path_nbh, paste0("clust_metrics_", std_method, "_optimal_somkmeans_metaggl_",met_aggl_labs_save, ".rds")))

# Silhouette
sorted_metric <- sort(sapply(somkmeans, function(x) x$Silhouette), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Silhouette Score: %.4f\n", name, sorted_metric[name]))
}

# CH
sorted_metric <- sort(sapply(somkmeans, function(x) x$Calinski_Harabasz), decreasing = T)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Calinski Harabasz: %.4f\n", name, sorted_metric[name]))
}

# DB
sorted_metric <- sort(sapply(somkmeans, function(x) x$Davies_Bouldin), decreasing = F)
for (name in names(sorted_metric)) {
  cat(sprintf("%-25s Davies Bouldin: %.4f\n", name, sorted_metric[name]))
}

# plot
somkmeans_sil_plot <- plot_clust_metric(somkmeans[1:19], "Silhouette", "Silhouette")
somkmeans_ch_plot <- plot_clust_metric(somkmeans[1:19], "Calinski_Harabasz", "Calinski Harabasz")
somkmeans_db_plot <- plot_clust_metric(somkmeans[1:19], "Davies_Bouldin", "Davies Bouldin")

png(
  file.path(path_results, 
            "figures", 
            "cluster_metrics", 
            paste0(
              "cluster_metrics_", 
              std_method, 
              "_somkmeans_",
              met_aggl_labs_save,
              ".png"
            )
  ),
  width = 5000,
  height = 1000,
  res = 300
)

gridExtra::grid.arrange(grobs = list(somkmeans_sil_plot,
                                     somkmeans_ch_plot,
                                     somkmeans_db_plot),
                        nrow = 3)

dev.off()

#-----------------------------------------------------------------------#
## 5.7 Plot clustergram ----
#-----------------------------------------------------------------------#

cluster_info <- list()
for(k in 2:30) {
  cluster_info[[as.character(k)]]$centers <- somkmeans[[as.character(k)]]$centroids
  cluster_info[[as.character(k)]]$clusters <- somkmeans[[as.character(k)]]$cluster_assign
}

for (factor_sel in 1:2) {
  png(file.path(path_results, "figures", "clustergram", paste0("clustergram_", std_method, "_somkmeans_PCA", factor_sel, "_", met_aggl_labs_save,".png")),1500,900,pointsize = 15)
  clustergram(
    Data = codes_vectors, 
    cluster_info = cluster_info,
    k.range = 2:20, 
    pca.loading = factor_sel,
    ylab = paste0("PCA weighted mean of the clusters (PCA Factor: ", factor_sel, ")")
  )
  dev.off()
}

#-----------------------------------------------------------------------#
## 5.8 Save potentially optimal cluster assignment ----
#-----------------------------------------------------------------------#
for (nclust in 8:10) {
  
  # Load data and SOM results 
  rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
  df_scale <- read_rds(file = file.path(path_nbh,rds_file))
  df_clust <- df_scale[, clust_cols, with = FALSE]
  res.som <- read_rds(file.path(path_nbh, paste0("final_SOM_gridsize_", grid_side1, "_dist_euclidean_", std_method, "_metaggl_", met_aggl_labs_save, ".rds")))
  somkmeans <- read_rds(file.path(path_nbh, paste0("clust_metrics_", std_method, "_optimal_somkmeans_metaggl_",met_aggl_labs_save, ".rds")))
  
  # Cluster
  cluster_nodes <- somkmeans[[as.character(nclust)]]$cluster_assign
  som_cluster <- as.matrix(cluster_nodes)
  som_palette <- get(paste0("tol",nclust,"qualitative"))
  
  # Plot cluster assignment 
  # plot_som_clust <- plot(
  #   res.som,
  #   type = "mapping",
  #   bgcol = som_palette[som_cluster],
  #   pc = "",
  #   shape = "straight",
  #   main = "Clusters"
  # )
  
  # To original dataframe
  bmu_indices <- res.som$unit.classif
  cluster_obs <- cluster_nodes[bmu_indices]
  df_scale$CLUSTER <- as.factor(cluster_obs)
  
  # Save dataset 
  cols_id <- c(paste0('VIERKANT', grid_size,'M'), 'YEAR')
  df_save_clust <- df_scale[, c(cols_id, "CLUSTER"), with = F]
  
  save_file <- paste0("data_clust_assign_", std_method, "_orig_somkmeans_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")
  write_rds(df_save_clust, file = file.path(path_nbh,save_file))
  rm(df_save_clust)
  gc()

}
