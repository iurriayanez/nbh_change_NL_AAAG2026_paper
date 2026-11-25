#------------------------------------------------------------------------------#
# Name: 2_2_nbh_cluster_model_selection_ave_distprof_plots.R
# Goal: Cluster model selection: plot average distance profiles (sociospatial signatures)
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_scaled_[std_method]_for_clustering_metaggl_all.rds
#                  data/nbh/data_clust_assign_[std_method]_orig_pca_mbkmeans_k_7_10_metaggl_all.rds
#                  data/nbh/data_clust_assign_[std_method]_orig_somkmeans_k[nclust]_metaggl_all.rds
# Output databases: 
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
## 0.3 Parameters ----
#-----------------------------------------------------------------------#
clust_methods <- c("mbkmeans","pca_mbkmeans", "somkmeans") # Cluster methods to compare 

# range of potentially optimal clusters (PCA+MBKMeans and Mini-Batch KMeans)
init_k <- 7 
end_k <- 10

# IDs for merge 
cols_id <- c(paste0('VIERKANT', grid_size,'M'), 'YEAR')

#-----------------------------------------------------------------------#
# 1. Import data ----
#-----------------------------------------------------------------------#

# Load data 
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file.path(path_nbh,rds_file))

# Lists with variable names and lookups

# All variables 
all_cols <- c(
  # Ethnic background
  c(grep("^PERC_ETHNL", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHEUR", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_scale), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_3_TO_6", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_scale), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_MIDINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_scale), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_PRIVRENTAL", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_scale), value = TRUE))
)

# Lists to change datatable from wide to long
list_all_cols_num <- list(
  # Ethnic background
  c(grep("^PERC_ETHNL", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHEUR", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_scale), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_3_TO_6", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_scale), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_MIDINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_scale), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_PRIVRENTAL", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_scale), value = TRUE))
)

all_cols_names <- c(
  # Ethnic background
  'PERC_ETHNL',
  'PERC_ETHEUR',
  'PERC_ETHNONEUR',
  # Age groups
  'PERC_AGE_GROUP_1',
  'PERC_AGE_GROUP_2',
  'PERC_AGE_GROUP_3_TO_6',
  'PERC_AGE_GROUP_7',
  # Household income
  'PERC_HH_LOWINC',
  'PERC_HH_MIDINC',
  'PERC_HH_HIGHINC',
  # Tenure
  'PERC_HH_HOMEOWNER',
  'PERC_HH_PRIVRENTAL',
  'PERC_HH_SUBRENTAL'
)

# All variable labels lookup
all_cols_labels <- c(
  # Ethnic background
  'Natives',
  'Europeans',
  'Non-Europeans',
  # Age groups
  'Children',
  'Young adults',
  'Adults',
  'Older adults',
  # Household income
  'Low income',
  'Middle income',
  'High income',
  # Tenure
  'Home-owners',
  'Priv. renters',
  'Sub. renters'
)

all_labels_lookup <- setNames(all_cols_labels, all_cols_names)

# Selected columns
clust_cols <- c(
  # Ethnic background
  c(grep("^PERC_ETHEUR", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_scale), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_scale), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_scale), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_scale), value = TRUE))
)

list_clust_cols_num <- list(
  # Ethnic background
  c(grep("^PERC_ETHEUR", names(df_scale), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df_scale), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df_scale), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df_scale), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df_scale), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df_scale), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df_scale), value = TRUE))
)

clust_cols_names <- c(
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

# Variable labels lookup
clust_cols_labels <- c(
  # Ethnic background
  'Europeans',
  'Non-Europeans',
  # Age groups
  'Children',
  'Young adults',
  'Older adults',
  # Household income
  'Low income',
  'High income',
  # Tenure
  'Home-owners',
  'Sub. renters'
)

clust_labels_lookup <- setNames(clust_cols_labels, clust_cols_names)


#-----------------------------------------------------------------------#
# 2. Plot (original assignment) ----
#-----------------------------------------------------------------------#

for (clust_method in clust_methods) {
  
  for (nclust in 8:10) {
    
    # Assign cluster variable to df
    if (clust_method %in% c("mbkmeans","pca_mbkmeans")) {
      
      rds_file <- paste0("data_clust_assign_", std_method, "_orig_", clust_method, "_k_", init_k, "_", end_k, "_metaggl_", met_aggl_labs_save, ".rds")
      df_save_clust <- read_rds(file.path(path_nbh,rds_file))
      
      df_save_clust <- df_save_clust[, c(cols_id, paste0("CLUSTERk", nclust)), with = F]
      setnames(df_save_clust, paste0("CLUSTERk", nclust), "CLUSTER")
      
    } else if (clust_method == "somkmeans") {
      
      rds_file <- paste0("data_clust_assign_", std_method, "_orig_somkmeans_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")
      df_save_clust <- read_rds(file.path(path_nbh,rds_file))
    
    }
  
    df_scale[, CLUSTER := NULL] # Remove if it already exists
    df_scale <- merge(df_scale, df_save_clust, by = cols_id, all.x = T)
    rm(df_save_clust)
    gc()
    
    #-----------------------------------------------------------------------#
    # all variables
    #-----------------------------------------------------------------------#
    
    # Create dataframe
    df_clust_mean_clust <- df_scale[, lapply(.SD, mean, na.rm = T), by = CLUSTER, .SDcols = all_cols]
    
    df_clust_mean_long <- melt(
      df_clust_mean_clust, 
      id.vars = c('CLUSTER'), 
      measure.vars = list_all_cols_num,
      variable.name = "SCALE", 
      value.name = all_cols_names
    )
    
    df_clust_mean_long <- melt(
      df_clust_mean_long, 
      id.vars = c('CLUSTER', 'SCALE'), 
      measure.vars = all_cols_names
    )
    
    # Max value of SCALE is the smallest
    df_clust_mean_long[, SCALE := ifelse(SCALE == max(as.numeric(SCALE)), 0, SCALE)]
    
    for (i in 1:nclust) {
      df_labels <- df_clust_mean_long %>%
        filter(CLUSTER == i & SCALE == 6) %>%
        group_by(variable) %>%
        slice(which.max(variable)) %>%
        mutate(labels = all_labels_lookup[variable])
      
      p <- cluster_dist_profiles(
        cluster_number = i,
        data = df_clust_mean_long, 
        data_labels = df_labels, 
        x_axis_labels = scale_labs,
        nudge_x_label = 0.75,
        label_size = 9,
        palette = c(colors_dim_3b, colors_dim_4p, colors_dim_3o, colors_dim_3g)
      )
      p <- p +
        theme(
          axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 25),
          plot.title = element_text(size = 40)
        )
      ggsave(
        file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_", std_method, "_all_vars_orig_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_cluster", i, ".jpg")),
        plot = p,
        width = 20,
        height = 16,
        units = "in"
      )
      
    }
    
    rm(df_clust_mean_clust, df_clust_mean_long, df_labels)
    gc()
    
    #-----------------------------------------------------------------------#
    # cluster variables
    #-----------------------------------------------------------------------#
    
    # Create dataframe
    df_clust_mean_clust <- df_scale[, lapply(.SD, mean, na.rm = T), by = CLUSTER, .SDcols = clust_cols]
    
    df_clust_mean_long <- melt(
      df_clust_mean_clust, 
      id.vars = c('CLUSTER'), 
      measure.vars = list_clust_cols_num,
      variable.name = "SCALE", 
      value.name = clust_cols_names
    )
    
    df_clust_mean_long <- melt(
      df_clust_mean_long, 
      id.vars = c('CLUSTER', 'SCALE'), 
      measure.vars = clust_cols_names
    )
    
    # Max value of SCALE is the smallest
    df_clust_mean_long[, SCALE := ifelse(SCALE == max(as.numeric(SCALE)), 0, SCALE)]
    
    for (i in 1:nclust) {
      df_labels <- df_clust_mean_long %>%
        filter(CLUSTER == i & SCALE == 6) %>%
        group_by(variable) %>%
        slice(which.max(variable)) %>%
        mutate(labels = clust_labels_lookup[variable])
      
      p <- cluster_dist_profiles(
        cluster_number = i,
        data = df_clust_mean_long, 
        data_labels = df_labels,
        x_axis_labels = scale_labs,
        nudge_x_label = 0.75,
        label_size = 9,
        palette = c(colors_dim_b, colors_dim_p, colors_dim_o, colors_dim_g)
      )
      p <- p +
        theme(
          axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 25),
          plot.title = element_text(size = 40)
        )
      ggsave(
        file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_", std_method, "_orig_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_cluster", i, ".jpg")),
        plot = p,
        width = 20,
        height = 16,
        units = "in"
      )
    }
    
    rm(df_clust_mean_clust, df_clust_mean_long, df_labels)
    gc()
    
  }
}


#-----------------------------------------------------------------------#
# 3. Plot (standard assignment) ----
#-----------------------------------------------------------------------#

# Map standard cluster assignment 
# k = 8 
# [MiniBatchKmeans - PCA + MiniBatchKmeans]: [1 - 6]; [2 - 4]; [3 - 3]; [4 - 2]; [5 - 1]; [6 - 5]; [7 - 8]; [8 - 7]; 
# [SOM + KMeans - PCA + MiniBatchKmeans]: NOT EQUIVALENT
# k = 9 
# [MiniBatchKmeans - PCA + MiniBatchKmeans]: [1 - 3]; [2 - 2]; [3 - 6]; [4 - 5]; [5 - 1]; [6 - 8]; [7 - 4]; [8 - 7]; [9 - 9] 
# [SOM + KMeans - PCA + MiniBatchKmeans]: [1 - 8]; [2 - 1]; [3 - 4]; [4 - 3]; [5 - 9]; [6 - 7]; [7 - 6]; [8 - 5]; [9 - 2]  
# k = 10
# [MiniBatchKmeans - PCA + MiniBatchKmeans]: [1 - 8]; [2 - 10]; [3 - 7]; [4 - 5]; [5 - 6]; [6 - 3]; [7 - 1]; [8 - 9]; [9 - 2]; [10 - 4]  
# [SOM + KMeans - PCA + MiniBatchKmeans]: [1 - 6]; [2 - 5]; [3 - 2]; [4 - 7]; [5 - 3]; [6 - 4]; [7 - 9]; [8 - 8]; [9 - 1]; [10 - 10]  

cluster_std_map <- list(
  mbkmeans = list(
    `8` = c(6, 4, 3, 2, 1, 5, 8, 7),
    `9` = c(3, 2, 6, 5, 1, 8, 4, 7, 9),
    `10` = c(8, 10, 7, 5, 6, 3, 1, 9, 2, 4)
  ),
  somkmeans = list(
    `9` = c(8, 1, 4, 3, 9, 7, 6, 5, 2),
    `10` = c(6, 5, 2, 7, 3, 4, 9, 8, 1, 10)
  )
)

# Load data 
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file.path(path_nbh,rds_file))

# Create plots
for (clust_method in clust_methods) {
  
  for (nclust in 8:10) {
    
    # No standardisation possible for SOM Kmeans when k=8
    if (clust_method == "somkmeans" & nclust == 8) {
      next
    }
    
    # Assign cluster variable to df
    if (clust_method %in% c("mbkmeans","pca_mbkmeans")) {
      
      rds_file <- paste0("data_clust_assign_", std_method, "_orig_", clust_method, "_k_", init_k, "_", end_k, "_metaggl_", met_aggl_labs_save, ".rds")
      df_save_clust <- read_rds(file.path(path_nbh,rds_file))
      
      df_save_clust <- df_save_clust[, c(cols_id, paste0("CLUSTERk", nclust)), with = F]
      setnames(df_save_clust, paste0("CLUSTERk", nclust), "CLUSTER")
      
    } else if (clust_method == "somkmeans") {
      
      rds_file <- paste0("data_clust_assign_", std_method, "_orig_somkmeans_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")
      df_save_clust <- read_rds(file.path(path_nbh,rds_file))
      
    }
    
    df_scale[, CLUSTER := NULL] # Remove if it already exists
    df_scale <- merge(df_scale, df_save_clust, by = cols_id, all.x = T)
    rm(df_save_clust)
    gc()
    
    #-----------------------------------------------------------------------#
    # standardise cluster assignment
    #-----------------------------------------------------------------------#
    map <- cluster_std_map[[clust_method]][[as.character(nclust)]]
    if (!is.null(map)) {
      df_scale[, CLUSTER := map[CLUSTER]]
    }
    
    #-----------------------------------------------------------------------#
    # all variables
    #-----------------------------------------------------------------------#
    
    # Create dataframe
    df_clust_mean_clust <- df_scale[, lapply(.SD, mean, na.rm = T), by = CLUSTER, .SDcols = all_cols]
    
    df_clust_mean_long <- melt(
      df_clust_mean_clust, 
      id.vars = c('CLUSTER'), 
      measure.vars = list_all_cols_num,
      variable.name = "SCALE", 
      value.name = all_cols_names
    )
    
    df_clust_mean_long <- melt(
      df_clust_mean_long, 
      id.vars = c('CLUSTER', 'SCALE'), 
      measure.vars = all_cols_names
    )
    
    # Max value of SCALE is the smallest
    df_clust_mean_long[, SCALE := ifelse(SCALE == max(as.numeric(SCALE)), 0, SCALE)]
    
    for (i in 1:nclust) {
      df_labels <- df_clust_mean_long %>%
        filter(CLUSTER == i & SCALE == 6) %>%
        group_by(variable) %>%
        slice(which.max(variable)) %>%
        mutate(labels = all_labels_lookup[variable])
      
      p <- cluster_dist_profiles(
        cluster_number = i,
        data = df_clust_mean_long, 
        data_labels = df_labels,
        x_axis_labels = scale_labs,
        nudge_x_label = 0.75,
        label_size = 9,
        palette = c(colors_dim_3b, colors_dim_4p, colors_dim_3o, colors_dim_3g)
      )
      p <- p +
        theme(
          axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 25),
          plot.title = element_text(size = 40)
        )
      
      ggsave(
        file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_", std_method, "_all_vars_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_cluster", i, ".jpg")),
        plot = p,
        width = 20,
        height = 16,
        units = "in"
      )
      
    }
    
    rm(df_clust_mean_clust, df_clust_mean_long, df_labels)
    gc()
    
    #-----------------------------------------------------------------------#
    # cluster variables
    #-----------------------------------------------------------------------#
    
    # Create dataframe
    df_clust_mean_clust <- df_scale[, lapply(.SD, mean, na.rm = T), by = CLUSTER, .SDcols = clust_cols]
    
    df_clust_mean_long <- melt(
      df_clust_mean_clust, 
      id.vars = c('CLUSTER'), 
      measure.vars = list_clust_cols_num,
      variable.name = "SCALE", 
      value.name = clust_cols_names
    )
    
    df_clust_mean_long <- melt(
      df_clust_mean_long, 
      id.vars = c('CLUSTER', 'SCALE'), 
      measure.vars = clust_cols_names
    )
    
    # Max value of SCALE is the smallest
    df_clust_mean_long[, SCALE := ifelse(SCALE == max(as.numeric(SCALE)), 0, SCALE)]
    
    for (i in 1:nclust) {
      df_labels <- df_clust_mean_long %>%
        filter(CLUSTER == i & SCALE == 6) %>%
        group_by(variable) %>%
        slice(which.max(variable)) %>%
        mutate(labels = clust_labels_lookup[variable])
      
      p <- cluster_dist_profiles(
        cluster_number = i,
        data = df_clust_mean_long, 
        data_labels = df_labels,
        x_axis_labels = scale_labs,
        nudge_x_label = 0.75,
        label_size = 9,
        palette = c(colors_dim_b, colors_dim_p, colors_dim_o, colors_dim_g)
      )
      p <- p +
        theme(
          axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 25),
          plot.title = element_text(size = 40)
        )
      ggsave(
        file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_cluster", i, ".jpg")),
        plot = p,
        width = 20,
        height = 16,
        units = "in"
      )
    }
    
    rm(df_clust_mean_clust, df_clust_mean_long, df_labels)
    gc()
    
  }
}
