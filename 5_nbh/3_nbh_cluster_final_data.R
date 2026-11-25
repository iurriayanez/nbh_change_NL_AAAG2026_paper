#------------------------------------------------------------------------------#
# Name: 3_nbh_cluster_final_data.R
# Goal: Final sociospatial cluster assignment
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_for_clustering_metaggl_all.rds
#                  data/nbh/data_clust_assign_[std_method]_orig_pca_mbkmeans_k_7_10_metaggl_all.rds
#                  data/nbh/data_clust_assign_[std_method]_orig_somkmeans_k[nclust]_metaggl_all.rds
# Output databases: data/nbh/data_final_std_cluster_assignment_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#------------------------------------------------------------------------------#


#-----------------------------------------------------------------------#
# 0. Setup ----
#-----------------------------------------------------------------------#
source(file.path(path_code, "functions.R"), echo = F)
#-----------------------------------------------------------------------#
## 0.1 Libraries ----
#-----------------------------------------------------------------------#
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('readr', 'dplyr', 'data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

#-----------------------------------------------------------------------#
## 0.2 Seed ----
#-----------------------------------------------------------------------#
set.seed(1409)

#-----------------------------------------------------------------------#
## 0.3 Parameters ----
#-----------------------------------------------------------------------#

nclust  <- 9 # Optimal number of clusters
clust_method <- "mbkmeans"
init_k <- 7 # To load cluster data (range of calculated optimal clusters)
end_k <- 10 # To load cluster data (range of calculated optimal clusters)
cols_id <- c(paste0('VIERKANT', grid_size,'M'), 'YEAR')


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

#-----------------------------------------------------------------------#
# 1. Import data ----
#-----------------------------------------------------------------------#

rds_file <- paste0("data_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df <- read_rds(file.path(path_nbh,rds_file))

# Select cluster variable to df
if (clust_method %in% c("mbkmeans","pca_mbkmeans")) {
  
  rds_file <- paste0("data_clust_assign_", std_method, "_orig_", clust_method, "_k_", init_k, "_", end_k, "_metaggl_", met_aggl_labs_save, ".rds")
  df_save_clust <- read_rds(file.path(path_nbh,rds_file))
  
  df_save_clust <- df_save_clust[, c(cols_id, paste0("CLUSTERk", nclust)), with = F]
  setnames(df_save_clust, paste0("CLUSTERk", nclust), "CLUSTER")
  
} else if (clust_method == "somkmeans") {
  
  rds_file <- paste0("data_clust_assign_", std_method, "_orig_somkmeans_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")
  df_save_clust <- read_rds(file.path(path_nbh,rds_file))
  
}

#-----------------------------------------------------------------------#
# 2. Merge ----
#-----------------------------------------------------------------------#

df[, CLUSTER := NULL] # Remove if it already exists
df <- merge(df, df_save_clust, by = cols_id, all.x = T)
rm(df_save_clust)
gc()

#-----------------------------------------------------------------------#
# 3. Standardise cluster assignment ----
#-----------------------------------------------------------------------#
map <- cluster_std_map[[clust_method]][[as.character(nclust)]]
if (!is.null(map)) {
  df[, CLUSTER := map[CLUSTER]]
}

#-----------------------------------------------------------------------#
# 4. Export dataset ----
#-----------------------------------------------------------------------#

save_file <- paste0(
  "data_final_std_cluster_assignment_", 
  std_method,
  "_",
  clust_method, 
  "_k",
  nclust, 
  "_metaggl_", met_aggl_labs_save, ".rds")
write_rds(df, file = file.path(path_nbh,save_file))

rm(df)
gc()