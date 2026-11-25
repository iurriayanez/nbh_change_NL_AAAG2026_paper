#------------------------------------------------------------------------------#
# Name: 2_3_nbh_cluster_model_selection_cluster_maps.R
# Goal: Cluster model selection: map cluster assignment of different cluster models
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

# Load data 
rds_file <- paste0("data_scaled_", std_method, "_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_scale <- read_rds(file.path(path_nbh,rds_file))

# Create polygons
polygons_sf <- grid_to_polygon(df_scale) 

# Land use 
land_sf <- sf::st_read(file.path(path_utilities, "Tools","GISHulpbestanden", "Bodemstatistiek", "2015", "BBG2015_Publicatiebestand.shp"))
water <- subset(land_sf, BG2015 >= 70 & BG2015 < 90)
roads <- subset(land_sf, BG2015 >= 10 & BG2015 < 20)
built <- subset(land_sf, BG2015 >= 20 & BG2015 < 40)
other <- subset(land_sf, BG2015 >= 40 & BG2015 < 70)

#-----------------------------------------------------------------------#
# 2. Map data ----
#-----------------------------------------------------------------------#
# Map 
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
    # SF data
    #-----------------------------------------------------------------------#
    clusters_sf <- sf::st_as_sf(merge(df_scale, polygons_sf, by.y = 'ID', by.x = 'VIERKANT100M', all.x = TRUE))
    
    # by year 
    for (i in years) {
      clusters_years <- paste0("clusters_sf_",i)
      assign(clusters_years, clusters_sf[clusters_sf$YEAR == i,]) 
    }
    
    #-----------------------------------------------------------------------#
    # Map
    #-----------------------------------------------------------------------#
    # aesthetics 
    clust_labs <- paste("Cluster",1:nclust)
    names(clust_labs) <- 1:nclust
    current_palette <- get(paste0("tol",nclust,"qualitative"))
    clustlabs <- scale_fill_manual(
      values = current_palette,
      labels = clust_labs, 
      drop = TRUE
    )
    
    for (urbreg_map in 1:22){
      
      # Subset landuse 
      bbox_list <- sf::st_bbox(clusters_sf[clusters_sf$URBREG == urbreg_map,])
      y_min <- bbox_list[["ymin"]] - 100
      y_max <- bbox_list[["ymax"]] + 100
      x_min <- bbox_list[["xmin"]] - 100
      x_max <- bbox_list[["xmax"]] + 100
      
      box_coords <- tibble(x = c(x_min, x_max), y = c(y_min, y_max)) %>% 
        sf::st_as_sf(coords = c("x", "y")) %>% 
        sf::st_set_crs(sf::st_crs(land_sf))
      bbox <- sf::st_bbox(box_coords) %>% sf::st_as_sfc()
      
      land_sf_sub <- sf::st_intersection(land_sf,bbox)
      water_sub <- sf::st_intersection(water,bbox)
      roads_sub <- sf::st_intersection(roads,bbox)
      built_sub <- sf::st_intersection(built,bbox)
      other_sub <- sf::st_intersection(other,bbox)
      
      # North Arrow left: 4, 6, 11, 12, 17
      # Scale bar left: 7, 9, 12, 17
      
      for (i in years) {
        plot.data <- get(paste("clusters_sf",i, sep = "_"))
        plot.data <- plot.data[plot.data$URBREG == urbreg_map,]
        
        if (urbreg_map %in% c(4,6,11)) {
          plot.clust <- map_variables(plot.data, "CLUSTER", "", add_breaks = FALSE, loc_arrow = 'tl')
        } else if (urbreg_map %in% c(7,9)) {
          plot.clust <- map_variables(plot.data, "CLUSTER", "", add_breaks = FALSE, loc_scale = 'bl')
        } else if (urbreg_map %in% c(12,17)) {
          plot.clust <- map_variables(plot.data, "CLUSTER", "", add_breaks = FALSE, loc_arrow = 'tl', loc_scale = 'bl')
        } else {
          plot.clust <- map_variables(plot.data, "CLUSTER", "", add_breaks = FALSE)
        }
        
        plot.clust <- plot.clust + 
          clustlabs +
          labs(subtitle = paste(urbreg_labels[urbreg_map],"- Year:", i, sep = " "))
        if (urbreg_map == 14) {
          ggsave(file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, "_urbreg_", urbreg_map,"_", i,".jpg")), plot = plot.clust, width = 20, height = 10, units = "in", dpi = 400)
        } else {
          ggsave(file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, "_urbreg_", urbreg_map,"_", i,".jpg")), plot = plot.clust, width = 15, height = 10, units = "in", dpi = 400)
        }
        
        # Trim whitespace
        img <- magick::image_read(file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, "_urbreg_", urbreg_map,"_", i,".jpg")))
        img_trimmed <- magick::image_trim(img)
        img_trimmed <- magick::image_border(img_trimmed, color = "white", geometry = "100x100")
        magick::image_write(img_trimmed, file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, "_urbreg_", urbreg_map,"_", i,".jpg")))
      }
    } 
    
    # Whole country
    land_sf_sub <- land_sf
    water_sub <- water
    roads_sub <- roads
    built_sub <- built
    other_sub <- other
    
    for (i in years) {
      plot.data <- get(paste("clusters_sf",i, sep = "_"))
      plot.clust <- map_variables(plot.data, "CLUSTER", "", add_breaks = FALSE)
      plot.clust <- plot.clust + 
        clustlabs +
        labs(subtitle = paste("The Netherlands - Year:", i, sep = " ")) +
        theme_minimal() +
        theme(
          plot.caption = element_text(size= 3, face = "italic"),
          plot.subtitle = element_text(size= 35, vjust = -8.5, hjust = 0.075),
          legend.position = "right",
          legend.text = element_text(size = 35), legend.title = element_text(size = 40),
          legend.key.size = unit(1, "cm"),
          legend.box.margin = margin(l = -12),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(-0.25,-3.5,0,-3.5), "cm")
        ) 
      ggsave(file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_allcountry_",i, ".jpg")), plot = plot.clust, width = 28, height = 24, units = "in", dpi = 500)
      
      # Trim whitespace
      img <- magick::image_read(file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_allcountry_",i, ".jpg")))
      img_trimmed <- magick::image_trim(img)
      img_trimmed <- magick::image_border(img_trimmed, color = "white", geometry = "100x100")
      magick::image_write(img_trimmed, file.path(path_results, "figures", "cluster_maps", "model_selection_maps", paste0("cluster_map_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save,"_allcountry_",i, ".jpg")))
    }
  }
}
