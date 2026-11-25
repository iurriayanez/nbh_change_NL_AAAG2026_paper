#------------------------------------------------------------------------------#
# Name: 8_nbh_seq_analysis_maps.R
# Goal: Plot maps with sequence clusters
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#               
# Output databases: 
#------------------------------------------------------------------------------#

# 0. Setup ----
## 0.1 Libraries ----
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(cowplot)
library(grid)
library(readr)
library(data.table)
library(cowplot)

source(file.path(path_code, "functions.R"), echo = F) 

#-----------------------------------------------------------------------#
# 1. Import data ----
#-----------------------------------------------------------------------#
clust_method <- "mbkmeans"
nclust <- 9
seqnclust <- 46
param_name <- "OMstran_sm2_i1_tri1_w01_previousF"

rds_file <- save_file <- paste0("data_final_seq_cluster_assignment_claraseqclust_",param_name,"_k",seqnclust,"_", std_method,"_",clust_method,"_k",nclust,"_metaggl_",met_aggl_labs_save,".rds")
df_seq_clust <- read_rds(file.path(path_nbh,rds_file))

# Add urban region and population (2023)
rds_file <- paste0("data_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_urbreg <- read_rds(file.path(path_nbh,rds_file))
setDT(df_urbreg)
df_urbreg <- df_urbreg[YEAR == 2023, c("VIERKANT100M", "URBREG", "TOT_IND"), with = FALSE]
df_urbreg <- unique(df_urbreg)

df_seq_clust <- merge(df_seq_clust, df_urbreg, by = "VIERKANT100M", all.x= TRUE, all.y = FALSE)
rm(df_urbreg)
gc()

# Order sequence clusters
new_seq_cluster_order <- c(
  # Constant
  1, 31, 26, 27, 12, 28, 34, 14, 7, 
  # From 1
  39, 6, 45,
  # From 2
  2, 24, 32, 30, 40,
  # From 3
  38, 8, 25, 42, 15, 29, 20, 43, 41, 46, 17,
  # From 5
  10, 11, 4,
  # From 6
  22, 23, 33,
  # From 7
  37, 
  # From 8
  5, 13, 44, 35, 36,
  # From 9
  3, 9, 18, 19, 21, 16
)

# Labels 
new_seq_cluster_labels <- c(
  # Constant
  "1", "2", "3", "4", "5", "6", "7", "8", "9",
  # From 1
  "1→4", "1→5", "1→8",
  # From 2
  "2→1", "2→3→2", "2→3", "2→8", "2→9",
  # From 3
  "3→1", "3→2", "3→2→3", "3→5→3", "3→5", "3→9→2", "3→9→3", "3→8", "3→9", "3→9→3→9", "3→2→9",
  # From 5
  "5→1", "5→3", "5→9",
  # From 6
  "6→1", "6→2", "6→8",
  # From 7
  "7→4", 
  # From 8
  "8→1", "8→3", "8→4", "8→7", "8→1→8",
  # From 9
  "9→1", "9→2", "9→3", "9→3→9", "9→5", "9→2→9"
)

# Convert to factor 
df_seq_clust[, SEQ_CLUSTER := factor(
  SEQ_CLUSTER, 
  levels = new_seq_cluster_order, 
  labels = new_seq_cluster_labels)]

# Select relevant columns 
df <- df_seq_clust %>%
  select(c("VIERKANT100M", "URBREG", "SEQ_CLUSTER", "TOT_IND"))

# Create polygons
polygons_sf <- grid_to_polygon(df)

# SF dataframe with cluster assignment
clusters_sf <- sf::st_as_sf(merge(df, polygons_sf, by.y = 'ID', by.x = 'VIERKANT100M', all.x = TRUE))

# Land use 
land_sf <- sf::st_read(file.path(path_utilities, "Tools","GISHulpbestanden", "Bodemstatistiek", "2015", "BBG2015_Publicatiebestand.shp"))
land_sf <- land_sf %>% 
  mutate(land_type = case_when(BG2015 >= 70 & BG2015 < 90 ~ 0,
                               BG2015 >= 10 & BG2015 < 20 ~ 1,
                               BG2015 >= 20 & BG2015 < 40 ~ 2,
                               BG2015 >= 40 & BG2015 < 70 ~ 3,
                               TRUE ~ NA_integer_))

# municipalities 
gem_sf <- sf::st_read(file.path(path_utilities, "Tools", "GISHulpbestanden", "Gemeentewijkbuurt", "2022", "gm_2022.shp"))

# Urban region borders 
df_spat <- fread(file.path(path_obj,"obj_spatial.csv"))
df_spat <- df_spat[, c("gem2022", "URBREG"), with = FALSE]
df_spat <- unique(df_spat)

df_gem_urbreg <- merge(df_spat, gem_sf, by.x = "gem2022", by.y = "GM2022")

df_gem_urbreg <- df_gem_urbreg[, c("URBREG", "gem2022", "geometry"), with = FALSE]
df_gem_urbreg <- unique(df_gem_urbreg, by = c("URBREG", "gem2022"))
df_gem_urbreg_sf <- st_as_sf(df_gem_urbreg) %>% st_make_valid() %>% 
  st_set_precision(10000) 

urbreg_sf <- df_gem_urbreg_sf %>% 
  group_by(URBREG) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop") %>% 
  st_make_valid()

urbreg_sf <- sf::st_cast(urbreg_sf, "MULTIPOLYGON") %>% 
  st_boundary()

# Select sequences 
seq_cluster_constant <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
seq_cluster_scale <- c("1→4", "1→5", "1→8", "2→1", "3→1", "5→1", "6→1", "7→4", "8→1", "8→7", "8→4", "9→1", "9→2")
seq_cluster_dim <- c("2→3","2→8","2→9","3→2","3→5","3→8","3→9","5→3","5→9","6→2","6→8","8→3","9→3","9→5")
seq_cluster_multi <- c("2→3→2","3→2→3","3→2→9","3→5→3","3→9→2","3→9→3","3→9→3→9","8→1→8","9→2→9","9→3→9")

# labels 
seq_labs <- list(
  # constant trajectories
  "1" = paste("Stable", tolower(clust_pen_labs[["1"]])),
  "2" = paste("Stable", tolower(clust_pen_labs[["2"]])),
  "3" = paste("Stable", tolower(clust_pen_labs[["3"]])),
  "4" = paste("Stable", tolower(clust_pen_labs[["4"]])),
  "5" = paste("Stable", tolower(clust_pen_labs[["5"]])),
  "6" = paste("Stable", tolower(clust_pen_labs[["6"]])),
  "7" = paste("Stable", tolower(clust_pen_labs[["7"]])),
  "8" = paste("Stable", tolower(clust_pen_labs[["8"]])),
  "9" = paste("Stable", tolower(clust_pen_labs[["9"]])),
  
  # scalar variability
  "1→4" = paste(clust_pen_labs[["1"]], "to \n", tolower(clust_pen_labs[["4"]])), 
  "1→5" = paste(clust_pen_labs[["1"]], "to \n", tolower(clust_pen_labs[["5"]])), 
  "1→8" = paste(clust_pen_labs[["1"]], "to \n", tolower(clust_pen_labs[["8"]])), 
  "2→1" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "3→1" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "5→1" = paste(clust_pen_labs[["5"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "6→1" = paste(clust_pen_labs[["6"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "7→4" = paste(clust_pen_labs[["7"]], "to \n", tolower(clust_pen_labs[["4"]])), 
  "8→1" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "8→4" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["4"]])), 
  "8→7" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["7"]])), 
  "9→1" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "9→2" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["2"]])), 
  
  # changes across scales 
  "2→3" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["3"]])),
  "2→8" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["8"]])),
  "2→9" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["9"]])),
  "3→2" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["2"]])), 
  "3→5" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["5"]])),
  "3→8" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["8"]])),
  "3→9" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["9"]])),
  "5→3" = paste(clust_pen_labs[["5"]], "to \n", tolower(clust_pen_labs[["3"]])), 
  "5→9" = paste(clust_pen_labs[["5"]], "to \n", tolower(clust_pen_labs[["9"]])), 
  "6→2" = paste(clust_pen_labs[["6"]], "to \n", tolower(clust_pen_labs[["2"]])),
  "6→8" = paste(clust_pen_labs[["6"]], "to \n", tolower(clust_pen_labs[["8"]])),
  "8→3" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["3"]])), 
  "9→3" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["3"]])), 
  "9→5" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["5"]])),
  
  # multiple transitions
  "2→3→2" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["3"]]), "and back"),
  "3→2→3" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["2"]]), "and back"),
  "3→2→9" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["2"]]), "to ageing elite"),
  "3→5→3" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["5"]]), "and back"),
  "3→9→2" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["9"]]), "to ageing middle-class"), 
  "3→9→3" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["9"]]), "and back"),
  "3→9→3→9" = paste("Changes between", tolower(clust_pen_labs[["3"]]), "\n and", tolower(clust_pen_labs[["9"]])),
  "8→1→8" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["1"]]), "and back"),
  "9→2→9" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["2"]]), "and back"),
  "9→3→9" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["3"]]), "and back")
  
)

#-----------------------------------------------------------------------#
# 2. Plot ----
#-----------------------------------------------------------------------#

all_sequences <- unique(clusters_sf$SEQ_CLUSTER)
params <- vector("list", 22)

params[[22]] <- list(loc_scale = "br",
                     loc_arrow = "tl",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.02,
                     bbox_padding_y_max = 0.02,
                     bbox_padding_x_min = 0.02,
                     bbox_padding_x_max = 0.2,
                     legend_box_x = 0.67,
                     legend_box_y = 0.65,
                     gem_name = "Maastricht",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[21]] <- list(loc_scale = "br",
                     loc_arrow = "tr",
                     loc_scale_small = "bl",
                     bbox_padding_y_min = 0.02,
                     bbox_padding_y_max = 0.02,
                     bbox_padding_x_min = 0.1,
                     bbox_padding_x_max = 0.02,
                     legend_box_x = 0.15,
                     legend_box_y = 0.65,
                     gem_name = "Heerlen",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[20]] <- list(loc_scale = "br",
                     loc_arrow = "tl",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.17,
                     bbox_padding_y_max = 0.3,
                     bbox_padding_x_min = 0.08,
                     bbox_padding_x_max = 0.3,
                     legend_box_x = 0.69,
                     legend_box_y = 0.67,
                     gem_name = "Sittard-Geleen",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[19]] <- list(loc_scale = "bl",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.02,
                     bbox_padding_y_max = 0.02,
                     bbox_padding_x_min = 0.05,
                     bbox_padding_x_max = 0.05,
                     legend_box_x = 0.67,
                     legend_box_y = 0.13,
                     gem_name = "Eindhoven",
                     map_order_12 = c(9, 7, 2, 6, 1, 3, 4, 8, 5, 10, 11, 12),
                     map_order_9 = c(9, 7, 2, 6, 1, 3, 4, 8, 5))

params[[18]] <- list(loc_scale = "bl",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.08,
                     bbox_padding_y_max = 0.08,
                     bbox_padding_x_min = 0.05,
                     bbox_padding_x_max = 0.35,
                     legend_box_x = 0.68,
                     legend_box_y = 0.15,
                     gem_name = "'s-Hertogenbosch",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[17]] <- list(loc_scale = "br",
                     loc_arrow = "tr",
                     loc_scale_small = "bl",
                     bbox_padding_y_min = 0.1,
                     bbox_padding_y_max = 0.02,
                     bbox_padding_x_min = 0.1,
                     bbox_padding_x_max = 0.02,
                     legend_box_x = 0.12,
                     legend_box_y = 0.14,
                     gem_name = "Tilburg",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[16]] <- list(loc_scale = "bl",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.15,
                     bbox_padding_y_max = 0,
                     bbox_padding_x_min = 0,
                     bbox_padding_x_max = 0.25,
                     legend_box_x = 0.69,
                     legend_box_y = 0.15,
                     gem_name = "Breda",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[15]] <- list(loc_scale = "bl",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.1,
                     bbox_padding_y_max = 0.02,
                     bbox_padding_x_min = 0.02,
                     bbox_padding_x_max = 0.1,
                     legend_box_x = 0.69,
                     legend_box_y = 0.15,
                     gem_name = "Dordrecht",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[14]] <- list(loc_scale = "bl",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0,
                     bbox_padding_y_max = 0.3,
                     bbox_padding_x_min = 0.4,
                     bbox_padding_x_max = 0.04,
                     legend_box_x = 0.13,
                     legend_box_y = 0.67,
                     gem_name = "Rotterdam",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[13]] <- list(loc_scale = "br",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.02,
                     bbox_padding_y_max = 0.15,
                     bbox_padding_x_min = 0.15,
                     bbox_padding_x_max = 0.02,
                     legend_box_x = 0.11,
                     legend_box_y = 0.67,
                     gem_name = "'s-Gravenhage",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[12]] <- list(loc_scale = "bl",
                     loc_arrow = "tr",
                     loc_scale_small = "bl",
                     bbox_padding_y_min = 0.1,
                     bbox_padding_y_max = 0.1,
                     bbox_padding_x_min = 0.15,
                     bbox_padding_x_max = 0.15,
                     legend_box_x = 0.12,
                     legend_box_y = 0.67,
                     gem_name = "Leiden",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[11]] <- list(loc_scale = "br",
                     loc_arrow = "tr",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0.02,
                     bbox_padding_y_max = 0.1,
                     bbox_padding_x_min = 0.1,
                     bbox_padding_x_max = 0.02,
                     legend_box_x = 0.12,
                     legend_box_y = 0.67,
                     gem_name = "Haarlem",
                     map_order_12 = NULL,
                     map_order_9 = NULL)

params[[10]] <- list(loc_scale = "br",
                     loc_arrow = "tl",
                     loc_scale_small = "br",
                     bbox_padding_y_min = 0,
                     bbox_padding_y_max = 0,
                     bbox_padding_x_min = -0.01,
                     bbox_padding_x_max = 0.07,
                     legend_box_x = 0.69,
                     legend_box_y = 0.67,
                     gem_name = "Amsterdam",
                     map_order_12 = c(2, 3, 1, 8, 5, 9, 4, 6, 7, 10, 11, 12),
                     map_order_9 = c(2, 3, 1, 8, 5, 9, 4, 6, 7))

params[[9]] <- list(loc_scale = "bl",
                     loc_arrow = "tl",
                    loc_scale_small = "bl",
                    bbox_padding_y_min = 0.02,
                    bbox_padding_y_max = 0.15,
                    bbox_padding_x_min = 0.02,
                    bbox_padding_x_max = 0.2,
                    legend_box_x = 0.69,
                    legend_box_y = 0.67,
                    gem_name = "Utrecht",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[8]] <- list(loc_scale = "bl",
                    loc_arrow = "tr",
                    loc_scale_small = "br",
                    bbox_padding_y_min = 0.1,
                    bbox_padding_y_max = 0.02,
                    bbox_padding_x_min = 0.02,
                    bbox_padding_x_max = 0.28,
                    legend_box_x = 0.67,
                    legend_box_y = 0.14,
                    gem_name = "Amersfoort",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[7]] <- list(loc_scale = "br",
                    loc_arrow = "tr",
                    loc_scale_small = "bl",
                    bbox_padding_y_min = 0.12,
                    bbox_padding_y_max = 0.04,
                    bbox_padding_x_min = 0.15,
                    bbox_padding_x_max = 0.04,
                    legend_box_x = 0.12,
                    legend_box_y = 0.14,
                    gem_name = "Nijmegen",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[6]] <- list(loc_scale = "br",
                    loc_arrow = "tr",
                    loc_scale_small = "bl",
                    bbox_padding_y_min = 0.02,
                    bbox_padding_y_max = 0.15,
                    bbox_padding_x_min = 0.08,
                    bbox_padding_x_max = 0.02,
                    legend_box_x = 0.13,
                    legend_box_y = 0.66,
                    gem_name = "Arnhem",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[5]] <- list(loc_scale = "bl",
                    loc_arrow = "tr",
                    loc_scale_small = "bl",
                    bbox_padding_y_min = 0.02,
                    bbox_padding_y_max = 0.1,
                    bbox_padding_x_min = 0.1,
                    bbox_padding_x_max = 0.02,
                    legend_box_x = 0.11,
                    legend_box_y = 0.67,
                    gem_name = "Apeldoorn",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[4]] <- list(loc_scale = "bl",
                    loc_arrow = "tr",
                    loc_scale_small = "br",
                    bbox_padding_y_min = 0.02,
                    bbox_padding_y_max = 0.2,
                    bbox_padding_x_min = 0.2,
                    bbox_padding_x_max = 0.02,
                    legend_box_x = 0.12,
                    legend_box_y = 0.67,
                    gem_name = "Enschede",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[3]] <- list(loc_scale = "bl",
                    loc_arrow = "tr",
                    loc_scale_small = "br",
                    bbox_padding_y_min = 0.3,
                    bbox_padding_y_max = 0.02,
                    bbox_padding_x_min = 0.02,
                    bbox_padding_x_max = 0.08,
                    legend_box_x = 0.51,
                    legend_box_y = 0.1,
                    gem_name = "Zwolle",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[2]] <- list(loc_scale = "bl",
                    loc_arrow = "tr",
                    loc_scale_small = "br",
                    bbox_padding_y_min = 0.15,
                    bbox_padding_y_max = 0.02,
                    bbox_padding_x_min = 0.02,
                    bbox_padding_x_max = 0.2,
                    legend_box_x = 0.68,
                    legend_box_y = 0.13,
                    gem_name = "Leeuwarden",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

params[[1]] <- list(loc_scale = "bl",
                    loc_arrow = "tr",
                    loc_scale_small = "bl",
                    bbox_padding_y_min = 0,
                    bbox_padding_y_max = 0.17,
                    bbox_padding_x_min = 0.17,
                    bbox_padding_x_max = 0,
                    legend_box_x = 0.11,
                    legend_box_y = 0.65,
                    gem_name = "Groningen",
                    map_order_12 = NULL,
                    map_order_9 = NULL)

# Legend labels 
legend_labels <- list()
for (i in 1:nclust) {
  legend_labels[[as.character(i)]] <- paste0("Cluster ", i, ": ", clust_pen_labs[[as.character(i)]])  
}
legend_labels[["other"]] <- "Transitioning sequences"  

# Pipeline
for (city_to_plot in 1:22) {
  seq_summary <- build_sequence_summary(clusters_sf, city_to_plot, "SEQ_CLUSTER", all_sequences = all_sequences)
  selected_sequences <- select_sequences(seq_summary, top_n = 12)
  df_city <- clusters_sf %>% filter(URBREG == city_to_plot)
  gem_sf_city <- gem_sf %>% filter(GM_NAAM == params[[city_to_plot]]$gem_name)
  urbreg_sf_city <- urbreg_sf %>% filter(URBREG == city_to_plot)
  
  center_map <- make_center_map(df_city,
                                landuse_sf = land_sf,
                                gem_sf = gem_sf_city,
                                urbreg_sf = urbreg_sf_city,
                                constant_sequences = as.character(1:nclust),
                                colour_palette = paste0("tol",nclust,"qualitative"),
                                other_colour = "grey75",
                                bbox_padding_y_min = params[[city_to_plot]]$bbox_padding_y_min,
                                bbox_padding_y_max = params[[city_to_plot]]$bbox_padding_y_max,
                                bbox_padding_x_min = params[[city_to_plot]]$bbox_padding_x_min,
                                bbox_padding_x_max = params[[city_to_plot]]$bbox_padding_x_max,
                                map_bg = "white",
                                show_border = FALSE,
                                loc_arrow = params[[city_to_plot]]$loc_arrow,
                                loc_scale = params[[city_to_plot]]$loc_scale
                                )

  cluster_legend <- make_cluster_legend(colour_palette = get(paste0("tol",nclust,"qualitative")),
                                        other_colour = "grey75",
                                        txt_colour = "black",
                                        bg_colour = "white",
                                        gem_name = params[[city_to_plot]]$gem_name,
                                        seq_summary = seq_summary,
                                        legend_labels = legend_labels)

  center_map_legend <- ggdraw() +
    draw_plot(center_map, 0, 0, 1, 1) +
    draw_plot(cluster_legend,
              x = params[[city_to_plot]]$legend_box_x,
              y = params[[city_to_plot]]$legend_box_y,
              width = 0.2,
              height = 0.2)

  small_maps_label <- purrr::map(selected_sequences, function(s){
    make_small_map(df_city,
                   gem_sf = gem_sf_city,
                   urbreg_sf = urbreg_sf_city,
                   sequence_id = s,
                   seq_clust_var_name = "SEQ_CLUSTER",
                   highlight_colour = "red",
                   other_colour = "grey75",
                   bbox_padding_y_min = params[[city_to_plot]]$bbox_padding_y_min,
                   bbox_padding_y_max = params[[city_to_plot]]$bbox_padding_y_max,
                   bbox_padding_x_min = params[[city_to_plot]]$bbox_padding_x_min,
                   bbox_padding_x_max = params[[city_to_plot]]$bbox_padding_x_max,
                   map_bg = "white",
                   label_size = 8,
                   txt_colour = "black",
                   gem_colour = "black",
                   show_pct = TRUE,
                   show_label = TRUE,
                   loc_arrow = params[[city_to_plot]]$loc_arrow,
                   loc_scale = params[[city_to_plot]]$loc_scale_small,
                   )
  })

  p_small_maps <- assemble_small_maps(small_maps_label, seq_summary, map_order = params[[city_to_plot]]$map_order_9)

  outfile <- file.path(path_results, "figures", "sequences", "maps", paste0("seq_cluster_top9_urbreg_", city_to_plot,".png"))
  ggsave(outfile, p_small_maps, width = 7, height = 5.2, units = "in", dpi = 600, limitsize = FALSE)
  
  outfile <- file.path(path_results, "figures", "sequences", "maps", paste0("seq_cluster_top9_urbreg_", city_to_plot, ".tiff"))
  ggsave(outfile, p_small_maps, width = 7, height = 5.2, units = "in", bg = "white", dpi = 600, limitsize = FALSE, device = "tiff")
  
  outfile <- file.path(path_results, "figures", "sequences", "maps", paste0("seq_cluster_cons_urbreg_", city_to_plot,".png"))
  ggsave(outfile, center_map_legend, width = 7, height = 5.2, units = "in", dpi = 600, limitsize = FALSE)
  
  outfile <- file.path(path_results, "figures", "sequences", "maps", paste0("seq_cluster_cons_urbreg_", city_to_plot,".tiff"))
  ggsave(outfile, center_map_legend, width = 7, height = 5.2, units = "in", bg = "white", dpi = 600, limitsize = FALSE, device = "tiff")

}
