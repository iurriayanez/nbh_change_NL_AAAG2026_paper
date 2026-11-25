#------------------------------------------------------------------------------#
# Name: 4_nbh_cluster_final_plots.R
# Goal: Sociospatial signature clusters final clustergram and distance profiles
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_final_std_cluster_assignment_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#                  data/nbh/clust_metrics_[std_method]_mbkmeans_metaggl_all.rds
#                  data/nbh/data_scaled_[std_method]_for_clustering_metaggl_all.rds
# Output databases: 
#------------------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# 0. Setup ----
#-----------------------------------------------------------------------#
source(file.path(path_code, "functions.R"), echo = F)
#-----------------------------------------------------------------------#
## 0.1 Libraries ----
#-----------------------------------------------------------------------#

library(ggplot2)
library(dplyr)
library(data.table)
library(readr)
library(sf)

#-----------------------------------------------------------------------#
## 0.2 Seed ----
#-----------------------------------------------------------------------#

set.seed(1409)

#-----------------------------------------------------------------------#
## 0.3 Parameters ----
#-----------------------------------------------------------------------#
nclust <- 9
clust_method <- "mbkmeans"

#-----------------------------------------------------------------------#
# 1. Clustergram and metrics ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 1.1 Import data ----
#-----------------------------------------------------------------------#
minibkmeans <- read_rds(file.path(path_nbh, paste0("clust_metrics_", std_method, "_mbkmeans_metaggl_",met_aggl_labs_save, ".rds")))
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
## 1.2 Metrics ----
#-----------------------------------------------------------------------#

minikmeans_sil_plot <- plot_clust_metric(minibkmeans, "Silhouette", "Silhouette", mark_k = nclust)
minikmeans_ch_plot <- plot_clust_metric(minibkmeans, "Calinski_Harabasz", "Calinski Harabasz", mark_k = nclust)
minikmeans_db_plot <- plot_clust_metric(minibkmeans, "Davies_Bouldin", "Davies Bouldin", mark_k = nclust)

png(
  file.path(path_results, 
            "figures", 
            "cluster_metrics", 
            paste0(
              "cluster_metrics_", 
              std_method, 
              "_mbkmeans_k",
              nclust,
              "_",
              met_aggl_labs_save,
              ".png"
            )
  ),
  width = 7, 
  height = 2.2,
  unit = "in",
  res = 600
)

gridExtra::grid.arrange(grobs = list(minikmeans_sil_plot,
                                     minikmeans_ch_plot,
                                     minikmeans_db_plot),
                        nrow = 1)

dev.off()

metrics <- gridExtra::grid.arrange(grobs = list(minikmeans_sil_plot,
                                     minikmeans_ch_plot,
                                     minikmeans_db_plot),
                        nrow = 1)

#-----------------------------------------------------------------------#
## 1.3 clustergram ----
#-----------------------------------------------------------------------#

cluster_info <- list()
for(k in 2:30) {
  cluster_info[[as.character(k)]]$centers <- minibkmeans[[as.character(k)]]$centroids
  cluster_info[[as.character(k)]]$clusters <- minibkmeans[[as.character(k)]]$cluster_assign
}

for (factor_sel in 1:2) {
  png(
    file.path(path_results, "figures", "clustergram", paste0("clustergram_", std_method, "_minibkmeans_k", nclust, "_PCA", factor_sel, "_", met_aggl_labs_save,".png")),
    width = 7, 
    height = 5.2,
    unit = "in",
    res = 600
    )
  clustergram(
    Data = df_clust, 
    cluster_info = cluster_info,
    k.range = 2:20, 
    pca.loading = factor_sel,
    ylab = paste0("PCA weighted mean of the clusters (PCA Factor: ", factor_sel, ")"),
    max_line_width = 12,
    mark_k = nclust,
    lty_mark = "dotdash",
    lwd_mark = 3
  )
  dev.off()
}

plot1 <- png::readPNG(file.path(path_results, "figures", "clustergram", paste0("clustergram_", std_method, "_minibkmeans_k", nclust, "_PCA2_", met_aggl_labs_save, ".png")))
plot2 <- png::readPNG(file.path(path_results, "figures", "cluster_metrics", paste0("cluster_metrics_", std_method, "_mbkmeans_k", nclust, "_", met_aggl_labs_save, ".png")))

p <- gridExtra::grid.arrange(grid::rasterGrob(plot1), grid::rasterGrob(plot2), ncol = 1, heights = c(0.7, 0.3))
ggsave(
  file.path(path_results, "figures", "clustergram", paste0("clustergram_and_metrics_", std_method, "_minibkmeans_k", nclust, "_", met_aggl_labs_save,".png")),
  plot = p,
  width = 7,
  height = 7,
  bg = "white",
  dpi = 600,
  units = "in"
)

ggsave(
  file.path(path_results, "figures", "clustergram", paste0("clustergram_and_metrics_", std_method, "_minibkmeans_k", nclust, "_", met_aggl_labs_save,".tiff")),
  plot = p,
  width = 7,
  height = 7,
  bg = "white",
  dpi = 600,
  units = "in",
  device = "tiff"
)

ggsave(
  file.path(path_results, "figures", "clustergram", paste0("clustergram_and_metrics_", std_method, "_minibkmeans_k", nclust, "_", met_aggl_labs_save,".eps")),
  plot = p,
  width = 7,
  height = 7,
  bg = "white",
  dpi = 600,
  units = "in",
  device = "eps"
)

rm(df_scale, df_clust, cluster_info, minibkmeans)
gc()

#-----------------------------------------------------------------------#
# 2. Distance profile (grid plot) ----
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 2.1 Setup ----
#-----------------------------------------------------------------------#
# Load data 
rds_file <- paste0("data_final_std_cluster_assignment_", std_method, "_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")
df <- read_rds(file = file.path(path_nbh,rds_file))

# Lists with variable names and lookups
# All variables 
all_cols <- c(
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

# Lists to change datatable from wide to long
list_all_cols_num <- list(
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
  c(grep("^PERC_ETHEUR", names(df), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df), value = TRUE))
)

list_clust_cols_num <- list(
  # Ethnic background
  c(grep("^PERC_ETHEUR", names(df), value = TRUE)),
  c(grep("^PERC_ETHNONEUR", names(df), value = TRUE)),
  # Age groups 
  c(grep("^PERC_AGE_GROUP_1", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_2", names(df), value = TRUE)),
  c(grep("^PERC_AGE_GROUP_7", names(df), value = TRUE)),
  # Socioeconomic variables 
  c(grep("^PERC_HH_LOWINC", names(df), value = TRUE)),
  c(grep("^PERC_HH_HIGHINC", names(df), value = TRUE)),
  # Housing variables 
  c(grep("^PERC_HH_HOMEOWNER", names(df), value = TRUE)),
  c(grep("^PERC_HH_SUBRENTAL", names(df), value = TRUE))
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

# Create final dataframe used for the plots

# Scale 
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

# Dimensions
df_clust_mean_long[, DIM := 
  ifelse(variable %in% c(grep("^PERC_ETH", unique(df_clust_mean_long$variable), value = TRUE)), "Mig. background",
         ifelse(variable %in% c(grep("^PERC_AGE", unique(df_clust_mean_long$variable), value = TRUE)), "Age",
                ifelse(variable %in% c(grep("INC$", unique(df_clust_mean_long$variable), value = TRUE)), "Income", "Tenure")))
]

#-----------------------------------------------------------------------#
## 2.2 Plot ----
#-----------------------------------------------------------------------#

# Line plot facetted by dimension 

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
    nudge_x_label = 0.2,
    nudge_y_label = 0,
    label_size = 2,
    palette = c(colors_dim_3b, colors_dim_4p, colors_dim_3o, colors_dim_3g)
  )
  
  p <- p + 
    facet_grid(~factor(DIM, levels = c('Income', 'Mig. background', 'Age', 'Tenure')))
  
  p <- p +
    theme(
      axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
      axis.title.x = element_text(size = 8),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 8),
      plot.title = element_text(size = 12),
      strip.text.x = element_text(size = 10)
    )
  
  ggsave(
    file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_facet_", std_method, "_all_vars_std_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, "_cluster", i, ".png")),
    plot = p,
    width = 7,
    height = 3,
    units = "in",
    bg = "white",
    dpi = 300
  )
  
  ggsave(
    file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_facet_", std_method, "_all_vars_std_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, "_cluster", i, ".tiff")),
    plot = p,
    width = 7,
    height = 3,
    units = "in",
    device = "tiff",
    dpi = 600,
    bg = "white"
  )

  ggsave(
    file.path(path_results, "figures", "cluster_distprof", paste0("ave_distprof_facet_", std_method, "_all_vars_std_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, "_cluster", i, ".eps")),
    plot = p,
    width = 7,
    height = 3,
    units = "in",
    device = "eps",
    dpi = 600,
    bg = "white"
  )
  
  assign(paste0("p", i), p)
  
}