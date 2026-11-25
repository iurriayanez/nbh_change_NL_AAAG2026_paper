#------------------------------------------------------------------------------#
# Name: 6_nbh_seq_analysis_rep_plots.R
# Goal: Plot sequence cluster representative trajectories
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#               
# Output databases: 
#------------------------------------------------------------------------------#

# 0. Setup ----
source(file.path(path_code, "functions.R"), echo = F)
## 0.1 Libraries ----
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c("readr", "ggplot2", "cowplot", "dplyr", "data.table", "TraMineR", "WeightedCluster", "ggseqplot")
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

## 0.2 Seed ----
set.seed(1409)

## 0.3 Parameters ----
clust_method <- "mbkmeans"
nclust <- 9
diss_params = list(method = "OMstran", sm = "CONSTANT", indel = 1, transindel = "constant", otto = 0.1, previous = FALSE)
seqnclust <- 46
param_name <- "OMstran_sm2_i1_tri1_w01_previousF"

## 0.4 Helpers ----
# sequence clusters labels dictionary
seq_labs <- list(
  # constant trajectories
  "1"  = paste("Stable", tolower(clust_pen_labs[["1"]])),
  "31" = paste("Stable", tolower(clust_pen_labs[["2"]])),
  "26" = paste("Stable", tolower(clust_pen_labs[["3"]])),
  "27" = paste("Stable", tolower(clust_pen_labs[["4"]])),
  "12" = paste("Stable", tolower(clust_pen_labs[["5"]])),
  "28" = paste("Stable", tolower(clust_pen_labs[["6"]])),
  "34" = paste("Stable", tolower(clust_pen_labs[["7"]])),
  "14" = paste("Stable", tolower(clust_pen_labs[["8"]])),
  "7"  = paste("Stable", tolower(clust_pen_labs[["9"]])),
  
  # scalar variability
  "39" = paste(clust_pen_labs[["1"]], "to \n", tolower(clust_pen_labs[["4"]])), 
  "6"  = paste(clust_pen_labs[["1"]], "to \n", tolower(clust_pen_labs[["5"]])), 
  "45" = paste(clust_pen_labs[["1"]], "to \n", tolower(clust_pen_labs[["8"]])), 
  "2"  = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "38" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "10" = paste(clust_pen_labs[["5"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "22" = paste(clust_pen_labs[["6"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "37" = paste(clust_pen_labs[["7"]], "to \n", tolower(clust_pen_labs[["4"]])), 
  "5"  = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["1"]])), 
  "44" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["4"]])), 
  "35" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["7"]])), 
  "3"  = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["1"]])),  
  "9"  = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["2"]])), 
  
  # changes across scales 
  "32" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["3"]])),
  "30" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["8"]])),
  "40" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["9"]])),
  "8"  = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["2"]])),
  "15" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["5"]])), 
  "43" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["8"]])),
  "41" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["9"]])),
  "11" = paste(clust_pen_labs[["5"]], "to \n", tolower(clust_pen_labs[["3"]])),   
  "4"  = paste(clust_pen_labs[["5"]], "to \n", tolower(clust_pen_labs[["9"]])), 
  "23" = paste(clust_pen_labs[["6"]], "to \n", tolower(clust_pen_labs[["2"]])), 
  "33" = paste(clust_pen_labs[["6"]], "to \n", tolower(clust_pen_labs[["8"]])),
  "13" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["3"]])),
  "18" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["3"]])), 
  "21" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["5"]])),
  
  # multiple transitions
  "24" = paste(clust_pen_labs[["2"]], "to \n", tolower(clust_pen_labs[["3"]]), "and back"),
  "25" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["2"]]), "and back"),
  "17" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["2"]]), "to ageing elite"),
  "42" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["5"]]), "and back"),
  "29" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["9"]]), "to ageing middle-class"), 
  "20" = paste(clust_pen_labs[["3"]], "to \n", tolower(clust_pen_labs[["9"]]), "and back"),
  "46" = paste("Changes between", tolower(clust_pen_labs[["3"]]), "\n and", tolower(clust_pen_labs[["9"]])),
  "36" = paste(clust_pen_labs[["8"]], "to \n", tolower(clust_pen_labs[["1"]]), "and back"),
  "19" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["2"]]), "and back"),
  "16" = paste(clust_pen_labs[["9"]], "to \n", tolower(clust_pen_labs[["3"]]), "and back")
  
)

#------------------------------------------------------------------------------#
# 1. Set dataframes ----
read_file <- paste0(
  "data_final_seq_cluster_assignment_claraseqclust_",
  param_name,
  "_k",
  seqnclust,
  "_",
  std_method,
  "_",
  clust_method,
  "_k",
  nclust,
  "_metaggl_",
  met_aggl_labs_save,
  ".rds"
)

df <- read_rds(
  file.path(
    path_nbh,
    read_file
  )
)

# Define sequence database 
df_seq <- df %>%
  relocate(SEQ_CLUSTER, .before = years[[1]]) %>% 
  seqdef(var = 3:ncol(df),
         cpal = get(paste0("tol",nclust,"qualitative")),
         labels = paste("Cluster", 1:nclust)
  )

# Create weighted sequence object
df_seq_agg <- wcAggregateCases(df_seq)
df_seq_w <- seqdef(df_seq[df_seq_agg$aggIndex,], 
                   labels = attr(df_seq, "labels"),
                   cpal = attr(df_seq, "cpal"),
                   weights = df_seq_agg$aggWeights)


#------------------------------------------------------------------------------#
# 2. Set dissimirality matrix ----

## 2.1.1 Define subcost matrix and indel costs ----

subcost_mat <- seqcost(
    df_seq, 
    method = diss_params$sm,
  )$sm

indel_cost <- diss_params$indel

  
### 2.1.2 Define seqdist arguments ----
# Build seqdist arguments
seqdist_args <- list(
  seqdata = df_seq_w,
  method = diss_params$method,
  sm = subcost_mat,
  indel = indel_cost
)
if (!is.null(diss_params$transindel)) seqdist_args$transindel <- diss_params$transindel
if (!is.null(diss_params$otto)) seqdist_args$otto <- diss_params$otto
if (!is.null(diss_params$previous)) seqdist_args$previous <- diss_params$previous
  
 
### 2.1.3 Dissimilarity matrix ----
diss_matrix <- do.call(seqdist, seqdist_args)

#------------------------------------------------------------------------------#
# 3. Plot representative sequences ----
sel_cluster <- df$SEQ_CLUSTER
sel_cluster_w <- sel_cluster[df_seq_agg$aggIndex]

##  For plotting with ggseqplot
names(attr(df_seq, "cpal")) <- paste("Cluster", 1:nclust)
attr(df_seq_w, "cpal") <- attr(df_seq, "cpal")

## 3.1 Plot all sequences ---
p <- ggseqrplot(
  df_seq_w,
  group = sel_cluster_w,
  diss = diss_matrix,
  criterion = "dist",
  coverage = 0.75,
  stats = F,
  facet_ncol = 9,
  proportional = TRUE,
  weighted = TRUE,
  border = FALSE
)

# Extract legend (for selected plots)
my_legend <- cowplot::get_legend(p + theme(legend.position = "bottom"))

# Number of sequences by cluster 
n_seq_cluster <- table(sel_cluster)

## 3.2 Scalar variability ----
txt_size = 10
title_size = 9
subtitle_size = 7

p_nc_1_4 <- plot_cluster_r_seq(39, "1 → 4", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_1_5 <- plot_cluster_r_seq(6, "1 → 5", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_1_8 <- plot_cluster_r_seq(45, "1 → 8", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_2_1 <- plot_cluster_r_seq(2, "2 → 1", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_1 <- plot_cluster_r_seq(38, "3 → 1", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_5_1 <- plot_cluster_r_seq(10, "5 → 1", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_6_1 <- plot_cluster_r_seq(22, "6 → 1", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_7_4 <- plot_cluster_r_seq(37, "7 → 4", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_8_1 <- plot_cluster_r_seq(5, "8 → 1", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_8_4 <- plot_cluster_r_seq(44, "8 → 4", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_8_7 <- plot_cluster_r_seq(35, "8 → 7", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_9_1 <- plot_cluster_r_seq(3, "9 → 1", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_9_2 <- plot_cluster_r_seq(9, "9 → 2", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)

# Create plot with regular legend (no constant N)
labels_legend <- c(
  "Cluster 1",
  "Cluster 2",
  "Cluster 3",
  "Cluster 4",
  "Cluster 5",
  "Cluster 6",
  "Cluster 7",
  "Cluster 8",
  "Cluster 9"
)

values_legend <-  c('#332288','#88ccee','#44aa99','#117733','#999933','#ddcc77','#cc6677', '#882255','#AA4499')

plot_legend <- ggseqdplot(df_seq_w) +
  patchwork::plot_layout(guides = "collect") +
  scale_fill_manual(
    values = values_legend,
    labels = labels_legend 
  ) +
  scale_colour_manual(
    values = values_legend,
    labels = labels_legend
  ) +
  guides(
    colour = guide_legend(nrow = 3, ncol = 3, byrow = FALSE),
    fill = guide_legend(nrow = 3, ncol = 3, byrow = FALSE)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.key.spacing.x = unit(0.5, "cm")
  ) 

my_legend <- cowplot::get_legend(
  plot_legend
)

plot_grid_matrix_scalar_below <- cowplot::plot_grid(
  p_nc_9_2, my_legend,
  align = "v", axis = "t",
  rel_widths = c(1, 2),
  ncol = 2
)

final_plot_scalar <- cowplot::plot_grid(
  plot_grid_matrix_scalar_above,
  plot_grid_matrix_scalar_below,
  ncol = 1,
  rel_heights = c(4,1)
)


ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    paste0(
      "rep_sequences_scalar_seq",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".jpg"
    )
  ),
  plot = final_plot_scalar,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 300
)


ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    paste0(
      "rep_sequences_scalar_seq",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".eps"
    )
  ),
  plot = final_plot_scalar,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 600,
  device = "eps"
)

## 3.3 Changes across all scales ----
txt_size = 10
title_size = 9
subtitle_size = 7
p_nc_2_3 <- plot_cluster_r_seq(32, "2 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_2_8 <- plot_cluster_r_seq(30, "2 → 8", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size) 
p_nc_2_9 <- plot_cluster_r_seq(40, "2 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_2 <- plot_cluster_r_seq(8, "3 → 2", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size) 
p_nc_3_5 <- plot_cluster_r_seq(15, "3 → 5", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_8 <- plot_cluster_r_seq(43, "3 → 8", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_9 <- plot_cluster_r_seq(41, "3 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_5_3 <- plot_cluster_r_seq(11, "5 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)  
p_nc_5_9 <- plot_cluster_r_seq(4, "5 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_6_2 <- plot_cluster_r_seq(23, "6 → 2", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_6_8 <- plot_cluster_r_seq(33, "6 → 8", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size) 
p_nc_8_3 <- plot_cluster_r_seq(13, "8 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_9_3 <- plot_cluster_r_seq(18, "9 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_9_5 <- plot_cluster_r_seq(21, "9 → 5", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)

plot_grid_matrix_dim_above <- cowplot::plot_grid(
  p_nc_2_3, p_nc_2_8, p_nc_2_9, 
  p_nc_3_2, p_nc_3_5, p_nc_3_8, 
  p_nc_3_9, p_nc_5_3, p_nc_5_9, 
  p_nc_6_2, p_nc_6_8, p_nc_8_3,
  ncol = 3
)


plot_legend <- ggseqdplot(df_seq_w) +
  patchwork::plot_layout(guides = "collect") +
  scale_fill_manual(
    values = values_legend,
    labels = labels_legend 
  ) +
  scale_colour_manual(
    values = values_legend,
    labels = labels_legend
  ) +
  guides(
    colour = guide_legend(nrow = 3, ncol = 3, byrow = FALSE),
    fill = guide_legend(nrow = 3, ncol = 3, byrow = FALSE)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.key.size = unit(0.7, "cm"),
    legend.key.spacing.x = unit(0.4, "cm")
  ) 

my_legend <- cowplot::get_legend(
  plot_legend
)

plot_grid_matrix_dim_below <- cowplot::plot_grid(
  p_nc_9_3, p_nc_9_5, my_legend,
  ncol = 3, align = "v", axis = "t",
  rel_widths = c(1,1,1)
)

final_plot_dim <- cowplot::plot_grid(
  plot_grid_matrix_dim_above,
  plot_grid_matrix_dim_below,
  ncol = 1,
  rel_heights = c(4, 1)
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    paste0(
      "rep_sequences_dim_seq",
      "_std_",
      clust_method,
      "_k",
      ".jpg"
    )
  ),
  plot = final_plot_dim,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 300
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    paste0(
      "rep_sequences_dim_seq",
      "_std_",
      clust_method,
      "_k",
      ".eps"
    )
  ),
  plot = final_plot_dim,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 600,
  device = "eps"
)

## 3.4 Multiple transitions ----
txt_size = 10
title_size = 9
subtitle_size = 7

p_nc_2_3_2 <- plot_cluster_r_seq(24, "2 → 3 → 2", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_2_3 <- plot_cluster_r_seq(25, "3 → 2 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size) 
p_nc_3_2_9 <-plot_cluster_r_seq(17, "3 → 2 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_5_3 <- plot_cluster_r_seq(42, "3 → 5 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_9_2 <- plot_cluster_r_seq(29, "3 → 9 → 2", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)  
p_nc_3_9_3 <-plot_cluster_r_seq(20, "3 → 9 → 3", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_3_9_3_9 <- plot_cluster_r_seq(46, "3 → 9 → 3 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_8_1_8 <- plot_cluster_r_seq(36, "8 → 1 → 8", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_9_3_9 <- plot_cluster_r_seq(19, "9 → 3 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)
p_nc_9_2_9 <- plot_cluster_r_seq(16, "9 → 2 → 9", title_size = title_size, subtitle_size = subtitle_size, txt_size = txt_size)


plot_grid_matrix_multi_above <- cowplot::plot_grid(
  p_nc_2_3_2, p_nc_3_2_3, p_nc_3_2_9, 
  p_nc_3_5_3, p_nc_3_9_2, p_nc_3_9_3, 
  p_nc_3_9_3_9, p_nc_8_1_8, p_nc_9_3_9, 
  ncol = 3
)

# With legend with constant N values
plot_legend_cons <- plot_legend_cons +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold", margin = margin(b = 0.2, unit = "cm")),
    legend.key.size = unit(1, "cm"),
    legend.key.spacing.x = unit(0.5, "cm")
  ) 

my_legend_cons <- cowplot::get_legend(
  plot_legend_cons
)

plot_grid_matrix_multi_below_cons <- cowplot::plot_grid(
  p_nc_9_2_9, my_legend_cons,
  ncol = 2, align = "v", axis = "t",
  rel_widths = c(1,2)
)

final_plot_multi <- cowplot::plot_grid(
  plot_grid_matrix_multi_above,
  plot_grid_matrix_multi_below_cons,
  ncol = 1,
  rel_heights = c(3, 1)
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    paste0(
      "rep_sequences_multi_seq_conslegend",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".jpg"
    )
  ),
  plot = final_plot_multi,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 300
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    paste0(
      "rep_sequences_multi_seq_conslegend",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".eps"
    )
  ),
  plot = final_plot_multi,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 600,
  device = "eps"
)