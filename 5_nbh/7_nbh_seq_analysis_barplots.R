#------------------------------------------------------------------------------#
# Name: 7_nbh_seq_analysis_barplots.R
# Goal: Plot sequence clusters distributions by urban region using barplots
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#               
# Output databases: 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 0. Setup ----
#------------------------------------------------------------------------------#
source(file.path(path_code, "functions.R"), echo = F)
#------------------------------------------------------------------------------#
## 0.1 Libraries ----
#------------------------------------------------------------------------------#

pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c("readr", "ggplot2", "dplyr", "data.table", "TraMineR", "WeightedCluster", "ggseqplot")
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

#------------------------------------------------------------------------------#
## 0.2 Seed ----
#------------------------------------------------------------------------------#
set.seed(1409)

#------------------------------------------------------------------------------#
## 0.3 Parameters ----
#------------------------------------------------------------------------------#

clust_method <- "mbkmeans"
nclust <- 9
diss_params = list(method = "OMstran", sm = "CONSTANT", indel = 1, transindel = "constant", otto = 0.1, previous = FALSE)
seqnclust <- 46
param_name <- "OMstran_sm2_i1_tri1_w01_previousF"

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

# Scalar variability 
seq_cluster_scale <- c(
  "1→4",
  "1→5",
  "1→8",
  "2→1", 
  "3→1",
  "5→1",
  "6→1",
  "7→4",
  "8→1",
  "8→7",
  "8→4",
  "9→1",
  "9→2"
)

# Dimension variability 
seq_cluster_dim <- c(
  "2→3",
  "2→8",
  "2→9",
  "3→2",
  "3→5",
  "3→8",
  "3→9",
  "5→3",
  "5→9",
  "6→2",
  "6→8",
  "8→3",
  "9→3",
  "9→5"
)

# Urban regions 
lookup_urbreg <- data.table(
  URBREG = seq(1:22),
  region_group = c("North", "North", 
                   "East", "East", "East", "East", "East", 
                   "West", "West", "West", "West", "West", "West", "West", "West",
                   "South", "South", "South", "South",
                   "South-East", "South-East", "South-East")
) 

#------------------------------------------------------------------------------#
# 1. Set dataframes ----
#------------------------------------------------------------------------------#

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


# Add urban region and population (2023)
rds_file <- paste0("data_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
df_urbreg <- read_rds(file.path(path_nbh,rds_file))
df_urbreg <- df_urbreg[YEAR == 2023, c("VIERKANT100M", "URBREG", "TOT_IND"), with = FALSE]
df_urbreg <- unique(df_urbreg)

df <- merge(df, df_urbreg, by = "VIERKANT100M", all.x= TRUE, all.y = FALSE)
rm(df_urbreg)
gc()

#------------------------------------------------------------------------------#
# 2. Plot heatmaps ----
#------------------------------------------------------------------------------#

# Cluster description 
lab_df <- data.frame(
  id = seq_len(nclust),
  label = clust_pen_labs[seq_len(nclust)]
)
lab_df$y <- ifelse(lab_df$id %% 2 == 1, -0.1, -0.2)
lab_df$col <- ceiling(lab_df$id / 2)
lab_df$x <- 1 + (lab_df$col - 1) * 0.7
lab_df$text <- paste0(lab_df$id, ": ", lab_df$label)

caption_plot <- ggplot(lab_df, aes(x = x, y = y, label = text)) +
  geom_text(hjust = 0, size = 1.95) +
  annotate(
    "text",
    x = 1,
    y = -0.05,
    label = "Cluster description",
    hjust = 0,
    vjust = 0,
    fontface = "bold",
    size = 2.5
  ) + 
  scale_x_continuous(
    limits = c(1, max(lab_df$x) + 0.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(-0.25, 0),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_void()

#------------------------------------------------------------------------------#
## 2.1 Share of total grid cells in the urban region ----
#------------------------------------------------------------------------------#

# Table of number of grid cells by sequence cluster and each urban region
tab <- df[, .N, by = .(URBREG, SEQ_CLUSTER)]
tab <- dcast(tab, URBREG ~ SEQ_CLUSTER, value.var = "N", fill = 0)

# Table of number of grid cells by sequence cluster and each urban region
tab_p <- df[, .N, by = .(URBREG, SEQ_CLUSTER)]
tab_p <- tab_p[, total := sum(N), by = URBREG]
tab_p  <- tab_p[, perc := 100 * N / total] 

tab_p[, SEQ_CLUSTER := factor(
  SEQ_CLUSTER, 
  levels = new_seq_cluster_order, 
  labels = new_seq_cluster_labels)]
lvl <- levels(tab_p$SEQ_CLUSTER)

tab_p <- merge(tab_p, lookup_urbreg, by = "URBREG", all.x = TRUE) 

tab_p[, URBREG := factor(
  URBREG,
  labels = urbreg_labels
  )]

tab_p[, region_group := factor(
  region_group,
  levels = c("North", "East", "West", "South", "South-East")
)]

tab_p <- tab_p %>% 
  mutate(
    region_code = as.numeric(URBREG),
    URBREG = forcats::fct_reorder(URBREG, -region_code)
  )

# Select Non-Constant Sequences
tab1 <- tab_p[SEQ_CLUSTER %in% lvl[10:46]]

# Calculate the percentage of non-constant sequences respectively
tab1[, total_g := sum(N), by = URBREG]
tab1[, perc_g := 100 * total_g / total]
tab1[, perc_w := 100 * N / total_g]

# Max percentage of non-constant sequences (to fix limit of legend)
max_limit_nc <- max(tab1$perc)

# Select sequences with scalar variability 
tab2 <- tab1[SEQ_CLUSTER %in% seq_cluster_scale]

# Select sequences with dimension variability 
tab3 <- tab1[SEQ_CLUSTER %in% seq_cluster_dim]

# Select sequences with more than 1 transition
tab4 <- tab1[!SEQ_CLUSTER %in% union(seq_cluster_scale, seq_cluster_dim)]

# All together 
p2 <- plot_heatmap_seq_clusters(tab2, "perc", "", "", max_limit = max_limit_nc)
p3 <- plot_heatmap_seq_clusters(tab3, "perc", "", "", max_limit = max_limit_nc)
p4 <- plot_heatmap_seq_clusters(tab4, "perc", "", "", max_limit = max_limit_nc)

my_legend <- cowplot::get_legend(p2)

p2 <- p2 + theme(
  legend.position = "none", 
  title = element_blank(), 
  subtitle = element_blank(),
  axis.text.x = element_text(angle = 45, vjust = 0.2, hjust = 0.1, face = "bold", size = 6),
  axis.text.y = element_text(face = "bold", size = 6),
  strip.text.y = element_text(face = "bold", size = 6)
) 
p3 <- p3 + theme(
  legend.position = "none", 
  title = element_blank(), 
  subtitle = element_blank(),
  axis.text.x = element_text(angle = 45, vjust = 0.2, hjust = 0.1, face = "bold", size = 6),
  axis.text.y = element_blank(),
  strip.text.y = element_blank()
)
p4 <- p4 + theme(
  legend.position = "none", 
  title = element_blank(), 
  subtitle = element_blank(), 
  axis.text.x = element_text(angle = 45, vjust = 0.2, hjust = 0.2, face = "bold", size = 6),
  axis.text.y = element_blank(),
  strip.text.y = element_blank()
)

plot_seq_nc <- cowplot::plot_grid(
  p2, p3, p4,
  nrow = 1, 
  align = "h",
  rel_widths = c(0.425, 0.325, 0.25),
  labels = c("Changes with scalar variability", "Changes across all scales", "Multiple transitions"),
  hjust = c(-0.5, -0.18, -0.2),
  vjust = c(3, 3, 3),
  label_size = 9
)
final_plot <- cowplot::plot_grid(
  plot_seq_nc,
  my_legend,
  ncol = 2,
  rel_widths = c(0.92, 0.07)
)
final_plot2 <- cowplot::plot_grid(
  final_plot,
  caption_plot,
  ncol = 1,
  rel_heights = c(0.9, 0.1)
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    "urbreg",
    paste0(
      "heatmap_seq_cluster_by_urbreg_tot_grid_",
      "nocons",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".jpg"
    )
  ),
  plot = final_plot2,
  width = 7,
  height = 5.2,
  units = "in",
  dpi = 300
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    "urbreg",
    paste0(
      "heatmap_seq_cluster_by_urbreg_tot_grid_",
      "nocons",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".tiff"
    )
  ),
  plot = final_plot2,
  width = 7,
  height = 5.2,
  units = "in",
  bg = "white",
  dpi = 600,
  device = "tiff"
)

#-------------------------------------#
# 3. Plot bar charts (stacked) ----
#-------------------------------------#

## 3.1 Share of total grid cells in the urban region ----
tab_p <- df[, .N, by = .(URBREG, SEQ_CLUSTER)]
tab_p <- tab_p[, total := sum(N), by = URBREG]
tab_p  <- tab_p[, perc := 100 * N / total] 

tab_p <- df[, .N, by = .(URBREG, SEQ_CLUSTER)]
tab_p <- tab_p[, total := sum(N), by = URBREG]
tab_p  <- tab_p[, perc := 100 * N / total] 

tab_p[, SEQ_CLUSTER := factor(
  SEQ_CLUSTER, 
  levels = new_seq_cluster_order, 
  labels = new_seq_cluster_labels)]
lvl <- levels(tab_p$SEQ_CLUSTER)

tab_p <- merge(tab_p, lookup_urbreg, by = "URBREG", all.x = TRUE) 

tab_p[, URBREG := factor(
  URBREG,
  labels = urbreg_labels
)]

tab_p[, region_group := factor(
  region_group,
  levels = c("North", "East", "West", "South", "South-East")
)]

tab_p <- tab_p %>% 
  mutate(
    region_code = as.numeric(URBREG),
    URBREG = forcats::fct_reorder(URBREG, -region_code)
  )

# Select Constant Sequences
tab1 <- tab_p[SEQ_CLUSTER %in% lvl[1:9]]
tab2 <- tab_p[SEQ_CLUSTER %in% lvl[10:46]]

# Calculate the percentage of constant and non-constant sequences respectively
tab1[, total_g := sum(N), by = URBREG]
tab1[, perc_g := 100 * total_g / total]
tab1[, perc_w := 100 * N / total_g]

tab2[, total_g := sum(N), by = URBREG]
tab2[, perc_g := 100 * total_g / total]
tab2[, perc_w := 100 * N / total_g]

### Constant sequences 
max_limit <- tab1 %>% 
  group_by(URBREG) %>% 
  summarise(tot_perc = sum(perc)) %>% 
  select(tot_perc) %>% 
  max() %>% 
  round(., 0)

thr <- 0.5
tab1[, value_text := ifelse(perc >= thr, sprintf("%.0f", perc), "")]
tab1$SEQ_CLUSTER <- factor(tab1$SEQ_CLUSTER, levels = 1:nclust)

# Labels
legend_labels <- list()
for (i in 1:nclust) {
  legend_labels[[as.character(i)]] <- paste0("Cluster ", i, ": ", clust_pen_labs[[as.character(i)]])  
}

# plot
p1 <- ggplot(data = tab1, aes(x = URBREG, y = perc, fill = forcats::fct_rev(SEQ_CLUSTER))) +
  geom_col(width = 0.8) +
  facet_grid(region_group~., scales = "free", space = "free_y", switch = "y") +
  coord_flip() +
  geom_text(
    aes(label = value_text),
    color = "white",
    position = position_stack(vjust = 0.5),
    inherit.aes = TRUE,
    size = 2
  ) +
  scale_fill_manual(
    values = get(paste0("tol", nclust, "qualitative")),
    labels = legend_labels,
    name = "Stable trajectories"
  ) +
  scale_x_discrete(drop = TRUE, expand = c(0,0)) +
  scale_y_continuous(
    limits = c(0,max_limit+1),
    breaks = seq(0,round(max_limit/10,0)*10, by = 10),
    expand = c(0,0)
  ) +
  labs(
    y = "Percentage of the urban region's 100x100m grid cells (%)", 
    #y = "Urban Region", 
    fill = ""
  ) +
  theme_bw() +
  guides(fill = guide_legend(
    reverse = TRUE, 
    ncol = 1,
    byrow = TRUE,
    label.position = "right")
  ) +
  theme(
    axis.title.y = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0.2, "in"),
    strip.background.y = element_blank(),
    strip.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "right",
    legend.direction = "horizontal",
    legend.margin = margin(t = 6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.title.position = "top",
    legend.title = element_text(size = 10)
  )

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    "urbreg",
    paste0(
      "barplot_seq_cluster_by_urbreg_tot_grid_",
      "constant",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".jpg"
    )
  ),
  plot = p1,
  width = 7,
  height = 5.2,
  units = "in",
  dpi = 300
)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    "urbreg",
    paste0(
      "barplot_seq_cluster_by_urbreg_tot_grid_",
      "constant",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".tiff"
    )
  ),
  plot = p1,
  width = 7,
  height = 5.2,
  units = "in",
  bg = "white",
  dpi = 600,
  device = "tiff"
)

### Non-constant sequences 

# keep top 5 sequences
tab2_top <- tab2 %>% 
  group_by(URBREG) %>% 
  arrange(desc(perc)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank<= 5)

# plot
### Constant sequences 
max_limit <- tab2_top %>% 
  group_by(URBREG) %>% 
  summarise(tot_perc = sum(perc)) %>% 
  select(tot_perc) %>% 
  max() %>% 
  round(., 0)

thr <- 1
tab2_top <- tab2_top %>% 
  mutate(value_text = ifelse(perc >= thr, as.character(SEQ_CLUSTER), ""))

# Labels
legend_labels <- list()
for (i in 1:nclust) {
  legend_labels[[as.character(i)]] <- paste0(i, ": ", clust_pen_labs[[as.character(i)]])  
}

# Legend with cluster description 
plot_legend <- ggplot(data = tab1, aes(x = URBREG, y = perc, fill = forcats::fct_rev(SEQ_CLUSTER))) +
  geom_col(width = 0.8) +
  scale_fill_manual(
    values = rep(NA, 9),
    labels = legend_labels,
    name = "Cluster description"
  ) +
  guides(fill = guide_legend(reverse = TRUE, override.aes = list(color = NA, fill = NA))) +
  theme(legend.key = element_rect(unit(0, "mm")),
        legend.title = element_text(size = 10, hjust = 0.25),
        legend.margin = margin(l = -5),
        legend.text = element_text(size = 8))

legend_clust <- cowplot::get_legend(plot_legend)

# plot
p2 <- ggplot(data = tab2_top, aes(x = URBREG, y = perc, fill = forcats::fct_rev(factor(rank)))) +
  geom_col(width = 0.8) +
  facet_grid(region_group~., scales = "free", space = "free_y", switch = "y") +
  coord_flip() +
  geom_text(
    aes(label = value_text),
    color = "white",
    position = position_stack(vjust = 0.5),
    inherit.aes = TRUE,
    size = 2
  ) +
  scale_fill_grey(
    name = "Rank",
    start = 0.7,
    end = 0.05,
    labels = paste("Top", 5:1)
  ) +
  scale_x_discrete(drop = TRUE, expand = c(0,0)) +
  scale_y_continuous(
    limits = c(0,max_limit+1),
    breaks = seq(0,round(max_limit/10,0)*10, by = 10),
    expand = c(0,0)
  ) +
  labs(
    y = "Percentage of the urban region's 100x100m grid cells (%)", 
    #y = "Urban Region", 
    fill = ""
  ) +
  theme_bw() +
  guides(fill = guide_legend(
    reverse = TRUE, 
    nrow = 1,
    byrow = TRUE,
    label.position = "bottom")
  ) +
  theme(
    axis.title.y = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0.2, "in"),
    strip.background.y = element_blank(),
    strip.text.y = element_text(size = 6),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.margin = margin(t = 6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.title = element_blank()
  )

p2 <- cowplot::ggdraw() +
  cowplot::draw_plot(p2, 0, 0, 1, 1) +
  cowplot::draw_plot(legend_clust,
                     x = 0.68,
                     y = 0.63,
                     width = 0.2,
                     height = 0.2)

ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    "urbreg",
    paste0(
      "barplot_seq_cluster_by_urbreg_tot_grid_",
      "top5_nocons",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".jpg"
    )
  ),
  plot = p2,
  width = 7,
  height = 5.2,
  units = "in",
  dpi = 300
)


ggsave(
  file.path(
    path_results,
    "figures",
    "sequences",
    "plot_seq_cluster",
    "urbreg",
    paste0(
      "barplot_seq_cluster_by_urbreg_tot_grid_",
      "top5_nocons",
      "_std_",
      clust_method,
      "_k",
      nclust,
      ".tiff"
    )
  ),
  plot = p2,
  width = 7,
  height = 5.2,
  units = "in",
  dpi = 600,
  bg = "white",
  device = "tiff"
)