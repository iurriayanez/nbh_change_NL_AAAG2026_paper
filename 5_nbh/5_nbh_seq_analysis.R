#------------------------------------------------------------------------------#
# Name: 5_nbh_seq_analysis.R
# Goal: Sequence analysis clustering hypertuning 
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/data_final_std_cluster_assignment_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#               
# Output databases: data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_[std_method]_[clust_method]_k[nclust]_metaggl_all.rds
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#

# 0. Setup ----
source(file.path(path_code, "functions.R"), echo = F)
## 0.1 Libraries ----
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c("readr", "ggplot2", "dplyr", "data.table", "TraMineR", "WeightedCluster")
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

## 0.2 Seed ----
set.seed(1409)

## 0.3 Parameters ----
clust_method <- "mbkmeans"

#------------------------------------------------------------------------------#

# 1. Set hypertuning ---

# Parameters:
# - nclust: Number of MBKMeans clusters (8 and 9)
# - diss_method: 
#     1. OM: 
#       method = "OM"
#       sm = "constant" (2)
#       indel = 1 (for time warping), 3 (HAMMING)
#     2. OMecenter:
#       method = "OM"
#       sm = euclidean of cluster centroids
#       indel = max(euclidean) (for time warping)
#     3. OMstrans: 
#       method = "OMstrans"
#       sm = ("constant", "TRATE")
#       indel = 1, 3
#       transindel = ("constant", "subcost")
#       otto (w) = 0.1, 0.5
#     4. OMtransc(enter): 
#       method = "OMstrans"
#       sm = euclidean of cluster centroids
#       indel = max(euclidean) (for time warping)
#       transindel = "subcost"
#       otto (w) = 0.1, 0.5
#
# - Number of sequence clusters: 5:35

## 1.1 Parameters ----
params <- list(
  # OM parameter sets
  OM_sm2_i1 = list(method = "OM", sm = "CONSTANT", indel = 1),
  OM_sm2_i3 = list(method = "OM", sm = "CONSTANT", indel = 3),
  
  # OMecenter
  OMecenter = list(method = "OM", sm = NULL, indel = NULL), # will be set per dataset
  
  # OMstran parameter sets
  #OMstran_sm2_i1_tri1_w01 = list(method = "OMstran", sm = "CONSTANT", indel = 1, transindel = "constant", otto = 0.1, previous = TRUE)
  #OMstran_sm2_i1_tri1_w01_previousF = list(method = "OMstran", sm = "CONSTANT", indel = 1, transindel = "constant", otto = 0.1, previous = FALSE),
  OMstran_sm2_i3_tri1_w01 = list(method = "OMstran", sm = "CONSTANT", indel = 3, transindel = "constant", otto = 0.1, previous = TRUE),
  OMstran_sm2_i1_tri1_w05 = list(method = "OMstran", sm = "CONSTANT", indel = 1, transindel = "constant", otto = 0.5, previous = TRUE),
  OMstran_sm2_i3_tri1_w05 = list(method = "OMstran", sm = "CONSTANT", indel = 3, transindel = "constant", otto = 0.5, previous = TRUE),  
  #OMstran_smT_i1_triSM_w01 = list(method = "OMstran", sm = "TRATE", indel = 1, transindel = "subcost", otto = 0.1, previous = TRUE),
  #OMstran_smT_i3_triSM_w01 = list(method = "OMstran", sm = "TRATE", indel = 3, transindel = "subcost", otto = 0.1, previous = TRUE),  
  #OMstran_sm2_i1_triSM_w01 = list(method = "OMstran", sm = "CONSTANT", indel = 1, transindel = "subcost", otto = 0.1, previous = TRUE),  
  #OMstran_smT_i1_triSM_w05 = list(method = "OMstran", sm = "TRATE", indel = 1, transindel = "subcost", otto = 0.5, previous = TRUE),
  #OMstran_smT_i3_triSM_w05 = list(method = "OMstran", sm = "TRATE", indel = 3, transindel = "subcost", otto = 0.5, previous = TRUE),   
  
  # OMstranscenter
  OMstranc_triSM_w01 = list(method = "OMstran", sm = NULL, indel = NULL, transindel = "subcost", otto = 0.1, previous = TRUE),
  OMstranc_triSM_w01_previousF = list(method = "OMstran", sm = NULL, indel = NULL, transindel = "subcost", otto = 0.1, previous = FALSE)
  #OMstranc_triSM_w05 = list(method = "OMstran", sm = NULL, indel = NULL, transindel = "subcost", otto = 0.5, previous = TRUE)
)

## 1.3 MBKMeans clusters ----
nclust_values <- 9

## 1.4 Clara Cluster Range parameters ----
R_subsamples <- 800
sample_size <- 10000
kvals <- 2:50

#------------------------------------------------------------------------------#

# 2. Compute ----

for (nclust in nclust_values) {
  
  ## 2.1 Loop over datasets ----
  rds_file <- paste0("data_final_std_cluster_assignment_", std_method, "_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")
  df <- read_rds(file = file.path(path_nbh,rds_file))
  
  ## 2.2 Set dataset for sequence analysis ----
    # STS format: one column per year with the value of the cluster
    df_wide_clust <- dcast(
      df, 
      VIERKANT100M ~ YEAR,
      value.var = "CLUSTER"
    )
  
  # For testing 
  # df_wide_clust <- df_wide_clust[sample(.N, 5000)]
  
  # Define sequence database 
  df_seq <- df_wide_clust %>%
    seqdef(var = 2:ncol(df_wide_clust),
           cpal = get(paste0("tol",nclust,"qualitative")),
           labels = paste("Cluster", 1:nclust)
    )
  
  # Create weighted sequence object
  df_seq_agg <- wcAggregateCases(df_seq)
  df_seq_w <- seqdef(df_seq[df_seq_agg$aggIndex,], 
                     labels = attr(df_seq, "labels"),
                     cpal = attr(df_seq, "cpal"),
                     weights = df_seq_agg$aggWeights)
  
  ## 2.3 Create subcost matrix based on cluster centroids ----
  op_cluster <- read_rds(file.path(path_nbh, paste0("clust_metrics_", std_method, "_optimal_", clust_method, "_metaggl_",met_aggl_labs_save, ".rds")))
  centroids <- op_cluster[[as.character(nclust)]]$centroids
  
  # Order centroids according to std assignment
  # Column names
  clust_cols <- c(
    # Mig. background
    c(grep("^PERC_ETHEUR", names(df), value = TRUE)),
    c(grep("^PERC_ETHNONEUR", names(df), value = TRUE)),
    # Age groups 
    c(grep("^PERC_AGE_GROUP_1", names(df), value = TRUE)),
    c(grep("^PERC_AGE_GROUP_2", names(df), value = TRUE)),
    c(grep("^PERC_AGE_GROUP_7", names(df), value = TRUE)),
    # Socioeconomic variables 
    c(grep("^PERC_HH_LOWINC", names(df), value = TRUE)),
    c(grep("^PERC_HH_HIGHINC", names(df), value = TRUE)),
    # Tenure
    c(grep("^PERC_HH_HOMEOWNER", names(df), value = TRUE)),
    c(grep("^PERC_HH_SUBRENTAL", names(df), value = TRUE))
  )
  
  if (length(clust_cols) == ncol(centroids)) {
    colnames(centroids) <- clust_cols
  } else {
    stop("Check column names")
  }
  
  # Row names (standardise cluster assingment first)
  
  if (clust_method == "mbkmeans" & nclust == 8) {

    new_order <- c(6, 4, 3, 2, 1, 5, 8, 7)
    inverse_order <- order(new_order)
    centroids <- centroids[inverse_order,]
    
  } else if (clust_method == "mbkmeans" & nclust == 9) {
    
    new_order <- c(3, 2, 6, 5, 1, 8, 4, 7, 9)
    inverse_order <- order(new_order)
    centroids <- centroids[inverse_order,]
    
  } else if (clust_method == "somkmeans" & nclust == 9) {
    
    new_order <- c(8, 1, 4, 3, 9, 7, 6, 5, 2)
    inverse_order <- order(new_order)
    centroids <- centroids[inverse_order,]
    
  } else {
    stop("Define standard cluster assignment")
  } 
  
  rownames(centroids) <- 1:nclust
  rm(df, op_cluster)
  gc()
  
  # Calculate distance matrix with centroids
  submat_centroid <- as.matrix(dist(centroids, method = "euclidean"))
  indel_cost <- max(submat_centroid)
  
  # Format for the seqdist function: 
  state_labels <- alphabet(df_seq) 
  rownames(submat_centroid) <- state_labels
  colnames(submat_centroid) <- state_labels
  indel_centroid <- rep(indel_cost, length(state_labels))
  names(indel_centroid) <- state_labels
  
  ## 2.4 Loop over parameter sets ----
  for (param_name in names(params)) {
    
    diss_params <- params[[param_name]]
    
    ### 2.4.1 Define subcost matrix and indel costs ----
      
    if (startsWith(param_name, "OMecenter") | startsWith(param_name, "OMstranc")) {
      indel_cost <-  indel_centroid
      subcost_mat <- submat_centroid
    } else {
      subcost_mat <- seqcost(
        df_seq, 
        method = diss_params$sm,
      )$sm
      indel_cost <- diss_params$indel
    }
    
    ### 2.4.2 Define seqdist arguments ----
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
    
    # Build seqdist arguments for seqclararange
    clara_seqdist_args <- list(
      method = diss_params$method,
      sm = subcost_mat,
      indel = indel_cost
    )
    if (!is.null(diss_params$transindel)) clara_seqdist_args$transindel <- diss_params$transindel
    if (!is.null(diss_params$otto)) clara_seqdist_args$otto <- diss_params$otto
    if (!is.null(diss_params$previous)) clara_seqdist_args$previous <- diss_params$previous
    
    ### 2.4.3 Dissimilarity matrix ----
    diss_matrix <- do.call(seqdist, seqdist_args)
    
    ### 2.4.4 Clara Cluster Range ----
    message(" - Running ", param_name)
    res_clara <- seqclararange(
      seqdata = df_seq,
      R = R_subsamples,
      sample.size = sample_size,
      kvals = kvals,
      seqdist.args = clara_seqdist_args,
      stability = TRUE,
      parallel = TRUE,
      progressbar = FALSE
    )

    ### 2.4.5 Save object ----
    write_rds(res_clara, file.path(path_nbh, "seq_cluster_tuning", paste0("cluster_seq_CLARA_", param_name, "_metrics_", std_method, "_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")), compress = "gz")
    message(" - Saved clara results for ", param_name)

    ### 2.4.6 Plot metrics ----
    png(
      file.path(path_results, "figures", "sequences", "seq_cluster_tuning", paste0("metrics_medoids_claraseqclust_", param_name, "_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, ".png")),
      width = 9.5, 
      height = 7,
      unit = "in",
      pointsize = 15,
      res = 300
    )
    plot(res_clara, norm = "range", legend.pos = "topright")
    dev.off()

    png(file.path(path_results, "figures", "sequences", "seq_cluster_tuning", paste0("metrics_stabmean_claraseqclust_", param_name, "_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, ".png")),1200,700,pointsize = 15)
    plot(res_clara, stat = "stabmean", legend.pos = "bottomright")
    dev.off()

    png(
      file.path(path_results, "figures", "sequences", "seq_cluster_tuning", paste0("metrics_stability_claraseqclust_", param_name, "_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, ".png")),
      width = 9.5, 
      height = 7,
      unit = "in",
      pointsize = 15,
      res = 300
    )
    plot(res_clara, stat = "stability", legend.pos = "bottomright")
    dev.off()
    
    
    ### Bootstrap of cluster quality indices 
    res_clara_boot <- bootclustrange(
      object = res_clara,
      seqdata = df_seq,
      seqdist.args = clara_seqdist_args,
      R = 100,
      sample.size = 10000,
      parallel = TRUE,
      progressbar = TRUE
    )
    write_rds(res_clara_boot, file.path(path_nbh, "seq_cluster_tuning", paste0("cluster_seq_CLARA_boot_", param_name, "_metrics_", std_method, "_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")), compress = "gz")
    message(" - Saved clara bootstrap results for ", param_name)
    
    png(file.path(path_results, "figures", "sequences", "seq_cluster_tuning", paste0("metrics_medoids_claraseqclust_boot_", param_name, "_", std_method, "_std_",clust_method,"_k", nclust, "_metaggl_", met_aggl_labs_save, ".png")),1200,700,pointsize = 15)
    plot(res_clara_boot, stat = "all", norm = "range", legendpos = "topleft")
    dev.off()
    
    ### 2.4.7 Plot sequences ----
    #res_clara <- read_rds(file.path(path_nbh, "seq_cluster_tuning", paste0("cluster_seq_CLARA_", param_name, "_metrics_", std_method, "_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")))

    for (seqnclust in 8:50) {
      # Reprensentative sequences
      sel_cluster <- factor(res_clara$clustering[[paste0("cluster", seqnclust)]])
      sel_cluster_w <- sel_cluster[df_seq_agg$aggIndex]

      # For plotting with ggseqplot
      names(attr(df_seq, "cpal")) <- paste("Cluster", 1:nclust)
      attr(df_seq_w, "cpal") <- attr(df_seq, "cpal")

      p <- ggseqplot::ggseqrplot(
        df_seq_w,
        group = sel_cluster_w,
        diss = diss_matrix,
        criterion = "dist",
        coverage = 0.75,
        stats = F,
        facet_ncol = 9,
        proportional = F,
        weighted = TRUE
      )

      ggsave(
        file.path(
          path_results,
          "figures",
          "sequences",
          "seq_cluster_tuning",
          "plot_seq_cluster",
          paste0(
            "rep_sequences_rep75%_claraseqclust_",
            param_name,
            "_k",
            seqnclust,
            "_",
            std_method,
            "_std_",
            clust_method,
            "_k",
            nclust,
            "_metaggl_",
            met_aggl_labs_save,
            ".jpg"
          )
        ),
        plot = p,
        width = 20,
        height = 14,
        units = "in",
        dpi = 300
      )
    }
      # All sequences ----
      # p <- ggseqplot::ggseqiplot(
      #   df_seq,
      #   group = sel_cluster,
      #   sortv = "from.start",
      #   weighted = F,
      #   facet_ncol = 9
      # )
      # ggsave(
      #   file.path(
      #     path_results,
      #     "figures",
      #     "sequences",
      #     "seq_cluster_tuning",
      #     "plot_seq_cluster",
      #     paste0(
      #       "all_sequences_claraseqclust_",
      #       param_name,
      #       "_k",
      #       seqnclust,
      #       "_",
      #       std_method,
      #       "_std_",
      #       clust_method,
      #       "_k",
      #       nclust,
      #       "_metaggl_",
      #       met_aggl_labs_save,
      #       ".jpg"
      #     )
      #   ),
      #   plot = p,
      #   width = 20,
      #   height = 14,
      #   units = "in",
      #   dpi = 300
      # )
    # }
    
    
  }
}


# 5. Save final dataset ----
seqnclust <- 46
param_name <- "OMstran_sm2_i1_tri1_w01_previousF"
res_clara <- read_rds(file.path(path_nbh, "seq_cluster_tuning", paste0("cluster_seq_CLARA_", param_name, "_metrics_", std_method, "_", clust_method, "_k", nclust, "_metaggl_", met_aggl_labs_save, ".rds")))

sel_cluster <- factor(res_clara$clustering[[paste0("cluster", seqnclust)]])

df_wide_clust$SEQ_CLUSTER <- sel_cluster
save_file <- paste0(
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

write_rds(
  df_wide_clust,
  file.path(
    path_nbh,
    save_file
  )
)




