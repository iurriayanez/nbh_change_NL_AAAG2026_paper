#------------------------------------------------------------------------------#
# Name: functions
# Goal: Script to create functions
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 1. Color palettes ----
colors_cluster <- c("#4477AA", "#FFDF00", "#882255", "#FF7300")
blue_gradient_pal <- c("#d7d7d7", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")

# Colors by dimension (4 dimensions)
colors_dim_b <- c("#332288","#88ccee")
colors_dim_g <- c("#117733","#44aa99")
colors_dim_o <- c("#ddcc77","#999933")
colors_dim_p <- c("#cc6677","#aa4499","#882255")
colors_4dim <- c(colors_dim_b, colors_dim_p, colors_dim_o, colors_dim_g)

colors_dim_3b <- c('#332288','#6699CC','#88ccee')
colors_dim_3g <- c('#117733','#999933','#44aa99')
colors_dim_3o <- c('#ddcc77','gray10','#661100')
colors_dim_4p <- c('#cc6677','#aa4466','#882255','#AA4499')
colors_4dim <- c(colors_dim_3b, colors_dim_4p, colors_dim_3o, colors_dim_3g)


# Paul Tol Palettes
tol1qualitative <- c("1" = '#4477AA')
tol2qualitative <- c("1" = '#4477AA',"2" = '#cc6677')
tol3qualitative <- c("1" = '#4477AA',"2" = '#ddcc77',"3" = '#cc6677')
tol4qualitative <- c("1" = '#4477AA',"2" = '#117733',"3" = '#ddcc77',"4" = '#cc6677')
tol5qualitative <- c("1" = '#332288',"2" = '#88ccee',"3" = '#117733',"4" = '#ddcc77',"5" = '#cc6677')
tol6qualitative <- c("1" = '#332288',"2" = '#88ccee',"3" = '#117733',"4" = '#ddcc77',"5" = '#cc6677',"6" = '#AA4499')
tol7qualitative <- c("1" = '#332288',"2" = '#88ccee',"3" = '#44aa99',"4" = '#117733',"5" = '#ddcc77',"6" = '#cc6677',"7" = '#AA4499')
tol8qualitative <- c("1" = '#332288',"2" = '#88ccee',"3" = '#44aa99',"4" = '#117733',"5" = '#999933',"6" = '#ddcc77',"7" = '#cc6677',"8" = '#AA4499')
tol9qualitative <- c("1" = '#332288',"2" = '#88ccee',"3" = '#44aa99',"4" = '#117733',"5" = '#999933',"6" = '#ddcc77',"7" = '#cc6677',"8" = '#882255',"9" = '#AA4499')
tol10qualitative <- c("1" = '#332288',"2" = '#88ccee',"3" = '#44aa99',"4" = '#117733',"5" = '#999933',"6" = '#ddcc77',"7" = '#661100',"8" = '#cc6677',"9" = '#882255',"10" = '#AA4499')
tol11qualitative <- c("1" = '#332288',"2" = '#6699CC',"3" = '#88ccee',"4" = '#44aa99',"5" = '#117733',"6" = '#999933',"7" = '#ddcc77',"8" = '#661100',"9" = '#cc6677',"10" = '#882255',"11" = '#AA4499')
tol12qualitative <- c("1" = '#332288',"2" = '#6699CC',"3" = '#88ccee',"4" = '#44aa99',"5" = '#117733',"6" = '#999933',"7" = '#ddcc77',"8" = '#661100',"9" = '#cc6677',"10" = '#aa4466',"11" = '#882255',"12" = '#AA4499')

tol6dark <- c("1" = "#EECC66", "2" = "#EE99AA", "3" = "#6699CC", "4" = "#997700", "5" = "#994455", "6" = "#004488")

# Blend colours 
blend_colour <- function(cols, mix_share = 0.5) {
  mixed <- colorspace::mixcolor(
    mix_share, 
    colorspace::hex2RGB(cols[1]),
    colorspace::hex2RGB(cols[2])
  )
  colorspace::hex(mixed)
}

# Desaturate colours 
desat_colour <- function(col, amount = 0.4) {
  # amount <- 1 = more desaturated
  hcl <- as(colorspace::hex2RGB(col), "polarLUV")
  hcl@coords[,"C"] <- hcl@coords[,"C"] * amount
  colorspace::hex(as(hcl, "RGB"))
}

#------------------------------------------------------------------------------#
# 2. Clustering ----

## 2.1 PCA data into long format (for plotting the factor loadings) ----
pca_data_long <- function(res.pca, measure.vars, value.name) {
  df_pca_coord <- as.data.table(t(res.pca$var$coord), keep.rownames = T)
  df_pca_coord <- df_pca_coord %>% 
    mutate(dim = seq(1:ncol(res.pca$var$coord))) %>% 
    select(-rn)
  
  # Wide to long    
  df_pca_coord_long <- melt(df_pca_coord, id.vars = c('dim'), measure.vars = measure.vars, 
                            variable.name = "SCALE", value.name = value.name)
  df_pca_coord_long <- melt(df_pca_coord_long, 
                            id.vars = c('dim', 'SCALE'), 
                            measure.vars = value.name)
  
  # Max value of SCALE is the smallest
  df_pca_coord_long[, SCALE := ifelse(SCALE == max(as.numeric(SCALE)), 0, SCALE)]
  return(df_pca_coord_long)
}


## 2.2 Cluster metrics ---- 
clust_metrics <- function(data, cluster_output, sample_size, label) {
  # Ensure cluster output is a vector of labels 
  if (is.list(cluster_output) && "classification" %in% names(cluster_output)) {
    clusters <- cluster_output$classification # For GMM
  } else if (is.list(cluster_output) && "cluster" %in% names(cluster_output)) {
    clusters <- cluster_output$cluster # K-Means
  } else {
    clusters <- cluster_output # HAC
  }
  
  num_clusters <- length(unique(clusters))
  if (num_clusters < 2) {
    stop("At least two clusters are required for validity metrics.")
  }
  
  #Initialize results 
  results <- list(
    Method = label, 
    K = num_clusters, 
    Silhouette = NA,
    Calinski_Harabasz = NA, 
    Davies_Bouldin = NA,
    cluster_freq = NA,
    cluster_assign = NA,
    Time_Sil = NA, 
    Time_CH = NA,
    Time_DB = NA
  )
  
  # Cluster frequencies 
  cluster_frequencies <- as.list(table(clusters))
  results$cluster_freq <- cluster_frequencies 
  
  # Cluster assignment 
  results$cluster_assign <- clusters
  
  # Compute silhouette with random sampling 
  time_silhouette <- system.time({
    subsample_indices <- sample(1:nrow(data), min(sample_size, nrow(data)))
    subsample_data <- data[subsample_indices, ]
    subsample_clusters <- clusters[subsample_indices]
    
    # Calculate silhouette
    results$Silhouette <- mean(silhouette(subsample_clusters, dist(subsample_data))[,3])
  }) ["elapsed"]
  results$Time_Sil <- time_silhouette
  
  # Compute Calinski-Harabasz
  time_calinski <- system.time({
    results$Calinski_Harabasz = fpc::calinhara(data,clusters)
  })["elapsed"]
  results$Time_CH <- time_calinski
  
  # Compute Davies-Bouldin
  time_davies <- system.time({
    results$Davies_Bouldin = clusterSim::index.DB(data, clusters)$DB
  })["elapsed"]
  results$Time_DB <- time_davies
  
  return(results)
}


## 2.3 Plot metrics from cluster_metric function ----
plot_clust_metric <- function(
    list_metrics, 
    metric_name, 
    metric_title, 
    mark_k = NULL, 
    col_mark = "#DDAA33",
    lty_mark = "dotdash"
) {
  
  # Extract the relevant data
  plot_data <- data.frame(
    Method = sapply(list_metrics, function(x) x$Method),
    K = sapply(list_metrics, function(x) x$K),
    Metric = sapply(list_metrics, function(x) x[[metric_name]])
  )
  
  if (metric_name == "Calinski_Harabasz") {
    plot_data <- plot_data |>
      mutate(Metric = Metric/1000)
  }
  
  # Create line plot
  p <- ggplot2::ggplot(plot_data, aes(x=K, y=Metric, color= Method, group= Method))+
    geom_line(size=0.7) +
    geom_point(size=1.2) +
    labs(
      x = "Number of clusters (k)",
      y = paste(metric_title, "score")
    ) +
    scale_x_continuous(breaks = seq(0,max(plot_data$K),5)) +
    scale_color_manual(values = "#004488") +
    theme_bw() +
    theme(
      legend.position = "none"
    )
  if (metric_name == "Calinski_Harabasz") {
    p <- p + 
      labs(
        x = "Number of clusters (k)",
        y = paste(metric_title, "score", "\n(x1000)")
      )
  }
  
  if (!is.null(mark_k)) {
    p <- p +
      geom_vline(
        xintercept = mark_k,
        linetype = lty_mark,
        color = col_mark,
        linewidth = 0.7
      )
  }
  return(p)
}

## 2.4 Plot cluster average distance profiles ----

# Cluster distance profiles 
cluster_dist_profiles <- function(cluster_number, data, data_labels, x_axis_labels = scale_labs_save, nudge_x_label = 1, nudge_y_label = 0, label_size = 0.25, palette) {
  max_abs <- max(abs(data[CLUSTER == cluster_number,value]))
  plot <- ggplot(
    data[CLUSTER == cluster_number,],
    aes(
      x = SCALE,
      y = value,
      color = variable,
      group = variable
    )
  ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = max(data[,SCALE]),
      ymin = -max_abs,
      ymax = 0,
      fill = "gray90",
      alpha = 0.3
    ) +
    geom_point(size = 0.8) +
    geom_line(linewidth = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = max(data[,SCALE]), y = 0, yend = 0),
                 linetype = "dashed",
                 color = "black",
                 alpha = 0.4,
                 linewidth = 0.3)
  
  #geom_hline(
  #  yintercept = 0,
  #  linetype = "dashed",
  #  color = "black",
  #  alpha = 0.5,
  #  linewidth = 0.4
  #  )
  
  
  plot <- plot +
    ggrepel::geom_label_repel(
      data = data_labels,
      aes(label = labels),
      nudge_x = nudge_x_label,
      nudge_y = nudge_y_label,
      size = label_size,
      direction = 'y',
      hjust = 0,
      fill = alpha("white", 0.8),
      segment.size = 0,
      segment.alpha = 0,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20,
      box.padding = .4
    ) +
    scale_x_continuous(breaks = 0:6, labels = x_axis_labels) +
    coord_cartesian(ylim = c(-max_abs, max_abs)) +
    ylab("Average z-score") +
    xlab("Scale of Bespoke Areas") +
    labs(
      title = paste("Cluster", cluster_number)
    ) +
    scale_color_manual(
      values = palette
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(margin = unit(c(7,0,0,0), "mm"))
    )
  
  return(plot)
}


## 2.5 Cluster descriptive statistics table ----
summary_table_clusters <- function(data, cluster_var = "CLUSTER", nclust, 
                                   col.names = NULL, decimals = 3,
                                   sort_rows = T,
                                   row_names = NULL) {
  
  # Check
  if(is.null(row_names)){
    stop("Please provide variable order for rows in row_names")
  }
  
  # Set default column names
  if (is.null(col.names)) {
    col.names <- seq(1:nclust)
  }
  
  ncol_clust <- which(colnames(data) == cluster_var)
  
  # Provide dataframe with numeric values and the cluster variable
  table <- FactoMineR::catdes(data, ncol_clust, proba = 1)
  
  # List 
  table_list <- list()
  
  # Define function to add stars 
  stars <- function(x, y, z) {
    ifelse(y<=0.001 & z>0, paste0(x, "***"), 
           ifelse(y<=0.01 & z>0, paste0(x, "**"),
                  ifelse(y<=0.05 & z>0, paste0(x, "*"), 
                         ifelse(y<=0.001 & z<=0, paste0(x, "^^^"), 
                                ifelse(y<=0.01 & z<=0, paste0(x, "^^"),
                                       ifelse(y<=0.05 & z<=0, paste0(x, "^"), x))))))
  }
  
  # Iterate over clusters 
  for (i in (1:nclust)) {
    # Table name
    tablename <- paste0('table',i)
    
    # Subset corresponding cluster category
    table_subset <- table[[2]][[as.character(i)]]
    
    # Sort rows
    if(sort_rows == T) {
      table_subset <- table_subset[row_names, ]
    }
    
    
    # Select desired columns of each table 
    table_subset <- as.data.frame(table_subset[,c(1,2,6)])
    
    # Round all values to 1 decimal place
    table_subset[,2] <- round(table_subset[,2], decimals)
    
    # Apply custom function to format cells 
    table_subset <- as.data.frame(mapply(stars, table_subset[, 2], table_subset[,3], table_subset[, 1]))
    
    # Col and rownames
    colnames(table_subset) <- as.character(i)
    rownames(table_subset) <- row_names
    
    # Set name 
    assign(tablename, table_subset)
    
    # Add to list
    table_list[[i]] <- get(tablename)
  }
  
  # Extract overall mean
  overall_mean <- table[[2]][[as.character(1)]]
  overall_mean <- overall_mean[row_names, ]
  overall_mean <- as.data.frame(overall_mean[,3])
  overall_mean[,1] <- round(overall_mean[,1], decimals)
  rownames(overall_mean) <- row_names
  colnames(overall_mean) <- "Overall"
  
  # Create table with cluster size 
  table_size <- prop.table(table(data[[cluster_var]])) * 100
  table_size <- as.data.frame(matrix(round(table_size, 1), ncol = nclust))
  table_size <- cbind(table_size, 100)
  colnames(table_size) <- c(col.names, "Overall")
  rownames(table_size) <- c("Cluster size (%)")
  
  # Combine all tables 
  table_final <- do.call(cbind, table_list)
  table_final <- cbind(table_final, overall_mean)
  table_final <- rbind(table_final, table_size)
  return(table_final)
}

#------------------------------------------------------------------------------#
# 3. Clustergram function ----
#  Modifications based on code by Tal Galili 

## 3.1 Calculate cluster assignment and centers ----
# HAC
clustergram.hac <- function(Data, k.range, 
                            hac.method = "ward.D2",...)
{
  # Distance matrix 
  dist.data <- dist(Data)
  
  results <- list()
  for(k in k.range){
    cl <-  cutree(hclust(dist.data, method = hac.method), k = k)
    clusters <- cl
    centers <- rowsum(Data, cl) / as.vector(table(cl))
    results[[as.character(k)]] <- list(clusters = clusters, centers = centers)
  }
  return(results) 
}

# Kmeans
clustergram.kmeans <- function(Data, k.range,
                               nstart = 10, ...)
{
  results <- list()
  for(k in k.range){
    cl <- kmeans(Data, centers = k, nstart = nstart, ...)
    clusters <- cl$cluster
    centers <- cl$centers
    results[[as.character(k)]] <- list(clusters = clusters, centers = centers)
  }
  return(results) 
}

# MiniBatchKMeans

clustergram.minikmeans <- function(Data, k.range,
                                   num_init = 50, 
                                   max_iters = 1000000, ...)
{
  results <- list()
  batch_size <- min(100000, nrow(Data))
  for(k in k.range){
    cl <- MiniBatchKmeans(Data, cluster = k, batch_size = batch_size, num_init = num_init, max_iters = max_iters, ...)
    minibatch_kmeans_clusters <- predict_MBatchKMeans(Data, cl$centroids)
    clusters <- minibatch_kmeans_clusters
    centers <- cl$centroids
    results[[as.character(k)]] <- list(clusters = clusters, centers = centers)
  }
  return(results) 
}

# Calculate PCA weighted mean 
clustergram.compute.pca <- function(Data, cluster_info, k.range, pca.loading) {
  # PCA loadings
  pca <- princomp(Data)$loadings[,pca.loading]
  
  # Set cluster_info as same length 
  cluster_info <- cluster_info[1:length(k.range)]
  
  weighted_centers <- list()
  for (k in k.range) {
    centers <- cluster_info[[as.character(k)]]$centers
    if (is.vector(centers)) {
      centers <- matrix(centers, ncol = length(centers))
    }
    # Compute PCA weighted mean 
    weighted_means <- centers %*% pca
    weighted_centers[[as.character(k)]] <- weighted_means 
  }
  return(weighted_centers)
}


## 3.2 Plot the clustergram ----
clustergram.plot <- function(
    Data, 
    cluster_info, 
    k.range, 
    max_line_width,
    pca.loading, 
    pca.weight,
    col.select = NULL,
    xlab = "Number of clusters (k)",
    ylab = "Cluster center (mean)",
    main = "Clustergram",
    col_seg = "#004488", 
    col_centers = "#BB5566",
    center_cex = 1.5, 
    add_centers = TRUE,
    mark_k = NULL, 
    col_mark = "#DDAA33",
    lty_mark = "dashed",
    lwd_mark = 2.5,
    ...)
{
  # Set cluster info as same length of range 
  cluster_info <- cluster_info[1:length(k.range)]
  
  # Determine y-axis limits from all centers 
  if (pca.weight == T) {
    weighted_centers <- clustergram.compute.pca(Data, cluster_info, k.range, pca.loading)
    for (k in k.range) {
      cluster_info[[as.character(k)]]$centers <- weighted_centers[[as.character(k)]]
    }
  } else {
    if (!is.null(col.select)) {
      # Select a particular variable (especially with PCA weights as variables)
      for (k in k.range) {
        cluster_info[[as.character(k)]]$centers <- matrix(cluster_info[[as.character(k)]]$centers[, col.select])
      }
    } else {
      # Overall mean across all variables 
      for (k in k.range) {
        cluster_info[[as.character(k)]]$centers <- rowMeans(cluster_info[[as.character(k)]]$centers)
      }
    }
  }
  
  all_centers <- unlist(lapply(cluster_info, function(info) info$centers))
  
  # Set up the empty plot 
  plot(0, 0, type = "n",
       xlim = range(k.range),
       ylim = range(all_centers),
       xlab = xlab,
       ylab = ylab,
       main = main,...)
  axis(side = 1, at = k.range)
  abline(v= k.range, col = "grey")
  
  # Step 1: Find the maximum transition count in a single pass 
  max_count <- 0
  transition_tables <- list()
  
  for(i in seq_along(k.range)[-length(k.range)]){
    k <- k.range[i]
    k_next <- k.range[i + 1]
    
    trans_table <- table(cluster_info[[as.character(k)]]$clusters,
                         cluster_info[[as.character(k_next)]]$clusters)
    transition_tables[[i]] <-  trans_table 
    max_count <- max(max_count, max(trans_table))
  }
  
  # Compute scaling factor 
  scale_factor <- max_line_width / max_count
  
  # Step 2: Plot connections using the transition tables 
  for(i in seq_along(k.range)[-length(k.range)]){
    k <- k.range[i]
    k_next <- k.range[i + 1]
    
    centers_k <- cluster_info[[as.character(k)]]$centers
    centers_k1 <- cluster_info[[as.character(k_next)]]$centers
    
    # Transition table 
    trans_table <- as.data.frame(transition_tables[[i]])
    names(trans_table) <- c('clusters_k', 'clusters_k1', 'Freq')
    
    for(j in seq_len(nrow(trans_table))) {
      count <- trans_table$Freq[j]
      if (count > 0){
        cl_k <- as.numeric(as.character(trans_table$clusters_k[j]))
        cl_k1 <- as.numeric(as.character(trans_table$clusters_k1[j]))
        x0 <- k
        y0 <- centers_k[cl_k]
        x1 <- k_next
        y1 <- centers_k1[cl_k1]
        segments(x0, y0, x1, y1, lwd = count * scale_factor, col = col_seg)
      }
    }
  }
  
  # Optionally, overlay cluster centers as points 
  if(add_centers) {
    for(k in k.range) {
      clusters_k <- cluster_info[[as.character(k)]]$clusters
      centers_k <- cluster_info[[as.character(k)]]$centers
      cluster_sizes <- table(clusters_k)
      
      # Normalize sizes 
      max_size <- max(cluster_sizes)
      sizes <- sqrt(cluster_sizes/max_size)
      base_size <- 1.2
      
      for (j in seq_along(centers_k)){
        y <- centers_k[j]
        x <- k
        s <- base_size * sizes[j]
        
        # White border circle
        points(x, y, pch = 21, bg = "white", col = "white", cex = s + 0.75)
        # Main colored circle
        points(x, y , pch = 21, bg = col_centers, col = col_centers, cex = s)
      }
    }
  }
  
  if (!is.null(mark_k)) {
    abline(v = mark_k, col = col_mark, lty = lty_mark, lwd = lwd_mark)
  }
}

## 3.3 Wrapper function: compute clusters (if not provided) then plot ----
clustergram <- function(Data = NULL, cluster_info = NULL,
                        k.range = 2:10, 
                        clustering.function = clustergram.kmeans,
                        max_line_width = 5, 
                        pca.weight = T,
                        pca.loading = 1,
                        col.select = NULL, ...) {
  if(is.null(cluster_info)) {
    if(is.null(Data)) stop("At least Data must be provided")
    cluster_info <- clustering.function(Data, k.range)
  } 
  p <- clustergram.plot(Data, cluster_info, k.range, max_line_width, pca.loading, pca.weight, col.select, ...)
  return(p)
  #return(invisible(cluster_info))
}


#------------------------------------------------------------------------------#
# 4. Spatial & Maps ----

## 4.1 Create polygons based on grid cell data ----
grid_to_polygon <- function(data, grid_size = 100) {
  if (class(data)[1] != "data.table") {
    data <- as.data.table(data)
  }
  
  if (grid_size == 100) {
    polygons_df <- data.table(VIERKANT100M = unique(data[,VIERKANT100M]))
    polygons_df[, `:=`(X = as.numeric(paste0(substr(VIERKANT100M, 2, 5),"50")),
                       Y = as.numeric(paste0(substr(VIERKANT100M, 7, 10),"50")))]
    polygons_df <- as.data.frame(polygons_df)
    
    polygons_sf <- sf::st_sf(
      geometry = sf::st_sfc(
        lapply(1:nrow(polygons_df), function(i){
          sf::st_polygon(list(rbind(
            c(polygons_df$X[i] - 50, polygons_df$Y[i] + 50),
            c(polygons_df$X[i] + 50, polygons_df$Y[i] + 50),
            c(polygons_df$X[i] + 50, polygons_df$Y[i] - 50),
            c(polygons_df$X[i] - 50, polygons_df$Y[i] - 50),
            c(polygons_df$X[i] - 50, polygons_df$Y[i] + 50)
          )))
        }),
        crs = 28992
      ),
      ID = polygons_df$VIERKANT100M
    )
    
  } else {
    polygons_df <- data.table(VIERKANT100M = unique(data[,VIERKANT500M]))
    polygons_df[, `:=`(X = (as.numeric(substr(VIERKANT500M, 2, 5)) * 100 + 250),
                       Y = (as.numeric(substr(VIERKANT500M, 7, 10)) * 100 + 250))]
    polygons_df <- as.data.frame(polygons_df)
    
    polygons_sf <- sf::st_sf(
      geometry = sf::st_sfc(
        lapply(1:nrow(polygons_df), function(i){
          sf::st_polygon(list(rbind(
            c(polygons_df$X[i] - 250, polygons_df$Y[i] + 250),
            c(polygons_df$X[i] + 250, polygons_df$Y[i] + 250),
            c(polygons_df$X[i] + 250, polygons_df$Y[i] - 250),
            c(polygons_df$X[i] - 250, polygons_df$Y[i] - 250),
            c(polygons_df$X[i] - 250, polygons_df$Y[i] + 250)
          )))
        }),
        crs = 28992
      ), 
      ID = polygons_df$VIERKANT500M
    )
    
  }
  return(polygons_sf)
}

## 4.2 Choropleth map ----
map_variables <- function(data, variable, legend_name, add_breaks = FALSE, br, bg_alpha = 0.12, loc_scale = 'br', loc_arrow = 'tr', map_palette = NULL) {
  var <- variable
  if (add_breaks == TRUE) {
    nbreaks <- length(br) - 1
    category_colors <- grDevices::palette.colors(nbreaks)
    labs <- round(br, 1)
    stop <- length(labs) - 1
    labs_plot <- paste0("[", labs[1:stop], "- ", labs[2:length(labs)], ")")
    labs_plot[[length(labs_plot)]] <- paste0("[", labs[(length(labs)-1)], "- ", labs[length(labs)], "]")
    
    data[[var]] <- as.factor(cut(data[[var]], breaks = br, labels  = FALSE, include.lowest = TRUE))
    
    discrete.scale <- TRUE
    
    fill_options = scale_fill_manual(
      values = blue_gradient_pal,
      labels = labs_plot
    )
    
    # fill_options = scale_fill_brewer(
    #   labels = labs_plot
    # )
    
  } else {
    
    data[[var]] <- data[[var]]
    discrete.scale <- FALSE
    fill_options = NULL
    
    if (class(data[[var]]) == "factor") {
      if (is.null(map_palette)) {
        nbreaks <- length(unique(data[[var]]))
        category_colors <- palette.colors(nbreaks)
        discrete.scale <- TRUE
        fill_options = scale_fill_manual(
          values = category_colors
          #limits = c(0,1)
        )
      } else {
        fill_options = scale_fill_manual(
          values = map_palette
          #limits = c(0,1)
        )
      }
      
    }
  }
  
  p <- ggplot() +
    geom_sf(data = other_sub, fill = "gray70", color = NA, alpha = bg_alpha) +
    geom_sf(data = water_sub, fill = "gray50", color = NA, alpha = bg_alpha) +
    geom_sf(data = built_sub, fill = "gray30", color = NA, alpha = bg_alpha) +
    geom_sf(data = roads_sub, fill = "gray10", color = NA, alpha = bg_alpha) +
    #geom_sf(data = metaggl_sf, fill = NA, color = "black", alpha = 0.8, linewidth = 0.1) +
    geom_sf(data = data[!is.na(data[[var]]),], aes(fill = !!sym(var)), 
            color = NA, size = 0
    ) +
    
    # Labs
    fill_options +
    labs(fill= legend_name) +
    
    # Theme
    theme_minimal() +
    theme(
      plot.caption = element_text(size= 3, face = "italic"),
      plot.subtitle = element_text(size= 15, vjust = -8.5, hjust = 0.075),
      legend.position = "right",
      legend.text = element_text(size = 15), legend.title = element_text(size = 18),
      legend.key.size = unit(1, "cm"),
      legend.box.margin = margin(l = -12),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(-0.25,-3.5,0,-3.5), "cm")
    ) +
    
    # Arrow and scale bar 
    ggspatial::annotation_scale(
      location = loc_scale, 
      width_hint = 0.12, 
      style = "ticks", 
      pad_x = unit(2.0, "cm"), 
      pad_y = unit(1.5, "cm"),
      line_width = 1, 
      text_pad = unit(0.5, "cm"), 
      tick_height = 0.5, 
      text_cex = 0.85,
      height = unit(0.3, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = loc_arrow, 
      which_north = TRUE, 
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = unit(2, "cm"),
      pad_y = unit(2, "cm"),
      style = ggspatial::north_arrow_orienteering(text_size = 6)
    )
  
  return(p)
}

## 5.3 Sequence clusters maps ----

# --------------------------------------------------------------------#
### 5.3.1 Build sequences summary info ----
# - df_sf: sf dataframe containing city polygons and sequence column
# - city_to_plot: single city id
# - seq_clust_var_name: name of sequence variable
# - all_sequences: optional character vector of sequences 
# to always include (even if absent)
# 
# Returns tibble: sequence, n_cells, pct_of_city, present
# --------------------------------------------------------------------#

build_sequence_summary <- function(df_sf, city_to_plot, seq_clust_var_name, all_sequences = NULL) {
  df_city <- df_sf %>% filter(URBREG == city_to_plot)
  total_cells <- nrow(df_city)
  
  seq_summary <- df_city %>% 
    st_set_geometry(NULL) %>% 
    rename(sequence = !!seq_clust_var_name) %>%
    dplyr::count(sequence, name = "n_cells") %>% 
    mutate(sequence = as.character(sequence),
           pct_of_city = 100 * n_cells / total_cells,
           present = TRUE) %>% 
    arrange(desc(pct_of_city))
  
  # add any sequences in all_sequences that are missing (n_cells = 0, present = FALSE)
  if (!is.null(all_sequences)) {
    missing_seqs <- setdiff(as.character(all_sequences), seq_summary$sequence)
    if (length(missing_seqs) > 0) {
      add_df <- tibble::tibble(sequence = as.character(missing_seqs),
                               n_cells = 0L,
                               pct_of_city = 0,
                               present = FALSE)
      seq_summary <- bind_rows(seq_summary,add_df) %>% 
        arrange(desc(pct_of_city))
    }
  }
  
  # select columns and return
  seq_summary <- seq_summary %>% 
    dplyr::select(sequence,  n_cells, pct_of_city, present)
  return(seq_summary)
}

# --------------------------------------------------------------------#
### 5.3.2 Create center map ----
# - df_city: sf for a single city (filtered)
# - constant_sequences: character vector of constant sequences
# - colour_palette: vector with named hex codes (e.g., "1" = "#ffff00")
# - other_colour: colour for non-constant sequences 
# - bbox_padding: fraction padding to add to bbox (0.02 default)
# 
# Returns ggplot map
# --------------------------------------------------------------------#

make_center_map <- function(df_city,
                            landuse_sf,
                            gem_sf,
                            urbreg_sf,
                            constant_sequences = as.character(1:nclust),
                            seq_clust_var_name = "SEQ_CLUSTER",
                            colour_palette = paste0("tol", nclust, "qualitative"), 
                            other_colour = "#000000",
                            bbox_padding_y_min = 0.02,
                            bbox_padding_y_max = 0.02,
                            bbox_padding_x_min = 0.02,
                            bbox_padding_x_max = 0.02,
                            map_bg = "white",
                            gem_colour = "black",
                            urbreg_colour = "black",
                            urbreg_linetype = "11",
                            show_border = FALSE,
                            loc_scale = "br",
                            loc_arrow = "tr",
                            pad_x_map_scale = 0.2,
                            pad_y_map_scale = 0.2,
                            pad_x_map_arrow = 0.2,
                            pad_y_map_arrow = 0.2) {
  
  # ensure sequence field is character 
  df_city <- df_city %>% 
    rename(sequence = !!seq_clust_var_name) %>%
    mutate(sequence = as.character(sequence))
  
  # build a fill column mapping
  df_city <- df_city %>% 
    mutate(fill_group = ifelse(sequence %in% constant_sequences,
                               as.character(sequence), "other"))
  
  # define fill values: include all constant sequences and "other"
  fill_values <- c(get(colour_palette), other = other_colour)
  # in case colour_palette has other names than "1"..., ensure names are characters
  names(fill_values) <- as.character(names(fill_values))
  
  # compute bbox with padding 
  bb <- st_bbox(df_city)
  dx <- bb$xmax - bb$xmin
  dy <- bb$ymax - bb$ymin
  pad_x_min <- dx * bbox_padding_x_min
  pad_x_max <- dx * bbox_padding_x_max
  pad_y_min <- dy * bbox_padding_y_min
  pad_y_max <- dy * bbox_padding_y_max
  xlim <- c(bb$xmin - pad_x_min, bb$xmax + pad_x_max)
  ylim <- c(bb$ymin - pad_y_min, bb$ymax + pad_y_max)
  
  box_coords <- tibble::tibble(x = xlim, y = ylim) %>% 
    st_as_sf(coords = c("x", "y")) %>% 
    st_set_crs(st_crs(landuse_sf))
  bbox_city <- st_bbox(box_coords) %>% st_as_sfc()
  
  # crop landuse
  landuse_sf_sub <- st_intersection(landuse_sf,bbox_city)
  
  # crop municipality
  #idx <- st_intersects(gem_sf, df_city)
  #gem_sf_sub <- gem_sf[lengths(idx) > 50, ]
  gem_sf_sub <- st_intersection(gem_sf,bbox_city)
  
  # crop urban region
  urbreg_sf_sub <- st_intersection(urbreg_sf,bbox_city)
  
  # plot 
  p <- ggplot() +
    # landuse
    geom_sf(data = landuse_sf_sub, 
            aes(fill = as.factor(land_type)), 
            color = NA, alpha = 0.25) +
    scale_fill_manual(values = c(map_bg, "grey90", "grey80", "grey70"),
                      guide = "none") +
    ggnewscale::new_scale_fill() +
    # constant sequences
    geom_sf(data = df_city, aes(geometry = geometry, fill = fill_group),
            color = if (show_border) "grey40" else NA, size = 0.1) +
    # municipal borders
    geom_sf(data = gem_sf_sub, fill = NA, colour = gem_colour, linewidth = 0.3) +
    # urban region borders
    geom_sf(data = urbreg_sf_sub, fill = NA, colour = urbreg_colour, linetype = urbreg_linetype, size = 0.3) +
    scale_fill_manual(values = fill_values, guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = map_bg, colour = NA),
          panel.background = element_blank(),
          legend.position = "none") +
    
    # add north arrow and scale bar
    ggspatial::annotation_scale(
      location = loc_scale, 
      width_hint = 0.1, 
      style = "ticks", 
      pad_x = unit(pad_x_map_scale, "cm"), 
      pad_y = unit(pad_y_map_scale, "cm"),
      line_width = 1, 
      text_pad = unit(0.1, "cm"), 
      tick_height = 0.5, 
      text_cex = 0.7,
      height = unit(0.1, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = loc_arrow, 
      which_north = TRUE, 
      height = unit(0.6, "cm"),
      width = unit(0.6, "cm"),
      pad_x = unit(pad_x_map_arrow, "cm"),
      pad_y = unit(pad_y_map_arrow, "cm"),
      style = ggspatial::north_arrow_orienteering(text_size = 6)
    )
  
  return(p)
}


# -----------------------------------------------------------------------------#
### 5.3.3 make_small_map ----
# - df_city: sf for a single city (filtered)
# - sequence_id: sequence to highlight (character)
# - highlight_colour: hex color for highlighting
# - other_colour: same logic as center map
# - bbox_padding: same logic as center map
# - show_pct: whether to draw percentage text automatically
# 
# Returns ggplot map highlighting the given sequence and including label + pct
# -----------------------------------------------------------------------------#

make_small_map <- function(df_city,
                           gem_sf,
                           urbreg_sf,
                           sequence_id,
                           seq_clust_var_name = "SEQ_CLUSTER",
                           highlight_colour = "red",
                           other_colour = "gray50",
                           bbox_padding_y_min = 0.02,
                           bbox_padding_y_max = 0.02,
                           bbox_padding_x_min = 0.02,
                           bbox_padding_x_max = 0.02,
                           map_bg = "white",
                           txt_colour = "black",
                           gem_colour = "black",
                           urbreg_colour = "black",
                           urbreg_linetype = "11",
                           label_size = 8,
                           pct_digits = 1,
                           show_pct = TRUE,
                           show_label = TRUE,
                           loc_scale = "br",
                           loc_arrow = "tr",
                           pad_x_map_scale = 0,
                           pad_y_map_scale = 0.2,
                           pad_x_map_arrow = 0,
                           pad_y_map_arrow = 0.08) {
  
  sequence_id <- as.character(sequence_id)
  
  # extract number of sequence and ensure sequence field is character 
  df_city <- df_city %>% 
    rename(sequence = !!seq_clust_var_name) %>% 
    mutate(sequence = as.character(sequence))
  
  # number of cells
  total_cells <- nrow(df_city)
  
  n_seq <- sum(df_city$sequence == sequence_id, na.rm = TRUE)
  pct_seq <- 100 * n_seq / total_cells
  pct_label <- sprintf("%.*f%%", pct_digits, pct_seq)
  
  # fill column: highlight sequence_id, otherwise background
  df_city <- df_city %>% 
    mutate(fill_group = ifelse(sequence == sequence_id,
                               "highlight", "other"))
  fill_values <- c(highlight = highlight_colour, other = other_colour)
  
  # compute bbox with padding 
  bb <- st_bbox(df_city)
  dx <- bb$xmax - bb$xmin
  dy <- bb$ymax - bb$ymin
  pad_x_min <- dx * bbox_padding_x_min
  pad_x_max <- dx * bbox_padding_x_max
  pad_y_min <- dy * bbox_padding_y_min
  pad_y_max <- dy * bbox_padding_y_max
  xlim <- c(bb$xmin - pad_x_min, bb$xmax + pad_x_max)
  ylim <- c(bb$ymin - pad_y_min, bb$ymax + pad_y_max)
  
  box_coords <- tibble::tibble(x = xlim, y = ylim) %>% 
    st_as_sf(coords = c("x", "y")) %>% 
    st_set_crs(st_crs(gem_sf))
  bbox_city <- st_bbox(box_coords) %>% st_as_sfc()
  
  # crop municipality
  #idx <- st_intersects(gem_sf, df_city)
  #gem_sf_sub <- gem_sf[lengths(idx) > 50, ]
  gem_sf_sub <- st_intersection(gem_sf, bbox_city)
  
  # crop urban region
  urbreg_sf_sub <- st_intersection(urbreg_sf, bbox_city)
  
  p <- ggplot() +
    # transitioning sequences
    geom_sf(data = df_city, aes(geometry = geometry, fill = fill_group),
            color = NA, size = 0) +
    scale_fill_manual(values = fill_values, guide = "none") +
    # municipal borders
    geom_sf(data = gem_sf_sub, fill = NA, colour = gem_colour, linewidth = 0.3) +
    # urban region borders
    geom_sf(data = urbreg_sf_sub, fill = NA, colour = urbreg_colour, linetype = urbreg_linetype, size = 0.3) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = map_bg, colour = NA),
          panel.background = element_blank(),
          legend.position = "none") +
    
    # add north arrow and scale bar
    ggspatial::annotation_scale(
      location = loc_scale, 
      width_hint = 0.15, 
      style = "ticks", 
      pad_x = unit(pad_x_map_scale, "cm"), 
      pad_y = unit(pad_y_map_scale, "cm"),
      line_width = 1, 
      text_pad = unit(0.1, "cm"), 
      tick_height = 0.5, 
      text_cex = 0.7,
      height = unit(0.1, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = loc_arrow, 
      which_north = TRUE, 
      height = unit(0.4, "cm"),
      width = unit(0.4, "cm"),
      pad_x = unit(pad_x_map_arrow, "cm"),
      pad_y = unit(pad_y_map_arrow, "cm"),
      style = ggspatial::north_arrow_orienteering(text_size = 5)
    )
  
  # add sequence percentage and ID
  if (show_pct == TRUE & show_label == FALSE) {
    p <- p +
      ggtitle(paste0("Sequence Cluster: " , sequence_id, " (", pct_label, ")")) +
      theme(
        plot.title = element_text(size = label_size, hjust = 0.5, colour = txt_colour)
      )
  } else if (show_pct == TRUE & show_label == TRUE) {
    p <- p +
      ggtitle(paste0(seq_labs[sequence_id], "\n (", sequence_id, " - ", pct_label, ")")) +
      theme(
        plot.title = element_text(size = label_size, hjust = 0.5, colour = txt_colour)
      )
  }
  
  return(p)
  
}

# -----------------------------------------------------------------------------#
### 5.3.4 Select sequences ----
# - seq_summary: tibble from build_sequence_summary
# - sequences_to_plot: explicit vector of sequences (may be NULL)
# - top_n: integer (may be NULL)
# - include_constants: if FALSE exclude constant single-digit sequences from 
# selection 
# 
# Returns character vector of sequences to display (length <= 12)
# -----------------------------------------------------------------------------#

select_sequences <- function(seq_summary,
                             sequences_to_plot = NULL,
                             top_n = NULL,
                             include_constants = FALSE,
                             max_maps = 12,
                             constant_pattern = "^[1-9]$") {
  
  seqs <- character(0)
  
  # exclude constant sequences
  if (include_constants == FALSE) {
    seq_summary <- seq_summary %>% filter(!grepl(constant_pattern, sequence))
  }
  
  if (!is.null(sequences_to_plot)) {
    seqs <- as.character(sequences_to_plot)
  } else if (!is.null(top_n)) {
    seqs <- seq_summary %>% arrange(desc(pct_of_city)) %>% 
      slice_head(n = top_n) %>% pull(sequence)
  } else {
    # default: top 8
    seqs <- seq_summary %>% arrange(desc(pct_of_city)) %>% 
      slice_head(n = min(8,nrow(seq_summary))) %>% pull(sequence)
  }
  seqs <- as.character(seqs)
  
  # trim to max maps 
  if (length(seqs) > max_maps) seqs <- seqs[1:max_map]
  
  return(seqs)
  
}

# -----------------------------------------------------------------------------#
### 5.3.5 Assemble small maps only ----
# - small_maps: list of ggplot objects in desired clockwise order (max 12)
# - seq_summary: tibble (used if labeling is needed)
# - layout_params: list controlling sizes, background, margin, etc.
#   list(
#     bg_colour = "white",
#     center_size = 0.45,
#     small_map_size = 0.18,
#     margin = 0.03,
#     show_percent = TRUE
#   ) 
# 
# Returns a ggdraw object (cowplot canvas) with all maps placed
# -----------------------------------------------------------------------------#

assemble_small_maps <- function(small_maps,
                                seq_summary = NULL,
                                bg_colour = "white",
                                map_order = NULL) {
  
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # pad list of small maps to length 12 with NULLs
  n_maps <- length(small_maps)
  if (n_maps > 9) {
    small_maps <- small_maps[1:9]
  } else {
    small_maps <- c(small_maps, rep(list(NULL), 9 - n_maps))
  }
  
  # assign positions 
  if (is.null(map_order)) {
    
    # clockwise from top left
    T1 <- small_maps[[1]]
    T2 <- small_maps[[2]]
    T3 <- small_maps[[3]]
    M1 <- small_maps[[4]]
    M2 <- small_maps[[5]]  
    M3 <- small_maps[[6]]
    B1 <- small_maps[[7]]
    B2 <- small_maps[[8]]
    B3 <- small_maps[[9]]
    
  } else {
    
    if (length(map_order) != 9) {
      stop("map_order vector must be of length 9")
    }
    
    T1 <- small_maps[[map_order[1]]]
    T2 <- small_maps[[map_order[2]]]
    T3 <- small_maps[[map_order[3]]]
    M1 <- small_maps[[map_order[4]]]
    M2 <- small_maps[[map_order[5]]]  
    M3 <- small_maps[[map_order[6]]]
    B1 <- small_maps[[map_order[7]]]
    B2 <- small_maps[[map_order[8]]]
    B3 <- small_maps[[map_order[9]]]
    
  }
  
  # define black plot placeholder 
  blank <- ggplot() + theme_void()
  bg_theme <- theme(
    plot.background = element_rect(fill = bg_colour, colour = NA)
  )
  blank <- blank + bg_theme 
  
  # build rows with plot_grid horizontally 
  top_row <- plot_grid(
    T1 %||% blank, T2 %||% blank, T3 %||% blank,
    nrow = 1, rel_widths = c(1,1,1)
  )
  
  mid_row <- plot_grid(
    M1 %||% blank, M2 %||% blank, M3 %||% blank,
    nrow = 1, rel_widths = c(1,1,1)
  )
  
  bottom_row <- plot_grid(
    B1 %||% blank, B2 %||% blank, B3 %||% blank,
    nrow = 1, rel_widths = c(1,1,1,1)
  )
  
  final_plot <- plot_grid(
    top_row,
    mid_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1, 1, 1)
  ) + bg_theme 
  
  return(final_plot)
  
}


# -----------------------------------------------------------------------------#
### 5.3.6 create cluster legend ----
# - colour_palette: vector with named hex codes (e.g., "1" = "#ffff00")
# - txt_colour: legend text colour
# - bg_colour: background colour
# - show_cons_pct: if TRUE show percentage of constant sequence in legend
# - seq_summary: tibble with percentage of sequences (for legend), default NULL
# - legend_labels: vector of labels (default is "Cluster [cluster number]")
# 
# Returns a grob object (cowplot legend) 
# -----------------------------------------------------------------------------#

make_cluster_legend <- function(colour_palette, 
                                other_colour,
                                txt_colour = "black", 
                                bg_colour = "white", 
                                show_cons_pct = TRUE, 
                                seq_summary = NULL, 
                                legend_labels = NULL,
                                gem_name,
                                gem_colour = "black",
                                urbreg_colour = "black",
                                urbreg_linetype = "11",
                                pct_digits = 1,
                                txt_size = 7,
                                key_size = 0.5,
                                spacing = 0.2,
                                constant_pattern = "^[1-9]$") {
  
  # use zgem to put it at the end 
  fill_values <- c(colour_palette, other = other_colour, zgem = NA, zurbreg = NA)
  colour_values <- c(colour_palette, other = other_colour, zgem = gem_colour, zurbreg = urbreg_colour)
  linetype_values <- c(setNames(rep("solid", length(colour_palette)), seq(1:nclust)), 
                       other = "solid", 
                       zgem = "solid",
                       zurbreg = urbreg_linetype)
  
  if (is.null(legend_labels)) {
    legend_labels <- paste("Cluster", 1:nclust)
    legend_labels[["other"]] <- "Transitioning sequences"
  }
  
  legend_labels[["zgem"]] <- paste0("Borders municipality of ", gem_name)
  legend_labels[["zurbreg"]] <- paste0("Borders of urban region")
  
  # add percentage in the legend label
  if (show_cons_pct == TRUE) {
    if (is.null(seq_summary)) {
      stop("Provide seq_summary tibble with percentages")
    } else {
      
      # select constant sequences 
      pct_cons_seq <- seq_summary %>% 
        filter(grepl(constant_pattern, sequence)) %>% 
        select(sequence, pct_of_city)
      
      # format pct and store
      pct_label <- list()
      for (i in 1:nclust) {
        pct_seq <- pct_cons_seq %>% filter(sequence == as.character(i)) %>% pull(pct_of_city)
        pct_label[i] <- sprintf("%.*f%%", pct_digits, pct_seq)
        legend_labels[i] <- paste0(legend_labels[i]," (", pct_label[i], ")")
      }
      
      # transitioning sequences 
      pct_seq <- seq_summary %>% 
        filter(!(grepl(constant_pattern, sequence))) %>%
        summarise(pct_nc = sum(pct_of_city, na.rm = TRUE)) %>% 
        pull(pct_nc)
      
      pct_label["other"] <- sprintf("%.*f%%", pct_digits, pct_seq)
      legend_labels["other"] <- paste0(legend_labels["other"]," (", pct_label["other"], ")")
    }
  }
  
  
  df_leg <- tibble(
    cluster = as.factor(c(1:nclust, "other", "zgem", "zurbreg")),
    x = 1:(nclust+3),
    y = 1:(nclust+3)
  )
  
  p_leg <- ggplot(df_leg, aes(x = x, y = y)) +
    geom_tile(aes(fill = cluster, colour = cluster, linetype = cluster), linewidth = 0.7) +
    scale_fill_manual(
      values = fill_values,
      labels = legend_labels,
      name = "Legend",
      na.value = NA
    ) +
    scale_colour_manual(
      values = colour_values,
      labels = legend_labels,
      name = "Legend",
      na.value = NA
    ) +
    scale_linetype_manual(
      values = linetype_values,
      labels = legend_labels,
      breaks = levels(df_leg$cluster),
      name = "Legend",
      na.value = NA
    ) +
    guides(fill = guide_legend(ncol = 1),
           colour = guide_legend(ncol = 1),
           linetype = guide_legend(ncol = 1)) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = txt_size, colour = txt_colour),
      legend.box.background = element_rect(fill = alpha("gray95", .5)),
      #legend.box.margin = margin(b = 0),
      legend.key.size = unit(key_size, "cm"),
      legend.key.spacing.x = unit(spacing, "cm")
    )
  
  clust_legend <- cowplot::get_legend(p_leg)
  return(clust_legend)
}



#------------------------------------------------------------------------------#
# 5. Sequence analysis ----

## 5.1 Plot representative sequences ----
plot_cluster_r_seq <- function(seq_clust_n, seq_plot_title, txt_size = 20, 
                               title_size = 15, subtitle_size = 10) {
  
  # add title and subtitle
  plot_r_seq <- p[[seq_clust_n]] + 
    labs(title = seq_plot_title, 
         subtitle = paste0(
           "N = ", 
           n_seq_cluster[[seq_clust_n]])
    ) + 
    theme(legend.position = "none",
          plot.title = element_text(size = title_size, face = "bold"),
          plot.subtitle = element_text(size = subtitle_size, face = "bold"),
          axis.text.x = element_text(size = 8, face = "bold"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_x_discrete(labels = c(2003, "", "", "", "", 2023))
  
  # add info with pen description of transition  
  plot_text <- ggdraw() +
    draw_plot(plot_r_seq, 0, 0, 1, 0.6, scale = 0.65) +
    draw_label(seq_labs[as.character(seq_clust_n)], x = 0.5, y = 0.1, size = txt_size, hjust = 0.5) +
    theme(
      plot.margin = unit(c(-3.5, 0, 0, 0), "cm"),
      panel.spacing = unit(0,"pt")
    )
  
  return(plot_text)
}


## 5.2 Heatmap of sequence clusters function ----
plot_heatmap_seq_clusters <- function(data, plot_var, title, subtitle = "", max_limit = NULL, angle_x_label = 90){
  
  if (is.null(max_limit)) {
    max_limit <- max(data[[plot_var]])
  } 
  
  thr <- 0.6 * max_limit
  data[, text_col := ifelse(data[[plot_var]] > thr, "white", "black")]
  
  ggplot(data, aes(x = factor(SEQ_CLUSTER), y = URBREG, fill = data[[plot_var]])) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.0f", data[[plot_var]])),
              color = data$text_col,
              size = 2.5) +
    scale_fill_gradient(
      low = "white",
      high = "#08519C",
      limits = c(0, max_limit),
      breaks = c(0, max_limit/2, max_limit),
      labels = c(0, round(max_limit/2, 0), round(max_limit, 0)),
      guide = guide_colorbar(ticks.colour = NA)
    ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(drop = TRUE, expand = c(0,0)) +
    facet_grid(region_group~., scales = "free", space = "free_y", switch = "y") +
    labs(
      #x = "Sequence Cluster", 
      #y = "Urban Region", 
      fill = "%", 
      title = title,
      subtitle = subtitle
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.placement = "outside",
      panel.spacing = unit(0.1, "in"),
      strip.background.y = element_blank(),
      axis.text.x = element_text(angle = angle_x_label, vjust = 0.5, hjust = 1, face = "bold", size = 6),
      axis.text.y = element_text(face = "bold", size = 6),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 7),
      legend.text = element_text(size = 6)
    )
}
