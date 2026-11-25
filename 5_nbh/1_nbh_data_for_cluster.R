#------------------------------------------------------------------------------#
# Name: 1_nbh_data_for_cluster.R
# Goal: Create dataset for sociospatial signature clustering 
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/grid/grid_var_scales_metaggl_all_[year].rds
# Output databases: data/nbh/data_for_clustering_metaggl_all.rds
#------------------------------------------------------------------------------#
# 0. Setup ----
library(dplyr)
library(data.table)
library(readr)


# 1. Import datasets by year ----

df_long = data.table()

## 1.1 Import the grid_var_scales dataset for selected years ----
for (i in years) {
  
  print(i)
  file_rds <- paste0("grid_var_scales_metaggl_", met_aggl_labs_save, "_", i, ".rds")
  df  <- read_rds(file.path(path_grid,file_rds))
  setDT(df)
  
  # Create year variable
  df[, YEAR := i]
  
  # Append
  df_long = rbind(df_long, df)
  rm(df)
  
}

df = df_long
rm(df_long)

# 2. Relevant variables ----
cols <- c(
  # IDs
  paste0('VIERKANT', grid_size,'M'),
  'URBREG', 
  'YEAR',
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
  c(grep("^PERC_HH_SUBRENTAL", names(df), value = TRUE)),
  # Population
  c(grep("^TOT_IND", names(df), value = TRUE))
)

# Data with selected variables 
df <- df[, cols, with = FALSE]

# 3. Preprocessing ---- 
## 3.1 Remove missing values ---- 
length(unique(df$VIERKANT100M))
df = na.omit(df)
length(unique(df$VIERKANT100M))

## 3.2 Keep grids without NAs throughout the window ---- 
df[, COUNT_VIERKANT := .N, by = VIERKANT100M]
df = df[COUNT_VIERKANT == length(years),]
df[, COUNT_VIERKANT := NULL]
length(unique(df$VIERKANT100M))

# 4. Export ----
save_file <- paste0("data_for_clustering_metaggl_", met_aggl_labs_save, ".rds")
write_rds(df, file = file.path(path_nbh,save_file), compress = "gz")   
rm(df); gc()