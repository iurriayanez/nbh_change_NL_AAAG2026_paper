#------------------------------------------------------------------------------#
# Name: 6_grid_var_scales_year.R
# Goal: Consolidate chunks to create final data base with grid cell info by scales
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/grid/grid_var_scales_[year]_[part].csv
# Output databases: data/grid/grid_var_scales_[year].rds
#------------------------------------------------------------------------------#

# Specific parameters
gemvar <- paste0("GEM", 2022)
start_year <- 2003

# Packages
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('ggplot2','readr','haven','dplyr','data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))


for (year in (start_year:inc_year)) {
  
  print(year)
  
  # 1. Import part 1 of grid_var_scale ---- 
    df <- fread(file.path(path_grid, paste0("grid_var_scales_", year, "_part_1", ".csv")))
  
  # 2. Append subsequent parts of grid_var_scale ----
    for (k in (2:num_chunks)) {
      print(k)
      df1 <- fread(file.path(path_grid, paste0("grid_var_scales_", year, "_part_", k, ".csv")))
      df <- rbind(df, df1)
      rm(df1)
    }
    
  # 3. Export ----
    save_file <- paste0("grid_var_scales_", year,".rds")
    write_rds(df, file = file.path(path_grid,save_file), compress = "gz")   
    rm(df)
    
  # 4. Remove chunks 
    for (k in 1:num_chunks) {
      csv_file <- paste0("grid_var_scales_", year, "_part_", k, ".csv")
      #file.remove(file.path(path_grid,csv_file))
    }
}

# Export selected grid cells
# Import grid cell database to add Metropolitan agglomeration and gemeente codes
grid <- read_rds(file.path(path_grid,paste0("grid_",inc_year,".rds")))
setDT(grid)

# Import the grid_var_scales dataset for selected years
for (i in start_year:inc_year) {
  
  file_rds <- paste0("grid_var_scales_",i,".rds")
  df  <- read_rds(file.path(path_grid,file_rds))
  setDT(df)
  
  # Add and filter by metropolitan agglomeration/urban region
  df<- merge(df, grid[, c('VIERKANT100M', 'URBREG')], by = 'VIERKANT100M', all.x = TRUE)
  df = df[URBREG %in% met_aggl]
  
  # Export 
  save_file <- paste0("grid_var_scales_metaggl_", met_aggl_labs_save, "_", i, ".rds")
  write_rds(df, file = file.path(path_grid,save_file), compress = "gz")   
  rm(df)
  
}