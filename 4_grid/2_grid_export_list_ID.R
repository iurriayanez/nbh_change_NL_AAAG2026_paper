#------------------------------------------------------------------------------#
# Name: 2_grid_export_list_ID.R
# Goal: Create list with all the grid cells with available information between 1999 - 2022
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  grid_[year].rds
# Output database: data/grid/list_VIERKANT[grid_size]M_1999_2023.csv    
#------------------------------------------------------------------------------#

# Specific parameters
gemvar <- paste0("GEM", 2022)
start_year <- 2003

# Packages
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('ggplot2','readr','haven','dplyr','data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

df_list = data.table()

for (year in start_year:inc_year) { 
  print(year)
  df = read_rds(file.path(path_grid,paste0("grid_",year,".rds")))
  setDT(df)
  df = df[, VIERKANT100M]
  df_list = rbind(df_list, df)
  df_list = unique(df_list)
  rm(df)
}

# Export 
setnames(df_list, 'x', 'VIERKANT100M')
fwrite(df_list, file = file.path(path_grid,vierkantlistname))   

rm(grid_df, ind_df, hh_df, obj_df)

  
    
  
 
