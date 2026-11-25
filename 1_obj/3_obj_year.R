#------------------------------------------------------------------------------#
# Name: 3_obj_year.R
# Goal: Create yearly databases (1999-2024) with address object information: spatial location and tenure/WOZ value
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  data/ind/ind_spa/obj_tenure_[year].csv ; data/ind/ind_spa/obj_spatial_[year].csv
# Output databases: data/ind/ind_spa/obj_[year].rds
#------------------------------------------------------------------------------#

# Specific parameters
end_year <- 2024

# Packages
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('haven','dplyr','data.table','readr')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

# 1. Import spatial database ----
  obj_spa <- fread(file.path(path_obj,"obj_spatial.csv"))
      
# 2. Import tenure (yearly), merge with spatial, export and remove ----
  
  pb = txtProgressBar(min = 0 , max = length(start_year:end_year), style = 3)       
  
  for (year in start_year:end_year) {    
    
    ## 2.1 Import tenure database and merge ----
    tenure_csv <- paste0("obj_tenure_",year,".csv")
    if (file.exists(file.path(path_obj,tenure_csv)) == TRUE) {
      obj_tenure <- fread(file.path(path_obj,tenure_csv))      
      df <- merge(obj_spa, obj_tenure, by = 'OBJ_ID', all.x = TRUE)
      rm(obj_tenure)
    } else {
      df <- obj_spa
    }

    ## 2.2 Export ----
      save_file <- paste0("obj_",year,".rds")
      write_rds(df, file = file.path(path_obj,save_file), compress = "gz")
      rm(df)
        
  
    ## 2.3 Remove tenure ----  
      csv_file <- paste0("obj_tenure_",year,".csv")
      if (file.exists(file.path(path_obj,csv_file)) == TRUE) {
        #file.remove(file.path(path_obj,csv_file))
      }
        
    # Update progress bar 
    setTxtProgressBar(pb, year - start_year + 1)     
  }
  
  close(pb)    

  ## 2.4 Remove spatial ----  
    #file.remove(file.path(path_obj,"obj_spatial.csv"))

          