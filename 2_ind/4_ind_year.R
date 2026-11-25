#------------------------------------------------------------------------------#
# Name: 5_ind_year.R
# Goal: Create yearly databases (1999-2023) with individual's info: spatial (location and housing), demographic, and economic.
# Date of creation: 22-01-2025

# R Version: R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/ind/ind_spatial_[year].csv ; ind_demo_[year].csv ; ind_inc_[year].csv ; ind_educ_[year].csv
# Output databases: data/ind/ind_[year].rds
#------------------------------------------------------------------------------#

# Specific parameters
end_year <- 2023

pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('ggplot2','readr','haven','dplyr','data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

# Import CPI database (European Harmonized Consumer Price Index 2015 = 100)
  cpi <- fread(cpitab)
  
# Loop
  
for (year in start_year:end_year) { 
  
  print(year)
  
  #---------------------------------------#
  # 1. Merge spatial and demographics ----
  #---------------------------------------# 
  
    ## 1.1 Import spatial database ----
      spa_csv <- paste0("ind_spatial_",year,".csv")
      ind_spa <- fread(file.path(path_ind,spa_csv))
      ind_spa[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
      
    ## 1.2 Import demographic database ----
      demo_csv <- paste0("ind_demo_",year,".csv")
      ind_demo <- fread(file.path(path_ind,demo_csv))      
      ind_demo[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
      
    ## 1.3 Merge ---- 
      df <- merge(ind_spa, ind_demo, by = 'IND_ID', all.x = TRUE)
      rm(ind_spa, ind_demo)
    
  #---------------------------------------------------#
  # 2. Merge with income and socioeconomic status ----
  #---------------------------------------------------#
    
    ## 2.1 Import and merge income and socioeconomic status database ----    
      inc_csv <- paste0("ind_inc_",year,".csv")
      sec_csv  <- paste0("ind_sec_",year,".csv")
      
      if (file.exists(file.path(path_ind,inc_csv)) == TRUE) {
        ind_inc <- fread(file.path(path_ind,inc_csv))
        ind_inc[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
        df <- merge(df, ind_inc, by = 'IND_ID', all.x = TRUE)
        rm(ind_inc)
      }
      
      if (file.exists(file.path(path_ind,sec_csv)) == TRUE) {
        ind_sec <- fread(file.path(path_ind,sec_csv))
        ind_sec[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
        df <- merge(df, ind_sec, by = 'IND_ID', all.x = TRUE)      
        rm(ind_sec)
      }


  #-----------------------------#        
  # 3. Additional variables ---- 
  #-----------------------------#   
    
    if (file.exists(file.path(path_ind,inc_csv)) == TRUE) {
      # These variables can only be created with the income variable, so we need to check that the file exists for that year
    
      ## 4.1 Remove students (we don't count their income)
        df[, STUD := as.integer(MONTHS_STUD>= 1 & MONTHS_STUD<=12 & !is.na(MONTHS_STUD))]

        cols <- c('GROSS_ANNUAL_INCOME', 'GROSS_ANNUAL_INCOME_WORK', 'GROSS_ANNUAL_INCOME_WORK_SB')
        df[, (cols) := lapply(.SD, function(x) (ifelse(STUD == 1, NA, x))), .SDcols = cols]
        
      ## 4.2 Working age ---- 
        df[, WORK_AGE := as.integer(AGE >= 20 & AGE<= 65)]
  
      ## 4.3 Inflation correction ----    
        # CPI value of that year (European Harmonized Consumer Price Index 2015 = 100)
          cpi_val <- as.numeric(cpi[YEAR == year, 2]/100)
          
        # Correct for inflation 
          df[, GROSS_ANNUAL_INCOME_CPI       := GROSS_ANNUAL_INCOME * cpi_val]
          df[, GROSS_ANNUAL_INCOME_WORK_CPI  := GROSS_ANNUAL_INCOME_WORK * cpi_val]
          df[, OBJ_WOZ_CPI       := OBJ_WOZ * cpi_val]
          
      ## 4.4 Low and high income ---- 
        
        # Sample selection for individual national distribution: working age individuals (between 20 and 65 years old) who did not study that year
        # Income quintiles: 
          df[WORK_AGE == 1, INCOME_QUANT := cut(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], 
                                                quantile(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], probs = seq(0, 1, 0.25), na.rm = TRUE), 
                                                labels = FALSE, include.lowest = TRUE)]
        
        # Low income: Income below the 40% of the median of the national distribution (risk of poverty)
          median_value_ind <- median(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], na.rm = TRUE)    # Earned income
          df[WORK_AGE == 1, LOWINC := as.integer(GROSS_ANNUAL_INCOME_WORK_SB <= (0.4 * median_value_ind))]

        # High income: Income above the 75th percentile of the income distribution      
          p75_ind <- quantile(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], probs = 0.75, na.rm = TRUE)    # Earned income
          df[WORK_AGE == 1, HIGHINC := as.integer(GROSS_ANNUAL_INCOME_WORK_SB >= p75_ind)]
    }
        
    ## 4.6 Age groups ---- 
      breaks <- c(-1,18,25,35,45,55,65,Inf)
      df[, AGE_GROUP :=  cut(AGE, breaks = breaks, labels = FALSE)]

  #---------------#        
  # 5. Export ---- 
  #---------------# 
    
    setnames(df, toupper(names(df)))  
    save_file <- paste0("ind_",year,".rds")
    write_rds(df, file = file.path(path_ind,save_file), compress = "gz")  
      
  #-----------------------------------------------------#        
  # 6. Remove intermediate databases from directory ---- 
  #-----------------------------------------------------#            
    
    # ind_spatial needed in hh process (tenure)

    ## 5.2 Demo ----  
      csv_file <- paste0("ind_demo_",year,".csv")
      #file.remove(file.path(path_ind,csv_file))
      
    ## 5.3 Econ ----  
      csv_file <- paste0("ind_inc_",year,".csv")
      if (file.exists(file.path(path_ind,csv_file)) == TRUE) {
        #file.remove(file.path(path_ind,csv_file))
      }
      
      csv_file <- paste0("ind_sec_",year,".csv")
      if (file.exists(file.path(path_ind,csv_file)) == TRUE) {
        #file.remove(file.path(path_ind,csv_file))
      }

    ## 5.4 Remove dataframe from memory to restart with the next year ----
      rm(df)     
} 
   