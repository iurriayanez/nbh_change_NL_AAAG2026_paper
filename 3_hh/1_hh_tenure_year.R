#------------------------------------------------------------------------------#
# Name: 1_hh_tenure_year_[version]
# Goal: Create yearly databases (2003-2023) with tenure information from IHI and INHA (also household income, but not consistent between IHI and INHA). 
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  INHA ; IHI
# Output databases: data/hh/hh_tenure_[year].rds
#------------------------------------------------------------------------------#

# Specific parameters
end_year <- 2023

pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('ggplot2','readr','haven','dplyr','data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

# Import CPI database (European Harmonized Consumer Price Index 2015 = 100)
cpi <- fread(cpitab)

# 1. INHA ----

## 1.1 Relevant variables for all years ----

cols_inha = c( # ID 
  'RINPERSOONHKW', 'RINPERSOONSHKW',
  # STANDARDISED DISPOSABLE HOUSEHOLD INCOME (VALUE AND PERCENTILES)
  'INHGESTINKH', 'INHP100HGESTES',
  # RENTAL SUBSIDY
  'INHEHALGR',
  # TYPE OF HOUSEHOLD
  'INHPOPIIV')  

# INHP100HGEST IS THE PERCENTILE OF STD DISPOSABLE HH INCOME WITHOUT INSTITUTIONAL AND STUDENT HOUSEHOLDS (NOT IN IHI).


# 1.2 Loop INHA (2011-2023) ----

# Over all years available for INHA (2011-2023)

for (year in 2011:inc_year) { 
  
  print(year)

  ### 1.2.1 Import the database ----
  
    # Define the path 
    assign("inhatab", get(paste0("inhatab_",year)), envir = .GlobalEnv)
  
    # Import
    inha = read_sav(inhatab)
    
    # Set as data.table
    setDT(inha)
    
    # Select columns
    setnames(inha, toupper(names(inha)))                    # All variable names to upper case
    inha <- inha[, ..cols_inha, with = FALSE]
    
    # Change factor variables to numeric or string
    inha[, (cols_inha) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols_inha]
    
    cols_factor = c('INHPOPIIV','INHGESTINKH','INHP100HGESTES','INHEHALGR')
    inha[, (cols_factor) := lapply(.SD, function(x) (as.numeric(x))), .SDcols = cols_factor]
  
    ### 1.2.2 Prep variables ----
  
    # ID: RINPERSOON from main breadwinner of the household
    
    # Mean Breadwinner ID
    inha$MB_ID = paste0(inha$RINPERSOONHKW,inha$RINPERSOONSHKW) 
    inha[, (c('RINPERSOONHKW', 'RINPERSOONSHKW')) := NULL]
    
    # Missing values 
    
    # We consider private households with income information
    inha = inha[ (inha$INHPOPIIV == 1),]
    
    # Income value
    cols_m = c('INHGESTINKH')
    inha[, (cols_m) := lapply(.SD, function(x) ifelse(x == 9999999999, NA, x)), .SDcols = cols_m]
    
    # Income percentile
    cols_m = c('INHP100HGESTES')
    inha[, (cols_m) := lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = cols_m]
    
    # Inflation correction 
    # CPI value of that year (European Harmonized Consumer Price Index 2015 = 100)
    cpi_val <- as.numeric(cpi[YEAR == year, 2]/100)
    
    # Correct for inflation 
    inha[, HH_STDDISPINC_CPI := INHGESTINKH * cpi_val]
    
    
    ### 1.2.3 Low and high income households (standardized disposable income)  ----
    # Low income: Lowest 25% 
    # High income: Top 25%
    
    # Low income household  
    inha[, HH_LOWSTDDISPINC := ifelse((INHP100HGESTES <= 25 & !is.na(INHP100HGESTES)), 1, 
                              ifelse((INHP100HGESTES > 25 & !is.na(INHP100HGESTES)),0,NA))]
    
    # High income household  
    inha[, HH_HIGHSTDDISPINC := ifelse((INHP100HGESTES >= 75 & !is.na(INHP100HGESTES)), 1, 
                               ifelse((INHP100HGESTES < 75 & !is.na(INHP100HGESTES)),0,NA))]        
    
    # Middle income household  
    inha[, HH_MIDSTDDISPINC := ifelse(HH_HIGHSTDDISPINC == 0 & HH_LOWSTDDISPINC == 0, 1, 
                             ifelse((is.na(HH_HIGHSTDDISPINC) | is.na(HH_LOWSTDDISPINC)),NA,0))]  
    ### 1.2.4 Location (grid cell ID)  ----
    read_file <- paste0("ind_spatial_",year,".csv")
    ind_spa <- fread(file.path(path_ind,read_file))
    cols_spa <- c('IND_ID', 'VIERKANT100M', 'VIERKANT500M','METAGGL','URBREG') 
    ind_spa <- ind_spa[, ..cols_spa, with = FALSE]
    ind_spa[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
    
    inha <- merge(inha, ind_spa, by.x = "MB_ID", by.y = "IND_ID", all.x = T, all.y = F)
    
    ### 1.2.5 Export ----
    # Rename columns 
    old_names <- c('INHGESTINKH','INHP100HGESTES','INHEHALGR')
    new_names <- c('HH_STDDISPINC', 'HH_STDDISPINCPERC', 'HH_TENURE')
    setnames(inha, old = old_names, new = new_names)
    
    # Export 
    cols <-  c(
      'MB_ID', 
      'HH_STDDISPINC', 
      'HH_STDDISPINC_CPI', 
      'HH_STDDISPINCPERC', 
      'HH_LOWSTDDISPINC', 
      'HH_HIGHSTDDISPINC', 
      'HH_MIDSTDDISPINC', 
      'HH_TENURE',
      'VIERKANT100M', 
      'VIERKANT500M',
      'METAGGL',
      'URBREG'
    )
    inha <- inha[, ..cols, with = FALSE]

    save_file <- paste0("hh_tenure_",year,".rds")
    write_rds(inha, file = file.path(path_hh,save_file), compress = "gz")   
    rm(inha, ind_spa)
}    


# 2. IHI ----

## 2.1 Relevant variables ----
cols_ihi = c( # ID 
  'RINPERSOONSKERN', 'RINPERSOONKERN',
  # STANDARDISED DISPOSABLE HOUSEHOLD INCOME (VALUE)
  'BVRGESTINKH', 
  # RENTAL SUBSIDY
  'BVREHALG',
  # TYPE OF HOUSEHOLD
  'BVRPOPIIV')  

# PERCENTILE OF STD DISPOSABLE HOUSEHOLD INCOME ('BVRPERCGESTINKH') EXCLUDES INSTITUTIONAL BUT CONSIDERS STUDENT HOUSEHOLDS SO WE CALCULATE IT

## 2.2 Loop (2003-2010) ----

for (year in 2003:2010) { 
  
  print(year)

  ### 2.2.1 Import the database ----
  
  # Define the path 
  assign("ihitab", get(paste0("ihi_",year)), envir = .GlobalEnv)
  
  # Import
  ihi = read_sav(ihitab)
    
  # Set as data.table
  setDT(ihi)
    
  # Select columns
  setnames(ihi, toupper(names(ihi)))                    # All variable names to upper case
  ihi <- ihi[, ..cols_ihi, with = FALSE]
    
  # Change factor variables to numeric or string
  ihi[, (cols_ihi) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols_ihi]
    
  cols_factor = c(
    # STANDARDISED DISPOSABLE HOUSEHOLD INCOME (VALUE)
    'BVRGESTINKH',
    # RENTAL SUBSIDY
    'BVREHALG',
    # TYPE OF HOUSEHOLD
    'BVRPOPIIV'
    )  
    ihi[, (cols_factor) := lapply(.SD, function(x) (as.numeric(x))), .SDcols = cols_factor]
  
  ### 2.2.2 Prep variables ----
  
  # ID: RINPERSOON from main breadwinner of the household
    
  # Mean Breadwinner ID
  ihi$MB_ID = paste0(ihi$RINPERSOONKERN,ihi$RINPERSOONSKERN) 
  ihi[, (c('RINPERSOONKERN', 'RINPERSOONSKERN')) := NULL]
  
  # Missing values 
  
  # We consider private households with income information
  ihi = ihi[ (ihi$BVRPOPIIV == 1),]
  
  # Income variables
  cols_m = c('BVRGESTINKH')
  ihi[, (cols_m) := lapply(.SD, function(x) ifelse(x == 999999999, NA, x)), .SDcols = cols_m]
  
  # Inflation correction 
  # CPI value of that year (European Harmonized Consumer Price Index 2015 = 100)
  cpi_val <- as.numeric(cpi[YEAR == year, 2]/100)
  
  # Correct for inflation 
  ihi[, HH_STDDISPINC_CPI := BVRGESTINKH * cpi_val]

  ### 2.2.3 Low and high income households (standardized disposable income)  ----
  # Low income: Lowest 25% 
  # High income: Top 25%
  
  # Percentiles 
  ihi[, HH_STDDISPINCPERC := cut(BVRGESTINKH, breaks = quantile(BVRGESTINKH, seq(0,1, by = 0.01), na.rm = TRUE), labels = FALSE, include.lowest = TRUE)]

  # Low income household  
  ihi[, HH_LOWSTDDISPINC := ifelse((HH_STDDISPINCPERC <= 25 & !is.na(HH_STDDISPINCPERC)), 1, 
                            ifelse((HH_STDDISPINCPERC > 25 & !is.na(HH_STDDISPINCPERC)),0,NA))]
  
  # High income household  
  ihi[, HH_HIGHSTDDISPINC := ifelse((HH_STDDISPINCPERC >= 75 & !is.na(HH_STDDISPINCPERC)), 1, 
                             ifelse((HH_STDDISPINCPERC < 75 & !is.na(HH_STDDISPINCPERC)),0,NA))]        
  
  # Middle income household  
  ihi[, HH_MIDSTDDISPINC := ifelse(HH_HIGHSTDDISPINC == 0 & HH_LOWSTDDISPINC == 0, 1, 
                           ifelse((is.na(HH_HIGHSTDDISPINC) | is.na(HH_LOWSTDDISPINC)),NA,0))]  
  
  ### 2.2.4 Location (grid cell ID)  ----
  read_file <- paste0("ind_spatial_",year,".csv")
  ind_spa <- fread(file.path(path_ind,read_file))
  cols_spa <- c('IND_ID', 'VIERKANT100M', 'VIERKANT500M','METAGGL','URBREG') 
  ind_spa <- ind_spa[, ..cols_spa, with = FALSE]
  ind_spa[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
  
  ihi <- merge(ihi, ind_spa, by.x = "MB_ID", by.y = "IND_ID", all.x = T, all.y = F)
  
  ## 2.2.4 Export ----
  
  ### Rename columns ----
  old_names <- c('BVRGESTINKH','BVREHALG')
  new_names <- c('HH_STDDISPINC','HH_TENURE')
  setnames(ihi, old = old_names, new = new_names)
  
  # Export 
  cols <-  c(
    'MB_ID', 
    'HH_STDDISPINC', 
    'HH_STDDISPINC_CPI', 
    'HH_STDDISPINCPERC', 
    'HH_LOWSTDDISPINC', 
    'HH_HIGHSTDDISPINC', 
    'HH_MIDSTDDISPINC', 
    'HH_TENURE',
    'VIERKANT100M', 
    'VIERKANT500M',
    'METAGGL',
    'URBREG'
    )
  ihi <- ihi[, ..cols, with = FALSE]
  
  save_file <- paste0("hh_tenure_",year,".rds")
  write_rds(ihi, file = file.path(path_hh,save_file), compress = "gz")   
  rm(ihi, ind_spa)

}                 
