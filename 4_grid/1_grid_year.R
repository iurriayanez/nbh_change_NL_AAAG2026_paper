#------------------------------------------------------------------------------#
# Name: 1_grid_year.R
# Goal: Create yearly databases (1999-2023) with grid's aggregate info: demographic, socioeconomic and housing composition
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/ind/ind_[year].rds, data/hh/hh_tenure_[year].rds, data/hh/hh_inc_[year].rds,
# Output databases: data/grid/grid_[year].rds
#------------------------------------------------------------------------------#

# Specific parameters
gemvar <- paste0("GEM", 2022)
start_year <- 2003

# Packages
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('ggplot2','readr','haven','dplyr','data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

# Loop
      
pb = txtProgressBar(min = 0 , max = length(start_year:inc_year), style = 3)
      
for (year in start_year:inc_year) { 
  
  print(year)
  
  #----------------------------------#
  # 1.Import individual database ----
  #----------------------------------#  
  
  ind_rds <- paste0("ind_",year,".rds")
  ind_df  <- read_rds(file.path(path_ind,ind_rds))
  setDT(ind_df)
  
  # Remove missing grid ID 
  ind_df <- ind_df[nchar(VIERKANT100M) == 10 & VIERKANT100M != '----------',]
  
  # Household database (household type)
  hh_df <- ind_df[HH_REFPER == 1,]
  
  # Household tenure database 
  hh_tenure_rds <- paste0("hh_tenure_",year,".rds")
  hh_tenure_df  <- read_rds(file.path(path_hh,hh_tenure_rds))   
  
  # Household income database 
  hh_income_rds <- paste0("hh_inc_",year,".rds")
  hh_income_df  <- read_rds(file.path(path_hh,hh_income_rds))  
  
  # Object (WOZ value)
  obj_df <- ind_df[, .(VIERKANT100M = first(VIERKANT100M), 
                       METAGGL = first(METAGGL), 
                       OBJ_WOZ_CPI  = first(OBJ_WOZ_CPI),
                       OBJ_RENTED   = first(OBJ_RENTED)), by = OBJ_ID ]
  
  #-------------------------#
  # 2. Total population ----
  #-------------------------#
  
  # Individuals  
  gemvar <- paste0("GEM", 2022)
  grid_df <- ind_df[, .(TOT_IND = .N, GEM = min(get(gemvar)), METAGGL = min(METAGGL), URBREG = min(URBREG)), by = .(VIERKANT100M)]
  setnames(grid_df, "GEM", gemvar)
  
  #-------------------#
  # 3. Age groups ----
  #-------------------#    
    
    # Population: All individuals 
  
    ## 3.1 Counts ----
      aux <- dcast(ind_df, VIERKANT100M ~ AGE_GROUP, fun.aggregate = length)
      setnames(aux, c('1','2','3','4','5','6','7'), paste0('AGE_GROUP_',1:7), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
    
    ## 3.2 Percentages ----  
      cols <- c(paste0('AGE_GROUP_',1:7))
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/TOT_IND)), .SDcols = cols ]
      
  #--------------------------#
  # 4. Ethnic background ----
  #--------------------------#    
    
    # Population: All individuals
    # CBS variable: 1 NL; 2 Europe; 3 Turkey; 4 Morocco; 5 Suriname; 6 Dutch Caribbean; 7 Indonesia; 8 Other Africa; 9 Other Asia; 10 Other America and Oceania

    ## 4.1 Counts ----
      aux <- dcast(ind_df, VIERKANT100M ~ ETHNEWCBS, fun.aggregate = length)
      setnames(aux, c('1','2','3','4','5','6','7', '8','9','10'), c('ETHNL','ETHEUR','ETHTUR',
                                                                         'ETHMOR', 'ETHSUR', 'ETHCAR', 'ETHINDO',
                                                                         'ETHOAF', 'ETHOAS', 'ETHOAM'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
      
      ## 4.2 Percentages ----  
      cols <- c('ETHNL','ETHEUR','ETHTUR',
                'ETHMOR', 'ETHSUR', 'ETHCAR', 'ETHINDO',
                'ETHOAF', 'ETHOAS', 'ETHOAM')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(ETHNL + ETHEUR +
                                                       ETHTUR + ETHMOR + ETHSUR + 
                                                       ETHCAR + ETHINDO +
                                                       ETHOAF + ETHOAS + ETHOAM))), .SDcols = cols ]     
      
  #-------------------------------------------#
  # 5. Low/high income private households ----
  #-------------------------------------------#   
    
    # Population: Private non-student households with low or high household income.
      # Private non-student household: Only one reference person who is not a student that year (PRIV_HH == 1 & HHREFPER == 1)
      # Household income: Sum of income from work, social benefits and pensions of people who are not students
      
    ## 5.1 Counts ----
      # Number of private households and households by income 
      aux <- hh_income_df[, .(TOT_PRIV_HH = .N, 
                              HH_HIGHINC = sum(HH_HIGHINC, na.rm = T), 
                              HH_MIDINC = sum(HH_MIDINC, na.rm = T), 
                              HH_LOWINC = sum(HH_LOWINC, na.rm = T)
                              ),  
                          by = VIERKANT100M]
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE, all.y = F)
      rm(aux)
      
      # Number of households by std disposable income (IHI and INHA)
      aux <- hh_tenure_df[, .(TOT_PRIV_HH_IHI = .N, 
                              HH_HIGHSTDDISPINC = sum(HH_HIGHSTDDISPINC, na.rm = T), 
                              HH_MIDSTDDISPINC = sum(HH_MIDSTDDISPINC, na.rm = T), 
                              HH_LOWSTDDISPINC = sum(HH_LOWSTDDISPINC, na.rm = T)),  
                          by = VIERKANT100M]
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE, all.y = F)
      
    ## 5.2 Percentages ----  
      cols <- c('HH_HIGHINC','HH_MIDINC','HH_LOWINC')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(HH_HIGHINC + HH_MIDINC + HH_LOWINC))), .SDcols = cols ]
        
      cols <- c('HH_HIGHSTDDISPINC','HH_MIDSTDDISPINC','HH_LOWSTDDISPINC')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(HH_HIGHSTDDISPINC + HH_MIDSTDDISPINC + HH_LOWSTDDISPINC))), .SDcols = cols ]
       
  #---------------------#
  # 6. Unemployment ----
  #---------------------#   
      
    # Population: Working age non-students individuals who received the unemployment benefit at least one month during the year
      
    ## 6.1 Counts ----
      ind_df[WORK_AGE == 1 & (STUD == 0 | is.na(STUD)), aux := ifelse(MONTHS_UNEMPL >= 1 & MONTHS_UNEMPL <= 12, 1, 
                                                                      ifelse(MONTHS_UNEMPL == 0 | is.na(MONTHS_UNEMPL), 0, NA))]
      ind_df[WORK_AGE == 1 & (STUD == 0 | is.na(STUD)) & is.na(MONTHS_UNEMPL), aux := 0]
      aux <- dcast(ind_df, VIERKANT100M ~ aux, fun.aggregate = length)
      setnames(aux, c('NA','0','1'), c('MISSINGUNEMPL','NOUNEMPL', 'UNEMPL'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      ind_df[, aux := NULL]
      rm(aux)
      
    ## 6.2 Percentages ----  
      cols <- c('NOUNEMPL','UNEMPL')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(NOUNEMPL + UNEMPL))), .SDcols = cols ]
      cols <- c('MISSINGUNEMPL')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(MISSINGUNEMPL + NOUNEMPL + UNEMPL))), .SDcols = cols ]

    
  #-------------------------#
  # 7. Social benefits ----
  #-------------------------#   
      
    # Population: Working age non-students individuals who received social benefits at least one month during the year
      
    ## 7.1 Counts ----
      ind_df[WORK_AGE == 1 & (STUD == 0 | is.na(STUD)), aux := ifelse(MONTHS_SOCBENEF >= 1 & MONTHS_SOCBENEF <= 12, 1, 
                                                                      ifelse(MONTHS_SOCBENEF == 0 | is.na(MONTHS_SOCBENEF), 0, NA))]
      ind_df[WORK_AGE == 1 & (STUD == 0 | is.na(STUD)) & is.na(MONTHS_SOCBENEF), aux := 0]
      aux <- dcast(ind_df, VIERKANT100M ~ aux, fun.aggregate = length)
      setnames(aux, c('NA','0','1'), c('MISSINGSOCBENEF','NOSOCBENEF', 'SOCBENEF'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      ind_df[, aux := NULL]
      rm(aux)
      
    ## 7.2 Percentages ----  
      cols <- c('NOSOCBENEF','SOCBENEF')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(NOSOCBENEF + SOCBENEF))), .SDcols = cols ]
      cols <- c('MISSINGSOCBENEF')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(MISSINGSOCBENEF + NOSOCBENEF + SOCBENEF))), .SDcols = cols ]
   
  #------------------#
  # 8. Students ----
  #------------------#   
      
    # Population: Individuals (>18) who studied at least one month during the year
      
    ## 8.1 Counts ----
      ind_df[, STUD_18 := 0]
      ind_df[STUD == 1 & AGE_GROUP > 1, STUD_18 := 1]
      aux <- dcast(ind_df, VIERKANT100M ~ STUD_18, fun.aggregate = length)
      setnames(aux, c('0','1'), c('NOSTUDENT', 'STUDENTS'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
      
    ## 8.2 Percentages ----  
      cols <- c('STUDENTS')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(NOSTUDENT + STUDENTS))), .SDcols = cols ]
      
  #---------------------------#
  # 9. Type of household ----
  #---------------------------#    
    
    # Population: Private households (CBS definition) (HHREFPER == 1)
    # 1: Single, 2: Couple without children, 3: Couple with children, 4: Single-parent households, 5: Others
    ## 9.1 Counts ----
      hh_df[, HH_TYPE := ifelse(HH_TYPE == 1, 1,
                                ifelse(HH_TYPE == 2 | HH_TYPE == 3, 2, 
                                       ifelse(HH_TYPE == 4 | HH_TYPE == 5, 3,
                                              ifelse(HH_TYPE == 6, 4, 
                                                     ifelse(HH_TYPE == 7, 5, 
                                                            ifelse(HH_TYPE == 8, 6, NA))))))]
      aux <- dcast(hh_df, VIERKANT100M ~ HH_TYPE, fun.aggregate = length)
      setnames(aux, c('1','2','3','4','5'), paste0('HH_TYPE_',1:5), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
      
    ## 9.2 Percentages ----  
      cols <- c(paste0('HH_TYPE_',1:5))
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(HH_TYPE_1 + HH_TYPE_2 + HH_TYPE_3 + HH_TYPE_4 + HH_TYPE_5 ))), .SDcols = cols ]   
    
  #-------------------#
  # 10. WOZ value ----
  #-------------------#    
        
    # Population: All residential address objects with non-zero positive WOZ values
        
    ## 10.1 Median value ----
      aux_obj <- obj_df[OBJ_WOZ_CPI > 0]  
      aux_obj$OBJ_WOZ_CPI <- as.numeric(aux_obj$OBJ_WOZ_CPI) 
      aux <- aux_obj[, .(MED_WOZ_CPI = median(OBJ_WOZ_CPI, na.rm = TRUE)), by = VIERKANT100M]
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
        
    ## 10.2 Count of objects by quantiles of WOZ value ----
      #quantiles <- c(0, 0.25, 0.5, 0.75, 1)
      quantiles <- seq(0,1, by = 0.1)
      aux_obj[, QUINT_WOZ := cut(OBJ_WOZ_CPI, breaks = quantile(OBJ_WOZ_CPI, quantiles, na.rm = TRUE), labels = FALSE)]
      aux <- dcast(aux_obj, VIERKANT100M ~ QUINT_WOZ, fun.aggregate = length)
      #setnames(aux, c('NA','1','2','3','4'), c('MISSINGWOZ','P25_WOZ_CPI', 'P50_WOZ_CPI', 'P75_WOZ_CPI', 'P100_WOZ_CPI'), skip_absent = TRUE)
      setnames(aux, c('NA','1','2','3','4','5','6','7','8','9','10'), 
               c('MISSINGWOZ','P10_WOZ_CPI', 'P20_WOZ_CPI', 'P30_WOZ_CPI',
                 'P40_WOZ_CPI', 'P50_WOZ_CPI', 'P60_WOZ_CPI', 'P70_WOZ_CPI',
                 'P80_WOZ_CPI','P90_WOZ_CPI','P100_WOZ_CPI'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux, aux_obj)
      
      ## 10.3 Percentages ----  
      cols <- c('P10_WOZ_CPI', 'P20_WOZ_CPI', 'P30_WOZ_CPI',
                'P40_WOZ_CPI', 'P50_WOZ_CPI', 'P60_WOZ_CPI', 'P70_WOZ_CPI',
                'P80_WOZ_CPI', 'P90_WOZ_CPI', 'P100_WOZ_CPI')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(P10_WOZ_CPI + P20_WOZ_CPI + P30_WOZ_CPI + P40_WOZ_CPI
                                                       + P50_WOZ_CPI + P60_WOZ_CPI + P70_WOZ_CPI + P80_WOZ_CPI
                                                       + P90_WOZ_CPI + P100_WOZ_CPI))), .SDcols = cols ]

  #----------------#
  # 11. Tenure ----
  #----------------#   
        
    # Population: Private households (excluding institutional and student households)
    # HH_TENURE: 1: Home-owners, 2: Private rental, 3: Rental subsidy
        
    ## 11.1 Counts ----
      aux <- dcast(hh_tenure_df, VIERKANT100M ~ HH_TENURE, fun.aggregate = length)
      setnames(aux, c('1','2','3'), c('HH_HOMEOWNER','HH_PRIVRENTAL', 'HH_SUBRENTAL'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE, all.y = F)
      
    ## 11.2 Percentages ----  
      cols <- c('HH_HOMEOWNER','HH_PRIVRENTAL','HH_SUBRENTAL')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(HH_HOMEOWNER + HH_PRIVRENTAL + HH_SUBRENTAL))), .SDcols = cols ]

  #------------------------------------#
  # 12. Export table of attributes ----
  #------------------------------------#  
    
    save_file <- paste0("grid_",year,".rds")
    write_rds(grid_df, file = file.path(path_grid,save_file), compress = "gz")   
    rm(grid_df, ind_df, hh_income_df, hh_tenure_df, hh_df, obj_df)

    # Update progress bar 
    setTxtProgressBar(pb, year - start_year + 1)
}
close(pb)