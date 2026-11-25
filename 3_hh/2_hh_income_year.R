#------------------------------------------------------------------------------#
# Name: 2_hh_inc_year.R
# Goal: Create yearly databases (1999-2023) with household income 
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/ind/ind_[year].rds
# Output databases: data/hh/hh_inc_[year].rds
#------------------------------------------------------------------------------#

# Specific parameters
end_year <- 2023

pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('ggplot2','readr','haven','dplyr','data.table')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

# Loop

for (year in start_year:inc_year) { 
  
  # 1. Import individual database (With individual income and hh id) ----
  ind_rds <- paste0("ind_",year,".rds")
  ind_df  <- read_rds(file.path(path_ind,ind_rds))
  setDT(ind_df)
  
  cols <- c(
    'IND_ID',
    'VIERKANT100M', 
    'VIERKANT500M',
    'METAGGL',
    'URBREG',
    'HH_ID',
    'HH_REFPER',
    'STUD',
    'WORK_AGE',
    'GROSS_ANNUAL_INCOME',
    'AGE'
  )
  
  ind_df <- ind_df[, ..cols, with = F]
  
  # Only private households
  # Private households: Must have one reference person and that reference person must not be a student
  ind_df[, aux := sum(HH_REFPER == 1 & (STUD == 0 | is.na(STUD)), na.rm = TRUE), by = HH_ID]
  ind_df[, PRIV_HH := as.integer(aux == 1)]
  ind_df[, aux := NULL]
  ind_df <- ind_df[PRIV_HH == 1,]
  
  # 2. Household income ---- 
  # Sum of individual income from work, social benefits and pensions of people who are not students
  aux = ind_df[!is.na(GROSS_ANNUAL_INCOME) & !is.na(HH_REFPER),
           .(HH_GROSS_ANNUAL_INCOME = sum(GROSS_ANNUAL_INCOME)), by = HH_ID]
  ind_df <- merge(ind_df, aux, by = 'HH_ID', all.x = TRUE)
  rm(aux)
  
  # Equivalised household income (CBS equivalence factors)
  # Number of adults and children 
  ind_df[, ADULTS := sum(AGE >= 18, na.rm = TRUE), by = 'HH_ID']
  ind_df[, KIDS   := sum(AGE <  18, na.rm = TRUE), by = 'HH_ID']
  
  # Equivalence factors
  if (year > 2017) {
    ind_df[, EQ_FACTOR := fcase(
      ADULTS == 1 & KIDS == 0 , 1,
      ADULTS == 2 & KIDS == 0 , 1.4,
      ADULTS == 3 & KIDS == 0 , 1.78,
      ADULTS == 4 & KIDS == 0 , 2.02,
      ADULTS == 1 & KIDS == 1 , 1.32,
      ADULTS == 2 & KIDS == 1 , 1.69,
      ADULTS == 3 & KIDS == 1 , 2.00,
      ADULTS == 4 & KIDS == 1 , 2.19,
      ADULTS == 1 & KIDS == 2 , 1.52,
      ADULTS == 2 & KIDS == 2 , 1.91,
      ADULTS == 3 & KIDS == 2 , 2.16,
      ADULTS == 4 & KIDS == 2 , 2.37,
      ADULTS == 1 & KIDS == 3 , 1.73,
      ADULTS == 2 & KIDS == 3 , 2.09,
      ADULTS == 3 & KIDS == 3 , 2.32,
      ADULTS == 4 & KIDS == 3 , 2.53,
      ADULTS == 1 & KIDS == 4 , 1.93,
      ADULTS == 2 & KIDS == 4 , 2.28,
      ADULTS == 3 & KIDS == 4 , 2.49,
      ADULTS == 4 & KIDS == 4 , 2.68,
      ADULTS >  4  | KIDS > 4 , (ADULTS + KIDS*0.8)^0.5
    )]
  } else {
    ind_df[, EQ_FACTOR := fcase(
      ADULTS == 1 & KIDS == 0 , 1,
      ADULTS == 2 & KIDS == 0 , 1.37,
      ADULTS == 3 & KIDS == 0 , 1.73,
      ADULTS == 4 & KIDS == 0 , 2.00,
      ADULTS == 1 & KIDS == 1 , 1.33,
      ADULTS == 2 & KIDS == 1 , 1.67,
      ADULTS == 3 & KIDS == 1 , 1.95,
      ADULTS == 4 & KIDS == 1 , 2.19,
      ADULTS == 1 & KIDS == 2 , 1.51,
      ADULTS == 2 & KIDS == 2 , 1.88,
      ADULTS == 3 & KIDS == 2 , 2.14,
      ADULTS == 4 & KIDS == 2 , 2.37,
      ADULTS == 1 & KIDS == 3 , 1.76,
      ADULTS == 2 & KIDS == 3 , 2.06,
      ADULTS == 3 & KIDS == 3 , 2.32,
      ADULTS == 4 & KIDS == 3 , 2.53,
      ADULTS == 1 & KIDS == 4 , 1.95,
      ADULTS == 2 & KIDS == 4 , 2.28,
      ADULTS == 3 & KIDS == 4 , 2.49,
      ADULTS == 4 & KIDS == 4 , 2.68,
      ADULTS >  4  | KIDS > 4 , (ADULTS + KIDS*0.8)^0.5
    )]
  }
  
  # Equivalised income 
  ind_df[, HH_EQ_GROSS_ANNUAL_INCOME := (HH_GROSS_ANNUAL_INCOME / EQ_FACTOR)]
  
  # There are some private households with EQ_FACTOR missing, but it is because these are households of underage individuals (not considering them).

  
  # 3. Household with low and high income ---- 
  
  # Sample selection for household national distribution: private households - reference person must not be a student (HH_REFPER == 1 & STUD != 1)
  
  # Income quintiles: 
  ind_df[, HH_INCOME_QUANT := cut(ind_df[, HH_EQ_GROSS_ANNUAL_INCOME], 
                                          quantile(ind_df[, HH_EQ_GROSS_ANNUAL_INCOME], probs = seq(0, 1, 0.25), na.rm = TRUE), 
                                          labels = FALSE, include.lowest = TRUE)]
  
  # Low income: Lowest 25% of the distribution
  ind_df[,HH_LOWINC := as.integer(HH_INCOME_QUANT == 1)]
  
  # High income: Income above the 75th percentile of the income distribution      
  ind_df[,HH_HIGHINC := as.integer(HH_INCOME_QUANT == 4)]
  
  # Middle income: Income above the between 25 and 75 percentile of the income distribution      
  ind_df[,HH_MIDINC := as.integer(HH_HIGHINC == 0 & HH_LOWINC == 0)]
  
  # 4. One observation per household ---- 
  df <- ind_df[HH_REFPER == 1,]
  cols <- c(
    'HH_ID',
    'VIERKANT100M', 
    'VIERKANT500M',
    'METAGGL',
    'URBREG',
    'HH_GROSS_ANNUAL_INCOME', 
    'KIDS',
    'ADULTS',
    'EQ_FACTOR',
    'HH_EQ_GROSS_ANNUAL_INCOME',
    'HH_INCOME_QUANT',
    'HH_LOWINC',
    'HH_HIGHINC',
    'HH_MIDINC'
  )
  df <- df[,..cols, with = F]
  
  # 5. Export ---- 
  setnames(df, toupper(names(df)))  
  save_file <- paste0("hh_inc_",year,".rds")
  write_rds(df, file = file.path(path_hh,save_file), compress = "gz")  
  rm(df, ind_df)
}



