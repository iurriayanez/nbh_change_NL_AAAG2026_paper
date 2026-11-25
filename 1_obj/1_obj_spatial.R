#------------------------------------------------------------------------------#
# Name: 1_obj_spatial
# Goal: Create database with address objects spatial info: address object ID, grid ID, admin. spatial unit codes
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  VSLVIERKANTTAB ; VSLGWBTAB ; GIN2018  
# Output databases: data/obj/obj_spatial.csv
#------------------------------------------------------------------------------#

# Packages
pacman::p_unload(pacman::p_loaded(), character.only = T)
packages = c('haven','dplyr','data.table','readr')
# Load packages 
invisible(lapply(packages, library, character.only = TRUE))

#------------------------------------#      
# 1. Grid info (VSLVIERKANTTAB) ---- 
#------------------------------------#   
      
  ## 1.1 Import grid info ----
    grid = read_spss(vslvierkanttab)

    # Data.table format 
      grid = setDT(grid)

    # Remove labelled values of variables 
      cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')
      grid[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]

  ## 2.2 ID ----
    # Objects (address)
      grid[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
      
      setnames(grid, c("VRLVIERKANT100M", "VRLVIERKANT500M"),
               c("VIERKANT100M", "VIERKANT500M"))
      
  ## 2.3 Check data ----
    summary(grid)                                                                  # General info 
    
    any(duplicated(grid, by= 'OBJ_ID'))                                            # No duplicates. Key variables.
    
    table(grid$SOORTOBJECTNUMMER)
    # B: From the Basic Registration of Addresseses (Basisregistratie Adressen en Gebouwen). Objects from 1-1-2012. 
    # H: Historical origin. Objects from before 1-1-2012. 
    # D: Origin from the municipal population registers. Not B nor H. 
    
    grid[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
    
#------------------------------------------------------# 
# 2. Municipal and neighbourhood codes (VSLGWBTAB) ----
#------------------------------------------------------# 
    
  ## 2.1 Import administrative location information with municipality codes (vslgwbtab). ----
    add_admin = read_spss(vslgwbtab) 

    # Data.table format 
      add_admin = setDT(add_admin)

    # Remove labelled values of variables 
      cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
    
    # As integers 
      cols = names(add_admin)[grep("^gem", names(add_admin))]
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
      add_admin[, (cols) := lapply(.SD, function(x) (as.integer(gsub("[^0-9]", "", x)))), .SDcols = cols]
      
      cols = names(add_admin)[grep("^wc", names(add_admin))]
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
      add_admin[, (cols) := lapply(.SD, function(x) (as.integer(gsub("[^0-9]", "", x)))), .SDcols = cols]
      
      cols = names(add_admin)[grep("^bc", names(add_admin))]
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
      add_admin[, (cols) := lapply(.SD, function(x) (as.integer(gsub("[^0-9]", "", x)))), .SDcols = cols]
      
  ## 2.2 ID ----
    # Objects (address)
      add_admin[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
      add_admin[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]

#------------------------------------------------------------------------------------#      
# 3. Add Metropolitan agglomeration codes to Municipal codes (GIN2018 database) ----
#------------------------------------------------------------------------------------#   
      
  # We match the 2018 agglomeration codes with the 2018 gemeente codes

  ## 3.1 Import GIN database ----
    gin2018 = read_spss(gin2018tab, col_select = c('gemeente', 'stadsgewest','grstedagg'))
    setDT(gin2018)

  ## 3.2 Change type of variable for merge afterwards ----
    gin2018[, (colnames(gin2018)) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = colnames(gin2018)]
    gin2018[, (colnames(gin2018)) := lapply(.SD, function(x) (as.integer(x))), .SDcols = colnames(gin2018)]

  ## 3.3 Merge ----
    add_admin = merge(add_admin,gin2018, by.x = 'gem2018', by.y = 'gemeente', all.x = TRUE)

  ## 3.4 Check ----
    # We check with gem[inc_year] because it is the last year available with income information. 
    # We assume that the agglomerations in 2018 remained the same from 2018 onwards.   
    gemvar <- paste0('gem',inc_year)
    tab = unique(add_admin[,c('gem2018',gemvar,'stadsgewest'), with = F], by = c(gemvar,'stadsgewest'))

    # Some objects have a missing value in gem2018 but are from Rotterdam in 2022. We input the agglomeration of Rotterdam (14) to these objects
    add_admin[get(gemvar) == 599 & is.na(grstedagg), grstedagg := 14]
    add_admin[get(gemvar) == 599 & is.na(stadsgewest), stadsgewest:= 14]
    
    # Some objects (less than 1%) are in one municipality in 2018 (corresponding to one urban region) but in other in 2022 (in another urban region). 
    # We follow the gem2022 code to define the urban region code in these cases.  
    # In cases where the gem2022 code is new, we keep the 2018 urban region
    tab[, rep := .N, keyby = gemvar]
    tab[rep > 1, ]
    
    add_admin[get(gemvar) == 394 & gem2018 == 393 & stadsgewest == 11, stadsgewest := 10]   
    add_admin[get(gemvar) == 439 & gem2018 == 370 & stadsgewest == 0, stadsgewest := 10]   
    add_admin[get(gemvar) == 824 & gem2018 == 788 & stadsgewest == 0, stadsgewest := 17]   
    add_admin[get(gemvar) == 855 & gem2018 == 788 & stadsgewest == 0, stadsgewest := 17]   
    add_admin[get(gemvar) == 865 & gem2018 == 788 & stadsgewest == 0, stadsgewest := 18]   
    add_admin[get(gemvar) == 1621 & gem2018 == 599 & stadsgewest == 14, stadsgewest := 0]  
    add_admin[get(gemvar) == 1945 & gem2018 == 268 & stadsgewest == 7, stadsgewest := 0] 
    add_admin[get(gemvar) == 1949 & gem2018 == 80 & stadsgewest == 2, stadsgewest := 0]   
    
    tab = unique(add_admin[,c('gem2018',gemvar,'stadsgewest'), with = F], by = c(gemvar,'stadsgewest'))
    tab[, rep := .N, keyby = gemvar]
    tab[rep > 1, ]
    
    # Same procedure for the metropolitan agglomeration
    tab = unique(add_admin[,c('gem2018',gemvar,'grstedagg'), with = F], by = c(gemvar,'grstedagg'))
    tab[, rep := .N, keyby = gemvar]
    tab[rep > 1, ]
    add_admin[get(gemvar) == 14 & gem2018 == 9 & grstedagg == 0, grstedagg := 1]
    add_admin[get(gemvar) == 855 & gem2018 == 788 & grstedagg == 0, grstedagg := 17]   
    add_admin[get(gemvar) == 865 & gem2018 == 788 & grstedagg == 0, grstedagg := 18] 
    add_admin[get(gemvar) == 1621 & gem2018 == 599 & grstedagg == 14, grstedagg := 0] 
    add_admin[get(gemvar) == 1945 & gem2018 == 268 & grstedagg == 7, grstedagg := 0] 
    add_admin[get(gemvar) == 1949 & gem2018 == 80 & grstedagg == 2, grstedagg := 0]
    
    tab = unique(add_admin[,c('gem2018',gemvar,'grstedagg'), with = F], by = c(gemvar,'grstedagg'))
    tab[, rep := .N, keyby = gemvar]
    tab[rep > 1, ]
    rm(tab)
    
#-------------------------#      
# 4. Final database ----
#-------------------------#  
  
  # 4.1 Merge databases ----
    col_names = c(paste0("gem",inc_year),paste0("wc",inc_year),paste0("bc",inc_year), "grstedagg" , "stadsgewest")
    df = merge(grid, add_admin[,c('OBJ_ID',col_names), with = FALSE], by='OBJ_ID')
  
  ## 4.2 Relevant variables ----
    cols <- c(# Object ID
      'OBJ_ID',
      # GRID ID
      'VIERKANT100M','VIERKANT500M',
      # Admin. spatial units
      col_names
    )
    
    df <- df[, ..cols, with = FALSE]
    
    setnames(df, c('grstedagg', 'stadsgewest'),
             c('METAGGL', 'URBREG'))
    
  ## 4.3 Export ----
    fwrite(df, file = file.path(path_obj,"obj_spatial.csv"))
    rm(df)
    rm(grid, gin2018, add_admin)   
    