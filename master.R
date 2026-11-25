#---------------------------------------------------------------------------------------------------------------------------------------#
# Name: 0_master
# Goal: Master script of all processes
# Date of creation: 22-01-2025

# R Version: R version 4.4.3 (2024-04-24 ucrt) -- "Trophy Case"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Based on code for "The Spatio-Temporal Evolution of Social Inequalities in
# Cities: A multidimensional, Multiscalar and Longitudinal Approach for Neighbourhood
# Classification" 
#---------------------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------------------------#
# 0. Setup ----
## 0.1 Packages ---- 
pacman::p_unload(pacman::p_loaded(), character.only = T)

## 0.2 Parameters ----
# Seed 
set.seed(1409)

# Grid 
grid_size <- 100                          # VIERKANT100M = 100; VIERKANT500M = 500

# Years (for the data processing)
start_year <- 1999
end_year <- 2024  # This can change of specific sub processes (CHECK subprocesses masters)
inc_year <- 2023

# Euclidean distance   
filter_area   <- FALSE                    # If TRUE: filter grids within specific area
city          <- paste0("vk",grid_size,"_",start_year,"_",inc_year)        # City/area under analysis. For file name
radius        <- 3500                     # Search radius for nearest neighbours (in meters)
export_chunks <- TRUE                     # If TRUE: export by chunks
num_chunks    <- 30                       # Number of chunks to export final database

# VIERKANT list file name
vierkantlistname = paste0("list_VIERKANT",grid_size, "M_", start_year,"_", inc_year, ".csv")

# Scales (in meters)
scale_values      <- c(100,200,400,800,1600,3200) # Scale to agregate variables (in meters)
clust_scales      <- c(0,1,2,3,4,5,6)             # Subset scales for cluster analysis (numeric value for the corresponding scale)

# Metropolitan agglomeration (cluster analysis)
met_aggl <- seq(1:22)
met_aggl_labs_save <- "all"   # If all are selected

# Urban regions 
urbreg_labels <- c("Groningen", "Leeuwarden", "Zwolle", "Enschede", "Apeldoorn", "Arnhem", "Nijmegen", "Amersfoort", "Utrecht", "Amsterdam",
                   "Haarlem", "Leiden", "The Hague", "Rotterdam", "Dordrecht", "Breda", "Tilburg", "'s-Hertogenbosch",
                   "Eindhoven", "Geleen/Sittard", "Heerlen", "Maastricht")

# Years (cluster analysis)
years    = c('2003','2007','2011','2015','2019','2023')           # Years for longitudinal classification 

# Cluster and sequence analysis parameters 
std_method <- "year_std" # Reference of z-score (year, city, city and year or full sample)
clust_labs <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4', 'Cluster 5', 'Cluster 6', 'Cluster 7', 'Cluster 8', 'Cluster 9')
clust_pen_labs <- c(
  "1" = 'Small-scale family elite enclave', 
  "2" = 'Large-scale middle-class aging area', 
  "3" = 'Large-scale middle-class family area',
  "4" = 'Large-scale young urban area',
  "5" = 'Large-scale family elite area',
  "6" = 'Large-scale middle-class European area',
  "7" = 'Large-scale working-class migrant area', 
  "8" = 'Middle-scale working-class migrant area',
  "9" = 'Large-scale aging elite area'
  )

scale_labs <- c("100x100m grid cells","100m radius","200m radius","400m radius","800m radius","1600m radius","3200m radius") 
scale_labs_save <- c("Grid","100m","200m","400m","800m","1600m","3200m") 

## 0.3 Paths  ----
# Directory 
path = "H:/Ignacio"
paper = "paper2"
setwd(path)

# Main folders 
microdata    = "G:"                                                 # Change according to the microdata location 
path_raw     = file.path(microdata)
path_data    = file.path(path, "data")
path_results = file.path(path, "output", paper)
path_code    = file.path(path, "code", paper)
path_log     = file.path(path, "log", paper)
path_utilities = file.path("K:","Utilities")

# Process subfolders 
path_obj              = file.path(path_data, "obj")
path_ind              = file.path(path_data, "ind")
path_hh               = file.path(path_data, "hh")
path_grid             = file.path(path_data, "grid")
path_nbh              = file.path(path_data, "nbh")

path_log_obj          = file.path(path_log, "obj")
path_log_ind          = file.path(path_log, "ind")
path_log_hh           = file.path(path_log, "hh")
path_log_grid         = file.path(path_log, "grid")
path_log_nbh          = file.path(path_log, "nbh")

subpaths = list(path_ind,path_obj,path_hh,path_grid,path_nbh,path_log_obj,path_log_ind,path_log_hh,path_log_grid,path_log_nbh)

# Check existance of subfolders to export the data
for (p in subpaths) {
  if (file.exists(p) == FALSE) {
    dir.create(p)
  }
}

## 0.4 Databases ----
  ### 0.4.1 Object ----
    # Spatial 
    vslvierkanttab        = file.path(path_raw,"BouwenWonen","VSLVIERKANTTAB","VRLVSLVIERKANTTABV2024031.sav")
    vslgwbtab             = file.path(path_raw,"BouwenWonen","VSLGWBTAB", "VSLGWB2024TAB03V1.sav")
    gin2018tab            = file.path(path_utilities,'HULPbestanden','GebiedeninNederland','GIN2018V1.sav')
    
    # Tenure
    eigendomwoztab        = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZTAB")
    
    objectwoningtab       = file.path(path_raw,"BouwenWonen", "OBJECTWONINGTAB")
    
    eigendomwozbagtab2012 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2012", "EIGENDOMWOZBAGTAB 2012V1.sav")
    eigendomwozbagtab2013 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2013", "EIGENDOMWOZBAGTAB 2013V1.sav")
    eigendomwozbagtab2014 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2014", "EIGENDOMWOZBAGTAB 2014V2.sav")
    eigendomwozbagtab2015 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2015", "EIGENDOMWOZBAG2015TABV1.sav")
    eigendomwozbagtab2016 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2016", "EIGENDOMWOZBAG2016TABV1.sav")
    eigendomwozbagtab2017 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2017", "EIGENDOMWOZBAG2017TABV2.sav")
    eigendomwozbagtab2018 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2018", "EIGENDOMWOZBAG2018TABV1.sav")
    eigendomwozbagtab2019 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2019", "EIGENDOMWOZBAG2019TABV1.sav")
    eigendomwozbagtab2020 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2020", "EIGENDOMWOZBAG2020TABV1.sav")
    eigendomwozbagtab2021 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2021", "EIGENDOMWOZBAG2021TABV1.sav")
    
    eigendomtab2012       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2012TABV6.sav")
    eigendomtab2013       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2013TABV6.sav")
    eigendomtab2014       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2014TABV6.sav")
    eigendomtab2015       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2015TABV6.sav")
    eigendomtab2016       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2016TABV6.sav")
    eigendomtab2017       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2017TABV5.sav")
    eigendomtab2018       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2018TABV3.sav")
    eigendomtab2019       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2019TABV3.sav")
    eigendomtab2020       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2020TABV3.sav")
    eigendomtab2021       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2021TABV2.sav")
    eigendomtab2022       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2022TABV2.sav")
    eigendomtab2023       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2023TABV1.sav")
    eigendomtab2024       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2024TABV1.sav")
    
    bagwoztab2019         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2019TABV2.sav")
    bagwoztab2020         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2020TABV2.sav")
    bagwoztab2021         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2021TABV2.sav")
    bagwoztab2022         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2022TABV2.sav")
    bagwoztab2023         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2023TABV2.sav")     
    bagwoztab2024         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2024TABV2.sav")     
    
  ### 0.4.2 Individuals ----
    # Spatial 
    gbaadressobject   = file.path(path_raw,"Bevolking","GBAADRESOBJECTBUS", "GBAADRESOBJECT2023BUSV1.sav")
    
    # Demo
    gbapersoontab  = file.path(path_raw,"Bevolking","GBAPERSOONTAB","2023", "GBAPERSOON2023TABV1.sav")
    landaktueelref = file.path(path_utilities,"Code_Listings","SSBreferentiebestanden","LANDAKTUEELREFV13.sav")
    gbahuishoudens = file.path(path_raw,"Bevolking","GBAHUISHOUDENSBUS", "GBAHUISHOUDENS2023BUSV1.sav")
    
    # Econ
    secmbus    = file.path(path_raw,"InkomenBestedingen","SECMBUS","SECMBUS2023V1.sav")
    werkndga   = file.path(path_raw,"InkomenBestedingen","SECMWERKNDGAMNDBEDRAGBUS","SECMWERKNDGAMNDBEDRAGBUSV20231.sav")
    beurs      = file.path(path_raw,"InkomenBestedingen","SECMBEURSMNDBEDRAGBUS","SECMBEURSMNDBEDRAGBUSV20231.sav")
    bijst      = file.path(path_raw,"InkomenBestedingen","SECMBIJSTMNDBEDRAGBUS","SECMBIJSTMNDBEDRAGBUSV20231.sav")
    ovact      = file.path(path_raw,"InkomenBestedingen","SECMOVACTMNDBEDRAGBUS","SECMOVACTMNDBEDRAGBUSV20231.sav")
    pensioen   = file.path(path_raw,"InkomenBestedingen","SECMPENSIOENMNDBEDRAGBUS","SECMPENSIOENMNDBEDRAGBUSV20231.sav")
    socvoorzov = file.path(path_raw,"InkomenBestedingen","SECMSOCVOORZOVMNDBEDRAGBUS","SECMSOCVOORZOVMNDBEDRAGBUSV20231.sav")
    werkl      = file.path(path_raw,"InkomenBestedingen","SECMWERKLMNDBEDRAGBUS","SECMWERKLMNDBEDRAGBUSV20231.sav")
    ziekteao   = file.path(path_raw,"InkomenBestedingen","SECMZIEKTEAOMNDBEDRAGBUS","SECMZIEKTEAOMNDBEDRAGBUSV20231.sav")
    zlf        = file.path(path_raw,"InkomenBestedingen","SECMZLFMNDBEDRAGBUS","SECMZLFMNDBEDRAGBUSV20231.sav")
    
    secmdata   = list(werkndga = werkndga, beurs = beurs, bijst = bijst, ovact = ovact, pensioen = pensioen, 
                      socvoorzov = socvoorzov, werkl = werkl, ziekteao = ziekteao, zlf = zlf)
    
    # Inflation
    cpitab = file.path(path,'raw',paper,'cpiEU_NL_1999_2023_2015100.csv') 
    
    # Household
    # INHATAB
    inhatab_2023 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2023TABV1.sav")
    inhatab_2022 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2022TABV2.sav")
    inhatab_2021 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2021TABV2.sav")
    inhatab_2020 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2020TABV2.sav")
    inhatab_2019 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2019TABV2.sav")
    inhatab_2018 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2018TABV2.sav")
    inhatab_2017 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2017TABV2.sav")
    inhatab_2016 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2016TABV2.sav")
    inhatab_2015 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2015TABV1.sav")
    inhatab_2014 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2014TABV1.sav")
    inhatab_2013 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2013TABV2.sav")
    inhatab_2012 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2012TABV2.sav")
    inhatab_2011 = file.path(path_raw,"InkomenBestedingen","INHATAB", "INHA2011TABV2.sav")
    
    # IHI
    ihi_2010 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2010", "HUISHBVRINK2010TABV3.sav")
    ihi_2009 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2009", "HUISHBVRINK2009TABV3.sav")
    ihi_2008 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2008", "HUISHBVRINK2008TABV3.sav")
    ihi_2007 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2007", "HUISHBVRINK2007TABV3.sav")
    ihi_2006 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2006", "HUISHBVRINK2006TABV3.sav")
    ihi_2005 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2005", "HUISHBVRINK2005TABV3.sav")
    ihi_2004 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2004", "HUISHBVRINK2004TABV3.sav")
    ihi_2003 = file.path(path_raw,"InkomenBestedingen","INTEGRAAL HUISHOUDENS INKOMEN", "2003", "HUISHBVRINK2003TABV3.sav")
    
## 0.5 Processes ----
# Functions 
source(file.path(path_code, "functions.R"), echo = F)        
 
# 1. Obj
source(file.path(path_code, "1_obj", "1_obj_spatial.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code, "1_obj", "2_obj_tenure_year.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path(path_code, "1_obj", "3_obj_year.R"), echo = TRUE, max.deparse.length = 10000)
    
# # 2. Ind 
source(file.path(path_code,"2_ind", "1_ind_spatial_year.R"), echo = TRUE, max.deparse.length = 10000)     
source(file.path(path_code,"2_ind", "2_ind_demo_year.R"), echo = TRUE, max.deparse.length = 10000)     
source(file.path(path_code,"2_ind", "3_ind_econ_year.R"), echo = TRUE, max.deparse.length = 10000)     
source(file.path(path_code,"2_ind", "4_ind_year.R"), echo = TRUE, max.deparse.length = 10000)     
    
# # 3. Household 
source(file.path(path_code,"3_hh", "1_hh_tenure_year.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"3_hh", "2_hh_income_year.R"), echo = TRUE, max.deparse.length = 10000)    

# # 4. Grid 
source(file.path(path_code,"4_grid", "1_grid_year.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"4_grid", "2_grid_export_list_ID.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"4_grid", "3_grid_distances.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"4_grid", "4_grid_var_scales.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"4_grid", "5_grid_var_scales_year.R"), echo = TRUE, max.deparse.length = 10000)
    
# 5. Neighbourhood class. & sequence analysis 
source(file.path(path_code,"5_nbh", "1_nbh_data_for_cluster.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path(path_code,"5_nbh", "2_1_nbh_cluster_model_selection.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path(path_code,"5_nbh", "2_2_nbh_cluster_model_selection.R"), echo = TRUE, max.deparse.length = 10000)     
source(file.path(path_code,"5_nbh", "2_3_nbh_cluster_model_selection.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"5_nbh", "3_nbh_cluster_final_data.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path(path_code,"5_nbh", "4_nbh_cluster_final_plots.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path(path_code,"5_nbh", "5_nbh_seq_analysis.R"), echo = TRUE, max.deparse.length = 10000)     
source(file.path(path_code,"5_nbh", "6_nbh_seq_analysis_rep_plots.R"), echo = TRUE, max.deparse.length = 10000)
source(file.path(path_code,"5_nbh", "7_nbh_seq_analysis_barplots.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path(path_code,"5_nbh", "8_nbh_seq_analysis_maps.R"), echo = TRUE, max.deparse.length = 10000)  
    