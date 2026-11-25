# Analysis code for "Beyond the Neighborhood: Multidimensional and Multiscale Sociospatial Change in Dutch Cities"

This repository contains the code and data processing scripts necessary to reproduce the analysis in [Beyond the Neighborhood: Multidimensional and Multiscale Sociospatial Change in Dutch Cities](https://doi.org/10.1080/24694452.2026.2756877) by [Ignacio Urria](https://orcid.org/0009-0005-0912-7627), [Ana Petrović](https://orcid.org/0000-0002-6337-964X), [Maarten van Ham](https://orcid.org/0000-0002-2106-0702) and [David Manley](https://orcid.org/0000-0003-1864-5141). 

Please refer to the original paper for a detailed description of the methodology and findings.

> **Note: This repository is based on the code underlying the paper "The Spatio-Temporal Evolution of Social Inequalities in Cities: A Multidimensional, Multiscalar and Longitudinal Approach for Neighbourhood Classification"** [![DOI](https://data.4tu.nl/v3/datasets/f0b25cba-abbb-42c6-823a-422aad8c0721/doi-badge.svg)](https://doi.org/10.4121/f0b25cba-abbb-42c6-823a-422aad8c0721)


## Table of Contents

- [1. Overview](#1-overview)
- [2. Repository Structure](#2-repository-structure)
- [3. Usage](#3-usage)
- [4. Data](#4-data)
- [5. Reproducibility](#5-reproducibility)
- [6. Dependencies](#6-dependencies)
- [7. License](#7-license)
- [8. Contact](#8-contact)
- [9. Citation](#9-citation)
- [10. Acknowledgements](#10-acknowledgements)

---

## 1. Overview


> **Note: The authors do not have permission to share data. However, under certain conditions, these data are accessible for statistical and scientific research. For further information: [microdata@cbs.nl](mailto:microdata@cbs.nl).<br><br>
The code is shared to facilitate reproducibility and transparency, enabling other researchers to replicate, adapt, or extend the analysis with the same or different data sources.**

This repository provides the R scripts and functions used to create and analyse the multiscale trajectories of sociospatial context change in the 22 urban regions of the Netherlands.

The methodological approach consists of four stages:

1. **Creation of multiscale sociospatial signatures:**

    We delineated bespoke neighbourhoods of increasing radii - doubling up from 100m to 3200m - around all the 100 x 100 m grid cells of the urban regions in the Netherlands. We employed these bespoke neighbourhoods to measure the multiscale sociospatial context of each cell using socioeconomic, demographic and housing characteristics.

2. **Classification of sociospatial signatures:**

    Using a Mini-Batch K-Means algorithm, we classified the multiscale sociospatial signatures into 9 clusters that grouped the cells according to their multidimensional and multiscale composition.

3. **Classification of sequences of change across sociospatial signature clusters**

    Using sequence analysis and clustering techniques, we created a typology of 46 pathways of change based on the transition between sociospatial signature clusters that each cell experienced between 2003 and 2023.

4. **Within- and between-city analysis:**

    We mapped and analysed the distribution and prevalence of the multidimensional and multiscale pathways of change in the 22 urban regions of the Netherlands. 


---

## 2. Repository Structure

The repository is structured as follows:
- **master.R**: Main script to run the full analysis pipeline.
- **functions.R**: Common functions used throughout the analysis.
- **1_obj/**: Scripts for processing data on residential objects (i.e., residential units).
    - **1_obj_spatial.R**: Create a database (1999-2023) with the spatial information of residential objects: address object ID, grid ID, and administrative spatial unit codes.
    - **2_obj_tenure_year.R**: Create yearly databases (1999-2023) with tenure information of residential objects.
    - **3_obj_year.R**: Create yearly databases (1999-2023) merging the spatial, tenure and housing value information of residential objects calculated in the previous 2 scripts.
- **2_ind/**: Scripts for processing individual-level data.
    - **1_ind_spatial_year.R**: Create yearly databases (1999-2023) with the individual's residential object information: address object ID, grid ID, administrative spatial unit codes, and tenure type.
    - **2_ind_demo_year.R**: Create yearly databases (1999-2023) with the individual's demographic information: gender, age and migration background.
    - **3_ind_econ_year.R**: Create yearly databases (1999-2023) with the individual's socioeconomic status and individual income (which is not used in this paper).
    - **4_ind_year.R**: Create yearly databases (1999-2023) merging the spatial, housing, demographic and socioeconomic information of individuals calculated in the previous 3 scripts.
- **3_hh/**: Scripts for identifying the type of tenure of the household (homeowners, private renters or social renters) and calculate household income by aggregating individual incomes (calculated in 3_ind_econ_year).
    - **1_hh_tenure_year.R**: Create yearly databases (1999-2023) with the type of tenure of the household.
    - **2_hh_income_year.R**: Create yearly databases (1999-2023) with the equivalised gross household income and quintiles of distribution.
- **4_grid/**: Scripts for aggregating individual and residential data at the 100x100m grid cell level and create the bespoke neighbourhoods.
    - **1_grid_year.R**: Create yearly databases (1999-2023) with the aggregated information of 100x100m grid cells: demographic, socioeconomic and housing composition.
    - **2_grid_export_list_ID.R**: Create list with all the grid cells with available information between 1999 - 2022.
    - **3_grid_distances.R**: Calculate euclidean distances between the grid cells obtained in the previous script.
    - **4_grid_var_scales.R**: Calculate the variables across scales for all grid cells by chunks.
    - **5_grid_var_scales_year.R**: Consolidate the chunks to create the final data base with grid cell information across all scales.
    - **6_select_grid_var_scales_met_aggl.R**: Select the final universe of grid cells in the corresponding study area from the database created in the previous script.
- **5_nbh/**: Scripts for clustering, and creation of outputs (figures, maps and tables).
    - **1_nbh_data_for_cluster.R**: Create dataset with relevant variables for clustering the sociospatial signatures.
    - **2_1_nbh_cluster_model_selection.R**: Hypertuning and comparison of quality metrics of different clustering methods (K-Means, PCA + K-Means and SOM + K-Means) with multiple clustering solutions. 
    - **2_2_nbh_cluster_model_selection_ave_distprof_plots.R**: Comparison of the optimal solutions of different clustering methods (K-Means, PCA + K-Means and SOM + K-Means) using the plots of the average z-score values by variable across scales.
    - **2_3_nbh_cluster_model_selection_cluster_maps.R**: Comparison of the optimal solutions of different clustering methods (K-Means, PCA + K-Means and SOM + K-Means) using maps.
    - **3_nbh_cluster_final_data.R**: Create final cluster classification of sociospatial signatures using the selected method (Mini Batch K-Means) and the optimal number of clusters (k=9).
    - **3_nbh_cluster_final_data.R**: Create final cluster classification of sociospatial signatures using the selected method (Mini Batch K-Means) and the optimal number of clusters (k=9).
    - **4_nbh_cluster_final_plots.R**: Create final plots of cluster classification (Clustergram and quality metrics) with average distance profiles. 
    - **5_nbh_seq_analysis.R**: Hypertuning of sequence analysis and sequence clustering. Creation of final sequence clusters (46 clusters) dataset. 
    - **6_nbh_seq_analysis_rep_plots.R**: Create plots with representative sequences by sequence cluster.
    - **7_nbh_seq_analysis_barplots.R**: Create plots that compare the distribution of sequence clusters across urban regions.
    - **8_nbh_seq_analysis_maps.R**: Create maps with the distribution of sequence clusters across urban regions.
    
---

## 3. Usage

All scripts should be run in the order specified in the `master.R` file. Each script is designed to be self-contained, but they might depend on the outputs of previous scripts.

To run the entire analysis, execute the `master.R` script in R or RStudio. Ensure that you have set the correct working directory.

---

## 4. Data 

### 4.1 Data sources:
Microdata from Statistics Netherlands (CBS) between 1999 and 2023. The final dataset is restricted to the 22 urban regions of the Netherlands between 2003 and 2023, but the code allows to use the whole country or a specific area (municipality, urban region, province, etc) in the Netherlands between 1999 and 2023. 

### 4.2 Data structure:

For every script, the input files sourced from CBS microdata and the corresponding output files generated by the script are listed below.

- **1_obj/**:
    - **1_obj_spatial_year.R**: 
        - **Input**: 
            1. [VRLVSLVIERKANTTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/vslvierkanttab-coordinaten-vierkanten-verblijfsobject): Link between the residential object ID and grid cells (100x100m and 500x500m) ID
            2. [VSLGWBTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/vslgwbtab-gwbcode-van-verblijfsobject-niet-gecoord--): All objects from the Basisregistratie Adressen en Gebouwen (BAG) and their spatial unit codes (buurt, wijk and gemeente).
            3. GIN2018: The GIN2018 file contains the code of the corresponding metropolitan agglomeration of the municipality. 
        
        - **Output**:
            1. `data/obj/obj_spatial.csv`: Database with the spatial information of residential objects across years: address object ID, grid ID, and administrative spatial unit codes.

    - **2_obj_tenure_year.R**:
        - **Input**:   
            1. [OBJECTWONINGTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/objectwoningtab-bewoonde-woningen-naar-eigendom-en-woz-waarde): Tenure information of residential objects (1999-2005).
            2. [EIGENDOMWOZTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/eigendomwoztab-woonruimten-naar-eigendom-en-woz-waarde--vervanger-van-woningtab-woonruimteeigendomtab-en-opgevolgd-door-eigendomwozbagtab--): Tenure information of residential objects (2006 - 2011).
            3. [EIGENDOMWOZBAGTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/eigendomwozbagtab-woz-waarde-en-eigendom): Definition of type of units (residential or not) of residential objects (2012 - 2018).
            4. [BAGWOZTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/bagwoztab-woz-waarde-van-woningen): Definition of type of units (residential or not) of residential objects (2019 - 2024).
            5. [EIGENDOMTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/eigendomtab-eigendom-en-eigenaar-van-woningen): Tenure information of residential objects (2012 - 2024).
        - **Output**:
            1. `data/obj/obj_tenure_[year].csv`: Yearly database with tenure information of residential objects.
    - **3_obj_year.R**:
        - **Input**: 
            1. `data/obj/obj_spatial_[year].csv`
            2. `data/obj/obj_tenure_[year].csv`
        - **Output**:
            1. `data/obj/obj_[year].rds`: Yearly database merging the spatial, tenure and housing value information of residential objects calculated in the previous 2 scripts.
- **2_ind/**:
    - **1_ind_spatial_year.R**:
        - **Input**: 
            1. `data/obj/obj_[year].csv`
            2. [GBAADRESOBJECTBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbaadresobjectbus-gba-adreskenmerken-van-personen): Link between the individual and the residential object ID.
        - **Output**:
            1. `ind_spatial_[year].csv`: Yearly database with the individual's residential object information: address object ID, grid ID, administrative spatial unit codes, tenure and WOZ value.
    - **2_ind_demo_year.R**:
        - **Input**: 
            1. [GBAPERSOONTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbapersoontab-persoonskenmerken-van-personen-in-de-brp): Demographic information of individuals.
            2. LANDAKTUEELREF: Listing with country codes.
            3. [GBAHUISHOUDENSBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbahuishoudensbus-huishoudenskenmerken): Household composition of individuals.
        - **Output**:
            1. `ind_demo_[year].csv`: Yearly database with the individual's demographic information.
    - **3_ind_econ_year.R**:
        - **Input**:
            1. [SECMBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmbus-personen-sociaaleconomische-categorie): Socioeconomic status of individuals. 
            2. [SECMWERKNDGAMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmwerkndgamndbedragbus-personen-met-loon-maand): Monthly income of individuals from employment. 
            3. [SECMZLFMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmzlfmndbedragbus-winstbedrag-van-zelfstandigen): Monthly income of individuals from self-employment.
            4. [SECMOVACTMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmovactmndbedragbus-personen-overige-arbeid): Monthly income of individuals from other work-related activities.
            5. [SECMBEURSMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmbeursmndbedragbus-personen-met-studiebeurs): Monthly income of individuals from student benefits.
            6. [SECMBIJSTMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmbijstmndbedragbus-personen-bijstand-verslagmaand): Monthly income of individuals from social assistance (_Bijstand_ in Dutch).
            7. [SECMSOCVOORZOVMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmsocvoorzovmndbedragbus-soc-econ-cat-ov-soc-voorzien-): Monthly income of individuals from other social assistance benefits.
            8. [SECMPENSIOENMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmpensioenmndbedragbus-personen-met-pensioenbedragen): Monthly income of individuals from pensions.
            9. [SECMWERKLMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmwerklmndbedragbus-personen-werkloosheidsuitkering): Monthly income of individuals from unemployment benefits.
            10. [SECMZIEKTEAOMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmziekteaomndbedragbus-personen-uitkering-bij-ziekte): Monthly income of individuals from sickness benefits.
        - **Output**:
            1. `data/ind/ind_sec_[year].csv`: Yearly database with the individual's socioeconomic status.
            2. `data/ind/ind_inc_[year].csv`: Yearly database with the individual's annual income.
    - **4_ind_year.R**:
        - **Input**:   
            1. `data/ind/ind_spatial_[year].csv`
            2. `data/ind/ind_demo_[year].csv`
            3. `data/ind/ind_sec_[year].csv`
            4. `data/ind/ind_inc_[year].csv`
            5. cpiEU_NL_1999_2023_2015100.csv [(available online here)](https://opendata.cbs.nl/#/CBS/en/dataset/83131ENG/table?ts=1751906408417): Consumer Price Index (CPI) data for the Netherlands between 1999 and 2023 (2015 = 100), used to adjust income values.
        - **Output**:
            1. `data/ind/ind_[year].rds`: Yearly database merging the spatial, demographic and socioeconomic information of individuals calculated in the previous 3 scripts.
- **3_hh/**:
    - **1_hh_tenure_year.R**:
        - **Input**:
            1. [IHI](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/ihi-integraal-huishoudens-inkomen): Yearly tenure type between 2001 and 2010. 
            2. [INHATAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/inhatab-inkomen-van-huishoudens): Yearly tenure type between 2011 and 2023.
        - **Output**:
            1. `data/hh/hh_tenure_[year].csv`: Yearly dataset with tenure type of household.
    - **2_hh_income_year.R**:
        - **Input**:     
            1. `data/ind/ind_[year].rds`: Information with individual income and household ID by individual.
        - **Output**:
            1. `data/hh/hh_inc_[year].rds`: Yearly dataset with household income (one observation per household).
- **4_grid/**:
    - **1_grid_year.R**:
        - **Input**: 
            1. `data/ind/ind_[year].rds`: Yearly database merging the spatial, demographic and socioeconomic information of individuals.
            2. `data/hh/hh_tenure_[year].rds`: Yearly database with tenure type by household.
            3. `data/hh/hh_inc_[year].rds`: Yearly database with household income.
        - **Output**:
            1. `data/grid/grid_[year].rds`: Yearly database with the aggregated information of 100x100m grid cells: demographic, socioeconomic and housing composition.
    - **2_grid_export_list_ID.R**:
        - **Input**:
            1. `data/grid/grid_[year].rds`
        - **Output**:
            1. `data/grid/list_VIERKANT100M_1999_2023.csv`: List with all the grid cells with available information between 1999 - 2022.
    - **3_grid_distances.R**:
        - **Input**:
            1. `data/grid/list_VIERKANT100M_1999_2023.csv`
        - **Output**:
            1. `data/grid/euclidean_dist_[max_radius]m_[city]_part_[i].csv`: Euclidean distances between the grid cells obtained in the previous script with a max search radius of `max_radius`, for the study area `city` (can be the whole country) and the chunk `i`.
    - **4_grid_var_scales.R**:
        - **Input**:
            1. `data/grid/grid_[year].rds`
            2. `data/grid/euclidean_dist_[max_radius]m_[city]_part_[i].csv`
        - **Output**:
            1. `data/grid/grid_var_scales_[part].csv`: Variables calculated across scales for all grid cells by chunks, where `part` is the chunk number.
    - **5_grid_var_scales_year.R**:
        - **Input**:
            1. `data/grid/grid_var_scales_[part].csv`
        - **Output**:
            1. `data/grid/grid_var_scales_[year].rds`: Consolidated database with grid cell information across all scales.
    - **6_select_grid_var_scales_met_aggl.R**:
        - **Input**:
            1. `data/grid/grid_var_scales_[year].rds`
        - **Output**:
            1. `data/grid/grid_var_scales_metaggl_all_[year].rds`: Final universe of grid cells in the corresponding study area (all for all urban regions) from the database created in the previous script.
- **5_nbh/**:
    - **1_nbh_data_for_cluster.R**:
        - **Input**:
            1. `data/grid/grid_var_scales_metaggl_all_[year].rds`: Grid-cell data across spatial scales for the selected years and metropolitan agglomeration.
        - **Output**:
            1. `data/nbh/data_for_clustering_metaggl_all.rds`: Filtered and complete dataset used for sociospatial signature clustering.
    - **2_1_nbh_cluster_model_selection.R**:
        - **Input**:
            1. `data/nbh/data_for_clustering_metaggl_all.rds`
        - **Output**:
            1. `data/nbh/data_scaled_year_std_for_clustering_metaggl_all.rds`: Standardised clustering data.
            2. `data/nbh/data_pca_year_std_for_clustering_metaggl_all.rds`: PCA-transformed clustering data.
            3. `data/nbh/data_clust_assign_year_std_orig_pca_mbkmeans_k_7_10_metaggl_all.rds`: PCA and mini-batch k-means assignments for cluster solutions from 7 to 10 clusters.
            4. `data/nbh/data_clust_assign_year_std_orig_somkmeans_k[nclust]_metaggl_all.rds`: SOM + k-means assignments.
            5. `data/nbh/clust_metrics_year_std_[method]_metaggl_all.rds`: Clustering metrics and model results for all clustering methods.
            6. `data/nbh/clust_metrics_year_std_optimal_[method]_metaggl_all.rds`: Selected optimal clustering results.
            7. `data/nbh/SOM_metrics_[...].rds` and `data/nbh/final_SOM_[...].rds`: SOM tuning and final model results.
            8. `output/figures/cluster_metrics/*.png` and `output/figures/clustergram/*.png`: Model-selection metric and clustergram plots.
    - **2_2_nbh_cluster_model_selection_ave_distprof_plots.R**:
        - **Input**:
            1. `data/nbh/data_scaled_year_std_for_clustering_metaggl_all.rds`
            2. `data/nbh/data_clust_assign_year_std_orig_pca_mbkmeans_k_7_10_metaggl_all.rds`
            3. `data/nbh/data_clust_assign_year_std_orig_somkmeans_k[nclust]_metaggl_all.rds`
        - **Output**:
            1. `output/figures/cluster_distprof/ave_distprof_[...].jpg`: Average cluster distance-profile plots for the model-selection solutions.
    - **2_3_nbh_cluster_model_selection_cluster_maps.R**:
        - **Input**:
            1. `data/nbh/data_scaled_year_std_for_clustering_metaggl_all.rds`
            2. `data/nbh/data_clust_assign_year_std_orig_pca_mbkmeans_k_7_10_metaggl_all.rds`
            3. `data/nbh/data_clust_assign_year_std_orig_somkmeans_k[nclust]_metaggl_all.rds`
        - **Output**:
            1. `output/figures/cluster_maps/model_selection_maps/cluster_map_[...].jpg`: Cluster maps by method, cluster solution, urban region, and year.
    - **3_nbh_cluster_final_data.R**:
        - **Input**:
            1. `data/nbh/data_for_clustering_metaggl_all.rds`
            2. `data/nbh/data_clust_assign_year_std_orig_[clust_method]_k_7_10_metaggl_all.rds`
            3. `data/nbh/data_clust_assign_year_std_orig_somkmeans_k[nclust]_metaggl_all.rds`
        - **Output**:
            1. `data/nbh/data_final_std_cluster_assignment_year_std_[clust_method]_k[nclust]_metaggl_all.rds`: Final standardised cluster assignments.
    - **4_nbh_cluster_final_plots.R**:
        - **Input**:
            1. `data/nbh/data_final_std_cluster_assignment_year_std_[clust_method]_k[nclust]_metaggl_all.rds`
            2. `data/nbh/clust_metrics_year_std_mbkmeans_metaggl_all.rds`
            3. `data/nbh/data_scaled_year_std_for_clustering_metaggl_all.rds`
        - **Output**:
            1. `output/figures/cluster_metrics/cluster_metrics_[...].png`: Final cluster-quality metrics.
            2. `output/figures/clustergram/clustergram_[...].png`: Final clustergrams.
            3. `output/figures/clustergram/clustergram_and_metrics_[...].png`, `.tiff`, `.eps`: Combined clustergram and metrics plots.
            4. `output/figures/cluster_distprof/ave_distprof_facet_[...].png`, `.tiff`, `.eps`: Faceted final average distance profiles.
    - **5_nbh_seq_analysis.R**:
        - **Input**:
            1. `data/nbh/data_final_std_cluster_assignment_year_std_[clust_method]_k[nclust]_metaggl_all.rds`
            2. `data/nbh/clust_metrics_year_std_optimal_[clust_method]_metaggl_all.rds`
        - **Output**:
            1. `data/nbh/seq_cluster_tuning/cluster_seq_CLARA_[param_name]_metrics_[...].rds`: CLARA sequence-clustering results.
            2. `data/nbh/seq_cluster_tuning/cluster_seq_CLARA_boot_[param_name]_metrics_[...].rds`: Bootstrap results for sequence-clustering quality.
            3. `output/figures/sequences/seq_cluster_tuning/*.png` and `.../plot_seq_cluster/*.jpg`: Sequence-clustering metrics and representative-sequence plots.
            4. `data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_year_std_[clust_method]_k[nclust]_metaggl_all.rds`: Final sequence-cluster assignments.
    - **6_nbh_seq_analysis_rep_plots.R**:
        - **Input**:
            1. `data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_year_std_[clust_method]_k[nclust]_metaggl_all.rds`
        - **Output**:
            1. `output/figures/sequences/plot_seq_cluster/rep_sequences_[...].jpg` and `.eps`: Representative sequence plots for scalar, dimensional, and multiple-transition trajectories.
    - **7_nbh_seq_analysis_barplots.R**:
        - **Input**:
            1. `data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_year_std_[clust_method]_k[nclust]_metaggl_all.rds`
            2. `data/nbh/data_for_clustering_metaggl_all.rds`
        - **Output**:
            1. `output/figures/sequences/plot_seq_cluster/urbreg/heatmap_seq_cluster_by_urbreg_*.jpg` and `.tiff`: Sequence-cluster heatmaps by urban region.
            2. `output/figures/sequences/plot_seq_cluster/urbreg/barplot_seq_cluster_by_urbreg_*.jpg` and `.tiff`: Sequence-cluster barplots by urban region.
    - **8_nbh_seq_analysis_maps.R**:
        - **Input**:
            1. `data/nbh/data_final_seq_cluster_assignment_claraseqclust_[param_name]_k[seqnclust]_year_std_[clust_method]_k[nclust]_metaggl_all.rds`
            2. `data/nbh/data_for_clustering_metaggl_all.rds`
        - **Output**:
            1. `output/figures/sequences/maps/seq_cluster_top9_urbreg_[city].png` and `.tiff`: Maps of the nine most common sequence clusters.
            2. `output/figures/sequences/maps/seq_cluster_cons_urbreg_[city].png` and `.tiff`: Maps of consistent sequence clusters.
---

## 5. Reproducibility

- All scripts are designed for reproducibility.
- Set random seeds where applicable.

---

## 6. Dependencies

The analysis requires R (version 4.0 or higher) and the following R packages:

- pacman
- haven
- dplyr
- tidyr
- readr
- ggplot2
- dbscan
- sf
- data.table
- cluster
- ClusterR
- tibble
- gridExtra
- ggspatial
- FactoMineR
- grDevices
- stats 
- factoextra
- mclust
- microbenchmark
- ggrepel
- gridExtra
- grid
- kohonen
- aweSOM
- magick
- png
- TraMineR
- WeightedCluster
- ggseqplot
- patchwork
- cowplot
- forcats
- stringr
- purrr

---


## 7. License

This software is licensed under the [MIT License](https://opensource.org/licenses/MIT).

---

## 8. Contact

For questions or further information, please contact:

Ignacio Urria Yáñez - [i.a.urriayanez@tudelft.nl](mailto:i.a.urriayanez@tudelft.nl) 


TU Delft, Faculty of Architecture and the Built Environment, Department of Urbanism, Urban Studies 

---

## 9. Citation

**If you use this code or analysis in your research, please cite the paper:**

Urria, I., Petrović, A., van Ham, M., & Manley, D. (2026). Beyond the Neighborhood: Multidimensional and Multiscale Sociospatial Change in Dutch Cities, Annals of the American Association of Geographers,
[https://doi.org/10.1080/24694452.2026.2756877](https://doi.org/10.1080/24694452.2026.2756877) 

**And you can also cite the code repository:**

Urria Yáñez, I. (2026). Code underlying the paper "Beyond the Neighborhood: Multidimensional and Multiscale Sociospatial Change in Dutch Cities" (Version 1) [Computer software]. 4TU.ResearchData. 

[![DOI](https://data.4tu.nl/v3/datasets/f0b25cba-abbb-42c6-823a-422aad8c0721/doi-badge.svg)](https://doi.org/10.4121/f475df28-939d-4eb9-9b3f-e6e239ccb4cb)

---

## 10. Acknowledgements

This research was funded by the Delft Technology Fellowship awarded to Ana Petrović. Ana Petrović gratefully acknowledges funding fromthe Veni SGW Grant, financed by the DutchResearch Council (NWO), under Grant [https://doi.org/10.61686/RQHBC14069](https://doi.org/10.61686/RQHBC14069).

The authors gratefully acknowledge the support of the microdata team from Statistics Netherlands (CBS). 