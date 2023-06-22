######## ------- MASTER SCRIPT for PhD paper 1 ------- #########

# For this paper, we conduct a Phenome Wide Association Study to examine 
# associations between educational level and the incidence of 833 diseases.
# In this master script, the process of conducting this study is described 
# while referring to the scripts used. 

# The master script is divided into three parts: 1) Data management 
#                                                2) Analysis
#                                                3) Presentation of results

# All scripts can be found in source("H:/KT22SVN/X30/PhD_Study1")
# And all tables are saved in the data base project D222107 (STATUDV)
library(DSTora)
library(DBI)
library(ROracle)

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")


#### ------- Part 1: Data management ------- ####

# All scripts for data management is located here:
# source("Code_data/")

#-- A)
# First, the population is created:
# source("Code_data/Population.R)
# The final table is named Population_2012_2021
Population_2012_2021 <- dbReadTable(conn_inci, "Population_2012_2021")

#-- B) 
# Secondly, we then need to create four data frames with diagnosis data
# 1) a data frame with all first time diagnoses: diags_all_level3
# 2) a data frame with all first time diagnoses for noncommunicable diseases: diags_NCD
# 3) a data frame with all first time diagnoses for communicable diseases (GBD): diags_CD_GBD
# 3) a data frame with all first time diagnoses for communicable diseases (ECDC): diags_CD_ECDC

# To do this we need data from LPR2 and LPR3. For the dfs for CDs we also need 
# diagnoses codes on the fourth level (not only on the third which is the case 
# for the other dfs). 

# separate dfs are created for LPR2 and 3 + the third and fourth level:
# source("Code_data/LPR2_diag_data_level3(all_NCD_CD).R)
# source("Code_data/LPR3_diag_data_level3(all_NCD_CD).R)
# source("Code_data/LPR2_diag_data_level4(CD).R)
# source("Code_data/LPR3_diag_data_level4(CD).R)

# We then create to dfs with all first time diagnoses - One for third level 
# diagnoses codes and one for fourth level diagnoses codes
# source("Code_data/diag_data_all_level3.R)
# source("Code_data/diag_data_all_level4.R)

diags_all_level3 <- dbReadTable(conn_inci, "diags_all_level3")
# This df is used for analysis

# With the dfs created we create the three other dfs used for analysis
# source(Code_data/diag_data_NCD.R)
diags_NCD <- dbReadTable(conn_inci, "diags_NCD")
# source(Code_data/diag_data_CD_GBD.R)
diags_CD_GBD <- dbReadTable(conn_inci, "diags_CD_GBD")
# source(Code_data/diag_data_CD_ECDC.R)
diags_CD_ECDC <- dbReadTable(conn_inci, "diags_CD_ECDC")



#### ------- Part 2: Analysis ------- ####

# All scripts for analysis is located here:
# source("Code_analysis/")

# In the analysis, we calculate age-standardized incidence rates (ASIR) 
# for all diagnoses (diags_all_level3), NCDs (diags_NCD) and for CDs 
# (diags_CD_GBD and diags_CD_ECDC)

# First, we estimate ASIR for all diagnosis codes
# source("Code_analysis/Estimating_ASIR_all.R)
# Final results are saved as a table named:
ASIR_all <- dbReadTable(conn_inci, "ASIR_all")

# Second, we estimate ASIR for NCDs
# source("Code_analysis/Estimating_ASIR_NCDs.R)
# Final results are saved as a table named:
ASIR_NCD <- dbReadTable(conn_inci, "ASIR_NCD")

# Third, we estimate ASIR for CDs (GBD)
# source("Code_analysis/Estimating_ASIR_CDs_GBD.R)
# Final results are saved as a table named:
ASIR_CD_GBD <- dbReadTable(conn_inci, "ASIR_CD_GBD")

# Third, we estimate ASIR for CDs (ECDC)
# source("Code_analysis/Estimating_ASIR_CDs_ECDC.R)
# Final results are saved as a table named:
ASIR_CD_ECDC <- dbReadTable(conn_inci, "ASIR_CD_ECDC")


# Lastly, we conduct a descriptive statistic in which we retrieve number of 
# observed cases of each diagnosis within our population and specified time
# frame. This information is used in plots that visualise the results. 
# We need frequencies on both third and fourth level diagnoses codes.

# source("Code_analysis/Frequency_all.R)
# Final results are saved as a table named:
N_all <- dbReadTable(conn_inci, "N_all")

# source("Code_analysis/Frequency_all_level4.R)
# Final results are saved as a table named:
N_level4 <- dbReadTable(conn_inci, "N_level4")



#### ------- Part 3: Presentation of results ------- ####

# All scripts for analysis is located here:
# source("Code_visualization/")

# First, Table 1 containing information on population characteristics is created.
# source(Code_visualization/Table1.R)
# The table is automatically saved here: H:/Table1.docx

# Second, Copenhagen plots are made to visualize the estimated ASIR. All plots 
# are generated from the same script. For the visualization we need the ICD10
# chapter names. These are generated from a separate script. 
source(Code_visualization_store_diag_names.R)
source(Code_visualization/Copenhagenplots.R)

# All plots are saved here: Results_plots/






