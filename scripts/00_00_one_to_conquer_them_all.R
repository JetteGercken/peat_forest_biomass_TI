# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken


# tis script is made to run all relevant momok and hbi scripts for the alnus and betual biomass paper

# !!!!!!!!!!!!!! ENTER YOUR CREDENTIAL HERE !!!!!!!!!!!!!!!!!!!!!!!!!!
db_name <- "bze3_altdaten"
db_server <- "134.110.100.88"
db_port <- "5432"
db_user <-  rstudioapi::askForPassword(prompt = "Please enter your username")
my_db_password <- rstudioapi::askForPassword(prompt = "Please enter your password")



#### common/ general operations -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# functions & packages
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))
# datasets import from postgres databank
source(paste0(getwd(),"/scripts/01_01_get_data_from_db.R"))
# data wrangling & sorting for bark and fruit types for forest structural indeyx calcualtion, we have to rename it: 01_02_...
source(paste0(getwd(), "/scripts/01_02_bark_fruit_types_FSI.R"))
# sort species into species groups required for data sorting & analysis, we have to rename it: 01_03_...
source(paste0(getwd(), "/scripts/01_03_species_groups.R"))

#### identify org/mineral soils -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# identify plots with organic soils: we have to rename it: 01_04_... 
source(paste0(getwd(),"/scripts/01_04_identify_org_soil_plots.R"))

#### sorting according to inv status -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# identify plots with organic soils: we have to rename it: 01_04_... 
source(paste0(getwd(),"/scripts/02_00_RG_LT_DW_inventory_plot_status_HBI.R"))

#### forest edges -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# LT
georef_on_off(source(paste0(getwd(), "/scripts/02_01_LT_forest_edges_HBI.R")), 
              source(paste0(getwd(), "/scripts/02_01_LT_forest_edges_HBI.R"))
              ## !!!! here one can select to work with the georefferenced or not georeffrenced plots and forest edges, default is not georefferenced
              , georefference = "not_georefferenced")
# RG
georef_on_off(source(paste0(getwd(), "/scripts/02_02_RG_forest_edges_HBI.R")), 
              source(paste0(getwd(), "/scripts/02_02_RG_forest_edges_HBI.R"))
              ## !!!! here one can select to work with the georefferenced or not georeffrenced plots and forest edges, default is not georefferenced
              , georefference = "not_georefferenced")


#### tree  inventory status  -------------------------------------------------------------------------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/02_03_LT_invetory_status_HBI_BZE3.R"))




#### tree height -------------------------------------------------------------------------------------------------------------------------------------------------------------------
## HBI & momok together: LT tree height
source(paste0(getwd(), "/scripts/03_01_LT_heights_HBI_BZE3.R"))




#### TapeS: biomass, C, N stocks -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# LT
source(paste0(getwd(), "/scripts/04_01_LT_stocks_HBI.R"))
# RG
source(paste0(getwd(), "/scripts/04_02_RG_stocks_HBI.R"))
# DW
source(paste0(getwd(), "/scripts/04_03_DW_stocks_HBI.R"))


#### summarizing hectar values -------------------------------------------------------------------------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/05_01_LT_summarising_hectar_values_HBI.R"))
source(paste0(getwd(), "/scripts/05_02_RG_summarising_hectar_values_HBI.R"))
source(paste0(getwd(), "/scripts/05_03_DW_summarising_hectar_values_HBI.R"))
source(paste0(getwd(), "/scripts/05_04_LT_RG_DW_summarising_hectar_values_HBI.R"))

#### biodiveristy index -------------------------------------------------------------------------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/06_00_biodiversity_index_HBI.R"))




#comparing heights
source(paste0(getwd(), "/scripts/03_02_comparing_height_org_min.R"))
