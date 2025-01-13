# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken


# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path <- ("output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_3.csv" which contains plot area and stand data additionally to 
# tree data
trees_data <- read.delim(file = here(paste0(out.path, "HBI_LT_update_3.csv")), sep = ",", dec = ".")
tapes_tree_data <- read.delim(file = here(paste0(out.path, "HBI_LT_update_4.csv")), sep = ",", dec = ".")
trees_removed <- read.delim(file = here(paste0(out.path, trees_data$inv[1], "_LT_removed.csv")), sep = ",", dec = ".")
# soil data
soil_types_db <- read.delim(file = here(out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")







# 1. CALCULATIONS ---------------------------------------------------------

# 1.1. --------------------------------------------------------------------
# here we summarize all individual tree stocks per species, plot and hectar (only stocks and ba)
# but we only select our organic sites with the different biomass calculation methods
# and then we take only the alnus and betula trees 
# and then we use sum mtheir stock up per CCS, plot, species, compartiment, and biomass method
# and then we use the BA share that the alnus and betula has at this plot to reduce the plot area by that share
# and then we have per plot a alnus ha and betula ha stock which is our pseudo-mono-stand
# following we average the c/ha stock per biomass method over all plots 
# then we compare them 


