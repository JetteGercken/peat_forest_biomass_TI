# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken



# here we summarize all individual tree stocks per species, plot and hectar (only stocks and ba)
# but we only select our organic sites with the different biomass calculation methods
# and then we take only the alnus and betula trees 
# and then we use sum mtheir stock up per CCS, plot, species, compartiment, and biomass method
# and then we use the BA share that the alnus and betula has at this plot to reduce the plot area by that share
# and then we have per plot a alnus ha and betula ha stock which is our pseudo-mono-stand
# following we average the c/ha stock per biomass method over all plots 
# then we compare them 

# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path <- ("/output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_3.csv" which contains plot area and stand data additionally to 
# tree data
trees_data <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_update_5.csv"), sep = ",", dec = ".")
trees_removed <- read.delim(file = paste0(getwd(), out.path, trees_data$inv[1], "_LT_removed.csv"), sep = ",", dec = ".")
# soil data
soil_types_db <- read.delim(file = paste0(getwd(), out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")

# import stand component wise summaries:
# these dataset contain the LT, DW and RG values summarised per ha on different levels of data grouping
# living trees
LT_summary <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_stocks_ha_all_groups.csv"),sep = ",", dec = ".")


# 0.4 subset data ---------------------------------------------------------
# only select our organic sites with the different biomass calculation methods
# & only the alnus and betula trees 
trees_org <- subset(trees_data, min_org == "org" & bot_genus %in% c("Betula", "Alnus"))

# add soil type to LT_summary
LT_summary <- setDT(LT_summary)[
  soil_types_db[, c("bfhnr_2", "min_org")],    # select only plot_ID, and site type
  on = c("plot_ID"= "bfhnr_2")]                # select only plot_ID, and site type

LT_summary[, min_org := (ifelse(is.na(min_org) & plot_ID %in% c(unique(trees_data$plot_ID[trees_data$inv == "momok"])), "org", min_org) )]



# 1. CALCULATIONS ---------------------------------------------------------

DT[, Mean:=mean(X), by=list(Y, Z)]

# calculate stock per CCS and then per ha 
trees_data %>% 
  group_by(plot_ID, CCS_r_m, inv, compartiment) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
          C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
          BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
          n_trees_CCS_ha = dplyr::n()/plot_A_ha) %>% 
  distinct()%>% 
  # now we summarise all the t/ha values of the cirlces per plot
  group_by(plot_ID, inv, compartiment) %>% 
  summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
            N_t_ha = sum(N_CCS_t_ha),
            BA_m2_ha = sum(BA_CCS_m2_ha), 
            n_ha = sum(n_trees_CCS_ha)) %>% 
  mutate(stand_component = "LT", 
         stand = "all", 
         SP_code = "all")









