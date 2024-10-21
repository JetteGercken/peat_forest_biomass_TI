# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# slightly adjusted for paper about peatland biomass of alder and birch trees 
# Henriette Gercken
# height on organic an non organic sites

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path <- here("output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: 
# this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we import a dataset called "HBI_LT_update_2.csv" which contains plot area and stand data additionally to the original tree data
HBI_trees <- read.delim(file = here(out.path, "HBI_LT_update_3.csv"), sep = ",", dec = ".")
soil_types_db <- read.delim(file = here(out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")

# heights of alnus and betual sepcies over all NSI plots
# by diameter 
# separeted in organic vs. minearl sites and species (group) (botanical name)
ggplot(data = HBI_trees %>% 
         # join in soil type of the respective profile
         left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
                   by = c("plot_ID" = "bfhnr_2")) %>% 
  filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
    startsWith(bot_name, "Alnus") & H_method == "sampled")%>% 
    filter(C_layer %in% c(1) & Kraft %in% c(1)))+ 
  geom_jitter(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+ 
  geom_smooth(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+
  facet_wrap(~bot_name)

# heights of alnus and betula over all NSI plots
# by diameter 
# separeted in organic vs. minearl sites and botanical genus
ggplot(data = HBI_trees %>% 
         left_join(., SP_names_com_ID_tapeS %>% select(bot_name, bot_genus), by = "bot_name") %>% 
         # join in soil type of the respective profile
         left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
                   by = c("plot_ID" = "bfhnr_2")) %>% 
         filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
                  startsWith(bot_name, "Alnus") & H_method == "sampled") )+
  geom_jitter(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+ 
  geom_smooth(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+
  facet_wrap(bot_genus~C_layer)+
  ylim(0, 50)


# dbh comparisson org min boxplot
ggplot(data = HBI_trees %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         left_join(., SP_names_com_ID_tapeS %>% select(bot_name, bot_genus), by = "bot_name") %>% 
         # join in soil type of the respective profile
         left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
                   by = c("plot_ID" = "bfhnr_2")) %>% 
         filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
                  startsWith(bot_name, "Alnus") & H_method == "sampled"))+
  geom_boxplot(aes(x = min_org, y = DBH_cm))+
  facet_wrap(~bot_genus)


ggplot(data = HBI_trees %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         left_join(., SP_names_com_ID_tapeS %>% select(bot_name, bot_genus), by = "bot_name") %>% 
         # join in soil type of the respective profile
         left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
                   by = c("plot_ID" = "bfhnr_2")) %>% 
         filter(bot_name == "Betula pubescens" & H_method == "sampled" |
         bot_name == "Alnus glutinosa" & H_method == "sampled")) +
  geom_boxplot(aes(x = min_org, y = DBH_cm))+
  geom_violin(aes(x = min_org, y = DBH_cm), alpha=0.2) +
  facet_wrap(~bot_name)


ggplot(data = HBI_trees %>%
         mutate(kraft_group = ifelse(Kraft %in% c(1, 2), "dominating", "not dominating")) %>% 
         filter(Kraft %in% c(1, 2, 3, 4)) %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         left_join(., SP_names_com_ID_tapeS %>% select(bot_name, bot_genus), by = "bot_name") %>% 
         # join in soil type of the respective profile
         left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
                   by = c("plot_ID" = "bfhnr_2")) %>% 
         filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
                  startsWith(bot_name, "Alnus") & H_method == "sampled"))+
  geom_boxplot(aes(x = min_org, y = H_m))+
  geom_violin(aes(x = min_org, y = H_m), alpha=0.2) +
  facet_grid(bot_genus~kraft_group)

