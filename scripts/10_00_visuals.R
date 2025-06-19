# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# visualisation script

# path to latex for tikz(): # C:\Users\gercken\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe
# options(tikzLatex = "C:/Program Files/texstudio/texstudio.exe")


# 0. setup ----------------------------------------------------------------


# 0.1 functions -----------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# options(tikzLatex = "C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe")

# 0.2 working directory ---------------------------------------------------
out.path <- paste0(getwd(), "/output/out_data/") 


# 0.1. import data --------------------------------------------------------
# soil data: which plot has which soil type 
soil_types_db <- read.delim(file = paste0(out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")

# data for height comparisson: measured and nls estiamted heights
trees_height_data <- read.delim(file = paste0(out.path, "HBI_LT_update_3.csv"), sep = ",", dec = ".")
trees_height_peterson <- read.delim(file = paste0(out.path, "HBI_LT_update_3_petterson.csv"), sep = ",", dec = ".")


# indiviual biomass data 
# only dbh based, no measured heights 
trees_data_bio <- read.delim(file = paste0(out.path, "HBI_LT_update_6_1.csv"), sep = ",", dec = ".")



# stock data 
# stockper species per plot in pseudo mono stands
pseudo_mono_P_SP <- read.delim(file = paste0(out.path, "C_stock_ha_pseudo_mono_P_SP.csv"), sep = ",", dec = ".")
pseudo_mono_mean_func <- read.delim(file = paste0(out.path, "C_stock_ha_pseudo_mono_func.csv"), sep = ",", dec = ".")


# importa data from literature research
bio_func_df <- read.delim(file = paste0(getwd(), "/data/input/", "B_lit_functions.csv"), sep = ",", dec = ".")


# 1. VISUALS ----------------------------------------------------------------------------------------------------


# 1.1. HEIGHT  -----------------------------------------------------------------
# 1.1.1. join in soil type info  -----------------------------------------------------------------
trees_height_data <- trees_height_data %>% 
  # join in soil type of the respective profile
  left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
            by = c("plot_ID" = "bfhnr_2")) %>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))




# 1. jitter and line plot -------------------------------------------------
 
DBH_H_al <- ggplot() +
  geom_point(data = (trees_height_data %>%
                       filter(H_method == "sampled" 
                              #    & BWI_SP_group %in% c("aLh", "aLn")
                              & bot_name %in% c("Alnus glutinosa")
                       ) %>% 
                       #left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                       mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
             aes(x = DBH_cm, y = H_m, color = min_org), alpha = 0.05)+
  geom_smooth(data = trees_height_peterson %>% 
                filter(H_method == "sampled" 
                       # & BWI_SP_group %in% c("aLh", "aLn")
                       # & bot_genus %in% c("Betula", "Alnus")
                       & bot_name %in% c("Alnus glutinosa")
                ),# %>%  
              #  mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
              aes(x = DBH_cm, y = H_m_nls, color = min_org))+ 
  theme_bw()+ 
 # facet_wrap(~bot_name)+
  xlab("diameter at breast height [cm]")+ 
  ylab("height [m]")+ 
  theme(legend.position="none") 


# betula dbh h 
DBH_H_bet <- ggplot() +
  geom_point(data = (trees_height_data %>%
                       filter(H_method == "sampled" 
                              #    & BWI_SP_group %in% c("aLh", "aLn")
                              & bot_name %in% c("Betula pubescens")
                       ) %>% 
                       #left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                       mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
             aes(x = DBH_cm, y = H_m, color = min_org), alpha = 0.05)+
  geom_smooth(data = trees_height_peterson %>% 
                filter(H_method == "sampled" 
                       # & BWI_SP_group %in% c("aLh", "aLn")
                       # & bot_genus %in% c("Betula", "Alnus")
                       & bot_name %in% c("Betula pubescens")
                ),# %>%  
              #  mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
              aes(x = DBH_cm, y = H_m_nls, color = min_org))+ 
  theme_bw()+ 
  xlab("diameter at breast height [cm]")+ 
  ylab("height [m]")+ 
  theme(legend.position="none") 


DBH_H_bet_al <- ggplot() +
  geom_point(data = (trees_height_data %>%
                       filter(H_method == "sampled" 
                              #    & BWI_SP_group %in% c("aLh", "aLn")
                              & bot_name %in% c("Betula pubescens", "Alnus glutinosa")
                       ) %>% 
                       #left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                       mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
             aes(x = DBH_cm, y = H_m, color = min_org), alpha = 0.05)+
  geom_smooth(data = trees_height_peterson %>% 
                filter(H_method == "sampled" 
                       # & BWI_SP_group %in% c("aLh", "aLn")
                       # & bot_genus %in% c("Betula", "Alnus")
                       & bot_name %in% c("Betula pubescens", "Alnus glutinosa")
                ),# %>%  
              #  mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
              aes(x = DBH_cm, y = H_m_nls, color = min_org))+ 
  theme_bw()+ 
  facet_wrap(~bot_name)+
  xlab("diameter at breast height [cm]")+ 
  ylab("height [m]")+ 
  theme(legend.position="none") 

# 1.1.2. height violin boxplot --------------------------------------------
# dbh comparisson violin plot org min boxplot separated  by species and soil type 
ggplot(data = trees_data %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         filter(bot_name == "Betula pubescens" & H_method == "sampled" |
                  bot_name == "Alnus glutinosa" & H_method == "sampled")) +
  geom_boxplot(aes(x = min_org, y = H_m))+
  geom_violin(aes(x = min_org, y = H_m), alpha=0.2) +
  facet_wrap(~bot_name)+
  xlab("site type (min: mineral, org: organic)")+ 
  ylab("height [m]")





# 1.2. BIOMASS ------------------------------------------------------------


# 1.2.1. Alnus biomass -------------------------------------------

# 1.2.1.1. alnus biomass data wrangling -----------------------------------
alnus_wag <-  trees_data_bio[trees_data_bio$compartiment %in% c("w_agb") &
                               # we only filter for alnus bot genus because we assume all
                               # alnus on org stands to be alnus glutinosa even those that are alnus sppp. 
                                trees_data_bio$bot_genus %in% c("Alnus") &  trees_data_bio$bot_species %in% c("glutinosa", "spp.") & 
                               trees_data_bio$min_org == "org" &
                                    trees_data_bio$paper_ID != 9
                                  , ]

alnus_wag_labels <- alnus_wag %>% group_by(paper_ID, func_ID, ID, country, peat) %>% 
  dplyr::summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c'),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name =  paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code)) %>% 
  distinct()

alnus_wag <- 
  alnus_wag %>% 
  mutate(
    farbe = ifelse(ID %like% c("tapes") , "red" , # tapes red
                   ifelse(peat == "yes",  "blue" , # "#53868B",
                          ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                 "grey" ) )))
color_map <- setNames(alnus_wag$farbe, alnus_wag$ID)


# sd mean betula 
m_b_al <- setDT(alnus_wag)[alnus_wag$ID != "9_w_agb", .(B_kg_tree=mean(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_al <- setDT(alnus_wag)[alnus_wag$ID != "9_w_agb", .(sd_B_kg_tree =sd(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]  
m_sd_b_al <- m_b_al[sd_b_al, on = list(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_al[, up_sd_B_kg_tree := (B_kg_tree + sd_B_kg_tree)]
m_sd_b_al[, low_sd_B_kg_tree := (B_kg_tree - sd_B_kg_tree)]  


# 1.2.1.2. alnus biomass ggplot  ------------------------------------------
# latex friendly export
# 1000, 847#

alnus_bio <-  
ggplot( )+ 
  geom_point(data = ungroup(alnus_wag)  %>% filter(!(ID %in% c("9_w_agb"))) 
             , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID), 
  )+
  geom_smooth(data = ungroup(alnus_wag) %>% filter(!(ID %in% c("9_w_agb")))
              , method= "loess"
              , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID) 
              , se = F 
  )+
  scale_color_manual(values = color_map)+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree), 
              method= "loess",
              data = m_sd_b_al,
              col = "black")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_al$low_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_al, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_al$up_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_al, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name), 
            data = alnus_wag_labels
            %>% filter(!(ID %in% c("9_w_agb")))
  )+
  ylim(0, 2700)+
  xlim(0, 64)+
  theme_bw()+
  theme(legend.position="none")+
  # ggtitle("Alnus woody aboveground biomass [$kg~tree^{-1}$] by diameter at breat height DBH [cm]")+
  xlab("Diameter at breast height [cm]")+ 
  ylab("Woody aboveground biomass [kg tree -1]")


# 1.2.2. BETULA biomass  --------------------------------------------------
# 1.2.2.1. betula biomass data wrangling ----------------------------------
# avbovegroun biomass of betula trees in kg by diameter
betula_wag <-  setDT(trees_data_bio)[compartiment %in% c("w_agb") & 
                                       bot_genus %in% c("Betula") & 
                                       bot_species %in% c("pubescens", "spp.") &
                                       min_org == "org", ]

# assign labels to functions
betula_wag_labels <- betula_wag %>% group_by(paper_ID, func_ID, peat, country, ID) %>% dplyr::summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c'),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name =  paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code)) %>% 
  distinct()

betula_wag <- 
  betula_wag %>% 
  mutate(
    farbe = ifelse(ID %like% c("tapes") , "red" , # tapes red
                   ifelse(peat == "yes",  "blue" , # "#53868B",
                          ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                 "grey" ) )))


color_map <- setNames(betula_wag$farbe, betula_wag$ID)

# sd mean betula 
m_b_be <- setDT(betula_wag)[ , .(B_kg_tree=mean(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_be <- setDT(betula_wag)[ , .(sd_B_kg_tree =sd(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]  
m_sd_b_be <- m_b_be[sd_b_be, on = list(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_be[, up_sd_B_kg_tree := (B_kg_tree + sd_B_kg_tree)]
m_sd_b_be[, low_sd_B_kg_tree := (B_kg_tree - sd_B_kg_tree)]  


# 1.2.2.2. betula biomass ggplot ------------------------------------------

betula_bio <- 
# plot 
ggplot( )+ 
  geom_point(data = ungroup(betula_wag) # %>% filter(!(ID %in% c("2_w_agb"))), 
             , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID), 
  )+
  geom_smooth(data = ungroup(betula_wag) #%>% filter(!(ID %in% c("2_w_agb")))
              , method= "loess"
              , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID) 
              , se = F 
  )+
  scale_color_manual(values = color_map)+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree), 
              method= "loess",
              data = m_sd_b_be,
              col = "black")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_be$low_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_be, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_be$up_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_be, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name), 
            data = betula_wag_labels
            %>% filter(!(ID %in% c("2_w_agb")))
  )+
  ylim(0, 2700)+
  xlim(0, 64)+
    theme_bw()+
    theme(legend.position="none")+
    # ggtitle("Alnus woody aboveground biomass [$kg~tree^{-1}$] by diameter at breat height DBH [cm]")+
  xlab("Diameter at breast height [cm]")+ 
  ylab("Woody aboveground biomass [kg tree -1]")
 






# 1.3. CARBON STOCK -------------------------------------------------------
# carbon stock if national carbon inventory (CI 2017): 
# total carbon: 52859
# aboveground carbon: 41811 kg/ ha

# assign names to carbon stock hectar summery
setDT(pseudo_mono_mean_func)[, `:=`( "names" = paste0(pseudo_mono_mean_func$paper_ID, ", ", 
                                                      ifelse(pseudo_mono_mean_func$func_ID != "w_agb", paste0(pseudo_mono_mean_func$func_ID, ", ") , ""), 
                                                      countrycode(pseudo_mono_mean_func$country, origin = 'country.name', destination = 'iso3c')) )]


# this is for outlayer plotting ggplot
n_fun <- function(x){
  return(data.frame(y = 0.95*70,
                    label = ""))
}

# 1.3.1. Alnus carbon stock -----------------------------------------------

# subset for boxplot

# pseudo_mono_P_SP species are already filtered into alnus glutinosa ans betula pubescens and additionally alsnus or betula spp. for org plots
values <- pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") 
                                  & pseudo_mono_P_SP$compartiment == "w_agb"
                                  & pseudo_mono_P_SP$paper_ID !=9]

names <-  setDT(pseudo_mono_P_SP[pseudo_mono_P_SP$bot_genus %in% c("Alnus") 
                                 & pseudo_mono_P_SP$compartiment == "w_agb" 
                                 & pseudo_mono_P_SP$paper_ID !=9,])[, `:=`(
                                   "names" = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code) )]$names

# assign colors: # mark only tapes plot 
farbe <-  setDT(pseudo_mono_P_SP[pseudo_mono_P_SP$bot_genus %in% c("Alnus") 
                                 & pseudo_mono_P_SP$compartiment == "w_agb" 
                                 & pseudo_mono_P_SP$paper_ID !=9,])[, `:=`(
                                   "farbe" = ifelse(ID %like% c("tapes") , "red" , # tapes red
                                                    ifelse(peat == "yes",  "blue" , # "#53868B",
                                                           ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                                                  "grey" ) )))]$farbe

# bind colors, names and values together 
alnus_wag <- as.data.frame(cbind(names, values, farbe))

color_map <- setNames(alnus_wag$farbe, alnus_wag$ID)

# mark only tapes plot
my.colors <- unique(alnus_wag[,c("names", "farbe")])$farbe


# 1.3.1.1. base r boxplot mean c stocks ha alnus per plot and species   ---------------------------------------
# alnus_c <- cowplot::ggdraw(function()
# plotting boxplot
boxplot(as.numeric(values) ~ as.factor(names),
        col=my.colors ,
        xlab = "Biomass equation",
        ylab = "Carbon stock C t ha-1", 
        # main = "Alnus spp. C stock t ha-1 by biomass equation", 
        ylim = c(0,275.1831))
# add nfi mean
# segments(x0 = 0.5, 
#          x1 = length(unique(names)) + 0.5,
#          y0 = ton(52859), y1 = ton(52859), col = "blue", lwd = 2) 
# # add nfi mean abovgorund c stock 
# segments(x0 = 0.5, 
#          x1 = length(unique(names)) + 0.5,
#          y0 = ton(41811), y1 = ton(41811), col = "green", lwd = 2) 
# add line for dataset mean across all equations
segments(x0 = 0.5, 
         x1 = length(unique(names)) + 0.5, 
         y0 = mean(as.numeric(na.omit(alnus_wag$values))), 
         y1 = mean(as.numeric(na.omit(alnus_wag$values))), col = "black", lwd = 2) #functions mean
# means of every function
points(as.numeric(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") 
                                                    & pseudo_mono_mean_func$compartiment == "w_agb"
                                                    & pseudo_mono_mean_func$paper_ID !=9]) 
       , col = "black",  pch = 16)
 


# 1.3.1.2. ggplot boxplot alnus c stock ha  -------------------------------


alnus_c <- ggplot(data = alnus_wag, 
                  aes(x = as.factor(names), y = as.numeric(values), fill = names)) +
  stat_boxplot(geom ='errorbar', width = 0.6) + # add wiskers: https://waterdata.usgs.gov/blog/boxplots/
  geom_boxplot(outliers = TRUE,
               outlier.color = "black",
               outlier.fill = "white",
               outlier.shape = 21) +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5)+
  scale_fill_manual(values = my.colors)+
  labs(
    x = "Biomass equation",
    y = "Carbon stock C t ha -1"
  ) +
   coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(legend.position = "none")+
# Add dataset-wide mean (black line)
 geom_hline(yintercept = mean(as.numeric(na.omit(alnus_wag$values))), 
                    color = "black", linewidth = 1)+
# Add per-function means (black points)
  geom_point(data = setDT(pseudo_mono_mean_func)[pseudo_mono_mean_func$bot_genus %in% c("Alnus") 
                                          & pseudo_mono_mean_func$compartiment == "w_agb"
                                          & pseudo_mono_mean_func$paper_ID !=9,],
                    aes(x = as.factor(names), 
                        y = as.numeric(na.omit((mean_C_t_ha)))), 
                    color = "black", size = 2)











# 1.3.2. Betula C stock boxplot-----------------------------------------------------
dev.off()
# subset for boxplot
values <- pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") 
                                  & pseudo_mono_P_SP$compartiment == "w_agb"]

names <-  setDT(pseudo_mono_P_SP[pseudo_mono_P_SP$bot_genus %in% c("Betula") 
                                 & pseudo_mono_P_SP$compartiment == "w_agb" ,])[, `:=`(
                                   "names" = paste0(paper_ID, ", ", 
                                                    ifelse(func_ID != "w_agb", 
                                                           paste0(func_ID, ", ") , ""), 
                                                    country_code) )]$names

# assign colors: # mark only tapes plot 
farbe <-  setDT(pseudo_mono_P_SP[pseudo_mono_P_SP$bot_genus %in% c("Betula") 
                                 & pseudo_mono_P_SP$compartiment == "w_agb" ,
])[, `:=`(
  "farbe" = ifelse(ID %like% c("tapes") , "red" , # tapes red
                   ifelse(peat == "yes",  "blue" , # "#53868B",
                          ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                 "grey" ) )))]$farbe

# bind colors, names and values together 
betula_wag <- as.data.frame(cbind(names, values, farbe))

color_map <- setNames(betula_wag$farbe, betula_wag$ID)

# mark only tapes plot
my.colors <- unique(betula_wag[,c("names", "farbe")])$farbe 



#  boxplot mean c stocks ha alnus per plot and species   ---------------------------------------
boxplot(as.numeric(values) ~ as.factor(names),
        col=my.colors ,
        xlab = "Biomass equation",
        ylab = "Carbon stock C t ha-1", 
        # main = "Alnus spp. C stock t ha-1 by biomass equation", 
        ylim = c(0,275.1831))
# add nfi mean
# segments(x0 = 0.5, 
#          x1 = length(unique(names)) + 0.5,
#          y0 = ton(52859), y1 = ton(52859), col = "blue", lwd = 2) 
# # add nfi mean abovgorund c stock 
# segments(x0 = 0.5, 
#          x1 = length(unique(names)) + 0.5,
#          y0 = ton(41811), y1 = ton(41811), col = "green", lwd = 2) 
# add line for dataset mean across all equations
segments(x0 = 0.5, 
         x1 = length(unique(names)) + 0.5, 
         y0 = mean(as.numeric(na.omit(betula_wag$values))), 
         y1 = mean(as.numeric(na.omit(betula_wag$values))), col = "black", lwd = 2) #functions mean
# means of every function
points(as.numeric(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") 
                                                    & pseudo_mono_mean_func$compartiment == "w_agb"]) 
       , col = "black",  pch = 16)





# ggplot boxplot mean c stock per  function -------------------------------
betula_c <- ggplot(data = betula_wag, 
                  aes(x = as.factor(names), y = as.numeric(values), fill = names) ) +
  scale_fill_manual(values = my.colors)+
   stat_boxplot(geom ='errorbar', width = 0.6) + # add wiskers: https://waterdata.usgs.gov/blog/boxplots/
  geom_boxplot(outliers = TRUE,
                 outlier.color = "black",
               outlier.fill = "white",
                 outlier.shape = 21) +
    stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5)+
  labs(
    x = "Biomass equation",
    y = "Carbon stock C t ha -1"
  ) +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(legend.position = "none")+
  # Add dataset-wide mean (black line)
  geom_hline(yintercept = mean(as.numeric(na.omit(betula_wag$values))), 
             color = "black", linewidth = 1)+
  # Add per-function means (black points)
  geom_point(data = setDT(pseudo_mono_mean_func)[pseudo_mono_mean_func$bot_genus %in% c("Betula") 
                                                 & pseudo_mono_mean_func$compartiment == "w_agb",],
             aes(x = as.factor(names), 
                 y = as.numeric(na.omit(mean_C_t_ha))), 
             color = "black", size = 2)







# 1.4 diamneter distribution --------------------------------------------------
# subset data accordingly
trees_sub <- unique(setDT(trees_data_bio)[ min_org == "org" & 
                                         bot_genus %in% c("Alnus", "Betula") & bot_species %in% c("pubescens", "glutinosa", "spp.") & 
                                         compartiment == "ag" |
                                         min_org == "org" & 
                                         bot_name %in% c("Alnus glutinosa", "Betula pubescens") &
                                         compartiment == "ag", ])
# alnus diamneter distribution --------------------------------------------------

# frequency
dev.off()
brk <- seq(from = 0, to = max(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"])+10, by = 1)
hist(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"], 
     breaks = brk, 
     main = "Alnus glutinosa DBH distribution",
     xlab = "diameter at breast height [cm]",
     ylab = "Frequency")



# density
dens_alnus <- density(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"]) 
brk <- seq(from = 0, to = max(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"])+10, by = 1)
hist(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"], 
     breaks = brk, 
     main = "Alnus glutinosa DBH distribution",
     xlab = "diameter at breast height [cm]",
     ylab = "Density", 
     freq = F)
lines(dens_alnus)


# betula diamneter distribution --------------------------------------------------

# frequency
dev.off()
brk <- seq(from = 0, to = max(trees_sub$DBH_cm[trees_sub$bot_genus == "Betula"])+10, by = 1)
hist(trees_sub$DBH_cm[trees_sub$bot_genus == "Betula"], 
     breaks = brk, 
     main = "Betula glutinosa DBH distribution",
     xlab = "diameter at breast height [cm]",
     ylab = "Frequency")



# density
dens_betula <- density(trees_sub$DBH_cm[trees_sub$bot_genus == "Betula"]) 
brk <- seq(from = 0, to = max(trees_sub$DBH_cm[trees_sub$bot_genus == "Betula"])+10, by = 1)
hist(trees_sub$DBH_cm[trees_sub$bot_genus == "Betula"], 
     breaks = brk, 
     main = "Betula glutinosa DBH distribution",
     xlab = "diameter at breast height [cm]",
     ylab = "Density", 
     freq = F)
lines(dens_betula)





# 1.5. biofunc table ------------------------------------------------------
# assign IDs to papers and functions
bio_func_df <- bio_func_df %>% 
  dplyr::distinct() %>% 
  arrange(author, title, species) %>% 
  dplyr::group_by(title, author, year, TSL) %>% 
  dplyr::mutate(func_ID = dplyr::row_number()) %>% 
  left_join(., 
            bio_func_df %>% 
              arrange(author, title, species) %>%
              select(title, author, year, TSL) %>% 
              dplyr::distinct() %>% 
              dplyr::mutate(paper_ID = dplyr::row_number()), 
            by = c("title", "author", "year", "TSL")) %>% 
  # assign country code
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c'))
# transform coefficients to numeric
bio_func_df[,13:28] <- lapply(bio_func_df[,13:28], as.numeric)
# add a column that combines func id and paper id
bio_func_df$ID <- paste0(bio_func_df$paper_ID,"_", bio_func_df$func_ID)
# exclude those functions that require age as we don´t always have it
bio_func_df <- bio_func_df[!(str_detect(bio_func_df$variables, "age")),]



# 1.5.1. subset -----------------------------------------------------------


# 1.5.1.1. Alnus ----------------------------------------------------------------
alnus_wag <-  trees_data_bio[trees_data_bio$compartiment %in% c("w_agb") &
                               trees_data_bio$bot_name %in% c("Alnus glutinosa", "Alnus spp.") & 
                               trees_data_bio$min_org == "org" &
                               trees_data_bio$paper_ID != 9
                                  , ]
# 1.5.1.1. Betula ----------------------------------------------------------------
betula_wag <-  trees_data_bio[compartiment %in% c("w_agb") & bot_name %in% c("Betula pubescens", "Betula spp.") & min_org == "org", ]

bio_func_wag_bet_al <- (bio_func_df %>% semi_join(., rbind(
  unique(setDT(alnus_wag)[, c("paper_ID",   "country") ]),
  unique(setDT(betula_wag)[, c("paper_ID", "country")]) ), 
  by = c("paper_ID",  "country")))[]


bio_func_wag_bet_al <- 
subset(setDT(bio_func_wag_bet_al), 
       ## filter alnus
       species %like% "Alnus glutinosa" &     # select only Alnus specific species
         !is.na(function.) &                    # select only those papers with functions
         compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb") | 
         species %like% "Alnus Glutinosa" &     # select only Alnus specific species
         !is.na(function.) &                    # select only those papers with functions
         compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb") | 
         species %like% "Alnus spp." &     # select only Alnus specific species
         !is.na(function.) &                    # select only those papers with functions
         compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb")|
         ## filter betula
         !is.na(function.) &        
         species %like% "Betula pubescens" &     # select only Alnus specific species
        compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb")
         ) 


bio_func_wag_bet_al <- setDT(bio_func_wag_bet_al)[, c(
  "paper_ID"
  , "func_ID"
  , "country"
   ,"species"
   ,"unit_B"
   #,unit_DBH
   #,unit_H	
   ,"DBH_cm_min"	
   ,"DBH_cm_max"	
   ,"H_m_min"
   ,"H_m_max"	
   , "age_min"
   , "age_max"
   #,reference	
   	
   ,"r2"
   ,"N"	
   ,"compartiment"
   ,"logarithm_B"	
   ,"function."	
   ,"a"	
   ,"b"	
   ,"c"	
   ,"d"	
   ,"e"	
   ,"f"
   ,"g"	
   ,"h"	
   ,"I"	
   ,"j"	
   ,"k"	
   ,"m"	
   ,"TSL"
   ,"u_k"	
   ,"e_ki"
   )
]

# add h and dbh unit
bio_func_wag_bet_al <- bio_func_wag_bet_al[, `:=`( 
  unit_DBH = ifelse((grepl( "DBH_cm*10", bio_func_wag_bet_al$function., fixed = TRUE)) == TRUE, "mm", "cm"),
  unit_H = ifelse((grepl( "H_m*10", bio_func_wag_bet_al$function., fixed = TRUE)) == TRUE, "dm", "m")
 , equation = bio_func_wag_bet_al$function.)] # add new col called equation

# create function col that only contains euqation form
bio_func_wag_bet_al <- bio_func_wag_bet_al[, `:=`(equation = gsub("DBH_cm", "DBH", bio_func_wag_bet_al$equation))]
bio_func_wag_bet_al <- bio_func_wag_bet_al[, `:=`(equation = gsub("DBH[*]10", "DBH", bio_func_wag_bet_al$equation))]
bio_func_wag_bet_al <- bio_func_wag_bet_al[, `:=`(equation = gsub("H_m", "H", bio_func_wag_bet_al$equation))]
bio_func_wag_bet_al <- bio_func_wag_bet_al[, `:=`(
equation = sub('.*=', '', bio_func_wag_bet_al$equation) # # create function col that only contains euqation form: select before and after symbol: https://stackoverflow.com/questions/37051288/extract-text-after-a-symbol-in-r
, compartiment = toupper(bio_func_wag_bet_al$compartiment))] # make compartiment into capital letters

# add ln to compartiemtn if necesarry
bio_func_wag_bet_al <- bio_func_wag_bet_al[, `:=`(
  compartiment = ifelse(is.na(bio_func_wag_bet_al$logarithm_B), bio_func_wag_bet_al$compartiment, paste0("ln(", bio_func_wag_bet_al$compartiment,")"))
  , DBH_range = paste0(bio_func_wag_bet_al$DBH_cm_min, "-",  bio_func_wag_bet_al$DBH_cm_max)
  , H_range = paste0(bio_func_wag_bet_al$H_m_min, "-",  bio_func_wag_bet_al$H_m_max)
  , age_range = paste0(bio_func_wag_bet_al$age_min, "-",  bio_func_wag_bet_al$age_max)
  # make equation latex friendly --> no need
  # ,equation = paste0("$", bio_func_wag_bet_al$equation, "$")
  )]

# rename compartiment column
bio_func_wag_bet_al$compartment <- bio_func_wag_bet_al$compartiment
# introduce reference column
bio_func_wag_bet_al$reference <- cat(paste("\\citep{}")) # \c didnt work because its a comand so we have to fool r: https://stackoverflow.com/questions/28421794/how-to-paste-a-backslash-character-to-a-string

bio_func_wag_bet_al$reference <- paste0("\\\\citep{", "author.year", "}")

# select only needed cols:
bio_func_wag_bet_al <- bio_func_wag_bet_al[,c(
  "paper_ID"
  , "func_ID"
  , "country"
  ,"reference"
  ,"species"
  ,"unit_B"
  ,"unit_DBH"
  ,"unit_H"	
  , "DBH_range"
  ,"H_range"	
  ,"age_range"
  ,"r2"
  ,"N"	
  ,"compartment"
  , "equation"
  ,"a"	
  ,"b"	
  ,"c"	
  ,"d"	
  ,"e"	
  ,"f"
  ,"g"	
  ,"h"	
  ,"I"	
  ,"j"	
  ,"k"	
  ,"m"	
  ,"TSL"
  ,"u_k"	
  ,"e_ki"
)]


# caption
# Biomass equations ("equation") for different biomass components ("component") where
# "AWG" reffers to the total aboveground biomass,
# "SW" is stemwood %(aboveground woody compartiments > 7~$cm$ diameter, above  0.3~$m$ height, without bark),
# "SB" is stemwood bark,
# "STW" is stumpwood %(aboveground woody compartiments > 7~$cm$ diameter, below  0.3~$m$ height, without bark),
# "STB" is stumpwood bark,
# "FWB" is finewood %(aboveground woody compartiments < 7~$cm$ diameter, above 0.3~$m$ height, without bark),
# NDL" is foliage.
# Columns "a", "b", "c", "d","e"	,"f","g"	,"h"	,"I"	,"j"	,"k"	,"m"	,"TSL","u_k"	and "e_ki" provide parameter values.
# Units of the diameter ("DBH"), height ("H") and biomass (B) are provided under "unit". The “ln” is the natural logarithm and "log10" the 10-based logarithm.
# Additional information like number of sampled trees ("N"), coefficients of determination ("r\^{2}"), and range of diameter
# ("DBH"), height ("H") and age ("age") of sampled trees are reported if available in the respective publication ("reference").
# The IDs refer to the subsequent number of the paper used for the figures ("paper_ID") and the number of the equation in the respective paper ("func_ID").


# 2. EXPORT ------------------------------------------------------------------


# 2.1. H ~ DBH min org comparisson  jitter line ---------------------------------------
 # 2.1.1. Alnus H ~ DBH min org comparisson  jitter line ---------------------------------------
  setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/DBH_H_alnus_HG.eps"))
 # plot
 DBH_H_al
 dev.off()
 
 # 2.1.2 Betula  H ~ DBH min org comparisson  jitter line ---------------------------------------
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/DBH_H_betula_HG.eps"))
 # plot
 DBH_H_bet
 dev.off()
 
 # 2.1.2 Betula and alnus facet H ~ DBH min org comparisson  jitter line ---------------------------------------
 
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/DBH_H_betula_alnus_HG.eps"))
 # plot
 DBH_H_bet_al
 dev.off()
 

 # 2.2. biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 
 
 # 2.2.1. alnus biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/alnus_bio_HG.eps"))
 # plot
 alnus_bio
 dev.off()

 # 2.2.2. betula biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/betula_bio_HG.eps"))
 # plot
 betula_bio
 dev.off()
 
 
 # 2.3. carbon per hectare ~ equation comparisson by equation boxplot ---------------------------------------
 
 # 2.3.1. alnus carbon per hectare ~ equation comparisson by equation boxplot  ---------------------------------------
 
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/alnus_c_HG.eps"))
 # plot
 alnus_c
 dev.off()
 
 # 2.3.2. betula carbon per hectare ~ equation comparisson by equation boxplot  ---------------------------------------
 
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/betula_c_HG.eps"))
 # plot
 betula_c
 dev.off()
 
 

# 2.4.1. export only alnus and betula funcs that we actually used ---------

 
 write.csv(bio_func_wag_bet_al, paste0(getwd(),"/output/out_data/bio_func_wag_bet_al.csv"))
 
 