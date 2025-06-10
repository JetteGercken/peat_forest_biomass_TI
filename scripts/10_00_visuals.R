# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# visualisation script



# 0. setup ----------------------------------------------------------------


# 0.1 functions -----------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


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



# 1. VISUALS ----------------------------------------------------------------------------------------------------


# 1.1. HEIGHT  -----------------------------------------------------------------
# 1.1.1. join in soil type info  -----------------------------------------------------------------
trees_height_data <- trees_height_data %>% 
  # join in soil type of the respective profile
  left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
            by = c("plot_ID" = "bfhnr_2")) %>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))




# 1. jitter and line plot -------------------------------------------------
tikzDevice::tikz(paste0(getwd(), '/output/out_graphs/DBH_H_al_bet.tex'),width=1000,height=847)

{
ggplot() +
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
}
dev.off()




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
                                    trees_data_bio$bot_genus %in% c("Alnus") & 
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
# 1000, 847

tikzDevice::tikz(paste0(getwd(), '/output/out_graphs/alnus_bio.tex'),width=1000,height=847)

{ 
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
  xlab("Diameter at breast height [$cm$]")+ 
  ylab("Woody aboveground biomass [$kg~tree^{-1}$]")
  }

dev.off()

# 1.2.2. BETULA biomass  --------------------------------------------------
# 1.2.2.1. betula biomass data wrangling ----------------------------------
# avbovegroun biomass of betula trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
betula_wag <-  trees_data_bio[compartiment %in% c("w_agb") & bot_genus %in% c("Betula") & min_org == "org", ]

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

tikzDevice::tikz(paste0(getwd(), '/output/out_graphs/betula_bio.tex'),width=1000,height=847)

{ 
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
  ggtitle("Betula woody aboveground biomass kg/tree by diameter cm")
}

dev.off()






# 1.3. CARBON STOCK -------------------------------------------------------
# carbon stock if national carbon inventory (CI 2017): 
# total carbon: 52859
# aboveground carbon: 41811 kg/ ha

# 1.3.1. Alnus carbon stock -----------------------------------------------
#  boxplot mean c stocks ha alnus per plot and species   ---------------------------------------
# subset for boxplot
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


tikzDevice::tikz(paste0(getwd(), '/output/out_graphs/alnus_c.tex'),width=1000,height=847)

{ 
# plotting boxplot
boxplot(as.numeric(values) ~ as.factor(names),
        col=my.colors ,
        xlab = "Biomass equation",
        ylab = "Carbon stock $C~t~ha^{-1}$", 
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
## legend
# legend("topleft", legend = c("tapeS", "literature eq. peat", "literature eq. partly peat", "literature eq.", 
#                              #"mean C t ha-1 NFI", 
#                              "mean C t ha-1 over all equations") , 
#        col = c("red", "#53868B", "#7AC5CD", "grey", # "blue", 
#                "black") , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(1.0 , 0.1))
}
dev.off()


# 1.3.2. Betula C stock boxplot-----------------------------------------------------

#  boxplot mean c stocks ha alnus per plot and species   ---------------------------------------
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


tikzDevice::tikz(paste0(getwd(), '/output/out_graphs/betula_c.tex'),width=1000,height=847)

{ 
  # plotting boxplot
  boxplot(as.numeric(values) ~ as.factor(names),
          col=my.colors ,
          xlab = "Biomass equation",
          ylab = "Carbon stock $C~t~ha^{-1}$", 
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
  points(as.numeric(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") 
                                                      & pseudo_mono_mean_func$compartiment == "w_agb"]) 
         , col = "black",  pch = 16)
  ## legend
  # legend("topleft", legend = c("tapeS", "literature eq. peat", "literature eq. partly peat", "literature eq.", 
  #                              #"mean C t ha-1 NFI", 
  #                              "mean C t ha-1 over all equations") , 
  #        col = c("red", "#53868B", "#7AC5CD", "grey", # "blue", 
  #                "black") , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(1.0 , 0.1))
}
dev.off()


