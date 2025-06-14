# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken
# summarizing Ha stocks of single tree biomass based on *DBH only*!!!!


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

out.path <- ("/output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_3.csv" which contains plot area and stand data additionally to 
# tree data
trees_data <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_update_6_1.csv"), sep = ",", dec = ".")
trees_removed <- read.delim(file = paste0(getwd(), out.path, trees_data$inv[1], "_LT_removed.csv"), sep = ",", dec = ".")

# import stand component wise summaries:
# these dataset contain the LT, DW and RG values summarised per ha on different levels of data grouping
# living trees
LT_summary <-read.delim(file = paste0(getwd(), out.path, "HBI_LT_stocks_ha_all_groups.csv"),sep = ",", dec = ".")


# 0.4 subset data ---------------------------------------------------------
# # filter only those trees and functions that were part of the single tree biomass analysis
# only select our organic sites with the different biomass calculation methods
# & only the alnus and betula trees 
trees_org <- subset(trees_data, min_org == "org" & bot_genus %in% c("Betula") & bot_species %in% c("pubescens", "spp.")| 
                      min_org == "org" & bot_genus %in% c( "Alnus") & bot_species %in% c("glutinosa", "spp.") & paper_ID != 9)

# join bot spec and genus to LT_summary 
LT_summary <- setDT(LT_summary)[setDT(unique(trees_data[, c("plot_ID", "inv", "min_org")])) , on = c("plot_ID", "inv") ,allow.cartesian=T]
LT_summary <- setDT(LT_summary)[,SP_code :=(tolower(SP_code))][setDT(SP_names_com_ID_tapeS)[,char_code_ger_lowcase:=(tolower(Chr_code_ger))], on = c("SP_code" = "char_code_ger_lowcase")]
LT_summary_org <- subset(LT_summary, min_org == "org" & bot_genus %in% c("Betula", "Alnus"))



# 1. CALCULATIONS ---------------------------------------------------------
# 1.1. BA related plot_A_ha ---------------------------------------------------------
# add BA share to tres dataset by species, plot and inv
trees_org <- setDT(trees_org)[setDT(unique(LT_summary_org[, c("plot_ID", "inv", "SP_code", "BA_percent")])) , on = c("plot_ID", "inv", "SP_code") ,allow.cartesian=T]
# calculate relative plot area per species
trees_org[, plot_A_ha_SP := (plot_A_ha*(BA_percent/100))] 
trees_org[, paper_ID := (ifelse(trees_org$func_ID == "tapes", max(na.omit(trees_org$paper_ID))+1 , paper_ID))]
trees_org[, ID:= (ifelse(trees_org$func_ID == "tapes", paste0(paper_ID, "_", func_ID), trees_org$ID)  )]
# add paper id to trees_org



# 1.2. pseudo-mono-stands: stock by plot & species, using realtive plot area ---------------------------------------------------------
pseudo_mono_P_SP <- trees_org %>% 
  # calculate stock per CCS and then per ha 
  group_by(plot_ID, CCS_r_m, paper_ID, func_ID, peat,  country, ID, bot_genus, compartiment, plot_A_ha_SP,plot_A_ha, BA_percent) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha_SP, # plot are is the area of the respecitve samplign circuit in ha 
          C_CCS_t_ha = sum(ton(B_kg_tree*0.5))/plot_A_ha_SP,
          BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha_SP, 
          n_trees_CCS_ha = dplyr::n()/plot_A_ha_SP) %>% 
  distinct()%>% 
  # now we summarise all the t/ha values of the cirlces per plot
  dplyr::group_by(plot_ID, paper_ID, func_ID, peat, country, ID, bot_genus, compartiment) %>% 
  dplyr::summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
           BA_m2_ha = sum(BA_CCS_m2_ha), 
            n_ha = sum(n_trees_CCS_ha)) 


# add country code to data set for lables
setDT(pseudo_mono_P_SP)[, `:=` (country_code = countrycode(pseudo_mono_P_SP$country, origin = 'country.name', destination = 'iso3c'))]

write.csv(pseudo_mono_P_SP, paste0(getwd(), out.path, "C_stock_ha_pseudo_mono_P_SP.csv"))


# 1.3. mean stock pseudo-mono-stands: by calculation method  ---------------------------------------------------------
pseudo_mono_mean_func <- pseudo_mono_P_SP %>% 
    group_by(paper_ID, func_ID, peat, country, ID, bot_genus, compartiment) %>% 
                                dplyr::summarise(mean_B_t_ha = mean(B_t_ha), 
                                          mean_C_t_ha = mean(C_t_ha), 
                                          mean_n_ha = mean(n_ha)) %>% 
  distinct() %>% 
  arrange(bot_genus, paper_ID,  func_ID, country, ID,compartiment)


# export for visuals
# this is the mean c stock per function based on the hectar values per plot per function calculated in pseudo_mono_P_SP
write.csv(pseudo_mono_mean_func, paste0(getwd(), out.path, "C_stock_ha_pseudo_mono_func.csv"))



# 1.4. Differences ---------------------------------------------------------------
# ((endwert-anfangswert)/ anfangswert)*100

# 1.4.1. differences TapeS NFI -----------------------------------------------
#mean NFI: 41811 kg/ha: https://bwi.info/inhalt1.3.aspx?Text=3.11%20Vorr%C3%A4te%20nach%20Baumartengruppen%20(ZIELMERKMALSTABELLE)%20(rechnerischer%20Reinbestand)&prRolle=public&prInv=THG2017&prKapitel=3.11
m_nfi_aLHn <- ton(41811)
# 1.4.1.1. Alnus differences TapeS NFI -----------------------------------------------
m_all_func_alnus <- mean(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" ])
sd_all_func_alnus <- sd(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" ])
var_all_func_alnus <- var(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" ])
cv_all_func_alnus <- sd_all_func_alnus/m_all_func_alnus


# percent difference:  88.16039% 36.86074 tons
((m_all_func_alnus - m_nfi_aLHn)/m_nfi_aLHn)*100 

# 1.4.1.2. Betula differences TapeS NFI -----------------------------------------------
m_all_func_betula <- mean(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb" ]))
sd_all_func_betula <- sd(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb" ]))
cv_all_func_betula <- sd_all_func_betula/m_all_func_betula


# percent difference: 47.15515 %, 19.71604 tonns
((m_all_func_betula - m_nfi_aLHn)/m_nfi_aLHn)*100 


# differenc Betula Alnus all func:  17.1447
m_all_func_alnus-m_all_func_betula

# 1.4.2. differences TapeS and overall mean -----------------------------------------------
# 1.4.2.1. Alnus differences TapeS and overall mean -----------------------------------------------
m_tapes_alnus <- mean(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$func_ID == "tapes"])

# percent difference: 0.03252872
((m_tapes_alnus - m_all_func_alnus)/m_all_func_alnus)*100 

# difference mean tapes alnus nfi 
((m_tapes_alnus - m_nfi_aLHn)/m_all_func_alnus)*100 

# 1.4.2.2. Betula differences TapeS and overall mean -----------------------------------------------
m_tapes_betula <- mean(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$func_ID == "tapes"]))

# percent difference: 14.85599
((m_tapes_betula - m_all_func_betula)/m_all_func_betula)*100 

# percent difference: 33.69055 %, 28.85649tons
((m_tapes_betula - m_nfi_aLHn)/m_nfi_aLHn)*100 






# 1.4.3. differences tapes and peat specific functions --------------------
# 1.4.3.1. Alnus differences tapes and peat specific functions --------------------
m_peat_alnus <- mean(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$peat %in% c("yes", "partly")])
sd_peat_alnus <- sd(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$peat %in% c("yes", "partly")])


m_tapes_alnus - m_peat_alnus #  -2.041231

# 1.4.3.2. Betula differences tapes and peat specific functions --------------------
m_peat_betula <- mean(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$peat %in% c("yes", "partly")])
sd_peat_betula <- sd(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$peat %in% c("yes", "partly")])


m_tapes_betula - m_peat_betula # 8.746796




# 1.4.4. differences tapes and nfi --------------------
# 1.4.4,1. Alnus differences tapes and nfi --------------------
# tapes to nfi 
m_tapes_alnus-m_nfi_aLHn

# 1.4.4,2. Betula differences tapes and nfi --------------------
# tapes to nfi 
m_tapes_betula -m_nfi_aLHn






# 1.4.5. stat summary stocks per ha  --------------------------------------
# 1.4.5.1. Alnus stat summary stocks per ha  --------------------------------------
## min 
# Tapes:  11.81495
min(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") & pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$func_ID == "tapes"]))

# min all:  8.920784
min(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") & pseudo_mono_P_SP$compartiment == "w_agb"]))
# min all func: "14_w_agb"
pseudo_mono_P_SP$ID[pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$C_t_ha == min(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") & pseudo_mono_P_SP$compartiment == "w_agb"]))]

# min mean all :  58.87383
min(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb"]))
# min mean all function:  14_w_agb"
pseudo_mono_mean_func$ID[ pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$mean_C_t_ha == min(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb"]))]

## max
# Tapes: 139.6858
max(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") & pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$func_ID == "tapes"]))

# max all:  174.1403
max(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") & pseudo_mono_P_SP$compartiment == "w_agb"]))
# max all func: "40_w_agb"
pseudo_mono_P_SP$ID[pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$C_t_ha == max(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Alnus") & pseudo_mono_P_SP$compartiment == "w_agb"]))]


# max mean all :  92.78914
max(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb"]))
# max mean  function:  ] "27_w_agb"
pseudo_mono_mean_func$ID[ pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$mean_C_t_ha == max(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb"]))]





# 1.4.5.2. Betula  stat summary stocks per ha  --------------------------------------
## min 
# Tapes:  1.010374
min(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$func_ID == "tapes"]))

# min all: 0.5585434
min(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb"]))
# min all func: "6_w_agb"
pseudo_mono_P_SP$ID[pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$C_t_ha == min(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb"]))]

# min mean all :  28.41816
min(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb"]))
# min mean all function:  6_w_agb
pseudo_mono_mean_func$ID[ pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$mean_C_t_ha == min(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb"]))]

## max
# Tapes:  70.66749
max(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$func_ID == "tapes"]))

# ma all: 250.3115
max(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb"]))
# min all func: "29_w_agb"
pseudo_mono_P_SP$ID[pseudo_mono_P_SP$compartiment == "w_agb" & pseudo_mono_P_SP$C_t_ha == max(na.omit(pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb"]))]


# max mean all :  110.9092
max(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb"]))
# max mean  function:  ] "29_w_agb"
pseudo_mono_mean_func$ID[ pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$mean_C_t_ha == max(na.omit(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb"]))]






# 2. visuals --------------------------------------------------------------
# https://bwi.info/inhalt1.3.aspx?Text=3.14%20Kohlenstoff%20[kg/ha]%20nach%20Baumartengruppe%20und%20Altersklasse%20(rechnerischer%20Reinbestand)&prRolle=public&prInv=THG2017&prKapitel=3.14
# mean c stock of "andere Laubhölzer niedlriger Lebensdauer (aLn) according to BWI: 52859 kg ha-1 


# 2.1.2. boxplot c stocks ha alnus  ---------------------------------------
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



# plotting boxplot
boxplot(as.numeric(values) ~ as.factor(names),
        col=my.colors ,
        xlab = "Biomass equation",
        ylab = "C stock t ha-1", 
        main = "Alnus spp. C stock t ha-1 by biomass equation", 
        ylim = c(0,275.1831))
# add nfi mean
# segments(x0 = 0.5, 
#          x1 = length(unique(names)) + 0.5,
#          y0 = ton(52859), y1 = ton(52859), col = "blue", lwd = 2) 
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
# legend
legend("topleft", legend = c("tapeS", "literature eq. peat", "literature eq. partly peat", "literature eq.", 
                             #"mean C t ha-1 NFI", 
                             "mean C t ha-1 over all equations") , 
       col = c("red", "#53868B", "#7AC5CD", "grey", # "blue", 
               "black") , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(1.0 , 0.1))



# 2.2.1. betula mean c t ha boxplot -----------------------------------------------------

# subset for boxplot
values <- pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb"]
names <- pseudo_mono_P_SP$ID[pseudo_mono_P_SP$bot_genus %in% c("Betula") & pseudo_mono_P_SP$compartiment == "w_agb"]
betula_wag <- as.data.frame(cbind(names, values))
# mark only tapes plot
my.colors <-my.colors <- ifelse(levels(as.factor(betula_wag$names)) %like% c("tapes") , "red" , # tapes red
                                ifelse(levels(as.factor(betula_wag$names)) %in% c(pseudo_mono_mean_func$ID[pseudo_mono_mean_func$peat == "yes"]), "blue", # "#53868B",
                                       ifelse(levels(as.factor(betula_wag$names)) %in% c(pseudo_mono_mean_func$ID[pseudo_mono_mean_func$peat == "partly"]), "turquoise1" , # "#7AC5CD",
                                              "grey" ) )) 

# plotting boxplot
boxplot(as.numeric(values) ~ as.factor(names),
        col=my.colors ,
        xlab = "Biomass calculation method",
        ylab = "mean C stock t ha-1", 
        main = "Betula spp. C stock t ha-1 by biomass method", 
        ylim = c(0, max(as.numeric(values), na.rm = TRUE) * 1.1))
# add nfi mean
# segments(x0 = 0.5, 
#          x1 = length(unique(names))- 0.5 ,
#          y0 = ton(52859), y1 = ton(52859), col = "blue", lwd = 2) 
# add line for dataset mean
segments(x0 = 0.5, 
         x1 = length(unique(names) )- 0.5 , 
         y0 = mean(as.numeric(na.omit(betula_wag$values))), 
         y1 = mean(as.numeric(na.omit(betula_wag$values))), col = "black", lwd = 2) #functions mean
# means of every function
points(as.numeric(pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") 
                                                    & pseudo_mono_mean_func$compartiment == "w_agb"]) 
       , col = "black", pch = 16)
# legend 
legend("topleft", legend = c("tapeS", "literature eq. peat", "literature eq. partly peat", "literature eq.", 
                             #"mean C t ha-1 NFI", 
                             "mean C t ha-1 over all equations") , 
       col = c("red", "#53868B", "#7AC5CD", "grey", # "blue", 
               "black") , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(1.0 , 0.1))



















# NOTES -------------------------------------------------------------------



names <- c(rep("Maestro", 20) , rep("Presto", 20) , 
           rep("Nerak", 20), rep("Eskimo", 20), rep("Nairobi", 20), rep("Artiko", 20))
value <- c(  sample(3:10, 20 , replace=T) , sample(2:5, 20 , replace=T) , 
             sample(6:10, 20 , replace=T), sample(6:10, 20 , replace=T) , 
             sample(1:7, 20 , replace=T), sample(3:10, 20 , replace=T) )
data <- data.frame(names,value)

# Prepare a vector of colors with specific color for Nairobi and Eskimo
myColors <- ifelse(levels(as.factor(data$names))=="Nairobi" , rgb(0.1,0.1,0.7,0.5) , 
                   ifelse(levels(as.factor(data$names))=="Eskimo", rgb(0.8,0.1,0.3,0.6),
                          "grey90" ) )

# Build the plot
boxplot(data$value ~ data$names , 
        col=myColors , 
        ylab="disease" , xlab="- variety -")












view(trees_org[trees_org$bot_genus %in% c("Betula") & trees_org$paper_ID %in% c("34", "36"),])
# n. soiltype to lt but not needed ----------------------------------------
# add soil type to LT_summary
LT_summary <- setDT(LT_summary)[
  soil_types_db[, c("bfhnr_2", "min_org")],    # select only plot_ID, and site type
  on = c("plot_ID"= "bfhnr_2")]                # select only plot_ID, and site type

LT_summary[, min_org := (ifelse(is.na(min_org) & plot_ID %in% c(unique(trees_data$plot_ID[trees_data$inv == "momok"])), "org", min_org) )]


# tapes test
SP_names_com_ID_tapeS %>% filter(bot_name %in% c("Betula pubescens", "Alnus glutinosa"))
TapeS::tprSpeciesCode() %>% filter(ID %in% c(28, 26))
#TapeS::tprTrees(spp = 26, )
obj <- tprTrees(
  spp = 35,
  Dm = list(c(30)),
  Hm = list(c(1.3)),
  Ht = 30,
  inv = NULL
)

TapeS::tprBiomass(obj)
getSpeciesCode(inSp = NULL, outSp = NULL)

bdat_sp <- as.data.frame(cbind(Bdat_BA = c(1:36), 
                               biomass_BA = c(1,1,2,2,4,4,4,3,5,5,5,1,1,1,6,9,7,7,17,17,8,10,10,11,10,15,12,16,6,14,13,6,6,18,6,6)))
sp_list_Bdat <- as.data.frame(getSpeciesCode(inSp = NULL, outSp = NULL))
sp_list_Bdat <- setDT(bdat_sp)[setDT(sp_list_Bdat), on = c("Bdat_BA" = "ID")]

sp_list_Bdat["long" %like% c("Birch", "Alder") , ]
getSpeciesCode(inSp = NULL, outSp = NULL)


sp_list_Bdat %>% arrange(biomass_BA)






# n barplot alnus c stocks ------------------------------------------------

# N. 2.1. alnus mean c t ha barplot ------------------------------------------
values <- pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$paper_ID != 41]
names <- pseudo_mono_mean_func$ID[pseudo_mono_mean_func$bot_genus %in% c("Alnus") & pseudo_mono_mean_func$compartiment == "w_agb" & pseudo_mono_mean_func$paper_ID != 41]
alnus_wag <- as.data.frame(cbind(names, values))# , mean_NFI = c(ton(52859))))

# plotting barplot
barplot(height = as.numeric(alnus_wag$values), names=alnus_wag$names, 
        xlab = "Biomass calculation method",
        ylab = "mean C stock t ha-1", 
        main = "Alnus spp. mean C stock t ha-1 by biomass method")
abline(h=ton(52859), col = "red") # nfi mean
abline(h=mean(as.numeric(alnus_wag$values)), col = "blue") #functions mean 


# N. 2.2.betula c stock ha barplot ------------------------------------------
  values <- pseudo_mono_mean_func$mean_C_t_ha[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb"]
names <- pseudo_mono_mean_func$ID[pseudo_mono_mean_func$bot_genus %in% c("Betula") & pseudo_mono_mean_func$compartiment == "w_agb"]
betula_wag <- as.data.frame(cbind(names, values))

# plotting barplot
barplot(height = as.numeric(betula_wag$values), names=betula_wag$names, 
        xlab = "Biomass calculation method",
        ylab = "mean C stock t ha-1", 
        main = "Betula spp. mean C stock t ha-1 by biomass method", 
        cex.names=0.75)
abline(h=ton(52859), col = "red")
abline(h=mean(as.numeric(na.omit(betula_wag$values))), col = "blue")





