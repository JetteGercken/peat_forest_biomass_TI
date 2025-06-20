# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# visualisation script

# path to latex for tikz(): # C:\Users\gercken\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe
# options(tikzLatex = "C:/Program Files/texstudio/texstudio.exe")


# 0. setup ----------------------------------------------------------------
install.packages("ggrepel")
library(ggrepel)
install.packages("ggforce")
library(ggforce)

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

DBH_H_bet_al

table(trees_height_data %>%
        filter(H_method == "sampled", bot_name %in% c("Betula pubescens", "Alnus glutinosa")) %>%
        mutate(min_org = ifelse(inv == "momok", "org", min_org)) %>%
        pull(min_org))



## entpixelte Regressionsgerade mit ggplot

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)





DBH_H_plot <- ggplot() +
  
  # Punktwolke mit gefüllten Punkten mit schwarzem Rand
  geom_point(
    data = trees_height_data %>%
      filter(H_method == "sampled", bot_name %in% c("Betula pubescens", "Alnus glutinosa")) %>%
      mutate(min_org = ifelse(inv == "momok", "org", min_org)),
    aes(x = DBH_cm, y = H_m, fill = min_org),  # Innenfarbe
    color = "black",                          # Schwarzer Rand
    shape = 21,                              # gefüllter Kreis mit Rand
    alpha = 0.3,
    size = 2
  ) +
  
  # Konfidenzband (fill für Füllfarbe)
  geom_smooth(
    data = trees_height_data %>%
      filter(H_method == "sampled", bot_name %in% c("Betula pubescens", "Alnus glutinosa")) %>%
      mutate(min_org = ifelse(inv == "momok", "org", min_org)),
    aes(x = DBH_cm, y = H_m, fill = min_org),
    method = "loess", se = TRUE, span = 0.8,
    color = NA, alpha = 0.2
  ) +  
  
  # Modellierte Linien (color)
  geom_line(
    data = trees_height_peterson %>%
      filter(H_method == "sampled", bot_name %in% c("Betula pubescens", "Alnus glutinosa")),
    aes(x = DBH_cm, y = H_m_nls, color = min_org),
    size = 1.2
  ) +
  
  # Farben anpassen: Achtung, in deinen Daten heißt es "mineral" oder "min"?
  scale_color_manual(values = c("min" = "grey30", "org" = "blue")) +
  scale_fill_manual(values = c("min" = "grey30", "org" = "blue")) +
  
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),        # Schriftgröße
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(hjust = 1)
  ) +
  
  facet_wrap(~ bot_name) +
  
  xlab("Diameter at breast height [cm]") + 
  ylab("Height [m]")




DBH_H_plot



# 1.1.2. height violin boxplot --------------------------------------------
# dbh comparisson violin plot org min boxplot separated  by species and soil type 
ggplot(data = trees_height_data %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         filter(bot_name == "Betula pubescens" & H_method == "sampled" |
                  bot_name == "Alnus glutinosa" & H_method == "sampled")) +
  geom_boxplot(aes(x = min_org, y = H_m))+
  geom_violin(aes(x = min_org, y = H_m), alpha=0.2) +
  facet_wrap(~bot_name)+
  xlab("site type (min: mineral, org: organic)")+ 
  ylab("height [m]")





# 1.2. BIOMASS ------------------------------------------------------------
install.packages("ggrepel")
library(ggrepel)

# 1.2.1. Alnus biomass -------------------------------------------

# 1.2.1.1. alnus biomass data wrangling -----------------------------------
# Alnus-Datensatz filtern
alnus_wag <- trees_data_bio %>%
  filter(compartiment == "w_agb",
         bot_genus == "Alnus",
         bot_species %in% c("glutinosa", "spp."),
         min_org == "org",
         paper_ID != 9) %>%
  mutate(
    ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
    farbe = case_when(
      grepl("tapes", ID, ignore.case = TRUE) ~ "darkorange1",
      peat == "yes" ~ "blue",
      peat == "partly" ~ "turquoise2",
      TRUE ~ "grey50"
    )
  )

# Farbzuordnung
color_map <- setNames(alnus_wag$farbe, alnus_wag$ID)

# Labels vorbereiten: max DBH und Biomasse je Gruppe
alnus_wag_labels <- alnus_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat) %>%
  slice_max(DBH_cm, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(
    country_code = countrycode(country, origin = "country.name", destination = "iso3c"),
    label_name = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", "), ""), country_code)
  ) %>%
  distinct()

# Mittelwerte und Standardabweichungen pro Messpunkt berechnen
m_b_al <- setDT(alnus_wag)[ID != "9_w_agb", .(B_kg_tree = mean(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_al <- setDT(alnus_wag)[ID != "9_w_agb", .(sd_B_kg_tree = sd(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_al <- m_b_al[sd_b_al, on = .(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_al[, `:=`(
  up_sd_B_kg_tree = B_kg_tree + sd_B_kg_tree,
  low_sd_B_kg_tree = B_kg_tree - sd_B_kg_tree
)]

# Funktion: Loess fitten und vorletzten Punkt aus der geglätteten Kurve holen
get_smooth_points <- function(data) {
  loess_fit <- loess(B_kg_tree ~ DBH_cm, data = data)
  preds <- predict(loess_fit, se = FALSE)
  df <- data.frame(DBH_cm = data$DBH_cm, B_kg_tree = preds)
  df <- df[order(df$DBH_cm), ]
  
  # Letzter Punkt der geglätteten Kurve nehmen:
  return(df[nrow(df), ])
}

# Für alle IDs Loess-Kurve auswerten
smooth_points <- alnus_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat) %>%
  group_modify(~ get_smooth_points(.x)) %>%
  ungroup() %>%
  mutate(
    label_x = DBH_cm + 10,
    label_y = B_kg_tree + 100
  )

# Labelnamen aus betula_wag_labels dazuholen per Join (nach ID)
smooth_points <- smooth_points %>%
  left_join(
    alnus_wag_labels %>% select(ID, label_name),
    by = "ID"
  )



alnus_bio <- ggplot() +
  # Loess-Fits pro ID ohne den Ausschluss "9_w_agb"
  geom_point(data = ungroup(alnus_wag) %>% filter(ID != "9_w_agb"),
             aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(data = filter(alnus_wag, ID != "9_w_agb"),
              aes(x = DBH_cm, y = B_kg_tree, color = ID, group = ID),
              method = "loess", se = FALSE) +
  
  # Farbskala
  scale_color_manual(values = color_map) +
  
  # Standard-Kurve + SD-Bereiche (optional, falls vorhanden)
  geom_smooth(data = m_sd_b_al, aes(x = DBH_cm, y = B_kg_tree),
              method = "loess", color = "red", se = F) +
  geom_smooth(data = m_sd_b_al, aes(x = DBH_cm, y = low_sd_B_kg_tree),
              method = "loess", color = "red", linetype = "dashed", se = FALSE) +
  geom_smooth(data = m_sd_b_al, aes(x = DBH_cm, y = up_sd_B_kg_tree),
              method = "loess", color = "red", linetype = "dashed", se = FALSE) +
  
  # Labels mit geom_text_repel, die sich von den Punkten nach rechts oben absetzen
  geom_text_repel(
    data = smooth_points,
    aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = ID),
    nudge_x = 3,
    nudge_y = 100,
    segment.color = "black",
    segment.size = 0.5,
    segment.linetype = "dotted",  # <-- hier!
    hjust = 0,
    size = 6,
    max.overlaps = Inf
  ) +
  
  # Achsenlimits und Thema
  ylim(0, 2750) +
  xlim(0, 75) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),        # Schriftgröße
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(hjust = 1)
  )  +
  theme(legend.position = "none") +
  
  # Achsenbeschriftungen
  xlab("Diameter at breast height [cm]") +
  ylab("Woody aboveground biomass [kg"~tree^{-1}*"]")

print(alnus_bio)






# 1.2.2.1. betula biomass data wrangling ----------------------------------
# avbovegroun biomass of betula trees in kg by diameter
# 1.2.2.1. betula biomass data wrangling ----------------------------------
# Betula-Datensatz filtern und farbige Labels vergeben
betula_wag <- trees_data_bio %>%
  filter(compartiment == "w_agb",
         bot_genus == "Betula",
         bot_species %in% c("pubescens", "spp."),
         min_org == "org") %>%
  mutate(
    ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
    farbe = case_when(
      grepl("tapes", ID, ignore.case = TRUE) ~ "darkorange1",
      peat == "yes" ~ "blue",
      peat == "partly" ~ "turquoise1",
      TRUE ~ "grey50"
    )
  )

# Farbzuordnung
color_map <- setNames(betula_wag$farbe, betula_wag$ID)

# Labels vorbereiten: max DBH und Biomasse je Gruppe
betula_wag_labels <- betula_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat) %>%
  slice_max(DBH_cm, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    country_code = countrycode(country, origin = "country.name", destination = "iso3c"),
    label_name = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", "), ""), country_code)
  ) %>%
  distinct()

# Mittelwerte und Standardabweichungen pro Messpunkt berechnen
m_b_be <- setDT(betula_wag)[, .(B_kg_tree = mean(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_be <- setDT(betula_wag)[, .(sd_B_kg_tree = sd(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]

m_sd_b_be <- m_b_be[sd_b_be, on = .(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_be[, `:=`(
  up_sd_B_kg_tree = B_kg_tree + sd_B_kg_tree,
  low_sd_B_kg_tree = B_kg_tree - sd_B_kg_tree
)]


# Funktion: Loess fitten und letzten Punkt aus der geglätteten Kurve holen
get_smooth_points <- function(data) {
  loess_fit <- loess(B_kg_tree ~ DBH_cm, data = data)
  preds <- predict(loess_fit, se = FALSE)
  df <- data.frame(DBH_cm = data$DBH_cm, B_kg_tree = preds)
  df <- df[order(df$DBH_cm), ]
  # Letzter Punkt der geglätteten Kurve
  return(df[nrow(df), ])
}

# Loess-Punkte für jede ID berechnen
# smooth_points berechnen (wie gehabt)
smooth_points <- betula_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat) %>%
  group_modify(~ get_smooth_points(.x)) %>%
  ungroup() %>%
  mutate(
    label_x = DBH_cm + 10,
    label_y = B_kg_tree + 100
  )

# Labelnamen aus betula_wag_labels dazuholen per Join (nach ID)
smooth_points <- smooth_points %>%
  left_join(
    betula_wag_labels %>% select(ID, label_name),
    by = "ID"
  )


# 1.2.2.2. betula biomass ggplot ------------------------------------------

betula_bio <- ggplot() +
  # Punkte und Loess-Fits pro ID ohne die größten Biomasse-Ausreißer
  geom_point(data = ungroup(betula_wag),
             aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID)) +
  
  geom_smooth(data = ungroup(betula_wag),
              aes(x = DBH_cm, y = B_kg_tree, color = ID, group = ID),
              method = "loess", se = FALSE) +
  
  # Farbskala
  scale_color_manual(values = color_map) +
  
  # Mittelwert-Kurve + SD-Bereiche
  geom_smooth(data = m_sd_b_be, aes(x = DBH_cm, y = B_kg_tree),
              method = "loess", color = "red", se = FALSE) +
  geom_smooth(data = m_sd_b_be, aes(x = DBH_cm, y = low_sd_B_kg_tree),
              method = "loess", color = "red", linetype = "dashed", se = FALSE) +
  geom_smooth(data = m_sd_b_be, aes(x = DBH_cm, y = up_sd_B_kg_tree),
              method = "loess", color = "red", linetype = "dashed", se = FALSE) +
  
  # Labels mit geom_text_repel, rechts oben von den Punkten
ggrepel::geom_text_repel(
    data = smooth_points,
    aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = ID),
    nudge_x = 3,
    nudge_y = 100,
    segment.color = "black",
    segment.size = 0.5,
    segment.linetype = "dotted",
    hjust = 0,
    size = 6,
    max.overlaps = Inf
  ) +
  
  # Achsenlimits und Thema
  ylim(0, 2750) +
  xlim(0, 75) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(hjust = 1)
  ) +
  
  # Achsenbeschriftungen
  xlab("Diameter at breast height [cm]") +
  ylab(expression("Woody aboveground biomass [kg" ~ tree^{-1}*"]"))

print(betula_bio)







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
                                   "farbe" = ifelse(ID %like% c("tapes") , "darkorange1" , # tapes red
                                                    ifelse(peat == "yes",  "blue" , # "#53868B",
                                                           ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                                                  "grey50" ) )))]$farbe

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
x_vals <- as.numeric(factor(unique(alnus_wag$names)))

mean_val <- mean(as.numeric(na.omit(alnus_wag$values)))

alnus_c <- ggplot(data = alnus_wag, 
                  aes(x = as.factor(names), y = as.numeric(values))) +
  stat_boxplot(geom = 'errorbar', width = 0.3,linewidth = 1, aes(color = names)) +
  geom_boxplot(outliers = TRUE,
               outlier.color = "black",
               outlier.fill = "white",
               outlier.shape = 21,
               linewidth = 1, 
               fill = "white",           # weiße Füllung
               aes(color = names)) +     # Farbe für die Umrandung
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_color_manual(values = my.colors) +  # Farben für die Umrandung
  labs(x = "Biomass equation", y = expression("Carbon stock [t "*ha^{-1}*"]")) +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val,
                                 yend = mean_val),
               aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE,
               color = "red", linewidth = 1) +
  geom_point(data = setDT(pseudo_mono_mean_func)[bot_genus %in% "Alnus" &
                                                   compartiment == "w_agb" &
                                                   paper_ID != 9,],
             aes(x = as.factor(names), 
                 y = as.numeric(mean_C_t_ha)), 
             color = "black", size = 3)



alnus_c



# 1.3.2. Betula C stock boxplot-----------------------------------------------------
dev.off()
# subset for boxplot
values <- pseudo_mono_P_SP$C_t_ha[pseudo_mono_P_SP$bot_genus %in% c("Betula") 
                                  & pseudo_mono_P_SP$compartiment == "w_agb"]

names <-  setDT(pseudo_mono_P_SP[pseudo_mono_P_SP$bot_genus %in% c("Betula") 
                                 & pseudo_mono_P_SP$compartiment == "w_agb" 
                                 & pseudo_mono_P_SP$paper_ID !=9,])[, `:=`(
                                   "names" = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code) )]$names

# assign colors: # mark only tapes plot 
farbe <-  setDT(pseudo_mono_P_SP[pseudo_mono_P_SP$bot_genus %in% c("Betula") 
                                 & pseudo_mono_P_SP$compartiment == "w_agb" ,
])[, `:=`(
  "farbe" = ifelse(ID %like% c("tapes") , "darkorange1" , # tapes red
                   ifelse(peat == "yes",  "blue" , # "#53868B",
                          ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                 "grey50" ) )))]$farbe

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
# Werte typisieren
betula_wag <- na.omit(betula_wag)

# x-Positionen für Mittelwertlinie
x_vals <- as.numeric(factor(unique(betula_wag$names)))

# Gesamtmittelwert berechnen
mean_val <- mean(as.numeric(na.omit(betula_wag$values)))

betula_wag$names <- factor(betula_wag$names, levels = unique(betula_wag$names))

# Plot
betula_c <- ggplot(data = betula_wag, 
                   aes(x = names, y = as.numeric(values))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, linewidth = 1, aes(color = names)) +
  geom_boxplot(outliers = TRUE,
               outlier.color = NULL,
               outlier.fill = "white",
               outlier.shape = 8,
               outlier.size = 2,
               linewidth = 1,
               fill = "white",
               aes(color = names)) +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_color_manual(values = my.colors) +
  labs(x = "Biomass equation", 
       y = expression("Carbon stock [t "*ha^{-1}*"]")) +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  # Rote Linie für Gesamtmittelwert
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val,
                                 yend = mean_val),
               aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE,
               color = "red", linewidth = 1) +
  # Schwarze Punkte für Funktionsmittelwerte
  geom_point(data = setDT(pseudo_mono_mean_func)[bot_genus %in% "Betula" &
                                                   compartiment == "w_agb",],
             aes(x = as.factor(names), 
                 y = as.numeric(mean_C_t_ha)), 
             color = "black", size = 3)

# Plot anzeigen
betula_c





# diamneter distribution --------------------------------------------------
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
# Daten filtern
alnus_dbh <- trees_sub[trees_sub$bot_genus == "Alnus", ]

# Breaks vorbereiten
brk <- seq(from = min(alnus_dbh$DBH_cm), to = max(alnus_dbh$DBH_cm) + 1, by = 1)

# ggplot-Histogramm
alnus_hist <-  ggplot(alnus_dbh, aes(x = DBH_cm)) +
  geom_histogram(breaks = brk, color = "black", fill = "white") +
  scale_x_continuous(breaks = seq(0, max(alnus_dbh$DBH_cm) + 10, by = 10)) +
  labs(
    title = "Alnus glutinosa DBH distribution",
    x = "Diameter at breast height [cm]",
    y = "Frequency"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )+
  coord_cartesian(xlim = c(0, 70))



# density
dens_alnus <- density(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"]) 
brk <- seq(from = 0, to = max(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"])+10, by = 1)
hist(trees_sub$DBH_cm[trees_sub$bot_genus == "Alnus"], 
     breaks = brk, 
     main = "Alnus glutinosa DBH distribution",
     xlab = "Diameter at breast height [cm]",
     ylab = "Density", 
     freq = F)
lines(dens_alnus)


# betula diamneter distribution --------------------------------------------------

# frequency
dev.off()
# Daten vorbereiten (optional, falls nicht bereits passiert)
betula_dbh <- trees_sub[trees_sub$bot_genus == "Betula", ]

# Breaks definieren
brk <- seq(from = min(betula_dbh$DBH_cm), to = max(betula_dbh$DBH_cm)+1, by = 1)

# ggplot-Histogramm
betula_hist <- ggplot(betula_dbh, aes(x = DBH_cm)) +
  geom_histogram(breaks = brk, color = "black", fill = "white") +
  scale_x_continuous(breaks = seq(0, max(betula_dbh$DBH_cm) + 20, by = 10)) +
  labs(
    title = "Betula pubescens DBH distribution",
    x = "Diameter at breast height [cm]",
    y = "Frequency"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )+
  coord_cartesian(xlim = c(0, 70))


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


# 2. EXPORT ------------------------------------------------------------------


# 2.1. H ~ DBH min org comparisson  jitter line ---------------------------------------
 # 2.1.1. Alnus H ~ DBH min org comparisson  jitter line ---------------------------------------
  setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/DBH_H_alnus.eps"))
 # plot
 DBH_H_al
 dev.off()
 
 # 2.1.2 Betula  H ~ DBH min org comparisson  jitter line ---------------------------------------
 setEPS()
 postscript(paste0(getwd(), "/output/out_graphs/DBH_H_betula.eps"))
 # plot
 DBH_H_bet
 dev.off()
 
 # 2.1.2 Betula and alnus facet H ~ DBH min org comparisson  jitter line ---------------------------------------
 install.packages("Cairo")
 library(Cairo)
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/DBH_H_betula_alnus.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = DBH_H_plot, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 
 
 

 # 2.2. biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 
 
 # 2.2.1. alnus biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 # 2.1.2 Betula and alnus facet H ~ DBH min org comparisson  jitter line ---------------------------------------
 library(Cairo)
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/alnus_bio.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = alnus_bio, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 

 # 2.2.2. betula biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 
 library(Cairo)
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/betula_bio.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = betula_bio, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 
 
 # 2.3. carbon per hectare ~ equation comparisson by equation boxplot ---------------------------------------
 
 # 2.3.1. alnus carbon per hectare ~ equation comparisson by equation boxplot  ---------------------------------------
 
 library(Cairo)
 
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/alnus_c.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = alnus_c, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 
 # 2.3.2. betula carbon per hectare ~ equation comparisson by equation boxplot  ---------------------------------------
 
 
 library(Cairo)
 
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/betula_c.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = betula_c, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 
 
 
 # 2.4. DBH_cm Histograms ---------------------------------------
 
 # 2.4.1. Alnus  ---------------------------------------
 
 library(Cairo)
 
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/alnus_hist.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = alnus_hist, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 
 # 2.4.2. Betula  ---------------------------------------
 
 
 library(Cairo)
 
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/betula_hist.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = betula_hist, device = cairo_ps, fallback_resolution = 900,
        width = 28, height = 24, units = "cm")
 
 