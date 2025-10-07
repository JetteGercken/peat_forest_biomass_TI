# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# visualisation script

# path to latex for tikz(): # C:\Users\gercken\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe
# options(tikzLatex = "C:/Program Files/texstudio/texstudio.exe")


# 0. setup ----------------------------------------------------------------
library(ggrepel)
library(ggforce)
library(dplyr)
library(countrycode)
library(data.table)

# 0.1 functions -----------------------------------------------------------
#source("//wo-sfs-001v-ew/INSTITUT/a7bze/ZZ_BZE3_Bestand_Auswertung/paper/data/R/peat_forest_biomass_TI/scripts/01_00_functions_library.R")

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
trees_data_bio_min <- read.delim(file = paste0(out.path, "HBI_LT_update_6_3.csv"), sep = ",", dec = ".")




# stock data 
# stockper species per plot in pseudo mono stands
pseudo_mono_P_SP <- read.delim(file = paste0(out.path, "C_stock_ha_pseudo_mono_P_SP.csv"), sep = ",", dec = ".")
pseudo_mono_mean_func <- read.delim(file = paste0(out.path, "C_stock_ha_pseudo_mono_func.csv"), sep = ",", dec = ".")


# stock data min sites
pseudo_mono_P_SP_min <- read.delim(file = paste0(out.path, "C_stock_ha_pseudo_mono_P_SP_min.csv"), sep = ",", dec = ".")
pseudo_mono_mean_func_min <- read.delim(file = paste0(out.path, "C_stock_ha_pseudo_mono_func_min.csv"), sep = ",", dec = ".")


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
      mutate(min_org = ifelse(inv == "momok", "org", min_org))%>% 
      mutate(min_org_label = ifelse(min_org == "min", "mineral", "peat")),
    aes(x = DBH_cm, y = H_m, fill = min_org_label),  # Innenfarbe
    color = "black",                          # Schwarzer Rand
    shape = 21,                              # gefüllter Kreis mit Rand
    alpha = 0.3,
    size = 2
  ) +
  
  # Konfidenzband (fill für Füllfarbe)
  geom_smooth(
    data = trees_height_data %>%
      filter(H_method == "sampled", bot_name %in% c("Betula pubescens", "Alnus glutinosa")) %>%
      mutate(min_org = ifelse(inv == "momok", "org", min_org)) %>% 
      mutate(min_org_label = ifelse(min_org == "min", "mineral", "peat")),
    aes(x = DBH_cm, y = H_m, fill = min_org_label),
    method = "loess", se = F, span = 0.8,
    color = NA, alpha = 0.2
  ) +  
  
  # Modellierte Linien (color)
  geom_line(
    data = trees_height_peterson %>%
      filter(H_method == "sampled", bot_name %in% c("Betula pubescens", "Alnus glutinosa")) %>% 
      mutate(min_org_label = ifelse(min_org == "min", "mineral", "peat")),
    aes(x = DBH_cm, y = H_m_nls, color = min_org_label),
    size = 1.2
  ) +
  
  # Farben anpassen: Achtung, in deinen Daten heißt es "mineral" oder "min"?
  scale_color_manual(values = c("mineral" = "grey30", "peat" = "blue")) +
  scale_fill_manual(values = c("mineral" = "grey30", "peat" = "blue")) +
  
  theme_bw() +
  theme(
    legend.position = "right",
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



##### ORGANIC SITES ---------------------------------------------------------

# 1.2. BIOMASS ------------------------------------------------------------


# 1.2.1. Alnus biomass -------------------------------------------

# 1.2.1.1. alnus biomass data wrangling -----------------------------------
# min data alnus 
alnus_wag_min <- setDT(trees_data_bio_min %>%
                         filter(compartiment == "w_agb",
                                bot_genus == "Alnus",
                                bot_species %in% c("glutinosa"),
                                min_org == "min",
                                paper_ID != 9) %>%
                         mutate(
                           ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
                           farbe = case_when(
                             grepl("tapes", ID, ignore.case = TRUE) ~ "darkorange1",
                             peat == "yes" ~ "blue",
                             peat == "partly" ~ "turquoise2",
                             TRUE ~ "grey50"
                           )
                         ) )
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
# B_kg_tree = mean of tree,  sd_B_kg_tree up_sd_B_kg_tree low_sd_B_kg_tree 0 standard deviation of trees biomass 
m_b_al <- setDT(alnus_wag)[ID != "9_w_agb", .(B_kg_tree = mean(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_al <- setDT(alnus_wag)[ID != "9_w_agb", .(sd_B_kg_tree = sd(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
# calculate mean biomass for mineral sites per tree across all equations
m_b_al_min <- setDT(alnus_wag_min)[ID != "9_w_agb", .(B_kg_tree_min = mean(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
# join means and sds together
m_sd_b_al <- m_b_al[sd_b_al, on = .(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_al <- m_sd_b_al %>% full_join(., m_b_al_min, by = c("plot_ID", "tree_ID", "compartiment", "DBH_cm"))

setDT(m_sd_b_al)[, `:=`(
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





###########


# Kategorien definieren
alnus_wag <- alnus_wag %>%
  mutate(Category = case_when(
    ID == "41_tapes" ~ "Riedel & Kändler\n(2017, TapeS)",
    ID == "40_w_agb" ~ "peatland",
    ID == "32_w_agb" ~ "mixed",
    ID %in% c("14_w_agb", "23_4", "23_5","24_1", "27_w_agb", "28_w_agb", "39_w_agb") ~ "mineral"
  ))

smooth_points <- smooth_points %>%
  mutate(Category = case_when(
    ID == "41_tapes" ~ "Riedel & Kändler\n(2017, TapeS)",
    ID == "40_w_agb" ~ "peatland",
    ID == "32_w_agb" ~ "mixed",
    ID %in% c("14_w_agb", "23_4", "23_5", "24_1", "27_w_agb", "28_w_agb", "39_w_agb") ~ "mineral"
  ))

# Kategorie als Faktor mit gewünschter Reihenfolge
alnus_wag <- alnus_wag %>%
  mutate(Category = factor(Category, levels = c(
    "Riedel & Kändler\n(2017, TapeS)",
    "peatland",
    "mixed",
    "mineral"
  )))

smooth_points <- smooth_points %>%
  mutate(Category = factor(Category, levels = c(
    "Riedel & Kändler\n(2017, TapeS)",
    "peatland",
    "mixed",
    "mineral"
  )))

# Farben für die Kategorien
category_colors <- c(
  "Riedel & Kändler\n(2017, TapeS)" = "darkorange1",
  "peatland" = "blue",
  "mixed" = "turquoise2",
  "mineral" = "grey50"
)


smooth_points <- smooth_points %>%
  mutate(label_name = case_when(
    label_name == "14, USA"        ~ "14, USA **",
    label_name == "23, 4, LVA"     ~ "23, 4, LVA",
    label_name == "23, 5, LVA"     ~ "23, 5, LVA",
    label_name == "27, ESP"        ~ "27, ESP *",
    label_name == "28, TUR"        ~ "28, TUR *",
    label_name == "32, SWE"        ~ "32, SWE",
    label_name == "39, POL"        ~ "39, POL",
    label_name == "40, POL"        ~ "40, POL",
    label_name == "41, tapes, DEU" ~ "41, DEU",
    label_name == "24, 1, GBR"     ~ "24, 1, GBR ***" , 
    TRUE ~ label_name  # alles andere unverändert lassen
  ))



alnus_bio <- ggplot() +
  geom_point(
    data = filter(alnus_wag, ID != "9_w_agb"),
    aes(x = DBH_cm, y = B_kg_tree, color = Category)
  ) +
  geom_smooth(
    data = filter(alnus_wag, ID != "9_w_agb"),
    aes(x = DBH_cm, y = B_kg_tree, color = Category, group = ID),
    method = "loess", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_al[!is.na(m_sd_b_al$B_kg_tree), ],
    aes(x = DBH_cm, y = B_kg_tree, linetype = "overall mean"),
    method = "loess", color = "red", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_al[!is.na(m_sd_b_al$B_kg_tree), ],
    aes(x = DBH_cm, y = low_sd_B_kg_tree, linetype = "± overall SD"),
    method = "loess", color = "red", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_al[!is.na(m_sd_b_al$B_kg_tree), ],
    aes(x = DBH_cm, y = up_sd_B_kg_tree, linetype = "± overall SD"),
    method = "loess", color = "red", se = FALSE
  ) +
  # geom_smooth(
  #   data = m_sd_b_al[!is.na(m_sd_b_al$B_kg_tree_min), ],
  #   aes(x = DBH_cm, y = B_kg_tree_min, linetype = "overal mean mineral"),
  #   method = "loess", color = "black", se = FALSE
  # ) +
  scale_linetype_manual(
    values = c("overall mean" = "solid"
               , "± overall SD" = "dashed"
    ),
    name = ""
  ) +
  # scale_linetype_manual(
  #   values = c("overall mean peat" = "solid"
  #              , "± overall SD" = "dashed"
  #              , "overal mean mineral" = "solid"
  #              ),
  #   name = ""
  # ) +
  scale_color_manual(
    values = category_colors,
    name = ""
  ) +
  geom_text_repel(
    data = smooth_points,
    aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = Category),
    nudge_x = 3,
    nudge_y = 100,
    segment.color = "black",
    segment.size = 0.5,
    segment.linetype = "dotted",
    hjust = 0,
    size = 6,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  ylim(0, 2750) +
  xlim(0, 75) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_text(),
    axis.text.x = element_text(hjust = 1)
  ) +
  xlab("Diameter at breast height [cm]") +
  ylab("Woody aboveground biomass [kg"~tree^{-1}*"]")

print(alnus_bio)






# 1.2.2.1. betula biomass data wrangling ----------------------------------
# avbovegroun biomass of betula trees in kg by diameter
betula_wag_min <- trees_data_bio_min %>%
  filter(compartiment == "w_agb",
         bot_genus == "Betula",
         bot_species %in% c("pubescens"),
         min_org == "min") %>%
  mutate(
    ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
    farbe = case_when(
      grepl("tapes", ID, ignore.case = TRUE) ~ "darkorange1",
      peat == "yes" ~ "blue",
      peat == "partly" ~ "turquoise1",
      TRUE ~ "grey50"
    )
  )
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

# calculate mean biomass for mineral sites per tree across all equations
m_b_be_min <- setDT(betula_wag_min)[, .(B_kg_tree_min = mean(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
# join means and sds together
m_sd_b_be <- m_b_be[sd_b_be, on = .(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_be <- m_sd_b_be %>% full_join(., m_b_be_min, by = c("plot_ID", "tree_ID", "compartiment", "DBH_cm"))

setDT(m_sd_b_be)[, `:=`(
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



# Kategorien definieren für Betula
betula_wag <- betula_wag %>%
  mutate(Category = case_when(
    ID == "41_tapes" ~ "Riedel & Kändler\n(2017, TapeS)",
    ID %in% c("11_2","19_w_agb","20_1") ~ "peatland",
    ID %in% c("26_w_agb") ~ "mixed",
    TRUE ~ "mineral"
  ))

smooth_points <- smooth_points %>%
  mutate(Category = case_when(
    label_name %in% c("11, 2, FIN","19, FIN","20, 1, FIN") ~ "peatland",
    label_name %in% c("26, FIN") ~ "mixed",
    label_name == "41, tapes, DEU" ~ "Riedel & Kändler\n(2017, TapeS)",
    TRUE ~ "mineral"
  ))

# Factor-Level-Reihenfolge für Legende
betula_wag <- betula_wag %>%
  mutate(Category = factor(Category, levels = c(
    "Riedel & Kändler\n(2017, TapeS)",
    "mixed",
    "peatland",
    "mineral"
  )))

smooth_points <- smooth_points %>%
  mutate(Category = factor(Category, levels = c(
    "Riedel & Kändler\n(2017, TapeS)",
    "mixed",
    "peatland",
    "mineral"
  )))



smooth_points <- smooth_points %>%
  mutate(label_name = case_when(
    label_name == "1, 1, DEU"       ~ "1, 1, DEU **",
    label_name == "1, 2, DEU"       ~ "1, 2, DEU **",
    label_name == "6, NOR"          ~ "6, NOR ***",
    label_name == "7, EST"          ~ "7, EST",
    label_name == "8, ESP"          ~ "8, ESP",
    label_name == "10, ISL"         ~ "10, ISL ***",
    label_name == "11, 2, FIN"      ~ "11, 2, FIN ***",   # optional mit Stern markieren
    label_name == "15, SWE"         ~ "15, SWE",
    label_name == "19, FIN"         ~ "19, FIN",      # optional mit Stern markieren
    label_name == "20, 1, FIN"      ~ "20, 1, FIN",   # optional mit Stern markieren
    label_name == "26, FIN"         ~ "26, FIN",     # optional mit zwei Sternen
    label_name == "29, 11, NOR"         ~ "29, 11, NOR ***",
    label_name == "29, 12, NOR"         ~ "29, 12, NOR",
    label_name == "41, tapes, DEU" ~ "41, DEU",
    TRUE ~ label_name
  ))



# 2. Plot erstellen
betula_bio <- ggplot() +
  geom_point(
    data = filter(betula_wag, !ID %in% c("NA")),  # ggf. Outlier ausschließen
    aes(x = DBH_cm, y = B_kg_tree, color = Category)
  ) +
  geom_smooth(
    data = filter(betula_wag, !ID %in% c("NA")),
    aes(x = DBH_cm, y = B_kg_tree, color = Category, group = ID),
    method = "loess", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_be[!is.na(m_sd_b_be$B_kg_tree), ],
    aes(x = DBH_cm, y = B_kg_tree, linetype = "overall mean"),
    method = "loess", color = "red", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_be[!is.na(m_sd_b_be$B_kg_tree), ],
    aes(x = DBH_cm, y = low_sd_B_kg_tree, linetype = "± overall SD"),
    method = "loess", color = "red", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_be[!is.na(m_sd_b_be$B_kg_tree), ],
    aes(x = DBH_cm, y = up_sd_B_kg_tree, linetype = "± overall SD"),
    method = "loess", color = "red", se = FALSE
  ) +
  #  geom_smooth(
  #   data = m_sd_b_be[!is.na(m_sd_b_be$B_kg_tree_min), ],
  #   aes(x = DBH_cm, y = B_kg_tree_min, linetype = "overal mean mineral"),
  #   method = "loess", color = "black", se = FALSE
  # ) +
  scale_linetype_manual(
    values = c("overall mean" = "solid"
               , "± overall SD" = "dashed"
    ),
    name = ""
  ) +
  # scale_linetype_manual(
  #   values = c("overall mean peat" = "solid"
  #              , "± overall SD" = "dashed"
  #              , "overal mean mineral" = "solid"
  #   ),
  #   name = ""
  # ) +
  scale_color_manual(
    values = category_colors,
    name = ""
  ) +
  geom_text_repel(
    data = smooth_points,
    aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = Category),
    nudge_x = 3,
    nudge_y = 100,
    segment.color = "black",
    segment.size = 0.5,
    segment.linetype = "dotted",
    hjust = 0,
    size = 6,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  ylim(0, 2750) +
  xlim(0, 75) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(hjust = 1)
  ) +
  xlab("Diameter at breast height [cm]") +
  ylab("Woody aboveground biomass [kg"~tree^{-1}*"]")

print(betula_bio)



#### this doesnt work
# 1.2.3. facet wrap trial for bio alnus betula together --------------------------
# 1.2.3. biomass data wrangling -----------------------------------
# Datensatz filtern
bet_aln_wag <- trees_data_bio %>%
  filter(compartiment == "w_agb",
         bot_genus %in% c("Betula", "Alnus"),
         bot_species %in% c("pubescens", "glutinosa", "spp."),
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
color_map <- setNames(bet_aln_wag$farbe, bet_aln_wag$ID)

# Labels vorbereiten: max DBH und Biomasse je Gruppe
bet_aln_wag_labels <- bet_aln_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat, bot_genus) %>%
  slice_max(DBH_cm, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(
    country_code = countrycode(country, origin = "country.name", destination = "iso3c"),
    label_name = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", "), ""), country_code)
  ) %>%
  distinct()


# Mittelwerte und Standardabweichungen pro Messpunkt berechnen
m_b_be <- setDT(bet_aln_wag)[, .(B_kg_tree = mean(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_be <- setDT(bet_aln_wag)[, .(sd_B_kg_tree = sd(B_kg_tree, na.rm = TRUE)), by = .(plot_ID, tree_ID, compartiment, DBH_cm)]

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
smooth_points <- bet_aln_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat, bot_genus) %>%
  group_modify(~ get_smooth_points(.x)) %>%
  ungroup() %>%
  mutate(
    label_x = DBH_cm + 10,
    label_y = B_kg_tree + 100
  )

# Labelnamen aus betula_wag_labels dazuholen per Join (nach ID)
smooth_points <- smooth_points %>%
  left_join(
    bet_aln_wag_labels %>% select(ID, label_name, bot_genus),
    by = c("ID", "bot_genus")
  )





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
smooth_points <- bet_aln_wag %>%
  group_by(paper_ID, func_ID, ID, country, peat, bot_genus) %>% # we can group by bot genus because we have 
  # already filtered all trees for alnus and betula at org stands
  group_modify(~ get_smooth_points(.x)) %>%
  ungroup() %>%
  mutate(
    label_x = DBH_cm + 10,
    label_y = B_kg_tree + 100
  )



# Labelnamen aus betula_wag_labels dazuholen per Join (nach ID)
smooth_points <- smooth_points %>%
  left_join(
    bet_aln_wag_labels %>% select(ID, label_name, bot_genus),
    by = c("ID", "bot_genus")
  )




# Kategorien definieren
bet_aln_wag <- bet_aln_wag %>%
  mutate(Category = case_when(
    ID == "41_tapes" ~ "Riedel & Kändler\n(2017, TapeS)",
    ID %in% c("11_2","19_w_agb","20_1", "40_w_agb") ~ "peatland",
    ID %in% c("26_w_agb", "32_w_agb" ) ~ "mixed",
    #ID %in% c("14_w_agb", "23_4", "23_5","24_1", "27_w_agb", "28_w_agb", "39_w_agb") ~ "mineral", 
    TRUE ~ "mineral"
  ))



smooth_points <- smooth_points %>%
  mutate(Category = case_when(
    label_name %in% c("11, 2, FIN","19, FIN","20, 1, FIN") | 
      ID == "40_w_agb" ~ "peatland",
    label_name %in% c("26, FIN") |   
      ID == "32_w_agb"  ~ "mixed",
    label_name == "41, tapes, DEU" |  
      ID == "41_tapes" ~ "Riedel & Kändler\n(2017, TapeS)",
    ID %in% c("14_w_agb", "23_4", "23_5", 
    "24_1", "27_w_agb", "28_w_agb", "39_w_agb")~ "mineral", 
    TRUE ~ "mineral",
  ))




# Kategorie als Faktor mit gewünschter Reihenfolge
bet_aln_wag <- bet_aln_wag %>%
  mutate(Category = factor(Category, levels = c(
    "Riedel & Kändler\n(2017, TapeS)",
    "peatland",
    "mixed",
    "mineral"
  )))

smooth_points <- smooth_points %>%
  mutate(Category = factor(Category, levels = c(
    "Riedel & Kändler\n(2017, TapeS)",
    "peatland",
    "mixed",
    "mineral"
  )))

# Farben für die Kategorien
category_colors <- c(
  "Riedel & Kändler\n(2017, TapeS)" = "darkorange1",
  "peatland" = "blue",
  "mixed" = "turquoise2",
  "mineral" = "grey50"
)


smooth_points <- smooth_points %>%
  mutate(label_name = case_when(
    label_name == "1, 1, DEU"       ~ "1, 1, DEU **",
    label_name == "1, 2, DEU"       ~ "1, 2, DEU **",
    label_name == "6, NOR"          ~ "6, NOR ***",
    label_name == "7, EST"          ~ "7, EST",
    label_name == "8, ESP"          ~ "8, ESP",
    label_name == "10, ISL"         ~ "10, ISL ***",
    label_name == "11, 2, FIN"      ~ "11, 2, FIN ***",   # optional mit Stern markieren
    label_name == "15, SWE"         ~ "15, SWE",
    label_name == "19, FIN"         ~ "19, FIN",      # optional mit Stern markieren
    label_name == "20, 1, FIN"      ~ "20, 1, FIN",   # optional mit Stern markieren
    label_name == "26, FIN"         ~ "26, FIN",     # optional mit zwei Sternen
    label_name == "29, 11, NOR"         ~ "29, 11, NOR ***",
    label_name == "29, 12, NOR"         ~ "29, 12, NOR",
    label_name == "41, tapes, DEU" ~ "41, DEU",
    label_name == "14, USA"        ~ "14, USA **",
    label_name == "23, 4, LVA"     ~ "23, 4, LVA",
    label_name == "23, 5, LVA"     ~ "23, 5, LVA",
    label_name == "27, ESP"        ~ "27, ESP *",
    label_name == "28, TUR"        ~ "28, TUR *",
    label_name == "32, SWE"        ~ "32, SWE",
    label_name == "39, POL"        ~ "39, POL",
    label_name == "40, POL"        ~ "40, POL",
    label_name == "41, tapes, DEU" ~ "41, DEU",
    label_name == "24, 1, GBR"     ~ "24, 1, GBR ***" , 
    TRUE ~ label_name  # alles andere unverändert lassen
  ))

# 1.3.1. . Plot erstellen
betula_alnus_bio <- ggplot() +
  geom_point(
    data = filter(bet_aln_wag, !ID %in% c("NA")),  # ggf. Outlier ausschließen
    aes(x = DBH_cm, y = B_kg_tree, color = Category)
  ) +
  geom_smooth(
    data = filter(bet_aln_wag, !ID %in% c("NA")),
    aes(x = DBH_cm, y = B_kg_tree, color = Category, group = ID),
    method = "loess", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_be,
    aes(x = DBH_cm, y = B_kg_tree, linetype = "overall mean"),
    method = "loess", color = "red", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_be,
    aes(x = DBH_cm, y = low_sd_B_kg_tree, linetype = "± overall SD"),
    method = "loess", color = "red", se = FALSE
  ) +
  geom_smooth(
    data = m_sd_b_be,
    aes(x = DBH_cm, y = up_sd_B_kg_tree, linetype = "± overall SD"),
    method = "loess", color = "red", se = FALSE
  ) +
  scale_color_manual(values = category_colors, name = "") +
  scale_linetype_manual(
    values = c("overall mean" = "solid", "± overall SD" = "dashed"),
    name = ""
  ) +
  facet_wrap(~bot_genus)+ 
  geom_text_repel(
    data = smooth_points,
    aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = Category),
    nudge_x = 3,
    nudge_y = 100,
    segment.color = "black",
    segment.size = 0.5,
    segment.linetype = "dotted",
    hjust = 0,
    size = 6,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  ylim(0, 2750) +
  xlim(0, 75) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(hjust = 1)
  ) +
  xlab("Diameter at breast height [cm]") +
  ylab("Woody aboveground biomass [kg"~tree^{-1}*"]")
  

print(betula_alnus_bio)







#### part above doesnt work


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

# change names
alnus_wag$names <- gsub(", tapes", "", alnus_wag$names)

# gleiche Namensanpassung auch hier
pseudo_mono_mean_func$names <- gsub(", tapes", "", pseudo_mono_mean_func$names)

# Letters to annotate
# letters_vec <- c("b", "ab", "ab", "a", "ab", "ab", "ab", "ab", "ab")
 #              27_w_agb 40_w_agb  39_w_agb  23_4     23_5     42_tapes    mean  32_w_agb.  28_w_agb.  14_w_agb     24_1 
letters_vec <- c("a",     "ab" ,   "abc" ,   "abc" ,   "abc",    "abc" ,   #"abc" 
                  "abc" ,   "abc" ,    "bc" ,     "c")

# Create a data frame for annotation
annot_df <- data.frame(
  names = unique(alnus_wag$names),  # same order as x-axis
  letters = letters_vec,
  y_pos = max(as.numeric(alnus_wag$values), na.rm = TRUE) + 55  # slightly above boxes
)


x_vals <- as.numeric(factor(unique(alnus_wag$names)))

mean_val <- mean(as.numeric(na.omit(alnus_wag$values)))
mean_val_min <- mean(as.numeric(na.omit(pseudo_mono_P_SP_min$C_t_ha[pseudo_mono_P_SP_min$bot_genus %in% c("Alnus") 
                                        & pseudo_mono_P_SP_min$compartiment == "w_agb"
                                        & pseudo_mono_P_SP_min$paper_ID !=9])))

# deine Daten vorbereiten ----
alnus_wag$farbe_cat <- factor(alnus_wag$farbe,
                              levels = c("darkorange1", "red","black", "blue", "turquoise1", "grey50"),
                              labels = c("Riedel & Kändler\n(2017, TapeS)", "overall mean", "overall mean mineral",  "peatland", "peatland & mineral", "mineral"))


# Boxplot mit ggplot ----
alnus_c <- ggplot(data = alnus_wag, 
                  aes(x = as.factor(names), y = as.numeric(values))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, linewidth = 1, aes(color = farbe_cat)) +
  geom_boxplot(outliers = TRUE,
               outlier.color = "black",
               outlier.fill = "white",
               outlier.shape = 8,
               linewidth = 1, 
               fill = "white",
               aes(color = farbe_cat)) + 
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_color_manual(
    values = c("Riedel & Kändler\n(2017, TapeS)" = "darkorange1",
               "overall mean" = "red",
               "overall mean mineral" = "black",
               "peatland" = "blue",
               "peatland & mineral" = "turquoise1",
               "mineral" = "grey50")
  ) +
  labs(x = "Biomass equation", y = expression("Carbon stock [t "*ha^{-1}*"]"), 
       color = "Legend") +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  # Linie für Gesamtmittel
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val,
                                 yend = mean_val,
                                 farbe_cat = "overall mean"),
               aes(x = x, xend = xend, y = y, yend = yend, color = farbe_cat),
               inherit.aes = FALSE,
               linewidth = 1) +
  # linie für gesamtmittel mineral
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val_min,
                                 yend = mean_val_min,
                                 farbe_cat = "overall mean mineral"),
               aes(x = x, xend = xend, y = y, yend = yend, color = farbe_cat),
               inherit.aes = FALSE,
               linewidth = 1) +
  # Punkte für Funktionsmittel
  geom_point(
    data = setDT(pseudo_mono_mean_func)[bot_genus %in% "Alnus" &
                                          compartiment == "w_agb" &
                                          paper_ID != 9, ],
    aes(x = as.factor(names), 
        y = as.numeric(mean_C_t_ha)), 
    color = "black", size = 3
  ) + 
  geom_text(
    data = annot_df,
    aes(x = names, y = y_pos, label = letters),
    inherit.aes = FALSE,
    size = 8
  )


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
betula_wag <- betula_wag[!betula_wag$names %in% c("NA, NANA"), ]

#change names
betula_wag$names <- gsub(", tapes", "", betula_wag$names)

# gleiche Namensanpassung auch hier
pseudo_mono_mean_func$names <- gsub(", tapes", "", pseudo_mono_mean_func$names)

# Letters to annotate
# letters_vec <- c("bcd", "bcd", "d", "bcd", "ab", "d", "cd", "bc", "ab", "ab", "bcd", "a", "bc")
#.              20_1 19_w_agb  8_w_agb 42_tapes 15_w_agb  7_w_agb    29_12     mean 
letters_vec <- c("a"  ,"a"      ,"a"    , "ab"     ,"ab"   , "abc"    ,"abc"   # ,"abc" 
                 # 26_w_agb      1_2      1_1    29_11     11_2 10_w_agb  6_w_agb 
                 , "abc",    "abc"  ,  "abc" ,   "abc" ,    "bc" ,     "c"  ,    "c" )

# Create a data frame for annotation
annot_df <- data.frame(
  names = unique(betula_wag$names),  # same order as x-axis
  letters = letters_vec,
  y_pos = max(as.numeric(betula_wag$values), na.rm = TRUE) + 20  # slightly above boxes
)

# x-Positionen für Mittelwertlinie
x_vals <- as.numeric(factor(unique(betula_wag$names)))

# Gesamtmittelwert berechnen
mean_val <- mean(as.numeric(na.omit(betula_wag$values)))
# mean mineral sites c stock 
mean_val_min <- mean(as.numeric(na.omit(pseudo_mono_P_SP_min$C_t_ha[pseudo_mono_P_SP_min$bot_genus %in% c("Betula") 
                                                                    & pseudo_mono_P_SP_min$compartiment == "w_agb"])))

# deine Daten vorbereiten ----
betula_wag$farbe_cat <- factor(betula_wag$farbe,
                              levels = c("darkorange1", "red","black" ,"blue", "turquoise1", "grey50"),
                              labels = c("Riedel & Kändler\n(2017, TapeS)", "overall mean", "overall mean mineral", "peatland", "peatland & mineral", "mineral"))


betula_wag$names <- factor(betula_wag$names, levels = unique(betula_wag$names))



betula_c <- ggplot(data = betula_wag, aes(x = names, y = as.numeric(values))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, linewidth = 1, aes(color = farbe_cat)) +
  geom_boxplot(outliers = TRUE,
               outlier.fill = "white",
               outlier.size = 2,
               outlier.shape = 8,
               linewidth = 1,
               fill = "white",
               aes(color = farbe_cat)) +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_color_manual(values = c("Riedel & Kändler\n(2017, TapeS)" = "darkorange1",
                                "overall mean" = "red",
                                "overall mean mineral" = "black",
                                "peatland" = "blue",
                                "peatland & mineral" = "turquoise1",
                                "mineral" = "grey50")) +
  labs(x = "Biomass equation", y = expression("Carbon stock [t "*ha^{-1}*"]"), color = "Legend") +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  # Linie für Gesamtmittelwert
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val,
                                 yend = mean_val,
                                 farbe_cat = "overall mean"),
               aes(x = x, xend = xend, y = y, yend = yend, color = farbe_cat),
               inherit.aes = FALSE,
               linewidth = 1) +
  # Linie für Gesamtmittelwert mineral
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val_min,
                                 yend = mean_val_min,
                                 farbe_cat = "overall mean mineral"),
               aes(x = x, xend = xend, y = y, yend = yend, color = farbe_cat),
               inherit.aes = FALSE,
               linewidth = 1) +
  # Punkte für Funktionsmittelwerte
  geom_point(data = setDT(pseudo_mono_mean_func)[bot_genus %in% "Betula" & compartiment == "w_agb", ],
             aes(x = names, y = as.numeric(mean_C_t_ha)), color = "black", size = 3) +
  # Buchstaben über Boxplots
  geom_text(data = annot_df, aes(x = names, y = y_pos, label = letters),
            inherit.aes = FALSE, size = 8)

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





#### MINERAL SITES -----------------------------------------------------------
# 1.3. CARBON STOCK -------------------------------------------------------
# carbon stock if national carbon inventory (CI 2017): 
# total carbon: 52859
# aboveground carbon: 41811 kg/ ha

# assign names to carbon stock hectar summery
setDT(pseudo_mono_mean_func_min)[, `:=`( "names" = paste0(pseudo_mono_mean_func_min$paper_ID, ", ", 
                                                      ifelse(pseudo_mono_mean_func_min$func_ID != "w_agb", paste0(pseudo_mono_mean_func_min$func_ID, ", ") , ""), 
                                                      countrycode(pseudo_mono_mean_func_min$country, origin = 'country.name', destination = 'iso3c')) )]


# this is for outlayer plotting ggplot
n_fun <- function(x){
  return(data.frame(y = 0.95*70,
                    label = ""))
}

# 1.3.1. Alnus carbon stock -----------------------------------------------

# subset for boxplot

# pseudo_mono_P_SP species are already filtered into alnus glutinosa ans betula pubescens and additionally alsnus or betula spp. for org plots
values <- pseudo_mono_P_SP_min$C_t_ha[pseudo_mono_P_SP_min$bot_genus %in% c("Alnus") 
                                  & pseudo_mono_P_SP_min$compartiment == "w_agb"
                                  & pseudo_mono_P_SP_min$paper_ID !=9]

names <-  setDT(pseudo_mono_P_SP_min[pseudo_mono_P_SP_min$bot_genus %in% c("Alnus") 
                                 & pseudo_mono_P_SP_min$compartiment == "w_agb" 
                                 & pseudo_mono_P_SP_min$paper_ID !=9,])[, `:=`(
                                   "names" = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code) )]$names

# assign colors: # mark only tapes plot 
farbe <-  setDT(pseudo_mono_P_SP_min[pseudo_mono_P_SP_min$bot_genus %in% c("Alnus") 
                                 & pseudo_mono_P_SP_min$compartiment == "w_agb" 
                                 & pseudo_mono_P_SP_min$paper_ID !=9,])[, `:=`(
                                   "farbe" = ifelse(ID %like% c("tapes") , "darkorange1" , # tapes red
                                                    ifelse(peat == "yes",  "blue" , # "#53868B",
                                                           ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                                                  "grey50" ) )))]$farbe

# bind colors, names and values together 
alnus_wag <- as.data.frame(cbind(names, values, farbe))

color_map <- setNames(alnus_wag$farbe, alnus_wag$ID)

# mark only tapes plot
my.colors <- unique(alnus_wag[,c("names", "farbe")])$farbe


# 1.3.1.2. ggplot boxplot alnus c stock ha  -------------------------------

# change names
alnus_wag$names <- gsub(", tapes", "", alnus_wag$names)

# gleiche Namensanpassung auch hier
pseudo_mono_mean_func_min$names <- gsub(", tapes", "", pseudo_mono_mean_func_min$names)

# Letters to annotate
# letters_vec <- c("b", "ab", "ab", "a", "ab", "ab", "ab", "ab", "ab")
#              27_w_agb 40_w_agb  39_w_agb  23_4     23_5     42_tapes    mean  32_w_agb.  28_w_agb.  14_w_agb     24_1 
letters_vec <- c("a",     "ab" ,   "abc" ,   "abc" ,   "abc",    "abc" ,   #"abc" 
                 "abc" ,   "abc" ,    "bc" ,     "c")

# Create a data frame for annotation
annot_df <- data.frame(
  names = unique(alnus_wag$names),  # same order as x-axis
  letters = letters_vec,
  y_pos = max(as.numeric(alnus_wag$values), na.rm = TRUE) + 55  # slightly above boxes
)


x_vals <- as.numeric(factor(unique(alnus_wag$names)))

mean_val <- mean(as.numeric(na.omit(alnus_wag$values)))

# deine Daten vorbereiten ----
alnus_wag$farbe_cat <- factor(alnus_wag$farbe,
                              levels = c("darkorange1", "red", "blue", "turquoise1", "grey50"),
                              labels = c("Riedel & Kändler\n(2017, TapeS)", "overall mean", "peatland", "peatland & mineral", "mineral"))


# Boxplot mit ggplot ----
alnus_c <- ggplot(data = alnus_wag, 
                  aes(x = as.factor(names), y = as.numeric(values))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, linewidth = 1, aes(color = farbe_cat)) +
  geom_boxplot(outliers = TRUE,
               outlier.color = "black",
               outlier.fill = "white",
               outlier.shape = 8,
               linewidth = 1, 
               fill = "white",
               aes(color = farbe_cat)) + 
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_color_manual(
    values = c("Riedel & Kändler\n(2017, TapeS)" = "darkorange1",
               "overall mean" = "red",
               "peatland" = "blue",
               "peatland & mineral" = "turquoise1",
               "mineral" = "grey50")
  ) +
  labs(x = "Biomass equation", y = expression("Carbon stock [t "*ha^{-1}*"]"), 
       color = "Legend") +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  # Linie für Gesamtmittel
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val,
                                 yend = mean_val,
                                 farbe_cat = "overall mean"),
               aes(x = x, xend = xend, y = y, yend = yend, color = farbe_cat),
               inherit.aes = FALSE,
               linewidth = 1) +
  # Punkte für Funktionsmittel
  geom_point(
    data = setDT(pseudo_mono_mean_func)[bot_genus %in% "Alnus" &
                                          compartiment == "w_agb" &
                                          paper_ID != 9, ],
    aes(x = as.factor(names), 
        y = as.numeric(mean_C_t_ha)), 
    color = "black", size = 3
  ) + 
  geom_text(
    data = annot_df,
    aes(x = names, y = y_pos, label = letters),
    inherit.aes = FALSE,
    size = 8
  )


alnus_c


# 1.3.2. Betula C stock boxplot-----------------------------------------------------
dev.off()
# subset for boxplot
values <- pseudo_mono_P_SP_min$C_t_ha[pseudo_mono_P_SP_min$bot_genus %in% c("Betula") 
                                  & pseudo_mono_P_SP_min$compartiment == "w_agb"]

names <-  setDT(pseudo_mono_P_SP_min[pseudo_mono_P_SP_min$bot_genus %in% c("Betula") 
                                 & pseudo_mono_P_SP_min$compartiment == "w_agb" 
                                 & pseudo_mono_P_SP_min$paper_ID !=9,])[, `:=`(
                                   "names" = paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code) )]$names

# assign colors: # mark only tapes plot 
farbe <-  setDT(pseudo_mono_P_SP_min[pseudo_mono_P_SP_min$bot_genus %in% c("Betula") 
                                 & pseudo_mono_P_SP_min$compartiment == "w_agb" ,
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



# ggplot boxplot mean c stock per  function -------------------------------
# Werte typisieren
betula_wag <- betula_wag[!betula_wag$names %in% c("NA, NANA"), ]

#change names
betula_wag$names <- gsub(", tapes", "", betula_wag$names)

# gleiche Namensanpassung auch hier
pseudo_mono_mean_func$names <- gsub(", tapes", "", pseudo_mono_mean_func_min$names)

# Letters to annotate
# letters_vec <- c("bcd", "bcd", "d", "bcd", "ab", "d", "cd", "bc", "ab", "ab", "bcd", "a", "bc")
#.              20_1 19_w_agb  8_w_agb 42_tapes 15_w_agb  7_w_agb    29_12     mean 
letters_vec <- c("a"  ,"a"      ,"a"    , "ab"     ,"ab"   , "abc"    ,"abc"   # ,"abc" 
                 # 26_w_agb      1_2      1_1    29_11     11_2 10_w_agb  6_w_agb 
                 , "abc",    "abc"  ,  "abc" ,   "abc" ,    "bc" ,     "c"  ,    "c" )

# Create a data frame for annotation
annot_df <- data.frame(
  names = unique(betula_wag$names),  # same order as x-axis
  letters = letters_vec,
  y_pos = max(as.numeric(betula_wag$values), na.rm = TRUE) + 20  # slightly above boxes
)

# x-Positionen für Mittelwertlinie
x_vals <- as.numeric(factor(unique(betula_wag$names)))

# Gesamtmittelwert berechnen
mean_val <- mean(as.numeric(na.omit(betula_wag$values)))

# deine Daten vorbereiten ----
betula_wag$farbe_cat <- factor(betula_wag$farbe,
                               levels = c("darkorange1", "red", "blue", "turquoise1", "grey50"),
                               labels = c("Riedel & Kändler\n(2017, TapeS)", "overall mean", "peatland", "peatland & mineral", "mineral"))


betula_wag$names <- factor(betula_wag$names, levels = unique(betula_wag$names))



betula_c <- ggplot(data = betula_wag, aes(x = names, y = as.numeric(values))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, linewidth = 1, aes(color = farbe_cat)) +
  geom_boxplot(outliers = TRUE,
               outlier.fill = "white",
               outlier.size = 2,
               outlier.shape = 8,
               linewidth = 1,
               fill = "white",
               aes(color = farbe_cat)) +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_color_manual(values = c("Riedel & Kändler\n(2017, TapeS)" = "darkorange1",
                                "overall mean" = "red",
                                "peatland" = "blue",
                                "peatland & mineral" = "turquoise1",
                                "mineral" = "grey50")) +
  labs(x = "Biomass equation", y = expression("Carbon stock [t "*ha^{-1}*"]"), color = "Legend") +
  coord_cartesian(ylim = c(0, 275.1831)) +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  # Linie für Gesamtmittelwert
  geom_segment(data = data.frame(x = min(x_vals) - 0.38,
                                 xend = max(x_vals) + 0.38,
                                 y = mean_val,
                                 yend = mean_val,
                                 farbe_cat = "overall mean"),
               aes(x = x, xend = xend, y = y, yend = yend, color = farbe_cat),
               inherit.aes = FALSE,
               linewidth = 1) +
  # Punkte für Funktionsmittelwerte
  geom_point(data = setDT(pseudo_mono_mean_func)[bot_genus %in% "Betula" & compartiment == "w_agb", ],
             aes(x = names, y = as.numeric(mean_C_t_ha)), color = "black", size = 3) +
  # Buchstaben über Boxplots
  geom_text(data = annot_df, aes(x = names, y = y_pos, label = letters),
            inherit.aes = FALSE, size = 8)

# Plot anzeigen
betula_c









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
# install.packages("Cairo")
  library(Cairo)
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/alnus_biov2.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = alnus_bio, device = cairo_ps, fallback_resolution = 900,
        width = 36, height = 24, units = "cm")
 

 # 2.2.2. betula biomass ~ DBH comparisson by equation jitter smooth  ---------------------------------------
 
 library(Cairo)
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/betula_biov2.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = betula_bio, device = cairo_ps, fallback_resolution = 900,
        width = 36, height = 24, units = "cm")
 
 
 # 2.3. carbon per hectare ~ equation comparisson by equation boxplot ---------------------------------------
 
 # 2.3.1. alnus carbon per hectare ~ equation comparisson by equation boxplot  ---------------------------------------
 
 library(Cairo)
 
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/alnus_cv2.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = alnus_c, device = cairo_ps, fallback_resolution = 900,
        width = 36, height = 24, units = "cm")
 
 # 2.3.2. betula carbon per hectare ~ equation comparisson by equation boxplot  ---------------------------------------
 
 
 library(Cairo)
 
 
 # Dateipfad
 outfile <- paste0(getwd(), "/output/out_graphs/betula_c.eps")
 
 # Plot speichern mit Cairo EPS (cairo_ps)
 ggsave(outfile, plot = betula_c, device = cairo_ps, fallback_resolution = 900,
        width = 36, height = 24, units = "cm")
 
 
 
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
 
 
 
 
 
 
 
 
 

# NOTES'N TRASH -----------------------------------------------------------

 # alnus_bio <- ggplot() +
 #   # Loess-Fits pro ID ohne den Ausschluss "9_w_agb"
 #   geom_point(data = ungroup(alnus_wag) %>% filter(ID != "9_w_agb"),
 #              aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
 #   geom_smooth(data = filter(alnus_wag, ID != "9_w_agb"),
 #               aes(x = DBH_cm, y = B_kg_tree, color = ID, group = ID),
 #               method = "loess", se = FALSE) +
 #   
 #   # Farbskala
 #   scale_color_manual(values = color_map) +
 #   
 #   # Standard-Kurve + SD-Bereiche (optional, falls vorhanden)
 #   geom_smooth(data = m_sd_b_al, aes(x = DBH_cm, y = B_kg_tree),
 #               method = "loess", color = "red", se = F) +
 #   geom_smooth(data = m_sd_b_al, aes(x = DBH_cm, y = low_sd_B_kg_tree),
 #               method = "loess", color = "red", linetype = "dashed", se = FALSE) +
 #   geom_smooth(data = m_sd_b_al, aes(x = DBH_cm, y = up_sd_B_kg_tree),
 #               method = "loess", color = "red", linetype = "dashed", se = FALSE) +
 #   
 #   # Labels mit geom_text_repel, die sich von den Punkten nach rechts oben absetzen
 #   geom_text_repel(
 #     data = smooth_points,
 #     aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = ID),
 #     nudge_x = 3,
 #     nudge_y = 100,
 #     segment.color = "black",
 #     segment.size = 0.5,
 #     segment.linetype = "dotted",  # <-- hier!
 #     hjust = 0,
 #     size = 6,
 #     max.overlaps = Inf
 #   ) +
 #   
 #   # Achsenlimits und Thema
 #   ylim(0, 2750) +
 #   xlim(0, 75) +
 #   theme_bw() +
 #   theme(
 #     legend.position = "right",
 #     text = element_text(size = 25),        # Schriftgröße
 #     axis.text = element_text(size = 20),
 #     legend.text = element_text(size = 25),
 #     legend.title = element_blank(),
 #     axis.text.x = element_text(hjust = 1)
 #   )  +
 #   
 #   # Achsenbeschriftungen
 #   xlab("Diameter at breast height [cm]") +
 #   ylab("Woody aboveground biomass [kg"~tree^{-1}*"]")
 # 
 # print(alnus_bio)
 
 
 
 
 # 1.2.2.2. betula biomass ggplot ------------------------------------------
 
 # betula_bio <- ggplot() +
 #   # Punkte und Loess-Fits pro ID ohne die größten Biomasse-Ausreißer
 #   geom_point(data = ungroup(betula_wag),
 #              aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID)) +
 #   
 #   geom_smooth(data = ungroup(betula_wag),
 #               aes(x = DBH_cm, y = B_kg_tree, color = ID, group = ID),
 #               method = "loess", se = FALSE) +
 #   
 #   # Farbskala
 #   scale_color_manual(values = color_map) +
 #   
 #   # Mittelwert-Kurve + SD-Bereiche
 #   geom_smooth(data = m_sd_b_be, aes(x = DBH_cm, y = B_kg_tree),
 #               method = "loess", color = "red", se = FALSE) +
 #   geom_smooth(data = m_sd_b_be, aes(x = DBH_cm, y = low_sd_B_kg_tree),
 #               method = "loess", color = "red", linetype = "dashed", se = FALSE) +
 #   geom_smooth(data = m_sd_b_be, aes(x = DBH_cm, y = up_sd_B_kg_tree),
 #               method = "loess", color = "red", linetype = "dashed", se = FALSE) +
 #   
 #   # Labels mit geom_text_repel, rechts oben von den Punkten
 # ggrepel::geom_text_repel(
 #     data = smooth_points,
 #     aes(x = DBH_cm, y = B_kg_tree, label = label_name, color = ID),
 #     nudge_x = 3,
 #     nudge_y = 100,
 #     segment.color = "black",
 #     segment.size = 0.5,
 #     segment.linetype = "dotted",
 #     hjust = 0,
 #     size = 6,
 #     max.overlaps = Inf
 #   ) +
 #   
 #   # Achsenlimits und Thema
 #   ylim(0, 2750) +
 #   xlim(0, 75) +
 #   theme_bw() +
 #   theme(
 #     legend.position = "right",
 #     text = element_text(size = 25),
 #     axis.text = element_text(size = 20),
 #     legend.text = element_text(size = 25),
 #     legend.title = element_blank(),
 #     axis.text.x = element_text(hjust = 1)
 #   ) +
 #   
 #   # Achsenbeschriftungen
 #   xlab("Diameter at breast height [cm]") +
 #   ylab(expression("Woody aboveground biomass [kg" ~ tree^{-1}*"]"))
 # 
 # print(betula_bio)
 
 
 
 
 