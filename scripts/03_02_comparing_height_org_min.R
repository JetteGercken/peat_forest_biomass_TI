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

out.path <- paste0(getwd(), "/output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: 
# this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we import a dataset called "HBI_LT_update_2.csv" which contains plot area and stand data additionally to the original tree data
trees_data <- read.delim(file = paste0(out.path, "HBI_LT_update_3.csv"), sep = ",", dec = ".")
soil_types_db <- read.delim(file = paste0(out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")

coef_H_com <- read.delim(file = paste0(out.path, "coef_H_HBI_NA.csv"), sep = ",", dec = ".")
coeff_H_SP_P <- coef_H_com %>% filter(plot_ID != "all")
coeff_H_SP  <- coef_H_com %>% filter(plot_ID == "all")
coeff_H_SP_P$plot_ID <- as.integer(coeff_H_SP_P$plot_ID)



# 1. data prep -----------------------------------------------------------------
# 1.1. join in soil type info  -----------------------------------------------------------------
trees_data <- trees_data %>% 
  # join in soil type of the respective profile
  left_join(., soil_types_db %>% select(bfhnr_2, min_org) %>% distinct(), 
            by = c("plot_ID" = "bfhnr_2")) %>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))



# 1.2. calcualte Ho -----------------------------------------------------
# here we calcualte the dominant height 
# dominant height is defined as: the height of a tree that represents the 20% strongest trees in the stand
# 1.6.1. create "pseudo stands" -------------------------------------------
LT_avg_SP_ST_P_list <- vector("list", length = length(unique(trees_data$plot_ID))) 
top_20_trees_list <- vector("list", length = length(unique(trees_data$plot_ID)))  
for (i in 1:length(unique(trees_data$plot_ID))) {
  # i = 1
  my.plot.id <- unique(trees_data$plot_ID)[i]
  # select all trees by only one compartiment of each tree to make sure the tree enters the dataframe only once
  my.tree.df <- trees_data[trees_data$plot_ID == my.plot.id & trees_data$C_layer == 1 & trees_data$stand == "A", ] 
  my.n.ha.df <- trees_data %>% filter(plot_ID == my.plot.id & C_layer == 1 & stand == "A") %>% group_by(plot_ID, CCS_r_m) %>% reframe(n_ha_CCS = n()/stand_plot_A_ha) %>% distinct()
  my.n.plot.df <- trees_data %>% filter( plot_ID == my.plot.id  & C_layer == 1 & stand == "A") %>% group_by(plot_ID, CCS_r_m) %>% reframe(n_CCS = n()) %>% distinct()
  
  # calcualte how often a tree in each ccs is repeated per plot to fill the whole hektar with trees
  my.n.ha.df$n.rep.each.tree <- round(my.n.ha.df$n_ha_CCS/my.n.plot.df$n_CCS)
  
  # repeat every tree per circle by the number this tree would be repeated by to reach it´s ha number
  # so every tree id repeated as often as it would be represented on a hectar)
  # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
  my.tree.rep.df <- rbind(
    # 5m circle
    my.tree.df[my.tree.df$CCS_r_m == 5.64, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 5.64, ])), 
                                                 each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 5.64]), ],
    # 12m circle
    my.tree.df[my.tree.df$CCS_r_m == 12.62, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 12.62, ])), 
                                                  each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 12.62] ), ],
    # 17m circle
    my.tree.df[my.tree.df$CCS_r_m == 17.84, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 17.84, ])), 
                                                  each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 17.84]), ])
  
  # calculate upper 20%
  n.20 <- 0.2 * nrow(my.tree.rep.df)
  
  LT_avg_SP_ST_P_list[[i]] <-( my.tree.rep.df %>% 
     # select only the 20% strongest trees  
    arrange(desc(DBH_cm)))[1:n.20, ] %>% 
    group_by(plot_ID, inv, min_org,  stand, C_layer, SP_code, H_SP_group) %>% 
    dplyr::summarise(mean_DBH_cm = mean(DBH_cm), 
              sd_DBH_cm = sd(DBH_cm),
              Dg_cm = ((sqrt(mean(BA_m2)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2),
              mean_H_m = mean(H_m), 
              sd_H_m = sd(H_m), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2))/sum(sum(BA_m2)), 
              Ho_m = NA) %>% 
    # calculate the height of the tree representing the mean BA of the upper 20% of the stand
    unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
                left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
                            select(plot_ID, SP_code, R2) %>% 
                            unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
                          by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
                left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
                          by = "SP_code") %>% 
                mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
                       H_method = case_when(is.na(Ho_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(Ho_m) & R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                                            is.na(Ho_m) & is.na(R2.x) & R2.y > 0.70| is.na(Ho_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                                            is.na(Ho_m) & is.na(R2_comb) & !is.na(Hg_m)| is.na(Ho_m) & R2_comb < 0.70 & !is.na(Hg_m) ~ "ehk_sloboda",
                                            is.na(Ho_m) & is.na(R2_comb) & is.na(Hg_m)| is.na(Ho_m) & R2_comb < 0.70 & is.na(Hg_m) ~ "h_curtis", 
                                            TRUE ~ "sampled")) %>% 
                # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
                mutate(Ho_m = as.numeric(case_when(is.na(Ho_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(Ho_m) & R2.x > R2.y & R2.x > 0.70 ~ h_nls_SP_P(SP_P_ID, Dg_cm),
                                                  # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                                                  # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                                                  # 0.75 then use the SP_P models
                                                  is.na(Ho_m) & is.na(R2.x) & R2.y > 0.70 | is.na(Ho_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, Dg_cm),
                                                  # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                                  # and hm is na but there is a h_g and d_G
                                                  is.na(Ho_m) & is.na(R2_comb) & !is.na(Hg_m)| is.na(Ho_m) & R2_comb < 0.70 & !is.na(Hg_m) ~ ehk_sloboda(H_SP_group, Dg_cm*10, mean_DBH_cm*10, Dg_cm*10, Hg_m*10),
                                                  # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                                  # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                                                  is.na(Ho_m) & is.na(R2_comb) & is.na(Hg_m)| is.na(Ho_m) & R2_comb < 0.70 & is.na(Hg_m) ~ h_curtis(H_SP_group, Dg_cm*10), 
                                                  TRUE ~ Ho_m)))
  
  top_20_trees_list[[i]] <- ( my.tree.rep.df %>% # select only the 20% strongest trees  
                      arrange(desc(DBH_cm)))[1:n.20, ] 
  
  print(paste(i, my.plot.id))
  
 
}
LT_avg_SP_ST_P <- as.data.frame(rbindlist(LT_avg_SP_ST_P_list))
top_20_trees <- as.data.frame(rbindlist(top_20_trees_list))



# fit height model  -------------------------------------------------------



view(
  trees_data %>% group_by(min_org, plot_ID, SP_code) %>% dplyr::summarise(n_SP = dplyr::n()) %>% 
    left_join(.,trees_data %>% group_by(min_org, plot_ID) %>% dplyr::summarise(n_tot = dplyr::n()) , by = c("plot_ID", "min_org") ) %>% 
    mutate(n_share = n_SP/n_tot*100) %>% 
    filter(min_org == "org") %>% 
    arrange(n_share)
)



# statistical comparisson -------------------------------------------------
# all sp together statistical comparisson -------------------------------------------------
trees_sub_all <-  trees_data[trees_data$H_method == "sampled", ]
setDT(trees_sub_all)[, `:=`(HD = H_m/DBH_cm)]

# test normality of height
shapiro.test(trees_sub_all$H_m)
# test normalitly of HD
shapiro.test(trees_sub_all$HD)

# test significant difference of mean HD
wilcox.test(HD~min_org, data = trees_sub_all, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
# p-value = 0.5739 --> there is no significant difference by min org if we compare all species together
# n_all: nrow(unique(trees_sub_all[, c("plot_ID", "tree_ID")])) -> 45766
# n_org: nrow(unique(trees_sub_all[trees_sub_all$min_org == "org", c("plot_ID", "tree_ID")])) -> 2597
# n_min: nrow(unique(trees_sub_all[trees_sub_all$min_org == "min", c("plot_ID", "tree_ID")])) -> 43169

# test significant difference of mean H
wilcox.test(H_m~min_org, data = trees_sub_all, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
# p-value < 2.2e-16 --> there is a sign. diff. between H of org and mineral plots

# test significant difference of mean D
wilcox.test(DBH_cm~min_org, data = trees_sub_all, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
# p-value p-value < 2.2e-16 --> there is a sign. diff. between DBH of org and mineral plots





# alnus betula statistical comparisson -------------------------------------------------
trees_sub <- trees_data[trees_data$bot_name %in% c("Betula pubescens", "Alnus glutinosa") & trees_data$H_method == "sampled" , ]
setDT(trees_sub)[, `:=`(HD = H_m/DBH_cm)]

# check n
# n_all: nrow(unique(trees_sub[, c("plot_ID", "tree_ID")])) -> 544
# n_org: nrow(unique(trees_sub[trees_sub$min_org == "org", c("plot_ID", "tree_ID")])) -> 310
# n_min: nrow(unique(trees_sub[trees_sub$min_org == "min", c("plot_ID", "tree_ID")])) -> 234
# n_org_alnus: nrow(unique(trees_sub[trees_sub$min_org == "org" & trees_sub$bot_name == "Alnus glutinosa", c("plot_ID", "tree_ID")])) -> 196
# n_min_alnus: nrow(unique(trees_sub[trees_sub$min_org == "min" & trees_sub$bot_name == "Alnus glutinosa", c("plot_ID", "tree_ID")])) -> 135 
# n_org_alnus: nrow(unique(trees_sub[trees_sub$min_org == "org" & trees_sub$bot_name == "Betula pubescens", c("plot_ID", "tree_ID")])) -> 114
# n_min_alnus: nrow(unique(trees_sub[trees_sub$min_org == "min" & trees_sub$bot_name == "Betula pubescens", c("plot_ID", "tree_ID")])) -> 99 



# test normality of height
shapiro.test(trees_sub$H_m)
# Shapiro-Wilk normality test
# 
# data:  trees_sub$H_m
# W = 0.99187, p-value = 1.648e-08 --> significant --> not normally 

shapiro.test(trees_sub$HD)
# Shapiro-Wilk normality test
# 
# data:  trees_sub$HD
# W = 0.9797, p-value = 7.197e-07 --> signidicant --> not normal distributed

# man withne u / wilcoxon test for non-parametric data of 2 indepentend groups whos mean we are comparing
wilcox.test(HD~min_org, data = trees_sub, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
# HD values are not significantly different: p-value = 0.09944 --> diff is not significant 
# H_m for alnus and betula together is significanlty different: p-value = 1.014e-07
# DBH_cm for alnus and betula together is significanlty different: p-value = 0.00169


wilcox.test(DBH_cm~min_org, data = trees_sub[trees_sub$bot_name == "Alnus glutinosa", ], 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
# DBH mean for alnus are different: p-value = 0.0003553
# H_m  means for Alnus are different: p-value = 5.058e-06 
# HD means are not different: p-value = 0.8471 

wilcox.test(DBH_cm~min_org, data = trees_sub[trees_sub$bot_name == "Betula pubescens", ], 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
# DBH mean for Betula are not different: p-value = 0.3822
# H_m  means for betula are different: p-value = 0.008783
# HD means are not different: p-value = 0.1163

trees_min_org <- trees_data[trees_data$bot_name %in% c("Betula pubescens", "Alnus glutinosa") & 
             trees_data$min_org == "min" &  
             trees_data$H_method == "sampled"  |
             trees_data$bot_name %in% c("Betula pubescens", "Betula spp.", "Alnus glutinosa", "Alnus spp.")& 
             trees_data$min_org == "org" &  
             trees_data$H_method == "sampled", ]
setDT(trees_min_org)[, `:=`(HD = H_m/DBH_cm)]

wilcox.test(H_m~min_org, data = trees_min_org[trees_min_org$bot_genus == "Betula", ], 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)

# 2. visuals of comparisson -----------------------------------------------------------------
# fit nls first so we can decide which function ggplot uses t fit the smooth
# nls : DBH vs. sampled hight of all trees at all plots split by org vs. mineral soil
trees_data_h_nls <- 
  left_join(trees_data %>% filter(H_method == "sampled")
            #%>% left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2")) %>% 
            # mutate(min_org = ifelse(inv == "momok", "org", min_org)),
            ,trees_data %>% filter(H_method == "sampled") %>%
              # left_join(., soil_types_db %>% 
              #             select(bfhnr_2 , min_org), 
              #           by = c("plot_ID" = "bfhnr_2"))%>%
              # mutate(min_org = ifelse(inv == "momok", "org", min_org)) %>% 
              group_by(min_org, bot_name) %>%
              nls_table( #H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
                #mod_start = c(b0=23, b1=0.03, b2 =1.3), 
                H_m ~ 1.3 + (DBH_cm / (b0 + b1 * DBH_cm))^3,  # Petterson function
                mod_start = c(b0=1, b1=1), 
                output = "table", 
                .groups = c("min_org", "bot_name")),
            by = c("min_org", "bot_name")) %>% 
  mutate(H_m_nls = 1.3 + (DBH_cm / (b0 + b1 * DBH_cm))^3, 
         H_method_nls = "nls")

write.csv(trees_data_h_nls, paste0(out.path, paste(trees_data_h_nls$inv[1], "LT_update_3_petterson", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")



# height mod min soil
model_min <- nls(H_m ~ 1.3 + (DBH_cm / (a + b * DBH_cm))^3, 
                 data = trees_data %>% 
                   #   left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                   filter(H_method == "sampled" #& compartiment == "ag" 
                          & min_org == "min")  , 
                 start = list(a = 1, b = 1))  # Initial guesses for a and b

# Step 4: View the results
summary(model_min)

# height mod org soil
model_org <- nls(H_m ~ 1.3 + (DBH_cm / (a + b * DBH_cm))^3, 
                 data = trees_data %>%
                   #left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>%
                   filter(H_method == "sampled"
                          # & compartiment == "ag" 
                          & min_org == "org")  , 
                 start = list(a = 1, b = 1))  # Initial guesses for a and b

# Step 4: View the results
summary(model_org)

h_DBH_all_SP <- ggplot() +
  geom_point(data = (trees_data %>%
                       filter(H_method == "sampled" 
                              #    & BWI_SP_group %in% c("aLh", "aLn")
                              & bot_name %in% c("Betula pubescens", "Alnus glutinosa")
                       ) %>% 
                       #left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                       mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
             aes(x = DBH_cm, y = H_m, color = min_org), alpha = 0.05)+
  geom_smooth(data = trees_data_h_nls %>% 
                filter(H_method == "sampled" 
                       # & BWI_SP_group %in% c("aLh", "aLn")
                       # & bot_genus %in% c("Betula", "Alnus")
                       & bot_name %in% c("Betula pubescens", "Alnus glutinosa")
                ),# %>%  
              #  mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
              aes(x = DBH_cm, y = H_m_nls, color = min_org))+ 
  theme_bw()+ 
  facet_wrap(~bot_name)+
  xlab("DBH [cm]")+ 
  ylab("height [m]")






# dominant heights by dg per plot (only trees in stand A and canopy layer 1) separeted by botanical species 
ggplot(data = LT_avg_SP_ST_P %>% 
         left_join(., SP_names_com_ID_tapeS %>% mutate(Chr_code_ger_lower = tolower(Chr_code_ger)) %>% select(Chr_code_ger_lower, bot_name, bot_genus, bot_species), by = c("SP_code" = "Chr_code_ger_lower")) %>% 
         filter(startsWith(bot_name, "Betula")| startsWith(bot_name, "Alnus")))+
  geom_jitter(aes(x = Dg_cm , y = Ho_m, colour = as.factor(min_org)))+ 
  geom_smooth(aes(x = Dg_cm , y = Ho_m, colour = as.factor(min_org)))+
  facet_wrap(~bot_genus)

## top 20% 
# heighst of top strongest trees per stand and canopy layer by dbh of falnus and betula species separated by species 
ggplot()+
  geom_jitter(data = top_20_trees %>% 
                filter(startsWith(bot_name, "Betula")  & H_method == "sampled"| startsWith(bot_name, "Alnus") & H_method == "sampled" 
                   #    | startsWith(bot_name, "Betula")  & H_method == "coeff_SP_P"| startsWith(bot_name, "Alnus") & H_method == "coeff_SP_P"
                ) , 
              aes(x = DBH_cm , y = H_m, colour = as.factor(min_org)))+ 
  geom_smooth(data = left_join(top_20_trees %>% filter(startsWith(bot_name, "Betula") | startsWith(bot_name, "Alnus") ),
                               (top_20_trees %>% filter(startsWith(bot_name, "Betula")  & H_method == "sampled"| startsWith(bot_name, "Alnus") & H_method == "sampled" ) %>% 
                                  group_by(bot_genus, min_org) %>%
                                  nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
                                             mod_start = c(b0=23, b1=0.03, b2 =1.3), 
                                             output = "table")),
                               by = c("bot_genus", "min_org")) %>% 
                mutate(H_m = b0 * (1 - exp( -b1 * DBH_cm))^b2, 
                       H_method = "nls"), 
              aes(x = DBH_cm , y = H_m, colour = as.factor(min_org)))+
  facet_wrap(~ bot_name)

# violi plot of  heighst of top strongest trees per stand and canopy layer by dbh of falnus and betula species separated by species 
top_20_trees %>% 
  filter(bot_name == "Betula pubescens" & H_method == "sampled" |
           bot_name == "Alnus glutinosa" & H_method == "sampled") %>% 
  distinct() %>%  
              ggplot() +
  geom_boxplot(aes(x = min_org, y = H_m))+
  geom_violin(aes(x = min_org, y = H_m), alpha=0.2) +
  geom_point(aes(x = min_org, y = H_m))+
  facet_wrap(~bot_name)+ 
  geom_label(data = top_20_trees %>% 
              distinct() %>% 
              filter(bot_name == "Betula pubescens" & H_method == "sampled" |
                       bot_name == "Alnus glutinosa" & H_method == "sampled") %>% 
              group_by(bot_name, min_org) %>% 
              summarise(n = n()), aes(x = min_org, y = 38, label = n ))



# heights of alnus and betual sepcies over all NSI plots
# by diameter 
# separeted in organic vs. minearl sites and species (group) (botanical name) only for dominating, vital trees trees 
ggplot(data = trees_data %>% 
  filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
    startsWith(bot_name, "Alnus") & H_method == "sampled")%>% 
    filter(C_layer %in% c(1) & Kraft %in% c(1)))+ 
  geom_jitter(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+ 
  geom_smooth(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+
  facet_wrap(~bot_name)

# heights of alnus and betula over all NSI plots
# by diameter 
# separeted in organic vs. minearl sites and botanical genus
ggplot(data = trees_data %>% 
         filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
                  startsWith(bot_name, "Alnus") & H_method == "sampled") )+
  geom_jitter(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+ 
  geom_smooth(aes(x = DBH_cm, y = H_m, colour = as.factor(min_org)))+
  facet_wrap(bot_genus~C_layer)+
  ylim(0, 50)


# dbh comparisson box plot org min boxplot separated  by bot genus and soil type
ggplot(data = trees_data %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
                  startsWith(bot_name, "Alnus") & H_method == "sampled"))+
  geom_boxplot(aes(x = min_org, y = DBH_cm))+
  facet_wrap(~bot_genus)

# dbh comparisson violin plot org min boxplot separated  by species and soil type 
ggplot(data = trees_data %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         filter(bot_name == "Betula pubescens" & H_method == "sampled" |
         bot_name == "Alnus glutinosa" & H_method == "sampled")) +
  geom_boxplot(aes(x = min_org, y = H_m))+
  geom_violin(aes(x = min_org, y = H_m), alpha=0.2) +
  facet_wrap(~bot_name)

# dbh comparisson violin plot org min boxplot separated  by species, soil type and dominating vs. not dominating social class 
ggplot(data = trees_data %>%
         mutate(kraft_group = ifelse(Kraft %in% c(1, 2), "dominating", "not dominating")) %>% 
         filter(Kraft %in% c(1, 2, 3, 4)) %>% 
         mutate(hd = H_m/DBH_cm) %>% 
         filter(startsWith(bot_name, "Betula") & H_method == "sampled" |
                  startsWith(bot_name, "Alnus") & H_method == "sampled"))+
  geom_boxplot(aes(x = min_org, y = H_m))+
  geom_violin(aes(x = min_org, y = H_m), alpha=0.2) +
  facet_grid(bot_genus~kraft_group)


# DBH vs. sampled hight of all trees at all plots split by org vs. mineral soil
ggplot() +
  geom_point(data = (trees_data %>%
                       filter(H_method == "sampled") %>% 
                       left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                       mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
             aes(x = DBH_cm, y = H_m, color = min_org), alpha = 0.035)+
  geom_smooth(data = (trees_data %>% 
                        filter(H_method == "sampled") %>% 
                        left_join(., soil_types_db %>% select(bfhnr_2 , min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
                        mutate(min_org = ifelse(inv == "momok", "org", min_org))), 
              aes(x = DBH_cm, y = H_m, color = min_org))




HD_box <- ggplot()+ 
  geom_boxplot(data = setDT(trees_data)[H_method == "sampled" , ':=' (HD = H_m/ DBH_cm)])




# base r line plo ---------------------------------------------------------

# Split data for faceting
trees_filtered <- subset(trees_data, H_method == "sampled" & bot_name %in% c("Betula pubescens", "Alnus glutinosa"))
trees_nls_filtered <- subset(trees_data_h_nls, H_method == "sampled" & bot_name %in% c("Betula pubescens", "Alnus glutinosa"))

# Get unique species
species_list <- unique(trees_filtered$bot_name)

# Set up faceted plotting
par(mfrow = c(1, length(species_list))) # Adjust panel layout (1 row, multiple columns)

# Define colors for min_org categories
colors <- c("org" = "red", "other" = "blue")  # Adjust "other" if there are more categories

for (species in species_list) {
  i = 1
  # Filter data for current species
  sub_data <- subset(trees_filtered, bot_name == species)
  sub_nls_data <- subset(trees_nls_filtered, bot_name == species)
  
  # Plot scatter points
plot(sub_data$DBH_cm, sub_data$H_m, col = adjustcolor(colors[sub_data$min_org], alpha.f = 0.05),
       pch = 16, xlab = "DBH [cm]", ylab = "height [m]", main = species)
  
  # Add smoothed line (if applicable)
  if (nrow(sub_nls_data) > 0) {
    lines(sub_nls_data$DBH_cm, sub_nls_data$H_m_nls, col = "black", lwd = 2)
  }
}

# Reset par settings
par(mfrow = c(1, 1))









tikz(here("output/out_graphs/h_DBH_all_SP.jpg"),
     width = 7,
     height = 7)
plot(h_DBH_all_SP)
dev.off()
