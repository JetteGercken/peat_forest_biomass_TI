
# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the the national soil inventory
  # estimating missing tree heights based on sampled height and diameter pairs
  # via nls per plot and species or just species and via SLOBODA and CURTIS

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
HBI_trees <- read.delim(file = paste0(out.path, "HBI_LT_update_1.csv"), sep = ",", dec = ".")
HBI_trees_removed <- read.delim(file = paste0(out.path, (HBI_trees$inv)[1], "_LT_removed.csv"), sep = ";", dec = ".")

# BZE3 BE dataset: 
# this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
# here we import a dataset called "BZE3_LT_update_2.csv" which contains plot area and stand data additionally to the original tree data
try({
  BZE3_trees <- read.delim(file = paste0(out.path, "BZE3_LT_update_2.csv"), sep = ",", dec = ".")
  BZE3_trees_removed <- read.delim(file = paste0(out.path, (BZE3_trees$inv)[1], "_LT_removed.csv"), sep = ",", dec = ".")},
  silent = T)


# ----- 0.6 dataprep -----------------------------------------------------------------------
# ----- 0.6.1. harmonising column names & structure ----------------------------------------
if(exists('BZE3_trees') == T){
  trees_total <-  rbind(HBI_trees, BZE3_trees %>% select(c(colnames(HBI_trees))))
}else{
    trees_total <- HBI_trees}

## HBI & BZE3 together 
trees_total[,c("plot_A_ha", "area_m2", "X_tree",  "Y_tree",
             "DBH_cm", "dist_m","CCS_r_m")] <- lapply(trees_total[,c("plot_A_ha", "area_m2", "X_tree",  "Y_tree",
                                                                   "DBH_cm", "dist_m",  "CCS_r_m")], as.numeric)
# change -2 in H_dm and C_h_dm to NA: https://stackoverflow.com/questions/14737773/replacing-occurrences-of-a-number-in-multiple-columns-of-data-frame-with-another
trees_total[,c("H_dm","C_h_dm")][trees_total[,c("H_dm","C_h_dm")]== -2] <- NA
trees_total[,c("H_dm","C_h_dm")][trees_total[,c("H_dm","C_h_dm")]== -9] <- NA


# ----- 0.6.2. calcualte diameter and change units  -------------------------------------------------
trees_total <- trees_total %>% 
  mutate(H_m = H_dm/10, 
         DBH_h_m = DBH_h_cm/100) %>%                               # change unit of DBH measuring height from cm into m by dividing by 100  
  #  apply regression of BWI (5.5.1.2.) for DBH estimation when mesasuring height differs from 1.3 m 
  mutate(DBH_class = DBH_c_function(DBH_cm, DBH_c = "class_5"), 
         BA_m2 = c_A(DBH_cm/2)*0.0001) %>% # *0.0001 to convert cm2 in m2 %>% 
 arrange(plot_ID, inv)


# test start
# trees_total <- rbind(trees_total %>% mutate(H_m = H_m+1.5, 
#                                       DBH_cm = DBH_cm+10), 
#                      trees_total %>% mutate(H_m = H_m-1.5, 
#                                             DBH_cm = DBH_cm-10),
#                      trees_total)
# test end
 
 
# 2. estimating tree height -------------------------------------------------------------
# 2.1. fitting own models H_m ~ nls(DBH_cm) ---------------------------------------------
# 2.1.1. non linear height model h ~ DBH per species and plot ----------------------------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled we create a non-linear regression for the heights
# in the following a dataframe with regression coefficients per 
# species per plot is created if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(
  # select variables needed for modeling 
  trees_total %>% select(plot_ID, SP_code, H_m, DBH_cm) %>% 
    # filter for measured heights that also the necessary info about the diameter and measuring height of diameter
    filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
    group_by(plot_ID, SP_code) %>%
    # filter for plots that have at least 3 heights measured per species
    filter(n() >= 3),
  # creaing & joining in coeff_H_SP_P dataset 
  trees_total %>% 
    select(plot_ID, SP_code, H_m, DBH_cm) %>% 
    filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
    #filter(DBH_cm <= 150) %>% 
    group_by(plot_ID, SP_code) %>% 
    # filter for plots where there is at least 3 heights measured for each species
    #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
    filter(n() >= 3)%>%    
    group_by(plot_ID, SP_code) %>%
    nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
               mod_start = c(b0=23, b1=0.03, b2 =1.3), 
               output = "table") %>%
    arrange(plot_ID, SP_code), 
  by = c("plot_ID", "SP_code"))%>%
  # mutating statistical precictors
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(plot_ID, SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(H_m, H_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             #N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est))

# 2.1.2. non linear height model h ~ DBH per species over all plots ----------------------------
# coefficents of non-linear height model per species but over all plots: 
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
#  building separate dataframe for speicies soecific models adding adding bias, rmse and rsqrd 
coeff_H_SP <- left_join(trees_total %>% 
                          select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                          group_by(SP_code) %>% 
                          filter(n() >= 3),
                        # dataset with height coefficients
                        trees_total %>% 
                          select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class) ) %>% 
                          #filter(DBH_cm <= 150) %>% 
                          group_by(SP_code) %>% 
                          # filter for plots where there is at least 3 heights measured for each species
                          #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
                          filter(n() >= 3)%>%    
                          group_by(SP_code) %>%
                           nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
                                      mod_start = c(b0=23, b1=0.03, b2 =1.3), 
                                      output = "table"), 
                        by = c("SP_code"))%>%
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(SP_code) %>% 
  dplyr::summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(H_m, H_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             #N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est)) %>% 
  mutate(plot_ID = as.factor('all')) %>% 
  select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, SSres, SStot, pseu_R2, diff_h)

 # 2.1.3 combined coefficients of height models -----------------------------------------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% dplyr::mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)


# 2.2 calculating Hg, Dg etc. per plot, stand, species-----------------------------------------------
# calcualte the height and diameter of a stem reprensenting the mean basal area 
Hg_Dg_trees_total.df <- 
 trees_total %>% 
   group_by(inv, plot_ID, stand, C_layer, SP_code) %>% 
   summarise(mean_DBH_mm = mean(DBH_cm)*10,
             D_g = sqrt(mean(BA_m2[!is.na(H_dm)])*4/pi)*100, # dg in cm --> that´s why the *100 --> m in cm
             H_g = sum((H_dm[!is.na(H_dm)]/10)*BA_m2[!is.na(H_dm)])/sum(BA_m2[!is.na(H_dm)]))  # hg in m --> because BA is in m2 and we divided H_dm by 10 to have H_m
 
 
  



# 2.3. height calculation -------------------------------------------------
# 2.3.1. height calculation HBI -------------------------------------------------
# 2.3.1.1. normal height calculation HBI -------------------------------------------------

HBI_trees_update_3 <-     # this should actually be the BZE3 Datset 
  trees_total %>% 
  filter(inv %in% c("HBI", "momok"))%>% ##change: we have to adjust this to select momok trees as well
## joining coefficients and Hg-Dg-data in
tidyr::unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  # this is joins in a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., Hg_Dg_trees_total.df,
            by = c("inv", "plot_ID", "stand", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = as.numeric(case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g*10, H_g*10),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m))) %>% 
  # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
  # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
  mutate(H_method = case_when(DBH_h_m > H_m & !is.na(H_g) ~ "ehk_sloboda_2",
                              DBH_h_m > H_m & is.na(H_g) ~ "curtis_2", 
                              TRUE ~ H_method),
         H_m = case_when(DBH_h_m > H_m & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g*10, H_g*10),
                         DBH_h_m > H_m & is.na(H_g) ~  h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m)) 

# 2.3.1.1. just DBH based height calculation HBI -------------------------------------------------
### H only estimated via DBH
# we need a HG calculated only from DBH too
# or we only use our models no matter what 
# lets look at a sloboda height without hg from h 

## make datatables  
trees_trial <- data.table(trees_total)
coeff_H_SP_P <- data.table(coeff_H_SP_P)
coeff_H_SP <- data.table(coeff_H_SP)

## data prep for join
trees_trial[, ':='(SP_P_ID = paste0(plot_ID, SP_code))]  # unite SP and plot_ID for nls coeficcients 
coeff_H_SP_P[,':='(SP_P_ID = paste0(plot_ID, SP_code)  )]                     # unite SP and plot_ID for nls coeficcients 
#coeff_H_SP[,':='(R2_SP = R2)]    # rename r2 in SP  specific nls to r2y

##  add coeficccients to dataset
# species and plot specific nls
trees_trial <- 
  trees_trial %>% 
  left_join(., coeff_H_SP_P[, c("plot_ID", "SP_code", "R2", "SP_P_ID")],  by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  # species specific nls
  left_join(., coeff_H_SP[, c("SP_code", "R2")], by = c("SP_code")) %>% 
  ##  height method
  mutate(H_m_nls_meth = case_when( !is.na(R2.x) & R2.x > 0.70 |  R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                                   is.na(R2.x) & R2.y > 0.70|  R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                                   TRUE ~ NA)) %>% 
  mutate(H_m_nls = case_when(!is.na(R2.x) & R2.x > 0.70 |  R2.x > R2.y & R2.x > 0.70 ~ as.numeric(h_nls_SP_P(SP_P_ID, DBH_cm)),
                             # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                             # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                             # 0.75 then use the SP_P models
                             is.na(R2.x) & R2.y > 0.70 |  R2.x < R2.y & R2.y > 0.70 ~ as.numeric(h_nls_SP(SP_code, DBH_cm)),
                             # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                             # and hm is na but there is a h_g and d_G
                            TRUE ~ NA)) %>% 
  # if the height is still NA
  mutate(H_m_nls_meth = case_when( is.na(H_m_nls)& !is.na(R2.x) & R2.x > R2.y  ~ "coeff_SP_P", 
                                   is.na(H_m_nls)& !is.na(R2.y) & R2.y > R2.x  ~ "coeff_sp",
                                   TRUE ~ NA)) %>%
  mutate(H_m_nls_meth = case_when( is.na(H_m_nls)& !is.na(R2.x) & R2.x > R2.y  ~ as.numeric(h_nls_SP_P(SP_P_ID, DBH_cm)), 
                                   is.na(H_m_nls)& !is.na(R2.y) & R2.y > R2.x  ~ as.numeric(h_nls_SP(SP_code, DBH_cm)),
                                   TRUE ~ NA)) %>% 
  mutate(H_dm_nls = H_m_nls*10)

# calcualte bg when itas only based on DBH estiamted heights
hg_dg_trees_trial <- trees_trial %>% 
  filter(!is.na(H_dm)) %>% # select only sampled trees anyways but their height is estimated via DBH nls
  group_by(inv, plot_ID, stand, C_layer, SP_code) %>% 
  summarise(mean_DBH_mm = mean(DBH_cm)*10 ,
            D_g_nls = sqrt(mean(BA_m2[!is.na(H_dm_nls)])*4/pi)*100, # dg in cm --> that´s why the *100 --> m in cm
            H_g_nls = sum((H_dm_nls[!is.na(H_dm_nls)]/10)*BA_m2[!is.na(H_dm_nls)])/sum(BA_m2[!is.na(H_dm_nls)]), 
            H_nls_mean = mean(H_m_nls))  # hg in m --> because BA is in m2 and we divided H_dm by 10 to have H_m

trees_only_DBH_height <- ## momok
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
trees_trial %>%  
  left_join(hg_dg_trees_trial, by = c("inv", "plot_ID", "stand", "C_layer", "SP_code")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x)) %>% # select the better R2
    mutate(H_m_nls_meth = case_when( !is.na(R2.x) & R2.x > 0.70 |  R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                              is.na(R2.x) & R2.y > 0.70|  R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(R2_comb) & !is.na(H_g_nls)|  R2_comb < 0.70 & !is.na(H_g_nls) ~ "ehk_sloboda",
                               is.na(R2_comb) & is.na(H_g_nls)|  R2_comb < 0.70 & is.na(H_g_nls) ~ "h_curtis", 
                              TRUE ~ NA)) %>% 
  mutate(H_m_nls = case_when(!is.na(R2.x) & R2.x > 0.70 |  R2.x > R2.y & R2.x > 0.70 ~ as.numeric(h_nls_SP_P(SP_P_ID, DBH_cm)),
                                        # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                                        # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                                        # 0.75 then use the SP_P models
                              is.na(R2.x) & R2.y > 0.70 |  R2.x < R2.y & R2.y > 0.70 ~ as.numeric(h_nls_SP(SP_code, DBH_cm)),
                                        # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                        # and hm is na but there is a h_g and d_G
                              is.na(R2_comb) & !is.na(H_g_nls)| R2_comb < 0.70 & !is.na(H_g_nls)~ as.numeric(ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g_nls*10, H_g_nls*10)),
                                        # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                        # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                              is.na(R2_comb) & is.na(H_g_nls)|  R2_comb < 0.70 & is.na(H_g_nls)~  as.numeric(h_curtis(H_SP_group, DBH_cm*10)), 
                             TRUE ~ H_nls_mean)) %>% 
  # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
  # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
  mutate(H_m_nls_meth = case_when(DBH_h_m > H_m_nls & !is.na(H_g_nls) ~ "ehk_sloboda_2",
                                DBH_h_m > H_m_nls & is.na(H_g_nls) ~ "curtis_2", 
                                TRUE ~ H_m_nls_meth),
         H_m_nls = case_when(DBH_h_m > H_m_nls & !is.na(H_g_nls) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g_nls*10, H_g_nls*10),
                             DBH_h_m > H_m_nls & is.na(H_g_nls) ~  h_curtis(H_SP_group, DBH_cm*10), 
                             TRUE ~ H_m_nls))
  


# add "purelry" DBH based heights to trees HBI update
HBI_trees_update_3 <- HBI_trees_update_3 %>% 
  left_join(trees_only_DBH_height[, c("plot_ID", "tree_ID",  "H_m_nls_meth",   "H_m_nls",  "H_dm_nls", "D_g_nls",  "H_g_nls", "H_nls_mean")], 
            by = c("plot_ID", "tree_ID")) %>% 
  distinct()
 

# 2.3.2. height calculation BZE 3 --------------------------------------------------------------------------------------------------------------------------------------
BZE3_trees_update_3 <-  trees_total %>% 
  filter(inv=="BZE3")%>% 
## 2.3.1. joining coefficients and Hg-Dg-data in 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  # this is joins in a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., Hg_Dg_trees_total.df,
            by = c("plot_ID", "inv","stand", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = as.numeric(case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g*10, H_g*10),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m))) %>% 
  # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
  # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
  mutate(H_method = case_when(DBH_h_m > H_m & !is.na(H_g) ~ "ehk_sloboda_2",
                              DBH_h_m > H_m & is.na(H_g) ~ "curtis_2", 
                              TRUE ~ H_method),
         H_m = case_when(DBH_h_m > H_m & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g*10, H_g*10),
                         DBH_h_m > H_m & is.na(H_g) ~  h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m)) %>% 
  mutate(H_meth_est = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                       is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                       is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                       is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                       TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m_est = as.numeric(case_when(!is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                                    # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                                    # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                                    # 0.75 then use the SP_P models
                                    is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                                    # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                    # and hm is na but there is a h_g and d_G
                                    is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g*10, H_g*10),
                                    # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                    # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                                    is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                                    TRUE ~ H_m))) %>% 
  # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
  # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
  mutate(H_meth_est = case_when(DBH_h_m > H_m_est & !is.na(H_g) ~ "ehk_sloboda_2",
                              DBH_h_m > H_m_est & is.na(H_g) ~ "curtis_2", 
                              TRUE ~ H_method),
         H_m_est = case_when(DBH_h_m > H_m_est & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g*10, H_g*10),
                         DBH_h_m > H_m_est & is.na(H_g) ~  h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m_est)) %>%
  # select columns that should enter the next step of data processing
  # select(plot_ID, inv, inv_year, stand, tree_ID,  tree_inventory_status,  multi_stem, dist_cm,  azi_gon, age, age_meth,  
  #        SP_code, Chr_code_ger, bot_name, tpS_ID, LH_NH, H_SP_group, BWI_SP_group, Bio_SP_group, N_SP_group, N_bg_SP_group, N_f_SP_group_MoMoK,
  #        DBH_class,  Kraft, C_layer, H_dm, H_m, H_method, C_h_dm, D_mm,   DBH_h_cm,  DBH_cm, BA_m2,
  #        CCS_r_m, stand, stand_plot_A_ha, plot_A_ha) %>% 
  filter(!is.na(H_m))

# 1.1.2.6. remove problematik trees ---------------------------------------
 # HBI
 HBI_trees_removed <- plyr::rbind.fill(HBI_trees_removed, 
                                         HBI_trees_update_3 %>%
                                           filter(DBH_h_cm/100 >= H_m | H_m >40 & H_method != "sampled") %>% 
                                           mutate(rem_reason = "LT excluded during height estimation"))
 # BZE3
if(exists('BZE3_trees')){
 BZE3_trees_removed <- plyr::rbind.fill(BZE3_trees_removed, 
                                          BZE3_trees_update_3 %>%
                                            filter(DBH_h_cm/100 >= H_m | H_m >40 & H_method != "sampled") %>% 
                                            mutate(rem_reason = "LT excluded during height estimation")) 
 }
 


# ---- 2. exporting dataset --------------------------
 # height nls coefficients
write.csv(coeff_H_comb, paste0(out.path, "coef_H_", unique(HBI_trees_update_3$inv)[1], "_", unique(HBI_trees_update_3$inv)[2] , ".csv"), row.names = FALSE, fileEncoding = "UTF-8") ##change : we had to adjust this so it saves the coefficients as "hbi_momok"
                               
# HBI dataset including estimated heights
write.csv(HBI_trees_update_3, paste0(out.path, paste(unique(HBI_trees_update_3$inv)[1], "LT_update_3", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(HBI_trees_removed, paste0(out.path, paste(unique(HBI_trees_update_3$inv)[1], "LT_removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

# BZE3 dataset including estimated heights
if(exists('BZE3_trees')){
write.csv(BZE3_trees_update_3, paste0(out.path, paste(unique(BZE3_trees_update_3$inv)[1], "LT_update_3", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(BZE3_trees_removed, paste0(out.path, paste(unique(BZE3_trees_update_3$inv)[1], "LT_removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
}



stop("this is where notes of  height calculations start")


# NOTES -------------------------------------------------------------------


ggplot(data = HBI_trees_update_3 %>% mutate(org_noorg = ifelse(plot_ID == 140010, "org", "min")) %>% filter(startsWith(bot_name, "Betula") & H_method == "sampled"))+ 
  geom_jitter(aes(x = DBH_cm, y = H_m, colour = as.factor(org_noorg)))



# old way to calculate H_g, D-G considering concetrtric sampling circuits, which si, according to sebastan Schnell not necesarry
# this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
Hg_Dg_trees_total.df <- trees_total %>%                              
  group_by(inv, plot_ID, stand, C_layer, SP_code, CCS_r_m) %>%    # group by plot and species, canopy layer and sampling circuit to calcualte all paremeters needed 
  summarise(no_trees_CC = n(),
            BA_CC = sum(BA_m2),                        # sum up basal  area per sampling circuit to then reffer it to the hektar value of the respective circuit
            CC_A_ha = mean(plot_A_ha),                   # mean area in ha per sampling circuit
            BA_CC_m2_ha = BA_CC/CC_A_ha,               # calculating the BA hectare value of each tree species per c layer to account for the different sampling circuits
            no_trees_CC_ha = no_trees_CC/CC_A_ha,
            mean_DBH_mm_CC = mean(DBH_cm*10),          # calculate mean DBH per sampling circuit and species and C layer and plot 
            mean_H_m_CC = mean(na.omit(H_m))) %>%      # calculate mean height per sampling circuit and species and C layer and plot    
  group_by(inv, plot_ID, stand, C_layer, SP_code )%>%            # group by plot and species,  canopy layer and sampling circuit to calcualte dg, hg 
  summarize(no_trees_ha = sum(no_trees_CC_ha),                                # calculate number of trees per plot
            BA_m2_ha = sum(BA_CC_m2_ha),               # calculate sum of BA across all sampling circuit to account for represnation of different trees in the sampling circuits
            mean_DBH_mm = mean(mean_DBH_mm_CC),        # calculate mean of DBH across all sampling circuit to account for represnation of different trees in the sampling circuits
            mean_H_m = mean(mean_H_m_CC),              # calculate mean of height across all sampling circuit to account for represnation of different trees in the sampling circuits
            mean_BA_m2_tree = BA_m2_ha/no_trees_ha,
            H_g = sum(mean(na.omit(mean_H_m))*BA_m2_ha)/sum(BA_m2_ha),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
            mean_DBH_mm = mean(mean_DBH_mm),                           # mean diameter per species per canopy layer per plot
            D_g = ((sqrt((mean_BA_m2_tree/pi)))*2)*100) %>%               #  Durchmesser des Grundflächenmittelstammes; *100 to get from 1m -> 100cm    
  arrange(plot_ID)

