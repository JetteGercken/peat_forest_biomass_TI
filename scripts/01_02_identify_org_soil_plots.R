# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# comparing org soil types in bze2 database and those occuring in bze2 analysis 

# goal of this script is to compare if the plots with org soil types in bze 
# database and those occuring in bze2 analysis are identical


# peat land definition:  
# Trepel, Michael & Zeitz, Jutta & Pfadenhauer, Jörg & Jeschke, Lebrecht. (2017). Germany. 10.1127/mireseurope/2017/0001. : 
# The German soil Classification System uses the term ‘Moor’ (‘peatland’) for soil 
# layers with 30% organic matter/humus content and a minimum depth of 30 cm (AG Boden 2005)
# does that mean there has to be any horizont of 30cm depth that holds 30% SOM? 
# or does it have to be the upper on? 


# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# 0.2. outpath ------------------------------------------------------------
# from getwd() or here() onwards, this path leads to the folder where all data ist stored
out.path <- here("output/out_data//") 

# 0.3. import data --------------------------------------------------------
# soil types database
soil_types_bze2_db <-  read.delim(file = here("data/input/vm_allgemeintab_2.csv"), sep = ",", dec = ".") 
# soil types 
soil_profiles_bze2_db <- read.delim(file = here("data/input/vm_minboden_profil_2.csv"), sep = ",", dec = ".")
  
# organic soil types analysis from Eric Grüneberg
org_soils_analysis <-  read.delim(file = here("data/input/org_soil_types_BZE2.csv"), sep = ",", dec = ".")

# organic soil types from Carina Peatzel
org_soils_paetzel <- read.delim(file = here("data/input/org_plots_BZE2_BZE1_Paetzel.csv"), sep = ",", dec = ".")

element_bze2_db <- read.delim(file = here("data/input/vm_minboden_element_gehalte_2.csv"), sep = ",", dec = ".") 



# 0.4. data prep & clearing -----------------------------------------------
# remove "empty" columns
soil_profiles_bze2_db <- soil_profiles_bze2_db[soil_profiles_bze2_db$horizont != -9 & soil_profiles_bze2_db$inventur == 2, ]
org_soils_analysis <- org_soils_analysis %>% filter(plot_inventur == 2 & !is.na(plot_bodtyp))


# 1. identify organic sites ---------------------------------------------------------
# possible names of organic soil types according to KA5
org_soil_types <- c(#"GH", "GM", "GH", 
                    "HH", "HN", "KH"
                    #, "KV",  "KM", "SGm" 
                    )
# possible names of organic horizonts
org_horizonts <- c("Aa", "H")


# 1.1. plots with org soil types -----------------------------------------
org_soils_types_db <- soil_types_bze2_db %>%
  filter(grepl(paste(org_soil_types, collapse = "|"), bodentyp_2, text, ignore.case = F) & erhebjahr_2 > 2000) %>% 
  select(bfhnr_2, bodentyp_2) %>% 
  rename(., bfhnr = bfhnr_2) %>% 
  rename(., plot_bodtyp = bodentyp_2) %>% 
  distinct()

org_plots_according_to_soiltype <- org_soils_types_db

# 1.2. plots with characteristics defining them organic -------------------

# 1.2.1. organic layer thickness ---------------------------------------------------
# if organic layer is Aa the horizont has to be at least 10cm thick
# if organic layer contains H the horizont has to be at least 30cm thick? --> not true look at niedermoorgley :/ 
org_horizonts_db <- 
  soil_profiles_bze2_db %>% 
semi_join(., 
  # for thickness we need: upper boarder of min horizin tnumber - lower boarder of max horizontnummer
soil_profiles_bze2_db %>% filter(
  # select organic horizonts only 
  grepl(paste(org_horizonts, collapse = "|"), horizont, text, ignore.case = F) & 
    # select bze2 data only
    inventur == 2) %>% 
  mutate(org_hori_type = ifelse(grepl("H", horizont, text, ignore.case = F) == T, "H", "Aa")) %>% 
  # select only max horizint number per plot among organic horizonts 
  group_by(bfhnr, inventur, org_hori_type) %>% 
  # select hghest and lowest boarder of organic horizionts
  summarise(begin_org_hori  = min(ot), 
            end_org_hori  = max(ut)) %>% 
  # calcaulte depth of the AA or H horizont
  mutate(depth = end_org_hori - begin_org_hori ) %>%
  # select only those plots and horizonts where the organic horizont is thicket then 10cm
  # due to the grouping by plot and organic horizint type (Aa or H) the filter will only select plots, that have at least one, or both organic horizonts with a depth of 10cm
  filter(depth >= 10),
by = c("bfhnr", "inventur"))

org_plots_according_to_hori <- soil_types_bze2_db %>% 
  semi_join(., org_horizonts_db %>% select(bfhnr) %>% distinct(), by = c("bfhnr_2" = "bfhnr"))

nrow(org_plots_according_to_hori)

# 1.2.2. organic layer carbon content ---------------------------------------------------
# SOC should be between 8.6 and 12% C per mass unit
# as the carbon content is determined in depth steps while the horizonts are depterined in horizont steps 
# therefore we have turn horizont boarders into depth steps for every horizont no matter if its  organic or not
# depth steps
    # 0-5
    # 5-10
    # 10-30
    # 30 - 60
    # 60-90

# 
depth_class_hori_list <- vector("list", length = nrow(unique(soil_profiles_bze2_db[, c("bfhnr", "horinr", "inventur")])))
depth_class_hori_boarders_list <- vector("list", length = nrow(unique(soil_profiles_bze2_db[, c("bfhnr", "horinr", "inventur")])))
for (i in 1:nrow(unique(soil_profiles_bze2_db[, c("bfhnr", "horinr", "inventur")]))) {
  # i = 3261
  my.plot.id <- unique(soil_profiles_bze2_db[, c("bfhnr", "horinr", "inventur")])[ i ,"bfhnr"]      # plot id
  my.horinr <- unique(soil_profiles_bze2_db[, c("bfhnr", "horinr", "inventur")])[ i ,"horinr"]      # horizont number
  my.inv <- unique(soil_profiles_bze2_db[, c("bfhnr", "horinr", "inventur")])[i ,"inventur"]       # inventory
  my.horiname <-  soil_profiles_bze2_db[soil_profiles_bze2_db$bfhnr == my.plot.id & 
                                          soil_profiles_bze2_db$horinr == my.horinr & 
                                          soil_profiles_bze2_db$inventur == my.inv , "horizont"]  # horizont category/ name
 
  
  # select upper and lower boarders of the respective horizont
  my.ot <- soil_profiles_bze2_db[soil_profiles_bze2_db$bfhnr == my.plot.id & 
                                   soil_profiles_bze2_db$horinr == my.horinr & 
                                   soil_profiles_bze2_db$inventur == my.inv , "ot"]  # upper boarder
  my.ut <- soil_profiles_bze2_db[soil_profiles_bze2_db$bfhnr == my.plot.id & 
                                   soil_profiles_bze2_db$horinr == my.horinr & 
                                   soil_profiles_bze2_db$inventur == my.inv , "ut"]  # lower boarder
  
  # assing detpth class to upper and lower boarder of the respective horizont
  ot.depth.class <- depth_class(my.ot)
  ut.depth.class <- depth_class(my.ut)
  
  diff.depth.class = ut.depth.class - ot.depth.class
  
  # if there is no difference in the depth class of the horizonts upper and 
  # lower boarder, we assign the depth class of the boarders to the horizont
  # if however there is a difference we have to assign all the classes between the differences
  # by adding them to the upper depth class
  if(diff.depth.class == 0){
    depth.classes.hori.list <- ot.depth.class
  }else{
    depth.classes.hori.list <- seq(from = ot.depth.class , to = ut.depth.class, by= 1)
  }
  
  # export dataset
  depth_class_hori_list[[i]] <- as.data.frame(cbind(
    "bfhnr" = c(my.plot.id)
    , "horinr" = c(my.horinr)
    , "inventur" = c(my.inv)
    , "horizont" = c(my.horiname)
    , "hori_depth_class" = depth.classes.hori.list
  ))
  
  # df with upper and lowe boarders and respective depth step in one column for wighting of carbon content
  depth_class_hori_boarders_list[[i]] <- as.data.frame(cbind(
    "bfhnr" = c(my.plot.id)
    , "horinr" = c(my.horinr)
    , "inventur" = c(my.inv)
    , "horizont" = c(my.horiname)
    , "hori_boarder_name" = c("ot", "ut")
    ,  "hori_boarder" = c(my.ot, my.ut)
    , "hori_depth_class" = c(ot.depth.class, ut.depth.class)
  ))
  
  
  print(paste0(my.plot.id, my.horiname, i))
  
}
depth_class_hori <- as.data.frame(rbindlist(depth_class_hori_list))
depth_class_hori_boarders <- as.data.frame(rbindlist(depth_class_hori_boarders_list))


# calculate how many cm the horizont reaches into the depth class
hori_with_partial_depth_classes <- 
# check for horizont boarders that extend beyong one depth step: 
depth_class_hori_boarders %>%
  # filter for plots where ot and ut do not belong to the same depth class --> where there are more then 1 depth class per horizont 
 semi_join(depth_class_hori %>% 
                  group_by(  bfhnr, horinr, inventur, horizont ) %>% 
                  summarise(n_depth_class_per_hori = n()) %>% 
                  filter(n_depth_class_per_hori > 1), 
           by = c("bfhnr", "horinr", "inventur", "horizont") ) %>% 
  mutate(across(c("bfhnr", "horinr", "inventur", "hori_depth_class", "hori_boarder"), as.numeric)) %>% 
  # join in horizont depth
  left_join(., soil_profiles_bze2_db %>%  mutate(depth_hori = ut - ot) %>% 
              select(bfhnr, inventur, horinr, horizont, depth_hori), 
            by = c("bfhnr", "horinr", "inventur", "horizont") ) %>% 
  # join in depth step info (upper lower boarder of depth class etc.)
  left_join(element_bze2_db %>% 
              mutate(TF_depth_class = depth_class(ut), # assign depth class factor/ name per depth class 
                     depth_TF = ut- ot) %>% # calculate depth of depth class
              # adjust names of deoth class boarder for join 
              rename("ut_TF" = "ut") %>%  
              rename("ot_TF" = "ot") %>% 
              select(bfhnr, inventur, TF_depth_class, depth_TF,  ot_TF, ut_TF), 
            by = c("bfhnr", "inventur", "hori_depth_class" = "TF_depth_class")) %>% 
  # calcualte how much of the depth class is occubied by the horizont 
  # if it´s a upper horizont boarder we calcualte the difference between the lower boarder of the depth step and the hori depth, 
  # because we want to know how much "above" the "lower" boarder of the depth step the horizont is located
  # while for lower horizont boarders we calcualte the difference between the 
  # hori depth and the upper boarder of the depth step, because we want to know how much "further" then the upper boarter the orizont reaches 
  mutate(diff_hori_TF_boarder_cm = ifelse(hori_boarder_name == "ot", abs(hori_boarder-ut_TF), abs(hori_boarder -ot_TF)),
         # calcualte the relative depth that is ooccuried by the horizonts part that "reaches"
         # into the depth class, compared to the total depth ot the horizont class
         relative_depth_hori_TF = diff_hori_TF_boarder_cm/depth_TF, 
         relative_depth_hori_hori = diff_hori_TF_boarder_cm/ depth_hori) # proportion of the horizonts total depth that is located in the "other " depth class




 
mean_SOC_horizont <- 
soil_profiles_bze2_db %>% 
  left_join(., depth_class_hori %>% 
              mutate(across(c("bfhnr", "horinr", "inventur", "hori_depth_class"), as.numeric)), 
            by = c("bfhnr", "horinr", "inventur", "horizont")) %>% 
  left_join(., hori_with_partial_depth_classes %>% 
              select(bfhnr, horinr, inventur, horizont, hori_depth_class, diff_hori_TF_boarder_cm, relative_depth_hori_TF, relative_depth_hori_hori), 
            by = c("bfhnr", "horinr", "inventur", "horizont", "hori_depth_class")) %>% 
  left_join(., element_bze2_db %>% 
              mutate(SOC =( m_ea_corg2/1000), # change unit of SOC content from g/kg to percent by divinding by 1000
                     hori_depth_class = depth_class(ut)) %>%  
              select(bfhnr, inventur, hori_depth_class, SOC),
            by = c("bfhnr", "inventur", "hori_depth_class")) %>% 
  # all NAs for the column with the proportion of the depth step occupied by the
  # respective horizont (occupied_depth_hori_TF) have to be repleaced by 1 as the repsetvie horizont doen´t cross a depth class boarder 
  # the difference between the hori boarder and tf is set to 0 for all those horizints that don cross the boarder of a depth class
  mutate(diff_hori_TF_boarder_cm = ifelse(is.na(diff_hori_TF_boarder_cm), as.numeric(0), diff_hori_TF_boarder_cm), 
         relative_depth_hori_hori = ifelse(is.na(relative_depth_hori_hori), as.numeric(1), relative_depth_hori_hori),
         relative_depth_hori_TF = ifelse(is.na(relative_depth_hori_TF), as.numeric(1), relative_depth_hori_TF)) %>% 
  group_by(bfhnr, horinr, inventur, horizont) %>% 
  # now we prepare to calcualte the mean, weighed by the proportion of each depth step that is occupied by a horizont
  summarise(n_depth_classes_weighed = sum(relative_depth_hori_TF), # sum of depth classes (1 = whole depth class occupied, <1 = parts of the depth class are occupied)
            n_depth_classes_unweighed = n(), # number of depth classes a horizont touches
            sum_SOC_hori = sum(SOC)) %>% 
  # calculate mean SOC content by horizont 
  mutate(weighed_mean_SOC_hori = sum_SOC_hori/ n_depth_classes_weighed, 
         unweighed_mean_SOC_hori = sum_SOC_hori/ n_depth_classes_unweighed) %>% 
  left_join(., soil_profiles_bze2_db %>% select(bfhnr, horinr, inventur, horizont, ot, ut), 
            by = c("bfhnr", "horinr", "inventur", "horizont")) 



mean_SOC_horizont_2 <- 
soil_profiles_bze2_db %>% 
  left_join(., depth_class_hori %>% 
              mutate(across(c("bfhnr", "horinr", "inventur", "hori_depth_class"), as.numeric)), 
            by = c("bfhnr", "horinr", "inventur", "horizont")) %>% 
  left_join(., hori_with_partial_depth_classes %>% 
              select(bfhnr, horinr, inventur, horizont, hori_depth_class, diff_hori_TF_boarder_cm, relative_depth_hori_TF, relative_depth_hori_hori), 
            by = c("bfhnr", "horinr", "inventur", "horizont", "hori_depth_class")) %>% 
  left_join(., element_bze2_db %>% 
              mutate(SOC_TF =( m_ea_corg2/1000), # change unit of SOC content from g/kg to percent by divinding by 1000
                     hori_depth_class = depth_class(ut)) %>%  
              # adjust names of deoth class boarder for join 
              rename("ut_TF" = "ut") %>%  
              rename("ot_TF" = "ot") %>% 
              select(bfhnr, inventur, hori_depth_class, SOC_TF, m_trd_fbv2, ot_TF, ut_TF),
            by = c("bfhnr", "inventur", "hori_depth_class")) %>% 
  # all NAs for the column with the proportion of the depth step occupied by the
  # respective horizont (occupied_depth_hori_TF) have to be repleaced by 1 as the repsetvie horizont doen´t cross a depth class boarder 
  # the difference between the hori boarder and tf is set to 0 for all those horizints that don cross the boarder of a depth class
  mutate(diff_hori_TF_boarder_cm = ifelse(is.na(diff_hori_TF_boarder_cm), as.numeric(0), diff_hori_TF_boarder_cm), 
         relative_depth_hori_hori = ifelse(is.na(relative_depth_hori_hori), as.numeric(1), relative_depth_hori_hori), # proportion of the horizonts deoth thats in the TF partial_hori/ total_depth_hori
         relative_depth_hori_TF = ifelse(is.na(relative_depth_hori_TF), as.numeric(1), relative_depth_hori_TF),   # propotion of the TF thats occupied by the hori partial_hori/total_depth_TF
         # weigh SOC content of the TF by the proportion of the horizontas dfeoth that "reaches" into the depth class
         weighted_SOC_JB = relative_depth_hori_hori*SOC_TF, 
         depth_TF = ut_TF - ot_TF,
         depth_hori_occupies_in_TF = ifelse(diff_hori_TF_boarder_cm == 0, depth_TF , diff_hori_TF_boarder_cm),
         # calcualte the fine soil stock occupied by the horizont in the depth class: 
         # trd*depht of depth_class = total stock TF -->  total stock Tf/depth TF = stock/cm TF --> stock per cm TF * depth Hori occupies in TF = stock hori occupies in TF
         FB_t_ha_hori_in_TF_MM = m_trd_fbv2*depth_hori_occupies_in_TF*100, #((m_trd_fbv2*depth_TF)/depth_TF)*depth_hori_occupies_in_TF,
         SOC_t_ha_hori_in_TF_MM = FB_t_ha_hori_in_TF_MM*SOC_TF, 
         FB_t_ha_hori_in_TF_EG = (m_trd_fbv2*depth_TF)*relative_depth_hori_hori*100,
         SOC_t_ha_hori_in_TF_EG = FB_t_ha_hori_in_TF_EG*SOC_TF
         )%>% 
   select(bfhnr, inventur, erhebjahr, horinr,  ot , ut, horizont, hori_depth_class, ot_TF, ut_TF,diff_hori_TF_boarder_cm ,relative_depth_hori_TF, relative_depth_hori_hori , SOC_TF, weighted_SOC_JB, 
          depth_TF, depth_hori_occupies_in_TF, FB_t_ha_hori_in_TF_MM, SOC_t_ha_hori_in_TF_MM, FB_t_ha_hori_in_TF_EG, SOC_t_ha_hori_in_TF_EG) %>% 
   group_by(bfhnr, inventur ,erhebjahr, horinr , horizont) %>% 
  # # now we prepare to calcualte the mean, weighed by the proportion of each depth step that is occupied by a horizont
   summarise(n_TF_per_hori = n(),
             SOC_t_ha_hori_MM = sum(SOC_t_ha_hori_in_TF_MM), # MM = Marius Möller Method
             FB_t_ha_hori_MM = sum(FB_t_ha_hori_in_TF_MM), 
             SOC_t_ha_hori_EG = sum(SOC_t_ha_hori_in_TF_EG),  # EG = Erik Grüneberg Method
             FB_t_ha_hori_EG = sum(FB_t_ha_hori_in_TF_EG),
             SOC_weighed_JB = sum(weighted_SOC_JB)) %>%       # JB = Judith Bilefeldt Method
  mutate(SOC_weighed_MM = SOC_t_ha_hori_MM/FB_t_ha_hori_MM, 
         SOC_weighed_EG = SOC_t_ha_hori_EG/FB_t_ha_hori_EG) %>% 
   left_join(., soil_profiles_bze2_db %>% select(bfhnr, horinr, inventur, horizont, ot, ut), 
             by = c("bfhnr", "horinr", "inventur", "horizont"))




# find horizonts with depth class with > 15% carbon mass-% in Aa or H 
mean_SOC_org_horizont <- 
soil_profiles_bze2_db %>%
  # select only plots that are organic by their horizont next
  semi_join(., org_plots_according_to_hori, by = c("bfhnr" = "bfhnr_2")) %>% 
  # assign org horizont groups: every hori containing an H gets the group H and every hori containing Aa becomes group Aa 
  mutate(org_hori_type = case_when(grepl("H", horizont, text, ignore.case = F) == T ~ "H",
                                   grepl("Aa", horizont, text, ignore.case = F) == T ~"Aa", 
                                   TRUE ~ horizont)) %>% 
  left_join(., depth_class_hori %>% 
              mutate(across(c("bfhnr", "horinr", "inventur", "hori_depth_class"), as.numeric)), 
            by = c("bfhnr", "horinr", "inventur", "horizont")) %>% 
  left_join(., hori_with_partial_depth_classes %>% 
              select(bfhnr, horinr, inventur, horizont, hori_depth_class, relative_depth_hori_TF), 
            by = c("bfhnr", "horinr", "inventur", "horizont", "hori_depth_class")) %>% 
  left_join(., element_bze2_db %>% 
              mutate(SOC =( m_ea_corg2/1000), # change unit of SOC content from g/kg to percent by divinding by 1000
                     hori_depth_class = depth_class(ut)) %>%  
              select(bfhnr, inventur, hori_depth_class, SOC),
            by = c("bfhnr", "inventur", "hori_depth_class")) %>% 
  # all NAs for the column with the proportion of the depth step occupied by the
  # respective horizont (occupied_depth_hori_TF) have to be repleaced by 1 as the repsetvie horizont doen´t cross a depth class boarder 
  mutate(relative_depth_hori_TF = ifelse(is.na(relative_depth_hori_TF), as.numeric(1), relative_depth_hori_TF)) %>% 
  group_by(bfhnr, inventur, org_hori_type) %>% 
  # now we prepare to calcualte the mean, weighed by the proportion of each depth step that is occupied by a horizont
  summarise(n_depth_classes_weighed = sum(relative_depth_hori_TF), # sum of depth classes (1 = whole depth class occupied, <1 = parts of the depth class are occupied)
            n_depth_classes_unweighed = n(), # number of depth classes a horizont touches
            sum_SOC_hori = sum(SOC)) %>% 
  # calculate mean SOC content by horizont 
  mutate(weighed_mean_SOC_hori = sum_SOC_hori/ n_depth_classes_weighed, 
         unweighed_mean_SOC_hori = sum_SOC_hori/ n_depth_classes_unweighed) 



# 1.2.3. horizont depth and carbon content --------------------------------
org_plots_according_to_hori_SOC <- 
  soil_types_bze2_db %>% 
  semi_join(., 
            soil_profiles_bze2_db %>%
  # select only plots that are organic by their horizont next
  semi_join(., org_plots_according_to_hori, by = c("bfhnr" = "bfhnr_2")) %>% 
  # assign org horizont groups: every hori containing an H gets the group H and every hori containing Aa becomes group Aa 
  mutate(org_hori_type = case_when(grepl("H", horizont, text, ignore.case = F) == T ~ "H",
                                   grepl("Aa", horizont, text, ignore.case = F) == T ~"Aa", 
                                   TRUE ~ horizont)) %>% 
  left_join(., mean_SOC_org_horizont %>% 
              select( bfhnr, inventur, org_hori_type, unweighed_mean_SOC_hori), 
            by = c("bfhnr", "inventur", "org_hori_type")) %>% 
  filter(org_hori_type == "H" & unweighed_mean_SOC_hori > 0.15 |
           org_hori_type == "Aa" & unweighed_mean_SOC_hori > 0.087),
  by = c("bfhnr_2" = "bfhnr")) %>% distinct()




# 4. filter for organic soil types only -----------------------------------

# 4.1. filter for organic soil types --------------------------------------
# offical bze2 database
# select only organic soil types from database dataset
org_soils_types_db <- soil_types_bze2_db %>%
  filter(grepl(paste(org_soil_types, collapse = "|"), bodentyp_2, text, ignore.case = F) ) %>%  # & erhebjahr_2 > 2000
  select(bfhnr_2, bodentyp_2) %>% 
  rename(., bfhnr = bfhnr_2) %>% 
  rename(., plot_bodtyp = bodentyp_2) %>% 
  distinct()

# add category "organic" minearal" to soil profile dataset
soil_types_bze2_db <- soil_types_bze2_db %>%
  mutate(min_org = ifelse(grepl(paste(org_soil_types, collapse = "|"), bodentyp_2, text, ignore.case = F), "org", "min") ) %>%  # & erhebjahr_2 > 2000
  distinct()

# eric grünebergs analysis database
org_soils_analysis <- org_soils_analysis %>% 
  filter(grepl(paste(org_soil_types, collapse = "|"), plot_bodtyp, text, ignore.case = F) & plot_inventur == 2) %>% 
  select(bfhnr, plot_bodtyp ) %>% distinct()


# 4.2. comparisson org soil types bd and analysis --------------------------------------------------------

# 4.2.1. bze2 database to erics data --------------------------------------
anti_join(org_plots_according_to_soiltype, org_soils_analysis, by = c("bfhnr")) #,  "plot_bodtyp"))
# plot present in org_soils_types_db but not in soil_types_analysis (EG) 
#   bfhnr    bodentyp_2
# 1 70109         HN
# 2 80058      HN-SG
# 3 80206         KV

nrow(org_plots_according_to_soiltype)
# numrber of rows: 38

anti_join(org_soils_analysis %>% select(bfhnr, plot_bodtyp) %>% distinct(), org_plots_according_to_soiltype,  by = c("bfhnr")) #,  "plot_bodtyp"))
# plot present in org_soils_analysis but not in org_soils_db 
# bfhnr      plot_bodtyp
# 1  30186         SSm
# 2  30519         KVu
# 3  30602         SSm
# 4  30634       GG-YU
# 5  80214          RR
# 6  80264         SGn
# 7  90875       BB-RN
# 8 120159          GG
# 9 140010       PP-GG

nrow(org_soils_analysis %>% select("bfhnr") %>% distinct())
# numrber of rows: 44

# these plots differ from 
org_soil_types_comp_db_EG <- 
  full_join(org_soils_analysis, org_plots_according_to_soiltype,  by = c("bfhnr")) %>% 
  mutate(same_same_but_different = ifelse(plot_bodtyp.x != plot_bodtyp.y | 
                                            is.na(plot_bodtyp.x) & !is.na(plot_bodtyp.y)| 
                                            !is.na(plot_bodtyp.x) & is.na(plot_bodtyp.y), "different", "same")) 



# 4.2.3. bze databse by soil types vs. bze2 org plots due to horizont & SOC chracteristics  -------------
# compare bze2 org soil types to org soil plots idientified by horziont characteristics
anti_join(org_plots_according_to_soiltype, org_plots_according_to_hori_SOC,  by = c("bfhnr" = "bfhnr_2") ) #,  "plot_bodtyp"))
# plot present in org_plots_according_to_soiltype but not in org_soils_db 
#    bfhnr    plot_bodtyp
# 1   30123       YE-HN
# 2   30148          GM
# 3   30625       KV-KM
# 4   30646          GH
# 5   70109          HN
# 6   80058       HN-SG
# 7   80112          KV
# 8   80206          KV
# 9   90594          GM
# 10  90865          GM
# 11  90877          GM
# 12 120080          GH
# 13 120098          GH
# 14 120119          GM

anti_join(org_plots_according_to_hori_SOC, org_plots_according_to_soiltype,  by = c("bfhnr_2" = "bfhnr") ) %>% select(bfhnr_2 , bodentyp_2 )
# plot present in org_plots_according_to_hori but not in org_plots_according_to_soiltype 
#      bfhnr_2    bodentyp_2
# 1   30186         SS
# 2   30519         LF
# 3   30602         SS
# 4   90875      BB-RN
# 5  120159         GG


nrow(org_plots_according_to_hori)
# numrber of rows: 51

# the following plots do not qualy as organic due to their organic horizont thickness 
# tho their soil type indicates they are organic
soil_profiles_bze2_db %>% 
  semi_join(., anti_join(org_plots_according_to_soiltype, org_plots_according_to_hori,  by = c("bfhnr" = "bfhnr_2") ), by =  "bfhnr" )


org_soil_types_comp_db_hori <- 
  full_join(org_plots_according_to_soiltype, org_plots_according_to_hori %>% select(bfhnr_2, bodentyp_2), by = c("bfhnr" = "bfhnr_2")) %>% 
  mutate(same_same_but_different = ifelse(plot_bodtyp != bodentyp_2 | 
                                            is.na(plot_bodtyp) & !is.na(bodentyp_2)| 
                                            !is.na(plot_bodtyp) & is.na(bodentyp_2), "different", "same"))

# 4.2.2. bze databse by soil types vs. bze2 org plots due to horizont & SOC chracteristics  -------------
# compare bze2 org soil types to org soil plots idientified by horziont characteristics
anti_join(org_plots_according_to_soiltype, org_plots_according_to_hori_SOC,  by = c("bfhnr" = "bfhnr_2") ) #,  "plot_bodtyp"))
# plot present in org_plots_according_to_soiltype but not in org_soils_db 
#    bfhnr    plot_bodtyp
# 1  90877          GM
# 2 120080          GH

anti_join(org_plots_according_to_hori_SOC, org_plots_according_to_soiltype,  by = c("bfhnr_2" = "bfhnr") ) %>% select(bfhnr_2 , bodentyp_2 )
# plot present in org_plots_according_to_hori but not in org_soils_types_db 
#      bfhnr_2    bodentyp_2
# 1    30186         SS
# 2    30519         LF
# 3    30602         SS
# 4    70096         GG
# 5    90578         GG
# 6    90755         SS
# 7    90787      SS-GG
# 8    90875      BB-RN
# 9   120004         RZ
# 10  120127         BB
# 11  120129      RQ-BB
# 12  120137         PP
# 13  120150      GG-PP
# 14  120159         GG
# 15  120170         BB




# 4.2.3. compare only hori filter vs. hori & SOC filter ------------------------------
anti_join(org_plots_according_to_hori, org_plots_according_to_hori_SOC, by = "bfhnr_2")
# 22 plots are excluded when instead of hori depth we also filter for SOC content  

nrow(org_plots_according_to_hori) # 51
nrow(org_plots_according_to_hori_SOC) # 29





# 6. data export ----------------------------------------------------------
write.csv(soil_types_bze2_db, here(out.path, "soils_types_profil_db.csv"), row.names = FALSE)


