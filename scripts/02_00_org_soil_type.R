# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# comparing org soil types in bze2 database and those occuring in bze2 analysis 

# goal of this script is to compare if the plots with org soil types in bze2 
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


# 0.2 connect to database -------------------------------------------------
# name of database
# db <- 'bze2'  #provide the name of your db
# # host of database: thuenen server --> VPN proably need to be activated 
# host_db <- '134.110.100.88'   # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
# # database port or any other port specified by the DBA
# db_port <- '5432'  # this info you can find in the PGadmin properties of the server
# # database username
# db_user <- 'hgercken'  # 'henriette.gercken@thuenen.de'  
# # database password
# db_password <-  'Ao1ieDahthaheoPh' # 'Jette$Thuenen_2024'

# 0.2.1. coencction details --------------------------------------------------
db_name <- "bze2"
db_server <- "134.110.100.88"
db_port <- "5432"
db_user <-  rstudioapi::askForPassword(prompt = "Please enter your username")
my_db_password <- rstudioapi::askForPassword(prompt = "Please enter your password")

# 0.2.2. enable connection ----------------------------------------------------------
con <- sqlconnection(db_name, db_server,db_port, db_user, my_db_password)

# 0.2.3. import dataset ----------------------------------------------------------------
# 0.2.3.1. soil data ------------------------------------------------------
# names of the tables we want to import to our raw data folder: 
soil_data_table_names <- c("vm_allgemeintab_2", "vm_minboden_profil_2", "vm_minboden_element_gehalte_2")
for (i in 1:length(data_table_names)) {
  # i = 1
  # get table name
  my.table.name <- data_table_names[i]
  # set schema name
  my.schema.name <- "bze2_extern"
  # set database name 
  db_name <- 'bze2'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw"), "/", my.table.name, ".csv"), row.names = FALSE)
}

## copy soil data files from raw data general to input general fo
# copy everything imported from database from raw folder to input folder
# 1. create raw data path: 
raw.path.code <- paste0(here("data/raw"), "/")
# 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
code.in.files <- list.files(raw.path.code) 
# 3. create input path
input.path.code <- paste0(here("data/input"), "/")
# copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path.code, code.in.files),
          to = paste0(input.path.code, code.in.files),
          overwrite = TRUE)

# 0.2.3.2. forest data ------------------------------------------------------
# names of the tables we want to import to our raw data folder: 
forest_data_table_names <- c("b2beab", "tit_1", "be", "beab", "be_waldraender", "bej", "bejb", "bedw", "bedw_liste", "punkt", "HBI_location")
for (i in 1:length(forest_data_table_names)) {
  # i = 1
  # get table name
  my.table.name <- forest_data_table_names[i]
  # set schema name
  my.schema.name <- "data"
  # set database name 
  db_name <- 'bze3_altdaten'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw"), "/", my.table.name, ".csv"), row.names = FALSE)
}

## copy soil data files from raw data general to input general fo
# copy everything imported from database from raw folder to input folder
# 1. create raw data path: 
raw.path.code <- paste0(here("data/raw"), "/")
# 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
code.in.files <- list.files(raw.path.code) 
# 3. create input path
input.path.code <- paste0(here("data/input"), "/")
# copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path.code, code.in.files),
          to = paste0(input.path.code, code.in.files),
          overwrite = TRUE)








# 0.2.3.3. code data ------------------------------------------------------
# names of the tables we want to import to our raw data folder: 
code_table_names <- c("neu_x_ld", "neu_k_tangenz", "x_bart_neu")
for (i in 1:length(code_table_names)) {
  # i = 1
  # get table name
  my.table.name <- code_table_names[i]
  # set schema name
  my.schema.name <- "code"
  # set database name 
  db_name <- 'bze3_altdaten'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw"), "/", my.table.name, ".csv"), row.names = FALSE)
}

## copy soil data files from raw data general to input general fo
# copy everything imported from database from raw folder to input folder
# 1. create raw data path: 
raw.path.code <- paste0(here("data/raw"), "/")
# 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
code.in.files <- list.files(raw.path.code) 
# 3. create input path
input.path.code <- paste0(here("data/input"), "/")
# copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path.code, code.in.files),
          to = paste0(input.path.code, code.in.files),
          overwrite = TRUE)




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

# 1. identify organic sites ---------------------------------------------------------
# possible names of organic soil types according to KA5
org_soil_types <- c("GH", "GM", "GH", "HH", "HN", "KH", "KV",  "KM", "SGm" )
org_horizonts <- c("Aa", "H")


# 1.1. plots with org soil types -----------------------------------------
org_soils_types_db <- soil_types_bze2_db %>%
  filter(grepl(paste(org_soil_types, collapse = "|"), bodentyp_2, text, ignore.case = F) & erhebjahr_2 > 2000) %>% 
  select(bfhnr_2, bodentyp_2) %>% 
  rename(., bfhnr = bfhnr_2) %>% 
  rename(., plot_bodtyp = bodentyp_2) %>% 
  distinct()



# 1.2. plots with characteristics defining them organic -------------------

# 1.2.1. organic layer ---------------------------------------------------
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


# try to turn horizints into depth steps
org_horizonts_db %>% 
  mutate(org_hori_type = ifelse(grepl("H", horizont, text, ignore.case = F) == T, "H", "Aa"))

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
  filter(depth >= 10)


org_plots_according_to_hori <- soil_types_bze2_db %>% 
 semi_join(., org_horizonts_db %>% select(bfhnr) %>% distinct(), by = c("bfhnr_2" = "bfhnr"))



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

# eric grünebergs analysis database
org_soils_analysis <- org_soils_analysis %>% 
  filter(grepl(paste(org_soil_types, collapse = "|"), plot_bodtyp, text, ignore.case = F) & plot_inventur == 2) %>% 
  select(bfhnr, plot_bodtyp ) %>% distinct()


# 4.2. comparisson org soil types bd and analysis --------------------------------------------------------

# 4.2.1. bze2 database to erics data --------------------------------------
anti_join(org_soils_types_db, org_soils_analysis, by = c("bfhnr")) #,  "plot_bodtyp"))
# plot present in org_soils_types_db but not in soil_types_analysis (EG) 
#   bfhnr    bodentyp_2
# 1 70109         HN
# 2 80058      HN-SG
# 3 80206         KV

nrow(org_soils_types_db)
# numrber of rows: 38

anti_join(org_soils_analysis, org_soils_types_db,  by = c("bfhnr")) #,  "plot_bodtyp"))
# plot present in org_soils_analysis but not in org_soils_db 
# bfhnr      plot_bodtyp
# 1 30519         KVu

nrow(org_soils_analysis)
# numrber of rows: 36

# these plots differ from 
org_soil_types_comp_db_EG <- 
  full_join(org_soils_analysis, org_soils_types_db,  by = c("bfhnr")) %>% 
  mutate(same_same_but_different = ifelse(plot_bodtyp.x != plot_bodtyp.y | 
                                            is.na(plot_bodtyp.x) & !is.na(plot_bodtyp.y)| 
                                            !is.na(plot_bodtyp.x) & is.na(plot_bodtyp.y), "different", "same")) 



# 4.2.2. bze databse by soil types vs. bze2 org plots due to horizont chracteristics  -------------
# compare bze2 org soil types to org soil plots idientified by horziont characteristics
anti_join(org_soils_types_db, org_plots_according_to_hori,  by = c("bfhnr" = "bfhnr_2") ) #,  "plot_bodtyp"))
# plot present in org_soils_types_db but not in org_soils_db 
#    bfhnr    plot_bodtyp
# 1  90877          GM
# 2 120080          GH

anti_join(org_plots_according_to_hori, org_soils_types_db,  by = c("bfhnr_2" = "bfhnr") ) %>% select(bfhnr_2 , bodentyp_2 )
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


nrow(org_plots_according_to_hori)
# numrber of rows: 51

# the following plots do not qualy as organic due to their organic horizont thickness 
# tho their soil type indicates they are organic
soil_profiles_bze2_db %>% 
  semi_join(., anti_join(org_soils_types_db, org_plots_according_to_hori,  by = c("bfhnr" = "bfhnr_2") ), by =  "bfhnr" )


org_soil_types_comp_db_hori <- 
  full_join(org_soils_types_db, org_plots_according_to_hori %>% select(bfhnr_2, bodentyp_2), by = c("bfhnr" = "bfhnr_2")) %>% 
  mutate(same_same_but_different = ifelse(plot_bodtyp != bodentyp_2 | 
                                            is.na(plot_bodtyp) & !is.na(bodentyp_2)| 
                                            !is.na(plot_bodtyp) & is.na(bodentyp_2), "different", "same"))





# 5. filter by carbon content in depth steps ------------------------------
element_bze2_db %>% mutate(corg_percent = (m_ea_corg2/1000)*100, 
                           thicknes = ut-ot) %>% filter(corg_percent >= 15 & thicknes >= 10) %>% 
  

