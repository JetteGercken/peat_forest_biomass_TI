# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken

# goal of this script is to aquire data from the national soil invengotry database to then carryout the subsequent steps of the analysis 


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
# db_name <- "bze2"
# db_server <- "134.110.100.88"
# db_port <- "5432"
# db_user <-  rstudioapi::askForPassword(prompt = "Please enter your username")
# my_db_password <- rstudioapi::askForPassword(prompt = "Please enter your password")

# 0.2.2. enable connection ----------------------------------------------------------
con <- sqlconnection(db_name, db_server,db_port, db_user, my_db_password)

# 0.2.3. import dataset ----------------------------------------------------------------
# 0.2.3.1. soil data ------------------------------------------------------
# names of the tables we want to import to our raw data folder: 
soil_data_table_names <- c("vm_allgemeintab_2", "vm_minboden_profil_2", "vm_minboden_element_gehalte_2", "vm_lokation_hbi")
for (i in 1:length(soil_data_table_names)) {
  # i = 1
  # get table name
  my.table.name <- soil_data_table_names[i]
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


# 0.2.3.2. forest data ------------------------------------------------------
# names of the tables we want to import to our raw data folder: 
forest_data_table_names <- c("tit", "be", "beab", "be_waldraender", "bej", "bejb", "be_totholz_punkt", "be_totholz_liste")#, "punkt")
for (i in 1:length(forest_data_table_names)) {
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


# 0.2.3.3. code data ------------------------------------------------------
# names of the tables we want to import to our raw data folder: 
code_table_names <- c("x_ld", "k_tangenz", "x_bart")
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

# 1. create raw data path: 
raw.path <- paste0(here("data/raw"), "/")
# 2. get names of all files in the raw outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
input.files <- list.files(raw.path) 
# 3. create input path
input.path <- paste0(here("data/input"), "/")
# copy the files from one raw filder to the input other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path, input.files),
          to = paste0(input.path, input.files),
          overwrite = TRUE)



# 0.3.1. MOMOK data ----------------------------------------------------------------
# 0.3.1.1. import MOMOK data ----------------------------------------------------------------
# get momok data
# the path to the dataset on the netword folder is the following: \\wo-sfs-001v-ew\INSTITUT\a7forum\LEVEL I\BZE\Moormonitoring\Standorte\Lagemessungen... etc. 
# we have to extract the individual sheepts from the excel workshet and then turn them into csvs
raw.path.momok <- here::here("data/raw//")
# https://stackoverflow.com/questions/50238645/r-split-excel-workbook-into-csv-files
# get names of the sheets in the excel working sheet in the raw folder
sheets <- readxl::excel_sheets(paste0(raw.path.momok, "Lagemessungen_Bestandeserfassung_MoMoK_ges_2.xlsx"))
# get data from respective ecxel working sheet
dats <- lapply(sheets, readxl::read_excel, path =paste0(raw.path.momok, "Lagemessungen_Bestandeserfassung_MoMoK_ges_2.xlsx") )
# create filenames and path for the excel sheets to go to
sheet_names <- c("info_momok", "lokation_momok", "LT_momok", "RG_momok", "DW_momok")
filenames <- paste0(input.path, paste0(sheet_names, ".csv"))
# export tibbles separately
purrr::walk2(
  dats,            # List of tibbles
  sheet_names,     # Names of the tibbles
  ~ write.csv(.x, file = file.path(here("data/input"), paste0(.y, ".csv")), row.names = FALSE)
)



# 0.3.1.2. data wrangling momok ----------------------------------------------------
# tables we need to construct: 
# "tit", "be", "beab", "be_waldraender", "bej", "bejb", "be_totholz_punkt", "be_totholz_liste", vm_lokation_hbi


# 0.3.1.2.2. deal with double plots ----------------------------------------------------     
# tit includes followong columns:
# "bund_nr"    "team"       "datum"      "status"     "re_form"    "re_lage"    "neigung"    "exposition" "anmerkung"
lokation_momok <- read.delim(file = here(paste0(input.path, "lokation_momok.csv")), sep = ",", dec = ".") %>% filter(!(is.na(MoMoK_Nr)))
colnames(lokation_momok)
# [1] "MoMoK_Nr"              "Name"                  "Skizzenpunkt"          "Messausgangspunkt"     "Azimut..gon."          "Azimut...."            "Distanz..cm."          "Distanz..m."          
# [9] "Ursprung"              "Erläuterung"           "Dezimalgrad.N..WGS84." "Dezimalgrad.E..WGS84."

# there are plots that had 2 invenotries with two different plot centres for the same region
# these plots are goin to be treated as two separate plots by changing their pot id slightly
# we have to identify these plots (should have 2 sampling centres in momok_lokation)
# add the number of the sampling centre to the plot ID: xxx_1 and xxx_2

# 1. identify plots with more then one center 
double_plots_momok <- lokation_momok %>%
  # filter only plot centres
  filter(stringr::str_detect(Skizzenpunkt, "MB")) %>% 
  # select only plot ID and name of the refference point 
  select(MoMoK_Nr, Skizzenpunkt) %>% distinct() %>%
  # count number of center points registered for that plot
  group_by(MoMoK_Nr)%>% 
  summarise(n_centres = n()) %>% 
  filter(n_centres > 1)



# 0.3.1.2.3. LOKATION: hbi lokation momok ---------------------------------------------------- 
# to calculate the coordinates we have to progress itteratively
# since the structure is the following: we have a reference point (center) with a known geoloc 
# then there is one or more points of the same plot that are measured based on that ref point
# so their coordinates depend on joining in the corect ref point. 
# then there will be points who´s ref point is not the ref point itself but another coordinate 
# like corner or so
# these coordinates can only be calculated AFTER the ref-point(center)-to-direkt-ref-point-dependent-points is calcualted
# thus we will repeat the process as often as the max number of ref points (center) per plot, 
# which is: 6
max(lokation_momok %>% select(MoMoK_Nr, Ursprung) %>% distinct() %>% group_by(MoMoK_Nr) %>% summarise(n_refpoints = n()) %>% pull(n_refpoints))

# 0.3.1.2.3.1.  prepare lokation -----------------------------------------------------------------------------------
# correct lokation ursprung and skizzenpunkt
lokation_momok <- lokation_momok %>%
  # if there is no skizzenpunkt (name of the point in question) look at the notes column
  mutate(Skizzenpunkt = case_when(is.na(Skizzenpunkt) & str_detect(Erläuterung, "Referenzpunkt") ~ "RP", 
                                  is.na(Skizzenpunkt) & str_detect(Erläuterung, "Mittelpunkt") ~ "MB",
                                  is.na(Skizzenpunkt) & str_detect(Erläuterung, "Eckpunkt") ~ str_sub(Erläuterung, start= 9), 
                                  TRUE ~ Skizzenpunkt)) %>% 
  # harmonize abbreviations used in reference point column and point name column
  mutate(Ursprung = case_when(str_detect(Ursprung, "RP") ~ "RP", 
                              str_detect(Ursprung, "1") | str_detect(Ursprung, "2") ~ paste0("MB", Ursprung),
                              str_detect(Ursprung, "MB") & !(str_detect(Ursprung, "1") & !(str_detect(Ursprung, "2")))~ "MB",
                              str_detect(Ursprung, "ESO")~ "SE",
                              startsWith(Ursprung, "E") ~ str_sub(Ursprung, start = -2),
                              is.na(Ursprung) ~ "RP",
                              TRUE ~ Ursprung)) 

# 0.3.1.2.3.2.  calculate coordinates -----------------------------------------------------------------------------------
# punkt includes followong columns:
# bund_nr ld ld_bze ld_wze bwi_tnr bwi_eck eu_wze srid_ist   istre   istho raster_8x8 raster_16x16 raster hoehenn
# lokation Hbi contains: bfhnr  rw_med  hw_med
# iteration 1
vm_lokation_momok_1 <- lokation_momok %>%
  # join in the the respective reference point coordinates  for the center of the forest inventory by "ursprung"
  left_join(., lokation_momok %>% select( "MoMoK_Nr",  "Skizzenpunkt",  "Dezimalgrad.N..WGS84.", "Dezimalgrad.E..WGS84.") %>% 
              rename("ref_northing" = "Dezimalgrad.N..WGS84.") %>% 
              rename("ref_easting" = "Dezimalgrad.E..WGS84."), 
            by = c("MoMoK_Nr", "Ursprung" = "Skizzenpunkt")) %>% 
  mutate(across(c("Dezimalgrad.E..WGS84.", "Dezimalgrad.N..WGS84.", "ref_northing", "ref_easting", "Distanz..m.", "Azimut..gon."), as.numeric)) %>% 
  # calcualte coordiantes of mb based on utm coordinates of RP (referenzpoint) 
  # as the center of the forest inventory is not the center of the forest inventory
  mutate(rw_med = ifelse(is.na(Dezimalgrad.E..WGS84.), coord(ref_easting,  ref_northing, Distanz..m., Azimut..gon., coordinate = "x"), Dezimalgrad.E..WGS84.), # x, easting, longitude, RW 
         hw_med = ifelse(is.na(Dezimalgrad.N..WGS84.), coord(ref_easting,  ref_northing, Distanz..m., Azimut..gon., coordinate = "y"), Dezimalgrad.N..WGS84.) # y, northing, latitude, HW 
  )

# iteration 2
vm_lokation_momok_2 <- vm_lokation_momok_1 %>%
  # join in the the respective reference point coordinates  for the center of the forest inventory by "ursprung"
  left_join(., vm_lokation_momok_1 %>% select( "MoMoK_Nr",  "Skizzenpunkt",  "hw_med", "rw_med") %>% 
              rename("ref_northing_1" = "hw_med") %>% 
              rename("ref_easting_1" = "rw_med"), 
            by = c("MoMoK_Nr", "Ursprung" = "Skizzenpunkt")) %>% 
  mutate(across(c("Dezimalgrad.E..WGS84.", "Dezimalgrad.N..WGS84.", "ref_northing_1", "ref_easting_1", "Distanz..m.", "Azimut..gon."), as.numeric)) %>% 
  # calcualte coordiantes of mb based on utm coordinates of RP (referenzpoint) 
  # as the center of the forest inventory is not the center of the forest inventory
  mutate(rw_med = ifelse(is.na(Dezimalgrad.E..WGS84.) & is.na(rw_med) , coord(ref_easting_1,  ref_northing_1, Distanz..m., Azimut..gon., coordinate = "x"), rw_med), # x, easting, longitude, RW 
         hw_med = ifelse(is.na(Dezimalgrad.N..WGS84.) & is.na(hw_med), coord(ref_easting_1,  ref_northing_1, Distanz..m., Azimut..gon., coordinate = "y"), hw_med) # y, northing, latitude, HW 
  )

# iteration 3
vm_lokation_momok_3 <- vm_lokation_momok_2 %>%
  # join in the the respective reference point coordinates  for the center of the forest inventory by "ursprung"
  left_join(., vm_lokation_momok_2 %>% select( "MoMoK_Nr",  "Skizzenpunkt",   "hw_med", "rw_med") %>% 
              rename("ref_northing_2" = "hw_med") %>% 
              rename("ref_easting_2" = "rw_med"), 
            by = c("MoMoK_Nr", "Ursprung" = "Skizzenpunkt")) %>% 
  mutate(across(c("Dezimalgrad.E..WGS84.", "Dezimalgrad.N..WGS84.", "ref_northing_2", "ref_easting_2", "Distanz..m.", "Azimut..gon."), as.numeric)) %>% 
  # calcualte coordiantes of mb based on utm coordinates of RP (referenzpoint) 
  # as the center of the forest inventory is not the center of the forest inventory
  mutate(rw_med = ifelse(is.na(Dezimalgrad.E..WGS84.) & is.na(rw_med) , coord(ref_easting_2,  ref_northing_2, Distanz..m., Azimut..gon., coordinate = "x"), rw_med), # x, easting, longitude, RW 
         hw_med = ifelse(is.na(Dezimalgrad.N..WGS84.) & is.na(hw_med), coord(ref_easting_2,  ref_northing_2, Distanz..m., Azimut..gon., coordinate = "y"), hw_med) # y, northing, latitude, HW 
  )

# iteration 4
vm_lokation_momok_4 <- vm_lokation_momok_3 %>%
  # join in the the respective reference point coordinates  for the center of the forest inventory by "ursprung"
  left_join(., vm_lokation_momok_3 %>% select( "MoMoK_Nr",  "Skizzenpunkt",   "hw_med", "rw_med") %>% 
              rename("ref_northing_3" = "hw_med") %>% 
              rename("ref_easting_3" = "rw_med"), 
            by = c("MoMoK_Nr", "Ursprung" = "Skizzenpunkt")) %>% 
  mutate(across(c("Dezimalgrad.E..WGS84.", "Dezimalgrad.N..WGS84.", "ref_northing_3", "ref_easting_3", "Distanz..m.", "Azimut..gon."), as.numeric)) %>% 
  # calcualte coordiantes of mb based on utm coordinates of RP (referenzpoint) 
  # as the center of the forest inventory is not the center of the forest inventory
  mutate(rw_med = ifelse(is.na(Dezimalgrad.E..WGS84.) & is.na(rw_med) , coord(ref_easting_3,  ref_northing_3, Distanz..m., Azimut..gon., coordinate = "x"), rw_med), # x, easting, longitude, RW 
         hw_med = ifelse(is.na(Dezimalgrad.N..WGS84.) & is.na(hw_med), coord(ref_easting_3,  ref_northing_3, Distanz..m., Azimut..gon., coordinate = "y"), hw_med) # y, northing, latitude, HW 
  )

# iteration 5
vm_lokation_momok_5 <- vm_lokation_momok_4 %>%
  # join in the the respective reference point coordinates  for the center of the forest inventory by "ursprung"
  left_join(., vm_lokation_momok_4 %>% select( "MoMoK_Nr",  "Skizzenpunkt",   "hw_med", "rw_med") %>% 
              rename("ref_northing_4" = "hw_med") %>% 
              rename("ref_easting_4" = "rw_med"), 
            by = c("MoMoK_Nr", "Ursprung" = "Skizzenpunkt")) %>% 
  mutate(across(c("Dezimalgrad.E..WGS84.", "Dezimalgrad.N..WGS84.", "ref_northing_4", "ref_easting_4", "Distanz..m.", "Azimut..gon."), as.numeric)) %>% 
  # calcualte coordiantes of mb based on utm coordinates of RP (referenzpoint) 
  # as the center of the forest inventory is not the center of the forest inventory
  mutate(rw_med = ifelse(is.na(Dezimalgrad.E..WGS84.) & is.na(rw_med) , coord(ref_easting_4,  ref_northing_4, Distanz..m., Azimut..gon., coordinate = "x"), rw_med), # x, easting, longitude, RW 
         hw_med = ifelse(is.na(Dezimalgrad.N..WGS84.) & is.na(hw_med), coord(ref_easting_4,  ref_northing_4, Distanz..m., Azimut..gon., coordinate = "y"), hw_med) # y, northing, latitude, HW 
  )






# 0.3.1.2.3.3.  export lokation -----------------------------------------------------------------------------------
# adjust plot Id according to double sampled plots 
 vm_lokation_momok <- vm_lokation_momok_5 %>% 
   # filter only plot centres
    filter(stringr::str_detect(Skizzenpunkt, "MB")) %>% 
   mutate(  MoMoK_Nr = ifelse(
     # idenfity plots with more then one center that have a more then 2 characters long Skizzenpunkt name, 
     MoMoK_Nr %in% double_plots_momok & nchar(Skizzenpunkt) > 2, 
     # add number of Skizzenpunkt to plot ID (https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
     paste0(MoMoK_Nr, str_sub(Skizzenpunkt, start= -1)),
     # otherwise leave old plot number
     MoMoK_Nr)) %>% 
   rename("bfhnr" = "MoMoK_Nr") %>% 
  select(bfhnr, rw_med, hw_med) %>% 
  distinct()

write.csv(vm_lokation_momok, paste0(input.path, "momok_vm_lokation", ".csv"), row.names = FALSE)





# 0.3.1.2.4. TREES momok ----------------------------------------------------
# 0.3.1.2.4.1. be momok ----------------------------------------------------
# create "proper" bze2 data from momok datasets
# lets start with be 
# columns we need are: 
# [1] "bund_nr"                "team"                   "datum"                  "hbi_status"             "beart"                  "besttyp"                "struktur"              
# [8] "schlussgrad_schi1"      "schlussgrad_schi2"      "mischung"               "pk1_aufnahme"           "pk2_aufnahme"           "pk3_aufnahme"           "geraet"                
# [15] "beschreibungbestockung" "anmerkung" 

be_momok <- read.delim(file = here(paste0(input.path, "info_momok.csv")), sep = ",", dec = ".")
be_momok <- be_momok %>%  
  select("MoMoK_Nr",                  # "bund_nr"    
         "Datum_Aufnahme",            #"datum"  
         "Betriebsart" ,              #"beart" 
         "Bestockungstyp",            #"besttyp"  
         "Vertikalstruktur",          #"struktur"
         "Kronen.SG1..Hauptbestand.", # "schlussgrad_schi1"
         "Kronen.SG2..Unterstand.",    #"schlussgrad_schi2"
         "Mischungsform"              #"mischung" 
  ) %>%  # close select
  # add columns that are in be but not in momok_be
  mutate(team = -9, 
         hbi_status = -9, # i set this to -9 so it fits the structure of bze (nsi) 
         pk1_aufnahme = 3, # for pk1 and pk2 (concentric cirlce sampling cirlce 1 and 2 ) we put stus 1 as the circle for momok was 12m ans all trees were assessed within these 12m 
         pk2_aufnahme = 1, # 1 Aufnahme wurde erfolgreich durchgeführt
         pk3_aufnahme = 3, 
         geraet = -9,
         beschreibungbestockung = -9,
         anmerkung = NA) %>% 
  distinct()
colnames(be_momok) <- c("bund_nr","datum" ,"beart" ,"besttyp","struktur" ,"schlussgrad_schi1","schlussgrad_schi2","mischung",
                        # not in momok be yet
                        "team", "hbi_status", "pk1_aufnahme","pk2_aufnahme","pk3_aufnahme", "geraet", "beschreibungbestockung", "anmerkung")

if(isTRUE(nrow(double_plots_momok) != 0) == T){
  be_momok_double_plots_list <- vector("list", length = nrow(double_plots_momok))
  for (i in 1:nrow(double_plots_momok)) {
    # i = 1
    my.plot.id = as.numeric(double_plots_momok[i, "MoMoK_Nr"])
    # select number of centres of the double plot
    n.rep <- as.numeric(double_plots_momok[i, "n_centres"])-1   # - 1 because one is already in the original dataset
    # select the rows form be_momok that have to be doublicated
    my.be.row.to.rep <- be_momok[be_momok$bund_nr == my.plot.id, ]
    # repeat the row as often as the plot has centres: https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
    my.be.row.to.rep <- my.be.row.to.rep[rep(seq_len(nrow(my.be.row.to.rep)), each = n.rep), ] %>% 
      mutate(bund_nr = paste0(bund_nr, row_number()))
    
    # save to export 
    be_momok_double_plots_list[[i]] <- my.be.row.to.rep
  }
  be_momok_double_plots <- as.data.frame(rbindlist(be_momok_double_plots_list))
  
  # bind doublicated rows into be_momok if they excist 
  be_momok <- rbind(be_momok, be_momok_double_plots) %>% arrange(bund_nr)
}
write.csv(be_momok, paste0(input.path, "momok_be.csv"), row.names = FALSE)

       



# 0.3.1.2.4.2. beab momok ----------------------------------------------------     
# beab hbi includes followong columns:
# [1] "bund_nr"       "lfd_nr"        "baumkennzahl"  "zwiesel"       "bart"          "alter"         "alter_methode" "d_mess"        "bhd_hoehe"     "hoehe"         "kransatz"      "azi"          
# [13] "hori"          "kraft"         "schi"  
beab_momok <- read.delim(file = here(paste0(input.path, "beab_momok.csv")), sep = ",", dec = ".") %>% filter(!(is.na(MoMoK_Nr)))
colnames(beab_momok)
# [1] "MoMoK_Nr"          "Name"              "Bundeland"         "Datum_Aufnahme"    "Nr_PK"             "BNr"               "ZW"                "St"                "Baumart..Code."   
# [10] "Baumart"           "Schi"              "Kraft"             "Alt"               "Alt.Meth"          "BHD..mm."          "BHD.Hoehe..cm."    "Permanent.Maßband" "Punktdendrometer" 
# [19] "BHD.Stufen"        "Hoehe..dm."        "Kronenansatz..dm." "Distanz..cm."      "Azimut..Gon."      "Azimut...."        "Bemerkung"   
beab_momok <- beab_momok %>%
  mutate(MoMoK_Nr = ifelse(
    # idenfity plots with more then one center that have a number in their sampling circuit number, 
    MoMoK_Nr %in% double_plots_momok & !is.na(as.numeric(Nr_PK)), 
    # add number of Skizzenpunkt to plot ID (https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
    paste0(MoMoK_Nr, Nr_PK),
    # otherwise leave old plot number
    MoMoK_Nr)) %>% 
   mutate(across(c("BHD..mm.", "Distanz..cm.",      "Azimut..Gon.",      "Azimut...."), as.numeric)) %>% 
  mutate(Azimut..Gon. = ifelse(is.na(Azimut..Gon.), (Azimut..../360)*400 , Azimut..Gon.)) %>% 
  select("MoMoK_Nr"                # "bund_nr"    
         ,"BNr"                     #lfd_nr
         ,  "ZW"                   # "zwiesel"
         , "Baumart"               # "bart"
         , "Alt"                   # "alter"
         ,  "Alt.Meth"             # "alter_methode"
         , "BHD..mm."              # "d_mess"
         , "BHD.Hoehe..cm."        # "bhd_hoehe" 
         ,  "Hoehe..dm."           # "hoehe"
         , "Kronenansatz..dm."     # "kransatz" 
         ,  "Azimut..Gon."         #  "azi"
         , "Distanz..cm."          # hori
         , "Kraft"                # kraft
         , "Schi"                 # schi
         )  %>% distinct() %>% 
  # there were different trees with the same tree id so we have to reassing the three IDs this cannot happen in BZE data since we have a plausi check for them
  group_by(MoMoK_Nr) %>% mutate(BNr = row_number() )
# assign new colnames corresponding with bze
colnames(beab_momok) <- c("bund_nr", "lfd_nr", "zwiesel","bart", "alter", "alter_methode", "d_mess",  "bhd_hoehe" , "hoehe", "kransatz",  "azi", "hori", "kraft", "schi")
  # export 
write.csv(beab_momok, paste0(input.path, "momok_beab.csv"), row.names = FALSE)                         



# 0.3.1.2.5 TITLE momok tit csv ----------------------------------------------------     
# tit includes followong columns:
# "bund_nr"    "team"       "datum"      "status"     "re_form"    "re_lage"    "neigung"    "exposition" "anmerkung"
tit_momok <- lokation_momok %>%
  filter(stringr::str_detect(Skizzenpunkt, "MB")) %>%
  mutate(  MoMoK_Nr = ifelse(
    # idenfity plots with more then one center that have a more then 2 characters long Skizzenpunkt name, 
    MoMoK_Nr %in% double_plots_momok & nchar(Skizzenpunkt) > 2, 
    # add number of Skizzenpunkt to plot ID (https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
    paste0(MoMoK_Nr, str_sub(Skizzenpunkt, start= -1)),
    # otherwise leave old plot number
    MoMoK_Nr)) %>% # only select the points that mark the center of the forest inventory
  select("MoMoK_Nr",                  # "bund_nr"    
         "Erläuterung"  ) %>%  # anmerkung
  # add columns that are in be but not in momok_be
  left_join(., be_momok %>% select(bund_nr, datum) %>% mutate(bund_nr = as.character(bund_nr)), by = c("MoMoK_Nr" = "bund_nr")) %>% 
  mutate(team = -9,       # "team"
         status = -9, # i set this to -9 so it fits the structure of bze (nsi) 
         re_form = -9,
         re_lage = -9, 
         neigung = -9, 
         exposition = -9) %>% 
  distinct()
# assign colnames<
colnames(tit_momok) <- c("bund_nr", "anmerkung", "datum",
                         # not in momok be yet
                         "team" ,  "status",     "re_form",    "re_lage",    "neigung" ,   "exposition")
write.csv(tit_momok, paste0(input.path, "momok_tit.csv"), row.names = FALSE)

# 0.3.1.2.6. REGENERATION momok ----------------------------------------------------     
RG_momok <- read.delim(file = here(paste0(input.path, "bej_momok.csv")), sep = ",", dec = ".") %>% filter(!(is.na(MoMoK_Nr)))

## alter plot_ID of double inventories plots
# we have to notice here that we can separate the sampling circuits per plot 
# so we can assign north 1 and north 2 of plot 34010 but we cannot actually say which 
# of the two sampling centres of 34010 they belong to. that selection remains random

RG_momok <- RG_momok %>% 
  # join in dataset that contains info about the centre the RG circuit was taken from 
  left_join(., RG_momok%>% 
  select("MoMoK_Nr", "Lage", "pk_maxdist..cm.") %>% 
  distinct() %>% 
  group_by(MoMoK_Nr, Lage) %>% mutate(pk_nr_double_plots = ifelse(MoMoK_Nr %in% c(double_plots_momok$MoMoK_Nr), row_number(), NA)), 
  by = c("MoMoK_Nr", "Lage", "pk_maxdist..cm.")) %>%
  # adjust plot_ID for double plots:
  mutate(MoMoK_Nr = ifelse(
    # idenfity plots with more then one center that have a number in their sampling circuit number, 
    (MoMoK_Nr) %in% c(double_plots_momok$MoMoK_Nr) & !is.na(as.numeric(pk_nr_double_plots)), 
    # add number of Skizzenpunkt to plot ID (https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
    paste0(MoMoK_Nr, pk_nr_double_plots),
    # otherwise leave old plot number
    as.character(MoMoK_Nr))) %>% 
  select(-c(pk_nr_double_plots))



# 0.3.1.2.6.1. bej momok ----------------------------------------------------     
# bej hbi includes followong columns:
# "bund_nr"      "pk_nr"        "pk_aufnahme"  "pk_richtung"  "pk_dist"      "pk_maxdist"   "pk_anmerkung"
colnames(RG_momok)
# [1] "MoMoK_Nr"        "Spalte1"         "Bundeland"       "Datum"           "Nr_VJ_PK"        "Lage"            "Distanz.MB..cm." "pk_maxdist..cm." "LfdNr"           "Baumart..Code." 
# [11] "Baumart"         "Hoehe..cm."      "grklasse" 


bej_momok <- RG_momok %>%
  select("MoMoK_Nr"                # "bund_nr"    
         ,"Nr_VJ_PK"               #pk_nr
         # "pk_aufnahme" missing
         , "Lage"                   # "pk_richtung"
         , "Distanz.MB..cm."        # "pk_dist" 
         , "pk_maxdist..cm."        # "pk_maxdist"
  )  %>% 
  mutate(pk_aufnahme = 1, # regenation assesment was successfully completed (as we dont have any other info)
         pk_anmerkung = NA) %>% 
  distinct() %>% 
  # assign consequitve number to sampling circuits
  group_by(MoMoK_Nr) %>% arrange(MoMoK_Nr , Lage  , pk_maxdist..cm.) %>% mutate(Nr_VJ_PK = row_number() ) 
# assign new colnames
colnames(bej_momok) <- c("bund_nr",  "pk_nr",  "pk_richtung",  "pk_dist", "pk_maxdist" ,    "pk_aufnahme",  "pk_anmerkung")
write.csv(bej_momok, paste0(input.path, "momok_bej.csv"), row.names = FALSE)



# 0.3.1.2.6.2. bejb momok ----------------------------------------------------     
# bej hbi includes followong columns:
# "bund_nr"  "pk_nr"    "lfd_nr"   "bart"     "hoehe"    "grklasse"
colnames(RG_momok)
# [1] "MoMoK_Nr"        "Spalte1"         "Bundeland"       "Datum"           "Nr_VJ_PK"        "Lage"            "Distanz.MB..cm." "pk_maxdist..cm." "LfdNr"           "Baumart..Code." 
# [11] "Baumart"         "Hoehe..cm."      "grklasse" 

bejb_momok <- RG_momok %>%
  select("MoMoK_Nr"                # "bund_nr"
         , "Lage"
         ,"Nr_VJ_PK"               #pk_nr
         , "pk_maxdist..cm."       # we need this to join in the correct pk number 
         , "LfdNr"                 # "lfd_nr" 
         ,"Baumart"         # "bart"
         ,"Hoehe..cm."             # "hoehe"
         ,"grklasse"               # "grklasse"
         ) %>% distinct() %>%
  group_by(MoMoK_Nr, Lage, pk_maxdist..cm.) %>% mutate(LfdNr = row_number() ) %>% 
  # join in correct RG sampling circuit number from bej
  left_join(., bej_momok %>% select(bund_nr, pk_nr, pk_richtung, pk_maxdist), by = c(c("MoMoK_Nr" = "bund_nr"), c("Lage" = "pk_richtung"), c("pk_maxdist..cm."  = "pk_maxdist" ))) %>% 
  # change Nr_VJ_PK to numbers from pk_nr column from bej_momok: 
  mutate(Nr_VJ_PK = ifelse(is.na(as.numeric(Nr_VJ_PK)), pk_nr, Nr_VJ_PK)) %>% 
  ungroup() %>% 
  select(-c("pk_maxdist..cm.", "pk_nr", "Lage"))

# assign new colnames
colnames(bejb_momok) <- c("bund_nr",  "pk_nr",    "lfd_nr" ,  "bart",     "hoehe",    "grklasse")
# export
write.csv(bejb_momok, paste0(input.path, "momok_bejb.csv"), row.names = FALSE)



# 0.3.1.2.7.DEADWOOD MOMOK ----------------------------------------------------   
DW_momok <- read.delim(file = here(paste0(input.path, "be_totholz_momok.csv")), sep = ",", dec = ".") %>% filter(!(is.na(MoMoK_Nr)))
colnames(DW_momok)
# [1] "MoMoK_Nr"          "Name"              "Bundesland"        "Datum"             "Nr_PK"             "Nr"                "Baumartengruppe"   "TYP"               "Hoehe.Laenge..dm."
# [10] "Durchmesser..cm."  "Zersetzungsgrad"

# 0.3.1.2.7.1. be_totholz_punkt momok ----------------------------------------------------     
# be_totholz_punkt hbi includes followong columns:
# "bund_nr" "status"  "pk_dist" "pk_azi"
be_totholz_punkt_momok <- DW_momok %>%
  # adjust Momok nr. if the plot is doublicated
  mutate(MoMoK_Nr = ifelse(
    # idenfity plots with more then one center that have a number in their sampling circuit number, 
    MoMoK_Nr %in% double_plots_momok & !is.na(as.numeric(Nr_PK)), 
    # add number of Skizzenpunkt to plot ID (https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
    paste0(MoMoK_Nr, Nr_PK),
    # otherwise leave old plot number
    MoMoK_Nr)) %>% 
  select("MoMoK_Nr"                # "bund_nr"    
         ) %>% 
  mutate(status = 1, 
         pk_dist = -9, 
         pk_azi = -9) %>% 
  rename("bund_nr" = "MoMoK_Nr") %>% 
  distinct()
write.csv(be_totholz_punkt_momok, paste0(input.path, "momok_be_totholz_punkt.csv"), row.names = FALSE)


# 0.3.1.2.7.2. be_totholz_liste momok ----------------------------------------------------     
# be_totholz_liste hbi includes followong columns:
 # "bund_nr"     "lfd_nr"      "typ"         "baumgruppe"  "anzahl"      "durchmesser" "laenge"      "zersetzung"
be_totholz_liste_momok <- DW_momok %>%
  # adjust Momok nr. if the plot is doublicated
  mutate(MoMoK_Nr = ifelse(
    # idenfity plots with more then one center that have a number in their sampling circuit number, 
    MoMoK_Nr %in% double_plots_momok & !is.na(as.numeric(Nr_PK)), 
    # add number of Skizzenpunkt to plot ID (https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
    paste0(MoMoK_Nr, Nr_PK),
    # otherwise leave old plot number
    MoMoK_Nr)) %>% 
  select("MoMoK_Nr"                # "bund_nr"    
         ,"Nr"                     # "lfd_nr"      
         , "TYP"                   #"typ"     
         , "Baumartengruppe"       # "baumgruppe"
         , "Durchmesser..cm."      #     "durchmesser"
         , "Hoehe.Laenge..dm."     # "laenge"
         , "Zersetzungsgrad"       #"zersetzung"
  ) %>% 
  mutate(anzahl = -9) %>%  
  distinct() %>% 
  group_by(MoMoK_Nr) %>% 
  mutate(Nr = row_number())

colnames(be_totholz_liste_momok) <- c("bund_nr","lfd_nr","typ" ,"baumgruppe" ,"durchmesser" ,"laenge" ,"zersetzung",  "anzahl")
write.csv(be_totholz_liste_momok, paste0(input.path, "momok_be_totholz_liste.csv"), row.names = FALSE)




stop("this is where getting data frmdb of paper ends")







