# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken
# lit research result


# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path <- ("/output/out_data/") 


# ----- 0.3 data import --------------------------------------------------------
bio_func_df <- read.delim(file = paste0(getwd(), "/data/input/", "B_lit_functions.csv"), sep = ",", dec = ".") %>% select(-c(func_ID, paper_ID))



# 0.4 data prep -----------------------------------------------------------
# assign IDs to papers and functions
bio_func_df <- bio_func_df %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(title, author, year) %>% 
  dplyr::mutate(func_ID = dplyr::row_number()) %>% 
  left_join(., 
            bio_func_df %>% 
              select(title, author, year) %>% 
              distinct() %>% 
              dplyr::mutate(paper_ID = dplyr::row_number()), 
            by = c("title", "author", "year")) 
# transform coefficients to numeric
bio_func_df[,13:27] <- lapply(bio_func_df[,13:27], as.numeric)
# add a column that combines func id and paper id
bio_func_df$ID <- paste0(bio_func_df$paper_ID,"_", bio_func_df$func_ID)



## filter applied to functions before being applied: this is the dataset the analysis eventually ran with
bio_func_final <- subset(bio_func_df, 
       species %like% "Betula" | species %like% "Alnus" &     # select only Alnus specific species
         !is.na("variables ") &                # select only those papers with functions
         compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb")& # only the usual compartiments 
         !(str_detect(bio_func_df$variables, "age"))   # only functions with DBH and height as explainatory variablse
) 

# 1. results -----------------------------------------------------------------
# 1.1. number of papers covered per genus -----------------------------------------------------------------
# assign genus group: 
bio_func_final[, genus_group :=(ifelse(species %like% "Betula", "Betula", 
                                       ifelse(species %like% "Alnus", "Alnus", "other")))]
# count paper by genus 
unique(setDT(bio_func_final)[!is.na("variables")&
                               genus_group %like% "Betula" | genus_group %like% "Alnus", 
                          c("title", "author", "year", "genus_group")])[,.N, by= list(genus_group)]

# result
#      genus_group     N
#         <char>   <int>
# 1:       Alnus    13
# 2:      Betula    26

# 1.2. number of papers per species -----------------------------------------------------------------
# harmonise species names
bio_func_final[, spec_group :=(ifelse(species %like% "Betula pubescens", "Betula pubescens", 
                                      ifelse(species %like% "Alnus glutinosa" |
                                               species %like% "Alnus Glutinosa", "Alnus glutinosa", "other")))]
# count paper by speices 
unique(setDT(bio_func_final)[!is.na("variables")&
                            species %like% "Betula" | species %like% "Alnus", 
                          c("title", "author", "year", "spec_group")])[,.N, by= list(spec_group)]

# result: 
#          spec_group     N
#               <char> <int>
# 1:  Alnus glutinosa     8
# 2: Betula pubescens    20
# 3:            other    10

# 1.3. countries of origin -----------------------------------------------------------------
## countries 
# count countries
unique(setDT(bio_func_df)[!is.na("variables") , 
                          c("title", "author", "year", "country")])[,.N, by= list(country)]

## region
# mediterrenean counties: https://www.undp.org/sites/g/files/zskgke326/files/migration/tr/akdenizrapor.pdf
mediteran <- c("Albania", "Algeria", "Bosnia and Herzegovina", "Croatia", "Cyprus", "Egypt", 
             "France", "Greece", "Israel", "Italy", "Lebanon", "Libya", "Malta", 
            "Monaco", "Montenegro", "Morocco", "Slovenia", "Spain", "Spain, Gallicia", "Syria", "Tunisia", "Turkey")
# boreal countries
boreal <- c("Canada", "China", "Finnland", "Japan", "Norway", "Russia", "Sweden", "USA", "Iceland", "Greenland")
# assign regions to country
bio_func_df[!is.na("variables"), region := (ifelse(country %in% mediteran, "mediterean", 
                                ifelse(country %in% boreal, "boreal", "temperate")))]
# count regions
unique(bio_func_df[!is.na("variables") , c("title", "author", "year", "country", "region")])[,.N, by= list(region)]

# region     N
# <char> <int>
# 1: mediterean     3
# 2:     boreal    18
# 3:  temperate    17

nrow(unique(bio_func_df[!is.na("variables") , c("title", "author", "year", "country", "region")]))

# 1.4. most common input variable (over all species) -----------------------------------------------------------------
unique(setDT(bio_func_final)[!is.na("variables"), c("title", "author", "year", "variables")])[, .N, by = list(variables)]

#               variables  N
#                  <char> <int>
# 1:              DBH_cm   21
# 2:              <NA>     8
# 3:       DBH_cm, H_m     8
# 4:               H_m     1
# 5:  H_mean, age, H_m     1
# 6:               H50     1
# 7: DBH_cm, age, H100     1
# 8:  DBH_cm, age, H_m     1


# 1.5. allow the separate calculation of woody aboveground biomass --------
# leafes_inkl_stat == "included"

nrow(
as.data.frame(unique(bio_func_df[, c("title", "author", "year", "leafes_inkl")])) %>% filter(leafes_inkl == "included")
  )
# 6 


# 1.6. most common function -----------------------------------------------
bio_func_final[, func:= (sub('.*=', '', bio_func_final$function.))]
unique(setDT(bio_func_final)[!is.na("variables"), c("title", "author", "year", "func")])[, .N, by = list(func)]

