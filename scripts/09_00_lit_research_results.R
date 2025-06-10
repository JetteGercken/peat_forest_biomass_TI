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
bio_func_df <- read.delim(file = paste0(getwd(), "/data/input/", "B_lit_functions.csv"), sep = ",", dec = ".") 



# 0.4 data prep -----------------------------------------------------------
# assign IDs to papers and functions
# assign IDs to papers and functions
bio_func_df <- bio_func_df %>% 
  dplyr::distinct() %>% 
  arrange(author, title, species) %>% 
  dplyr::group_by(title, author, year, TSL) %>% 
  dplyr::mutate(func_ID = dplyr::row_number()) %>% 
  left_join(., 
            bio_func_df %>% 
              arrange(author, title, species) %>%
              select(title, author, year, TSL) %>% 
              dplyr::distinct() %>% 
              dplyr::mutate(paper_ID = dplyr::row_number()), 
            by = c("title", "author", "year", "TSL")) %>% 
  # assign country code
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c'))
# transform coefficients to numeric
bio_func_df[,13:28] <- lapply(bio_func_df[,13:28], as.numeric)
# add a column that combines func id and paper id
bio_func_df$ID <- paste0(bio_func_df$paper_ID,"_", bio_func_df$func_ID)



# 0.5 data preparation trees ---------------------------------------------------------
trees_data <- trees_data %>% 
  dplyr::rename("H_m_old" = "H_m") %>% # change name of H_m which includes sampled and not sampled hieghts that are estimated with DBH and HG
  mutate(H_m = as.numeric(H_m_nls))  %>% # change HM to H_nls which is only SBH based heights
  distinct() %>% 
  # join in soil data
  left_join(soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))





# 1. results -----------------------------------------------------------------
# number of peatland sites
 length(unique(soil_types_db$bfhnr_2[soil_types_db$min_org == "org"]))
# 31
 nrow(unique(trees_data[trees_data$min_org == "org", c("plot_ID", "inv", "min_org")]))
# 65
 nrow(unique(trees_data[trees_data$min_org == "org" & trees_data$inv == "momok", c("plot_ID", "inv", "min_org")]))
# 47
 nrow(unique(trees_data[trees_data$min_org == "org" & trees_data$inv == "HBI", c("plot_ID", "inv", "min_org")]))
# 18
 nrow(unique(trees_data[trees_data$min_org == "min" & trees_data$inv == "HBI", c("plot_ID", "inv", "min_org")]))
 # 18
 
anti_join( (soil_types_db[soil_types_db$min_org == "org", c("bfhnr_2", "erhebjahr_2")]) %>% distinct(),
          (trees_data[trees_data$min_org == "org" & trees_data$inv == "HBI", c("plot_ID", "inv", "min_org")]) %>% distinct(), 
          by = c("bfhnr_2" = "plot_ID"))

# number of papers in general ---------------------------------------------
# inital number
nrow(unique(bio_func_df[!is.na(bio_func_df$variables), "paper_ID"])) # 29

# boreal, extratropical functions 
view(unique(bio_func_df[!is.na(bio_func_df$variables), c("paper_ID", "country")]))

# view functions
view(unique(bio_func_df[!is.na(bio_func_df$variables), c("paper_ID", "function.")]))


# count functions excluded because variabels are not available 
nrow(unique(bio_func_df[!(bio_func_df$variables %in% c("H_m, DBH_cm", "DBH_cm, H_m", "DBH_cm")) & !is.na(bio_func_df$variables), "paper_ID"])) # 1
(unique(bio_func_df[!(bio_func_df$variables %in% c("H_m, DBH_cm", "DBH_cm, H_m", "DBH_cm")) & !is.na(bio_func_df$variables), "paper_ID"])) # 1


# count functions unsuitable to calculate woody aboceground biomass
nrow(unique(bio_func_df[!(bio_func_df$leafes_inkl %in% c("not included", "possible")) & !is.na(bio_func_df$variables), "paper_ID"])) # 6
(unique(bio_func_df[!(bio_func_df$leafes_inkl %in% c("not included", "possible")) & !is.na(bio_func_df$variables), "paper_ID"])) # 6




#+
#+## filter applied to functions before being applied: this is the dataset the analysis eventually ran with
bio_func_sub <- setDT(bio_func_df)[
    !is.na(bio_func_df$variables) &                # select only those papers with functions
    bio_func_df$compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb")& # only the usual compartiments 
      bio_func_df$leafes_inkl %in% c("not included", "possible") & # select only those suitable for aboveground wood y biomass
    bio_func_df$variables %in% c("DBH_cm, H_m", "DBH_cm"),    # only functions with DBH and height as explainatory variablse
]
 
bio_func_final <- bio_func_sub[bio_func_sub$species %like% "Betula pubescens" | # select only betula specific functions or
  bio_func_sub$ species %like% "Alnus glutinosa" ,]     # select only Alnus specific species



unique(setDT(bio_func_sub)[bio_func_sub$ species %like% "Alnus glutinosa" , c("author", "year", "title", "paper_ID")] )


# final amount of functions 
length(unique(bio_func_final$paper_ID))  # 19




# 1.1. number of papers covered per genus -----------------------------------------------------------------
# assign genus group: 
setDT(bio_func_final)[, genus_group := (ifelse(species %like% "Betula", "Betula", 
                                       ifelse(species %like% "Alnus", "Alnus", "other")))]
# count paper by genus 
unique(setDT(bio_func_final)[!is.na("variables")&
                               genus_group %like% "Betula" | genus_group %like% "Alnus", 
                          c("title", "author", "year", "genus_group")])[,.N, by= list(genus_group)]

# result
#        genus_group     N
#            <char> <int>
#   1:      Betula    18
#   2:       Alnus    14

# 1.2. number of papers per species -----------------------------------------------------------------
# harmonise species names
bio_func_sub[, spec_group :=(ifelse(species %like% "Betula pubescens", "Betula pubescens", 
                                      ifelse(species %like% "Alnus glutinosa" |
                                               species %like% "Alnus Glutinosa", "Alnus glutinosa", "other")))]
# count paper by speices 
unique(setDT(bio_func_sub)[, c("title", "author", "year", "spec_group")])[,.N, by= list(spec_group)]

# result: 
#          spec_group     N
#               <char> <int>
# 1:  Alnus glutinosa     8
# 2: Betula pubescens    20
# 3:            other    10

# 1.3. countries of origin -----------------------------------------------------------------
## countries 
# count countries
unique(setDT(bio_func_sub)[!is.na("variables") , 
                          c("title", "author", "year", "country")])[,.N, by= list(country)]

## region
# mediterrenean counties: https://www.undp.org/sites/g/files/zskgke326/files/migration/tr/akdenizrapor.pdf
mediteran <- c("Albania", "Algeria", "Bosnia and Herzegovina", "Croatia", "Cyprus", "Egypt", 
             "France", "Greece", "Israel", "Italy", "Lebanon", "Libya", "Malta", 
            "Monaco", "Montenegro", "Morocco", "Slovenia", "Spain", "Spain, Gallicia", "Syria", "Tunisia", "Turkey")
# boreal countries
boreal <- c("Canada", "China", "Finnland", "Japan", "Norway", "Russia", "Sweden", "USA", "Iceland", "Greenland", "Latvia","Lituania", "Estonia")
# assign regions to country
bio_func_sub[!is.na("variables"), region := (ifelse(country %in% mediteran, "mediterean", 
                                ifelse(country %in% boreal, "boreal", "temperate")))]
# count regions
unique(bio_func_sub[!is.na("variables") , c("title", "author", "year", "country", "region")])[,.N, by= list(region)]

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

