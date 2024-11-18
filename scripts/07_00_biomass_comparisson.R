# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken


# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path <- ("output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_3.csv" which contains plot area and stand data additionally to 
# tree data
trees_data <- read.delim(file = here(paste0(out.path, "HBI_LT_update_3.csv")), sep = ",", dec = ".")
trees_removed <- read.delim(file = here(paste0(out.path, trees_data$inv[1], "_LT_removed.csv")), sep = ",", dec = ".")

# importa data from literature research
bio_func <- read.delim(file = here(paste0(input.path, "B_lit_functions.csv")), sep = ",", dec = ".") 



# 0.4 data preparation ---------------------------------------------------------
trees_data <- trees_data %>% mutate(H_m = as.numeric(H_m))  %>% distinct() 

# assign IDs to papers and functions
bio_func <- bio_func %>% 
  left_join(., 
            bio_func %>% 
              select(title, author, year) %>% 
              distinct() %>% 
              mutate(func_ID = row_number()), 
            by = c("title", "author", "year"))


# 1. Biomass calculations -------------------------------------------------
# now we will try to implement a loop for all biomass functions in the list 

alnus_agb_func_no_ln <- bio_func %>% filter(compartiment %in% c("agb") & 
                                              stringr::str_detect(species, "Alnus") & is.na(logarithm_B))

for (i in 1:length(unique(c(alnus_agb_func_no_ln$title, alnus_agb_func_no_ln$author)))){
 # i = 1
  
  func <- alnus_agb_func_no_ln$function.[i]
  # select only part behind "=": https://stackoverflow.com/questions/57204659/how-to-extract-everything-after-a-specific-string
  func <- sub('.+=(.+)', '\\1', func)
  unit <- alnus_agb_func_no_ln$unit_B[i]
  vars.list <- alnus_agb_func_no_ln$parameters.required[i]
  coef.list <- alnus_agb_func_no_ln[i,12:20]
  

  
  tree.df <- (trees_data[trees_data$bot_genus %in% c("Alnus"),][1,])%>% 
    # https://stackoverflow.com/questions/58592636/r-how-to-select-columns-that-contains-strings-where-the-string-is-any-eleme
    select(plot_ID, tree_ID, SP_code, matches(paste(vars.list, collapse="|")))
 
   tree.df %>% mutate(B_kg_tree = )
  
  
}




















