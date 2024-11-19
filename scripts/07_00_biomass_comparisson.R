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
tapes_tree_data <- read.delim(file = here(paste0(out.path, "HBI_LT_update_4.csv")), sep = ",", dec = ".")
trees_removed <- read.delim(file = here(paste0(out.path, trees_data$inv[1], "_LT_removed.csv")), sep = ",", dec = ".")
# soil data
soil_types_db <- read.delim(file = here(out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")
# importa data from literature research
bio_func_df <- read.delim(file = here(paste0(input.path, "B_lit_functions.csv")), sep = ",", dec = ".") 



# 0.4 data preparation ---------------------------------------------------------
trees_data <- trees_data %>% mutate(H_m = as.numeric(H_m))  %>% distinct() %>% 
  # join in soil data
  left_join(soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))

tapes_tree_data <- tapes_tree_data %>% mutate(H_m = as.numeric(H_m))  %>% distinct() %>% 
  # join in soil data
  left_join(soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))

# assign IDs to papers and functions
bio_func_df <- bio_func_df %>% 
  left_join(., 
            bio_func_df %>% 
              select(title, author, year, function.) %>% 
              distinct() %>% 
              mutate(func_ID = row_number()), 
            by = c("title", "author", "year", "function.")) 
# convert coeffcients into numbers: https://stackoverflow.com/questions/56963214/how-can-i-use-gsub-in-multiple-specific-column-in-r
bio_func_df[,13:27] <- lapply(bio_func_df[,13:27], gsub, pattern = "[^0-9.-]", replacement = "")
bio_func_df[,13:27] <- lapply(bio_func_df[,13:27], as.numeric)


# 1. Biomass calculations -------------------------------------------------
# now we will try to implement a loop for all biomass functions in the list 
# select all biomass functions that calculate aboveground biomass, are for Alnus trees, and don´t need to be backtransformed
alnus_agb_func_no_ln <- unique(bio_func_df[bio_func_df$compartiment %in% c("agb") & stringr::str_detect(bio_func_df$species, "Alnus") & is.na(bio_func_df$logarithm_B),])
# select alnus trees at organic sites
tree_data_alnus <- trees_data[trees_data$bot_genus %in% c("Alnus") & trees_data$min_org == "org",]  
alnus_agb_kg_tree <- vector("list", length = nrow(tree.df))
for (i in 1:nrow(alnus_agb_func_no_ln)){
 # i = 12
  
  func_id <- alnus_agb_func_no_ln$func_ID[i]  # ID of the function in literature research csv
  func <- alnus_agb_func_no_ln$function.[i]   # biomass function taken from respective reference 
  unit <- alnus_agb_func_no_ln$unit_B[i]      # unit of biomass returned, when g then /1000 for kg
  variables <- alnus_agb_func_no_ln$variables[i] # input variables for respective function 

  ## get input variables
  input.df <- tree_data_alnus[, unlist(strsplit(variables, '\\, ')), drop = FALSE]
  
  ## get coefficients 
  # select only those cooeficients that are needed https://sparkbyexamples.com/r-programming/select-columns-by-condition-in-r/
  coef.df <- as.data.frame((alnus_agb_func_no_ln[i,13:27]) %>% select_if(~ !all(is.na(.))))
 # create a vector that holds all coefficients as a character string to print it later when the function is build 
   coef.print <- vector("list", length = ncol(coef.df))
  for (j in 1:ncol(coef.df)) {
    # j = 1
    # take every coefficient 
    coef.print[[j]] <- paste(colnames(coef.df)[j], '<-', as.numeric(coef.df[,j]),';')
  } 
  # https://www.geeksforgeeks.org/how-to-collapse-a-list-of-characters-into-a-single-string-in-r/
  coef.print <- paste(coef.print, collapse= '' )
  
  
  ## create function: https://stackoverflow.com/questions/26164078/r-define-a-function-from-character-string
  bio_func_code <-paste(
    'bio_func <- function(', variables, ') {',
    coef.print, 
    'return(' , func , ') } '
    , sep='')
  
  ## check if function is valid
  eval(parse(text = bio_func_code))
  
  ## apply function 
  bio_tree <- apply(input.df, 1, function(row) {
    # Convert the row to a list and call bio_func with do.call
    do.call(bio_func, as.list(row))
  })
  
  # convert results to a numeric vector if needed
  
  tree.df <- as.data.frame(cbind(tree_data_alnus, "B_kg_tree" = c(bio_tree), "func_id" = c(func_id), "unit_b" = c(unit))) # 
  tree.df <- tree.df %>% mutate(B_kg_tree = ifelse(unit_b == "kg", as.numeric(B_kg_tree), as.numeric(B_kg_tree)/1000))
  
  alnus_agb_kg_tree[[i]] <- tree.df
  
  # Print or store results
  print(paste(i, func_id))
}

alnus_agb_kg_tree_df <- as.data.frame(rbindlist(alnus_agb_kg_tree)) %>% arrange(plot_ID, tree_ID)




# 2. visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
alnus_ag <-  plyr::rbind.fill(alnus_agb_kg_tree_df, 
                              (tapes_tree_data[
                                tapes_tree_data$compartiment == "ag" & 
                                  tapes_tree_data$bot_genus %in% c("Alnus") & 
                                  tapes_tree_data$min_org == "org",]) %>% 
                                mutate(func_id = "tapes")) 

ggplot(data = alnus_ag %>% filter(!(func_id %in% c("25", "24"))) # 25 and 25 are somehow weird so i kicked it out 
       )+ 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = func_id, color = as.factor(func_id)  ))+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree, group = func_id, color = as.factor(func_id)  ))






# NOTES -------------------------------------------------------------------

# select input for function: https://stackoverflow.com/questions/30474729/string-split-into-list-r
tree.df[,match(as.list(strsplit(args, '\\, ')[[1]]), names(tree.df))]
input_cols <- c(as.data.frame((tree.df[,match(as.list(strsplit(args, '\\, ')[[1]]), names(tree.df))])) )

# add coefficients to tree dataset by cbind
tree.df <- trees_data[trees_data$bot_genus %in% c("Alnus"),][1:10,]  
# https://stackoverflow.com/questions/58592636/r-how-to-select-columns-that-contains-strings-where-the-string-is-any-eleme
#select(plot_ID, tree_ID, SP_code, matches(paste(args, collapse="|"))) %>% 
tree.df$B_kg_tree <- bio_func((tree.df[1,match(as.list(strsplit(args, '\\, ')[[1]]), names(tree.df))]))
input_cols <- (tree.df[,match(as.list(strsplit(args, '\\, ')[[1]]), names(tree.df))]) %>% 
  mutate_all(., function(x) as.numeric(as.character(x)))











