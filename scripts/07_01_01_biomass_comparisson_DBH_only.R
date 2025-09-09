# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# paper about biomass of alnus and betula at peatland sites
# Henriette Gercken


# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path <- ("/output/out_data/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_3.csv" which contains plot area and stand data additionally to 
# tree data
tapes_tree_data <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_update_5.csv"), sep = ",", dec = ".")
trees_data <-tapes_tree_data %>% dplyr::select(-c("compartiment","B_kg_tree", "N_kg_tree", "C_kg_tree")) %>% dplyr::distinct()
trees_removed <- read.delim(file =paste0(getwd(), out.path, trees_data$inv[1], "_LT_removed.csv"), sep = ",", dec = ".")
# soil data
soil_types_db <- read.delim(file = paste0(getwd(), out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")
# importa data from literature research
bio_func_df <- read.delim(file = paste0(getwd(), "/data/input/", "B_lit_functions.csv"), sep = ",", dec = ".")
# summaries
all_summary <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_RG_DW_stocks_ha_all_groups.csv"), sep = ",", dec = ".")
LT_summary <- all_summary %>% filter(stand_component == "LT") %>% select(-c(dw_sp, dw_type, decay, inv_year, ST_LY_type, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m, n_dec, n_dw_TY))




# 0.4 data preparation ---------------------------------------------------------
trees_data <- trees_data %>% 
  dplyr::rename("H_m_old" = "H_m") %>% # change name of H_m which includes sampled and not sampled hieghts that are estimated with DBH and HG
  mutate(H_m = as.numeric(H_m_nls))  %>% # change HM to H_nls which is only SBH based heights
  distinct() %>% 
  # join in soil data
  left_join(soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))

tapes_tree_data <- tapes_tree_data %>% 
  dplyr::rename("H_m_old" = "H_m") %>% # change name of H_m which includes sampled and not sampled hieghts that are estimated with DBH and HG
  mutate(H_m = as.numeric(H_m_nls))  %>% # change HM to H_nls which is only SBH based heights
  distinct() %>% 
  # join in soil data
  left_join(soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2"))%>% 
  mutate(min_org = ifelse(inv == "momok", "org", min_org))

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
# exclude those functions that require age as we don´t always have it
bio_func_df <- bio_func_df[!(str_detect(bio_func_df$variables, "age")),]
write.csv(bio_func_df, paste0(getwd(), out.path, "bio_func_ID.csv"))

# view(bio_func_df %>% filter(peat %in% c("yes", "partly")) %>% select(paper_ID, author, year, species) %>% distinct() %>% ungroup()
# )
# 1. Biomass calculations -------------------------------------------------
# we need the biomass without leafes. 
# so we have these 3 scenarios: 
# leafes are one of many compartiments and possible to be excluded --> possible
# L> we calculate all compartiments and only sum up the woody ones 
# leafes are included but not possible to be excluded --> included 
# L> these functions are excluded
# status of leafes is unklnown --> unknown
# L> there functions are excluded
# the leafes are not available as a compartiment --> not included 
# L> no further calcualtions necesarry, we just do the biomass for agb and thats it 


# 1.1. ALNUS Biomass calculations -------------------------------------------------
# now we will try to implement a loop for all biomass functions in the list 
# select all biomass functions that calculate aboveground biomass, are for Alnus trees, and don´t need to be backtransformed
alnus_func <-subset(bio_func_df, species %like% "Alnus glutinosa" &     # select only Alnus specific species
                      !is.na(function.) &                    # select only those papers with functions
                      compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb") | 
                      species %like% "Alnus Glutinosa" &     # select only Alnus specific species
                      !is.na(function.) &                    # select only those papers with functions
                      compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb") | 
                      species %like% "Alnus spp." &     # select only Alnus specific species
                      !is.na(function.) &                    # select only those papers with functions
                      compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb")) 



# select alnus trees at organic sites
# with incana trees: 822 rows
# without incana trees: 817
tree_data_alnus <- trees_data[trees_data$bot_name %in% c("Alnus glutinosa") & 
                                trees_data$min_org == "org" |
                                trees_data$bot_name %in% c("Alnus spp.") &
                                trees_data$min_org == "org",]  
alnus_agb_kg_tree <- vector("list", length = nrow(tree_data_alnus))
for (i in 1:nrow(alnus_func)){
  # i = 23
  
  paper_id <- alnus_func$paper_ID[i]# ID of the paper in literature research csv
  func_id <- alnus_func$func_ID[i]  # ID of the function in literature research csv
  id <- alnus_func$ID[i]            # combination of func and paper id 
  func <- alnus_func$function.[i]   # biomass function taken from respective reference 
  peat_stat <-  alnus_func$peat[i]
  my.country <-  alnus_func$country[i]
  unit <- alnus_func$unit_B[i]      # unit of biomass returned, when g then /1000 for kg
  comp <- alnus_func$compartiment[i] 
  ln_stat <- alnus_func$logarithm_B[i]
  variables <- alnus_func$variables[i] # input variables for respective function 
  # if the ln status is == "ln", we have to later on backtransform the results.
  # to build the function automatically tho, we have to remove the ln from the function. column
  func <- ifelse(!is.na(ln_stat) & ln_stat == "ln" | !is.na(ln_stat) & ln_stat == "log10", 
                 paste0(gsub(".*\\((.*)\\).*", "\\1", sub('\\=.*', '', func)), '=', sub('.*=', '', func)), # select before and after symbol: https://stackoverflow.com/questions/37051288/extract-text-after-a-symbol-in-r
                 func)
  
  ## get input variables
  input.df <- tree_data_alnus[, unlist(strsplit(variables, '\\, ')), drop = FALSE]
  
  ## get coefficients 
  # select only those cooeficients that are needed https://sparkbyexamples.com/r-programming/select-columns-by-condition-in-r/
  coef.df <- as.data.frame((alnus_func[i,13:27]) %>% select_if(~ !all(is.na(.))))
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
  
  tree.df <- as.data.frame(cbind(tree_data_alnus
                                 , "B_kg_tree" = c(bio_tree)
                                 , "paper_ID" = c(paper_id)
                                 , "func_ID" = c(func_id)
                                 , "ID" = c(id)
                                 , "unit_B" = c(unit)
                                 , "logarithm_B" = c(ln_stat)
                                 , "compartiment" = c(comp)
                                 , "peat" = c(peat_stat)
                                 , "country" =c(my.country))) # 
  
  tree.df <- tree.df %>% mutate(  B_kg_tree = dplyr::case_when(!is.na(logarithm_B) & logarithm_B == "ln" ~ as.numeric(exp(B_kg_tree)), 
                                                               !is.na(logarithm_B) & logarithm_B == "log10" ~ as.numeric(10^(B_kg_tree)), 
                                                               TRUE ~ as.numeric(B_kg_tree))) %>%  # backtransform  the ln 
    mutate(  B_kg_tree = dplyr::case_when(unit_B == "kg" ~ as.numeric(B_kg_tree), 
                                          unit_B == "g" ~ as.numeric(B_kg_tree)/1000, 
                                          TRUE ~ as.numeric(B_kg_tree)))
  
  alnus_agb_kg_tree[[i]] <- tree.df
  
  # Print or store results
  print(paste(i, func_id))
}
alnus_agb_kg_tree_df <- as.data.frame(rbindlist(alnus_agb_kg_tree)) %>% arrange(plot_ID, tree_ID, paper_ID)

# summarise those trees biomass that was calculated by compartiment
# normal  compatiments without ag: !(alnus_agb_kg_tree_df$compartiment %in% c("abg", "agb")) 
## total ag
# ag for those papers that include already ndl alnus_func$ID[alnus_func$leafes_inkl == "included"] and have a function that calculates agb explicitly 
# can include ndl: alnus_func$ID[alnus_func$leafes_inkl == "possible"]  and have a function that calculates agb explicitly 
# can include ndl: alnus_func$ID[alnus_func$leafes_inkl == "possible"]  and do not have a function that calculates agb explicitly 


## wood ag 
# ag that doesnt include leafes alnus_func$ID[alnus_func$leafes_inkl == "not included"]
# ag that can include leafes but we exclude them: alnus_func$ID[alnus_func$leafes_inkl == "not included"] !(alnus_agb_kg_tree_df$compartiment %in% c("abg", "agb", "ndl"))

# now do we need thefilter which indentify the papers that do not come with a explicit function for ag  ?
# --> yes because we also have papers that have tree compartimens and ag functions 
# while if there is a ag function we want to use the results of that function and we want to aviod the ag be added up with the other compartiments 


# had to replace rbind.fill: https://stackoverflow.com/questions/18003717/efficient-way-to-rbind-data-frames-with-different-columns, https://stackoverflow.com/questions/44464441/r-is-there-a-good-replacement-for-plyrrbind-fill-in-dplyr 
alnus_agb_kg_tree_df <- rbind(
  # seperate tree compartiments without ag 
  setDT(alnus_agb_kg_tree_df[!(alnus_agb_kg_tree_df$compartiment %in% c("abg", "agb")),]),
  # agb including leaf mass from functions that have an explicit function for ag
  setDT(alnus_agb_kg_tree_df[alnus_agb_kg_tree_df$compartiment %in% c("abg", "agb") & alnus_agb_kg_tree_df$ID %in% c(alnus_func$ID[alnus_func$leafes_inkl %in% c("included", "possible")]) ,]), 
  # join in agb calculate from compartiments based on papers that don´t have seperate agb function but also  allow to exclude leafes: so they have eg. fwb, ndl and sw but no agb compartiment in their list 
  setDT(tree_data_alnus %>% 
          left_join(., setDT(alnus_agb_kg_tree_df)[  # this is an inner join in data.tabe: https://medium.com/analytics-vidhya/r-data-table-joins-48f00b46ce29 
            setDT(bio_func_df %>% 
                    filter(leafes_inkl %in% c("possible", "inlcuded")) %>%                 # agb shoul be included or possible to include
                    select(paper_ID, compartiment) %>%                                     # select papaer ID and compartiments 
                    distinct() %>%                                                         # make sure we only select them once
                    mutate(number = str_count(compartiment, "agb")) %>%                    # count the occurence of "agb" per paper and compartiment
                    group_by(paper_ID) %>% dplyr::summarise(mean_agb_number = mean(number)) %>%   # summarise the number of occurences of "agb" per paper 
                    filter(mean_agb_number == 0)),                                          # filter those papers that allow to calculate the "true" agb with leaves but dont have a agb function and by that have to be summed up  
            on = .(paper_ID), nomatch = NULL] %>% # close inner join data.table
              dplyr::group_by(plot_ID, tree_ID, paper_ID, peat, country, unit_B, logarithm_B) %>%  #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
              dplyr::summarise(B_kg_tree = sum(B_kg_tree)) %>%                      # sum up compartiemtns per tree per paper
              mutate(compartiment = "agb",                                          # name the calculates sum of compartiments agb
                     func_ID = "agb", 
                     ID = paste0(paper_ID, "_", func_ID)), 
            by =  c("plot_ID", "tree_ID")) ),
  # biomass of trees where function already EXcludes leaf mass
  setDT((alnus_agb_kg_tree_df[alnus_agb_kg_tree_df$compartiment %in% c("abg", "agb") &          # compartiment ag
                                alnus_agb_kg_tree_df$ID %in% c(alnus_func$ID[alnus_func$leafes_inkl == "not included"])   # of papers which have "not included"
                              , ]) %>% mutate(compartiment = "w_agb")),  
  # biomass of trees where function does not already excludes leaf mass and we have to sum up the woody compartiments  
  setDT(tree_data_alnus %>% 
          # join the tree info with the agb compartiment per tree
          left_join(., (alnus_agb_kg_tree_df[!(alnus_agb_kg_tree_df$compartiment %in% c("abg", "agb", "ndl")) &            # select seperate compartiments that are not ag or leafes 
                                               alnus_agb_kg_tree_df$ID %in% c(alnus_func$ID[alnus_func$leafes_inkl == "possible"],
                                                                              alnus_func$ID[alnus_func$leafes_inkl == "not included"]), ]) %>%   # of papers which have "possible"
                      dplyr::group_by(plot_ID, tree_ID, paper_ID, peat, country, unit_B, logarithm_B) %>%              #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
                      dplyr::summarise(B_kg_tree = sum(B_kg_tree)) %>%                                                        # sum up compartiemtns per tree per paper
                      mutate(compartiment = "w_agb",
                             func_ID = "w_agb", 
                             ID = paste0(paper_ID, "_", func_ID)), 
                    by =  c("plot_ID", "tree_ID")) ),
  fill = T) %>% 
  arrange(plot_ID, tree_ID, paper_ID, func_ID)






# 1.2. BETULA  -------------------------------------------------
# now we will try to implement a loop for all biomass functions in the list 
# select all biomass functions that calculate aboveground biomass, are for Alnus trees, and don´t need to be backtransformed
betula_func <- subset(bio_func_df, species %like% "Betula pubescens" &     # select only Alnus specific species
                        !is.na(function.) &                    # select only those papers with functions
                        compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb"))            # select only those paper which have leafes not icluded or a possible compartimentalisation

# 1.2.1. BETULA compartiment biomass calculations -------------------------------------------------
# select betula pubescens or spp trees at organic sites
# with betula pednula 518 rows, without 518
tree_data_betula <- trees_data[trees_data$bot_name %in% c("Betula pubescens") & 
                                 trees_data$min_org == "org" |
                                 trees_data$bot_name %in% c("Betula spp.") & 
                                 trees_data$min_org == "org" ,]  
betula_agb_kg_tree <- vector("list", length = nrow(tree.df))
for (i in 1:nrow(betula_func)){
  # i = 43
  
  paper_id <- betula_func$paper_ID[i]
  func_id <- betula_func$func_ID[i]  # ID of the function in literature research csv
  func <- betula_func$function.[i]   # biomass function taken from respective reference 
  id <- betula_func$ID[i]            # combination of func and paper id 
  peat_stat <- betula_func$peat[i]  
  my.country <- betula_func$country[i]  
  unit <- betula_func$unit_B[i]      # unit of biomass returned, when g then /1000 for kg
  comp <- betula_func$compartiment[i] 
  ln_stat <- betula_func$logarithm_B[i]
  variables <- betula_func$variables[i] # input variables for respective function 
  # if the ln status is == "ln", we have to later on backtransform the results.
  # to build the function automatically tho, we have to remove the ln from the function. column
  func <- ifelse(!is.na(ln_stat) & ln_stat == "ln" | !is.na(ln_stat) & ln_stat == "log10", 
                 paste0(gsub(".*\\((.*)\\).*", "\\1", sub('\\=.*', '', func)), '=', sub('.*=', '', func)), # select before and after symbol: https://stackoverflow.com/questions/37051288/extract-text-after-a-symbol-in-r
                 func)
  
  ## get input variables
  input.df <- tree_data_betula[, unlist(strsplit(variables, '\\, ')), drop = FALSE]
  
  ## get coefficients 
  # select only those cooeficients that are needed https://sparkbyexamples.com/r-programming/select-columns-by-condition-in-r/
  coef.df <- as.data.frame((betula_func[i,13:27]) %>% select_if(~ !all(is.na(.))))
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
  
  tree.df <- as.data.frame(cbind(tree_data_betula
                                 , "B_kg_tree" = c(bio_tree)
                                 , "paper_ID" = c(paper_id)
                                 , "func_ID" = c(func_id)
                                 , "ID" = c(id)
                                 , "unit_B" = c(unit)
                                 , "logarithm_B" = c(ln_stat)
                                 , "compartiment" = c(comp)
                                 , "peat" = c(peat_stat)
                                 , "country" =c(my.country)))
  tree.df <- tree.df %>% mutate(  B_kg_tree = dplyr::case_when(!is.na(logarithm_B) & logarithm_B == "ln" ~ as.numeric(exp(B_kg_tree)), 
                                                               !is.na(logarithm_B) & logarithm_B == "log10" ~ as.numeric(10^(B_kg_tree)), 
                                                               TRUE ~ as.numeric(B_kg_tree))) %>%  # backtransform  the ln 
    mutate(  B_kg_tree = dplyr::case_when(unit_B == "kg" ~ as.numeric(B_kg_tree), 
                                          unit_B == "g" ~ as.numeric(B_kg_tree)/1000, 
                                          TRUE ~ as.numeric(B_kg_tree)))
  
  betula_agb_kg_tree[[i]] <- tree.df
  
  # Print or store results
  print(paste(i, func_id))
}
betula_agb_kg_tree_df <- as.data.frame(rbindlist(betula_agb_kg_tree)) %>% arrange(plot_ID, tree_ID, paper_ID)


# 1.2.2. BETULA agb, wagb compartiement calculation ---------------------------------------------------------
# had to replace rbind.fill: https://stackoverflow.com/questions/18003717/efficient-way-to-rbind-data-frames-with-different-columns, https://stackoverflow.com/questions/44464441/r-is-there-a-good-replacement-for-plyrrbind-fill-in-dplyr 
betula_agb_kg_tree_df <- rbind(
  # seperate tree compartiments without ag 
  setDT(betula_agb_kg_tree_df[!(betula_agb_kg_tree_df$compartiment %in% c("agb")),]),
  # agb including leaf mass from functions that have an explicit function for ag
  setDT(betula_agb_kg_tree_df[betula_agb_kg_tree_df$compartiment %in% c("agb") & betula_agb_kg_tree_df$ID %in% c(betula_func$ID[betula_func$leafes_inkl %in% c("included", "possible")]) ,]), 
  # agb calculate from compartiments based on papers that don´t have seperate agb function but also  allow to exclude leafes: so they have eg. fwb, ndl and sw but no agb compartiment in their list 
  setDT(tree_data_betula %>% 
          left_join(., setDT(betula_agb_kg_tree_df)[  # this is an anti join in data.tabe 
            setDT(bio_func_df %>% 
                    filter(leafes_inkl %in% c("possible", "inlcuded")) %>%                 # agb shoul be included or possible to include
                    select(paper_ID, compartiment) %>%                                     # select papaer ID and compartiments 
                    distinct() %>%                                                         # make sure we only select them once
                    mutate(number = str_count(compartiment, "agb")) %>%                    # count the occurence of "agb" per paper and compartiment
                    group_by(paper_ID) %>% dplyr::summarise(mean_agb_number = mean(number)) %>%   # summarise the number of occurences of "agb" per paper 
                    filter(mean_agb_number == 0)),                                          # filter those papers that allow to calculate the "true" agb with leaves but dont have a agb function and by that have to be summed up  
            on = .(paper_ID), 
            nomatch = NULL] %>% # close anti join data.table
              dplyr::group_by(plot_ID, tree_ID, paper_ID, peat, country, unit_B, logarithm_B) %>%  #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
              dplyr::summarise(B_kg_tree = sum(B_kg_tree)) %>%                      # sum up compartiemtns per tree per paper
              mutate(compartiment = "agb",                                          # name the calculates sum of compartiments agb
                     func_ID = "agb", 
                     ID = paste0(paper_ID, "_", func_ID)), 
            by =  c("plot_ID", "tree_ID")) ),
  # biomass of trees where function already EXcludes leaf mass
  setDT((betula_agb_kg_tree_df[betula_agb_kg_tree_df$compartiment %in% c("abg", "agb") &          # compartiment ag
                                 betula_agb_kg_tree_df$ID %in% c(betula_func$ID[betula_func$leafes_inkl == "not included"])   # of papers which have "not included"
                               , ]) %>% mutate(compartiment = "w_agb")),  
  # biomass of trees where function does not already excludes leaf mass and we have to sum up the woody compartiments  
  setDT(tree_data_betula %>% 
          # join the tree info with the agb compartiment per tree
          left_join(., (betula_agb_kg_tree_df[!(betula_agb_kg_tree_df$compartiment %in% c("abg", "agb", "ndl")) &            # select seperate compartiments that are not ag or leafes 
                                                betula_agb_kg_tree_df$ID %in% c(betula_func$ID[betula_func$leafes_inkl == "possible"],
                                                                                betula_func$ID[betula_func$leafes_inkl == "not included"]), ]) %>%   # of papers which have "possible"
                      dplyr::group_by(plot_ID, tree_ID, paper_ID, peat, country, unit_B, logarithm_B) %>%              #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
                      dplyr::summarise(B_kg_tree = sum(B_kg_tree)) %>%                                                        # sum up compartiemtns per tree per paper
                      mutate(compartiment = "w_agb",
                             func_ID = "w_agb", 
                             ID = paste0(paper_ID, "_", func_ID)), 
                    by =  c("plot_ID", "tree_ID")) ),
  fill = T) %>% 
  arrange(plot_ID, tree_ID, paper_ID, func_ID)




# 2. tapeS wagb compartiement calculation ---------------------------------------------------------
# caclulate the compartimetn "wag" woody aboveground biomass for trees calcualted with tapes by sumiinf up all woody compartiments per single tree 
wagb_tapes <- unique(
  # select remove biomass etc. from the tapes tree dataset to join the wag per single tree in
  setDT(subset(tapes_tree_data, select = -c(B_kg_tree,   N_kg_tree,   C_kg_tree, compartiment))) [setDT( # this is a leaft join in data.table
    # here comes the summary
    tapes_tree_data [!(tapes_tree_data$compartiment %in% c("ag", "bg", "total", "ndl")),] %>% 
      group_by(plot_ID, tree_ID) %>% 
      dplyr::summarise(B_kg_tree = sum(B_kg_tree)) %>% 
      mutate(compartiment = "w_agb",
             paper_ID = max(na.omit(as.numeric(bio_func_df$paper_ID)))+1, 
             func_ID = "tapes",
             ID = paste0(paper_ID, "_", func_ID), 
             country = "Germany", 
             country_code = "GER", 
             peat = "no")
  ), on = .(plot_ID, tree_ID) ])

# 2.1. alnus tapeS wagb compartiement calculation ---------------------------------------------------------
alnus_wagb_tapes <- wagb_tapes[wagb_tapes$bot_genus %in% c("Alnus") & 
                                 wagb_tapes$bot_species %in% c("glutinosa", "spp.") &
                                 wagb_tapes$min_org == "org", ]

# 2.2. betula tapeS wagb compartiement calculation ---------------------------------------------------------
betula_wagb_tapes <- wagb_tapes[wagb_tapes$bot_genus %in% c("Betula") &
                                  wagb_tapes$bot_species %in% c("pubescens", "spp.") &
                                  wagb_tapes$min_org == "org", ]



# 3. EXPORT ------------------------------------------------------------------
# 3.1. prepare for export: bind all datasets with incdinvidual tree infos together ------------------------------------------------------------------
trees_data_update_5 <- rbind(
  # tapes all trees data
  setDT(tapes_tree_data) %>% mutate(
    paper_ID = max(na.omit(as.numeric(bio_func_df$paper_ID)))+1, 
    func_ID = "tapes", 
    country = "Germany",
    country_code = "DEU",
    peat = "no",
    ID = paste0(paper_ID, "_", func_ID)) ,
  # betula
  setDT(betula_wagb_tapes),  # betula tapes wag
  setDT(betula_agb_kg_tree_df), # betula external biomass functions
  # alnus
  setDT(alnus_wagb_tapes), # alnus tapes wag 
  setDT(alnus_agb_kg_tree_df),  # alnus external biomass functions
  fill = T) %>%  
  arrange(plot_ID, tree_ID, paper_ID, func_ID)



# 3.2. export: all datasets with incdinvidual tree infos together ------------------------------------------------------------------
write.csv(trees_data_update_5, paste0(getwd(), out.path, paste(trees_data_update_5$inv[1], "LT_update_6_1", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")


stop("biomass comparisson end")

# 4. statistical characteristics ---------------------------------------------------------------
# ((endwert-anfangswert)/ anfangswert)*100
# general statistics of tree dataset
trees_data %>% 
  filter(bot_genus %in% c("Alnus", "Betula") & 
           bot_species %in% c("glutinosa", "pubescens", "spp.") & 
           min_org == "org" & 
           compartiment == "ag") %>% 
  group_by(bot_genus, min_org) %>% 
  dplyr::summarise(mean_DBH = mean(DBH_cm), 
                   min_DBH = min(DBH_cm), 
                   max_DBH = max(DBH_cm),
                   mean_H = mean(H_m), 
                   mean_H_old = mean(H_m_old), 
                   n = n() )


# bot_name         min_org mean_DBH mean_H
# <chr>            <chr>      <dbl>  <dbl>
# 1 Alnus glutinosa  min       25.4   19.9
# 2 Alnus glutinosa  org         20.8   17.6
# 3 Betula pubescens min         18.0   17.6
# 4 Betula pubescens org         17.0   16.0

# bot_genus min_org mean_DBH min_DBH max_DBH mean_H mean_H_old
# <chr>     <chr>      <dbl>   <dbl>   <dbl>  <dbl>      <dbl>
#  Alnus     min         25.4     7.2    60.0   19.9       20.0
#  Alnus     org         21.1     7.1    61.4   17.6       17.6
#  Betula    min         18.0     7      38.2   17.6       17.7
#  Betula    org         16.7     7.1    50     16.2       16.6


trees_data %>% 
  filter(H_method == "sampled" & compartiment == "ag") %>% 
  group_by(bot_name, min_org) %>% 
  dplyr::summarise(mean_DBH = mean(DBH_cm), 
                   min_DBH = min(DBH_cm), 
                   max_DBH = max(DBH_cm),
                   mean_H_sampled = mean(H_m_old), 
                   min_H = min(H_m_old), 
                   max_H = max(H_m_old), 
                   n = n()) %>% 
  filter(bot_name %in% c("Alnus glutinosa", "Betula pubescens"))

# bot_name         min_org mean_DBH min_DBH max_DBH mean_H_sampled min_H max_H     n
# <chr>            <chr>      <dbl>   <dbl>   <dbl>          <dbl> <dbl> <dbl> <int>
# Alnus glutinosa  min         25.4    8.15    60.0           19.4   4.6  35.5   135
# Alnus glutinosa  org         20.5    7.1     61.4           16.0   4.6  32.5   196
# Betula pubescens min         19.3    7       38.2           17.8   4.8  26.8    99
# Betula pubescens org         19.0    7.1     50             16.0   4.5  25.1   114


# 4.1.Alnus ------------------------------------------------------------------------
# 4.1.1. Alnus all functions -----------------------------------------------
alnus_wag <-  trees_data_update_5[trees_data_update_5$compartiment %in% c("w_agb") &
                                    trees_data_update_5$bot_genus %in% c("Alnus") & 
                                    trees_data_update_5$min_org == "org" &
                                    trees_data_update_5$paper_ID != 9
                                  , ]


m_B_alnus <- mean(alnus_wag$B_kg_tree) # mean
sd_B_alnus <- sd(alnus_wag$B_kg_tree)  # sd
cv_B_alnus <- sd_B_alnus/m_B_alnus     # cv

m_dbh_alnus = mean(alnus_wag$DBH_cm)
min_dbh_alnus = min(alnus_wag$DBH_cm)
max_dbh_alnus = max(alnus_wag$DBH_cm)

# 4.1.2. Alnus Tapes -----------------------------------------------
alnus_wag_tapes <-  trees_data_update_5[trees_data_update_5$compartiment %in% c("w_agb") &
                                          trees_data_update_5$bot_genus %in% c("Alnus") & 
                                          trees_data_update_5$min_org == "org" &
                                          trees_data_update_5$func_ID == "tapes"
                                        , ]


m_B_alnus_tapes <- mean(alnus_wag_tapes$B_kg_tree) # mean
sd_B_alnus_tapes <- sd(alnus_wag_tapes$B_kg_tree)  # sd
cv_B_alnus_tapes <- sd_B_alnus_tapes/m_B_alnus_tapes     # cv

max(alnus_wag_tapes$B_kg_tree)

min(alnus_wag_tapes$B_kg_tree)

# 4.1.3. alnus extreme function characteristics ---------------------------
# summarize mean B per tree per function per species
alnus_wag_mean_func <- alnus_wag %>% 
  group_by(paper_ID, func_ID, bot_genus, compartiment) %>% 
  dplyr::summarise(B_kg_tree = mean(B_kg_tree))



# 4.1.3.1. min alnus  function characteristics ---------------------------
# min biomass
min(alnus_wag$B_kg_tree) # 4.389455

# min function: 28 
alnus_wag$paper_ID[alnus_wag$B_kg_tree == min(alnus_wag$B_kg_tree)] # 28
alnus_wag_mean_func$paper_ID[alnus_wag_mean_func$B_kg_tree == min(alnus_wag_mean_func$B_kg_tree)] # 14!!!!

# min mean biomass
min(alnus_wag_mean_func$B_kg_tree) #  145.8856
# mean of min function 28: 160.6613
mean(alnus_wag$B_kg_tree[alnus_wag$paper_ID == alnus_wag$paper_ID[alnus_wag$B_kg_tree == min(alnus_wag$B_kg_tree)]]) 


# 4.1.3.2. max alnus  function characteristics ---------------------------
# max biomass: 
max(alnus_wag$B_kg_tree) # 2657.473
# max function: 
alnus_wag$paper_ID[alnus_wag$B_kg_tree == max(alnus_wag$B_kg_tree)] # 40 
alnus_wag_mean_func$paper_ID[alnus_wag_mean_func$B_kg_tree == max(alnus_wag_mean_func$B_kg_tree)] # 27!!!

# mean of max function: 219.4056 
mean(alnus_wag$B_kg_tree[alnus_wag$paper_ID == alnus_wag$paper_ID[alnus_wag$B_kg_tree == max(alnus_wag$B_kg_tree)]]) 
# biomass of the function that predicts averagely highest biomass 
max(alnus_wag_mean_func$B_kg_tree) #  231.9356




boxplot(as.numeric(alnus_wag$B_kg_tree) ~ as.factor(alnus_wag$ID))

# 4.1.4. Alnus difference tapes - all functions ------------------------------------------------------------------
m_B_alnus_tapes - m_B_alnus



# 4.2. Betula ------------------------------------------------------------------------
# 4.2.1. Betula all functions ---------------------------------------------
betula_wag <-  trees_data_update_5[compartiment %in% c("w_agb") & bot_genus %in% c("Betula") & min_org == "org", ]

m_B_betula <- mean(betula_wag$B_kg_tree)
sd_B_betula <- sd(betula_wag$B_kg_tree)
cv_B_betula <- sd_B_betula/m_B_betula


m_dbh_betula = mean(betula_wag$DBH_cm)
min_dbh_betula = min(betula_wag$DBH_cm)
max_dbh_betula = max(betula_wag$DBH_cm)
# 4.2.2. Betula tapes ---------------------------------------------
betula_wag_tapes <-  trees_data_update_5[compartiment %in% c("w_agb") & 
                                           bot_genus %in% c("Betula") & 
                                           min_org == "org" & 
                                           trees_data_update_5$func_ID == "tapes", ]

m_B_betula_tapes <- mean(betula_wag_tapes$B_kg_tree)
sd_B_betula_tapes <- sd(betula_wag_tapes$B_kg_tree)
cv_B_betula_tapes <- sd_B_betula_tapes/m_B_betula_tapes
max(betula_wag_tapes$B_kg_tree)

boxplot(as.numeric(betula_wag$B_kg_tree) ~ as.factor(betula_wag$ID))





# 4.2.3. Betula extreme function characteristics ---------------------------
# summarize mean B per tree per function per species
betula_wag_mean_func <- betula_wag %>% 
  group_by(paper_ID, func_ID, bot_genus, compartiment) %>% 
  dplyr::summarise(B_kg_tree = mean(B_kg_tree))


# 4.2.3.1. min Betula  function characteristics ---------------------------
# min biomass
min(betula_wag$B_kg_tree) # 8.123259
# min function: 28 
betula_wag$paper_ID[betula_wag$B_kg_tree == min(betula_wag$B_kg_tree)] # 6
betula_wag_mean_func$paper_ID[betula_wag_mean_func$B_kg_tree == min(betula_wag_mean_func$B_kg_tree)] # 10!!!!
# mean of min function: 
mean(betula_wag$B_kg_tree[betula_wag$paper_ID == betula_wag$paper_ID[betula_wag$B_kg_tree == min(betula_wag$B_kg_tree)]]) # 160.6613
# biomass of the function that produces min mean biomass
min(betula_wag_mean_func$B_kg_tree) # 56.54606


# 4.2.3.2. max betula  function characteristics ---------------------------
# max biomass: 
max(betula_wag$B_kg_tree) # 2587.323
# max function: 
betula_wag$paper_ID[betula_wag$B_kg_tree == max(betula_wag$B_kg_tree)] # 20
betula_wag_mean_func$paper_ID[betula_wag_mean_func$B_kg_tree == max(betula_wag_mean_func$B_kg_tree)] # 29!!!!
# mean of max function: 
mean(betula_wag$B_kg_tree[betula_wag$paper_ID == betula_wag$paper_ID[betula_wag$B_kg_tree == max(betula_wag$B_kg_tree)]]) # 164.8527
max(betula_wag_mean_func$B_kg_tree) # 229.5567


# 4.2.3.3. betula difference tapes - all functions ------------------------------------------------------------------
m_B_betula_tapes - m_B_betula





# 4.3. Statistical analysis of biomass equation groups  --------------------------------------------------------------------------------
# 4.3.1. Test for any sign. differences between equations -----------------------------------------------------------------------------
# lets test for significant differences between the groups but without knowing what nature the differences have

# 4.3.1.1. test for requirements of ANOVA/ Kuskal-Wallis ----------------------------------------------------------
# requirements for ANOVA:
# https://www.sthda.com/english/wiki/one-way-anova-test-in-r
# - The observations are obtained independently and randomly from the population defined by the factor levels
# - The data of each factor level are normally distributed. --> Shapiro test 
#- These normal populations have a common variance. (Levene’s test can be used to check this.)
# - more common for continuous data

# 4.3.1.1.1. test for normality: shapiro ----------------------------------------------------------
# to test for normality we perform a shapiro test by group: 
# subset trees dataset by only selecting the WAG of betula and alnus at organic plots 
trees_data_al_bet_wag <- (trees_data_update_5[compartiment == "w_agb" & 
                                                bot_name %in% c("Betula pubescens"
                                                                ,"Betula spp."  # spp is being selected too because we assume alnus and betula at organic plots to be glutinosa/ pubescens
                                                                ,"Alnus glutinosa" 
                                                                ,"Alnus spp."
                                                ) & 
                                                min_org == "org" & 
                                                paper_ID != 9, ])
# as we also want to test the difference between tapes and the mean across all functions, 
# we are going to add another row per tree that holds the mean abovagrond biomass of that
# tree across all equations 
trees_data_al_bet_wag <- rbind(
  trees_data_al_bet_wag,
  # as we want to carry on the tree atributes we going to join the summary per tree back together
  # with the single tree atributes. therefore we have to deslect all function related info first 
  # and then add it as the end as the mean works like another biomass equation in this comparisson: 
  trees_data_al_bet_wag %>% select(-c("B_kg_tree", "func_ID", "paper_ID", "ID", 
                                      "country", "country_code",   "peat", 
                                      "unit_B", "logarithm_B")) %>% distinct() %>% 
    left_join(., 
              trees_data_al_bet_wag %>% 
                group_by(plot_ID, tree_ID) %>% 
                summarise(B_kg_tree = mean(B_kg_tree)) %>% # summarise mean of biomass per tree
                # add "mean" function info
                mutate(ID = "mean", 
                       func_ID = "mean", 
                       paper_ID = "mean", 
                       country = "all", 
                       country_code = "all",   
                       peat = "partly", 
                       unit_B = "kg", 
                       logarithm_B = NA), 
              # join back in with normal tree data 
              by = c("plot_ID", "tree_ID"))
)# close r bind 

# create list for output tibbles
shap.output <- vector("list", length = nrow(unique(trees_data_al_bet_wag[, c("bot_genus", "ID")])))

# loop to run shapiro for ever group: Species and function
for (i in 1:nrow(unique(trees_data_al_bet_wag[, c("bot_genus", "ID")])) ) {
  #i = 1
  
  my.func.id <- unique(trees_data_al_bet_wag[, c("bot_genus", "ID")])[i, "ID"]
  my.spec <- unique(trees_data_al_bet_wag[, c("bot_genus", "ID")])[i, "bot_genus"]
  # subset data for shapiro
  df_for_shapiro <- unique(trees_data_al_bet_wag[bot_genus == my.spec & ID == my.func.id, ])
  # run shapiro test per group
  shap.output.df <- tidy(shapiro.test(unique(df_for_shapiro$B_kg_tree)) )
  # put output of shap it in dataframe
  shap.output[[i]] <- as.data.frame(cbind(my.func.id # add func id ans species 
                                          , my.spec
                                          , shap.output.df))
  #control print
  print(c(i, my.func.id, my.spec))
  
}
# save output to dataframe
shap_out_al_bet <- as.data.frame(rbindlist(shap.output))

# interpret results: 
# https://www.sthda.com/english/wiki/normality-test-in-r#google_vignette
# https://rstudiodatalab.medium.com/how-to-perform-and-interpret-the-shapiro-wilk-test-in-r-afab5234997d
# from the output, the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# In other words, we can assume the normality.
# If the p-value is less than 0.05, we reject the null hypothesis that the data is normally distributed.
# --> p-value > 0.05 --> normality

nrow(shap_out_al_bet["p.value" < 0.05, ])
# there is no group with p-value above 0.05 so all biomasses of all groups are normally distributed

# 4.3.1.1.2. test common variance: Levene’s test ----------------------------------------------------------
# https://www.geeksforgeeks.org/r-language/levenes-test-in-r-programming/

levene.output <- vector("list", length =length(unique(trees_data_al_bet_wag$bot_genus) ) )
for (i in 1:length(unique(trees_data_al_bet_wag$bot_genus) ) ) {
  #i = 2
  my.spec <- as.character(unique(trees_data_al_bet_wag$bot_genus)[i])
  # subset data for levene
  df_for_levene <- unique(trees_data_al_bet_wag[bot_genus == my.spec, ])
  
  # run levene by group 
  levene.output.df <- tidy(car::leveneTest(B_kg_tree ~ ID, df_for_levene))
  
  # bind results of levene it in dataframe
  levene.output[[i]] <- as.data.frame(cbind( my.spec
                                             , levene.output.df))
  
  #control print
  print(c(i, my.spec))
  
}
# save output to dataframe
levene_out_al_bet <- as.data.frame(rbindlist(levene.output))

# interpret results
# H0: p-value > 0.05 --> All populations variances are equal --> which is what we want
# H1: p-value < 0.05 --> variances are significantly different 
nrow(levene_out_al_bet["p.value" < 0.05, ])

# there are no groups that have significantly different variances 


# 4.3.1.2. ANOVA ----------------------------------------------------------
# we have met the requirements for anova (continuous, normality, common variance)
# so we do an anova per species to compare all equations
anova.output <- vector("list", length =length(unique(trees_data_al_bet_wag$bot_genus) ) )
for (i in 1:length(unique(trees_data_al_bet_wag$bot_genus) ) ) {
  #i = 2
  my.spec <- as.character(unique(trees_data_al_bet_wag$bot_genus)[i])
  # subset data for levene
  df_for_anova <- unique(trees_data_al_bet_wag[bot_genus == my.spec, ])
  
  # run anova by specie and group 
  res.anova <- aov(B_kg_tree ~ ID, data = df_for_anova)
  anova.output.df <- tidy(aov(B_kg_tree ~ ID, data = df_for_anova))
  # bind results of levene it in dataframe
  anova.output[[i]] <- as.data.frame(cbind( my.spec
                                            , anova.output.df))
  
  
  #control print
  print(c(i, my.spec))
  print(summary(aov(B_kg_tree ~ ID, data = df_for_anova)))
  
}
# save output to dataframe
anova_out_al_bet <- as.data.frame(rbindlist(anova.output))


# interpret results
# https://www.sthda.com/english/wiki/one-way-anova-test-in-r 
# As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences 
# between the groups highlighted with “*" in the model summary.
anova_out_al_bet[!is.na(as.numeric(anova_out_al_bet$p.value)) & as.numeric(anova_out_al_bet$p.value)< 0.05, ]

## we find a p value below 0.05 for both species -->  so both species 
## have significant differences within the results of the equations

# some diagnostics
plot(res.anova, 1)
plot(res.anova, 2)
plot(res.anova, 3)
plot(res.anova, 4)
shapiro.test(res.anova$residuals)



# 4.3.2. test where differneces are: Turkey test ------------------------------------------
# In one-way ANOVA test, a significant p-value indicates that some of
# the group means are different, but we don’t know which pairs of groups are different.
# It’s possible to perform multiple pairwise-comparison, 
# to determine if the mean difference between specific pairs 
# of group are statistically significant, e.g. with Tukey multiple pairwise-comparisons
# As the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) for performing multiple pairwise-comparison between the means of groups.
# lets explore the nature of the differences between the groups. 
# how does TapeS predict in comparisson to all other functions 


turkey.output <- vector("list", length =length(unique(trees_data_al_bet_wag$bot_genus) ) )
for (i in 1:length(unique(trees_data_al_bet_wag$bot_genus) ) ) {
  #i = 2
  my.spec <- as.character(unique(trees_data_al_bet_wag$bot_genus)[i])
  # subset data for levene
  df_for_anova <- unique(trees_data_al_bet_wag[bot_genus == my.spec, ])
  
  # run anova by specie and group 
  res.aov <- aov(B_kg_tree ~ ID, data = df_for_anova)
  turkey <- TukeyHSD(res.aov)
  
  
  # run turkey and safe outout in dataframe 
  turkey.output.df <- tidy(turkey)
  
  # bind results of levene it in dataframe
  turkey.output[[i]] <- as.data.frame(cbind( my.spec
                                             , turkey.output.df))
  
  
  #control print
  print(c(i, my.spec))
  print(TukeyHSD(res.aov))
  # compute letters to show significance
  print(multcompView::multcompLetters4(res.aov, turkey))
  
}
# save output to dataframe
turkey_out_al_bet <- as.data.frame(rbindlist(turkey.output))

# interpret results
# It can be seen from the output, that only the difference  with an adjusted p-value < 0.05 are significant


#filter for tapes comparissons: 
# filter for tapes comparissons that show a significant difference
turkey_out_al_bet_tapes <- setDT(turkey_out_al_bet)[str_detect(turkey_out_al_bet$contrast, "tapes"), ]
# filter for tapes comparissons that dont show a significant difference 
turkey_out_al_bet_tapes[turkey_out_al_bet_tapes$adj.p.value >= 0.05, ]
# filter for tapes comparissons that  show a significant difference 
setDT(turkey_out_al_bet_tapes)[turkey_out_al_bet_tapes$adj.p.value < 0.05, ]

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




stop("this is where biomass comparison singe tree script paper stops")




# 4.4. Statistical analysis of biomass by peat/no peat --------------------------------------------------------------------------------
# 4.4.1. Test for any sign. differences between equations -----------------------------------------------------------------------------
# lets test for significant differences between the groups but without knowing what nature the differences have

# 4.4.1.1. test for requirements of ANOVA/ Kuskal-Wallis ----------------------------------------------------------
# requirements for ANOVA:
# https://www.sthda.com/english/wiki/one-way-anova-test-in-r
# - The observations are obtained independently and randomly from the population defined by the factor levels
# - The data of each factor level are normally distributed. --> Shapiro test 
#- These normal populations have a common variance. (Levene’s test can be used to check this.)
# - more common for continuous data

# 4.4.1.1.1. test for normality: shapiro ----------------------------------------------------------
# to test for normality we perform a shapiro test by group: 
# subset trees dataset by only selecting the WAG of betula and alnus at organic plots 
trees_data_al_bet_wag <- (trees_data_update_5[compartiment == "w_agb" & 
                                                bot_name %in% c("Betula pubescens"
                                                                ,"Betula spp."  # spp is being selected too because we assume alnus and betula at organic plots to be glutinosa/ pubescens
                                                                ,"Alnus glutinosa" 
                                                                ,"Alnus spp."
                                                ) & 
                                                min_org == "org" & 
                                                paper_ID != 9, ])


# create list for output tibbles
shap.output <- vector("list", length = nrow(unique(trees_data_al_bet_wag[, c("bot_genus", "peat")])))

# loop to run shapiro for ever group: Species and function
for (i in 1:nrow(unique(trees_data_al_bet_wag[, c("bot_genus", "peat")])) ) {
  #i = 1
  
  my.peat <- unique(trees_data_al_bet_wag[, c("bot_genus", "peat")])[i, "peat"]
  my.spec <- unique(trees_data_al_bet_wag[, c("bot_genus", "peat")])[i, "bot_genus"]
  # subset data for shapiro
  df_for_shapiro <- unique(trees_data_al_bet_wag[bot_genus == my.spec & peat == my.peat, ])
  # run shapiro test per group
  shap.output.df <- tidy(shapiro.test(unique(df_for_shapiro$B_kg_tree)) )
  # put output of shap it in dataframe
  shap.output[[i]] <- as.data.frame(cbind(my.peat # add func id ans species 
                                          , my.spec
                                          , shap.output.df))
  #control print
  print(c(i, my.peat, my.spec))
  
}
# save output to dataframe
shap_out_peat_al_bet <- as.data.frame(rbindlist(shap.output))

# interpret results: 
# https://www.sthda.com/english/wiki/normality-test-in-r#google_vignette
# https://rstudiodatalab.medium.com/how-to-perform-and-interpret-the-shapiro-wilk-test-in-r-afab5234997d
# from the output, the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# In other words, we can assume the normality.
# If the p-value is less than 0.05, we reject the null hypothesis that the data is normally distributed.
# --> p-value > 0.05 --> normality

nrow(shap_out_peat_al_bet["p.value" < 0.05, ])
# there is no group with p-value above 0.05 so all biomasses of all groups are normally distributed

# 4.3.1.1.2. test common variance: Levene’s test ----------------------------------------------------------
# https://www.geeksforgeeks.org/r-language/levenes-test-in-r-programming/

levene.output <- vector("list", length =length(unique(trees_data_al_bet_wag$bot_genus) ) )
for (i in 1:length(unique(trees_data_al_bet_wag$bot_genus) ) ) {
  #i = 2
  my.spec <- as.character(unique(trees_data_al_bet_wag$bot_genus)[i])
  # subset data for levene
  df_for_levene <- unique(trees_data_al_bet_wag[bot_genus == my.spec, ])
  
  # run levene by group 
  levene.output.df <- tidy(car::leveneTest(B_kg_tree ~ peat, df_for_levene))
  
  # bind results of levene it in dataframe
  levene.output[[i]] <- as.data.frame(cbind( my.spec
                                             , levene.output.df))
  
  #control print
  print(c(i, my.spec))
  
}
# save output to dataframe
levene_out_al_bet <- as.data.frame(rbindlist(levene.output))

# interpret results
# H0: p-value > 0.05 --> All populations variances are equal --> which is what we want
# H1: p-value < 0.05 --> variances are significantly different 
nrow(levene_out_al_bet["p.value" < 0.05, ])

# there are no groups that have significantly different variances 


# 4.3.1.2. ANOVA ----------------------------------------------------------
# we have met the requirements for anova (continuous, normality, common variance)
# so we do an anova per species to compare all equations
anova.output <- vector("list", length =length(unique(trees_data_al_bet_wag$bot_genus) ) )
for (i in 1:length(unique(trees_data_al_bet_wag$bot_genus) ) ) {
  #i = 1
  my.spec <- as.character(unique(trees_data_al_bet_wag$bot_genus)[i])
  # subset data for levene
  df_for_anova <- unique(trees_data_al_bet_wag[bot_genus == my.spec, ])
  
  # run anova by specie and group 
  res.anova <- aov(B_kg_tree ~ peat, data = df_for_anova)
  anova.output.df <- tidy(aov(B_kg_tree ~ peat, data = df_for_anova))
  # bind results of levene it in dataframe
  anova.output[[i]] <- as.data.frame(cbind( my.spec
                                            , anova.output.df))
  
  
  #control print
  print(c(i, my.spec))
  print(summary(aov(B_kg_tree ~ peat, data = df_for_anova)))
  
}
# save output to dataframe
anova_out_al_bet <- as.data.frame(rbindlist(anova.output))


# interpret results
# https://www.sthda.com/english/wiki/one-way-anova-test-in-r 
# As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences 
# between the groups highlighted with “*" in the model summary.
anova_out_al_bet[!is.na(as.numeric(anova_out_al_bet$p.value)) & as.numeric(anova_out_al_bet$p.value)< 0.05, ]

## we find a p value below 0.05 for both species -->  so both species 
## have significant differences within the results of the equations that are peat or not peat specific 

# some diagnostics
plot(res.anova, 1)
plot(res.anova, 2)
plot(res.anova, 3)
plot(res.anova, 4)
shapiro.test(res.anova$residuals)



# 4.3.2. test where differneces are: Turkey test ------------------------------------------
# In one-way ANOVA test, a significant p-value indicates that some of
# the group means are different, but we don’t know which pairs of groups are different.
# It’s possible to perform multiple pairwise-comparison, 
# to determine if the mean difference between specific pairs 
# of group are statistically significant, e.g. with Tukey multiple pairwise-comparisons
# As the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) for performing multiple pairwise-comparison between the means of groups.
# lets explore the nature of the differences between the groups. 
# how does TapeS predict in comparisson to all other functions 


turkey.output <- vector("list", length =length(unique(trees_data_al_bet_wag$bot_genus) ) )
for (i in 1:length(unique(trees_data_al_bet_wag$bot_genus) ) ) {
  #i = 2
  my.spec <- as.character(unique(trees_data_al_bet_wag$bot_genus)[i])
  # subset data for levene
  df_for_anova <- unique(trees_data_al_bet_wag[bot_genus == my.spec, ])
  
  # run anova by specie and group 
  res.aov <- aov(B_kg_tree ~ peat, data = df_for_anova)
  turkey <- TukeyHSD(res.aov)
  
  
  # run turkey and safe outout in dataframe 
  turkey.output.df <- tidy(turkey)
  
  # bind results of levene it in dataframe
  turkey.output[[i]] <- as.data.frame(cbind( my.spec
                                             , turkey.output.df))
  
  
  #control print
  print(c(i, my.spec))
  print(TukeyHSD(res.aov))
  # compute letters to show significance
  print(multcompView::multcompLetters4(res.aov, turkey))
  
}
# save output to dataframe
turkey_out_al_bet <- as.data.frame(rbindlist(turkey.output))

# interpret results
# It can be seen from the output, that only the difference  with an adjusted p-value < 0.05 are significant


#filter for tapes comparissons: 
# filter for tapes comparissons that show a significant difference
# turkey_out_al_bet_tapes <- setDT(turkey_out_al_bet)[str_detect(turkey_out_al_bet$contrast, "tapes"), ]
turkey_out_al_bet[turkey_out_al_bet$adj.p.value >= 0.05, ]
# filter for tapes comparissons that dont show a significant difference 
setDT(turkey_out_al_bet)[turkey_out_al_bet$adj.p.value < 0.05, ]

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# for alnus: for alnus peat functions are different to no and partly peat functions 
#            but no significant difference between partly peat and no peat
# for betula there are no signficicant differences at all. not between partly and no peat, peat and no peat, peat and partly peat


stop("this is where biomass comparison singe tree script paper stops")






# 4. visuals --------------------------------------------------------------
# 4.1. ALNUS visuals --------------------------------------------------------------
# 4.1.1. ALNUS ag visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
alnus_ag <-  trees_data_update_5[compartiment %in% c("agb", "ag") & bot_genus %in% c("Alnus") & min_org == "org", ]

alnus_ag_labels <- alnus_ag %>% group_by(paper_ID, func_ID, ID) %>% dplyr::summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  left_join(., ungroup(bio_func_df %>% filter(str_detect(species, "Alnus")) %>% select(paper_ID, country)) %>% distinct()#%>% mutate_at("paper_ID", ~as.character(.))
            , by = "paper_ID" ) %>% 
  mutate(country_code = ifelse(func_ID == "tapes", "GER", countrycode(country, origin = 'country.name', destination = 'iso3c')),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name = paste0(ID, ", ",country_code))

ggplot(data = ungroup(alnus_ag) # %>% filter(!(ID %in% c("13_1"))) # "16_4" and "16_5" are somehow weird so i kicked it out 
)+ 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, color = "self_fit"), col = "black")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name, color = label_name), 
            data = alnus_ag_labels # %>% filter(!(ID %in% c("13_1")))
  )+
  theme_bw()+
  #theme(legend.position="none")+
  ggtitle("Alnus Biomass kg/tree by diameter cm")



# 4.1.2. ALNUS wabg visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
# trees_data_update_5 <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_update_6_1.csv"), sep = ",", dec = ".")
# 4.1.2.1. ggplot ---------------------------------------------------------
alnus_wag <-  trees_data_update_5[trees_data_update_5$compartiment %in% c("w_agb") &
                                    trees_data_update_5$bot_genus %in% c("Alnus") & 
                                    trees_data_update_5$min_org == "org" &
                                    trees_data_update_5$paper_ID != 9
                                  , ]

alnus_wag_labels <- alnus_wag %>% group_by(paper_ID, func_ID, ID, country, peat) %>% 
  dplyr::summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c'),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name =  paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code)) %>% 
  distinct()

alnus_wag <- 
  alnus_wag %>% 
  mutate(
    farbe = ifelse(ID %like% c("tapes") , "red" , # tapes red
                   ifelse(peat == "yes",  "blue" , # "#53868B",
                          ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                 "grey" ) )))
color_map <- setNames(alnus_wag$farbe, alnus_wag$ID)


# sd mean betula 
m_b_al <- alnus_wag[alnus_wag$ID != "9_w_agb", .(B_kg_tree=mean(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]
m_b_al <- alnus_wag[alnus_wag$ID != "9_w_agb", .(B_kg_tree=mean(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_al <- alnus_wag[alnus_wag$ID != "9_w_agb", .(sd_B_kg_tree =sd(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]  
m_sd_b_al <- m_b_al[sd_b_al, on = list(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_al[, up_sd_B_kg_tree := (B_kg_tree + sd_B_kg_tree)]
m_sd_b_al[, low_sd_B_kg_tree := (B_kg_tree - sd_B_kg_tree)]  



# plot 
ggplot( )+ 
  geom_point(data = ungroup(alnus_wag)  %>% filter(!(ID %in% c("9_w_agb"))) 
             , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID), 
  )+
  geom_smooth(data = ungroup(alnus_wag) %>% filter(!(ID %in% c("9_w_agb")))
              , method= "loess"
              , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID) 
              , se = F 
  )+
  scale_color_manual(values = color_map)+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree), 
              method= "loess",
              data = m_sd_b_al,
              col = "black")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_al$low_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_al, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_al$up_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_al, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name), 
            data = alnus_wag_labels
            %>% filter(!(ID %in% c("9_w_agb")))
  )+
  ylim(0, 2700)+
  xlim(0, 64)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Alnus woody aboveground biomass kg/tree by diameter cm")



# 4.1.2.2. base r ---------------------------------------------------------
## plot alnus base r

# Change the margins of the plot (the fourth is the right margin)
par(mar = c(4, 4, 2, 10), xpd=TRUE)

grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 1)

# mark only tapes plot
my.colors <- ifelse(levels(as.factor(alnus_wag$ID)) %like% "tapes" , "red" , # tapes red
                    ifelse(levels(as.factor(alnus_wag$ID)) %in% c(bio_func_df$ID[bio_func_df$peat == "yes"]), "grey10",
                           ifelse(levels(as.factor(alnus_wag$ID)) %in% c(bio_func_df$ID[bio_func_df$peat == "partly"]), "grey30",
                                  "grey" ) )) # full peat dark grey

# plot 
plot(alnus_wag$DBH_cm[alnus_wag$ID != "2_w_agb"], alnus_wag$B_kg_tree[alnus_wag$ID != "2_w_agb"], 
     frame = T, 
     pch = 19, 
     cex = 0.5, 
     col = factor(alnus_wag$ID[alnus_wag$ID != "2_w_agb"]), 
     xlab = "DBH cm",
     ylab = "Biomass kg tree-1", 
     main = "Alnus spp. biomass kg tree-1 by diameter")

# move legend to side: https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
legend("topright", inset=c(-0.2,0), legend= alnus_wag_labels$label_name, 
       col = factor(alnus_wag$ID), pch=19, title="Paper ID and country")

on.exit(par(opar))







# 4.2. BETULA visuals --------------------------------------------------------------
# 4.2.1. BETULA ag visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
betula_ag <- 
  rbind(setDT(betula_agb_kg_tree_df[betula_agb_kg_tree_df$compartiment == "agb", ]),
        setDT((tapes_tree_data[
          tapes_tree_data$compartiment == "ag" & 
            tapes_tree_data$bot_genus %in% c("Betula") & 
            tapes_tree_data$min_org == "org",]) %>% 
            mutate(paper_ID = "tapes", 
                   func_ID = "tapes", 
                   country = "Germany")), fill = T ) %>% 
  unite( "ID", paper_ID, func_ID, remove = F) %>% distinct()

betula_ag_labels <- betula_ag %>% group_by(paper_ID, func_ID, ID) %>% dplyr::summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree))%>% #mutate_at("paper_ID", ~as.integer(.)) %>% 
  left_join(., ungroup(bio_func_df %>% filter(str_detect(species, "Betula")) %>% select(paper_ID, country)) %>% distinct() %>% mutate_at("paper_ID", ~as.character(.)), by = "paper_ID" ) %>% 
  mutate(country_code = ifelse(func_ID == "tapes", "GER", countrycode(country, origin = 'country.name', destination = 'iso3c') ),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name = paste0(ID, ", ",country_code)) %>% 
  distinct()


# plot
ggplot(data = ungroup(betula_ag) # %>% filter(!(ID %in% c("13_1"))) # "16_4" and "16_5" are somehow weird so i kicked it out 
)+ 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, color = "self_fit"), col = "black")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name, color = label_name), 
            data = betula_ag_labels # %>% filter(!(ID %in% c("13_1")))
  )+
  theme_bw()+
  #theme(legend.position="none")+
  ggtitle("Betula Biomass kg/tree by diameter cm")


# 4.1.2. BETULA wabg visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
betula_wag <-  trees_data_update_5[compartiment %in% c("w_agb") & bot_genus %in% c("Betula") & min_org == "org", ]

betula_wag_labels <- betula_wag %>% group_by(paper_ID, func_ID, peat, country, ID) %>% dplyr::summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c'),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name =  paste0(paper_ID, ", ", ifelse(func_ID != "w_agb", paste0(func_ID, ", ") , ""), country_code)) %>% 
  distinct()

betula_wag <- 
  betula_wag %>% 
  mutate(
    farbe = ifelse(ID %like% c("tapes") , "red" , # tapes red
                   ifelse(peat == "yes",  "blue" , # "#53868B",
                          ifelse(peat == "partly", "turquoise1", # "#7AC5CD",
                                 "grey" ) )))

# betula_wag <- 
#   betula_wag %>% 
#   mutate(
#     farbe = ifelse(ID %like% c("tapes") , "red" , # tapes red
#                    ifelse(peat == "yes", "#53868B",
#                           ifelse(peat == "partly", "#7AC5CD",
#                                  "grey" ) ))
#   )
color_map <- setNames(betula_wag$farbe, betula_wag$ID)

# sd mean betula 
m_b_be <- betula_wag[ , .(B_kg_tree=mean(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]
sd_b_be <- betula_wag[ , .(sd_B_kg_tree =sd(B_kg_tree)),.(plot_ID, tree_ID, compartiment, DBH_cm)]  
m_sd_b_be <- m_b_be[sd_b_be, on = list(plot_ID, tree_ID, compartiment, DBH_cm)]
m_sd_b_be[, up_sd_B_kg_tree := (B_kg_tree + sd_B_kg_tree)]
m_sd_b_be[, low_sd_B_kg_tree := (B_kg_tree - sd_B_kg_tree)]  


# plot 
ggplot( )+ 
  geom_point(data = ungroup(betula_wag) # %>% filter(!(ID %in% c("2_w_agb"))), 
             , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID), 
  )+
  geom_smooth(data = ungroup(betula_wag) #%>% filter(!(ID %in% c("2_w_agb")))
              , method= "loess"
              , aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID) 
              , se = F 
  )+
  scale_color_manual(values = color_map)+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree), 
              method= "loess",
              data = m_sd_b_be,
              col = "black")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_be$low_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_be, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  geom_smooth(aes(x = DBH_cm, y = m_sd_b_be$up_sd_B_kg_tree), 
              method= "loess",
              data = m_sd_b_be, 
              col = "black", 
              se = F , 
              linetype = "dashed")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name), 
            data = betula_wag_labels
            %>% filter(!(ID %in% c("2_w_agb")))
  )+
  ylim(0, 2700)+
  xlim(0, 64)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Betula woody aboveground biomass kg/tree by diameter cm")


## base r
par(mar = c(4, 4, 2, 10), xpd=TRUE)

# grid(nx = NULL, ny = NULL,
#      lty = 2, col = "gray", lwd = 1)

plot(betula_wag$DBH_cm, betula_wag$B_kg_tree, 
     frame = T, 
     pch = 19,
     cex = 0.5,
     col = factor(betula_wag$ID), 
     xlab = "DBH cm",
     ylab = "Biomass kg tree-1", 
     main = "Betula spp. biomass kg tree-1 by diameter")

# move legend to side: https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
legend("topright", inset=c(-0.3,0), legend= betula_wag_labels$label_name, 
       col = factor(betula_wag$ID), pch=19, title="Paper ID and country")








# 4.3. Betula & Alnus together --------------------------------------------
bet_aln_ag <- dplyr::bind_rows(alnus_ag, betula_ag)
ggplot(data = bet_aln_ag %>% filter(!(ID %in% c("16_4", "16_5")))
)+ # "16_4" and "16_5" are somehow weird so i kicked it out 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = as.factor(ID)  ))+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = as.factor(ID)  ))+
  theme_bw()+
  facet_wrap(~bot_genus)+
  ggtitle("Biomass kg/tree by diameter cm by species")




# 4.4. BA verteilung over all organic plots -------------------------------
install.packages("forcats")
library(forcats)

BA_distri <- LT_summary %>% filter(SP_code != "all") %>% select(plot_ID, SP_code, BA_m2_ha) %>% mutate(plot_ID = as.integer(plot_ID)) %>% 
  left_join(., soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2")) %>% 
  filter(min_org == "org") %>% distinct() %>% 
  group_by(SP_code, BA_m2_ha, min_org) %>% 
  dplyr::summarise(BA_m2_ha = sum(BA_m2_ha))

ggplot(data = BA_distri %>% arrange(BA_m2_ha), aes(x=fct_infreq(SP_code), y= BA_m2_ha, fill = SP_code)) + 
  geom_bar( stat = "identity")+
  theme_bw()+
  ggtitle("Basal area in m2 per ha by tree species")


(group)












# NOTES -------------------------------------------------------------------
# n. old way to filter glutinosa func -------------------------------------

subset(bio_func_df, species %like% "Alnus" &     # select only Alnus specific species
         !is.na(function.) &                    # select only those papers with functions
         compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb"))            # select only those paper which have leafes not icluded or a possible compartimentalisation
# n. old way to filter betula func -------------------------------------
betula_func <- subset(bio_func_df, species %like% "Betula" &     # select only Alnus specific species
                        !is.na(function.) &                    # select only those papers with functions
                        compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb"))            # select only those paper which have leafes not icluded or a possible compartimentalisation






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






# 3d plot including height: https://stackoverflow.com/questions/45052188/how-to-plot-3d-scatter-diagram-using-ggplot
install.packages("plotly")
library(plotly)

plot_ly(x=alnus_ag$DBH_cm, y= alnus_ag$H_m, z = alnus_ag$B_kg_tree, 
        type="scatter3d", 
        color=alnus_ag$ID, 
        size = 0.5)  
# %>% add_trace(x=alnus_ag$DBH_cm, y= alnus_ag$H_m, z = alnus_ag$B_kg_tree,color=alnus_ag$ID, 
#                 type="scatter3d", mode="lines",
#                 line = list(width=8),
#                 opacity = 1) 

library(splines)
fit <- lm(cbind(alnus_ag$DBH_cm, alnus_ag$H_m) ~ ns(alnus_ag$B_kg_tree, df = length(unique(alnus_ag$ID))))

#The fitted values will be returned in a two-column matrix by predict(fit). To plot the result, you can use rgl:
install.packages("rgl")
library(rgl)

lines3d(cbind(predict(fit), z = alnus_ag$B_kg_tree))

plot(plot3d(alnus_ag$DBH_cm, alnus_ag$H_m, alnus_ag$B_kg_tree, colors = alnus_ag$ID))

# %>% 
#   add_trace(x=alnus_ag$DBH_cm, y= alnus_ag$H_m, z = alnus_ag$B_kg_tree, 
#             color = alnus_ag$ID,
#             type = "scatter3d",
#             mode = "lines", 
#             trendline = "ols")

# 3d plot including height: https://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

colors <- colors[as.factor(alnus_ag$ID)]
rainbow(length(unique(alnus_ag$ID)))[as.factor(alnus_ag$ID)]

scatterplot3d(alnus_ag[,c("DBH_cm", "H_m", "B_kg_tree")],
              main="3D Scatter Plot",
              xlab = "DBH (cm)",
              ylab = "Height (m)",
              zlab = "Biomass (kg tree-1)", 
              color =  rainbow(length(unique(alnus_ag$ID)))[as.factor(alnus_ag$ID)])
demo("regression")








write.csv(bio_func_df, paste0(out.path, "bio_func_df.csv"), row.names = FALSE)



ln(B_g) =  ln(a) + b*ln(DBH_cm) + c*(ln(    ( (    (H_m* (1 + a* (50/age)^c -1 )  ) / ( 1-b*H_m*((50/age)^c -1) )     ) / (  1+(a+b* ( (H_m* (1 + a* (50/age)^c -1 )  ) / ( 1-b*H_m*((50/age)^c -1) ) )   )*(0.5^c-1 ) )      )) )+ d*ln(age)


# leafes or no leafes 
bio_func_df %>% mutate(A_B = case_when(str_detect(species, "Alnus") ~ "Alnus",
                                       str_detect(species, "Betula") ~ "Betula",
                                       TRUE ~ NA)) %>% 
  select(paper_ID, A_B, leafes_inkl) %>% 
  distinct() %>% group_by(leafes_inkl, A_B) %>% 
  dplyr::summarise(n = n())

bio_func_df %>% mutate(A_B = case_when(str_detect(species, "Alnus") ~ "Alnus",
                                       str_detect(species, "Betula") ~ "Betula",
                                       TRUE ~ NA)) %>% 
  select(A_B, leafes_inkl, country) %>% 
  filter(leafes_inkl %in% c("possible", "not included")) %>% 
  distinct() %>% 
  arrange(A_B) %>% ungroup()
