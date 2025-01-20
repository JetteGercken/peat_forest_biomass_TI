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
trees_data <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_update_3.csv"), sep = ",", dec = ".")
tapes_tree_data <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_update_4.csv"), sep = ",", dec = ".")
trees_removed <- read.delim(file =paste0(getwd(), out.path, trees_data$inv[1], "_LT_removed.csv"), sep = ",", dec = ".")
# soil data
soil_types_db <- read.delim(file = paste0(getwd(), out.path, "soils_types_profil_db.csv"), sep = ",", dec = ".")
# importa data from literature research
bio_func_df <- read.delim(file = paste0(getwd(), "/data/input/", "B_lit_functions.csv"), sep = ",", dec = ".") %>% select(-c(func_ID, paper_ID))
# summaries
all_summary <- read.delim(file = paste0(getwd(), out.path, "HBI_LT_RG_DW_stocks_ha_all_groups.csv"), sep = ",", dec = ".")
LT_summary <- all_summary %>% filter(stand_component == "LT") %>% select(-c(dw_sp, dw_type, decay, inv_year, ST_LY_type, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m, n_dec, n_dw_TY))


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
alnus_func <- subset(bio_func_df, species %like% "Alnus" &     # select only Alnus specific species
                            !is.na(function.) &                    # select only those papers with functions
                            compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb", "abg"))            # select only those paper which have leafes not icluded or a possible compartimentalisation

# select alnus trees at organic sites
tree_data_alnus <- trees_data[trees_data$bot_genus %in% c("Alnus") & trees_data$min_org == "org",]  
alnus_agb_kg_tree <- vector("list", length = nrow(tree_data_alnus))
for (i in 1:nrow(alnus_func)){
 # i = 1
  
  paper_id <- alnus_func$paper_ID[i]# ID of the paper in literature research csv
  func_id <- alnus_func$func_ID[i]  # ID of the function in literature research csv
  id <- alnus_func$ID[i]            # combination of func and paper id 
  func <- alnus_func$function.[i]   # biomass function taken from respective reference 
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
  
  tree.df <- as.data.frame(cbind(tree_data_alnus, "B_kg_tree" = c(bio_tree), "paper_ID" = c(paper_id), "func_ID" = c(func_id), "ID" = c(id), "unit_B" = c(unit), "logarithm_B" = c(ln_stat), "compartiment" = c(comp))) # 
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
  # agb calculate from compartiments based on papers that don´t have seperate agb function but also  allow to exclude leafes: so they have eg. fwb, ndl and sw but no agb compartiment in their list 
  setDT(tree_data_alnus %>% 
       left_join(., setDT(alnus_agb_kg_tree_df)[  # this is an anti join in data.tabe 
       setDT(bio_func_df %>% 
          filter(leafes_inkl %in% c("possible", "inlcuded")) %>%                 # agb shoul be included or possible to include
          select(paper_ID, compartiment) %>%                                     # select papaer ID and compartiments 
          distinct() %>%                                                         # make sure we only select them once
          mutate(number = str_count(compartiment, "agb")) %>%                    # count the occurence of "agb" per paper and compartiment
          group_by(paper_ID) %>% summarise(mean_agb_number = mean(number)) %>%   # summarise the number of occurences of "agb" per paper 
          filter(mean_agb_number == 0)),                                          # filter those papers that allow to calculate the "true" agb with leaves but dont have a agb function and by that have to be summed up  
      on = .(paper_ID), nomatch = NULL] %>% # close anti join data.table
                dplyr::group_by(plot_ID, tree_ID, paper_ID, unit_B, logarithm_B) %>%  #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
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
                                               alnus_agb_kg_tree_df$ID %in% c(alnus_func$ID[alnus_func$leafes_inkl == "possible"]), ]) %>%   # of papers which have "possible"
                      dplyr::group_by(plot_ID, tree_ID, paper_ID, unit_B, logarithm_B) %>%              #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
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
betula_func <- subset(bio_func_df, species %like% "Betula" &     # select only Alnus specific species
                        !is.na(function.) &                    # select only those papers with functions
                        compartiment %in% c("ndl", "fwb", "sw", "swb", "stb", "stw", "agb", "abg"))            # select only those paper which have leafes not icluded or a possible compartimentalisation

# 1.2.1. BETULA compartiment biomass calculations -------------------------------------------------
# select alnus trees at organic sites
tree_data_betula <- trees_data[trees_data$bot_genus %in% c("Betula") & trees_data$min_org == "org",]  
betula_agb_kg_tree <- vector("list", length = nrow(tree.df))
for (i in 1:nrow(betula_func)){
  # i = 45
  
  paper_id <- betula_func$paper_ID[i]
  func_id <- betula_func$func_ID[i]  # ID of the function in literature research csv
  func <- betula_func$function.[i]   # biomass function taken from respective reference 
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
  
  tree.df <- as.data.frame(cbind(tree_data_betula, "B_kg_tree" = c(bio_tree), "paper_ID" = c(paper_id), "func_ID" = c(func_id), "unit_B" = c(unit), "logarithm_B" = c(ln_stat), "compartiment" = c(comp))) # 
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
  setDT(betula_agb_kg_tree_df[!(betula_agb_kg_tree_df$compartiment %in% c("abg", "agb")),]),
  # agb including leaf mass from functions that have an explicit function for ag
  setDT(betula_agb_kg_tree_df[betula_agb_kg_tree_df$compartiment %in% c("abg", "agb") & betula_agb_kg_tree_df$ID %in% c(betula_func$ID[betula_func$leafes_inkl %in% c("included", "possible")]) ,]), 
  # agb calculate from compartiments based on papers that don´t have seperate agb function but also  allow to exclude leafes: so they have eg. fwb, ndl and sw but no agb compartiment in their list 
  setDT(tree_data_betula %>% 
          left_join(., setDT(betula_agb_kg_tree_df)[  # this is an anti join in data.tabe 
            setDT(bio_func_df %>% 
                    filter(leafes_inkl %in% c("possible", "inlcuded")) %>%                 # agb shoul be included or possible to include
                    select(paper_ID, compartiment) %>%                                     # select papaer ID and compartiments 
                    distinct() %>%                                                         # make sure we only select them once
                    mutate(number = str_count(compartiment, "agb")) %>%                    # count the occurence of "agb" per paper and compartiment
                    group_by(paper_ID) %>% summarise(mean_agb_number = mean(number)) %>%   # summarise the number of occurences of "agb" per paper 
                    filter(mean_agb_number == 0)),                                          # filter those papers that allow to calculate the "true" agb with leaves but dont have a agb function and by that have to be summed up  
            on = .(paper_ID), nomatch = NULL] %>% # close anti join data.table
              dplyr::group_by(plot_ID, tree_ID, paper_ID, unit_B, logarithm_B) %>%  #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
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
                                               betula_agb_kg_tree_df$ID %in% c(betula_func$ID[betula_func$leafes_inkl == "possible"]), ]) %>%   # of papers which have "possible"
                      dplyr::group_by(plot_ID, tree_ID, paper_ID, unit_B, logarithm_B) %>%              #  group by tree per plot per paper as we ahve to sum up the different compartiments originating from the same paper (and not all available compartiments per tree)
                      dplyr::summarise(B_kg_tree = sum(B_kg_tree)) %>%                                                        # sum up compartiemtns per tree per paper
                      mutate(compartiment = "w_agb",
                             func_ID = "w_agb", 
                             ID = paste0(paper_ID, "_", func_ID)), 
                    by =  c("plot_ID", "tree_ID")) ),
  fill = T) %>% 
  arrange(plot_ID, tree_ID, paper_ID, func_ID)




# 2. tapeS wagb compartiement calculation ---------------------------------------------------------
# 2.1. alnus tapeS wagb compartiement calculation ---------------------------------------------------------
alnus_wagb_tapes <- setDT(tapes_tree_data[, -c("B_kg_tree",   "N_kg_tree",   "C_kg_tree", "compartiment")])[setDT(
  tapes_tree_data [!(tapes_tree_data$compartiment %in% c("ag", "bg", "total", "ndl")) & 
  tapes_tree_data$bot_genus %in% c("Alnus") & 
  tapes_tree_data$min_org == "org",] %>% 
    group_by(plot_ID, tree_ID) %>% 
    summarise(B_kg_tree = sum(B_kg_tree)) %>% 
    mutate(compartiment = "w_agb",
           paper_ID = as.numeric(max(bio_func_df$paper_ID))+1, 
           func_ID = "tapes",
           ID = paste0(paper_ID, "_", func_ID), 
           country = "Germany")
  ), on = .(plot_ID, tree_ID) ]

# 2.2. betula tapeS wagb compartiement calculation ---------------------------------------------------------
betula_wagb_tapes <- setDT(tapes_tree_data[, -c("B_kg_tree",   "N_kg_tree",   "C_kg_tree", "compartiment")])[setDT(
  tapes_tree_data [!(tapes_tree_data$compartiment %in% c("ag", "bg", "total", "ndl")) & 
                     tapes_tree_data$bot_genus %in% c("Betula") & 
                     tapes_tree_data$min_org == "org",] %>% 
    group_by(plot_ID, tree_ID) %>% 
    summarise(B_kg_tree = sum(B_kg_tree)) %>% 
    mutate(compartiment = "w_agb",
           paper_ID = as.numeric(max(bio_func_df$paper_ID))+1, 
           func_ID = "tapes",
           ID = paste0(paper_ID, "_", func_ID),  
           country = "Germany")
), on = .(plot_ID, tree_ID) ]




# 2. visuals --------------------------------------------------------------
# 2.1. ALNUS visuals --------------------------------------------------------------
# 2.1.1. ALNUS ag visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
alnus_ag <-  rbind(setDT(alnus_agb_kg_tree_df),
                   setDT((tapes_tree_data[
                     tapes_tree_data$compartiment == "ag" & 
                       tapes_tree_data$bot_genus %in% c("Alnus") & 
                       tapes_tree_data$min_org == "org",]) %>% 
                       mutate(paper_ID = "tapes", 
                              func_ID = "tapes", 
                              country = "Germany")), fill = T )

alnus_ag_labels <- alnus_ag %>% group_by(paper_ID, func_ID, ID) %>% summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  left_join(., ungroup(bio_func_df %>% filter(str_detect(species, "Alnus")) %>% select(paper_ID, country)) %>% distinct()%>% mutate_at("paper_ID", ~as.character(.)), by = "paper_ID" ) %>% 
  mutate(country_code = toupper(substr(country, start = 1, stop = 2)),
         label_name = paste0(ID, ", ",country_code))

ggplot(data = ungroup(alnus_ag) %>% filter(!(ID %in% c("13_1"))) # "16_4" and "16_5" are somehow weird so i kicked it out 
       )+ 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, color = "self_fit"), col = "black")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
 geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name, color = label_name), 
           data = alnus_ag_labels%>% filter(!(ID %in% c("13_1"))))+
  theme_bw()+
 #theme(legend.position="none")+
  ggtitle("Alnus Biomass kg/tree by diameter cm")



# 2.1.2. ALNUS wabg visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
alnus_wag <-  rbind(setDT(alnus_agb_kg_tree_df[alnus_agb_kg_tree_df$compartiment == "w_agb", ]),
                   setDT(alnus_wagb_tapes), fill = T )

alnus_wag_labels <- alnus_wag %>% group_by(paper_ID, func_ID, ID) %>% summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree)) %>% 
  left_join(., ungroup(bio_func_df %>% filter(str_detect(species, "Alnus")) %>% select(paper_ID, country)) %>% distinct() 
             # mutate_at("paper_ID", ~as.character(.))
            , by = "paper_ID" ) %>% 
  mutate(country_code = toupper(substr(country, start = 1, stop = 2)),
         ID = ifelse(is.na(ID), paste0(paper_ID, "_", func_ID), ID),
         label_name = paste0(ID, ", ",country_code)) %>% 
  distinct()

ggplot(data = ungroup(alnus_wag) %>% filter(!(ID %in% c("4_w_agb"))) # "16_4" and "16_5" are somehow weird so i kicked it out 
)+ 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, color = "self_fit"), col = "black")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name, color = label_name), 
            data = alnus_wag_labels %>% filter(!(ID %in% c("4_w_agb")))
            )+
  theme_bw()+
  #theme(legend.position="none")+
  ggtitle("Alnus Biomass kg/tree by diameter cm")



# 2.2. BETULA visuals --------------------------------------------------------------
# avbovegroun biomass of alnus trees in kg by diameter, without ln functions and those that have multiple compartiments yet 
betula_ag <- 
  rbind(setDT(betula_agb_kg_tree_df),
        setDT((tapes_tree_data[
          tapes_tree_data$compartiment == "ag" & 
            tapes_tree_data$bot_genus %in% c("Betula") & 
            tapes_tree_data$min_org == "org",]) %>% 
            mutate(paper_ID = "tapes", 
                   func_ID = "tapes", 
                   country = "Germany")), fill = T ) %>% 
  unite( "ID", paper_ID, func_ID, remove = F) %>% distinct()

betula_ag_labels <- betula_ag %>% group_by(paper_ID, func_ID, ID) %>% summarise(DBH_cm = max(DBH_cm), B_kg_tree = max(B_kg_tree))%>% #mutate_at("paper_ID", ~as.integer(.)) %>% 
  left_join(., ungroup(bio_func_df %>% filter(str_detect(species, "Betula")) %>% select(paper_ID, country)) %>% distinct() %>% mutate_at("paper_ID", ~as.character(.)), by = "paper_ID" ) %>% 
  mutate(country_code = toupper(substr(country, start = 1, stop = 2)),
         label_name = paste0(ID, ", ",country_code))

ggplot(data = betula_ag %>% filter(!(ID %in% c("34_4", "36_1")))
       )+ # "16_4" and "16_5" are somehow weird so i kicked it out 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, group = ID, color = ID))+
  geom_smooth(method= "loess", aes(x = DBH_cm, y = B_kg_tree, color = "self_fit"), col = "black")+
  # add labels to plot: https://stackoverflow.com/questions/61415263/add-text-labels-to-geom-smooth-mean-lines
  geom_text(aes(x = DBH_cm+2, y = B_kg_tree, label = label_name, color = label_name), 
            data = (ungroup(betula_ag_labels %>% filter(!(ID %in% c("34_4", "36_1")))# "17_1")))#, "38_1", "38_2", "38_3", "38_4", "38_5")))  
                            )))+
  theme_bw()+
  #theme(legend.position="none")+
  ggtitle("Betula Biomass kg/tree by diameter cm")



# 2.3. Betula & Alnus together --------------------------------------------
bet_aln_ag <- dplyr::bind_rows(alnus_ag, betula_ag)
ggplot(data = bet_aln_ag %>% filter(!(ID %in% c("16_4", "16_5")))
)+ # "16_4" and "16_5" are somehow weird so i kicked it out 
  geom_point(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = as.factor(ID)  ))+
  geom_smooth(aes(x = DBH_cm, y = B_kg_tree, group = ID, color = as.factor(ID)  ))+
  theme_bw()+
  facet_wrap(~bot_genus)+
  ggtitle("Biomass kg/tree by diameter cm by species")




# 2.4. BA verteilung over all organic plots -------------------------------
install.packages("forcats")
library(forcats)

BA_distri <- LT_summary %>% filter(SP_code != "all") %>% select(plot_ID, SP_code, BA_m2_ha) %>% mutate(plot_ID = as.integer(plot_ID)) %>% 
  left_join(., soil_types_db %>% select(bfhnr_2, min_org), by = c("plot_ID" = "bfhnr_2")) %>% 
  filter(min_org == "org") %>% distinct() %>% 
  group_by(SP_code, BA_m2_ha, min_org) %>% 
  summarise(BA_m2_ha = sum(BA_m2_ha))

ggplot(data = BA_distri %>% arrange(BA_m2_ha), aes(x=fct_infreq(SP_code), y= BA_m2_ha, fill = SP_code)) + 
  geom_bar( stat = "identity")+
  theme_bw()+
  ggtitle("Basal area in m2 per ha by tree species")


(group)


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
  summarise(n = n())

bio_func_df %>% mutate(A_B = case_when(str_detect(species, "Alnus") ~ "Alnus",
                                       str_detect(species, "Betula") ~ "Betula",
                                       TRUE ~ NA)) %>% 
  select(A_B, leafes_inkl, country) %>% 
  filter(leafes_inkl %in% c("possible", "not included")) %>% 
  distinct() %>% 
  arrange(A_B) %>% ungroup()