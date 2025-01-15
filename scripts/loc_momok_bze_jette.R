##################################################
## sf::project: paper biomass functions
## Script purpose: turning coord into gpkg
## Date:2024/11/08
## Author: Marius Moeller
##################################################
library(sf)
library(data.table)
library(terra)
library(mapview)
##################################################
# Section: HBI ----
##################################################
setwd("~/R/data/tables/paper_biomass_jette")
getwd()
hbi <- fread("soils_types_profil_db.csv", sep = ",", header = T)

# rename column bfhnr_2
setnames(hbi, "bfhnr_2", "bfhnr")

# check the column min_org
(unique(hbi$min_org))

# subsetting only mineral soils 
hbi_min <- subset(hbi, min_org == "min")
(unique(hbi_min$min_org))

# subsetting only organic soils
hbi_org <- subset(hbi, min_org == "org")
(unique(hbi_org$min_org))

# load wze gpkg
wze_gpkg <-  terra::vect("/home/data/BZE_datacube/scripts/wzepunkt_p_GK3_with_info.gpkg")
plot(wze_gpkg)

# turning vector into df
wze_df <- as.data.frame(wze_gpkg)

# merge wze_df with hbi subsets 
hbi_org_coord <- merge(wze_df, hbi_org, by = "bfhnr")     # organic
hbi_min_coord <- merge(wze_df, hbi_min, by = "bfhnr")     # mineral

# turning subset dfs into sf-objects
# organic
hbi_org_sf <- st_as_sf(hbi_org_coord,
                       coords = c("Easting_LAEA", "Northing_LAEA"),
                       crs = 3035)

# mineral
hbi_min_sf <- st_as_sf(hbi_min_coord,
                       coords = c("Easting_LAEA", "Northing_LAEA"),
                       crs = 3035)

# plotting
# organic
plot(hbi_org_sf$geometry)
mapview(hbi_org_sf$geometry)

# mineral
plot(hbi_min_sf$geometry)
mapview(hbi_min_sf$geometry)

# combination of org and min in mapview
mapview(hbi_org_sf$geometry, cex = 5)+
  mapview(hbi_min_sf$geometry, cex =2)

# export as gpkg
st_write(hbi_org_sf, "~/R/data/output/hbi_org.gpkg")
st_write(hbi_min_sf, "~/R/data/output/hbi_min.gpkg")

# ##################################################
# # Section: MOMOK???? ----
# ##################################################
# 
# loc_momok <- subset(loc, inv =="momok")
# 
# # Entferne 21020 rw= ??? lw=???
# loc_momok <- loc_momok[loc_momok$bfhnr != 21020, ]
# 
# # turning df in spatvector
# loc_momok_vect <- st_as_sf(loc_momok, coords = c("rw_med", "hw_med"), crs = 4326) # crs WGS84
# crs(loc_momok_vect)
# plot(loc_momok_vect)
# 
# # Transformiere die Koordinaten nach LAEA (EPSG:3035)
# 
# loc_momok_vect_3035 <- st_transform(loc_momok_vect, "EPSG:3035")
# crs(loc_momok_vect_3035)
# plot(loc_momok_vect_3035)
# 
# # Exportiere den SpatVector als Shapefile
# writeVector(loc_momok_vect_3035, "~/R/data/output/loc_momok.shp", overwrite = T)
# 
# 
# # Subsection: plotting ----
# ##################################################
# setwd("~/QGIS/data_input/gadm41_DEU_0_3035")
# 
# deu <- vect("gadm41_DEU_0_3035.shp")
# 
# crs(deu)
# 
# # Plot für das Hintergrundobjekt (deu)
# plot(deu, main = "Plot von deu, loc_momok_vect und loc_hbi_vect")
# 
# # Erzeuge den Plot für den ersten SpatVector in Blau
# plot(loc_momok_vect_3035, col = "blue", add = TRUE)
# 
# # Füge den zweiten SpatVector in Rot hinzu
# plot(loc_hbi_vect_3035, col = "red", add = TRUE)
