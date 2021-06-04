

# Preparation -------------------------------------------------------------

# packages

library(tidyverse)
library(lingtypology)

# lingtypology tutorial for customization: https://ropensci.github.io/lingtypology/lingtypology_creating_maps.html

# loading data (tab-separated csv)

villages <- read_tsv("tald_villages.csv")

# convert data tables to .csv and load them

agreementslots <- read_tsv("agreementslots.csv")
causatives <- read_tsv("causatives.csv")
spatialcasesystems <- read_tsv("spatialcasesystems.csv")

# or load in .xlsx
#df <- readxl::read_xlsx("data.xlsx")


# Agreement slots ---------------------------------------------------------

# filter genlang_points from feature dataset

agreement_genlang <- agreementslots %>%
  filter(genlang_point == "yes")

# ...



# Causatives --------------------------------------------------------------

# filter genlang_points from feature dataset

causatives_genlang <- causatives %>%
  filter(genlang_point == "yes")

# ...


# Spatial case systems ----------------------------------------------------

# filter genlang_points from feature dataset

spatialcase_genlang <- spatialcasesystems %>%
  filter(genlang_point == "yes")

# merge villages, coordinates, and language metadata with feature information

spatialcase_vill <- merge(villages, spatialcase_genlang, by = "lang") 

# как-то непонятно пропадают языки из столбца lang
# + in the original feature table, Dargwa dialects were marked 
# as general language points > Dargwa can have only 1: in the csv it's Akusha


# create a simple map 

spatialcase_simple <- spatialcase_vill %>%
  filter(!duplicated(default_level))

map.feature(lang.gltc(spatialcase_simple$gltc_lang),
            features = spatialcase_simple$value,
            color = "magma")

# create a language-villages + feature map

map.feature(lang.gltc(spatialcase_vill$gltc_lang),
            latitude = spatialcase_vill$lat, # custom coordinates
            longitude = spatialcase_vill$lon, # custom coordinates
            features = spatialcase_vill$default_level, # inner dot feature
            color = spatialcase_vill$lang_col, # inner dot color > standard palette
            legend = F,
            stroke.features = spatialcase_vill$value, # outer dot feature
            stroke.color = "magma", # outer dot color
            stroke.title = "Spatial case systems",
            tile = c("Esri.WorldTopoMap"), # map style
            zoom.control = TRUE)

# create a feature-villages map

map.feature(lang.gltc(spatialcase_vill$gltc_lang),
            latitude = spatialcase_vill$lat,
            longitude = spatialcase_vill$lon,
            features = spatialcase_vill$value,
            color = "magma",
            title = "Spatial case systems",
            tile = c("Esri.WorldTopoMap"),
            zoom.control = TRUE)

# create a general datapoint map

map.feature(lang.gltc(spatialcase_simple$gltc_lang),
            latitude = spatialcase_simple$lat, # custom coordinates
            longitude = spatialcase_simple$lon, # custom coordinates
            features = spatialcase_simple$default_level, # inner dot feature
            color = spatialcase_simple$lang_col, # inner dot color > standard palette
            legend = F,
            stroke.features = spatialcase_simple$value, # outer dot feature
            stroke.color = "magma", # outer dot color
            stroke.title = "Spatial case systems",
            tile = c("Esri.WorldTopoMap"), # map style
            zoom.control = TRUE)
