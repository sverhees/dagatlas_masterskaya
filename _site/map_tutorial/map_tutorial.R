

# Preparation -------------------------------------------------------------

# working directory

getwd()
setwd("/home/samira/Git/dagatlas_masterskaya/map_tutorial")

# packages

library(tidyverse)
library(lingtypology)

# loading data (tab-separated csv)

villages <- read_csv("villages.csv")
meta <- read_tsv("meta.csv")

prohibitives <- read_tsv("prohibitives.csv")
comitatives <- read_tsv("comitative.csv")

prohibitives <- readxl::read_xlsx("prohibitives.xlsx")

# merge village, language, and feature data

# remove villages for which we do not have coordinates (yet)

villages <- villages[complete.cases(villages$lat),]

# remove idioms not (yet) recognized as distinct

meta_core <- meta %>% 
  filter(core == "yes")

# merge villages and coordinates with language metadata

vill_meta <- merge(villages, meta_core, by = "lang") 

# filter core values from feature dataset

prohib_core <- prohibitives %>%
  filter(core == "yes")

# merge villages, coordinates, and language metadata with feature information

prohib_vill <- merge(vill_meta, prohib_core, by = "lang") 



comit_core <- comitatives %>%
  filter(core == "yes")

# merge villages, coordinates, and language metadata with feature information

comit_vill <- merge(vill_meta, comit_core, by = "lang")



# Simple map --------------------------------------------------------------

# give me the language for this glottocode

lang.gltc("bagv1239")

# show me which languages are in the prohibitives dataset

languages <- unique(prohib_vill$glottocode)

# create a dataframe for a general map (= one language, one datapoint)

general_map <- prohib_vill %>%
  filter(!duplicated(prohib_vill$glottocode))

# draw map!

map.feature(lang.gltc(general_map$glottocode), # use glottocodes as reference for the languages to plot
            features = general_map$value_encoding_type)

map.feature(lang.gltc(general_map$glottocode),
            features = general_map$value_morpheme_type)

# now let's do comitatives

general_map2 <- comit_vill %>%
  filter(!duplicated(comit_vill$glottocode))  

map.feature(lang.gltc(general_map2$glottocode),
            features = general_map2$value_for_map_1,
            color = c("white", "black", "yellow", "blue",
                      "green", "red", "pink", "purple"))

# Villages map ------------------------------------------------------------

# order language names in the legend

prohib_vill$lang <- factor(prohib_vill$lang, levels =c(
  "Dargwa", "Lak", "Bats", "Ingush", "Chechen", "Khinalug", 
  "Archi", "Tsakhur", "Rutul", "Kryz", "Budukh", "Udi", "Lezgian", 
  "Agul", "Tabasaran", "Avar", "Andi", "Botlikh", "Godoberi", 
  "Chamalal", "Bagvalal", "Tindi", "Karata", "Akhvakh", "Tsez", 
  "Hinuq", "Bezhta", "Hunzib", "Khwarshi", "Nogai", "Kumyk", 
  "Azerbaijani", "Armenian"))


map.feature(lang.gltc(prohib_vill),
            latitude = prohib_vill$lat, # use village coordinates
            longitude = prohib_vill$lon,
            features = prohib_vill$lang,
            color = prohib_vill$lang_color,
            stroke.features = prohib_vill$value_encoding_type,
            stroke.color = c("black", "white", "pink"),
            popup = prohib_vill$family.x,
            tile = c("Esri.WorldTopoMap"),
            zoom.control = TRUE)









