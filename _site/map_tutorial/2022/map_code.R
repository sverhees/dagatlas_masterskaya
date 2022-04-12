
# Data preparation -------------------------------------------------------------

# packages

library(tidyverse)
library(lingtypology)

# lingtypology tutorial for customization: https://ropensci.github.io/lingtypology/lingtypology_creating_maps.html

# loading data (tab-separated csv)

#feature data
case_systems <- read_tsv("case_systems.tsv")

#coordinates
villages <- read_tsv("tald_villages.tsv")
genlang <- read_tsv("genlangpoints.csv")

# or load in .xlsx
#df <- readxl::read_xlsx("data.xlsx")

# remove data not for mapping
case_systems <- case_systems[(case_systems$map == "yes"),]

# split feature data into dialect levels
# this returns a list of dataframes (ordered alphabetically :(), which you then split

case_systems_group <- case_systems %>%
  group_by(type) %>%
  group_split()

case_systems_tl <- data.frame(case_systems_group[[1]])
case_systems_l <- data.frame(case_systems_group[[2]])
case_systems_v <- data.frame(case_systems_group[[3]])

# assign granularity level for each set

case_systems_tl$granularity <- "toplevel dialect"
case_systems_v$granularity <- "village dialect"
case_systems_l$granularity <- "language"

# merge feature data with village coordinates

## create matching columns

colnames(case_systems_tl)[colnames(case_systems_tl) == "idiom"] <- "dialect_toplevel"
colnames(case_systems_v)[colnames(case_systems_v) == "idiom"] <- "village_dialect"
colnames(case_systems_l)[colnames(case_systems_l) == "idiom"] <- "standard"

## merge villages and data per level

tlevel_villages <- merge(villages, case_systems_tl, by = "dialect_toplevel")
v_villages <- merge(villages, case_systems_v, by = "village_dialect")
lang_villages <- merge(villages, case_systems_l, by = "standard")

## merge the different levels with villages (in order high granularity (village) < low granularity (dialect_toplevel))

dialects_villages <- bind_rows(v_villages, tlevel_villages, lang_villages)
dialects_villages_clean <- dialects_villages[!duplicated(dialects_villages$village),]

### isolate general language data

case_systems_gl <- case_systems %>%
  filter(genlang_point == "yes") %>%
  mutate(granularity = "language") %>%
  mutate(default_level = lang) %>%
  select(-idiom)

### merge feature data and village set

glang_villages <- merge(villages, case_systems_gl, by = "default_level")

## merge everything

alldata <- bind_rows(dialects_villages_clean, glang_villages)
alldata_clean <- alldata[!duplicated(alldata$village),]

# prepare data for general language point maps
# split coordinate dataframe into Dargwa vs non-Dargwa

gen_dargwa <- genlang[(genlang$aff == "Dargwa"),]
gen_dargwa$idiom <- gen_dargwa$lang
gen_nodargwa <- genlang[!(genlang$aff == "Dargwa"),]

# filter general language points from feature dataset

case_systems_genlang <- case_systems %>%
  filter(genlang_point == "yes")

case_dargwa <- case_systems_genlang[(case_systems_genlang$lang == "Dargwa"),]
case_nodargwa <- case_systems_genlang[!(case_systems_genlang$lang == "Dargwa"),]

# check if each language has max 1 genlang point (besides Dargwa)

#table(duplicated(case_nodargwa$lang))

# merge feature with general coordinates

gencase <- merge(case_nodargwa, gen_nodargwa, by = "lang")
gencase_dargwa <- merge(case_dargwa, gen_dargwa, by = "idiom")

# drop, rearrange and rename some columns from Dargwa frame (this can probably be done with one string of pipes)

gencase_dargwa <- gencase_dargwa[!names(gencase_dargwa) %in% c("lang.y")]

colnames(gencase_dargwa)[5] <- "lang"

gencase_dargwa <- gencase_dargwa %>%
  relocate(idiom, .after = lang)

gencase <- gencase %>%
  relocate(lang, .before = group)

all_genpoints <- rbind(gencase, gencase_dargwa)


# Villages, languages and feature -----------------------------------------

map.feature(lang.gltc(alldata_clean$gltc_lang),
            latitude = alldata_clean$lat, 
            longitude = alldata_clean$lon,
            features = alldata_clean$default_level,
            color = alldata_clean$lang_col,
            legend = F,
            label = alldata_clean$lang,
            stroke.features = as.factor(alldata_clean$value2),
            stroke.color = "magma",
            stroke.title = unique(alldata_clean$value2_name),
            zoom.control = TRUE,
            popup = paste(alldata_clean$village, "|",
                          alldata_clean$rus_village, "<br>",
                          "data:", alldata_clean$granularity))


# Villages and feature ----------------------------------------------------

map.feature(lang.gltc(alldata_clean$gltc_lang),
            latitude = alldata_clean$lat,
            longitude = alldata_clean$lon,
            features = as.factor(alldata_clean$value2),
            color = "magma",
            label = alldata_clean$lang,
            title = unique(alldata_clean$value2_name),
            zoom.control = TRUE,
            popup = paste(alldata_clean$village, "|",
                          alldata_clean$rus_village, "<br>",
                          "data:", alldata_clean$granularity))

# Data granularity --------------------------------------------------------

map.feature(lang.gltc(alldata_clean$gltc_lang),
            latitude = alldata_clean$lat,
            longitude = alldata_clean$lon,
            features = as.factor(alldata_clean$value2),
            title = unique(alldata_clean$value2_name),
            label = alldata_clean$lang.x,
            color = "magma",
            popup = paste(alldata_clean$village, "|", alldata_clean$rus_village, "<br>",
                          "data:", alldata_clean$granularity),
            control = alldata_clean$granularity,
            zoom.control = T)

# General datapoints ------------------------------------------------------

map.feature(lang.gltc(all_genpoints$gltc),
            latitude = all_genpoints$lat,
            longitude = all_genpoints$lon,
            features = all_genpoints$lang,
            color = all_genpoints$lang_col,
            legend = F,
            label = all_genpoints$lang,
            stroke.features = all_genpoints$value2, 
            stroke.color = "magma", 
            stroke.title = unique(all_genpoints$value2_name),
            zoom.control = TRUE)

# General datapoints (feature only) ---------------------------------------

map.feature(lang.gltc(all_genpoints$gltc),
            latitude = all_genpoints$lat,
            longitude = all_genpoints$lon,
            features = all_genpoints$value3,
            color = c("pink", "green"),
            title = unique(all_genpoints$value3_name),
            label = all_genpoints$lang,
            zoom.control = T)
