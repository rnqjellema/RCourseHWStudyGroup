# clear the workspace ----
rm(list =ls())

# load data ----
load("~/Documents/GitHub/RCourseHWStudyGroup/data/Example_LongFormatHashed.RData")
load("~/Documents/GitHub/RCourseHWStudyGroup/data/Example_WideFormatHashed.RData")

# load packages -----------------
library(fansi)
library(tidyverse)
library(janitor)
library(here)

# clean data -----
cleannames_data_long<-clean_names(Example_LongFormat)
view(cleannames_data_long)

# selecting colums-----
select_data_long <- select(cleannames_data_long,behandeling,rounddescription,everything())
view(select_data_long)
select_data_long <- select(select_data_long,-patient_traject_id)

# pipe %>% -----
data_long_clean <- Example_LongFormat%>%
  clean_names()%>%
  select(behandeling,rounddescription,everything())%>%
  select(-patient_traject_id)
view(data_long_clean)

# summary
summary(Example_LongFormat)

# arrange filter sort
cleannames_data_long %>% arrange(-vas_pijn_gemiddeld_1)

woman_long <- cleannames_data_long %>%
  filter (geslacht == "F") %>%
  arrange(-vas_pijn_gemiddeld_1) %>%
  select(vas_pijn_gemiddeld_1)

men_long <- cleannames_data_long %>%
  filter (geslacht == "M") %>%
  arrange(-vas_pijn_gemiddeld_1) %>%
  select(vas_pijn_gemiddeld_1)

# group by and summary, remove missing data
cleannames_data_long %>%
  group_by(geslacht) %>%
  summarise(max_vaspijn= max(vas_pijn_gemiddeld_1,na.rm=TRUE),
            min_vaspijn= min(vas_pijn_gemiddeld_1, na.rm=TRUE),
            mean_vaspijn= mean(vas_pijn_gemiddeld_1, na.rm=TRUE))

geslacht_zijde <- cleannames_data_long %>%
  group_by(geslacht, zijde) %>%
  summarise(max_vaspijn= max(vas_pijn_gemiddeld_1,na.rm=TRUE),
            min_vaspijn= min(vas_pijn_gemiddeld_1, na.rm=TRUE),
            mean_vaspijn= mean(vas_pijn_gemiddeld_1, na.rm=TRUE),
            med_vaspijn= median(vas_pijn_gemiddeld_1,na.rm=TRUE),
            sd_vaspijn= sd(vas_pijn_gemiddeld_1, na.rm=TRUE))

# compute variable using mutate
data_long_gempijn <- cleannames_data_long %>%
  mutate(gempijn = ((vas_pijn_gemiddeld_1 + vas_pijn_rust_1 + vas_pijn_belasten_1)/3)) %>%
  mutate(veelpijn= (gempijn > 50))

# save data
save(data_long_gempijn, file = "data_long_gempijn.RData")
  
