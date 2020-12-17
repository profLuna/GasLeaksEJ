# Analysis of relationship between gas leaks and demographics

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(ggplot2)
library(foreign) # for reading in dbf
library(tidytext) # for reordering within ggplot2 facets

# Load demographic and gas leaks data
load("Data/Demographics.rds")
load("Data/HEET2019Leaks.rds")

# Load natural gas utility service areas dbf from MassGIS and join to MassGIS towns layer
ng_dbf <- read.dbf("Data/pubutil/TOWNS_POLY_UTILITIES.dbf")

ng_service_areas <- st_read(dsn = "Data/townssurvey_shp",
                            layer = "TOWNSSURVEY_POLYM") %>% 
  select(-TOWN) %>% 
  left_join(., ng_dbf, by = "TOWN_ID") %>% 
  select(TOWN, GAS, GAS_LABEL) %>%  
  st_transform(., crs = st_crs(ma_blkgrps18)) %>% 
  st_make_valid()

# assign natural gas territories to block groups and keep only those block groups in utility territories for which we have leaks data
ma_blkgrps18 <- ng_service_areas %>% 
  select(-TOWN) %>% 
  st_join(ma_blkgrps18, ., largest = TRUE) %>% 
  filter(!GAS %in% c("Blackstone Gas Company", 
                     "Middleboro Municipal Gas and Electric Department",
                     "Westfield Gas and Electric Department",
                     "Wakefield Municipal Gas and Light Department",
                     "Holyoke Gas and Electric Department",
                     "No gas service")) %>% 
  filter(!st_is_empty(.))


# assign block group data to each unrepaired leak it intersects. 
# join block group info to each leak that falls within it
unrepaired2019_with_blkgrp <- ma_blkgrps18 %>% 
  select(GEOID) %>%
  st_join(unrepaired2019final, ., largest = TRUE)
# do same for repaired leaks
repaired2019_with_blkgrp <- ma_blkgrps18 %>% 
  select(GEOID) %>%
  st_join(repaired2019final, ., largest = TRUE)

# break them out by leak class
unrepaired2019_with_blkgrpC1 <- unrepaired2019_with_blkgrp %>% 
  filter(Class == "1")

unrepaired2019_with_blkgrpC2 <- unrepaired2019_with_blkgrp %>% 
  filter(Class == "2")

unrepaired2019_with_blkgrpC3 <- unrepaired2019_with_blkgrp %>% 
  filter(Class == "3")
# do same for repaired
repaired2019_with_blkgrpC1 <- repaired2019_with_blkgrp %>% 
  filter(Class == "1")

repaired2019_with_blkgrpC2 <- repaired2019_with_blkgrp %>% 
  filter(Class == "2")

repaired2019_with_blkgrpC3 <- repaired2019_with_blkgrp %>% 
  filter(Class == "3")

# group by GEOID to get a simple count of all leaks within each block group
unrepaired2019_by_blkgrp <- unrepaired2019_with_blkgrp %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019total = n())

unrepaired2019_by_blkgrpC1 <- unrepaired2019_with_blkgrpC1 %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019totalC1 = n())

unrepaired2019_by_blkgrpC2 <- unrepaired2019_with_blkgrpC2 %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019totalC2 = n())

unrepaired2019_by_blkgrpC3 <- unrepaired2019_with_blkgrpC3 %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019totalC3 = n())
# do the same for repaired, but also compute avg days to repair
repaired2019_by_blkgrp <- repaired2019_with_blkgrp %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019total = n(), 
            DaysToRepairAvg = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMed = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_blkgrpC1 <- repaired2019_with_blkgrpC1 %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019totalC1 = n(), 
            DaysToRepairAvgC1 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC1 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_blkgrpC2 <- repaired2019_with_blkgrpC2 %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019totalC2 = n(), 
            DaysToRepairAvgC2 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC2 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_blkgrpC3 <- repaired2019_with_blkgrpC3 %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019totalC3 = n(), 
            DaysToRepairAvgC3 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC3 = median(DaysToRepair, na.rm = TRUE))

# group by two columns to get the count of leaks by unique combinations of Utility and GEOID so that we know which utility is responsible for which leaks in each block group.
unrepaired2019_by_utility <- unrepaired2019_with_blkgrp %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019total = n())

unrepaired2019_by_utilityC1 <- unrepaired2019_with_blkgrpC1 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019totalC1 = n())

unrepaired2019_by_utilityC2 <- unrepaired2019_with_blkgrpC2 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019totalC2 = n())

unrepaired2019_by_utilityC3 <- unrepaired2019_with_blkgrpC3 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019totalC3 = n())
# do the same for repaired
repaired2019_by_utility <- repaired2019_with_blkgrp %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019total = n(), 
            DaysToRepairAvg = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMed = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_utilityC1 <- repaired2019_with_blkgrpC1 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019totalC1 = n(), 
            DaysToRepairAvgC1 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC1 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_utilityC2 <- repaired2019_with_blkgrpC2 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019totalC2 = n(), 
            DaysToRepairAvgC2 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC2 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_utilityC3 <- repaired2019_with_blkgrpC3 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019totalC3 = n(), 
            DaysToRepairAvgC3= mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC3 = median(DaysToRepair, na.rm = TRUE))


# reshape df to get separate columns of leak counts by utility, with one row for every GEOID that has at least one leak from some utility
unrepaired2019_by_utility2 <- unrepaired2019_by_utility %>% 
  pivot_wider(., names_from = Utility, values_from = unrepaired2019total,
              names_glue = "{Utility}_19unrepaired")

unrepaired2019_by_utility2C1 <- unrepaired2019_by_utilityC1 %>% 
  pivot_wider(., names_from = Utility, values_from = unrepaired2019totalC1,
              names_glue = "{Utility}_19unrepairedC1")

unrepaired2019_by_utility2C2 <- unrepaired2019_by_utilityC2 %>% 
  pivot_wider(., names_from = Utility, values_from = unrepaired2019totalC2,
              names_glue = "{Utility}_19unrepairedC2")

unrepaired2019_by_utility2C3 <- unrepaired2019_by_utilityC3 %>% 
  pivot_wider(., names_from = Utility, values_from = unrepaired2019totalC3,
              names_glue = "{Utility}_19unrepairedC3")
# repeat for repaired, repeating for avg times
repaired2019_by_utility2 <- repaired2019_by_utility %>% 
  select(-starts_with("Days")) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = repaired2019total,
              names_glue = "{Utility}_19repaired")

repaired2019_by_utility2.b <- repaired2019_by_utility %>% 
  select(GEOID, Utility, DaysToRepairAvg) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairAvg,
              names_glue = "{Utility}_19repairedDaysAvg")

repaired2019_by_utility2.c <- repaired2019_by_utility %>% 
  select(GEOID, Utility, DaysToRepairMed) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairMed,
              names_glue = "{Utility}_19repairedDaysMed")

repaired2019_by_utility2C1 <- repaired2019_by_utilityC1 %>% 
  select(-starts_with("Days")) %>% 
  pivot_wider(., names_from = Utility, values_from = repaired2019totalC1,
              names_glue = "{Utility}_19repairedC1")

repaired2019_by_utility2C1.b <- repaired2019_by_utilityC1 %>% 
  select(GEOID, Utility, DaysToRepairAvgC1) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairAvgC1,
              names_glue = "{Utility}_19repairedDaysAvgC1")

repaired2019_by_utility2C1.c <- repaired2019_by_utilityC1 %>% 
  select(GEOID, Utility, DaysToRepairMedC1) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairMedC1,
              names_glue = "{Utility}_19repairedDaysMedC1")

repaired2019_by_utility2C2 <- repaired2019_by_utilityC2 %>% 
  select(-starts_with("Days")) %>% 
  pivot_wider(., names_from = Utility, values_from = repaired2019totalC2,
              names_glue = "{Utility}_19repairedC2")

repaired2019_by_utility2C2.b <- repaired2019_by_utilityC2 %>% 
  select(GEOID, Utility, DaysToRepairAvgC2) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairAvgC2,
              names_glue = "{Utility}_19repairedDaysAvgC2")

repaired2019_by_utility2C2.c <- repaired2019_by_utilityC2 %>% 
  select(GEOID, Utility, DaysToRepairMedC2) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairMedC2,
              names_glue = "{Utility}_19repairedDaysMedC2")

repaired2019_by_utility2C3 <- repaired2019_by_utilityC3 %>% 
  select(-starts_with("Days")) %>% 
  pivot_wider(., names_from = Utility, values_from = repaired2019totalC3,
              names_glue = "{Utility}_19repairedC3")

repaired2019_by_utility2C3.b <- repaired2019_by_utilityC3 %>% 
  select(GEOID, Utility, DaysToRepairAvgC3) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairAvgC3,
              names_glue = "{Utility}_19repairedDaysAvgC3")

repaired2019_by_utility2C3.c <- repaired2019_by_utilityC3 %>% 
  select(GEOID, Utility, DaysToRepairMedC3) %>% 
  pivot_wider(., names_from = Utility, 
              values_from = DaysToRepairMedC3,
              names_glue = "{Utility}_19repairedDaysMedC3")



# join total leak counts to the block group demographics
ma_blkgrps18 <- lapply(list(unrepaired2019_by_blkgrp,
                            unrepaired2019_by_blkgrpC1,
                            unrepaired2019_by_blkgrpC2,
                            unrepaired2019_by_blkgrpC3), function(x){
                              as.data.frame(x) %>% 
                                select(-geometry)}) %>% 
  reduce(full_join, by = "GEOID") %>% 
  # distinct(GEOID, .keep_all = TRUE) %>% # get rid of duplicate rows
  left_join(ma_blkgrps18, ., by = "GEOID") %>% 
  mutate(area_sqkm = as.numeric(st_area(.)/10^6),
         leaks_sqkm = if_else(unrepaired2019total > 0,
                              unrepaired2019total/area_sqkm, 0),
         leaks_sqkmC1 = if_else(unrepaired2019totalC1 > 0,
                              unrepaired2019totalC1/area_sqkm, 0),
         leaks_sqkmC2 = if_else(unrepaired2019totalC2 > 0,
                                unrepaired2019totalC2/area_sqkm, 0),
         leaks_sqkmC3 = if_else(unrepaired2019totalC3 > 0,
                                unrepaired2019totalC3/area_sqkm, 0))

# join leak counts by utility to the block group demographics
ma_blkgrps18 <- list(ma_blkgrps18, unrepaired2019_by_utility2, 
                          unrepaired2019_by_utility2C1,
                          unrepaired2019_by_utility2C2,
                          unrepaired2019_by_utility2C3) %>% 
  reduce(left_join, by = "GEOID") %>% 
  mutate(
    across(unrepaired2019total:`National Grid - Colonial Gas_19unrepairedC3`, 
           ~replace_na(., 0)))

# repeat for repaired leaks
# join total leak counts to the block group demographics
ma_blkgrps18 <- lapply(list(repaired2019_by_blkgrp,
                            repaired2019_by_blkgrpC1,
                            repaired2019_by_blkgrpC2,
                            repaired2019_by_blkgrpC3), function(x){
                              as.data.frame(x) %>% 
                                select(-geometry)}) %>% 
  reduce(full_join, by = "GEOID") %>% 
  left_join(ma_blkgrps18, ., by = "GEOID") %>% 
  mutate(REPleaks_sqkm = if_else(repaired2019total > 0,
                              repaired2019total/area_sqkm, 0),
         REPleaks_sqkmC1 = if_else(repaired2019totalC1 > 0,
                                repaired2019totalC1/area_sqkm, 0),
         REPleaks_sqkmC2 = if_else(repaired2019totalC2 > 0,
                                repaired2019totalC2/area_sqkm, 0),
         REPleaks_sqkmC3 = if_else(repaired2019totalC3 > 0,
                                repaired2019totalC3/area_sqkm, 0))

# join leak counts by utility to the block group demographics
ma_blkgrps18 <- list(ma_blkgrps18, repaired2019_by_utility2, 
                     repaired2019_by_utility2C1,
                     repaired2019_by_utility2C1.b,
                     repaired2019_by_utility2C1.c,
                     repaired2019_by_utility2C2,
                     repaired2019_by_utility2C2.b,
                     repaired2019_by_utility2C2.c,
                     repaired2019_by_utility2C3,
                     repaired2019_by_utility2C3.b,
                     repaired2019_by_utility2C3.c) %>% 
  reduce(left_join, by = "GEOID") %>% 
  mutate(
    across(c(starts_with("repaired"), starts_with("REP"), 
             `Berkshire Gas_19repaired`:`National Grid - Colonial Gas_19repaired`,
             `Berkshire Gas_19repairedC1`:`National Grid - Colonial Gas_19repairedC1`,
             `Berkshire Gas_19repairedC2`:`National Grid - Colonial Gas_19repairedC2`,
             `Berkshire Gas_19repairedC3`:`National Grid - Colonial Gas_19repairedC3`), 
           ~replace_na(., 0)),
    AllLeaks2019 = unrepaired2019total + repaired2019total,
    AllLeaks2019C1 = unrepaired2019totalC1 + repaired2019totalC1, 
    AllLeaks2019C2 = unrepaired2019totalC2 + repaired2019totalC2,
    AllLeaks2019C3 = unrepaired2019totalC3 + repaired2019totalC3) %>% 
  mutate(across(starts_with("AllLeaks2019"), ~ . /area_sqkm, 
                .names = "{.col}_sqkm"))


# # Look at distributions of leak counts by block group
# ma_blkgrps18 %>% 
#   ggplot(aes(x = unrepaired2019total)) + geom_histogram()
# 
# summary(ma_blkgrps18$unrepaired2019total)
# 
# tm_shape(ma_blkgrps18) + tm_fill(col = "unrepaired2019total", style = "fisher")
# 
# ma_blkgrps18 %>% 
#   ggplot(aes(x = leaks_sqkm)) + geom_histogram()
# 
# summary(ma_blkgrps18$leaks_sqkm)
# 
# tm_shape(ma_blkgrps18) + tm_fill(col = "leaks_sqkm", style = "quantile")
# tm_shape(ma_blkgrps18) + tm_fill(col = "leaks_sqkm", style = "fisher") + 
#   tm_shape(ng_service_areas) + tm_polygons(border.col = "GAS")
# 
# ng_service_areas2 <- ng_service_areas %>% 
#   group_by(GAS) %>% 
#   summarize()
# 
# ma_blkgrps18 %>% 
#   ggplot(aes(x = unrepaired2019totalC1)) + geom_histogram()
# 
# summary(ma_blkgrps18$unrepaired2019totalC1)
# 
# ma_blkgrps18 %>% 
#   ggplot(aes(x = unrepaired2019totalC2)) + geom_histogram()
# 
# summary(ma_blkgrps18$unrepaired2019totalC2)
# 
# ma_blkgrps18 %>% 
#   ggplot(aes(x = unrepaired2019totalC3)) + geom_histogram()
# 
# summary(ma_blkgrps18$unrepaired2019totalC3)


# comparison of population-weighted leak frequency and density by demographic group for priority populations, restricting to areas served by gas utilities for which we have leak data
ppLeakDensity <-  ma_blkgrps18 %>% 
  as.data.frame() %>% 
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH,0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), starts_with("leaks_"), 
         starts_with("AllLeaks"), starts_with("REPleaks_"), 
         starts_with("DaystoRepairAvg")) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKm = weighted.mean(x = leaks_sqkm, w = Pop),
            wLeaksPerSqKmC1 = weighted.mean(x = leaks_sqkmC1, w = Pop),
            wLeaksPerSqKmC2 = weighted.mean(x = leaks_sqkmC2, w = Pop),
            wLeaksPerSqKmC3 = weighted.mean(x = leaks_sqkmC3, w = Pop),
            wLeaksPerSqKmALL = weighted.mean(x = AllLeaks2019_sqkm, 
                                             w = Pop),
            wLeaksPerSqKmALLC1 = weighted.mean(x = AllLeaks2019C1_sqkm, 
                                             w = Pop),
            wLeaksPerSqKmALLC2 = weighted.mean(x = AllLeaks2019C2_sqkm, 
                                            w = Pop),
            wLeaksPerSqKmALLC3 = weighted.mean(x = AllLeaks2019C3_sqkm, 
                                            w = Pop),
            wLeaksPerSqKmREP = weighted.mean(x = REPleaks_sqkm, 
                                             w = Pop),
            wLeaksPerSqKmREPC1 = weighted.mean(x = REPleaks_sqkmC1, 
                                               w = Pop),
            wLeaksPerSqKmREPC2 = weighted.mean(x = REPleaks_sqkmC2, 
                                               w = Pop),
            wLeaksPerSqKmREPC3 = weighted.mean(x = REPleaks_sqkmC3, 
                                               w = Pop),
            wDaysToRepairAvg = weighted.mean(x = DaysToRepairAvg, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgC1 = weighted.mean(x = DaysToRepairAvgC1, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgC2 = weighted.mean(x = DaysToRepairAvgC2, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgC3 = weighted.mean(x = DaysToRepairAvgC3, 
                                               w = Pop, na.rm = T)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))

ppLeakDensityUC <-  ma_blkgrps18 %>% 
  as.data.frame() %>% 
  mutate(MA_INCOME17_UC = if_else(MA_INCOME17_UC == "I", totalpopE_UC, 0)) %>% 
  mutate(MA_INCOME17_UC = replace_na(MA_INCOME17_UC,0)) %>% 
  mutate(MA_INCOME21_UC = if_else(MA_INCOME21_UC == "I", totalpopE_UC, 0)) %>% 
  mutate(MA_INCOME21_UC = replace_na(MA_INCOME21_UC,0)) %>% 
  mutate(MA_MINORITY17_UC = if_else(MA_MINORITY17_UC == "M", totalpopE_UC,0)) %>%
  mutate(MA_MINORITY17_UC = replace_na(MA_MINORITY17_UC,0)) %>% 
  mutate(MA_MINORITY21_UC = if_else(MA_MINORITY21_UC == "M", totalpopE_UC,0)) %>%
  mutate(MA_MINORITY21_UC = replace_na(MA_MINORITY21_UC,0)) %>% 
  mutate(MA_ENGLISH_UC = if_else(MA_ENGLISH_UC == "E", totalpopE_UC,0)) %>%
  mutate(MA_ENGLISH_UC = replace_na(MA_ENGLISH_UC,0)) %>%
  select(ends_with("_UC") & (starts_with("nh") | starts_with("MA_")), 
         hisppop_UC, minority_UC, totalpop_UC, eng_hh_UC, under5E_UC, over64E_UC,
         eng_limitE_UC, num2povE_UC, lthsE_UC, total_occ_units_UC, 
         renter_occ_units_UC, leaks_sqkm) %>% 
  pivot_longer(., cols = MA_INCOME21_UC:renter_occ_units_UC, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKm = weighted.mean(x = leaks_sqkm, w = Pop)) %>% 
  rename(., wLeaksPerSqKmUC = wLeaksPerSqKm) %>%
  mutate(Group = recode(Group, "hisppop_UC" = "Hispanic", 
                        "minority_UC" = "People of Color",
                        "nh2morepop_UC" = "Two or more races",
                        "nhamerindpop_UC" = "Native American",
                        "nhasianpop_UC" = "Asian",
                        "nhblackpop_UC" = "Black",
                        "nhnativpop_UC" = "Native Pacific Islander",
                        "nhotherpop_UC" = "Other race",
                        "nhwhitepop_UC" = "White",
                        "totalpop_UC" = "Total Population",
                        "eng_hh_UC" = "Total Households",
                        "under5E_UC" = "Under 5",
                        "over64E_UC" = "Over 64",
                        "eng_limitE_UC" = "Limited English HH",
                        "num2povE_UC" = "Low Income",
                        "lthsE_UC" = "No HS Diploma",
                        "total_occ_units_UC" = "Total Occupied HU",
                        "renter_occ_units_UC" = "Renter Occupied HU",
                        "MA_INCOME17_UC" = "MA_INCOME17",
                        "MA_INCOME21_UC" = "MA_INCOME21",
                        "MA_MINORITY17_UC" = "MA_MINORITY17",
                        "MA_MINORITY21_UC"= "MA_MINORITY21",
                        "MA_ENGLISH_UC" = "MA_ENGLISH"))

ppLeakDensityLC <-  ma_blkgrps18 %>% 
  as.data.frame() %>% 
  mutate(MA_INCOME17_LC = if_else(MA_INCOME17_LC == "I", totalpopE_LC, 0)) %>% 
  mutate(MA_INCOME17_LC = replace_na(MA_INCOME17_LC,0)) %>% 
  mutate(MA_INCOME21_LC = if_else(MA_INCOME21_LC == "I", totalpopE_LC, 0)) %>% 
  mutate(MA_INCOME21_LC = replace_na(MA_INCOME21_LC,0)) %>% 
  mutate(MA_MINORITY17_LC = if_else(MA_MINORITY17_LC == "M", totalpopE_LC,0)) %>%
  mutate(MA_MINORITY17_LC = replace_na(MA_MINORITY17_LC,0)) %>% 
  mutate(MA_MINORITY21_LC = if_else(MA_MINORITY21_LC == "M", totalpopE_LC,0)) %>%
  mutate(MA_MINORITY21_LC = replace_na(MA_MINORITY21_LC,0)) %>% 
  mutate(MA_ENGLISH_LC = if_else(MA_ENGLISH_LC == "E", totalpopE_LC,0)) %>%
  mutate(MA_ENGLISH_LC = replace_na(MA_ENGLISH_LC,0)) %>%
  select(ends_with("_LC") & (starts_with("nh") | starts_with("MA_")), 
         hisppop_LC, minority_LC, totalpop_LC, eng_hh_LC, under5E_LC, over64E_LC,
         eng_limitE_LC, num2povE_LC, lthsE_LC, total_occ_units_LC, 
         renter_occ_units_LC, leaks_sqkm) %>% 
  pivot_longer(., cols = MA_INCOME21_LC:renter_occ_units_LC, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKm = weighted.mean(x = leaks_sqkm, w = Pop)) %>% 
  rename(., wLeaksPerSqKmLC = wLeaksPerSqKm) %>%
  mutate(Group = recode(Group, "hisppop_LC" = "Hispanic", 
                        "minority_LC" = "People of Color",
                        "nh2morepop_LC" = "Two or more races",
                        "nhamerindpop_LC" = "Native American",
                        "nhasianpop_LC" = "Asian",
                        "nhblackpop_LC" = "Black",
                        "nhnativpop_LC" = "Native Pacific Islander",
                        "nhotherpop_LC" = "Other race",
                        "nhwhitepop_LC" = "White",
                        "totalpop_LC" = "Total Population",
                        "eng_hh_LC" = "Total Households",
                        "under5E_LC" = "Under 5",
                        "over64E_LC" = "Over 64",
                        "eng_limitE_LC" = "Limited English HH",
                        "num2povE_LC" = "Low Income",
                        "lthsE_LC" = "No HS Diploma",
                        "total_occ_units_LC" = "Total Occupied HU",
                        "renter_occ_units_LC" = "Renter Occupied HU",
                        "MA_INCOME17_LC" = "MA_INCOME17",
                        "MA_INCOME21_LC" = "MA_INCOME21",
                        "MA_MINORITY17_LC" = "MA_MINORITY17",
                        "MA_MINORITY21_LC"= "MA_MINORITY21",
                        "MA_ENGLISH_LC" = "MA_ENGLISH"))

# bring df together
ppLeakDensityJoined <- ppLeakDensity %>% 
  select(-c(wLeaksPerSqKmC1:wLeaksPerSqKmC3)) %>% 
  left_join(., ppLeakDensityLC, by = "Group") %>% 
  left_join(., ppLeakDensityUC, by = "Group")

# create a bar plot of total leaks with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKm), y = wLeaksPerSqKm)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab(expression(paste("Population-weighted mean leak density (leaks/", 
                        km^2, ")", " by Census Block Group",sep = ""))) +
  theme_minimal() +
  geom_errorbar(aes(ymin = wLeaksPerSqKmUC, ymax = wLeaksPerSqKmLC)) +
  ggtitle("Priority Populations and Gas Leaks in 2018 across Massachusetts")

ggsave("Images/LeaksPP_blkgrp_moe.png")

# create a facet wrap bar graph by leak grade and ordered by leaks
ppLeakDensity %>% 
  pivot_longer(wLeaksPerSqKm:wLeaksPerSqKmC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKm" = "All Leaks",
                            "wLeaksPerSqKmC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Block Group",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2018 across Massachusetts")

ggsave("Images/LeaksPPbyClass_blkgrp.png")


# Compare leak density distribution by utility
# National Grid; includes both NG-Boston and NG-Colonial Gas
ppLeakDensityNG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^National") | 
           GAS == "Colonial Gas") %>% # limit to BGs in NG/Colonial svc area
  mutate(leaks_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                           `National Grid - Colonial Gas_19unrepaired`)/area_sqkm) %>% 
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH,0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), leaks_sqkmNG) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmNG = weighted.mean(x = leaks_sqkmNG, w = Pop)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))

# Eversource Energy
ppLeakDensityEV <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Eversource") | 
           str_detect(GAS, "Energy$")) %>% # limit to BGs in Eversource svc area
  mutate(leaks_sqkmEV = (`Eversource Energy_19unrepaired`)/area_sqkm) %>%
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH,0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), leaks_sqkmEV) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmEV = weighted.mean(x = leaks_sqkmEV, w = Pop)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))

# Columbia Gas
ppLeakDensityCG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Columbia") | 
           str_detect(GAS, "Columbia Gas$")) %>% # limit to BGs in CG svc area
  mutate(leaks_sqkmCG = (`Columbia Gas_19unrepaired`)/area_sqkm) %>%
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH,0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), leaks_sqkmCG) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmCG = weighted.mean(x = leaks_sqkmCG, w = Pop)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))

# Fitchburg Gas aka Unitil
ppLeakDensityFG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Unitil") | 
           str_detect(GAS, "Unitil$")) %>% # limit to BGs in FG svc area
  mutate(leaks_sqkmFG = (`Fitchburg Gas_19unrepaired`)/area_sqkm) %>%
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH, 0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), leaks_sqkmFG) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmFG = weighted.mean(x = leaks_sqkmFG, w = Pop, 
                                            na.rm = TRUE)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))

# Liberty Utilities
ppLeakDensityLU <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(GAS == "Liberty Utilities") %>% # limit to BGs in LU svc area
  mutate(leaks_sqkmLU = (`Liberty Utilities_19unrepaired`)/area_sqkm) %>%
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH, 0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), leaks_sqkmLU) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmLU = weighted.mean(x = leaks_sqkmLU, w = Pop, 
                                            na.rm = TRUE)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))

# The Berkshire Gas Company 
ppLeakDensityBG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(GAS == "The Berkshire Gas Company") %>% # limit to BGs in BG svc area
  mutate(leaks_sqkmBG = (`Berkshire Gas_19unrepaired`)/area_sqkm) %>%
  mutate(MA_INCOME17 = if_else(MA_INCOME17 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME17 = replace_na(MA_INCOME17,0)) %>% 
  mutate(MA_INCOME21 = if_else(MA_INCOME21 == "I", totalpop_E, 0)) %>% 
  mutate(MA_INCOME21 = replace_na(MA_INCOME21,0)) %>% 
  mutate(MA_MINORITY17 = if_else(MA_MINORITY17 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY17 = replace_na(MA_MINORITY17,0)) %>% 
  mutate(MA_MINORITY21 = if_else(MA_MINORITY21 == "M", totalpop_E,0)) %>%
  mutate(MA_MINORITY21 = replace_na(MA_MINORITY21,0)) %>% 
  mutate(MA_ENGLISH = if_else(MA_ENGLISH == "E", totalpop_E,0)) %>%
  mutate(MA_ENGLISH = replace_na(MA_ENGLISH, 0)) %>%
  select(ends_with("_E"), ends_with("17"), ends_with("21"), MA_ENGLISH, 
         eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), leaks_sqkmBG) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmBG = weighted.mean(x = leaks_sqkmBG, w = Pop, 
                                            na.rm = TRUE)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                        "minority_E" = "People of Color",
                        "nh2morepop_E" = "Two or more races",
                        "nhamerindpop_E" = "Native American",
                        "nhasianpop_E" = "Asian",
                        "nhblackpop_E" = "Black",
                        "nhnativpop_E" = "Native Pacific Islander",
                        "nhotherpop_E" = "Other race",
                        "nhwhitepop_E" = "White",
                        "totalpop_E" = "Total Population",
                        "eng_hhE" = "Total Households",
                        "under5E" = "Under 5",
                        "over64E" = "Over 64",
                        "eng_limitE" = "Limited English HH",
                        "num2povE" = "Low Income",
                        "lthsE" = "No HS Diploma",
                        "total_occ_unitsE" = "Total Occupied HU",
                        "renter_occ_unitsE" = "Renter Occupied HU"))


# Join the utility df together
ppLeakDensityJoinedU <- list(ppLeakDensityBG,
                               ppLeakDensityCG,
                               ppLeakDensityEV,
                               ppLeakDensityFG,
                               ppLeakDensityLU,
                               ppLeakDensityNG) %>% 
  reduce(left_join, by = "Group")


# Facet wrap by utility
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmBG:wLeaksPerSqKmNG, 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wLeaksPerSqKmBG" = "Berkshire Gas",
                          "wLeaksPerSqKmCG" = "Columbia Gas",
                          "wLeaksPerSqKmEV" = "Eversource Energy",
                          "wLeaksPerSqKmFG" = "Unitil/Fitchburg Gas",
                          "wLeaksPerSqKmLU" = "Liberty Utilities",
                          "wLeaksPerSqKmNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Block Group", sep = "")),
       title = "Piority Populations and Unrepaired Gas Leaks by Utility for 2018 across Massachusetts")

ggsave("Images/LeaksPPbyUtility_blkgrp.png")




# conduct a weighted t-test to determine if differences are statistically significant
library(weights)
wtd.t.test(x = ma_blkgrps18$leaks_sqkm, y = ma_blkgrps18$leaks_sqkm, 
           weight = ma_blkgrps18$nhwhitepop_E, 
           weighty = ma_blkgrps18$nhblackpop_E, 
           bootse = TRUE, bootp = TRUE)



# correlation matrix between leak frequency and demographic group