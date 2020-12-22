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
ma_blkgrps18 <- list(ma_blkgrps18, 
                     unrepaired2019_by_utility2,
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
ma_blkgrps18 <- list(ma_blkgrps18, 
                     repaired2019_by_utility2, 
                     repaired2019_by_utility2.b,
                     repaired2019_by_utility2.c,
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
                .names = "{.col}_sqkm")) %>% 
  mutate(PctRepaired19 = if_else((repaired2019total == 0 | AllLeaks2019 == 0), 
                                 0, (repaired2019total/AllLeaks2019)*100),
         PctRepaired19C1 = if_else((repaired2019totalC1 == 0 | 
                                      AllLeaks2019C1 == 0), 0,
                                   (repaired2019totalC1/AllLeaks2019C1)*100),
         PctRepaired19C2 = if_else((repaired2019totalC2 == 0 | 
                                      AllLeaks2019C2 == 0), 0,
                                   (repaired2019totalC2/AllLeaks2019C2)*100),
         PctRepaired19C3 = if_else((repaired2019totalC3 == 0 | 
                                      AllLeaks2019C3 == 0), 0,
                                   (repaired2019totalC3/AllLeaks2019C3)*100),
         leaks_hu = if_else((unrepaired2019total == 0 | 
                               total_occ_unitsE == 0), 0,
                            (unrepaired2019total/total_occ_unitsE)*100),
         leaks_huC1 = if_else((unrepaired2019totalC1 == 0 | 
                               total_occ_unitsE == 0), 0,
                            (unrepaired2019totalC1/total_occ_unitsE)*100),
         leaks_huC2 = if_else((unrepaired2019totalC2 == 0 | 
                                 total_occ_unitsE == 0), 0,
                              (unrepaired2019totalC2/total_occ_unitsE)*100),
         leaks_huC3 = if_else((unrepaired2019totalC3 == 0 | 
                                 total_occ_unitsE == 0), 0,
                              (unrepaired2019totalC3/total_occ_unitsE)*100),
         REPleaks_hu = if_else((repaired2019total == 0 | 
                               total_occ_unitsE == 0), 0,
                            (repaired2019total/total_occ_unitsE)*100),
         REPleaks_huC1 = if_else((repaired2019totalC1 == 0 | 
                                 total_occ_unitsE == 0), 0,
                              (repaired2019totalC1/total_occ_unitsE)*100),
         REPleaks_huC2 = if_else((repaired2019totalC2 == 0 | 
                                 total_occ_unitsE == 0), 0,
                              (repaired2019totalC2/total_occ_unitsE)*100),
         REPleaks_huC3 = if_else((repaired2019totalC3 == 0 | 
                                 total_occ_unitsE == 0), 0,
                              (repaired2019totalC3/total_occ_unitsE)*100),
         ALLleaks_hu = if_else((AllLeaks2019 == 0 | 
                                  total_occ_unitsE == 0), 0,
                               (AllLeaks2019/total_occ_unitsE)*100),
         ALLleaks_huC1 = if_else((AllLeaks2019C1 == 0 | 
                                    total_occ_unitsE == 0), 0,
                                 (AllLeaks2019C1/total_occ_unitsE)*100),
         ALLleaks_huC2 = if_else((AllLeaks2019C2 == 0 | 
                                    total_occ_unitsE == 0), 0,
                                 (AllLeaks2019C2/total_occ_unitsE)*100),
         ALLleaks_huC3 = if_else((AllLeaks2019C3 == 0 | 
                                    total_occ_unitsE == 0), 0,
                                 (AllLeaks2019C3/total_occ_unitsE)*100))


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
         starts_with("DaystoRepairAvg"), starts_with("PctRepaired19"),
         starts_with("leaks_hu"), starts_with("REPleaks_hu"),
         starts_with("ALLleaks_hu")) %>% 
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
                                               w = Pop, na.rm = T),
            wPctRepaired19 = weighted.mean(x = PctRepaired19, w = Pop, 
                                           na.rm = T),
            wPctRepaired19C1 = weighted.mean(x = PctRepaired19C1, w = Pop, 
                                           na.rm = T),
            wPctRepaired19C2 = weighted.mean(x = PctRepaired19C2, w = Pop, 
                                           na.rm = T),
            wPctRepaired19C3 = weighted.mean(x = PctRepaired19C3, w = Pop, 
                                           na.rm = T),
            wLeaksPerHU = weighted.mean(x = leaks_hu, w = Pop, 
                                        na.rm = T),
            wLeaksPerHUC1 = weighted.mean(x = leaks_huC1, w = Pop, 
                                        na.rm = T),
            wLeaksPerHUC2 = weighted.mean(x = leaks_huC2, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUC3 = weighted.mean(x = leaks_huC3, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHU = weighted.mean(x = REPleaks_hu, w = Pop, 
                                        na.rm = T),
            wREPLeaksPerHUC1 = weighted.mean(x = REPleaks_huC1, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHUC2 = weighted.mean(x = REPleaks_huC2, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHUC3 = weighted.mean(x = REPleaks_huC3, w = Pop, 
                                          na.rm = T),
            wALLLeaksPerHU = weighted.mean(x = ALLleaks_hu, w = Pop, 
                                           na.rm = T),
            wALLLeaksPerHUC1 = weighted.mean(x = ALLleaks_huC1, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHUC2 = weighted.mean(x = ALLleaks_huC2, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHUC3 = weighted.mean(x = ALLleaks_huC3, w = Pop, 
                                             na.rm = T)) %>% 
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
         hisppop_UC, minority_UC, totalpop_UC, eng_hh_UC, under5E_UC, 
         over64E_UC, eng_limitE_UC, num2povE_UC, lthsE_UC, total_occ_units_UC, 
         renter_occ_units_UC, starts_with("leaks_"), 
         starts_with("AllLeaks"), starts_with("REPleaks_"), 
         starts_with("DaystoRepairAvg"), starts_with("PctRepaired19"),
         starts_with("leaks_hu"), starts_with("REPleaks_hu"),
         starts_with("ALLleaks_hu")) %>% 
  pivot_longer(., cols = MA_INCOME21_UC:renter_occ_units_UC, names_to = "Group", 
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
                                               w = Pop, na.rm = T),
            wPctRepaired19 = weighted.mean(x = PctRepaired19, w = Pop, 
                                           na.rm = T),
            wPctRepaired19C1 = weighted.mean(x = PctRepaired19C1, w = Pop, 
                                             na.rm = T),
            wPctRepaired19C2 = weighted.mean(x = PctRepaired19C2, w = Pop, 
                                             na.rm = T),
            wPctRepaired19C3 = weighted.mean(x = PctRepaired19C3, w = Pop, 
                                             na.rm = T),
            wLeaksPerHU = weighted.mean(x = leaks_hu, w = Pop, 
                                        na.rm = T),
            wLeaksPerHUC1 = weighted.mean(x = leaks_huC1, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUC2 = weighted.mean(x = leaks_huC2, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUC3 = weighted.mean(x = leaks_huC3, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHU = weighted.mean(x = REPleaks_hu, w = Pop, 
                                           na.rm = T),
            wREPLeaksPerHUC1 = weighted.mean(x = REPleaks_huC1, w = Pop, 
                                             na.rm = T),
            wREPLeaksPerHUC2 = weighted.mean(x = REPleaks_huC2, w = Pop, 
                                             na.rm = T),
            wREPLeaksPerHUC3 = weighted.mean(x = REPleaks_huC3, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHU = weighted.mean(x = ALLleaks_hu, w = Pop, 
                                           na.rm = T),
            wALLLeaksPerHUC1 = weighted.mean(x = ALLleaks_huC1, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHUC2 = weighted.mean(x = ALLleaks_huC2, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHUC3 = weighted.mean(x = ALLleaks_huC3, w = Pop, 
                                             na.rm = T)) %>% 
  # rename(., wLeaksPerSqKmUC = wLeaksPerSqKm) %>%
  rename_with(., .fn = ~paste0(., "UC"), .cols = starts_with("w")) %>% 
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
         renter_occ_units_LC, starts_with("leaks_"), 
         starts_with("AllLeaks"), starts_with("REPleaks_"), 
         starts_with("DaystoRepairAvg"), starts_with("PctRepaired19"),
         starts_with("leaks_hu"), starts_with("REPleaks_hu"),
         starts_with("ALLleaks_hu")) %>% 
  pivot_longer(., cols = MA_INCOME21_LC:renter_occ_units_LC, names_to = "Group", 
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
                                               w = Pop, na.rm = T),
            wPctRepaired19 = weighted.mean(x = PctRepaired19, w = Pop, 
                                           na.rm = T),
            wPctRepaired19C1 = weighted.mean(x = PctRepaired19C1, w = Pop, 
                                             na.rm = T),
            wPctRepaired19C2 = weighted.mean(x = PctRepaired19C2, w = Pop, 
                                             na.rm = T),
            wPctRepaired19C3 = weighted.mean(x = PctRepaired19C3, w = Pop, 
                                             na.rm = T),
            wLeaksPerHU = weighted.mean(x = leaks_hu, w = Pop, 
                                        na.rm = T),
            wLeaksPerHUC1 = weighted.mean(x = leaks_huC1, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUC2 = weighted.mean(x = leaks_huC2, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUC3 = weighted.mean(x = leaks_huC3, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHU = weighted.mean(x = REPleaks_hu, w = Pop, 
                                           na.rm = T),
            wREPLeaksPerHUC1 = weighted.mean(x = REPleaks_huC1, w = Pop, 
                                             na.rm = T),
            wREPLeaksPerHUC2 = weighted.mean(x = REPleaks_huC2, w = Pop, 
                                             na.rm = T),
            wREPLeaksPerHUC3 = weighted.mean(x = REPleaks_huC3, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHU = weighted.mean(x = ALLleaks_hu, w = Pop, 
                                           na.rm = T),
            wALLLeaksPerHUC1 = weighted.mean(x = ALLleaks_huC1, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHUC2 = weighted.mean(x = ALLleaks_huC2, w = Pop, 
                                             na.rm = T),
            wALLLeaksPerHUC3 = weighted.mean(x = ALLleaks_huC3, w = Pop, 
                                             na.rm = T)) %>% 
  # rename(., wLeaksPerSqKmLC = wLeaksPerSqKm) %>%
  rename_with(., .fn = ~paste0(., "LC"), .cols = starts_with("w")) %>%
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
  # select(-c(wLeaksPerSqKmC1:wLeaksPerSqKmC3)) %>% 
  left_join(., ppLeakDensityLC, by = "Group") %>% 
  left_join(., ppLeakDensityUC, by = "Group")

# create a bar plot of total unrepaired leak density with error bars
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
  ggtitle("Priority Populations and Unrepaired Gas Leaks in 2019\nacross Massachusetts")

ggsave("Images/LeaksPP_blkgrp_moe.png")

# create a bar plot of total unrepaired leaks per occupied housing unit with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerHU), y = wLeaksPerHU)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted mean leaks per occupied housing unit\nby Census Block Group") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wLeaksPerHUUC, ymax = wLeaksPerHULC)) +
  ggtitle("Priority Populations and Unrepaired Gas Leaks\nPer Housing Unit in 2019 across Massachusetts")

ggsave("Images/LeaksPP_HU_blkgrp_moe.png")

# create a bar plot of total leaks (unrepaired + repaired) with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKmALL), y = wLeaksPerSqKmALL)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab(expression(paste("Population-weighted mean leak density (leaks/", 
                        km^2, ")", " by Census Block Group",sep = ""))) +
  theme_minimal() +
  geom_errorbar(aes(ymin = wLeaksPerSqKmALLUC, ymax = wLeaksPerSqKmALLLC)) +
  ggtitle("Priority Populations and All Repaired and Unrepaired Gas Leaks\nin 2019 across Massachusetts")

ggsave("Images/LeaksPPall_blkgrp_moe.png")

# create a bar plot of total leaks (unrepaired + repaired) per occupied housing unit with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wALLLeaksPerHU), y = wALLLeaksPerHU)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted mean leaks per occupied housing unit\nby Census Block Group") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wALLLeaksPerHUUC, ymax = wALLLeaksPerHULC)) +
  ggtitle("Priority Populations and All Repaired and Unrepaired Gas\nLeaks Per Housing Unit in 2019 across Massachusetts")

ggsave("Images/LeaksPPall_HU_blkgrp_moe.png")

# create a bar plot of repaired leaks with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKmREP), y = wLeaksPerSqKmREP)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab(expression(paste("Population-weighted mean leak density (leaks/", 
                        km^2, ")", " by Census Block Group",sep = ""))) +
  theme_minimal() +
  geom_errorbar(aes(ymin = wLeaksPerSqKmREPUC, ymax = wLeaksPerSqKmREPLC)) +
  ggtitle("Priority Populations and Repaired Gas Leaks\nin 2019 across Massachusetts")

ggsave("Images/LeaksPPrepaired_blkgrp_moe.png")

# create a bar plot of total repaired leaks per occupied housing unit with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wREPLeaksPerHU), y = wREPLeaksPerHU)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted mean leaks per occupied housing unit\nby Census Block Group") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wREPLeaksPerHUUC, ymax = wREPLeaksPerHULC)) +
  ggtitle("Priority Populations and Repaired Gas Leaks\nPer Housing Unit in 2019 across Massachusetts")

ggsave("Images/LeaksPPrepaired_HU_blkgrp_moe.png")


# create a bar plot of avg repair times with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wDaysToRepairAvg), y = wDaysToRepairAvg)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted mean leak repair time (days) by Census Block Group") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wDaysToRepairAvgUC, ymax = wDaysToRepairAvgLC)) +
  ggtitle("Priority Populations and Average Leak Repair Times\nin 2019 across Massachusetts")

ggsave("Images/LeaksPPtimeRep_blkgrp_moe.png")

# create a bar plot of percentage of leaks repaired with error bars
ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>% 
  ggplot(aes(x = reorder(Group,wPctRepaired19), y = wPctRepaired19)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted Percentage of Leaks Repaired in 2019\nby Census Block Group") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wPctRepaired19UC, ymax = wPctRepaired19LC)) +
  ggtitle("Priority Populations and Percentage of Leaks Repaired\nin 2019 across Massachusetts")

ggsave("Images/LeaksPPpctFixed_blkgrp_moe.png")


# create a facet wrap bar graph by leak grade and ordered by unrepaired leaks
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
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClass_blkgrp.png")

# create a facet wrap bar graph by leak grade and ordered by unrepaired leaks per occupied housing unit
ppLeakDensity %>% 
  pivot_longer(wLeaksPerHU:wLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerHU" = "All Leaks",
                            "wLeaksPerHUC1" = "Class 1 Leaks",
                            "wLeaksPerHUC2" = "Class 2 Leaks",
                            "wLeaksPerHUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leaks per occupied housing unit by Census Block Group",
       title = "Priority Populations and Unrepaired Gas Leaks Per Housing Unit in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClass_HU_blkgrp.png")


# create a facet wrap bar graph by leak grade and ordered by all repaired and unrepaired leaks
ppLeakDensity %>% 
  pivot_longer(wLeaksPerSqKmALL:wLeaksPerSqKmALLC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALL" = "All Leaks",
                            "wLeaksPerSqKmALLC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmALLC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmALLC3" = "Class 3 Leaks"),
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
       title = "Priority Populations and All Repaired and Unrepaired Gas Leaks in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClassAll_blkgrp.png")

# create a facet wrap bar graph by leak grade and ordered by all repaired and unrepaired leaks per occupied housing unit
ppLeakDensity %>% 
  pivot_longer(wALLLeaksPerHU:wALLLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHU" = "All Leaks",
                            "wALLLeaksPerHUC1" = "Class 1 Leaks",
                            "wALLLeaksPerHUC2" = "Class 2 Leaks",
                            "wALLLeaksPerHUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leaks per occupied housing unit by Census Block Group",
       title = "Priority Populations and All Repaired and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClassAll_HU_blkgrp.png")


# create a facet wrap bar graph by leak grade and ordered by all repaired leaks
ppLeakDensity %>% 
  pivot_longer(wLeaksPerSqKmREP:wLeaksPerSqKmREPC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREP" = "All Leaks",
                            "wLeaksPerSqKmREPC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmREPC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmREPC3" = "Class 3 Leaks"),
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
       title = "Priority Populations and All Repaired Gas Leaks in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClassREP_blkgrp.png")

# create a facet wrap bar graph by leak grade and ordered by all repaired leaks per occupied housing unit
ppLeakDensity %>% 
  pivot_longer(wREPLeaksPerHU:wREPLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHU" = "All Leaks",
                            "wREPLeaksPerHUC1" = "Class 1 Leaks",
                            "wREPLeaksPerHUC2" = "Class 2 Leaks",
                            "wREPLeaksPerHUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leaks per occupied housing unit by Census Block Group",
       title = "Priority Populations and All Repaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClassREP_HU_blkgrp.png")


# create a facet wrap bar graph by leak grade and ordered by avg leak repair time
ppLeakDensity %>% 
  pivot_longer(wDaysToRepairAvg:wDaysToRepairAvgC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvg" = "All Leaks",
                            "wDaysToRepairAvgC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time (days) by Census Block Group",
       title = "Priority Populations and Average Leak Repair Time in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClassTime_blkgrp.png")


# create a facet wrap bar graph by leak grade and ordered by avg pct of leaks fixed
ppLeakDensity %>% 
  pivot_longer(wPctRepaired19:wPctRepaired19C3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  mutate(leakClass = recode(leakClass, "wPctRepaired19" = "All Leaks",
                            "wPctRepaired19C1" = "Class 1 Leaks",
                            "wPctRepaired19C2" = "Class 2 Leaks",
                            "wPctRepaired19C3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean percentage of leaks repaired in 2019 by Census Block Group",
       title = "Priority Populations and Percent Repaired Gas Leaks in 2019 across Massachusetts")

ggsave("Images/LeaksPPbyClassPctFix_blkgrp.png")




# Compare leak density distribution by utility
# National Grid; includes both NG-Boston and NG-Colonial Gas
ppLeakDensityNG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^National") | 
           GAS == "Colonial Gas") %>% # limit to BGs in NG/Colonial svc area
  mutate(leaks_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                           `National Grid - Colonial Gas_19unrepaired`)/area_sqkm,
         REPleaks_sqkmNG = (`National Grid - Boston Gas_19repaired` +
                            `National Grid - Colonial Gas_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                                `National Grid - Colonial Gas_19unrepaired` + 
                                `National Grid - Boston Gas_19repaired` +
                                `National Grid - Colonial Gas_19repaired`)/area_sqkm,
         leaks_huNG = if_else(total_occ_unitsE == 0, 0, 
                              (`National Grid - Boston Gas_19unrepaired` +
                           `National Grid - Colonial Gas_19unrepaired`)/total_occ_unitsE),
         REPleaks_huNG = if_else(total_occ_unitsE == 0, 0,
                                 (`National Grid - Boston Gas_19repaired` +
                            `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huNG = if_else(total_occ_unitsE == 0, 0,
                                     (`National Grid - Boston Gas_19unrepaired` + `National Grid - Colonial Gas_19unrepaired` + 
                                `National Grid - Boston Gas_19repaired` +
                                `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
         PctRepaired19NG = (`National Grid - Boston Gas_19repaired` +
                              `National Grid - Colonial Gas_19repaired`)/ (`National Grid - Boston Gas_19unrepaired` +
                              `National Grid - Colonial Gas_19unrepaired` + 
                              `National Grid - Boston Gas_19repaired` +
                              `National Grid - Colonial Gas_19repaired`)*100) %>% 
  rowwise() %>% 
  mutate(DaysToRepairAvgNG = sum(`National Grid - Boston Gas_19repairedDaysAvg`,                                 `National Grid - Colonial Gas_19repairedDaysAvg`, na.rm = TRUE)/2) %>% 
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
         ends_with("unitsE"), (starts_with("leaks_") & ends_with("NG")), 
         (starts_with("AllLeaks") & ends_with("NG")), 
         (starts_with("REPleaks_") & ends_with("NG")), 
         (starts_with("DaystoRepairAvg") & ends_with("NG")), 
         (starts_with("PctRepaired19") & ends_with("NG")),
         (starts_with("leaks_hu") & ends_with("NG")), 
         (starts_with("REPleaks_hu") & ends_with("NG")),
         (starts_with("ALLleaks_hu") & ends_with("NG"))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmNG = weighted.mean(x = leaks_sqkmNG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmREPNG = weighted.mean(x = REPleaks_sqkmNG, 
                                             w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLNG = weighted.mean(x = AllLeaks2019_sqkmNG, 
                                             w = Pop, na.rm = TRUE),
            wLeaksPerHUNG = weighted.mean(x = leaks_huNG, w = Pop, 
                                        na.rm = T),
            wREPLeaksPerHUNG = weighted.mean(x = REPleaks_huNG, 
                                                     w = Pop, na.rm = T),
            wALLLeaksPerHUNG = weighted.mean(x = AllLeaks2019_huNG, 
                                                     w = Pop, na.rm = T),
            wPctRepaired19NG = weighted.mean(x = PctRepaired19NG, 
                                                     w = Pop, na.rm = T),
            wDaysToRepairAvgNG = weighted.mean(x = DaysToRepairAvgNG, 
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

# Eversource Energy
ppLeakDensityEV <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Eversource") | 
           str_detect(GAS, "Energy$")) %>% # limit to BGs in Eversource svc area
  mutate(leaks_sqkmEV = `Eversource Energy_19unrepaired`/area_sqkm,
         REPleaks_sqkmEV = `Eversource Energy_19repaired`/area_sqkm,
         AllLeaks2019_sqkmEV = (`Eversource Energy_19unrepaired` + 
                                  `Eversource Energy_19repaired`)/area_sqkm,
         leaks_huEV = if_else(total_occ_unitsE == 0, 0, 
                              `Eversource Energy_19unrepaired`/
                                total_occ_unitsE),
         REPleaks_huEV = if_else(total_occ_unitsE == 0, 0,
                                 `Eversource Energy_19repaired`/
                                   total_occ_unitsE),
         AllLeaks2019_huEV = if_else(total_occ_unitsE == 0, 0,
                                     (`Eversource Energy_19unrepaired` + 
                                        `Eversource Energy_19repaired`)
                                     /total_occ_unitsE),
         PctRepaired19EV = `Eversource Energy_19repaired`/ 
           (`Eversource Energy_19unrepaired` + 
              `Eversource Energy_19repaired`)*100,
         DaysToRepairAvgEV = `Eversource Energy_19repairedDaysAvg`) %>% 
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
         ends_with("unitsE"), (starts_with("leaks_") & ends_with("EV")), 
         (starts_with("AllLeaks") & ends_with("EV")), 
         (starts_with("REPleaks_") & ends_with("EV")), 
         (starts_with("DaystoRepairAvg") & ends_with("EV")), 
         (starts_with("PctRepaired19") & ends_with("EV")),
         (starts_with("leaks_hu") & ends_with("EV")), 
         (starts_with("REPleaks_hu") & ends_with("EV")),
         (starts_with("ALLleaks_hu") & ends_with("EV"))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmEV = weighted.mean(x = leaks_sqkmEV, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmREPEV = weighted.mean(x = REPleaks_sqkmEV, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLEV = weighted.mean(x = AllLeaks2019_sqkmEV, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerHUEV = weighted.mean(x = leaks_huEV, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHUEV = weighted.mean(x = REPleaks_huEV, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUEV = weighted.mean(x = AllLeaks2019_huEV, 
                                             w = Pop, na.rm = T),
            wPctRepaired19EV = weighted.mean(x = PctRepaired19EV, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgEV = weighted.mean(x = DaysToRepairAvgEV, 
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

# Columbia Gas
ppLeakDensityCG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Columbia") | 
           str_detect(GAS, "Columbia Gas$")) %>% # limit to BGs in CG svc area
  mutate(leaks_sqkmCG = `Columbia Gas_19unrepaired`/area_sqkm,
         REPleaks_sqkmCG = `Columbia Gas_19repaired`/area_sqkm,
         AllLeaks2019_sqkmCG = (`Columbia Gas_19unrepaired` + 
                                  `Columbia Gas_19repaired`)/area_sqkm,
         leaks_huCG = if_else(total_occ_unitsE == 0, 0, 
                              `Columbia Gas_19unrepaired`/
                                total_occ_unitsE),
         REPleaks_huCG = if_else(total_occ_unitsE == 0, 0,
                                 `Columbia Gas_19repaired`/
                                   total_occ_unitsE),
         AllLeaks2019_huCG = if_else(total_occ_unitsE == 0, 0,
                                     (`Columbia Gas_19unrepaired` + 
                                        `Columbia Gas_19repaired`)
                                     /total_occ_unitsE),
         PctRepaired19CG = `Columbia Gas_19repaired`/ 
           (`Columbia Gas_19unrepaired` + 
              `Columbia Gas_19repaired`)*100,
         DaysToRepairAvgCG = `Columbia Gas_19repairedDaysAvg`) %>%
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
         ends_with("unitsE"), (starts_with("leaks_") & ends_with("CG")), 
         (starts_with("AllLeaks") & ends_with("CG")), 
         (starts_with("REPleaks_") & ends_with("CG")), 
         (starts_with("DaystoRepairAvg") & ends_with("CG")), 
         (starts_with("PctRepaired19") & ends_with("CG")),
         (starts_with("leaks_hu") & ends_with("CG")), 
         (starts_with("REPleaks_hu") & ends_with("CG")),
         (starts_with("ALLleaks_hu") & ends_with("CG"))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmCG = weighted.mean(x = leaks_sqkmCG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmREPCG = weighted.mean(x = REPleaks_sqkmCG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLCG = weighted.mean(x = AllLeaks2019_sqkmCG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerHUCG = weighted.mean(x = leaks_huCG, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHUCG = weighted.mean(x = REPleaks_huCG, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUCG = weighted.mean(x = AllLeaks2019_huCG, 
                                             w = Pop, na.rm = T),
            wPctRepaired19CG = weighted.mean(x = PctRepaired19CG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgCG = weighted.mean(x = DaysToRepairAvgCG, 
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

# Fitchburg Gas aka Unitil
ppLeakDensityFG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Unitil") | 
           str_detect(GAS, "Unitil$")) %>% # limit to BGs in FG svc area
  mutate(leaks_sqkmFG = `Fitchburg Gas_19unrepaired`/area_sqkm,
         REPleaks_sqkmFG = `Fitchburg Gas_19repaired`/area_sqkm,
         AllLeaks2019_sqkmFG = (`Fitchburg Gas_19unrepaired` + 
                                  `Fitchburg Gas_19repaired`)/area_sqkm,
         leaks_huFG = if_else(total_occ_unitsE == 0, 0, 
                              `Fitchburg Gas_19unrepaired`/
                                total_occ_unitsE),
         REPleaks_huFG = if_else(total_occ_unitsE == 0, 0,
                                 `Fitchburg Gas_19repaired`/
                                   total_occ_unitsE),
         AllLeaks2019_huFG = if_else(total_occ_unitsE == 0, 0,
                                     (`Fitchburg Gas_19unrepaired` + 
                                        `Fitchburg Gas_19repaired`)
                                     /total_occ_unitsE),
         PctRepaired19FG = `Fitchburg Gas_19repaired`/ 
           (`Fitchburg Gas_19unrepaired` + 
              `Fitchburg Gas_19repaired`)*100,
         DaysToRepairAvgFG = `Fitchburg Gas_19repairedDaysAvg`) %>%
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
         ends_with("unitsE"), (starts_with("leaks_") & ends_with("FG")), 
         (starts_with("AllLeaks") & ends_with("FG")), 
         (starts_with("REPleaks_") & ends_with("FG")), 
         (starts_with("DaystoRepairAvg") & ends_with("FG")), 
         (starts_with("PctRepaired19") & ends_with("FG")),
         (starts_with("leaks_hu") & ends_with("FG")), 
         (starts_with("REPleaks_hu") & ends_with("FG")),
         (starts_with("ALLleaks_hu") & ends_with("FG"))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmFG = weighted.mean(x = leaks_sqkmFG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmREPFG = weighted.mean(x = REPleaks_sqkmFG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLFG = weighted.mean(x = AllLeaks2019_sqkmFG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerHUFG = weighted.mean(x = leaks_huFG, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHUFG = weighted.mean(x = REPleaks_huFG, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUFG = weighted.mean(x = AllLeaks2019_huFG, 
                                             w = Pop, na.rm = T),
            wPctRepaired19FG = weighted.mean(x = PctRepaired19FG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgFG = weighted.mean(x = DaysToRepairAvgFG, 
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

# Liberty Utilities
ppLeakDensityLU <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(GAS == "Liberty Utilities") %>% # limit to BGs in LU svc area
  mutate(leaks_sqkmLU = `Liberty Utilities_19unrepaired`/area_sqkm,
         REPleaks_sqkmLU = `Liberty Utilities_19repaired`/area_sqkm,
         AllLeaks2019_sqkmLU = (`Liberty Utilities_19unrepaired` + 
                                  `Liberty Utilities_19repaired`)/area_sqkm,
         leaks_huLU = if_else(total_occ_unitsE == 0, 0, 
                              `Liberty Utilities_19unrepaired`/
                                total_occ_unitsE),
         REPleaks_huLU = if_else(total_occ_unitsE == 0, 0,
                                 `Liberty Utilities_19repaired`/
                                   total_occ_unitsE),
         AllLeaks2019_huLU = if_else(total_occ_unitsE == 0, 0,
                                     (`Liberty Utilities_19unrepaired` + 
                                        `Liberty Utilities_19repaired`)
                                     /total_occ_unitsE),
         PctRepaired19LU = `Liberty Utilities_19repaired`/ 
           (`Liberty Utilities_19unrepaired` + 
              `Liberty Utilities_19repaired`)*100,
         DaysToRepairAvgLU = `Liberty Utilities_19repairedDaysAvg`) %>%
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
         ends_with("unitsE"), (starts_with("leaks_") & ends_with("LU")), 
         (starts_with("AllLeaks") & ends_with("LU")), 
         (starts_with("REPleaks_") & ends_with("LU")), 
         (starts_with("DaystoRepairAvg") & ends_with("LU")), 
         (starts_with("PctRepaired19") & ends_with("LU")),
         (starts_with("leaks_hu") & ends_with("LU")), 
         (starts_with("REPleaks_hu") & ends_with("LU")),
         (starts_with("ALLleaks_hu") & ends_with("LU"))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmLU = weighted.mean(x = leaks_sqkmLU, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmREPLU = weighted.mean(x = REPleaks_sqkmLU, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLLU = weighted.mean(x = AllLeaks2019_sqkmLU, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerHULU = weighted.mean(x = leaks_huLU, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHULU = weighted.mean(x = REPleaks_huLU, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHULU = weighted.mean(x = AllLeaks2019_huLU, 
                                             w = Pop, na.rm = T),
            wPctRepaired19LU = weighted.mean(x = PctRepaired19LU, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgLU = weighted.mean(x = DaysToRepairAvgLU, 
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

# The Berkshire Gas Company 
ppLeakDensityBG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(GAS == "The Berkshire Gas Company") %>% # limit to BGs in BG svc area
  mutate(leaks_sqkmBG = `Berkshire Gas_19unrepaired`/area_sqkm,
         REPleaks_sqkmBG = `Berkshire Gas_19repaired`/area_sqkm,
         AllLeaks2019_sqkmBG = (`Berkshire Gas_19unrepaired` + 
                                  `Berkshire Gas_19repaired`)/area_sqkm,
         leaks_huBG = if_else(total_occ_unitsE == 0, 0, 
                              `Berkshire Gas_19unrepaired`/
                                total_occ_unitsE),
         REPleaks_huBG = if_else(total_occ_unitsE == 0, 0,
                                 `Berkshire Gas_19repaired`/
                                   total_occ_unitsE),
         AllLeaks2019_huBG = if_else(total_occ_unitsE == 0, 0,
                                     (`Berkshire Gas_19unrepaired` + 
                                        `Berkshire Gas_19repaired`)
                                     /total_occ_unitsE),
         PctRepaired19BG = `Berkshire Gas_19repaired`/ 
           (`Berkshire Gas_19unrepaired` + 
              `Berkshire Gas_19repaired`)*100,
         DaysToRepairAvgBG = `Berkshire Gas_19repairedDaysAvg`) %>%
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
         ends_with("unitsE"), (starts_with("leaks_") & ends_with("BG")), 
         (starts_with("AllLeaks") & ends_with("BG")), 
         (starts_with("REPleaks_") & ends_with("BG")), 
         (starts_with("DaystoRepairAvg") & ends_with("BG")), 
         (starts_with("PctRepaired19") & ends_with("BG")),
         (starts_with("leaks_hu") & ends_with("BG")), 
         (starts_with("REPleaks_hu") & ends_with("BG")),
         (starts_with("ALLleaks_hu") & ends_with("BG"))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmBG = weighted.mean(x = leaks_sqkmBG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmREPBG = weighted.mean(x = REPleaks_sqkmBG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLBG = weighted.mean(x = AllLeaks2019_sqkmBG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerHUBG = weighted.mean(x = leaks_huBG, w = Pop, 
                                          na.rm = T),
            wREPLeaksPerHUBG = weighted.mean(x = REPleaks_huBG, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUBG = weighted.mean(x = AllLeaks2019_huBG, 
                                             w = Pop, na.rm = T),
            wPctRepaired19BG = weighted.mean(x = PctRepaired19BG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgBG = weighted.mean(x = DaysToRepairAvgBG, 
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


# Join the utility df together
ppLeakDensityJoinedU <- list(ppLeakDensityBG,
                               ppLeakDensityCG,
                               ppLeakDensityEV,
                               ppLeakDensityFG,
                               ppLeakDensityLU,
                               ppLeakDensityNG) %>% 
  reduce(left_join, by = "Group")


# Facet wrap by utility for unrepaired leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerSqKmBG, wLeaksPerSqKmCG, wLeaksPerSqKmEV, 
                 wLeaksPerSqKmFG, wLeaksPerSqKmLU, wLeaksPerSqKmNG), 
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
       title = "Piority Populations and Unrepaired Gas Leaks by Utility for 2019 across Massachusetts")

ggsave("Images/LeaksPPbyUtility_blkgrp.png")

# Facet wrap by utility for repaired leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerSqKmREPBG, wLeaksPerSqKmREPCG, wLeaksPerSqKmREPEV, 
                 wLeaksPerSqKmREPFG, wLeaksPerSqKmREPLU, wLeaksPerSqKmREPNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wLeaksPerSqKmREPBG" = "Berkshire Gas",
                          "wLeaksPerSqKmREPCG" = "Columbia Gas",
                          "wLeaksPerSqKmREPEV" = "Eversource Energy",
                          "wLeaksPerSqKmREPFG" = "Unitil/Fitchburg Gas",
                          "wLeaksPerSqKmREPLU" = "Liberty Utilities",
                          "wLeaksPerSqKmREPNG" = "National Grid"),
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
       title = "Piority Populations and Repaired Gas Leaks by Utility for 2019 across Massachusetts")

ggsave("Images/LeaksPPbyUtilityREP_blkgrp.png")

# Facet wrap by utility for all (repaired + unrepaired) leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerSqKmALLBG, wLeaksPerSqKmALLCG, wLeaksPerSqKmALLEV, 
                 wLeaksPerSqKmALLFG, wLeaksPerSqKmALLLU, wLeaksPerSqKmALLNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wLeaksPerSqKmALLBG" = "Berkshire Gas",
                          "wLeaksPerSqKmALLCG" = "Columbia Gas",
                          "wLeaksPerSqKmALLEV" = "Eversource Energy",
                          "wLeaksPerSqKmALLFG" = "Unitil/Fitchburg Gas",
                          "wLeaksPerSqKmALLLU" = "Liberty Utilities",
                          "wLeaksPerSqKmALLNG" = "National Grid"),
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
       title = "Piority Populations and All Repaired and Unrepaired Gas Leaks by Utility for 2019 across Massachusetts")

ggsave("Images/LeaksPPbyUtilityALL_blkgrp.png")

# Facet wrap by utility for all (repaired + unrepaired) leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wALLLeaksPerHUBG, wALLLeaksPerHUCG, wALLLeaksPerHUEV, 
                 wALLLeaksPerHUFG, wALLLeaksPerHULU, wALLLeaksPerHUNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wALLLeaksPerHUBG" = "Berkshire Gas",
                          "wALLLeaksPerHUCG" = "Columbia Gas",
                          "wALLLeaksPerHUEV" = "Eversource Energy",
                          "wALLLeaksPerHUFG" = "Unitil/Fitchburg Gas",
                          "wALLLeaksPerHULU" = "Liberty Utilities",
                          "wALLLeaksPerHUNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leaks per occupied housing unity by Census Block Group",
       title = "Piority Populations and All Repaired and Unrepaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")

ggsave("Images/LeaksPPbyUtilityALL_HU_blkgrp.png")

# Facet wrap by utility for percentage of leaks repaired
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wPctRepaired19BG, wPctRepaired19CG, wPctRepaired19EV, 
                 wPctRepaired19FG, wPctRepaired19LU, wPctRepaired19NG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wPctRepaired19BG" = "Berkshire Gas",
                          "wPctRepaired19CG" = "Columbia Gas",
                          "wPctRepaired19EV" = "Eversource Energy",
                          "wPctRepaired19FG" = "Unitil/Fitchburg Gas",
                          "wPctRepaired19LU" = "Liberty Utilities",
                          "wPctRepaired19NG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean percentage of leaks repaired in 2019 by Census Block Group",
       title = "Piority Populations Percent Repaired Gas Leaks in 2019 by Utility across Massachusetts")

ggsave("Images/LeaksPPbyUtilityPctFix_blkgrp.png")

# Facet wrap by utility for average time to repair leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wDaysToRepairAvgBG, wDaysToRepairAvgCG, wDaysToRepairAvgEV, 
                 wDaysToRepairAvgFG, wDaysToRepairAvgLU, wDaysToRepairAvgNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY17" = "MA Minority EJ2017",
                        "MA_MINORITY21" = "MA Minority EJ2021",
                        "MA_INCOME17" = "MA Low Income EJ2017",
                        "MA_INCOME21" = "MA Low Income EJ2021")) %>%
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wDaysToRepairAvgBG" = "Berkshire Gas",
                          "wDaysToRepairAvgCG" = "Columbia Gas",
                          "wDaysToRepairAvgEV" = "Eversource Energy",
                          "wDaysToRepairAvgFG" = "Unitil/Fitchburg Gas",
                          "wDaysToRepairAvgLU" = "Liberty Utilities",
                          "wDaysToRepairAvgNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean percentage of leak repair time(days) by Census Block Group",
       title = "Piority Populations and Average Leak Repair Time in 2019 by Utility across Massachusetts")

ggsave("Images/LeaksPPbyUtilityTime_blkgrp.png")


# conduct a weighted t-test to determine if differences are statistically significant
# library(weights)
weights::wtd.t.test(x = ma_blkgrps18$leaks_sqkm, y = ma_blkgrps18$leaks_sqkm, 
           weight = ma_blkgrps18$nhwhitepop_E, 
           weighty = ma_blkgrps18$nhblackpop_E, 
           bootse = TRUE, bootp = TRUE)

weights::wtd.t.test(x = ma_blkgrps18$PctRepaired19, 
                    y = ma_blkgrps18$PctRepaired19, 
           weight = ma_blkgrps18$nhwhitepop_E, 
           weighty = ma_blkgrps18$eng_limitE, 
           bootse = TRUE, bootp = TRUE)




# correlation matrix between leak frequency and demographic group