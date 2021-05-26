# Analysis of relationship between gas leaks and demographics

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(ggplot2)
library(foreign) # for reading in dbf
library(tidytext) # for reordering within ggplot2 facets
library(kableExtra)
library(sp)
library(spdep)

# Load demographic and gas leaks data
load("Data/Demographics.rds")
load("Data/HEET2019Leaksv2.rds")

# Harmonize CRS
repaired2019 <- st_transform(repaired2019, crs = st_crs(ma_blkgrps))
unrepaired2019 <- st_transform(unrepaired2019, crs = st_crs(ma_blkgrps))

# Load natural gas utility service areas dbf from MassGIS and join to MassGIS towns layer
ng_dbf <- read.dbf("Data/pubutil/TOWNS_POLY_UTILITIES.dbf")

ng_service_areas <- st_read(dsn = "Data/townssurvey_shp",
                            layer = "TOWNSSURVEY_POLYM") %>% 
  select(-TOWN) %>% 
  left_join(., ng_dbf, by = "TOWN_ID") %>% 
  select(TOWN, GAS, GAS_LABEL) %>%  
  st_transform(., crs = st_crs(ma_tracts)) %>% 
  st_make_valid()

# assign natural gas territories to tracts and keep only those tracts in utility territories for which we have leaks data
ma_tracts <- ng_service_areas %>% 
  select(-TOWN) %>% 
  st_join(ma_tracts, ., largest = TRUE) %>% 
  filter(!GAS %in% c("Blackstone Gas Company", 
                     "Middleboro Municipal Gas and Electric Department",
                     "Westfield Gas and Electric Department",
                     "Wakefield Municipal Gas and Light Department",
                     "Holyoke Gas and Electric Department",
                     "No gas service")) %>% 
  filter(!st_is_empty(.))


# assign tract data to each unrepaired leak it intersects. 
# join tract info to each leak that falls within it
unrepaired2019_with_tract <- ma_tracts %>% 
  select(GEOID) %>%
  st_join(unrepaired2019, ., largest = TRUE)
# do same for repaired leaks
repaired2019_with_tract <- ma_tracts %>% 
  select(GEOID) %>%
  st_join(repaired2019, ., largest = TRUE)

# break them out by leak class
unrepaired2019_with_tractC1 <- unrepaired2019_with_tract %>% 
  filter(Class == "1")

unrepaired2019_with_tractC2 <- unrepaired2019_with_tract %>% 
  filter(Class == "2")

unrepaired2019_with_tractC3 <- unrepaired2019_with_tract %>% 
  filter(Class == "3")
# do same for repaired
repaired2019_with_tractC1 <- repaired2019_with_tract %>% 
  filter(Class == "1")

repaired2019_with_tractC2 <- repaired2019_with_tract %>% 
  filter(Class == "2")

repaired2019_with_tractC3 <- repaired2019_with_tract %>% 
  filter(Class == "3")

# group by GEOID to get a simple count of all leaks within each tract, as well as avg leak age in days
unrepaired2019_by_tract <- unrepaired2019_with_tract %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019total = n(),
            LeakAgeDaysAvg = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMed = median(LeakAgeDays, na.rm = TRUE))

unrepaired2019_by_tractC1 <- unrepaired2019_with_tractC1 %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019totalC1 = n(),
            LeakAgeDaysAvgC1 = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMedC1 = median(LeakAgeDays, na.rm = TRUE))

unrepaired2019_by_tractC2 <- unrepaired2019_with_tractC2 %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019totalC2 = n(),
            LeakAgeDaysAvgC2 = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMedC2 = median(LeakAgeDays, na.rm = TRUE))

unrepaired2019_by_tractC3 <- unrepaired2019_with_tractC3 %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019totalC3 = n(),
            LeakAgeDaysAvgC3 = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMedC3 = median(LeakAgeDays, na.rm = TRUE))
# do the same for repaired, but also compute avg days to repair
repaired2019_by_tract <- repaired2019_with_tract %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019total = n(), 
            DaysToRepairAvg = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMed = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_tractC1 <- repaired2019_with_tractC1 %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019totalC1 = n(), 
            DaysToRepairAvgC1 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC1 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_tractC2 <- repaired2019_with_tractC2 %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019totalC2 = n(), 
            DaysToRepairAvgC2 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC2 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_tractC3 <- repaired2019_with_tractC3 %>% 
  group_by(GEOID) %>% 
  summarize(repaired2019totalC3 = n(), 
            DaysToRepairAvgC3 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC3 = median(DaysToRepair, na.rm = TRUE))

# group by two columns to get the count of leaks by unique combinations of Utility and GEOID so that we know which utility is responsible for which leaks in each tract.
unrepaired2019_by_utility <- unrepaired2019_with_tract %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019total = n(),
            LeakAgeDaysAvg = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMed = median(LeakAgeDays, na.rm = TRUE))

unrepaired2019_by_utilityC1 <- unrepaired2019_with_tractC1 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019totalC1 = n(),
            LeakAgeDaysAvgC1 = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMedC1 = median(LeakAgeDays, na.rm = TRUE))

unrepaired2019_by_utilityC2 <- unrepaired2019_with_tractC2 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019totalC2 = n(),
            LeakAgeDaysAvgC2 = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMedC2 = median(LeakAgeDays, na.rm = TRUE))

unrepaired2019_by_utilityC3 <- unrepaired2019_with_tractC3 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(unrepaired2019totalC3 = n(),
            LeakAgeDaysAvgC3 = mean(LeakAgeDays, na.rm = TRUE),
            LeakAgeDaysMedC3 = median(LeakAgeDays, na.rm = TRUE))
# do the same for repaired
repaired2019_by_utility <- repaired2019_with_tract %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019total = n(), 
            DaysToRepairAvg = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMed = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_utilityC1 <- repaired2019_with_tractC1 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019totalC1 = n(), 
            DaysToRepairAvgC1 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC1 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_utilityC2 <- repaired2019_with_tractC2 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019totalC2 = n(), 
            DaysToRepairAvgC2 = mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC2 = median(DaysToRepair, na.rm = TRUE))

repaired2019_by_utilityC3 <- repaired2019_with_tractC3 %>% 
  as.data.frame() %>% 
  group_by(Utility, GEOID) %>% 
  summarise(repaired2019totalC3 = n(), 
            DaysToRepairAvgC3= mean(DaysToRepair, na.rm = TRUE),
            DaysToRepairMedC3 = median(DaysToRepair, na.rm = TRUE))


# reshape df to get separate columns of leak counts by utility, with one row for every GEOID that has at least one leak from some utility, repeating for avg leak age
unrepaired2019_by_utility2 <- unrepaired2019_by_utility %>% 
  select(-starts_with("LeakAgeDays")) %>% 
  pivot_wider(., names_from = Utility, values_from = unrepaired2019total,
              names_glue = "{Utility}_19unrepaired")

unrepaired2019_by_utility2.b <- unrepaired2019_by_utility %>% 
  select(GEOID, Utility, LeakAgeDaysAvg) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysAvg,
              names_glue = "{Utility}_19unrepairedDaysAvg")

unrepaired2019_by_utility2.c <- unrepaired2019_by_utility %>% 
  select(GEOID, Utility, LeakAgeDaysMed) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysMed,
              names_glue = "{Utility}_19unrepairedDaysMed")

unrepaired2019_by_utility2C1 <- unrepaired2019_by_utilityC1 %>% 
  select(-starts_with("LeakAgeDays")) %>%
  pivot_wider(., names_from = Utility, values_from = unrepaired2019totalC1,
              names_glue = "{Utility}_19unrepairedC1")

unrepaired2019_by_utility2C1.b <- unrepaired2019_by_utilityC1 %>% 
  select(GEOID, Utility, LeakAgeDaysAvgC1) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysAvgC1,
              names_glue = "{Utility}_19unrepairedDaysAvgC1")

unrepaired2019_by_utility2C1.c <- unrepaired2019_by_utilityC1 %>% 
  select(GEOID, Utility, LeakAgeDaysMedC1) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysMedC1,
              names_glue = "{Utility}_19unrepairedDaysMedC1")

unrepaired2019_by_utility2C2 <- unrepaired2019_by_utilityC2 %>% 
  select(-starts_with("LeakAgeDays")) %>%
  pivot_wider(., names_from = Utility, values_from = unrepaired2019totalC2,
              names_glue = "{Utility}_19unrepairedC2")

unrepaired2019_by_utility2C2.b <- unrepaired2019_by_utilityC2 %>% 
  select(GEOID, Utility, LeakAgeDaysAvgC2) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysAvgC2,
              names_glue = "{Utility}_19unrepairedDaysAvgC2")

unrepaired2019_by_utility2C2.c <- unrepaired2019_by_utilityC2 %>% 
  select(GEOID, Utility, LeakAgeDaysMedC2) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysMedC2,
              names_glue = "{Utility}_19unrepairedDaysMedC2")

unrepaired2019_by_utility2C3 <- unrepaired2019_by_utilityC3 %>% 
  select(-starts_with("LeakAgeDays")) %>%
  pivot_wider(., names_from = Utility, values_from = unrepaired2019totalC3,
              names_glue = "{Utility}_19unrepairedC3")

unrepaired2019_by_utility2C3.b <- unrepaired2019_by_utilityC3 %>% 
  select(GEOID, Utility, LeakAgeDaysAvgC3) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysAvgC3,
              names_glue = "{Utility}_19unrepairedDaysAvgC3")

unrepaired2019_by_utility2C3.c <- unrepaired2019_by_utilityC3 %>% 
  select(GEOID, Utility, LeakAgeDaysMedC3) %>%
  pivot_wider(., names_from = Utility, 
              values_from = LeakAgeDaysMedC3,
              names_glue = "{Utility}_19unrepairedDaysMedC3")
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



# join total leak counts to the tract demographics
ma_tracts <- lapply(list(unrepaired2019_by_tract,
                            unrepaired2019_by_tractC1,
                            unrepaired2019_by_tractC2,
                            unrepaired2019_by_tractC3), function(x){
                              as.data.frame(x) %>% 
                                select(-geometry)}) %>% 
  reduce(full_join, by = "GEOID") %>% 
  # distinct(GEOID, .keep_all = TRUE) %>% # get rid of duplicate rows
  left_join(ma_tracts, ., by = "GEOID") %>% 
  mutate(area_sqkm = as.numeric(st_area(.)/10^6),
         leaks_sqkm = if_else(unrepaired2019total > 0,
                              unrepaired2019total/area_sqkm, 0),
         leaks_sqkmC1 = if_else(unrepaired2019totalC1 > 0,
                                unrepaired2019totalC1/area_sqkm, 0),
         leaks_sqkmC2 = if_else(unrepaired2019totalC2 > 0,
                                unrepaired2019totalC2/area_sqkm, 0),
         leaks_sqkmC3 = if_else(unrepaired2019totalC3 > 0,
                                unrepaired2019totalC3/area_sqkm, 0))

# join leak counts by utility to the tract demographics
ma_tracts <- list(ma_tracts, 
                     unrepaired2019_by_utility2,
                     unrepaired2019_by_utility2.b,
                     unrepaired2019_by_utility2.c,
                     unrepaired2019_by_utility2C1,
                     unrepaired2019_by_utility2C1.b,
                     unrepaired2019_by_utility2C1.c,
                     unrepaired2019_by_utility2C2,
                     unrepaired2019_by_utility2C2.b,
                     unrepaired2019_by_utility2C2.c,
                     unrepaired2019_by_utility2C3,
                     unrepaired2019_by_utility2C3.b,
                     unrepaired2019_by_utility2C3.c) %>% 
  reduce(left_join, by = "GEOID") %>% 
  mutate(
    across(ends_with("total")|ends_with("totalC1")|ends_with("totalC2")|
             ends_with("totalC3")|ends_with("unrepaired")|
             ends_with("unrepairedC1")|ends_with("unrepairedC2")|
             ends_with("unrepairedC3")|starts_with("leaks_"),
           ~replace_na(., 0)))

# repeat for repaired leaks
# join total leak counts to the tract demographics
ma_tracts <- lapply(list(repaired2019_by_tract,
                            repaired2019_by_tractC1,
                            repaired2019_by_tractC2,
                            repaired2019_by_tractC3), function(x){
                              as.data.frame(x) %>% 
                                select(-geometry)}) %>% 
  reduce(full_join, by = "GEOID") %>% 
  left_join(ma_tracts, ., by = "GEOID") %>% 
  mutate(REPleaks_sqkm = if_else(repaired2019total > 0,
                                 repaired2019total/area_sqkm, 0),
         REPleaks_sqkmC1 = if_else(repaired2019totalC1 > 0,
                                   repaired2019totalC1/area_sqkm, 0),
         REPleaks_sqkmC2 = if_else(repaired2019totalC2 > 0,
                                   repaired2019totalC2/area_sqkm, 0),
         REPleaks_sqkmC3 = if_else(repaired2019totalC3 > 0,
                                   repaired2019totalC3/area_sqkm, 0))

# join leak counts by utility to the tract demographics
ma_tracts <- list(ma_tracts, 
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
             `Berkshire Gas_19repaired`:`National Grid_19repaired`,
             `Berkshire Gas_19repairedC1`:`National Grid_19repairedC1`,
             `Berkshire Gas_19repairedC2`:`National Grid_19repairedC2`,
             `Berkshire Gas_19repairedC3`:`National Grid_19repairedC3`), 
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


# save output to save time
saveRDS(ma_tracts, file = "Data/ma_tracts2019.Rds")

# load output
ma_tracts <- readRDS("Data/ma_tracts2019.Rds")


# comparison of population-weighted leak frequency and density by demographic group for priority populations, restricting to areas served by gas utilities for which we have leak data
ppLeakDensity <-  ma_tracts %>% 
  as.data.frame() %>% 
  select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
         eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
         starts_with("leaks_"), 
         starts_with("AllLeaks"), starts_with("REPleaks_"), 
         starts_with("LeakAgeDaysAvg"),
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
            wLeakAgeDaysAvg = weighted.mean(x = LeakAgeDaysAvg,
                                            w = Pop, na.rm = T),
            wLeakAgeDaysAvgC1 = weighted.mean(x = LeakAgeDaysAvgC1,
                                              w = Pop, na.rm = T),
            wLeakAgeDaysAvgC2 = weighted.mean(x = LeakAgeDaysAvgC2,
                                              w = Pop, na.rm = T),
            wLeakAgeDaysAvgC3 = weighted.mean(x = LeakAgeDaysAvgC3,
                                              w = Pop, na.rm = T),
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "disabledOver18E" = "Disabled Adults",
                        "house_burdened_E" = "Housing Burdened"))

ppLeakDensityUC <-  ma_tracts %>% 
  as.data.frame() %>% 
  select(ends_with("_UC") & (starts_with("nh")), 
         hisppop_UC, minority_UC, totalpop_UC, eng_hh_UC, under5E_UC, 
         over64E_UC, eng_limitE_UC, num2povE_UC, lthsE_UC, total_occ_units_UC, 
         renter_occ_units_UC, disabledOver18E_UC, house_burdened_UC, 
         starts_with("leaks_"), 
         starts_with("AllLeaks"), starts_with("REPleaks_"), 
         starts_with("LeakAgeDaysAvg"),
         starts_with("DaystoRepairAvg"), starts_with("PctRepaired19"),
         starts_with("leaks_hu"), starts_with("REPleaks_hu"),
         starts_with("ALLleaks_hu")) %>% 
  pivot_longer(., cols = nhwhitepop_UC:house_burdened_UC, names_to = "Group",                values_to = "Pop", values_drop_na = TRUE) %>% 
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
            wLeakAgeDaysAvg = weighted.mean(x = LeakAgeDaysAvg,
                                            w = Pop, na.rm = T),
            wLeakAgeDaysAvgC1 = weighted.mean(x = LeakAgeDaysAvgC1,
                                              w = Pop, na.rm = T),
            wLeakAgeDaysAvgC2 = weighted.mean(x = LeakAgeDaysAvgC2,
                                              w = Pop, na.rm = T),
            wLeakAgeDaysAvgC3 = weighted.mean(x = LeakAgeDaysAvgC3,
                                              w = Pop, na.rm = T),
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
                        "disabledOver18E_UC" = "Disabled Adults",
                        "house_burdened_UC" = "Housing Burdened"))

ppLeakDensityLC <-  ma_tracts %>% 
  as.data.frame() %>% 
  select(ends_with("_LC") & (starts_with("nh")), 
         hisppop_LC, minority_LC, totalpop_LC, eng_hh_LC, under5E_LC, 
         over64E_LC, eng_limitE_LC, num2povE_LC, lthsE_LC, total_occ_units_LC, 
         renter_occ_units_LC, disabledOver18E_LC, house_burdened_LC, 
         starts_with("leaks_"), 
         starts_with("AllLeaks"), starts_with("REPleaks_"), 
         starts_with("LeakAgeDaysAvg"),
         starts_with("DaystoRepairAvg"), starts_with("PctRepaired19"),
         starts_with("leaks_hu"), starts_with("REPleaks_hu"),
         starts_with("ALLleaks_hu")) %>% 
  pivot_longer(., cols = nhwhitepop_LC:house_burdened_LC, names_to = "Group",                values_to = "Pop", values_drop_na = TRUE) %>% 
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
            wLeakAgeDaysAvg = weighted.mean(x = LeakAgeDaysAvg,
                                            w = Pop, na.rm = T),
            wLeakAgeDaysAvgC1 = weighted.mean(x = LeakAgeDaysAvgC1,
                                              w = Pop, na.rm = T),
            wLeakAgeDaysAvgC2 = weighted.mean(x = LeakAgeDaysAvgC2,
                                              w = Pop, na.rm = T),
            wLeakAgeDaysAvgC3 = weighted.mean(x = LeakAgeDaysAvgC3,
                                              w = Pop, na.rm = T),
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
                        "disabledOver18E_LC" = "Disabled Adults",
                        "house_burdened_LC" = "Housing Burdened"))

# bring df together
ppLeakDensityJoined <- ppLeakDensity %>% 
  # select(-c(wLeaksPerSqKmC1:wLeaksPerSqKmC3)) %>% 
  left_join(., ppLeakDensityLC, by = "Group") %>% 
  left_join(., ppLeakDensityUC, by = "Group")

# save object to save time
saveRDS(ppLeakDensityJoined, file = "Data/ppLeakDensityTR2019.Rds")

# load data
ppLeakDensityJoined <- readRDS("Data/ppLeakDensityTR2019.Rds")


# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensity_pops_df <- ppLeakDensityJoined %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17", "Housing Burdened",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRR = wLeaksPerSqKm/wLeaksPerSqKm[Group == "Total Population"],
         wLeaksRRC1 = wLeaksPerSqKmC1/wLeaksPerSqKmC1[Group == "Total Population"],
         wLeaksRRC2 = wLeaksPerSqKmC2/wLeaksPerSqKmC2[Group == "Total Population"],
         wLeaksRRC3 = wLeaksPerSqKmC3/wLeaksPerSqKmC3[Group == "Total Population"],
         wLeaksRRrepair = wLeaksPerSqKmREP/wLeaksPerSqKmREP[Group == "Total Population"],
         wLeaksRRrepairC1 = wLeaksPerSqKmREPC1/wLeaksPerSqKmREPC1[Group == "Total Population"],
         wLeaksRRrepairC2 = wLeaksPerSqKmREPC2/wLeaksPerSqKmREPC2[Group == "Total Population"],
         wLeaksRRrepairC3 = wLeaksPerSqKmREPC3/wLeaksPerSqKmREPC3[Group == "Total Population"],
         wLeaksRRtotal = wLeaksPerSqKmALL/wLeaksPerSqKmALL[Group == "Total Population"],
         wLeaksRRtotalC1 = wLeaksPerSqKmALLC1/wLeaksPerSqKmALLC1[Group == "Total Population"],
         wLeaksRRtotalC2 = wLeaksPerSqKmALLC2/wLeaksPerSqKmALLC2[Group == "Total Population"],
         wLeaksRRtotalC3 = wLeaksPerSqKmALLC3/wLeaksPerSqKmALLC3[Group == "Total Population"],
         wLeaksPerHURR = wLeaksPerHU/wLeaksPerHU[Group == "Total Population"],
         wLeaksPerHURRC1 = wLeaksPerHUC1/wLeaksPerHUC1[Group == "Total Population"],
         wLeaksPerHURRC2 = wLeaksPerHUC2/wLeaksPerHUC2[Group == "Total Population"],
         wLeaksPerHURRC3 = wLeaksPerHUC3/wLeaksPerHUC3[Group == "Total Population"],
         wREPLeaksPerHURR = wREPLeaksPerHU/wREPLeaksPerHU[Group == "Total Population"],
         wREPLeaksPerHURRC1 = wREPLeaksPerHUC1/wREPLeaksPerHUC1[Group == "Total Population"],
         wREPLeaksPerHURRC2 = wREPLeaksPerHUC2/wREPLeaksPerHUC2[Group == "Total Population"],
         wREPLeaksPerHURRC3 = wREPLeaksPerHUC3/wREPLeaksPerHUC3[Group == "Total Population"],
         wALLLeaksPerHURR = wALLLeaksPerHU/wALLLeaksPerHU[Group == "Total Population"],
         wALLLeaksPerHURRC1 = wALLLeaksPerHUC1/wALLLeaksPerHUC1[Group == "Total Population"],
         wALLLeaksPerHURRC2 = wALLLeaksPerHUC2/wALLLeaksPerHUC2[Group == "Total Population"],
         wALLLeaksPerHURRC3 = wALLLeaksPerHUC3/wALLLeaksPerHUC3[Group == "Total Population"],
         wDaysToRepairAvgRR = wDaysToRepairAvg/wDaysToRepairAvg[Group == "Total Population"],
         wDaysToRepairAvgRRC1 = wDaysToRepairAvgC1/wDaysToRepairAvgC1[Group == "Total Population"],
         wDaysToRepairAvgRRC2 = wDaysToRepairAvgC2/wDaysToRepairAvgC2[Group == "Total Population"],
         wDaysToRepairAvgRRC3 = wDaysToRepairAvgC3/wDaysToRepairAvgC3[Group == "Total Population"],
         wLeakAgeDaysAvgRR = wLeakAgeDaysAvg/wLeakAgeDaysAvg[Group == "Total Population"],
         wLeakAgeDaysAvgRRC1 = wLeakAgeDaysAvgC1/wLeakAgeDaysAvgC1[Group == "Total Population"],
         wLeakAgeDaysAvgRRC2 = wLeakAgeDaysAvgC2/wLeakAgeDaysAvgC2[Group == "Total Population"],
         wLeakAgeDaysAvgRRC3 = wLeakAgeDaysAvgC3/wLeakAgeDaysAvgC3[Group == "Total Population"])

ppLeakDensity_LEH_df <- ppLeakDensityJoined %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRR = wLeaksPerSqKm/wLeaksPerSqKm[Group == "Total Households"],
         wLeaksRRC1 = wLeaksPerSqKmC1/wLeaksPerSqKmC1[Group == "Total Households"],
         wLeaksRRC2 = wLeaksPerSqKmC2/wLeaksPerSqKmC2[Group == "Total Households"],
         wLeaksRRC3 = wLeaksPerSqKmC3/wLeaksPerSqKmC3[Group == "Total Households"],
         wLeaksRRrepair = wLeaksPerSqKmREP/wLeaksPerSqKmREP[Group == "Total Households"],
         wLeaksRRrepairC1 = wLeaksPerSqKmREPC1/wLeaksPerSqKmREPC1[Group == "Total Households"],
         wLeaksRRrepairC2 = wLeaksPerSqKmREPC2/wLeaksPerSqKmREPC2[Group == "Total Households"],
         wLeaksRRrepairC3 = wLeaksPerSqKmREPC3/wLeaksPerSqKmREPC3[Group == "Total Households"],
         wLeaksRRtotal = wLeaksPerSqKmALL/wLeaksPerSqKmALL[Group == "Total Households"],
         wLeaksRRtotalC1 = wLeaksPerSqKmALLC1/wLeaksPerSqKmALLC1[Group == "Total Households"],
         wLeaksRRtotalC2 = wLeaksPerSqKmALLC2/wLeaksPerSqKmALLC2[Group == "Total Households"],
         wLeaksRRtotalC3 = wLeaksPerSqKmALLC3/wLeaksPerSqKmALLC3[Group == "Total Households"],
         wLeaksPerHURR = wLeaksPerHU/wLeaksPerHU[Group == "Total Households"],
         wLeaksPerHURRC1 = wLeaksPerHUC1/wLeaksPerHUC1[Group == "Total Households"],
         wLeaksPerHURRC2 = wLeaksPerHUC2/wLeaksPerHUC2[Group == "Total Households"],
         wLeaksPerHURRC3 = wLeaksPerHUC3/wLeaksPerHUC3[Group == "Total Households"],
         wREPLeaksPerHURR = wREPLeaksPerHU/wREPLeaksPerHU[Group == "Total Households"],
         wREPLeaksPerHURRC1 = wREPLeaksPerHUC1/wREPLeaksPerHUC1[Group == "Total Households"],
         wREPLeaksPerHURRC2 = wREPLeaksPerHUC2/wREPLeaksPerHUC2[Group == "Total Households"],
         wREPLeaksPerHURRC3 = wREPLeaksPerHUC3/wREPLeaksPerHUC3[Group == "Total Households"],
         wALLLeaksPerHURR = wALLLeaksPerHU/wALLLeaksPerHU[Group == "Total Households"],
         wALLLeaksPerHURRC1 = wALLLeaksPerHUC1/wALLLeaksPerHUC1[Group == "Total Households"],
         wALLLeaksPerHURRC2 = wALLLeaksPerHUC2/wALLLeaksPerHUC2[Group == "Total Households"],
         wALLLeaksPerHURRC3 = wALLLeaksPerHUC3/wALLLeaksPerHUC3[Group == "Total Households"],
         wDaysToRepairAvgRR = wDaysToRepairAvg/wDaysToRepairAvg[Group == "Total Households"],
         wDaysToRepairAvgRRC1 = wDaysToRepairAvgC1/wDaysToRepairAvgC1[Group == "Total Households"],
         wDaysToRepairAvgRRC2 = wDaysToRepairAvgC2/wDaysToRepairAvgC2[Group == "Total Households"],
         wDaysToRepairAvgRRC3 = wDaysToRepairAvgC3/wDaysToRepairAvgC3[Group == "Total Households"],
         wLeakAgeDaysAvgRR = wLeakAgeDaysAvg/wLeakAgeDaysAvg[Group == "Total Households"],
         wLeakAgeDaysAvgRRC1 = wLeakAgeDaysAvgC1/wLeakAgeDaysAvgC1[Group == "Total Households"],
         wLeakAgeDaysAvgRRC2 = wLeakAgeDaysAvgC2/wLeakAgeDaysAvgC2[Group == "Total Households"],
         wLeakAgeDaysAvgRRC3 = wLeakAgeDaysAvgC3/wLeakAgeDaysAvgC3[Group == "Total Households"])

ppLeakDensity_renters_df <- ppLeakDensityJoined %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened", 
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRR = wLeaksPerSqKm/wLeaksPerSqKm[Group == "Total Occupied HU"],
         wLeaksRRC1 = wLeaksPerSqKmC1/wLeaksPerSqKmC1[Group == "Total Occupied HU"],
         wLeaksRRC2 = wLeaksPerSqKmC2/wLeaksPerSqKmC2[Group == "Total Occupied HU"],
         wLeaksRRC3 = wLeaksPerSqKmC3/wLeaksPerSqKmC3[Group == "Total Occupied HU"],
         wLeaksRRrepair = wLeaksPerSqKmREP/wLeaksPerSqKmREP[Group == "Total Occupied HU"],
         wLeaksRRrepairC1 = wLeaksPerSqKmREPC1/wLeaksPerSqKmREPC1[Group == "Total Occupied HU"],
         wLeaksRRrepairC2 = wLeaksPerSqKmREPC2/wLeaksPerSqKmREPC2[Group == "Total Occupied HU"],
         wLeaksRRrepairC3 = wLeaksPerSqKmREPC3/wLeaksPerSqKmREPC3[Group == "Total Occupied HU"],
         wLeaksRRtotal = wLeaksPerSqKmALL/wLeaksPerSqKmALL[Group == "Total Occupied HU"],
         wLeaksRRtotalC1 = wLeaksPerSqKmALLC1/wLeaksPerSqKmALLC1[Group == "Total Occupied HU"],
         wLeaksRRtotalC2 = wLeaksPerSqKmALLC2/wLeaksPerSqKmALLC2[Group == "Total Occupied HU"],
         wLeaksRRtotalC3 = wLeaksPerSqKmALLC3/wLeaksPerSqKmALLC3[Group == "Total Occupied HU"],
         wLeaksPerHURR = wLeaksPerHU/wLeaksPerHU[Group == "Total Occupied HU"],
         wLeaksPerHURRC1 = wLeaksPerHUC1/wLeaksPerHUC1[Group == "Total Occupied HU"],
         wLeaksPerHURRC2 = wLeaksPerHUC2/wLeaksPerHUC2[Group == "Total Occupied HU"],
         wLeaksPerHURRC3 = wLeaksPerHUC3/wLeaksPerHUC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURR = wREPLeaksPerHU/wREPLeaksPerHU[Group == "Total Occupied HU"],
         wREPLeaksPerHURRC1 = wREPLeaksPerHUC1/wREPLeaksPerHUC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURRC2 = wREPLeaksPerHUC2/wREPLeaksPerHUC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURRC3 = wREPLeaksPerHUC3/wREPLeaksPerHUC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURR = wALLLeaksPerHU/wALLLeaksPerHU[Group == "Total Occupied HU"],
         wALLLeaksPerHURRC1 = wALLLeaksPerHUC1/wALLLeaksPerHUC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURRC2 = wALLLeaksPerHUC2/wALLLeaksPerHUC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURRC3 = wALLLeaksPerHUC3/wALLLeaksPerHUC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRR = wDaysToRepairAvg/wDaysToRepairAvg[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRC1 = wDaysToRepairAvgC1/wDaysToRepairAvgC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRC2 = wDaysToRepairAvgC2/wDaysToRepairAvgC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRC3 = wDaysToRepairAvgC3/wDaysToRepairAvgC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRR = wLeakAgeDaysAvg/wLeakAgeDaysAvg[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRC1 = wLeakAgeDaysAvgC1/wLeakAgeDaysAvgC1[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRC2 = wLeakAgeDaysAvgC2/wLeakAgeDaysAvgC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRC3 = wLeakAgeDaysAvgC3/wLeakAgeDaysAvgC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensity_df <- rbind(ppLeakDensity_pops_df,
                          ppLeakDensity_LEH_df,
                          ppLeakDensity_renters_df)

# save object
saveRDS(ppLeakDensity_df, file = "Data/ppLeakDensity_df_tracts2019.Rds")

# load object
ppLeakDensity_df <- readRDS("Data/ppLeakDensity_df_tracts.Rds")



##### Compare leak density distribution by utility
### National Grid; includes both NG-Boston and NG-Colonial Gas
ppLeakDensityNG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^National")) %>% # limit to BGs in NG/Colonial svc area
  mutate(leaks_sqkmNG = (`National Grid_19unrepaired`)/area_sqkm,
         leaks_sqkmNGC1 = (`National Grid_19unrepairedC1`)/area_sqkm,
         leaks_sqkmNGC2 = (`National Grid_19unrepairedC2`)/area_sqkm,
         leaks_sqkmNGC3 = (`National Grid_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmNG = (`National Grid_19repaired`)/area_sqkm,
         REPleaks_sqkmNGC1 = (`National Grid_19repairedC1`)/area_sqkm,
         REPleaks_sqkmNGC2 = (`National Grid_19repairedC2`)/area_sqkm,
         REPleaks_sqkmNGC3 = (`National Grid_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmNG = (`National Grid_19unrepaired` + 
                                  `National Grid_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmNGC1 = (`National Grid_19unrepairedC1` + 
                                    `National Grid_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmNGC2 = (`National Grid_19unrepairedC2` + 
                                    `National Grid_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmNGC3 = (`National Grid_19unrepairedC3` + 
                                    `National Grid_19repairedC3`)/area_sqkm,
         leaks_huNG = if_else(total_occ_unitsE == 0, 0, 
                              (`National Grid_19unrepaired`)/total_occ_unitsE),
         leaks_huNGC1 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid_19unrepairedC1`)/total_occ_unitsE),
         leaks_huNGC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid_19unrepairedC2`)/total_occ_unitsE),
         leaks_huNGC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huNG = if_else(total_occ_unitsE == 0, 0,
                                 (`National Grid_19repaired`)/total_occ_unitsE),
         REPleaks_huNGC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid_19repairedC1`)/total_occ_unitsE),
         REPleaks_huNGC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid_19repairedC2`)/total_occ_unitsE),
         REPleaks_huNGC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huNG = if_else(total_occ_unitsE == 0, 0,
                                     (`National Grid_19unrepaired` + 
                                        `National Grid_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huNGC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid_19unrepairedC1` + 
                                          `National Grid_19repairedC1` )/total_occ_unitsE),
         AllLeaks2019_huNGC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid_19unrepairedC2` + 
                                          `National Grid_19repairedC2` )/total_occ_unitsE),
         AllLeaks2019_huNGC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid_19unrepairedC3` + 
                                          `National Grid_19repairedC3` )/total_occ_unitsE),
         PctRepaired19NG = (`National Grid_19repaired`)/ (`National Grid_19unrepaired` + `National Grid_19repaired`)*100) %>% 
  rowwise() %>% 
  mutate(DaysToRepairAvgNG = `National Grid_19repairedDaysAvg`,
         DaysToRepairAvgNGC1 = `National Grid_19repairedDaysAvgC1`,
         DaysToRepairAvgNGC2 = `National Grid_19repairedDaysAvgC2`,
         DaysToRepairAvgNGC3 = `National Grid_19repairedDaysAvgC3`,
         LeakAgeDaysAvgNG = `National Grid_19unrepairedDaysAvg`,
         LeakAgeDaysAvgNGC1 = `National Grid_19unrepairedDaysAvgC1`,
         LeakAgeDaysAvgNGC2 = `National Grid_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgNGC3 = `National Grid_19unrepairedDaysAvgC3`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), (starts_with("leaks_") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("AllLeaks") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))),
         (starts_with("REPleaks_") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))),
         (starts_with("leaks_hu") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3")))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmNG = weighted.mean(x = leaks_sqkmNG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmNGC1 = weighted.mean(x = leaks_sqkmNGC1, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmNGC2 = weighted.mean(x = leaks_sqkmNGC2, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmNGC3 = weighted.mean(x = leaks_sqkmNGC3, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmREPNG = weighted.mean(x = REPleaks_sqkmNG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPNGC1 = weighted.mean(x = REPleaks_sqkmNGC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPNGC2 = weighted.mean(x = REPleaks_sqkmNGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPNGC3 = weighted.mean(x = REPleaks_sqkmNGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLNG = weighted.mean(x = AllLeaks2019_sqkmNG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLNGC1 = weighted.mean(x = AllLeaks2019_sqkmNGC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLNGC2 = weighted.mean(x = AllLeaks2019_sqkmNGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLNGC3 = weighted.mean(x = AllLeaks2019_sqkmNGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgNG = weighted.mean(x = LeakAgeDaysAvgNG,
                                              w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgNGC1 = weighted.mean(x = LeakAgeDaysAvgNGC1,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgNGC2 = weighted.mean(x = LeakAgeDaysAvgNGC2,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgNGC3 = weighted.mean(x = LeakAgeDaysAvgNGC3,
                                                w = Pop, na.rm = TRUE),
            wLeaksPerHUNG = weighted.mean(x = leaks_huNG, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUNGC1 = weighted.mean(x = leaks_huNGC1, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUNGC2 = weighted.mean(x = leaks_huNGC2, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUNGC3 = weighted.mean(x = leaks_huNGC3, w = Pop, 
                                            na.rm = T),
            wREPLeaksPerHUNG = weighted.mean(x = REPleaks_huNG, 
                                             w = Pop, na.rm = T),
            wREPLeaksPerHUNGC1 = weighted.mean(x = REPleaks_huNGC1, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUNGC2 = weighted.mean(x = REPleaks_huNGC2, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUNGC3 = weighted.mean(x = REPleaks_huNGC3, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUNG = weighted.mean(x = AllLeaks2019_huNG, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUNGC1 = weighted.mean(x = AllLeaks2019_huNGC1, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUNGC2 = weighted.mean(x = AllLeaks2019_huNGC2, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUNGC3 = weighted.mean(x = AllLeaks2019_huNGC3, 
                                               w = Pop, na.rm = T),
            wPctRepaired19NG = weighted.mean(x = PctRepaired19NG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgNG = weighted.mean(x = DaysToRepairAvgNG, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgNGC1 = weighted.mean(x = DaysToRepairAvgNGC1, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgNGC2 = weighted.mean(x = DaysToRepairAvgNGC2, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgNGC3 = weighted.mean(x = DaysToRepairAvgNGC3, 
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "disabledOver18E" = "Disabled Adults",
                        "house_burdened_E" = "Housing Burdened"))

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityNG_pops_df <- ppLeakDensityNG %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17", "Housing Burdened",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRRNG = wLeaksPerSqKmNG/wLeaksPerSqKmNG[Group == "Total Population"],
         wLeaksRRNGC1 = wLeaksPerSqKmNGC1/wLeaksPerSqKmNGC1[Group == "Total Population"],
         wLeaksRRNGC2 = wLeaksPerSqKmNGC2/wLeaksPerSqKmNGC2[Group == "Total Population"],
         wLeaksRRNGC3 = wLeaksPerSqKmNGC3/wLeaksPerSqKmNGC3[Group == "Total Population"],
         wLeaksRRrepairNG = wLeaksPerSqKmREPNG/wLeaksPerSqKmREPNG[Group == "Total Population"],
         wLeaksRRrepairNGC1 = wLeaksPerSqKmREPNGC1/wLeaksPerSqKmREPNGC1[Group == "Total Population"],
         wLeaksRRrepairNGC2 = wLeaksPerSqKmREPNGC2/wLeaksPerSqKmREPNGC2[Group == "Total Population"],
         wLeaksRRrepairNGC3 = wLeaksPerSqKmREPNGC3/wLeaksPerSqKmREPNGC3[Group == "Total Population"],
         wLeaksRRtotalNG = wLeaksPerSqKmALLNG/wLeaksPerSqKmALLNG[Group == "Total Population"],
         wLeaksRRtotalNGC1 = wLeaksPerSqKmALLNGC1/wLeaksPerSqKmALLNGC1[Group == "Total Population"],
         wLeaksRRtotalNGC2 = wLeaksPerSqKmALLNGC2/wLeaksPerSqKmALLNGC2[Group == "Total Population"],
         wLeaksRRtotalNGC3 = wLeaksPerSqKmALLNGC3/wLeaksPerSqKmALLNGC3[Group == "Total Population"],
         wLeaksPerHURRNG = wLeaksPerHUNG/wLeaksPerHUNG[Group == "Total Population"],
         wLeaksPerHURRNGC1 = wLeaksPerHUNGC1/wLeaksPerHUNGC1[Group == "Total Population"],
         wLeaksPerHURRNGC2 = wLeaksPerHUNGC2/wLeaksPerHUNGC2[Group == "Total Population"],
         wLeaksPerHURRNGC3 = wLeaksPerHUNGC3/wLeaksPerHUNGC3[Group == "Total Population"],
         wREPLeaksPerHURRNG = wREPLeaksPerHUNG/wREPLeaksPerHUNG[Group == "Total Population"],
         wREPLeaksPerHURRNGC1 = wREPLeaksPerHUNGC1/wREPLeaksPerHUNGC1[Group == "Total Population"],
         wREPLeaksPerHURRNGC2 = wREPLeaksPerHUNGC2/wREPLeaksPerHUNGC2[Group == "Total Population"],
         wREPLeaksPerHURRNGC3 = wREPLeaksPerHUNGC3/wREPLeaksPerHUNGC3[Group == "Total Population"],
         wALLLeaksPerHURRNG = wALLLeaksPerHUNG/wALLLeaksPerHUNG[Group == "Total Population"],
         wALLLeaksPerHURRNGC1 = wALLLeaksPerHUNGC1/wALLLeaksPerHUNGC1[Group == "Total Population"],
         wALLLeaksPerHURRNGC2 = wALLLeaksPerHUNGC2/wALLLeaksPerHUNGC2[Group == "Total Population"],
         wALLLeaksPerHURRNGC3 = wALLLeaksPerHUNGC3/wALLLeaksPerHUNGC3[Group == "Total Population"],
         wDaysToRepairAvgRRNG = wDaysToRepairAvgNG/wDaysToRepairAvgNG[Group == "Total Population"],
         wDaysToRepairAvgRRNGC1 = wDaysToRepairAvgNGC1/wDaysToRepairAvgNGC1[Group == "Total Population"],
         wDaysToRepairAvgRRNGC2 = wDaysToRepairAvgNGC2/wDaysToRepairAvgNGC2[Group == "Total Population"],
         wDaysToRepairAvgRRNGC3 = wDaysToRepairAvgNGC3/wDaysToRepairAvgNGC3[Group == "Total Population"],
         wLeakAgeDaysAvgRRNG = wLeakAgeDaysAvgNG/wLeakAgeDaysAvgNG[Group == "Total Population"],
         wLeakAgeDaysAvgRRNGC1 = wLeakAgeDaysAvgNGC1/wLeakAgeDaysAvgNGC1[Group == "Total Population"],
         wLeakAgeDaysAvgRRNGC2 = wLeakAgeDaysAvgNGC2/wLeakAgeDaysAvgNGC2[Group == "Total Population"],
         wLeakAgeDaysAvgRRNGC3 = wLeakAgeDaysAvgNGC3/wLeakAgeDaysAvgNGC3[Group == "Total Population"])

ppLeakDensityNG_LEH_df <- ppLeakDensityNG %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRRNG = wLeaksPerSqKmNG/wLeaksPerSqKmNG[Group == "Total Households"],
         wLeaksRRNGC1 = wLeaksPerSqKmNGC1/wLeaksPerSqKmNGC1[Group == "Total Households"],
         wLeaksRRNGC2 = wLeaksPerSqKmNGC2/wLeaksPerSqKmNGC2[Group == "Total Households"],
         wLeaksRRNGC3 = wLeaksPerSqKmNGC3/wLeaksPerSqKmNGC3[Group == "Total Households"],
         wLeaksRRrepairNG = wLeaksPerSqKmREPNG/wLeaksPerSqKmREPNG[Group == "Total Households"],
         wLeaksRRrepairNGC1 = wLeaksPerSqKmREPNGC1/wLeaksPerSqKmREPNGC1[Group == "Total Households"],
         wLeaksRRrepairNGC2 = wLeaksPerSqKmREPNGC2/wLeaksPerSqKmREPNGC2[Group == "Total Households"],
         wLeaksRRrepairNGC3 = wLeaksPerSqKmREPNGC3/wLeaksPerSqKmREPNGC3[Group == "Total Households"],
         wLeaksRRtotalNG = wLeaksPerSqKmALLNG/wLeaksPerSqKmALLNG[Group == "Total Households"],
         wLeaksRRtotalNGC1 = wLeaksPerSqKmALLNGC1/wLeaksPerSqKmALLNGC1[Group == "Total Households"],
         wLeaksRRtotalNGC2 = wLeaksPerSqKmALLNGC2/wLeaksPerSqKmALLNGC2[Group == "Total Households"],
         wLeaksRRtotalNGC3 = wLeaksPerSqKmALLNGC3/wLeaksPerSqKmALLNGC3[Group == "Total Households"],
         wLeaksPerHURRNG = wLeaksPerHUNG/wLeaksPerHUNG[Group == "Total Households"],
         wLeaksPerHURRNGC1 = wLeaksPerHUNGC1/wLeaksPerHUNGC1[Group == "Total Households"],
         wLeaksPerHURRNGC2 = wLeaksPerHUNGC2/wLeaksPerHUNGC2[Group == "Total Households"],
         wLeaksPerHURRNGC3 = wLeaksPerHUNGC3/wLeaksPerHUNGC3[Group == "Total Households"],
         wREPLeaksPerHURRNG = wREPLeaksPerHUNG/wREPLeaksPerHUNG[Group == "Total Households"],
         wREPLeaksPerHURRNGC1 = wREPLeaksPerHUNGC1/wREPLeaksPerHUNGC1[Group == "Total Households"],
         wREPLeaksPerHURRNGC2 = wREPLeaksPerHUNGC2/wREPLeaksPerHUNGC2[Group == "Total Households"],
         wREPLeaksPerHURRNGC3 = wREPLeaksPerHUNGC3/wREPLeaksPerHUNGC3[Group == "Total Households"],
         wALLLeaksPerHURRNG = wALLLeaksPerHUNG/wALLLeaksPerHUNG[Group == "Total Households"],
         wALLLeaksPerHURRNGC1 = wALLLeaksPerHUNGC1/wALLLeaksPerHUNGC1[Group == "Total Households"],
         wALLLeaksPerHURRNGC2 = wALLLeaksPerHUNGC2/wALLLeaksPerHUNGC2[Group == "Total Households"],
         wALLLeaksPerHURRNGC3 = wALLLeaksPerHUNGC3/wALLLeaksPerHUNGC3[Group == "Total Households"],
         wDaysToRepairAvgRRNG = wDaysToRepairAvgNG/wDaysToRepairAvgNG[Group == "Total Households"],
         wDaysToRepairAvgRRNGC1 = wDaysToRepairAvgNGC1/wDaysToRepairAvgNGC1[Group == "Total Households"],
         wDaysToRepairAvgRRNGC2 = wDaysToRepairAvgNGC2/wDaysToRepairAvgNGC2[Group == "Total Households"],
         wDaysToRepairAvgRRNGC3 = wDaysToRepairAvgNGC3/wDaysToRepairAvgNGC3[Group == "Total Households"],
         wLeakAgeDaysAvgRRNG = wLeakAgeDaysAvgNG/wLeakAgeDaysAvgNG[Group == "Total Households"],
         wLeakAgeDaysAvgRRNGC1 = wLeakAgeDaysAvgNGC1/wLeakAgeDaysAvgNGC1[Group == "Total Households"],
         wLeakAgeDaysAvgRRNGC2 = wLeakAgeDaysAvgNGC2/wLeakAgeDaysAvgNGC2[Group == "Total Households"],
         wLeakAgeDaysAvgRRNGC3 = wLeakAgeDaysAvgNGC3/wLeakAgeDaysAvgNGC3[Group == "Total Households"])

ppLeakDensityNG_renters_df <- ppLeakDensityNG %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened", 
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRRNG = wLeaksPerSqKmNG/wLeaksPerSqKmNG[Group == "Total Occupied HU"],
         wLeaksRRNGC1 = wLeaksPerSqKmNGC1/wLeaksPerSqKmNGC1[Group == "Total Occupied HU"],
         wLeaksRRNGC2 = wLeaksPerSqKmNGC2/wLeaksPerSqKmNGC2[Group == "Total Occupied HU"],
         wLeaksRRNGC3 = wLeaksPerSqKmNGC3/wLeaksPerSqKmNGC3[Group == "Total Occupied HU"],
         wLeaksRRrepairNG = wLeaksPerSqKmREPNG/wLeaksPerSqKmREPNG[Group == "Total Occupied HU"],
         wLeaksRRrepairNGC1 = wLeaksPerSqKmREPNGC1/wLeaksPerSqKmREPNGC1[Group == "Total Occupied HU"],
         wLeaksRRrepairNGC2 = wLeaksPerSqKmREPNGC2/wLeaksPerSqKmREPNGC2[Group == "Total Occupied HU"],
         wLeaksRRrepairNGC3 = wLeaksPerSqKmREPNGC3/wLeaksPerSqKmREPNGC3[Group == "Total Occupied HU"],
         wLeaksRRtotalNG = wLeaksPerSqKmALLNG/wLeaksPerSqKmALLNG[Group == "Total Occupied HU"],
         wLeaksRRtotalNGC1 = wLeaksPerSqKmALLNGC1/wLeaksPerSqKmALLNGC1[Group == "Total Occupied HU"],
         wLeaksRRtotalNGC2 = wLeaksPerSqKmALLNGC2/wLeaksPerSqKmALLNGC2[Group == "Total Occupied HU"],
         wLeaksRRtotalNGC3 = wLeaksPerSqKmALLNGC3/wLeaksPerSqKmALLNGC3[Group == "Total Occupied HU"],
         wLeaksPerHURRNG = wLeaksPerHUNG/wLeaksPerHUNG[Group == "Total Occupied HU"],
         wLeaksPerHURRNGC1 = wLeaksPerHUNGC1/wLeaksPerHUNGC1[Group == "Total Occupied HU"],
         wLeaksPerHURRNGC2 = wLeaksPerHUNGC2/wLeaksPerHUNGC2[Group == "Total Occupied HU"],
         wLeaksPerHURRNGC3 = wLeaksPerHUNGC3/wLeaksPerHUNGC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURRNG = wREPLeaksPerHUNG/wREPLeaksPerHUNG[Group == "Total Occupied HU"],
         wREPLeaksPerHURRNGC1 = wREPLeaksPerHUNGC1/wREPLeaksPerHUNGC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURRNGC2 = wREPLeaksPerHUNGC2/wREPLeaksPerHUNGC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURRNGC3 = wREPLeaksPerHUNGC3/wREPLeaksPerHUNGC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURRNG = wALLLeaksPerHUNG/wALLLeaksPerHUNG[Group == "Total Occupied HU"],
         wALLLeaksPerHURRNGC1 = wALLLeaksPerHUNGC1/wALLLeaksPerHUNGC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURRNGC2 = wALLLeaksPerHUNGC2/wALLLeaksPerHUNGC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURRNGC3 = wALLLeaksPerHUNGC3/wALLLeaksPerHUNGC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRNG = wDaysToRepairAvgNG/wDaysToRepairAvgNG[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRNGC1 = wDaysToRepairAvgNGC1/wDaysToRepairAvgNGC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRNGC2 = wDaysToRepairAvgNGC2/wDaysToRepairAvgNGC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRNGC3 = wDaysToRepairAvgNGC3/wDaysToRepairAvgNGC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRNG = wLeakAgeDaysAvgNG/wLeakAgeDaysAvgNG[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRNGC1 = wLeakAgeDaysAvgNGC1/wLeakAgeDaysAvgNGC1[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRNGC2 = wLeakAgeDaysAvgNGC2/wLeakAgeDaysAvgNGC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRNGC3 = wLeakAgeDaysAvgNGC3/wLeakAgeDaysAvgNGC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensityNG_df <- rbind(ppLeakDensityNG_pops_df,
                            ppLeakDensityNG_LEH_df,
                            ppLeakDensityNG_renters_df)


### Eversource
ppLeakDensityEV <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Eversource") | 
           GAS == "Energy$") %>% # limit to BGs in EV svc area
  mutate(leaks_sqkmEV = (`Eversource_19unrepaired`)/area_sqkm,
         # leaks_sqkmEVC1 = (`Eversource_19unrepairedC1`)/area_sqkm,
         leaks_sqkmEVC1 = 0,
         leaks_sqkmEVC2 = (`Eversource_19unrepairedC2`)/area_sqkm,
         leaks_sqkmEVC3 = (`Eversource_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmEV = (`Eversource_19repaired`)/area_sqkm,
         REPleaks_sqkmEVC1 = (`Eversource_19repairedC1`)/area_sqkm,
         REPleaks_sqkmEVC2 = (`Eversource_19repairedC2`)/area_sqkm,
         REPleaks_sqkmEVC3 = (`Eversource_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmEV = (`Eversource_19unrepaired` +
                                  `Eversource_19repaired`)/area_sqkm,
         # AllLeaks2019_sqkmEVC1 = (`Eversource_19unrepairedC1` + 
         #                            `Eversource_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmEVC1 = (`Eversource_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmEVC2 = (`Eversource_19unrepairedC2` + 
                                    `Eversource_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmEVC3 = (`Eversource_19unrepairedC3` + 
                                    `Eversource_19repairedC3`)/area_sqkm,
         leaks_huEV = if_else(total_occ_unitsE == 0, 0, 
                              (`Eversource_19unrepaired`)/total_occ_unitsE),
         # leaks_huEVC1 = if_else(total_occ_unitsE == 0, 0, 
         #                        (`Eversource_19unrepairedC1`)/total_occ_unitsE),
         leaks_huEVC1 = 0,
         leaks_huEVC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`Eversource_19unrepairedC2`)/total_occ_unitsE),
         leaks_huEVC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Eversource_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huEV = if_else(total_occ_unitsE == 0, 0,
                                 (`Eversource_19repaired`)/total_occ_unitsE),
         REPleaks_huEVC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource_19repairedC1`)/total_occ_unitsE),
         REPleaks_huEVC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource_19repairedC2`)/total_occ_unitsE),
         REPleaks_huEVC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huEV = if_else(total_occ_unitsE == 0, 0,
                                     (`Eversource_19unrepaired` + 
                                        `Eversource_19repaired`)/total_occ_unitsE),
         # AllLeaks2019_huEVC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Eversource_19unrepairedC1` +
         #                                  `Eversource_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huEVC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huEVC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource_19unrepairedC2` +
                                          `Eversource_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huEVC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource_19unrepairedC3` +
                                          `Eversource_19repairedC3`)/total_occ_unitsE),
         PctRepaired19EV = (`Eversource_19repaired`)/ (`Eversource_19unrepaired` + `Eversource_19repaired`)*100,
         DaysToRepairAvgEV = `Eversource_19repairedDaysAvg`,
         DaysToRepairAvgEVC1 = `Eversource_19repairedDaysAvgC1`,
         DaysToRepairAvgEVC2 = `Eversource_19repairedDaysAvgC2`,
         DaysToRepairAvgEVC3 = `Eversource_19repairedDaysAvgC3`,
         LeakAgeDaysAvgEV = `Eversource_19unrepairedDaysAvg`,
         # LeakAgeDaysAvgEVC1 = `Eversource_19unrepairedDaysAvgC1`,
         LeakAgeDaysAvgEVC1 = NA,
         LeakAgeDaysAvgEVC2 = `Eversource_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgEVC3 = `Eversource_19unrepairedDaysAvgC3`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), (starts_with("leaks_") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("AllLeaks") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))),
         (starts_with("REPleaks_") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("PctRepaired19") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))),
         (starts_with("leaks_hu") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3")))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmEV = weighted.mean(x = leaks_sqkmEV, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmEVC1 = weighted.mean(x = leaks_sqkmEVC1, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmEVC2 = weighted.mean(x = leaks_sqkmEVC2, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmEVC3 = weighted.mean(x = leaks_sqkmEVC3, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmREPEV = weighted.mean(x = REPleaks_sqkmEV, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPEVC1 = weighted.mean(x = REPleaks_sqkmEVC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPEVC2 = weighted.mean(x = REPleaks_sqkmEVC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPEVC3 = weighted.mean(x = REPleaks_sqkmEVC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLEV = weighted.mean(x = AllLeaks2019_sqkmEV, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLEVC1 = weighted.mean(x = AllLeaks2019_sqkmEVC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLEVC2 = weighted.mean(x = AllLeaks2019_sqkmEVC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLEVC3 = weighted.mean(x = AllLeaks2019_sqkmEVC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgEV = weighted.mean(x = LeakAgeDaysAvgEV,
                                              w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgEVC1 = weighted.mean(x = LeakAgeDaysAvgEVC1,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgEVC2 = weighted.mean(x = LeakAgeDaysAvgEVC2,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgEVC3 = weighted.mean(x = LeakAgeDaysAvgEVC3,
                                                w = Pop, na.rm = TRUE),
            wLeaksPerHUEV = weighted.mean(x = leaks_huEV, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUEVC1 = weighted.mean(x = leaks_huEVC1, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUEVC2 = weighted.mean(x = leaks_huEVC2, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUEVC3 = weighted.mean(x = leaks_huEVC3, w = Pop, 
                                            na.rm = T),
            wREPLeaksPerHUEV = weighted.mean(x = REPleaks_huEV, 
                                             w = Pop, na.rm = T),
            wREPLeaksPerHUEVC1 = weighted.mean(x = REPleaks_huEVC1, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUEVC2 = weighted.mean(x = REPleaks_huEVC2, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUEVC3 = weighted.mean(x = REPleaks_huEVC3, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUEV = weighted.mean(x = AllLeaks2019_huEV, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUEVC1 = weighted.mean(x = AllLeaks2019_huEVC1, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUEVC2 = weighted.mean(x = AllLeaks2019_huEVC2, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUEVC3 = weighted.mean(x = AllLeaks2019_huEVC3, 
                                               w = Pop, na.rm = T),
            wPctRepaired19EV = weighted.mean(x = PctRepaired19EV, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgEV = weighted.mean(x = DaysToRepairAvgEV, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgEVC1 = weighted.mean(x = DaysToRepairAvgEVC1, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgEVC2 = weighted.mean(x = DaysToRepairAvgEVC2, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgEVC3 = weighted.mean(x = DaysToRepairAvgEVC3, 
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "disabledOver18E" = "Disabled Adults",
                        "house_burdened_E" = "Housing Burdened"))

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityEV_pops_df <- ppLeakDensityEV %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "Limited English HH", "Renter Occupied HU", 
                       "Housing Burdened",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRREV = wLeaksPerSqKmEV/wLeaksPerSqKmEV[Group == "Total Population"],
         wLeaksRREVC1 = wLeaksPerSqKmEVC1/wLeaksPerSqKmEVC1[Group == "Total Population"],
         wLeaksRREVC2 = wLeaksPerSqKmEVC2/wLeaksPerSqKmEVC2[Group == "Total Population"],
         wLeaksRREVC3 = wLeaksPerSqKmEVC3/wLeaksPerSqKmEVC3[Group == "Total Population"],
         wLeaksRRrepairEV = wLeaksPerSqKmREPEV/wLeaksPerSqKmREPEV[Group == "Total Population"],
         wLeaksRRrepairEVC1 = wLeaksPerSqKmREPEVC1/wLeaksPerSqKmREPEVC1[Group == "Total Population"],
         wLeaksRRrepairEVC2 = wLeaksPerSqKmREPEVC2/wLeaksPerSqKmREPEVC2[Group == "Total Population"],
         wLeaksRRrepairEVC3 = wLeaksPerSqKmREPEVC3/wLeaksPerSqKmREPEVC3[Group == "Total Population"],
         wLeaksRRtotalEV = wLeaksPerSqKmALLEV/wLeaksPerSqKmALLEV[Group == "Total Population"],
         wLeaksRRtotalEVC1 = wLeaksPerSqKmALLEVC1/wLeaksPerSqKmALLEVC1[Group == "Total Population"],
         wLeaksRRtotalEVC2 = wLeaksPerSqKmALLEVC2/wLeaksPerSqKmALLEVC2[Group == "Total Population"],
         wLeaksRRtotalEVC3 = wLeaksPerSqKmALLEVC3/wLeaksPerSqKmALLEVC3[Group == "Total Population"],
         wLeaksPerHURREV = wLeaksPerHUEV/wLeaksPerHUEV[Group == "Total Population"],
         wLeaksPerHURREVC1 = wLeaksPerHUEVC1/wLeaksPerHUEVC1[Group == "Total Population"],
         wLeaksPerHURREVC2 = wLeaksPerHUEVC2/wLeaksPerHUEVC2[Group == "Total Population"],
         wLeaksPerHURREVC3 = wLeaksPerHUEVC3/wLeaksPerHUEVC3[Group == "Total Population"],
         wREPLeaksPerHURREV = wREPLeaksPerHUEV/wREPLeaksPerHUEV[Group == "Total Population"],
         wREPLeaksPerHURREVC1 = wREPLeaksPerHUEVC1/wREPLeaksPerHUEVC1[Group == "Total Population"],
         wREPLeaksPerHURREVC2 = wREPLeaksPerHUEVC2/wREPLeaksPerHUEVC2[Group == "Total Population"],
         wREPLeaksPerHURREVC3 = wREPLeaksPerHUEVC3/wREPLeaksPerHUEVC3[Group == "Total Population"],
         wALLLeaksPerHURREV = wALLLeaksPerHUEV/wALLLeaksPerHUEV[Group == "Total Population"],
         wALLLeaksPerHURREVC1 = wALLLeaksPerHUEVC1/wALLLeaksPerHUEVC1[Group == "Total Population"],
         wALLLeaksPerHURREVC2 = wALLLeaksPerHUEVC2/wALLLeaksPerHUEVC2[Group == "Total Population"],
         wALLLeaksPerHURREVC3 = wALLLeaksPerHUEVC3/wALLLeaksPerHUEVC3[Group == "Total Population"],
         wDaysToRepairAvgRREV = wDaysToRepairAvgEV/wDaysToRepairAvgEV[Group == "Total Population"],
         wDaysToRepairAvgRREVC1 = wDaysToRepairAvgEVC1/wDaysToRepairAvgEVC1[Group == "Total Population"],
         wDaysToRepairAvgRREVC2 = wDaysToRepairAvgEVC2/wDaysToRepairAvgEVC2[Group == "Total Population"],
         wDaysToRepairAvgRREVC3 = wDaysToRepairAvgEVC3/wDaysToRepairAvgEVC3[Group == "Total Population"],
         wLeakAgeDaysAvgRREV = wLeakAgeDaysAvgEV/wLeakAgeDaysAvgEV[Group == "Total Population"],
         wLeakAgeDaysAvgRREVC1 = wLeakAgeDaysAvgEVC1/wLeakAgeDaysAvgEVC1[Group == "Total Population"],
         wLeakAgeDaysAvgRREVC2 = wLeakAgeDaysAvgEVC2/wLeakAgeDaysAvgEVC2[Group == "Total Population"],
         wLeakAgeDaysAvgRREVC3 = wLeakAgeDaysAvgEVC3/wLeakAgeDaysAvgEVC3[Group == "Total Population"])

ppLeakDensityEV_LEH_df <- ppLeakDensityEV %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRREV = wLeaksPerSqKmEV/wLeaksPerSqKmEV[Group == "Total Households"],
         wLeaksRREVC1 = wLeaksPerSqKmEVC1/wLeaksPerSqKmEVC1[Group == "Total Households"],
         wLeaksRREVC2 = wLeaksPerSqKmEVC2/wLeaksPerSqKmEVC2[Group == "Total Households"],
         wLeaksRREVC3 = wLeaksPerSqKmEVC3/wLeaksPerSqKmEVC3[Group == "Total Households"],
         wLeaksRRrepairEV = wLeaksPerSqKmREPEV/wLeaksPerSqKmREPEV[Group == "Total Households"],
         wLeaksRRrepairEVC1 = wLeaksPerSqKmREPEVC1/wLeaksPerSqKmREPEVC1[Group == "Total Households"],
         wLeaksRRrepairEVC2 = wLeaksPerSqKmREPEVC2/wLeaksPerSqKmREPEVC2[Group == "Total Households"],
         wLeaksRRrepairEVC3 = wLeaksPerSqKmREPEVC3/wLeaksPerSqKmREPEVC3[Group == "Total Households"],
         wLeaksRRtotalEV = wLeaksPerSqKmALLEV/wLeaksPerSqKmALLEV[Group == "Total Households"],
         wLeaksRRtotalEVC1 = wLeaksPerSqKmALLEVC1/wLeaksPerSqKmALLEVC1[Group == "Total Households"],
         wLeaksRRtotalEVC2 = wLeaksPerSqKmALLEVC2/wLeaksPerSqKmALLEVC2[Group == "Total Households"],
         wLeaksRRtotalEVC3 = wLeaksPerSqKmALLEVC3/wLeaksPerSqKmALLEVC3[Group == "Total Households"],
         wLeaksPerHURREV = wLeaksPerHUEV/wLeaksPerHUEV[Group == "Total Households"],
         wLeaksPerHURREVC1 = wLeaksPerHUEVC1/wLeaksPerHUEVC1[Group == "Total Households"],
         wLeaksPerHURREVC2 = wLeaksPerHUEVC2/wLeaksPerHUEVC2[Group == "Total Households"],
         wLeaksPerHURREVC3 = wLeaksPerHUEVC3/wLeaksPerHUEVC3[Group == "Total Households"],
         wREPLeaksPerHURREV = wREPLeaksPerHUEV/wREPLeaksPerHUEV[Group == "Total Households"],
         wREPLeaksPerHURREVC1 = wREPLeaksPerHUEVC1/wREPLeaksPerHUEVC1[Group == "Total Households"],
         wREPLeaksPerHURREVC2 = wREPLeaksPerHUEVC2/wREPLeaksPerHUEVC2[Group == "Total Households"],
         wREPLeaksPerHURREVC3 = wREPLeaksPerHUEVC3/wREPLeaksPerHUEVC3[Group == "Total Households"],
         wALLLeaksPerHURREV = wALLLeaksPerHUEV/wALLLeaksPerHUEV[Group == "Total Households"],
         wALLLeaksPerHURREVC1 = wALLLeaksPerHUEVC1/wALLLeaksPerHUEVC1[Group == "Total Households"],
         wALLLeaksPerHURREVC2 = wALLLeaksPerHUEVC2/wALLLeaksPerHUEVC2[Group == "Total Households"],
         wALLLeaksPerHURREVC3 = wALLLeaksPerHUEVC3/wALLLeaksPerHUEVC3[Group == "Total Households"],
         wDaysToRepairAvgRREV = wDaysToRepairAvgEV/wDaysToRepairAvgEV[Group == "Total Households"],
         wDaysToRepairAvgRREVC1 = wDaysToRepairAvgEVC1/wDaysToRepairAvgEVC1[Group == "Total Households"],
         wDaysToRepairAvgRREVC2 = wDaysToRepairAvgEVC2/wDaysToRepairAvgEVC2[Group == "Total Households"],
         wDaysToRepairAvgRREVC3 = wDaysToRepairAvgEVC3/wDaysToRepairAvgEVC3[Group == "Total Households"],
         wLeakAgeDaysAvgRREV = wLeakAgeDaysAvgEV/wLeakAgeDaysAvgEV[Group == "Total Households"],
         wLeakAgeDaysAvgRREVC1 = wLeakAgeDaysAvgEVC1/wLeakAgeDaysAvgEVC1[Group == "Total Households"],
         wLeakAgeDaysAvgRREVC2 = wLeakAgeDaysAvgEVC2/wLeakAgeDaysAvgEVC2[Group == "Total Households"],
         wLeakAgeDaysAvgRREVC3 = wLeakAgeDaysAvgEVC3/wLeakAgeDaysAvgEVC3[Group == "Total Households"])

ppLeakDensityEV_renters_df <- ppLeakDensityEV %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened", 
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRREV = wLeaksPerSqKmEV/wLeaksPerSqKmEV[Group == "Total Occupied HU"],
         wLeaksRREVC1 = wLeaksPerSqKmEVC1/wLeaksPerSqKmEVC1[Group == "Total Occupied HU"],
         wLeaksRREVC2 = wLeaksPerSqKmEVC2/wLeaksPerSqKmEVC2[Group == "Total Occupied HU"],
         wLeaksRREVC3 = wLeaksPerSqKmEVC3/wLeaksPerSqKmEVC3[Group == "Total Occupied HU"],
         wLeaksRRrepairEV = wLeaksPerSqKmREPEV/wLeaksPerSqKmREPEV[Group == "Total Occupied HU"],
         wLeaksRRrepairEVC1 = wLeaksPerSqKmREPEVC1/wLeaksPerSqKmREPEVC1[Group == "Total Occupied HU"],
         wLeaksRRrepairEVC2 = wLeaksPerSqKmREPEVC2/wLeaksPerSqKmREPEVC2[Group == "Total Occupied HU"],
         wLeaksRRrepairEVC3 = wLeaksPerSqKmREPEVC3/wLeaksPerSqKmREPEVC3[Group == "Total Occupied HU"],
         wLeaksRRtotalEV = wLeaksPerSqKmALLEV/wLeaksPerSqKmALLEV[Group == "Total Occupied HU"],
         wLeaksRRtotalEVC1 = wLeaksPerSqKmALLEVC1/wLeaksPerSqKmALLEVC1[Group == "Total Occupied HU"],
         wLeaksRRtotalEVC2 = wLeaksPerSqKmALLEVC2/wLeaksPerSqKmALLEVC2[Group == "Total Occupied HU"],
         wLeaksRRtotalEVC3 = wLeaksPerSqKmALLEVC3/wLeaksPerSqKmALLEVC3[Group == "Total Occupied HU"],
         wLeaksPerHURREV = wLeaksPerHUEV/wLeaksPerHUEV[Group == "Total Occupied HU"],
         wLeaksPerHURREVC1 = wLeaksPerHUEVC1/wLeaksPerHUEVC1[Group == "Total Occupied HU"],
         wLeaksPerHURREVC2 = wLeaksPerHUEVC2/wLeaksPerHUEVC2[Group == "Total Occupied HU"],
         wLeaksPerHURREVC3 = wLeaksPerHUEVC3/wLeaksPerHUEVC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURREV = wREPLeaksPerHUEV/wREPLeaksPerHUEV[Group == "Total Occupied HU"],
         wREPLeaksPerHURREVC1 = wREPLeaksPerHUEVC1/wREPLeaksPerHUEVC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURREVC2 = wREPLeaksPerHUEVC2/wREPLeaksPerHUEVC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURREVC3 = wREPLeaksPerHUEVC3/wREPLeaksPerHUEVC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURREV = wALLLeaksPerHUEV/wALLLeaksPerHUEV[Group == "Total Occupied HU"],
         wALLLeaksPerHURREVC1 = wALLLeaksPerHUEVC1/wALLLeaksPerHUEVC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURREVC2 = wALLLeaksPerHUEVC2/wALLLeaksPerHUEVC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURREVC3 = wALLLeaksPerHUEVC3/wALLLeaksPerHUEVC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRREV = wDaysToRepairAvgEV/wDaysToRepairAvgEV[Group == "Total Occupied HU"],
         wDaysToRepairAvgRREVC1 = wDaysToRepairAvgEVC1/wDaysToRepairAvgEVC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRREVC2 = wDaysToRepairAvgEVC2/wDaysToRepairAvgEVC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRREVC3 = wDaysToRepairAvgEVC3/wDaysToRepairAvgEVC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRREV = wLeakAgeDaysAvgEV/wLeakAgeDaysAvgEV[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRREVC1 = wLeakAgeDaysAvgEVC1/wLeakAgeDaysAvgEVC1[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRREVC2 = wLeakAgeDaysAvgEVC2/wLeakAgeDaysAvgEVC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRREVC3 = wLeakAgeDaysAvgEVC3/wLeakAgeDaysAvgEVC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensityEV_df <- rbind(ppLeakDensityEV_pops_df,
                            ppLeakDensityEV_LEH_df,
                            ppLeakDensityEV_renters_df)



### Columbia Gas
ppLeakDensityCG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Columbia") | 
           GAS == "Columbia Gas$") %>% # limit to BGs in CG svc area
  mutate(leaks_sqkmCG = (`Columbia Gas_19unrepaired`)/area_sqkm,
         leaks_sqkmCGC1 = (`Columbia Gas_19unrepairedC1`)/area_sqkm,
         # leaks_sqkmCGC1 = 0,
         leaks_sqkmCGC2 = (`Columbia Gas_19unrepairedC2`)/area_sqkm,
         leaks_sqkmCGC3 = (`Columbia Gas_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmCG = (`Columbia Gas_19repaired`)/area_sqkm,
         REPleaks_sqkmCGC1 = (`Columbia Gas_19repairedC1`)/area_sqkm,
         REPleaks_sqkmCGC2 = (`Columbia Gas_19repairedC2`)/area_sqkm,
         REPleaks_sqkmCGC3 = (`Columbia Gas_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmCG = (`Columbia Gas_19unrepaired` +
                                  `Columbia Gas_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmCGC1 = (`Columbia Gas_19unrepairedC1` +
                                    `Columbia Gas_19repairedC1`)/area_sqkm,
         # AllLeaks2019_sqkmCGC1 = (`Columbia Gas_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmCGC2 = (`Columbia Gas_19unrepairedC2` + 
                                    `Columbia Gas_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmCGC3 = (`Columbia Gas_19unrepairedC3` + 
                                    `Columbia Gas_19repairedC3`)/area_sqkm,
         leaks_huCG = if_else(total_occ_unitsE == 0, 0, 
                              (`Columbia Gas_19unrepaired`)/total_occ_unitsE),
         leaks_huCGC1 = if_else(total_occ_unitsE == 0, 0,
                                (`Columbia Gas_19unrepairedC1`)/total_occ_unitsE),
         # leaks_huCGC1 = 0,
         leaks_huCGC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`Columbia Gas_19unrepairedC2`)/total_occ_unitsE),
         leaks_huCGC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Columbia Gas_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huCG = if_else(total_occ_unitsE == 0, 0,
                                 (`Columbia Gas_19repaired`)/total_occ_unitsE),
         REPleaks_huCGC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`Columbia Gas_19repairedC1`)/total_occ_unitsE),
         REPleaks_huCGC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Columbia Gas_19repairedC2`)/total_occ_unitsE),
         REPleaks_huCGC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Columbia Gas_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huCG = if_else(total_occ_unitsE == 0, 0,
                                     (`Columbia Gas_19unrepaired` + 
                                        `Columbia Gas_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huCGC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`Columbia Gas_19unrepairedC1` +
                                          `Columbia Gas_19repairedC1`)/total_occ_unitsE),
         # AllLeaks2019_huCGC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Columbia Gas_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huCGC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`Columbia Gas_19unrepairedC2` +
                                          `Columbia Gas_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huCGC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Columbia Gas_19unrepairedC3` +
                                          `Columbia Gas_19repairedC3`)/total_occ_unitsE),
         PctRepaired19CG = (`Columbia Gas_19repaired`)/ (`Columbia Gas_19unrepaired` + `Columbia Gas_19repaired`)*100,
         DaysToRepairAvgCG = `Columbia Gas_19repairedDaysAvg`,
         DaysToRepairAvgCGC1 = `Columbia Gas_19repairedDaysAvgC1`,
         DaysToRepairAvgCGC2 = `Columbia Gas_19repairedDaysAvgC2`,
         DaysToRepairAvgCGC3 = `Columbia Gas_19repairedDaysAvgC3`,
         LeakAgeDaysAvgCG = `Columbia Gas_19unrepairedDaysAvg`,
         LeakAgeDaysAvgCGC1 = `Columbia Gas_19unrepairedDaysAvgC1`,
         # LeakAgeDaysAvgCGC1 = NA,
         LeakAgeDaysAvgCGC2 = `Columbia Gas_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgCGC3 = `Columbia Gas_19unrepairedDaysAvgC3`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), (starts_with("leaks_") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("AllLeaks") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))),
         (starts_with("REPleaks_") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))),
         (starts_with("leaks_hu") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3")))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmCG = weighted.mean(x = leaks_sqkmCG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmCGC1 = weighted.mean(x = leaks_sqkmCGC1, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmCGC2 = weighted.mean(x = leaks_sqkmCGC2, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmCGC3 = weighted.mean(x = leaks_sqkmCGC3, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmREPCG = weighted.mean(x = REPleaks_sqkmCG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPCGC1 = weighted.mean(x = REPleaks_sqkmCGC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPCGC2 = weighted.mean(x = REPleaks_sqkmCGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPCGC3 = weighted.mean(x = REPleaks_sqkmCGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLCG = weighted.mean(x = AllLeaks2019_sqkmCG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLCGC1 = weighted.mean(x = AllLeaks2019_sqkmCGC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLCGC2 = weighted.mean(x = AllLeaks2019_sqkmCGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLCGC3 = weighted.mean(x = AllLeaks2019_sqkmCGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgCG = weighted.mean(x = LeakAgeDaysAvgCG,
                                              w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgCGC1 = weighted.mean(x = LeakAgeDaysAvgCGC1,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgCGC2 = weighted.mean(x = LeakAgeDaysAvgCGC2,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgCGC3 = weighted.mean(x = LeakAgeDaysAvgCGC3,
                                                w = Pop, na.rm = TRUE),
            wLeaksPerHUCG = weighted.mean(x = leaks_huCG, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUCGC1 = weighted.mean(x = leaks_huCGC1, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUCGC2 = weighted.mean(x = leaks_huCGC2, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUCGC3 = weighted.mean(x = leaks_huCGC3, w = Pop, 
                                            na.rm = T),
            wREPLeaksPerHUCG = weighted.mean(x = REPleaks_huCG, 
                                             w = Pop, na.rm = T),
            wREPLeaksPerHUCGC1 = weighted.mean(x = REPleaks_huCGC1, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUCGC2 = weighted.mean(x = REPleaks_huCGC2, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUCGC3 = weighted.mean(x = REPleaks_huCGC3, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUCG = weighted.mean(x = AllLeaks2019_huCG, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUCGC1 = weighted.mean(x = AllLeaks2019_huCGC1, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUCGC2 = weighted.mean(x = AllLeaks2019_huCGC2, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUCGC3 = weighted.mean(x = AllLeaks2019_huCGC3, 
                                               w = Pop, na.rm = T),
            wPctRepaired19CG = weighted.mean(x = PctRepaired19CG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgCG = weighted.mean(x = DaysToRepairAvgCG, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgCGC1 = weighted.mean(x = DaysToRepairAvgCGC1, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgCGC2 = weighted.mean(x = DaysToRepairAvgCGC2, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgCGC3 = weighted.mean(x = DaysToRepairAvgCGC3, 
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "disabledOver18E" = "Disabled Adults",
                        "house_burdened_E" = "Housing Burdened"))

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityCG_pops_df <- ppLeakDensityCG %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17", "Housing Burdened",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRRCG = wLeaksPerSqKmCG/wLeaksPerSqKmCG[Group == "Total Population"],
         wLeaksRRCGC1 = wLeaksPerSqKmCGC1/wLeaksPerSqKmCGC1[Group == "Total Population"],
         wLeaksRRCGC2 = wLeaksPerSqKmCGC2/wLeaksPerSqKmCGC2[Group == "Total Population"],
         wLeaksRRCGC3 = wLeaksPerSqKmCGC3/wLeaksPerSqKmCGC3[Group == "Total Population"],
         wLeaksRRrepairCG = wLeaksPerSqKmREPCG/wLeaksPerSqKmREPCG[Group == "Total Population"],
         wLeaksRRrepairCGC1 = wLeaksPerSqKmREPCGC1/wLeaksPerSqKmREPCGC1[Group == "Total Population"],
         wLeaksRRrepairCGC2 = wLeaksPerSqKmREPCGC2/wLeaksPerSqKmREPCGC2[Group == "Total Population"],
         wLeaksRRrepairCGC3 = wLeaksPerSqKmREPCGC3/wLeaksPerSqKmREPCGC3[Group == "Total Population"],
         wLeaksRRtotalCG = wLeaksPerSqKmALLCG/wLeaksPerSqKmALLCG[Group == "Total Population"],
         wLeaksRRtotalCGC1 = wLeaksPerSqKmALLCGC1/wLeaksPerSqKmALLCGC1[Group == "Total Population"],
         wLeaksRRtotalCGC2 = wLeaksPerSqKmALLCGC2/wLeaksPerSqKmALLCGC2[Group == "Total Population"],
         wLeaksRRtotalCGC3 = wLeaksPerSqKmALLCGC3/wLeaksPerSqKmALLCGC3[Group == "Total Population"],
         wLeaksPerHURRCG = wLeaksPerHUCG/wLeaksPerHUCG[Group == "Total Population"],
         wLeaksPerHURRCGC1 = wLeaksPerHUCGC1/wLeaksPerHUCGC1[Group == "Total Population"],
         wLeaksPerHURRCGC2 = wLeaksPerHUCGC2/wLeaksPerHUCGC2[Group == "Total Population"],
         wLeaksPerHURRCGC3 = wLeaksPerHUCGC3/wLeaksPerHUCGC3[Group == "Total Population"],
         wREPLeaksPerHURRCG = wREPLeaksPerHUCG/wREPLeaksPerHUCG[Group == "Total Population"],
         wREPLeaksPerHURRCGC1 = wREPLeaksPerHUCGC1/wREPLeaksPerHUCGC1[Group == "Total Population"],
         wREPLeaksPerHURRCGC2 = wREPLeaksPerHUCGC2/wREPLeaksPerHUCGC2[Group == "Total Population"],
         wREPLeaksPerHURRCGC3 = wREPLeaksPerHUCGC3/wREPLeaksPerHUCGC3[Group == "Total Population"],
         wALLLeaksPerHURRCG = wALLLeaksPerHUCG/wALLLeaksPerHUCG[Group == "Total Population"],
         wALLLeaksPerHURRCGC1 = wALLLeaksPerHUCGC1/wALLLeaksPerHUCGC1[Group == "Total Population"],
         wALLLeaksPerHURRCGC2 = wALLLeaksPerHUCGC2/wALLLeaksPerHUCGC2[Group == "Total Population"],
         wALLLeaksPerHURRCGC3 = wALLLeaksPerHUCGC3/wALLLeaksPerHUCGC3[Group == "Total Population"],
         wDaysToRepairAvgRRCG = wDaysToRepairAvgCG/wDaysToRepairAvgCG[Group == "Total Population"],
         wDaysToRepairAvgRRCGC1 = wDaysToRepairAvgCGC1/wDaysToRepairAvgCGC1[Group == "Total Population"],
         wDaysToRepairAvgRRCGC2 = wDaysToRepairAvgCGC2/wDaysToRepairAvgCGC2[Group == "Total Population"],
         wDaysToRepairAvgRRCGC3 = wDaysToRepairAvgCGC3/wDaysToRepairAvgCGC3[Group == "Total Population"],
         wLeakAgeDaysAvgRRCG = wLeakAgeDaysAvgCG/wLeakAgeDaysAvgCG[Group == "Total Population"],
         wLeakAgeDaysAvgRRCGC1 = wLeakAgeDaysAvgCGC1/wLeakAgeDaysAvgCGC1[Group == "Total Population"],
         wLeakAgeDaysAvgRRCGC2 = wLeakAgeDaysAvgCGC2/wLeakAgeDaysAvgCGC2[Group == "Total Population"],
         wLeakAgeDaysAvgRRCGC3 = wLeakAgeDaysAvgCGC3/wLeakAgeDaysAvgCGC3[Group == "Total Population"])

ppLeakDensityCG_LEH_df <- ppLeakDensityCG %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRRCG = wLeaksPerSqKmCG/wLeaksPerSqKmCG[Group == "Total Households"],
         wLeaksRRCGC1 = wLeaksPerSqKmCGC1/wLeaksPerSqKmCGC1[Group == "Total Households"],
         wLeaksRRCGC2 = wLeaksPerSqKmCGC2/wLeaksPerSqKmCGC2[Group == "Total Households"],
         wLeaksRRCGC3 = wLeaksPerSqKmCGC3/wLeaksPerSqKmCGC3[Group == "Total Households"],
         wLeaksRRrepairCG = wLeaksPerSqKmREPCG/wLeaksPerSqKmREPCG[Group == "Total Households"],
         wLeaksRRrepairCGC1 = wLeaksPerSqKmREPCGC1/wLeaksPerSqKmREPCGC1[Group == "Total Households"],
         wLeaksRRrepairCGC2 = wLeaksPerSqKmREPCGC2/wLeaksPerSqKmREPCGC2[Group == "Total Households"],
         wLeaksRRrepairCGC3 = wLeaksPerSqKmREPCGC3/wLeaksPerSqKmREPCGC3[Group == "Total Households"],
         wLeaksRRtotalCG = wLeaksPerSqKmALLCG/wLeaksPerSqKmALLCG[Group == "Total Households"],
         wLeaksRRtotalCGC1 = wLeaksPerSqKmALLCGC1/wLeaksPerSqKmALLCGC1[Group == "Total Households"],
         wLeaksRRtotalCGC2 = wLeaksPerSqKmALLCGC2/wLeaksPerSqKmALLCGC2[Group == "Total Households"],
         wLeaksRRtotalCGC3 = wLeaksPerSqKmALLCGC3/wLeaksPerSqKmALLCGC3[Group == "Total Households"],
         wLeaksPerHURRCG = wLeaksPerHUCG/wLeaksPerHUCG[Group == "Total Households"],
         wLeaksPerHURRCGC1 = wLeaksPerHUCGC1/wLeaksPerHUCGC1[Group == "Total Households"],
         wLeaksPerHURRCGC2 = wLeaksPerHUCGC2/wLeaksPerHUCGC2[Group == "Total Households"],
         wLeaksPerHURRCGC3 = wLeaksPerHUCGC3/wLeaksPerHUCGC3[Group == "Total Households"],
         wREPLeaksPerHURRCG = wREPLeaksPerHUCG/wREPLeaksPerHUCG[Group == "Total Households"],
         wREPLeaksPerHURRCGC1 = wREPLeaksPerHUCGC1/wREPLeaksPerHUCGC1[Group == "Total Households"],
         wREPLeaksPerHURRCGC2 = wREPLeaksPerHUCGC2/wREPLeaksPerHUCGC2[Group == "Total Households"],
         wREPLeaksPerHURRCGC3 = wREPLeaksPerHUCGC3/wREPLeaksPerHUCGC3[Group == "Total Households"],
         wALLLeaksPerHURRCG = wALLLeaksPerHUCG/wALLLeaksPerHUCG[Group == "Total Households"],
         wALLLeaksPerHURRCGC1 = wALLLeaksPerHUCGC1/wALLLeaksPerHUCGC1[Group == "Total Households"],
         wALLLeaksPerHURRCGC2 = wALLLeaksPerHUCGC2/wALLLeaksPerHUCGC2[Group == "Total Households"],
         wALLLeaksPerHURRCGC3 = wALLLeaksPerHUCGC3/wALLLeaksPerHUCGC3[Group == "Total Households"],
         wDaysToRepairAvgRRCG = wDaysToRepairAvgCG/wDaysToRepairAvgCG[Group == "Total Households"],
         wDaysToRepairAvgRRCGC1 = wDaysToRepairAvgCGC1/wDaysToRepairAvgCGC1[Group == "Total Households"],
         wDaysToRepairAvgRRCGC2 = wDaysToRepairAvgCGC2/wDaysToRepairAvgCGC2[Group == "Total Households"],
         wDaysToRepairAvgRRCGC3 = wDaysToRepairAvgCGC3/wDaysToRepairAvgCGC3[Group == "Total Households"],
         wLeakAgeDaysAvgRRCG = wLeakAgeDaysAvgCG/wLeakAgeDaysAvgCG[Group == "Total Households"],
         wLeakAgeDaysAvgRRCGC1 = wLeakAgeDaysAvgCGC1/wLeakAgeDaysAvgCGC1[Group == "Total Households"],
         wLeakAgeDaysAvgRRCGC2 = wLeakAgeDaysAvgCGC2/wLeakAgeDaysAvgCGC2[Group == "Total Households"],
         wLeakAgeDaysAvgRRCGC3 = wLeakAgeDaysAvgCGC3/wLeakAgeDaysAvgCGC3[Group == "Total Households"])

ppLeakDensityCG_renters_df <- ppLeakDensityCG %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened", 
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRRCG = wLeaksPerSqKmCG/wLeaksPerSqKmCG[Group == "Total Occupied HU"],
         wLeaksRRCGC1 = wLeaksPerSqKmCGC1/wLeaksPerSqKmCGC1[Group == "Total Occupied HU"],
         wLeaksRRCGC2 = wLeaksPerSqKmCGC2/wLeaksPerSqKmCGC2[Group == "Total Occupied HU"],
         wLeaksRRCGC3 = wLeaksPerSqKmCGC3/wLeaksPerSqKmCGC3[Group == "Total Occupied HU"],
         wLeaksRRrepairCG = wLeaksPerSqKmREPCG/wLeaksPerSqKmREPCG[Group == "Total Occupied HU"],
         wLeaksRRrepairCGC1 = wLeaksPerSqKmREPCGC1/wLeaksPerSqKmREPCGC1[Group == "Total Occupied HU"],
         wLeaksRRrepairCGC2 = wLeaksPerSqKmREPCGC2/wLeaksPerSqKmREPCGC2[Group == "Total Occupied HU"],
         wLeaksRRrepairCGC3 = wLeaksPerSqKmREPCGC3/wLeaksPerSqKmREPCGC3[Group == "Total Occupied HU"],
         wLeaksRRtotalCG = wLeaksPerSqKmALLCG/wLeaksPerSqKmALLCG[Group == "Total Occupied HU"],
         wLeaksRRtotalCGC1 = wLeaksPerSqKmALLCGC1/wLeaksPerSqKmALLCGC1[Group == "Total Occupied HU"],
         wLeaksRRtotalCGC2 = wLeaksPerSqKmALLCGC2/wLeaksPerSqKmALLCGC2[Group == "Total Occupied HU"],
         wLeaksRRtotalCGC3 = wLeaksPerSqKmALLCGC3/wLeaksPerSqKmALLCGC3[Group == "Total Occupied HU"],
         wLeaksPerHURRCG = wLeaksPerHUCG/wLeaksPerHUCG[Group == "Total Occupied HU"],
         wLeaksPerHURRCGC1 = wLeaksPerHUCGC1/wLeaksPerHUCGC1[Group == "Total Occupied HU"],
         wLeaksPerHURRCGC2 = wLeaksPerHUCGC2/wLeaksPerHUCGC2[Group == "Total Occupied HU"],
         wLeaksPerHURRCGC3 = wLeaksPerHUCGC3/wLeaksPerHUCGC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURRCG = wREPLeaksPerHUCG/wREPLeaksPerHUCG[Group == "Total Occupied HU"],
         wREPLeaksPerHURRCGC1 = wREPLeaksPerHUCGC1/wREPLeaksPerHUCGC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURRCGC2 = wREPLeaksPerHUCGC2/wREPLeaksPerHUCGC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURRCGC3 = wREPLeaksPerHUCGC3/wREPLeaksPerHUCGC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURRCG = wALLLeaksPerHUCG/wALLLeaksPerHUCG[Group == "Total Occupied HU"],
         wALLLeaksPerHURRCGC1 = wALLLeaksPerHUCGC1/wALLLeaksPerHUCGC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURRCGC2 = wALLLeaksPerHUCGC2/wALLLeaksPerHUCGC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURRCGC3 = wALLLeaksPerHUCGC3/wALLLeaksPerHUCGC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRCG = wDaysToRepairAvgCG/wDaysToRepairAvgCG[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRCGC1 = wDaysToRepairAvgCGC1/wDaysToRepairAvgCGC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRCGC2 = wDaysToRepairAvgCGC2/wDaysToRepairAvgCGC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRCGC3 = wDaysToRepairAvgCGC3/wDaysToRepairAvgCGC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRCG = wLeakAgeDaysAvgCG/wLeakAgeDaysAvgCG[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRCGC1 = wLeakAgeDaysAvgCGC1/wLeakAgeDaysAvgCGC1[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRCGC2 = wLeakAgeDaysAvgCGC2/wLeakAgeDaysAvgCGC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRCGC3 = wLeakAgeDaysAvgCGC3/wLeakAgeDaysAvgCGC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensityCG_df <- rbind(ppLeakDensityCG_pops_df,
                            ppLeakDensityCG_LEH_df,
                            ppLeakDensityCG_renters_df)




### Fitchburg. As of 2019, Fitchburg/Unitil had no class 1 leaks listed as repaired; only listed in eliminated by replacement. Only class 2 and 3 repaired. For unrepaired, only class 3 leaks. 
ppLeakDensityFG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Unitil") | 
           GAS == "Unitil$") %>% # limit to BGs in FG svc area
  mutate(leaks_sqkmFG = (`Fitchburg_19unrepaired`)/area_sqkm,
         leaks_sqkmFGC1 = if("Fitchburg_19unrepairedC1" %in% colnames(.)) `Fitchburg_19unrepairedC1`/area_sqkm else 0,
         # leaks_sqkmFGC1 = 0,
         leaks_sqkmFGC2 = if("Fitchburg_19unrepairedC2" %in% colnames(.)) `Fitchburg_19unrepairedC2`/area_sqkm else 0,
         # leaks_sqkmFGC2 = 0,
         leaks_sqkmFGC3 = (`Fitchburg_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmFG = (`Fitchburg_19repaired`)/area_sqkm,
         REPleaks_sqkmFGC1 = if("Fitchburg_19repairedC1" %in% colnames(.)) `Fitchburg_19repairedC1`/area_sqkm else 0,
         REPleaks_sqkmFGC2 = (`Fitchburg_19repairedC2`)/area_sqkm,
         REPleaks_sqkmFGC3 = (`Fitchburg_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmFG = (`Fitchburg_19unrepaired` +
                                  `Fitchburg_19repaired`)/area_sqkm,
         # AllLeaks2019_sqkmFGC1 = (`Fitchburg_19unrepairedC1` +
         # `Fitchburg_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmFGC1 = 0,
         # AllLeaks2019_sqkmFGC1 = (`Fitchburg_19repairedC1`)/area_sqkm,
         # AllLeaks2019_sqkmFGC2 = (`Fitchburg_19unrepairedC2` + 
         #                            `Fitchburg_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmFGC2 = 0,
         AllLeaks2019_sqkmFGC3 = (`Fitchburg_19unrepairedC3` + 
                                    `Fitchburg_19repairedC3`)/area_sqkm,
         leaks_huFG = if_else(total_occ_unitsE == 0, 0, 
                              (`Fitchburg_19unrepaired`)/total_occ_unitsE),
         # leaks_huFGC1 = if_else(total_occ_unitsE == 0, 0,
         #                        (`Fitchburg_19unrepairedC1`)/total_occ_unitsE),
         leaks_huFGC1 = 0,
         # leaks_huFGC2 = if_else(total_occ_unitsE == 0, 0, 
         #                        (`Fitchburg_19unrepairedC2`)/total_occ_unitsE),
         leaks_huFGC2 = 0,
         leaks_huFGC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Fitchburg_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huFG = if_else(total_occ_unitsE == 0, 0,
                                 (`Fitchburg_19repaired`)/total_occ_unitsE),
         # REPleaks_huFGC1 = if_else(total_occ_unitsE == 0, 0,
         # (`Fitchburg_19repairedC1`)/total_occ_unitsE),
         REPleaks_huFGC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Fitchburg_19repairedC2`)/total_occ_unitsE),
         REPleaks_huFGC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Fitchburg_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huFG = if_else(total_occ_unitsE == 0, 0,
                                     (`Fitchburg_19unrepaired` + 
                                        `Fitchburg_19repaired`)/total_occ_unitsE),
         # AllLeaks2019_huFGC1 = if_else(total_occ_unitsE == 0, 0,
         # (`Fitchburg_19unrepairedC1` +
         #    `Fitchburg_19repairedC1`)/total_occ_unitsE),
         # AllLeaks2019_huFGC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Fitchburg_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huFGC2 = if_else(total_occ_unitsE == 0, 0,
                                       (
                                         # `Fitchburg_19unrepairedC2` +
                                         `Fitchburg_19repairedC2`)/total_occ_unitsE),
         # AllLeaks2019_huFGC2 = 0,
         AllLeaks2019_huFGC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Fitchburg_19unrepairedC3` +
                                          `Fitchburg_19repairedC3`)/total_occ_unitsE),
         PctRepaired19FG = (`Fitchburg_19repaired`)/ (`Fitchburg_19unrepaired` + `Fitchburg_19repaired`)*100,
         DaysToRepairAvgFG = `Fitchburg_19repairedDaysAvg`,
         # DaysToRepairAvgFGC1 = `Fitchburg_19repairedDaysAvgC1`,
         DaysToRepairAvgFGC2 = `Fitchburg_19repairedDaysAvgC2`,
         DaysToRepairAvgFGC3 = `Fitchburg_19repairedDaysAvgC3`,
         LeakAgeDaysAvgFG = `Fitchburg_19unrepairedDaysAvg`,
         # LeakAgeDaysAvgFGC1 = `Fitchburg_19unrepairedDaysAvgC1`,
         # LeakAgeDaysAvgFGC1 = NA,
         # LeakAgeDaysAvgFGC2 = `Fitchburg_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgFGC3 = `Fitchburg_19unrepairedDaysAvgC3`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, 
         num2povE, lthsE, 
         ends_with("unitsE"), (starts_with("leaks_") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("AllLeaks") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))),
         (starts_with("REPleaks_") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))),
         (starts_with("leaks_hu") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3")))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmFG = weighted.mean(x = leaks_sqkmFG, w = Pop, 
                                            na.rm = TRUE),
            # wLeaksPerSqKmFGC1 = weighted.mean(x = leaks_sqkmFGC1, w = Pop, 
            #                                   na.rm = TRUE),
            wLeaksPerSqKmFGC1 = 0,
            # wLeaksPerSqKmFGC2 = weighted.mean(x = leaks_sqkmFGC2, w = Pop, 
            #                                   na.rm = TRUE),
            wLeaksPerSqKmFGC2 = 0,
            wLeaksPerSqKmFGC3 = weighted.mean(x = leaks_sqkmFGC3, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmREPFG = weighted.mean(x = REPleaks_sqkmFG, 
                                               w = Pop, na.rm = TRUE),
            # wLeaksPerSqKmREPFGC1 = weighted.mean(x = REPleaks_sqkmFGC1, 
            #                                      w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPFGC1 = 0,
            wLeaksPerSqKmREPFGC2 = weighted.mean(x = REPleaks_sqkmFGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPFGC3 = weighted.mean(x = REPleaks_sqkmFGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLFG = weighted.mean(x = AllLeaks2019_sqkmFG, 
                                               w = Pop, na.rm = TRUE),
            # wLeaksPerSqKmALLFGC1 = weighted.mean(x = AllLeaks2019_sqkmFGC1, 
            #                                      w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLFGC2 = weighted.mean(x = AllLeaks2019_sqkmFGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLFGC3 = weighted.mean(x = AllLeaks2019_sqkmFGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgFG = weighted.mean(x = LeakAgeDaysAvgFG,
                                              w = Pop, na.rm = TRUE),
            # wLeakAgeDaysAvgFGC1 = weighted.mean(x = LeakAgeDaysAvgFGC1,
            #                                     w = Pop, na.rm = TRUE),
            # wLeakAgeDaysAvgFGC2 = weighted.mean(x = LeakAgeDaysAvgFGC2,
            #                                     w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgFGC3 = weighted.mean(x = LeakAgeDaysAvgFGC3,
                                                w = Pop, na.rm = TRUE),
            wLeaksPerHUFG = weighted.mean(x = leaks_huFG, w = Pop, 
                                          na.rm = T),
            # wLeaksPerHUFGC1 = weighted.mean(x = leaks_huFGC1, w = Pop, 
            #                                 na.rm = T),
            # wLeaksPerHUFGC2 = weighted.mean(x = leaks_huFGC2, w = Pop, 
            #                                 na.rm = T),
            wLeaksPerHUFGC2 = 0,
            wLeaksPerHUFGC3 = weighted.mean(x = leaks_huFGC3, w = Pop, 
                                            na.rm = T),
            wREPLeaksPerHUFG = weighted.mean(x = REPleaks_huFG, 
                                             w = Pop, na.rm = T),
            # wREPLeaksPerHUFGC1 = weighted.mean(x = REPleaks_huFGC1, 
            #                                    w = Pop, na.rm = T),
            wREPLeaksPerHUFGC1 = 0,
            wREPLeaksPerHUFGC2 = weighted.mean(x = REPleaks_huFGC2, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUFGC3 = weighted.mean(x = REPleaks_huFGC3, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUFG = weighted.mean(x = AllLeaks2019_huFG, 
                                             w = Pop, na.rm = T),
            # wALLLeaksPerHUFGC1 = weighted.mean(x = AllLeaks2019_huFGC1, 
            #                                    w = Pop, na.rm = T),
            wALLLeaksPerHUFGC2 = weighted.mean(x = AllLeaks2019_huFGC2, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUFGC3 = weighted.mean(x = AllLeaks2019_huFGC3, 
                                               w = Pop, na.rm = T),
            wPctRepaired19FG = weighted.mean(x = PctRepaired19FG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgFG = weighted.mean(x = DaysToRepairAvgFG, 
                                               w = Pop, na.rm = T),
            # wDaysToRepairAvgFGC1 = weighted.mean(x = DaysToRepairAvgFGC1, 
            #                                      w = Pop, na.rm = T),
            wDaysToRepairAvgFGC2 = weighted.mean(x = DaysToRepairAvgFGC2, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgFGC3 = weighted.mean(x = DaysToRepairAvgFGC3, 
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

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityFG_pops_df <- ppLeakDensityFG %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRRFG = wLeaksPerSqKmFG/wLeaksPerSqKmFG[Group == "Total Population"],
         # wLeaksRRFGC1 = wLeaksPerSqKmFGC1/wLeaksPerSqKmFGC1[Group == "Total Population"],
         # wLeaksRRFGC2 = wLeaksPerSqKmFGC2/wLeaksPerSqKmFGC2[Group == "Total Population"],
         wLeaksRRFGC2 = 0,
         wLeaksRRFGC3 = wLeaksPerSqKmFGC3/wLeaksPerSqKmFGC3[Group == "Total Population"],
         wLeaksRRrepairFG = wLeaksPerSqKmREPFG/wLeaksPerSqKmREPFG[Group == "Total Population"],
         # wLeaksRRrepairFGC1 = wLeaksPerSqKmREPFGC1/wLeaksPerSqKmREPFGC1[Group == "Total Population"],
         wLeaksRRrepairFGC2 = wLeaksPerSqKmREPFGC2/wLeaksPerSqKmREPFGC2[Group == "Total Population"],
         wLeaksRRrepairFGC3 = wLeaksPerSqKmREPFGC3/wLeaksPerSqKmREPFGC3[Group == "Total Population"],
         wLeaksRRtotalFG = wLeaksPerSqKmALLFG/wLeaksPerSqKmALLFG[Group == "Total Population"],
         # wLeaksRRtotalFGC1 = wLeaksPerSqKmALLFGC1/wLeaksPerSqKmALLFGC1[Group == "Total Population"],
         wLeaksRRtotalFGC2 = wLeaksPerSqKmALLFGC2/wLeaksPerSqKmALLFGC2[Group == "Total Population"],
         wLeaksRRtotalFGC3 = wLeaksPerSqKmALLFGC3/wLeaksPerSqKmALLFGC3[Group == "Total Population"],
         wLeaksPerHURRFG = wLeaksPerHUFG/wLeaksPerHUFG[Group == "Total Population"],
         # wLeaksPerHURRFGC1 = wLeaksPerHUFGC1/wLeaksPerHUFGC1[Group == "Total Population"],
         # wLeaksPerHURRFGC2 = wLeaksPerHUFGC2/wLeaksPerHUFGC2[Group == "Total Population"],
         wLeaksPerHURRFGC2 = 0,
         wLeaksPerHURRFGC3 = wLeaksPerHUFGC3/wLeaksPerHUFGC3[Group == "Total Population"],
         wREPLeaksPerHURRFG = wREPLeaksPerHUFG/wREPLeaksPerHUFG[Group == "Total Population"],
         # wREPLeaksPerHURRFGC1 = wREPLeaksPerHUFGC1/wREPLeaksPerHUFGC1[Group == "Total Population"],
         wREPLeaksPerHURRFGC2 = wREPLeaksPerHUFGC2/wREPLeaksPerHUFGC2[Group == "Total Population"],
         wREPLeaksPerHURRFGC3 = wREPLeaksPerHUFGC3/wREPLeaksPerHUFGC3[Group == "Total Population"],
         wALLLeaksPerHURRFG = wALLLeaksPerHUFG/wALLLeaksPerHUFG[Group == "Total Population"],
         # wALLLeaksPerHURRFGC1 = wALLLeaksPerHUFGC1/wALLLeaksPerHUFGC1[Group == "Total Population"],
         wALLLeaksPerHURRFGC2 = wALLLeaksPerHUFGC2/wALLLeaksPerHUFGC2[Group == "Total Population"],
         wALLLeaksPerHURRFGC3 = wALLLeaksPerHUFGC3/wALLLeaksPerHUFGC3[Group == "Total Population"],
         wDaysToRepairAvgRRFG = wDaysToRepairAvgFG/wDaysToRepairAvgFG[Group == "Total Population"],
         # wDaysToRepairAvgRRFGC1 = wDaysToRepairAvgFGC1/wDaysToRepairAvgFGC1[Group == "Total Population"],
         wDaysToRepairAvgRRFGC2 = wDaysToRepairAvgFGC2/wDaysToRepairAvgFGC2[Group == "Total Population"],
         wDaysToRepairAvgRRFGC3 = wDaysToRepairAvgFGC3/wDaysToRepairAvgFGC3[Group == "Total Population"],
         wLeakAgeDaysAvgRRFG = wLeakAgeDaysAvgFG/wLeakAgeDaysAvgFG[Group == "Total Population"],
         # wLeakAgeDaysAvgRRFGC1 = wLeakAgeDaysAvgFGC1/wLeakAgeDaysAvgFGC1[Group == "Total Population"],
         # wLeakAgeDaysAvgRRFGC2 = wLeakAgeDaysAvgFGC2/wLeakAgeDaysAvgFGC2[Group == "Total Population"],
         wLeakAgeDaysAvgRRFGC3 = wLeakAgeDaysAvgFGC3/wLeakAgeDaysAvgFGC3[Group == "Total Population"])

ppLeakDensityFG_LEH_df <- ppLeakDensityFG %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRRFG = wLeaksPerSqKmFG/wLeaksPerSqKmFG[Group == "Total Households"],
         # wLeaksRRFGC1 = wLeaksPerSqKmFGC1/wLeaksPerSqKmFGC1[Group == "Total Households"],
         # wLeaksRRFGC2 = wLeaksPerSqKmFGC2/wLeaksPerSqKmFGC2[Group == "Total Households"],
         wLeaksRRFGC2 = 0,
         wLeaksRRFGC3 = wLeaksPerSqKmFGC3/wLeaksPerSqKmFGC3[Group == "Total Households"],
         wLeaksRRrepairFG = wLeaksPerSqKmREPFG/wLeaksPerSqKmREPFG[Group == "Total Households"],
         # wLeaksRRrepairFGC1 = wLeaksPerSqKmREPFGC1/wLeaksPerSqKmREPFGC1[Group == "Total Households"],
         wLeaksRRrepairFGC2 = wLeaksPerSqKmREPFGC2/wLeaksPerSqKmREPFGC2[Group == "Total Households"],
         wLeaksRRrepairFGC3 = wLeaksPerSqKmREPFGC3/wLeaksPerSqKmREPFGC3[Group == "Total Households"],
         wLeaksRRtotalFG = wLeaksPerSqKmALLFG/wLeaksPerSqKmALLFG[Group == "Total Households"],
         # wLeaksRRtotalFGC1 = wLeaksPerSqKmALLFGC1/wLeaksPerSqKmALLFGC1[Group == "Total Households"],
         wLeaksRRtotalFGC2 = wLeaksPerSqKmALLFGC2/wLeaksPerSqKmALLFGC2[Group == "Total Households"],
         wLeaksRRtotalFGC3 = wLeaksPerSqKmALLFGC3/wLeaksPerSqKmALLFGC3[Group == "Total Households"],
         wLeaksPerHURRFG = wLeaksPerHUFG/wLeaksPerHUFG[Group == "Total Households"],
         # wLeaksPerHURRFGC1 = wLeaksPerHUFGC1/wLeaksPerHUFGC1[Group == "Total Households"],
         # wLeaksPerHURRFGC2 = wLeaksPerHUFGC2/wLeaksPerHUFGC2[Group == "Total Households"],
         wLeaksPerHURRFGC2 = 0,
         wLeaksPerHURRFGC3 = wLeaksPerHUFGC3/wLeaksPerHUFGC3[Group == "Total Households"],
         wREPLeaksPerHURRFG = wREPLeaksPerHUFG/wREPLeaksPerHUFG[Group == "Total Households"],
         # wREPLeaksPerHURRFGC1 = wREPLeaksPerHUFGC1/wREPLeaksPerHUFGC1[Group == "Total Households"],
         wREPLeaksPerHURRFGC2 = wREPLeaksPerHUFGC2/wREPLeaksPerHUFGC2[Group == "Total Households"],
         wREPLeaksPerHURRFGC3 = wREPLeaksPerHUFGC3/wREPLeaksPerHUFGC3[Group == "Total Households"],
         wALLLeaksPerHURRFG = wALLLeaksPerHUFG/wALLLeaksPerHUFG[Group == "Total Households"],
         # wALLLeaksPerHURRFGC1 = wALLLeaksPerHUFGC1/wALLLeaksPerHUFGC1[Group == "Total Households"],
         wALLLeaksPerHURRFGC2 = wALLLeaksPerHUFGC2/wALLLeaksPerHUFGC2[Group == "Total Households"],
         wALLLeaksPerHURRFGC3 = wALLLeaksPerHUFGC3/wALLLeaksPerHUFGC3[Group == "Total Households"],
         wDaysToRepairAvgRRFG = wDaysToRepairAvgFG/wDaysToRepairAvgFG[Group == "Total Households"],
         # wDaysToRepairAvgRRFGC1 = wDaysToRepairAvgFGC1/wDaysToRepairAvgFGC1[Group == "Total Households"],
         wDaysToRepairAvgRRFGC2 = wDaysToRepairAvgFGC2/wDaysToRepairAvgFGC2[Group == "Total Households"],
         wDaysToRepairAvgRRFGC3 = wDaysToRepairAvgFGC3/wDaysToRepairAvgFGC3[Group == "Total Households"],
         wLeakAgeDaysAvgRRFG = wLeakAgeDaysAvgFG/wLeakAgeDaysAvgFG[Group == "Total Households"],
         # wLeakAgeDaysAvgRRFGC1 = wLeakAgeDaysAvgFGC1/wLeakAgeDaysAvgFGC1[Group == "Total Households"],
         # wLeakAgeDaysAvgRRFGC2 = wLeakAgeDaysAvgFGC2/wLeakAgeDaysAvgFGC2[Group == "Total Households"],
         wLeakAgeDaysAvgRRFGC3 = wLeakAgeDaysAvgFGC3/wLeakAgeDaysAvgFGC3[Group == "Total Households"])

ppLeakDensityFG_renters_df <- ppLeakDensityFG %>% 
  filter(Group %in% c("Renter Occupied HU", "Total Occupied HU")) %>% 
  mutate(wLeaksRRFG = wLeaksPerSqKmFG/wLeaksPerSqKmFG[Group == "Total Occupied HU"],
         # wLeaksRRFGC1 = wLeaksPerSqKmFGC1/wLeaksPerSqKmFGC1[Group == "Total Occupied HU"],
         # wLeaksRRFGC2 = wLeaksPerSqKmFGC2/wLeaksPerSqKmFGC2[Group == "Total Occupied HU"],
         wLeaksRRFGC2 = 0,
         wLeaksRRFGC3 = wLeaksPerSqKmFGC3/wLeaksPerSqKmFGC3[Group == "Total Occupied HU"],
         wLeaksRRrepairFG = wLeaksPerSqKmREPFG/wLeaksPerSqKmREPFG[Group == "Total Occupied HU"],
         # wLeaksRRrepairFGC1 = wLeaksPerSqKmREPFGC1/wLeaksPerSqKmREPFGC1[Group == "Total Occupied HU"],
         wLeaksRRrepairFGC2 = wLeaksPerSqKmREPFGC2/wLeaksPerSqKmREPFGC2[Group == "Total Occupied HU"],
         wLeaksRRrepairFGC3 = wLeaksPerSqKmREPFGC3/wLeaksPerSqKmREPFGC3[Group == "Total Occupied HU"],
         wLeaksRRtotalFG = wLeaksPerSqKmALLFG/wLeaksPerSqKmALLFG[Group == "Total Occupied HU"],
         # wLeaksRRtotalFGC1 = wLeaksPerSqKmALLFGC1/wLeaksPerSqKmALLFGC1[Group == "Total Occupied HU"],
         wLeaksRRtotalFGC2 = wLeaksPerSqKmALLFGC2/wLeaksPerSqKmALLFGC2[Group == "Total Occupied HU"],
         wLeaksRRtotalFGC3 = wLeaksPerSqKmALLFGC3/wLeaksPerSqKmALLFGC3[Group == "Total Occupied HU"],
         wLeaksPerHURRFG = wLeaksPerHUFG/wLeaksPerHUFG[Group == "Total Occupied HU"],
         # wLeaksPerHURRFGC1 = wLeaksPerHUFGC1/wLeaksPerHUFGC1[Group == "Total Occupied HU"],
         # wLeaksPerHURRFGC2 = wLeaksPerHUFGC2/wLeaksPerHUFGC2[Group == "Total Occupied HU"],
         wLeaksPerHURRFGC2 = 0,
         wLeaksPerHURRFGC3 = wLeaksPerHUFGC3/wLeaksPerHUFGC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURRFG = wREPLeaksPerHUFG/wREPLeaksPerHUFG[Group == "Total Occupied HU"],
         # wREPLeaksPerHURRFGC1 = wREPLeaksPerHUFGC1/wREPLeaksPerHUFGC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURRFGC2 = wREPLeaksPerHUFGC2/wREPLeaksPerHUFGC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURRFGC3 = wREPLeaksPerHUFGC3/wREPLeaksPerHUFGC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURRFG = wALLLeaksPerHUFG/wALLLeaksPerHUFG[Group == "Total Occupied HU"],
         # wALLLeaksPerHURRFGC1 = wALLLeaksPerHUFGC1/wALLLeaksPerHUFGC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURRFGC2 = wALLLeaksPerHUFGC2/wALLLeaksPerHUFGC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURRFGC3 = wALLLeaksPerHUFGC3/wALLLeaksPerHUFGC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRFG = wDaysToRepairAvgFG/wDaysToRepairAvgFG[Group == "Total Occupied HU"],
         # wDaysToRepairAvgRRFGC1 = wDaysToRepairAvgFGC1/wDaysToRepairAvgFGC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRFGC2 = wDaysToRepairAvgFGC2/wDaysToRepairAvgFGC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRFGC3 = wDaysToRepairAvgFGC3/wDaysToRepairAvgFGC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRFG = wLeakAgeDaysAvgFG/wLeakAgeDaysAvgFG[Group == "Total Occupied HU"],
         # wLeakAgeDaysAvgRRFGC1 = wLeakAgeDaysAvgFGC1/wLeakAgeDaysAvgFGC1[Group == "Total Occupied HU"],
         # wLeakAgeDaysAvgRRFGC2 = wLeakAgeDaysAvgFGC2/wLeakAgeDaysAvgFGC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRFGC3 = wLeakAgeDaysAvgFGC3/wLeakAgeDaysAvgFGC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensityFG_df <- rbind(ppLeakDensityFG_pops_df,
                            ppLeakDensityFG_LEH_df,
                            ppLeakDensityFG_renters_df)




### Liberty Utilities
ppLeakDensityLU <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(GAS == "Liberty Utilities") %>% # limit to BGs in LU svc area
  mutate(leaks_sqkmLU = (`Liberty Utilities_19unrepaired`)/area_sqkm,
         # leaks_sqkmLUC1 = (`Liberty Utilities_19unrepairedC1`)/area_sqkm,
         leaks_sqkmLUC1 = 0,
         leaks_sqkmLUC2 = (`Liberty Utilities_19unrepairedC2`)/area_sqkm,
         leaks_sqkmLUC3 = (`Liberty Utilities_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmLU = (`Liberty Utilities_19repaired`)/area_sqkm,
         REPleaks_sqkmLUC1 = (`Liberty Utilities_19repairedC1`)/area_sqkm,
         REPleaks_sqkmLUC2 = (`Liberty Utilities_19repairedC2`)/area_sqkm,
         REPleaks_sqkmLUC3 = (`Liberty Utilities_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmLU = (`Liberty Utilities_19unrepaired` +
                                  `Liberty Utilities_19repaired`)/area_sqkm,
         # AllLeaks2019_sqkmLUC1 = (`Liberty Utilities_19unrepairedC1` +
         #                            `Liberty Utilities_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmLUC1 = (`Liberty Utilities_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmLUC2 = (`Liberty Utilities_19unrepairedC2` + 
                                    `Liberty Utilities_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmLUC3 = (`Liberty Utilities_19unrepairedC3` + 
                                    `Liberty Utilities_19repairedC3`)/area_sqkm,
         leaks_huLU = if_else(total_occ_unitsE == 0, 0, 
                              (`Liberty Utilities_19unrepaired`)/total_occ_unitsE),
         # leaks_huLUC1 = if_else(total_occ_unitsE == 0, 0,
         #                        (`Liberty Utilities_19unrepairedC1`)/total_occ_unitsE),
         leaks_huLUC1 = 0,
         leaks_huLUC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`Liberty Utilities_19unrepairedC2`)/total_occ_unitsE),
         leaks_huLUC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Liberty Utilities_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huLU = if_else(total_occ_unitsE == 0, 0,
                                 (`Liberty Utilities_19repaired`)/total_occ_unitsE),
         REPleaks_huLUC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`Liberty Utilities_19repairedC1`)/total_occ_unitsE),
         REPleaks_huLUC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Liberty Utilities_19repairedC2`)/total_occ_unitsE),
         REPleaks_huLUC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Liberty Utilities_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huLU = if_else(total_occ_unitsE == 0, 0,
                                     (`Liberty Utilities_19unrepaired` + 
                                        `Liberty Utilities_19repaired`)/total_occ_unitsE),
         # AllLeaks2019_huLUC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Liberty Utilities_19unrepairedC1` +
         #                                  `Liberty Utilities_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huLUC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`Liberty Utilities_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huLUC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`Liberty Utilities_19unrepairedC2` +
                                          `Liberty Utilities_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huLUC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Liberty Utilities_19unrepairedC3` +
                                          `Liberty Utilities_19repairedC3`)/total_occ_unitsE),
         PctRepaired19LU = (`Liberty Utilities_19repaired`)/ (`Liberty Utilities_19unrepaired` + `Liberty Utilities_19repaired`)*100,
         DaysToRepairAvgLU = `Liberty Utilities_19repairedDaysAvg`,
         DaysToRepairAvgLUC1 = `Liberty Utilities_19repairedDaysAvgC1`,
         DaysToRepairAvgLUC2 = `Liberty Utilities_19repairedDaysAvgC2`,
         DaysToRepairAvgLUC3 = `Liberty Utilities_19repairedDaysAvgC3`,
         LeakAgeDaysAvgLU = `Liberty Utilities_19unrepairedDaysAvg`,
         # LeakAgeDaysAvgLUC1 = `Liberty Utilities_19unrepairedDaysAvgC1`,
         LeakAgeDaysAvgLUC1 = NA,
         LeakAgeDaysAvgLUC2 = `Liberty Utilities_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgLUC3 = `Liberty Utilities_19unrepairedDaysAvgC3`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), (starts_with("leaks_") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("AllLeaks") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))),
         (starts_with("REPleaks_") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("PctRepaired19") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))),
         (starts_with("leaks_hu") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3")))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmLU = weighted.mean(x = leaks_sqkmLU, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmLUC1 = weighted.mean(x = leaks_sqkmLUC1, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmLUC2 = weighted.mean(x = leaks_sqkmLUC2, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmLUC3 = weighted.mean(x = leaks_sqkmLUC3, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmREPLU = weighted.mean(x = REPleaks_sqkmLU, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPLUC1 = weighted.mean(x = REPleaks_sqkmLUC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPLUC2 = weighted.mean(x = REPleaks_sqkmLUC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPLUC3 = weighted.mean(x = REPleaks_sqkmLUC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLLU = weighted.mean(x = AllLeaks2019_sqkmLU, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLLUC1 = weighted.mean(x = AllLeaks2019_sqkmLUC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLLUC2 = weighted.mean(x = AllLeaks2019_sqkmLUC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLLUC3 = weighted.mean(x = AllLeaks2019_sqkmLUC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgLU = weighted.mean(x = LeakAgeDaysAvgLU,
                                              w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgLUC1 = weighted.mean(x = LeakAgeDaysAvgLUC1,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgLUC2 = weighted.mean(x = LeakAgeDaysAvgLUC2,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgLUC3 = weighted.mean(x = LeakAgeDaysAvgLUC3,
                                                w = Pop, na.rm = TRUE),
            wLeaksPerHULU = weighted.mean(x = leaks_huLU, w = Pop, 
                                          na.rm = T),
            wLeaksPerHULUC1 = weighted.mean(x = leaks_huLUC1, w = Pop, 
                                            na.rm = T),
            wLeaksPerHULUC2 = weighted.mean(x = leaks_huLUC2, w = Pop, 
                                            na.rm = T),
            wLeaksPerHULUC3 = weighted.mean(x = leaks_huLUC3, w = Pop, 
                                            na.rm = T),
            wREPLeaksPerHULU = weighted.mean(x = REPleaks_huLU, 
                                             w = Pop, na.rm = T),
            wREPLeaksPerHULUC1 = weighted.mean(x = REPleaks_huLUC1, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHULUC2 = weighted.mean(x = REPleaks_huLUC2, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHULUC3 = weighted.mean(x = REPleaks_huLUC3, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHULU = weighted.mean(x = AllLeaks2019_huLU, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHULUC1 = weighted.mean(x = AllLeaks2019_huLUC1, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHULUC2 = weighted.mean(x = AllLeaks2019_huLUC2, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHULUC3 = weighted.mean(x = AllLeaks2019_huLUC3, 
                                               w = Pop, na.rm = T),
            wPctRepaired19LU = weighted.mean(x = PctRepaired19LU, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgLU = weighted.mean(x = DaysToRepairAvgLU, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgLUC1 = weighted.mean(x = DaysToRepairAvgLUC1, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgLUC2 = weighted.mean(x = DaysToRepairAvgLUC2, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgLUC3 = weighted.mean(x = DaysToRepairAvgLUC3, 
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "disabledOver18E" = "Disabled Adults",
                        "house_burdened_E" = "Housing Burdened"))

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityLU_pops_df <- ppLeakDensityLU %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "Housing Burdened",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRRLU = wLeaksPerSqKmLU/wLeaksPerSqKmLU[Group == "Total Population"],
         wLeaksRRLUC1 = wLeaksPerSqKmLUC1/wLeaksPerSqKmLUC1[Group == "Total Population"],
         wLeaksRRLUC2 = wLeaksPerSqKmLUC2/wLeaksPerSqKmLUC2[Group == "Total Population"],
         wLeaksRRLUC3 = wLeaksPerSqKmLUC3/wLeaksPerSqKmLUC3[Group == "Total Population"],
         wLeaksRRrepairLU = wLeaksPerSqKmREPLU/wLeaksPerSqKmREPLU[Group == "Total Population"],
         wLeaksRRrepairLUC1 = wLeaksPerSqKmREPLUC1/wLeaksPerSqKmREPLUC1[Group == "Total Population"],
         wLeaksRRrepairLUC2 = wLeaksPerSqKmREPLUC2/wLeaksPerSqKmREPLUC2[Group == "Total Population"],
         wLeaksRRrepairLUC3 = wLeaksPerSqKmREPLUC3/wLeaksPerSqKmREPLUC3[Group == "Total Population"],
         wLeaksRRtotalLU = wLeaksPerSqKmALLLU/wLeaksPerSqKmALLLU[Group == "Total Population"],
         wLeaksRRtotalLUC1 = wLeaksPerSqKmALLLUC1/wLeaksPerSqKmALLLUC1[Group == "Total Population"],
         wLeaksRRtotalLUC2 = wLeaksPerSqKmALLLUC2/wLeaksPerSqKmALLLUC2[Group == "Total Population"],
         wLeaksRRtotalLUC3 = wLeaksPerSqKmALLLUC3/wLeaksPerSqKmALLLUC3[Group == "Total Population"],
         wLeaksPerHURRLU = wLeaksPerHULU/wLeaksPerHULU[Group == "Total Population"],
         wLeaksPerHURRLUC1 = wLeaksPerHULUC1/wLeaksPerHULUC1[Group == "Total Population"],
         wLeaksPerHURRLUC2 = wLeaksPerHULUC2/wLeaksPerHULUC2[Group == "Total Population"],
         wLeaksPerHURRLUC3 = wLeaksPerHULUC3/wLeaksPerHULUC3[Group == "Total Population"],
         wREPLeaksPerHURRLU = wREPLeaksPerHULU/wREPLeaksPerHULU[Group == "Total Population"],
         wREPLeaksPerHURRLUC1 = wREPLeaksPerHULUC1/wREPLeaksPerHULUC1[Group == "Total Population"],
         wREPLeaksPerHURRLUC2 = wREPLeaksPerHULUC2/wREPLeaksPerHULUC2[Group == "Total Population"],
         wREPLeaksPerHURRLUC3 = wREPLeaksPerHULUC3/wREPLeaksPerHULUC3[Group == "Total Population"],
         wALLLeaksPerHURRLU = wALLLeaksPerHULU/wALLLeaksPerHULU[Group == "Total Population"],
         wALLLeaksPerHURRLUC1 = wALLLeaksPerHULUC1/wALLLeaksPerHULUC1[Group == "Total Population"],
         wALLLeaksPerHURRLUC2 = wALLLeaksPerHULUC2/wALLLeaksPerHULUC2[Group == "Total Population"],
         wALLLeaksPerHURRLUC3 = wALLLeaksPerHULUC3/wALLLeaksPerHULUC3[Group == "Total Population"],
         wDaysToRepairAvgRRLU = wDaysToRepairAvgLU/wDaysToRepairAvgLU[Group == "Total Population"],
         wDaysToRepairAvgRRLUC1 = wDaysToRepairAvgLUC1/wDaysToRepairAvgLUC1[Group == "Total Population"],
         wDaysToRepairAvgRRLUC2 = wDaysToRepairAvgLUC2/wDaysToRepairAvgLUC2[Group == "Total Population"],
         wDaysToRepairAvgRRLUC3 = wDaysToRepairAvgLUC3/wDaysToRepairAvgLUC3[Group == "Total Population"],
         wLeakAgeDaysAvgRRLU = wLeakAgeDaysAvgLU/wLeakAgeDaysAvgLU[Group == "Total Population"],
         wLeakAgeDaysAvgRRLUC1 = wLeakAgeDaysAvgLUC1/wLeakAgeDaysAvgLUC1[Group == "Total Population"],
         wLeakAgeDaysAvgRRLUC2 = wLeakAgeDaysAvgLUC2/wLeakAgeDaysAvgLUC2[Group == "Total Population"],
         wLeakAgeDaysAvgRRLUC3 = wLeakAgeDaysAvgLUC3/wLeakAgeDaysAvgLUC3[Group == "Total Population"])

ppLeakDensityLU_LEH_df <- ppLeakDensityLU %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRRLU = wLeaksPerSqKmLU/wLeaksPerSqKmLU[Group == "Total Households"],
         wLeaksRRLUC1 = wLeaksPerSqKmLUC1/wLeaksPerSqKmLUC1[Group == "Total Households"],
         wLeaksRRLUC2 = wLeaksPerSqKmLUC2/wLeaksPerSqKmLUC2[Group == "Total Households"],
         wLeaksRRLUC3 = wLeaksPerSqKmLUC3/wLeaksPerSqKmLUC3[Group == "Total Households"],
         wLeaksRRrepairLU = wLeaksPerSqKmREPLU/wLeaksPerSqKmREPLU[Group == "Total Households"],
         wLeaksRRrepairLUC1 = wLeaksPerSqKmREPLUC1/wLeaksPerSqKmREPLUC1[Group == "Total Households"],
         wLeaksRRrepairLUC2 = wLeaksPerSqKmREPLUC2/wLeaksPerSqKmREPLUC2[Group == "Total Households"],
         wLeaksRRrepairLUC3 = wLeaksPerSqKmREPLUC3/wLeaksPerSqKmREPLUC3[Group == "Total Households"],
         wLeaksRRtotalLU = wLeaksPerSqKmALLLU/wLeaksPerSqKmALLLU[Group == "Total Households"],
         wLeaksRRtotalLUC1 = wLeaksPerSqKmALLLUC1/wLeaksPerSqKmALLLUC1[Group == "Total Households"],
         wLeaksRRtotalLUC2 = wLeaksPerSqKmALLLUC2/wLeaksPerSqKmALLLUC2[Group == "Total Households"],
         wLeaksRRtotalLUC3 = wLeaksPerSqKmALLLUC3/wLeaksPerSqKmALLLUC3[Group == "Total Households"],
         wLeaksPerHURRLU = wLeaksPerHULU/wLeaksPerHULU[Group == "Total Households"],
         wLeaksPerHURRLUC1 = wLeaksPerHULUC1/wLeaksPerHULUC1[Group == "Total Households"],
         wLeaksPerHURRLUC2 = wLeaksPerHULUC2/wLeaksPerHULUC2[Group == "Total Households"],
         wLeaksPerHURRLUC3 = wLeaksPerHULUC3/wLeaksPerHULUC3[Group == "Total Households"],
         wREPLeaksPerHURRLU = wREPLeaksPerHULU/wREPLeaksPerHULU[Group == "Total Households"],
         wREPLeaksPerHURRLUC1 = wREPLeaksPerHULUC1/wREPLeaksPerHULUC1[Group == "Total Households"],
         wREPLeaksPerHURRLUC2 = wREPLeaksPerHULUC2/wREPLeaksPerHULUC2[Group == "Total Households"],
         wREPLeaksPerHURRLUC3 = wREPLeaksPerHULUC3/wREPLeaksPerHULUC3[Group == "Total Households"],
         wALLLeaksPerHURRLU = wALLLeaksPerHULU/wALLLeaksPerHULU[Group == "Total Households"],
         wALLLeaksPerHURRLUC1 = wALLLeaksPerHULUC1/wALLLeaksPerHULUC1[Group == "Total Households"],
         wALLLeaksPerHURRLUC2 = wALLLeaksPerHULUC2/wALLLeaksPerHULUC2[Group == "Total Households"],
         wALLLeaksPerHURRLUC3 = wALLLeaksPerHULUC3/wALLLeaksPerHULUC3[Group == "Total Households"],
         wDaysToRepairAvgRRLU = wDaysToRepairAvgLU/wDaysToRepairAvgLU[Group == "Total Households"],
         wDaysToRepairAvgRRLUC1 = wDaysToRepairAvgLUC1/wDaysToRepairAvgLUC1[Group == "Total Households"],
         wDaysToRepairAvgRRLUC2 = wDaysToRepairAvgLUC2/wDaysToRepairAvgLUC2[Group == "Total Households"],
         wDaysToRepairAvgRRLUC3 = wDaysToRepairAvgLUC3/wDaysToRepairAvgLUC3[Group == "Total Households"],
         wLeakAgeDaysAvgRRLU = wLeakAgeDaysAvgLU/wLeakAgeDaysAvgLU[Group == "Total Households"],
         wLeakAgeDaysAvgRRLUC1 = wLeakAgeDaysAvgLUC1/wLeakAgeDaysAvgLUC1[Group == "Total Households"],
         wLeakAgeDaysAvgRRLUC2 = wLeakAgeDaysAvgLUC2/wLeakAgeDaysAvgLUC2[Group == "Total Households"],
         wLeakAgeDaysAvgRRLUC3 = wLeakAgeDaysAvgLUC3/wLeakAgeDaysAvgLUC3[Group == "Total Households"])

ppLeakDensityLU_renters_df <- ppLeakDensityLU %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened",
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRRLU = wLeaksPerSqKmLU/wLeaksPerSqKmLU[Group == "Total Occupied HU"],
         wLeaksRRLUC1 = wLeaksPerSqKmLUC1/wLeaksPerSqKmLUC1[Group == "Total Occupied HU"],
         wLeaksRRLUC2 = wLeaksPerSqKmLUC2/wLeaksPerSqKmLUC2[Group == "Total Occupied HU"],
         wLeaksRRLUC3 = wLeaksPerSqKmLUC3/wLeaksPerSqKmLUC3[Group == "Total Occupied HU"],
         wLeaksRRrepairLU = wLeaksPerSqKmREPLU/wLeaksPerSqKmREPLU[Group == "Total Occupied HU"],
         wLeaksRRrepairLUC1 = wLeaksPerSqKmREPLUC1/wLeaksPerSqKmREPLUC1[Group == "Total Occupied HU"],
         wLeaksRRrepairLUC2 = wLeaksPerSqKmREPLUC2/wLeaksPerSqKmREPLUC2[Group == "Total Occupied HU"],
         wLeaksRRrepairLUC3 = wLeaksPerSqKmREPLUC3/wLeaksPerSqKmREPLUC3[Group == "Total Occupied HU"],
         wLeaksRRtotalLU = wLeaksPerSqKmALLLU/wLeaksPerSqKmALLLU[Group == "Total Occupied HU"],
         wLeaksRRtotalLUC1 = wLeaksPerSqKmALLLUC1/wLeaksPerSqKmALLLUC1[Group == "Total Occupied HU"],
         wLeaksRRtotalLUC2 = wLeaksPerSqKmALLLUC2/wLeaksPerSqKmALLLUC2[Group == "Total Occupied HU"],
         wLeaksRRtotalLUC3 = wLeaksPerSqKmALLLUC3/wLeaksPerSqKmALLLUC3[Group == "Total Occupied HU"],
         wLeaksPerHURRLU = wLeaksPerHULU/wLeaksPerHULU[Group == "Total Occupied HU"],
         wLeaksPerHURRLUC1 = wLeaksPerHULUC1/wLeaksPerHULUC1[Group == "Total Occupied HU"],
         wLeaksPerHURRLUC2 = wLeaksPerHULUC2/wLeaksPerHULUC2[Group == "Total Occupied HU"],
         wLeaksPerHURRLUC3 = wLeaksPerHULUC3/wLeaksPerHULUC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURRLU = wREPLeaksPerHULU/wREPLeaksPerHULU[Group == "Total Occupied HU"],
         wREPLeaksPerHURRLUC1 = wREPLeaksPerHULUC1/wREPLeaksPerHULUC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURRLUC2 = wREPLeaksPerHULUC2/wREPLeaksPerHULUC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURRLUC3 = wREPLeaksPerHULUC3/wREPLeaksPerHULUC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURRLU = wALLLeaksPerHULU/wALLLeaksPerHULU[Group == "Total Occupied HU"],
         wALLLeaksPerHURRLUC1 = wALLLeaksPerHULUC1/wALLLeaksPerHULUC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURRLUC2 = wALLLeaksPerHULUC2/wALLLeaksPerHULUC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURRLUC3 = wALLLeaksPerHULUC3/wALLLeaksPerHULUC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRLU = wDaysToRepairAvgLU/wDaysToRepairAvgLU[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRLUC1 = wDaysToRepairAvgLUC1/wDaysToRepairAvgLUC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRLUC2 = wDaysToRepairAvgLUC2/wDaysToRepairAvgLUC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRLUC3 = wDaysToRepairAvgLUC3/wDaysToRepairAvgLUC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRLU = wLeakAgeDaysAvgLU/wLeakAgeDaysAvgLU[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRLUC1 = wLeakAgeDaysAvgLUC1/wLeakAgeDaysAvgLUC1[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRLUC2 = wLeakAgeDaysAvgLUC2/wLeakAgeDaysAvgLUC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRLUC3 = wLeakAgeDaysAvgLUC3/wLeakAgeDaysAvgLUC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensityLU_df <- rbind(ppLeakDensityLU_pops_df,
                            ppLeakDensityLU_LEH_df,
                            ppLeakDensityLU_renters_df)




### Berkshire Gas
ppLeakDensityBG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(GAS == "The Berkshire Gas Company") %>% # limit to BGs in BG svc area
  mutate(leaks_sqkmBG = (`Berkshire Gas_19unrepaired`)/area_sqkm,
         # leaks_sqkmBGC1 = (`Berkshire Gas_19unrepairedC1`)/area_sqkm,
         leaks_sqkmBGC1 = 0,
         leaks_sqkmBGC2 = (`Berkshire Gas_19unrepairedC2`)/area_sqkm,
         leaks_sqkmBGC3 = (`Berkshire Gas_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmBG = (`Berkshire Gas_19repaired`)/area_sqkm,
         REPleaks_sqkmBGC1 = (`Berkshire Gas_19repairedC1`)/area_sqkm,
         REPleaks_sqkmBGC2 = (`Berkshire Gas_19repairedC2`)/area_sqkm,
         REPleaks_sqkmBGC3 = (`Berkshire Gas_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmBG = (`Berkshire Gas_19unrepaired` +
                                  `Berkshire Gas_19repaired`)/area_sqkm,
         # AllLeaks2019_sqkmBGC1 = (`Berkshire Gas_19unrepairedC1` +
         #                            `Berkshire Gas_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmBGC1 = (`Berkshire Gas_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmBGC2 = (`Berkshire Gas_19unrepairedC2` + 
                                    `Berkshire Gas_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmBGC3 = (`Berkshire Gas_19unrepairedC3` + 
                                    `Berkshire Gas_19repairedC3`)/area_sqkm,
         leaks_huBG = if_else(total_occ_unitsE == 0, 0, 
                              (`Berkshire Gas_19unrepaired`)/total_occ_unitsE),
         # leaks_huBGC1 = if_else(total_occ_unitsE == 0, 0,
         #                        (`Berkshire Gas_19unrepairedC1`)/total_occ_unitsE),
         leaks_huBGC1 = 0,
         leaks_huBGC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`Berkshire Gas_19unrepairedC2`)/total_occ_unitsE),
         leaks_huBGC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Berkshire Gas_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huBG = if_else(total_occ_unitsE == 0, 0,
                                 (`Berkshire Gas_19repaired`)/total_occ_unitsE),
         REPleaks_huBGC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`Berkshire Gas_19repairedC1`)/total_occ_unitsE),
         REPleaks_huBGC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Berkshire Gas_19repairedC2`)/total_occ_unitsE),
         REPleaks_huBGC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Berkshire Gas_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huBG = if_else(total_occ_unitsE == 0, 0,
                                     (`Berkshire Gas_19unrepaired` + 
                                        `Berkshire Gas_19repaired`)/total_occ_unitsE),
         # AllLeaks2019_huBGC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Berkshire Gas_19unrepairedC1` +
         #                                  `Berkshire Gas_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huBGC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`Berkshire Gas_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huBGC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`Berkshire Gas_19unrepairedC2` +
                                          `Berkshire Gas_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huBGC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Berkshire Gas_19unrepairedC3` +
                                          `Berkshire Gas_19repairedC3`)/total_occ_unitsE),
         PctRepaired19BG = (`Berkshire Gas_19repaired`)/ (`Berkshire Gas_19unrepaired` + `Berkshire Gas_19repaired`)*100,
         DaysToRepairAvgBG = `Berkshire Gas_19repairedDaysAvg`,
         DaysToRepairAvgBGC1 = `Berkshire Gas_19repairedDaysAvgC1`,
         DaysToRepairAvgBGC2 = `Berkshire Gas_19repairedDaysAvgC2`,
         DaysToRepairAvgBGC3 = `Berkshire Gas_19repairedDaysAvgC3`,
         LeakAgeDaysAvgBG = `Berkshire Gas_19unrepairedDaysAvg`,
         # LeakAgeDaysAvgBGC1 = `Berkshire Gas_19unrepairedDaysAvgC1`,
         LeakAgeDaysAvgBGC1 = NA,
         LeakAgeDaysAvgBGC2 = `Berkshire Gas_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgBGC3 = `Berkshire Gas_19unrepairedDaysAvgC3`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
         ends_with("unitsE"), (starts_with("leaks_") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("AllLeaks") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))),
         (starts_with("REPleaks_") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))),
         (starts_with("leaks_hu") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3")))) %>% 
  pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmBG = weighted.mean(x = leaks_sqkmBG, w = Pop, 
                                            na.rm = TRUE),
            wLeaksPerSqKmBGC1 = weighted.mean(x = leaks_sqkmBGC1, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmBGC2 = weighted.mean(x = leaks_sqkmBGC2, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmBGC3 = weighted.mean(x = leaks_sqkmBGC3, w = Pop, 
                                              na.rm = TRUE),
            wLeaksPerSqKmREPBG = weighted.mean(x = REPleaks_sqkmBG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPBGC1 = weighted.mean(x = REPleaks_sqkmBGC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPBGC2 = weighted.mean(x = REPleaks_sqkmBGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmREPBGC3 = weighted.mean(x = REPleaks_sqkmBGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLBG = weighted.mean(x = AllLeaks2019_sqkmBG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLBGC1 = weighted.mean(x = AllLeaks2019_sqkmBGC1, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLBGC2 = weighted.mean(x = AllLeaks2019_sqkmBGC2, 
                                                 w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLBGC3 = weighted.mean(x = AllLeaks2019_sqkmBGC3, 
                                                 w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgBG = weighted.mean(x = LeakAgeDaysAvgBG,
                                              w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgBGC1 = weighted.mean(x = LeakAgeDaysAvgBGC1,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgBGC2 = weighted.mean(x = LeakAgeDaysAvgBGC2,
                                                w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgBGC3 = weighted.mean(x = LeakAgeDaysAvgBGC3,
                                                w = Pop, na.rm = TRUE),
            wLeaksPerHUBG = weighted.mean(x = leaks_huBG, w = Pop, 
                                          na.rm = T),
            wLeaksPerHUBGC1 = weighted.mean(x = leaks_huBGC1, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUBGC2 = weighted.mean(x = leaks_huBGC2, w = Pop, 
                                            na.rm = T),
            wLeaksPerHUBGC3 = weighted.mean(x = leaks_huBGC3, w = Pop, 
                                            na.rm = T),
            wREPLeaksPerHUBG = weighted.mean(x = REPleaks_huBG, 
                                             w = Pop, na.rm = T),
            wREPLeaksPerHUBGC1 = weighted.mean(x = REPleaks_huBGC1, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUBGC2 = weighted.mean(x = REPleaks_huBGC2, 
                                               w = Pop, na.rm = T),
            wREPLeaksPerHUBGC3 = weighted.mean(x = REPleaks_huBGC3, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUBG = weighted.mean(x = AllLeaks2019_huBG, 
                                             w = Pop, na.rm = T),
            wALLLeaksPerHUBGC1 = weighted.mean(x = AllLeaks2019_huBGC1, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUBGC2 = weighted.mean(x = AllLeaks2019_huBGC2, 
                                               w = Pop, na.rm = T),
            wALLLeaksPerHUBGC3 = weighted.mean(x = AllLeaks2019_huBGC3, 
                                               w = Pop, na.rm = T),
            wPctRepaired19BG = weighted.mean(x = PctRepaired19BG, 
                                             w = Pop, na.rm = T),
            wDaysToRepairAvgBG = weighted.mean(x = DaysToRepairAvgBG, 
                                               w = Pop, na.rm = T),
            wDaysToRepairAvgBGC1 = weighted.mean(x = DaysToRepairAvgBGC1, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgBGC2 = weighted.mean(x = DaysToRepairAvgBGC2, 
                                                 w = Pop, na.rm = T),
            wDaysToRepairAvgBGC3 = weighted.mean(x = DaysToRepairAvgBGC3, 
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "disabledOver18E" = "Disabled Adults",
                        "house_burdened_E" = "Housing Burdened"))

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityBG_pops_df <- ppLeakDensityBG %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "Housing Burdened",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRRBG = wLeaksPerSqKmBG/wLeaksPerSqKmBG[Group == "Total Population"],
         wLeaksRRBGC1 = wLeaksPerSqKmBGC1/wLeaksPerSqKmBGC1[Group == "Total Population"],
         wLeaksRRBGC2 = wLeaksPerSqKmBGC2/wLeaksPerSqKmBGC2[Group == "Total Population"],
         wLeaksRRBGC3 = wLeaksPerSqKmBGC3/wLeaksPerSqKmBGC3[Group == "Total Population"],
         wLeaksRRrepairBG = wLeaksPerSqKmREPBG/wLeaksPerSqKmREPBG[Group == "Total Population"],
         wLeaksRRrepairBGC1 = wLeaksPerSqKmREPBGC1/wLeaksPerSqKmREPBGC1[Group == "Total Population"],
         wLeaksRRrepairBGC2 = wLeaksPerSqKmREPBGC2/wLeaksPerSqKmREPBGC2[Group == "Total Population"],
         wLeaksRRrepairBGC3 = wLeaksPerSqKmREPBGC3/wLeaksPerSqKmREPBGC3[Group == "Total Population"],
         wLeaksRRtotalBG = wLeaksPerSqKmALLBG/wLeaksPerSqKmALLBG[Group == "Total Population"],
         wLeaksRRtotalBGC1 = wLeaksPerSqKmALLBGC1/wLeaksPerSqKmALLBGC1[Group == "Total Population"],
         wLeaksRRtotalBGC2 = wLeaksPerSqKmALLBGC2/wLeaksPerSqKmALLBGC2[Group == "Total Population"],
         wLeaksRRtotalBGC3 = wLeaksPerSqKmALLBGC3/wLeaksPerSqKmALLBGC3[Group == "Total Population"],
         wLeaksPerHURRBG = wLeaksPerHUBG/wLeaksPerHUBG[Group == "Total Population"],
         wLeaksPerHURRBGC1 = wLeaksPerHUBGC1/wLeaksPerHUBGC1[Group == "Total Population"],
         wLeaksPerHURRBGC2 = wLeaksPerHUBGC2/wLeaksPerHUBGC2[Group == "Total Population"],
         wLeaksPerHURRBGC3 = wLeaksPerHUBGC3/wLeaksPerHUBGC3[Group == "Total Population"],
         wREPLeaksPerHURRBG = wREPLeaksPerHUBG/wREPLeaksPerHUBG[Group == "Total Population"],
         wREPLeaksPerHURRBGC1 = wREPLeaksPerHUBGC1/wREPLeaksPerHUBGC1[Group == "Total Population"],
         wREPLeaksPerHURRBGC2 = wREPLeaksPerHUBGC2/wREPLeaksPerHUBGC2[Group == "Total Population"],
         wREPLeaksPerHURRBGC3 = wREPLeaksPerHUBGC3/wREPLeaksPerHUBGC3[Group == "Total Population"],
         wALLLeaksPerHURRBG = wALLLeaksPerHUBG/wALLLeaksPerHUBG[Group == "Total Population"],
         wALLLeaksPerHURRBGC1 = wALLLeaksPerHUBGC1/wALLLeaksPerHUBGC1[Group == "Total Population"],
         wALLLeaksPerHURRBGC2 = wALLLeaksPerHUBGC2/wALLLeaksPerHUBGC2[Group == "Total Population"],
         wALLLeaksPerHURRBGC3 = wALLLeaksPerHUBGC3/wALLLeaksPerHUBGC3[Group == "Total Population"],
         wDaysToRepairAvgRRBG = wDaysToRepairAvgBG/wDaysToRepairAvgBG[Group == "Total Population"],
         wDaysToRepairAvgRRBGC1 = wDaysToRepairAvgBGC1/wDaysToRepairAvgBGC1[Group == "Total Population"],
         wDaysToRepairAvgRRBGC2 = wDaysToRepairAvgBGC2/wDaysToRepairAvgBGC2[Group == "Total Population"],
         wDaysToRepairAvgRRBGC3 = wDaysToRepairAvgBGC3/wDaysToRepairAvgBGC3[Group == "Total Population"],
         wLeakAgeDaysAvgRRBG = wLeakAgeDaysAvgBG/wLeakAgeDaysAvgBG[Group == "Total Population"],
         wLeakAgeDaysAvgRRBGC1 = wLeakAgeDaysAvgBGC1/wLeakAgeDaysAvgBGC1[Group == "Total Population"],
         wLeakAgeDaysAvgRRBGC2 = wLeakAgeDaysAvgBGC2/wLeakAgeDaysAvgBGC2[Group == "Total Population"],
         wLeakAgeDaysAvgRRBGC3 = wLeakAgeDaysAvgBGC3/wLeakAgeDaysAvgBGC3[Group == "Total Population"])

ppLeakDensityBG_LEH_df <- ppLeakDensityBG %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRRBG = wLeaksPerSqKmBG/wLeaksPerSqKmBG[Group == "Total Households"],
         wLeaksRRBGC1 = wLeaksPerSqKmBGC1/wLeaksPerSqKmBGC1[Group == "Total Households"],
         wLeaksRRBGC2 = wLeaksPerSqKmBGC2/wLeaksPerSqKmBGC2[Group == "Total Households"],
         wLeaksRRBGC3 = wLeaksPerSqKmBGC3/wLeaksPerSqKmBGC3[Group == "Total Households"],
         wLeaksRRrepairBG = wLeaksPerSqKmREPBG/wLeaksPerSqKmREPBG[Group == "Total Households"],
         wLeaksRRrepairBGC1 = wLeaksPerSqKmREPBGC1/wLeaksPerSqKmREPBGC1[Group == "Total Households"],
         wLeaksRRrepairBGC2 = wLeaksPerSqKmREPBGC2/wLeaksPerSqKmREPBGC2[Group == "Total Households"],
         wLeaksRRrepairBGC3 = wLeaksPerSqKmREPBGC3/wLeaksPerSqKmREPBGC3[Group == "Total Households"],
         wLeaksRRtotalBG = wLeaksPerSqKmALLBG/wLeaksPerSqKmALLBG[Group == "Total Households"],
         wLeaksRRtotalBGC1 = wLeaksPerSqKmALLBGC1/wLeaksPerSqKmALLBGC1[Group == "Total Households"],
         wLeaksRRtotalBGC2 = wLeaksPerSqKmALLBGC2/wLeaksPerSqKmALLBGC2[Group == "Total Households"],
         wLeaksRRtotalBGC3 = wLeaksPerSqKmALLBGC3/wLeaksPerSqKmALLBGC3[Group == "Total Households"],
         wLeaksPerHURRBG = wLeaksPerHUBG/wLeaksPerHUBG[Group == "Total Households"],
         wLeaksPerHURRBGC1 = wLeaksPerHUBGC1/wLeaksPerHUBGC1[Group == "Total Households"],
         wLeaksPerHURRBGC2 = wLeaksPerHUBGC2/wLeaksPerHUBGC2[Group == "Total Households"],
         wLeaksPerHURRBGC3 = wLeaksPerHUBGC3/wLeaksPerHUBGC3[Group == "Total Households"],
         wREPLeaksPerHURRBG = wREPLeaksPerHUBG/wREPLeaksPerHUBG[Group == "Total Households"],
         wREPLeaksPerHURRBGC1 = wREPLeaksPerHUBGC1/wREPLeaksPerHUBGC1[Group == "Total Households"],
         wREPLeaksPerHURRBGC2 = wREPLeaksPerHUBGC2/wREPLeaksPerHUBGC2[Group == "Total Households"],
         wREPLeaksPerHURRBGC3 = wREPLeaksPerHUBGC3/wREPLeaksPerHUBGC3[Group == "Total Households"],
         wALLLeaksPerHURRBG = wALLLeaksPerHUBG/wALLLeaksPerHUBG[Group == "Total Households"],
         wALLLeaksPerHURRBGC1 = wALLLeaksPerHUBGC1/wALLLeaksPerHUBGC1[Group == "Total Households"],
         wALLLeaksPerHURRBGC2 = wALLLeaksPerHUBGC2/wALLLeaksPerHUBGC2[Group == "Total Households"],
         wALLLeaksPerHURRBGC3 = wALLLeaksPerHUBGC3/wALLLeaksPerHUBGC3[Group == "Total Households"],
         wDaysToRepairAvgRRBG = wDaysToRepairAvgBG/wDaysToRepairAvgBG[Group == "Total Households"],
         wDaysToRepairAvgRRBGC1 = wDaysToRepairAvgBGC1/wDaysToRepairAvgBGC1[Group == "Total Households"],
         wDaysToRepairAvgRRBGC2 = wDaysToRepairAvgBGC2/wDaysToRepairAvgBGC2[Group == "Total Households"],
         wDaysToRepairAvgRRBGC3 = wDaysToRepairAvgBGC3/wDaysToRepairAvgBGC3[Group == "Total Households"],
         wLeakAgeDaysAvgRRBG = wLeakAgeDaysAvgBG/wLeakAgeDaysAvgBG[Group == "Total Households"],
         wLeakAgeDaysAvgRRBGC1 = wLeakAgeDaysAvgBGC1/wLeakAgeDaysAvgBGC1[Group == "Total Households"],
         wLeakAgeDaysAvgRRBGC2 = wLeakAgeDaysAvgBGC2/wLeakAgeDaysAvgBGC2[Group == "Total Households"],
         wLeakAgeDaysAvgRRBGC3 = wLeakAgeDaysAvgBGC3/wLeakAgeDaysAvgBGC3[Group == "Total Households"])

ppLeakDensityBG_renters_df <- ppLeakDensityBG %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened",
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRRBG = wLeaksPerSqKmBG/wLeaksPerSqKmBG[Group == "Total Occupied HU"],
         wLeaksRRBGC1 = wLeaksPerSqKmBGC1/wLeaksPerSqKmBGC1[Group == "Total Occupied HU"],
         wLeaksRRBGC2 = wLeaksPerSqKmBGC2/wLeaksPerSqKmBGC2[Group == "Total Occupied HU"],
         wLeaksRRBGC3 = wLeaksPerSqKmBGC3/wLeaksPerSqKmBGC3[Group == "Total Occupied HU"],
         wLeaksRRrepairBG = wLeaksPerSqKmREPBG/wLeaksPerSqKmREPBG[Group == "Total Occupied HU"],
         wLeaksRRrepairBGC1 = wLeaksPerSqKmREPBGC1/wLeaksPerSqKmREPBGC1[Group == "Total Occupied HU"],
         wLeaksRRrepairBGC2 = wLeaksPerSqKmREPBGC2/wLeaksPerSqKmREPBGC2[Group == "Total Occupied HU"],
         wLeaksRRrepairBGC3 = wLeaksPerSqKmREPBGC3/wLeaksPerSqKmREPBGC3[Group == "Total Occupied HU"],
         wLeaksRRtotalBG = wLeaksPerSqKmALLBG/wLeaksPerSqKmALLBG[Group == "Total Occupied HU"],
         wLeaksRRtotalBGC1 = wLeaksPerSqKmALLBGC1/wLeaksPerSqKmALLBGC1[Group == "Total Occupied HU"],
         wLeaksRRtotalBGC2 = wLeaksPerSqKmALLBGC2/wLeaksPerSqKmALLBGC2[Group == "Total Occupied HU"],
         wLeaksRRtotalBGC3 = wLeaksPerSqKmALLBGC3/wLeaksPerSqKmALLBGC3[Group == "Total Occupied HU"],
         wLeaksPerHURRBG = wLeaksPerHUBG/wLeaksPerHUBG[Group == "Total Occupied HU"],
         wLeaksPerHURRBGC1 = wLeaksPerHUBGC1/wLeaksPerHUBGC1[Group == "Total Occupied HU"],
         wLeaksPerHURRBGC2 = wLeaksPerHUBGC2/wLeaksPerHUBGC2[Group == "Total Occupied HU"],
         wLeaksPerHURRBGC3 = wLeaksPerHUBGC3/wLeaksPerHUBGC3[Group == "Total Occupied HU"],
         wREPLeaksPerHURRBG = wREPLeaksPerHUBG/wREPLeaksPerHUBG[Group == "Total Occupied HU"],
         wREPLeaksPerHURRBGC1 = wREPLeaksPerHUBGC1/wREPLeaksPerHUBGC1[Group == "Total Occupied HU"],
         wREPLeaksPerHURRBGC2 = wREPLeaksPerHUBGC2/wREPLeaksPerHUBGC2[Group == "Total Occupied HU"],
         wREPLeaksPerHURRBGC3 = wREPLeaksPerHUBGC3/wREPLeaksPerHUBGC3[Group == "Total Occupied HU"],
         wALLLeaksPerHURRBG = wALLLeaksPerHUBG/wALLLeaksPerHUBG[Group == "Total Occupied HU"],
         wALLLeaksPerHURRBGC1 = wALLLeaksPerHUBGC1/wALLLeaksPerHUBGC1[Group == "Total Occupied HU"],
         wALLLeaksPerHURRBGC2 = wALLLeaksPerHUBGC2/wALLLeaksPerHUBGC2[Group == "Total Occupied HU"],
         wALLLeaksPerHURRBGC3 = wALLLeaksPerHUBGC3/wALLLeaksPerHUBGC3[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRBG = wDaysToRepairAvgBG/wDaysToRepairAvgBG[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRBGC1 = wDaysToRepairAvgBGC1/wDaysToRepairAvgBGC1[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRBGC2 = wDaysToRepairAvgBGC2/wDaysToRepairAvgBGC2[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRBGC3 = wDaysToRepairAvgBGC3/wDaysToRepairAvgBGC3[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRBG = wLeakAgeDaysAvgBG/wLeakAgeDaysAvgBG[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRBGC1 = wLeakAgeDaysAvgBGC1/wLeakAgeDaysAvgBGC1[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRBGC2 = wLeakAgeDaysAvgBGC2/wLeakAgeDaysAvgBGC2[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRBGC3 = wLeakAgeDaysAvgBGC3/wLeakAgeDaysAvgBGC3[Group == "Total Occupied HU"])

# bring them together
ppLeakDensityBG_df <- rbind(ppLeakDensityBG_pops_df,
                            ppLeakDensityBG_LEH_df,
                            ppLeakDensityBG_renters_df)


### BRING UTILITY DATA TOGETHER
# Join the utility df together
ppLeakDensityJoinedU <- list(ppLeakDensityBG_df,
                             ppLeakDensityCG_df,
                             ppLeakDensityEV_df,
                             ppLeakDensityFG_df,
                             ppLeakDensityLU_df,
                             ppLeakDensityNG_df) %>% 
  reduce(left_join, by = "Group")

# save output
saveRDS(ppLeakDensityJoinedU, file = "Data/ppLeakDensityJoinedU_TR2019.Rds")

# load data
ppLeakDensityJoinedU <- readRDS("Data/ppLeakDensityJoinedU_TR2019.Rds")



#### FIGURES
# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksRR:wLeaksRRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRR" = "All Leaks",
                            "wLeaksRRC1" = "Class 1 Leaks",
                            "wLeaksRRC2" = "Class 2 Leaks",
                            "wLeaksRRC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassRR_tract2019.png")


# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksRRrepair:wLeaksRRrepairC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksRRrepair" = "All Leaks",
                            "wLeaksRRrepairC1" = "Class 1 Leaks",
                            "wLeaksRRrepairC2" = "Class 2 Leaks",
                            "wLeaksRRrepairC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREPRR_tract2019.png")


# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksRRtotal:wLeaksRRtotalC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksRRtotal" = "All Leaks",
                            "wLeaksRRtotalC1" = "Class 1 Leaks",
                            "wLeaksRRtotalC2" = "Class 2 Leaks",
                            "wLeaksRRtotalC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAllRR_tract2019.png")


# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerHURR:wLeaksPerHURRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerHURR" = "All Leaks",
                            "wLeaksPerHURRC1" = "Class 1 Leaks",
                            "wLeaksPerHURRC2" = "Class 2 Leaks",
                            "wLeaksPerHURRC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = "Ratio of group population-weighted mean leaks per occupied housing unit to population mean by Census Tract",
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_RR_tract2019.png")


# faceted dot graph of RR for repaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wREPLeaksPerHURR:wREPLeaksPerHURRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHURR" = "All Leaks",
                            "wREPLeaksPerHURRC1" = "Class 1 Leaks",
                            "wREPLeaksPerHURRC2" = "Class 2 Leaks",
                            "wREPLeaksPerHURRC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = "Ratio of group population-weighted mean leaks per occupied housing unit to population mean by Census Tract",
       title = "Relative Risk of Priority Populations and Repaired Leaks Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_RR_tract2019.png")


# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wALLLeaksPerHURR:wALLLeaksPerHURRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURR" = "All Leaks",
                            "wALLLeaksPerHURRC1" = "Class 1 Leaks",
                            "wALLLeaksPerHURRC2" = "Class 2 Leaks",
                            "wALLLeaksPerHURRC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = "Ratio of group population-weighted mean leaks per occupied housing unit to population mean by Census Tract",
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_RR_tract2019.png")


# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wDaysToRepairAvgRR:wDaysToRepairAvgRRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgRR" = "All Leaks",
                            "wDaysToRepairAvgRRC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgRRC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgRRC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = "Ratio of group population-weighted mean leak repair time to population mean by Census Tract",
       title = "Relative Risk of Priority Populations and Mean Leak Repair Time in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_RR_tract2019.png")


# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeakAgeDaysAvgRR:wLeakAgeDaysAvgRRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgRR" = "All Leaks",
                            "wLeakAgeDaysAvgRRC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgRRC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgRRC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ leakClass, scales = "free_y") +
  labs(x = NULL, 
       y = "Ratio of group population-weighted mean leak age to population mean by Census Tract",
       title = "Relative Risk of Priority Populations and Mean Age of Unrepaired Leaks in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_RR_tract2019.png")


# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksRRBG, wLeaksRRCG, wLeaksRREV, 
                 wLeaksRRFG, wLeaksRRLU, wLeaksRRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wLeaksRRBG" = "Berkshire Gas",
                          "wLeaksRRCG" = "Columbia Gas",
                          "wLeaksRREV" = "Eversource Energy",
                          "wLeaksRRLU" = "Liberty Utilities",
                          "wLeaksRRNG" = "National Grid",
                          "wLeaksRRFG" = "Unitil/Fitchburg Gas"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityRR_tract2019.png")


# faceted dot graph of RR for unrepaired leaks per occupied housing unit around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerHURRBG, wLeaksPerHURRCG, wLeaksPerHURREV, 
                 wLeaksPerHURRFG, wLeaksPerHURRLU, wLeaksPerHURRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wLeaksPerHURRBG" = "Berkshire Gas",
                          "wLeaksPerHURRCG" = "Columbia Gas",
                          "wLeaksPerHURREV" = "Eversource Energy",
                          "wLeaksPerHURRFG" = "Unitil/Fitchburg Gas",
                          "wLeaksPerHURRLU" = "Liberty Utilities",
                          "wLeaksPerHURRNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtility_HU_RR_tract2019.png")


# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksRRrepairBG, wLeaksRRrepairCG, wLeaksRRrepairEV, 
                 wLeaksRRrepairFG, wLeaksRRrepairLU, wLeaksRRrepairNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wLeaksRRrepairBG" = "Berkshire Gas",
                          "wLeaksRRrepairCG" = "Columbia Gas",
                          "wLeaksRRrepairEV" = "Eversource Energy",
                          "wLeaksRRrepairLU" = "Liberty Utilities",
                          "wLeaksRRrepairNG" = "National Grid",
                          "wLeaksRRrepairFG" = "Unitil/Fitchburg Gas"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityREP_RR_tract2019.png")


# faceted dot graph of RR for repaired leaks per occupied housing unit around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wREPLeaksPerHURRBG, wREPLeaksPerHURRCG, wREPLeaksPerHURREV, 
                 wREPLeaksPerHURRFG, wREPLeaksPerHURRLU, wREPLeaksPerHURRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wREPLeaksPerHURRBG" = "Berkshire Gas",
                          "wREPLeaksPerHURRCG" = "Columbia Gas",
                          "wREPLeaksPerHURREV" = "Eversource Energy",
                          "wREPLeaksPerHURRFG" = "Unitil/Fitchburg Gas",
                          "wREPLeaksPerHURRLU" = "Liberty Utilities",
                          "wREPLeaksPerHURRNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityREP_HU_RR_tract2019.png")


# faceted dot graph of RR for total leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksRRtotalBG, wLeaksRRtotalCG, wLeaksRRtotalEV, 
                 wLeaksRRtotalFG, wLeaksRRtotalLU, wLeaksRRtotalNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wLeaksRRtotalBG" = "Berkshire Gas",
                          "wLeaksRRtotalCG" = "Columbia Gas",
                          "wLeaksRRtotalEV" = "Eversource Energy",
                          "wLeaksRRtotalLU" = "Liberty Utilities",
                          "wLeaksRRtotalNG" = "National Grid",
                          "wLeaksRRtotalFG" = "Unitil/Fitchburg Gas"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityAll_RR_tract2019.png")


# faceted dot graph of RR for total leaks per occupied housing unit around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wALLLeaksPerHURRBG, wALLLeaksPerHURRCG, wALLLeaksPerHURREV, 
                 wALLLeaksPerHURRFG, wALLLeaksPerHURRLU, wALLLeaksPerHURRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wALLLeaksPerHURRBG" = "Berkshire Gas",
                          "wALLLeaksPerHURRCG" = "Columbia Gas",
                          "wALLLeaksPerHURREV" = "Eversource Energy",
                          "wALLLeaksPerHURRFG" = "Unitil/Fitchburg Gas",
                          "wALLLeaksPerHURRLU" = "Liberty Utilities",
                          "wALLLeaksPerHURRNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = expression(paste("Ratio of group population-weighted mean leak density (leaks/", 
                            km^2, ")", " to population mean by Census Tract",sep = "")),
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019\nby Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityAll_HU_RR_tract2019.png")


# faceted dot graph of RR for age of unrepaired leaks around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeakAgeDaysAvgRRBG, wLeakAgeDaysAvgRRCG, wLeakAgeDaysAvgRREV, 
                 wLeakAgeDaysAvgRRFG, wLeakAgeDaysAvgRRLU, wLeakAgeDaysAvgRRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wLeakAgeDaysAvgRRBG" = "Berkshire Gas",
                          "wLeakAgeDaysAvgRRCG" = "Columbia Gas",
                          "wLeakAgeDaysAvgRREV" = "Eversource Energy",
                          "wLeakAgeDaysAvgRRLU" = "Liberty Utilities",
                          "wLeakAgeDaysAvgRRNG" = "National Grid",
                          "wLeakAgeDaysAvgRRFG" = "Unitil/Fitchburg Gas"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Relative Risk of Priority Populations and Average Age of Unrepaired Leaks in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityAge_RR_tract2019.png")


# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wDaysToRepairAvgRRBG, wDaysToRepairAvgRRCG, wDaysToRepairAvgRREV, 
                 wDaysToRepairAvgRRFG, wDaysToRepairAvgRRLU, wDaysToRepairAvgRRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
  drop_na(leakDensity) %>%
  mutate(Utility = recode(Utility, "wDaysToRepairAvgRRBG" = "Berkshire Gas",
                          "wDaysToRepairAvgRRCG" = "Columbia Gas",
                          "wDaysToRepairAvgRREV" = "Eversource Energy",
                          "wDaysToRepairAvgRRLU" = "Liberty Utilities",
                          "wDaysToRepairAvgRRNG" = "National Grid",
                          "wDaysToRepairAvgRRFG" = "Unitil/Fitchburg Gas"),
         Group = reorder_within(Group, leakDensity, Utility),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>%
  ggplot(aes(x = Group, y = leakDensity, color = Status)) + 
  geom_hline(yintercept = 1, color = "gray30") +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  theme(legend.title=element_blank(), legend.position = "top") +
  facet_wrap(~ Utility, scales = "free_y") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time(days) by Census Tract",
       title = "Relative Risk of Priority Populations and Average Leak Repair Time in 2019 by Utility across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyUtilityTime_RR_tract2019.png")
