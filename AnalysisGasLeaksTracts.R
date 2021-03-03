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
load("Data/HEET2019Leaks.rds")

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
  st_join(unrepaired2019final, ., largest = TRUE)
# do same for repaired leaks
repaired2019_with_tract <- ma_tracts %>% 
  select(GEOID) %>%
  st_join(repaired2019final, ., largest = TRUE)

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


# save output to save time
saveRDS(ma_tracts, file = "Data/ma_tractsMEAN.Rds")

# load output
ma_tracts <- readRDS("Data/ma_tractsMEAN.Rds")

summary(ma_tracts$leaks_sqkm)
ma_tracts %>% 
  ggplot(aes(x = leaks_sqkm)) + geom_histogram()

# # Look at distributions of leak counts by tract
# ma_tracts %>% 
#   ggplot(aes(x = unrepaired2019total)) + geom_histogram()
# 
# summary(ma_tracts$unrepaired2019total)
# 
# tm_shape(ma_tracts) + tm_fill(col = "unrepaired2019total", style = "fisher")
# 
# ma_tracts %>% 
#   ggplot(aes(x = leaks_sqkm)) + geom_histogram()
# 
# summary(ma_tracts$leaks_sqkm)
# 
# tm_shape(ma_tracts) + tm_fill(col = "leaks_sqkm", style = "quantile")
# tm_shape(ma_tracts) + tm_fill(col = "leaks_sqkm", style = "fisher") + 
#   tm_shape(ng_service_areas) + tm_polygons(border.col = "GAS")
# 
# ng_service_areas2 <- ng_service_areas %>% 
#   group_by(GAS) %>% 
#   summarize()
# 
# ma_tracts %>% 
#   ggplot(aes(x = unrepaired2019totalC1)) + geom_histogram()
# 
# summary(ma_tracts$unrepaired2019totalC1)
# 
# ma_tracts %>% 
#   ggplot(aes(x = unrepaired2019totalC2)) + geom_histogram()
# 
# summary(ma_tracts$unrepaired2019totalC2)
# 
# ma_tracts %>% 
#   ggplot(aes(x = unrepaired2019totalC3)) + geom_histogram()
# 
# summary(ma_tracts$unrepaired2019totalC3)


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
saveRDS(ppLeakDensityJoined, file = "Data/ppLeakDensityAVGtracts.Rds")

# load data
ppLeakDensityJoined <- readRDS("Data/ppLeakDensityAVGtracts.Rds")


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
saveRDS(ppLeakDensity_df, file = "Data/ppLeakDensity_df_tracts.Rds")

# load object
ppLeakDensity_df <- readRDS("Data/ppLeakDensity_df_tracts.Rds")

# create tables and figures for each metric
# Unrepaired leaks per square kilometer
ppLeakDensity_df %>% 
  select(Group, wLeaksPerSqKm, wLeaksRR, wLeaksPerSqKmC1, wLeaksRRC1, wLeaksPerSqKmC2, wLeaksRRC2, wLeaksPerSqKmC3, wLeaksRRC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wLeaksPerSqKm)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean leak density (leaks/sqkm) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,2,2,2,2,2,2), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Unrepaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak density by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerSqKm:wLeaksPerSqKmC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_tract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksRR:wLeaksRRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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

ggsave("Images/LeaksPPbyClassRR_tract.png")


# Repaired leaks per square kilometer
ppLeakDensity_df %>% 
  select(Group, wLeaksPerSqKmREP, wLeaksRRrepair, wLeaksPerSqKmREPC1, wLeaksRRrepairC1, wLeaksPerSqKmREPC2, wLeaksRRrepairC2, wLeaksPerSqKmREPC3, wLeaksRRrepairC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wLeaksPerSqKmREP)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean leak density (leaks/sqkm) of repaired leaks in 2019.", align = "r", digits = c(0,1,2,2,2,2,2,2,2), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Repaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leak density by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerSqKmREP:wLeaksPerSqKmREPC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Repaired Gas Leaks in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_tract.png")

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

ggsave("Images/LeaksPPbyClassREPRR_tract.png")


# Total leaks per square kilometer
ppLeakDensity_df %>% 
  select(Group, wLeaksPerSqKmALL, wLeaksRRtotal, wLeaksPerSqKmALLC1, wLeaksRRtotalC1, wLeaksPerSqKmALLC2, wLeaksRRtotalC2, wLeaksPerSqKmALLC3, wLeaksRRtotalC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wLeaksPerSqKmALL)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean leak density (leaks/sqkm) of all leaks (unrepaired + repaired) in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leak density by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerSqKmALL:wLeaksPerSqKmALLC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_tract.png")

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

ggsave("Images/LeaksPPbyClassAllRR_tract.png")


# Unrepaired leaks per occupied housing unit
ppLeakDensity_df %>% 
  select(Group, wLeaksPerHU, wLeaksPerHURR, wLeaksPerHUC1, wLeaksPerHURRC1, wLeaksPerHUC2, wLeaksPerHURRC2, wLeaksPerHUC3, wLeaksPerHURRC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wLeaksPerHU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean unrepaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,2,2,4,2,2,2,2,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerHU:wLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_tract.png")

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

ggsave("Images/LeaksPPbyClass_HU_RR_tract.png")


# Repaired leaks per occupied housing unit
ppLeakDensity_df %>% 
  select(Group, wREPLeaksPerHU, wREPLeaksPerHURR, wREPLeaksPerHUC1, wREPLeaksPerHURRC1, wREPLeaksPerHUC2, wREPLeaksPerHURRC2, wREPLeaksPerHUC3, wREPLeaksPerHURRC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wREPLeaksPerHU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean repaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,1,2,2,2,2,2,2,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leaks per occupied housing unit by leak class
ppLeakDensity_df %>% 
  pivot_longer(wREPLeaksPerHU:wREPLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
       y = "Population-weighted mean repaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_tract.png")

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

ggsave("Images/LeaksPPbyClassREP_HU_RR_tract.png")


# Total leaks per occupied housing unit
ppLeakDensity_df %>% 
  select(Group, wALLLeaksPerHU, wALLLeaksPerHURR, wALLLeaksPerHUC1, wALLLeaksPerHURRC1, wALLLeaksPerHUC2, wALLLeaksPerHURRC2, wALLLeaksPerHUC3, wALLLeaksPerHURRC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wALLLeaksPerHU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean total leaks (repaired + unrepaired) per occupied housing unit in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensity_df %>% 
  pivot_longer(wALLLeaksPerHU:wALLLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
       y = "Population-weighted mean total leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_tract.png")

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

ggsave("Images/LeaksPPbyClassAll_HU_RR_tract.png")


# Average leak repair times
ppLeakDensity_df %>% 
  select(Group, wDaysToRepairAvg, wDaysToRepairAvgRR, wDaysToRepairAvgC1, wDaysToRepairAvgRRC1, wDaysToRepairAvgC2, wDaysToRepairAvgRRC2, wDaysToRepairAvgC3, wDaysToRepairAvgRRC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wDaysToRepairAvg)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean leak repair times (days) in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of average leak repair time by leak class
ppLeakDensity_df %>% 
  pivot_longer(wDaysToRepairAvg:wDaysToRepairAvgC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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
       y = "Population-weighted mean leak repair time (days) by Census Tract",
       title = "Priority Populations and Mean Leak Repair Time in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_tract.png")

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

ggsave("Images/LeaksPPbyClassTime_RR_tract.png")


# Average age of unrepaired leaks
ppLeakDensity_df %>% 
  select(Group, wLeakAgeDaysAvg, wLeakAgeDaysAvgRR, wLeakAgeDaysAvgC1, wLeakAgeDaysAvgRRC1, wLeakAgeDaysAvgC2, wLeakAgeDaysAvgRRC2, wLeakAgeDaysAvgC3, wLeakAgeDaysAvgRRC3) %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  arrange(desc(wLeakAgeDaysAvg)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Population-weighted mean age (days) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak age by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeakAgeDaysAvg:wLeakAgeDaysAvgC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvg" = "All Leaks",
                            "wLeakAgeDaysAvgC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Priority Populations and Mean Age of Unrepaired Leaks in 2019 across Massachusetts", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_tract.png")

# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeakAgeDaysAvgRR:wLeakAgeDaysAvgRRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
                        "MA_MINORITY21" = "MA Minority",
                        "MA_INCOME21" = "MA Low Income")) %>%
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

ggsave("Images/LeaksPPbyClassAge_RR_tract.png")



##### Compare leak density distribution by utility
### National Grid; includes both NG-Boston and NG-Colonial Gas
ppLeakDensityNG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^National") | 
           GAS == "Colonial Gas") %>% # limit to BGs in NG/Colonial svc area
  mutate(leaks_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                           `National Grid - Colonial Gas_19unrepaired`)/area_sqkm,
         leaks_sqkmNGC1 = (`National Grid - Boston Gas_19unrepairedC1` +
                             `National Grid - Colonial Gas_19unrepairedC1`)/area_sqkm,
         leaks_sqkmNGC2 = (`National Grid - Boston Gas_19unrepairedC2` +
                             `National Grid - Colonial Gas_19unrepairedC2`)/area_sqkm,
         leaks_sqkmNGC3 = (`National Grid - Boston Gas_19unrepairedC3` +
                             `National Grid - Colonial Gas_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmNG = (`National Grid - Boston Gas_19repaired` +
                              `National Grid - Colonial Gas_19repaired`)/area_sqkm,
         REPleaks_sqkmNGC1 = (`National Grid - Boston Gas_19repairedC1` +
                                `National Grid - Colonial Gas_19repairedC1`)/area_sqkm,
         REPleaks_sqkmNGC2 = (`National Grid - Boston Gas_19repairedC2` +
                                `National Grid - Colonial Gas_19repairedC2`)/area_sqkm,
         REPleaks_sqkmNGC3 = (`National Grid - Boston Gas_19repairedC3` +
                                `National Grid - Colonial Gas_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                                  `National Grid - Colonial Gas_19unrepaired` + 
                                  `National Grid - Boston Gas_19repaired` +
                                  `National Grid - Colonial Gas_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmNGC1 = (`National Grid - Boston Gas_19unrepairedC1` +
                                    `National Grid - Colonial Gas_19unrepairedC1` + 
                                    `National Grid - Boston Gas_19repairedC1` +
                                    `National Grid - Colonial Gas_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmNGC2 = (`National Grid - Boston Gas_19unrepairedC2` +
                                    `National Grid - Colonial Gas_19unrepairedC2` + 
                                    `National Grid - Boston Gas_19repairedC2` +
                                    `National Grid - Colonial Gas_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmNGC3 = (`National Grid - Boston Gas_19unrepairedC3` +
                                    `National Grid - Colonial Gas_19unrepairedC3` + 
                                    `National Grid - Boston Gas_19repairedC3` +
                                    `National Grid - Colonial Gas_19repairedC3`)/area_sqkm,
         leaks_huNG = if_else(total_occ_unitsE == 0, 0, 
                              (`National Grid - Boston Gas_19unrepaired` +
                                 `National Grid - Colonial Gas_19unrepaired`)/total_occ_unitsE),
         leaks_huNGC1 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid - Boston Gas_19unrepairedC1` +
                                   `National Grid - Colonial Gas_19unrepairedC1`)/total_occ_unitsE),
         leaks_huNGC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid - Boston Gas_19unrepairedC2` +
                                   `National Grid - Colonial Gas_19unrepairedC2`)/total_occ_unitsE),
         leaks_huNGC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid - Boston Gas_19unrepairedC3` +
                                   `National Grid - Colonial Gas_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huNG = if_else(total_occ_unitsE == 0, 0,
                                 (`National Grid - Boston Gas_19repaired` +
                                    `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
         REPleaks_huNGC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid - Boston Gas_19repairedC1` +
                                      `National Grid - Colonial Gas_19repairedC1`)/total_occ_unitsE),
         REPleaks_huNGC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid - Boston Gas_19repairedC2` +
                                      `National Grid - Colonial Gas_19repairedC2`)/total_occ_unitsE),
         REPleaks_huNGC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid - Boston Gas_19repairedC3` +
                                      `National Grid - Colonial Gas_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huNG = if_else(total_occ_unitsE == 0, 0,
                                     (`National Grid - Boston Gas_19unrepaired` + `National Grid - Colonial Gas_19unrepaired` + 
                                        `National Grid - Boston Gas_19repaired` +
                                        `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huNGC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid - Boston Gas_19unrepairedC1` + `National Grid - Colonial Gas_19unrepairedC1` + 
                                          `National Grid - Boston Gas_19repairedC1` +
                                          `National Grid - Colonial Gas_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huNGC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid - Boston Gas_19unrepairedC2` + `National Grid - Colonial Gas_19unrepairedC2` + 
                                          `National Grid - Boston Gas_19repairedC2` +
                                          `National Grid - Colonial Gas_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huNGC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid - Boston Gas_19unrepairedC3` + `National Grid - Colonial Gas_19unrepairedC3` + 
                                          `National Grid - Boston Gas_19repairedC3` +
                                          `National Grid - Colonial Gas_19repairedC3`)/total_occ_unitsE),
         PctRepaired19NG = (`National Grid - Boston Gas_19repaired` +
                              `National Grid - Colonial Gas_19repaired`)/ (`National Grid - Boston Gas_19unrepaired` +
                                                                             `National Grid - Colonial Gas_19unrepaired` + 
                                                                             `National Grid - Boston Gas_19repaired` +
                                                                             `National Grid - Colonial Gas_19repaired`)*100) %>% 
  rowwise() %>% 
  mutate(DaysToRepairAvgNG = sum(`National Grid - Boston Gas_19repairedDaysAvg`,                                 `National Grid - Colonial Gas_19repairedDaysAvg`, na.rm = TRUE)/2,
         DaysToRepairAvgNGC1 = sum(`National Grid - Boston Gas_19repairedDaysAvgC1`,                                 `National Grid - Colonial Gas_19repairedDaysAvgC1`, na.rm = TRUE)/2,
         DaysToRepairAvgNGC2 = sum(`National Grid - Boston Gas_19repairedDaysAvgC2`,                                 `National Grid - Colonial Gas_19repairedDaysAvgC2`, na.rm = TRUE)/2,
         DaysToRepairAvgNGC3 = sum(`National Grid - Boston Gas_19repairedDaysAvgC3`,                                 `National Grid - Colonial Gas_19repairedDaysAvgC3`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNG = sum(`National Grid - Boston Gas_19unrepairedDaysAvg`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvg`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNGC1 = sum(`National Grid - Boston Gas_19unrepairedDaysAvgC1`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvgC1`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNGC2 = sum(`National Grid - Boston Gas_19unrepairedDaysAvgC2`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvgC2`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNGC3 = sum(`National Grid - Boston Gas_19unrepairedDaysAvgC3`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvgC3`, na.rm = TRUE)/2) %>% 
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
  mutate(leaks_sqkmEV = (`Eversource Energy_19unrepaired`)/area_sqkm,
         # leaks_sqkmEVC1 = (`Eversource Energy_19unrepairedC1`)/area_sqkm,
         leaks_sqkmEVC1 = 0,
         leaks_sqkmEVC2 = (`Eversource Energy_19unrepairedC2`)/area_sqkm,
         leaks_sqkmEVC3 = (`Eversource Energy_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmEV = (`Eversource Energy_19repaired`)/area_sqkm,
         REPleaks_sqkmEVC1 = (`Eversource Energy_19repairedC1`)/area_sqkm,
         REPleaks_sqkmEVC2 = (`Eversource Energy_19repairedC2`)/area_sqkm,
         REPleaks_sqkmEVC3 = (`Eversource Energy_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmEV = (`Eversource Energy_19unrepaired` +
                                  `Eversource Energy_19repaired`)/area_sqkm,
         # AllLeaks2019_sqkmEVC1 = (`Eversource Energy_19unrepairedC1` + 
         #                            `Eversource Energy_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmEVC1 = (`Eversource Energy_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmEVC2 = (`Eversource Energy_19unrepairedC2` + 
                                    `Eversource Energy_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmEVC3 = (`Eversource Energy_19unrepairedC3` + 
                                    `Eversource Energy_19repairedC3`)/area_sqkm,
         leaks_huEV = if_else(total_occ_unitsE == 0, 0, 
                              (`Eversource Energy_19unrepaired`)/total_occ_unitsE),
         # leaks_huEVC1 = if_else(total_occ_unitsE == 0, 0, 
         #                        (`Eversource Energy_19unrepairedC1`)/total_occ_unitsE),
         leaks_huEVC1 = 0,
         leaks_huEVC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`Eversource Energy_19unrepairedC2`)/total_occ_unitsE),
         leaks_huEVC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Eversource Energy_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huEV = if_else(total_occ_unitsE == 0, 0,
                                 (`Eversource Energy_19repaired`)/total_occ_unitsE),
         REPleaks_huEVC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource Energy_19repairedC1`)/total_occ_unitsE),
         REPleaks_huEVC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource Energy_19repairedC2`)/total_occ_unitsE),
         REPleaks_huEVC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource Energy_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huEV = if_else(total_occ_unitsE == 0, 0,
                                     (`Eversource Energy_19unrepaired` + 
                                        `Eversource Energy_19repaired`)/total_occ_unitsE),
         # AllLeaks2019_huEVC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Eversource Energy_19unrepairedC1` +
         #                                  `Eversource Energy_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huEVC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource Energy_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huEVC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource Energy_19unrepairedC2` +
                                          `Eversource Energy_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huEVC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource Energy_19unrepairedC3` +
                                          `Eversource Energy_19repairedC3`)/total_occ_unitsE),
         PctRepaired19EV = (`Eversource Energy_19repaired`)/ (`Eversource Energy_19unrepaired` + `Eversource Energy_19repaired`)*100,
         DaysToRepairAvgEV = `Eversource Energy_19repairedDaysAvg`,
         DaysToRepairAvgEVC1 = `Eversource Energy_19repairedDaysAvgC1`,
         DaysToRepairAvgEVC2 = `Eversource Energy_19repairedDaysAvgC2`,
         DaysToRepairAvgEVC3 = `Eversource Energy_19repairedDaysAvgC3`,
         LeakAgeDaysAvgEV = `Eversource Energy_19unrepairedDaysAvg`,
         # LeakAgeDaysAvgEVC1 = `Eversource Energy_19unrepairedDaysAvgC1`,
         LeakAgeDaysAvgEVC1 = NA,
         LeakAgeDaysAvgEVC2 = `Eversource Energy_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgEVC3 = `Eversource Energy_19unrepairedDaysAvgC3`) %>% 
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




### Fitchburg Gas aka Unitil. NO CLASS FOR LEAKS!.
ppLeakDensityFG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Unitil") | 
           GAS == "Unitil$") %>% # limit to BGs in FG svc area
  mutate(leaks_sqkmFG = (`Fitchburg Gas_19unrepaired`)/area_sqkm,
         REPleaks_sqkmFG = (`Fitchburg Gas_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmFG = (`Fitchburg Gas_19unrepaired` +
                                  `Fitchburg Gas_19repaired`)/area_sqkm,
         leaks_huFG = if_else(total_occ_unitsE == 0, 0, 
                              (`Fitchburg Gas_19unrepaired`)/total_occ_unitsE),
         REPleaks_huFG = if_else(total_occ_unitsE == 0, 0,
                                 (`Fitchburg Gas_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huFG = if_else(total_occ_unitsE == 0, 0,
                                     (`Fitchburg Gas_19unrepaired` + 
                                        `Fitchburg Gas_19repaired`)/total_occ_unitsE),
         PctRepaired19FG = (`Fitchburg Gas_19repaired`)/ (`Fitchburg Gas_19unrepaired` + `Fitchburg Gas_19repaired`)*100,
         DaysToRepairAvgFG = `Fitchburg Gas_19repairedDaysAvg`,
         LeakAgeDaysAvgFG = `Fitchburg Gas_19unrepairedDaysAvg`) %>% 
  select(ends_with("_E"), eng_hhE, under5E, over64E, eng_limitE, num2povE, lthsE, 
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
            wLeaksPerSqKmREPFG = weighted.mean(x = REPleaks_sqkmFG, 
                                               w = Pop, na.rm = TRUE),
            wLeaksPerSqKmALLFG = weighted.mean(x = AllLeaks2019_sqkmFG, 
                                               w = Pop, na.rm = TRUE),
            wLeakAgeDaysAvgFG = weighted.mean(x = LeakAgeDaysAvgFG,
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
                        "renter_occ_unitsE" = "Renter Occupied HU",
                        "house_burdened_E" = "Housing Burdened"))

# create a table with exposure values and relative risks, and separating out HH and HU groups
ppLeakDensityFG_pops_df <- ppLeakDensityFG %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "Housing Burdened",
                       "Limited English HH", "Renter Occupied HU",
                       "Total Households", "Total Occupied HU")) %>% 
  mutate(wLeaksRRFG = wLeaksPerSqKmFG/wLeaksPerSqKmFG[Group == "Total Population"],
         wLeaksRRrepairFG = wLeaksPerSqKmREPFG/wLeaksPerSqKmREPFG[Group == "Total Population"],
         wLeaksRRtotalFG = wLeaksPerSqKmALLFG/wLeaksPerSqKmALLFG[Group == "Total Population"],
         wLeaksPerHURRFG = wLeaksPerHUFG/wLeaksPerHUFG[Group == "Total Population"],
         wREPLeaksPerHURRFG = wREPLeaksPerHUFG/wREPLeaksPerHUFG[Group == "Total Population"],
         wALLLeaksPerHURRFG = wALLLeaksPerHUFG/wALLLeaksPerHUFG[Group == "Total Population"],
         wDaysToRepairAvgRRFG = wDaysToRepairAvgFG/wDaysToRepairAvgFG[Group == "Total Population"],
         wLeakAgeDaysAvgRRFG = wLeakAgeDaysAvgFG/wLeakAgeDaysAvgFG[Group == "Total Population"])

ppLeakDensityFG_LEH_df <- ppLeakDensityFG %>% 
  filter(Group %in% c("Limited English HH", "Total Households")) %>% 
  mutate(wLeaksRRFG = wLeaksPerSqKmFG/wLeaksPerSqKmFG[Group == "Total Households"],
         wLeaksRRrepairFG = wLeaksPerSqKmREPFG/wLeaksPerSqKmREPFG[Group == "Total Households"],
         wLeaksRRtotalFG = wLeaksPerSqKmALLFG/wLeaksPerSqKmALLFG[Group == "Total Households"],
         wLeaksPerHURRFG = wLeaksPerHUFG/wLeaksPerHUFG[Group == "Total Households"],
         wREPLeaksPerHURRFG = wREPLeaksPerHUFG/wREPLeaksPerHUFG[Group == "Total Households"],
         wALLLeaksPerHURRFG = wALLLeaksPerHUFG/wALLLeaksPerHUFG[Group == "Total Households"],
         wDaysToRepairAvgRRFG = wDaysToRepairAvgFG/wDaysToRepairAvgFG[Group == "Total Households"],
         wLeakAgeDaysAvgRRFG = wLeakAgeDaysAvgFG/wLeakAgeDaysAvgFG[Group == "Total Households"])

ppLeakDensityFG_renters_df <- ppLeakDensityFG %>% 
  filter(Group %in% c("Renter Occupied HU", "Housing Burdened", 
                      "Total Occupied HU")) %>% 
  mutate(wLeaksRRFG = wLeaksPerSqKmFG/wLeaksPerSqKmFG[Group == "Total Occupied HU"],
         wLeaksRRrepairFG = wLeaksPerSqKmREPFG/wLeaksPerSqKmREPFG[Group == "Total Occupied HU"],
         wLeaksRRtotalFG = wLeaksPerSqKmALLFG/wLeaksPerSqKmALLFG[Group == "Total Occupied HU"],
         wLeaksPerHURRFG = wLeaksPerHUFG/wLeaksPerHUFG[Group == "Total Occupied HU"],
         wREPLeaksPerHURRFG = wREPLeaksPerHUFG/wREPLeaksPerHUFG[Group == "Total Occupied HU"],
         wALLLeaksPerHURRFG = wALLLeaksPerHUFG/wALLLeaksPerHUFG[Group == "Total Occupied HU"],
         wDaysToRepairAvgRRFG = wDaysToRepairAvgFG/wDaysToRepairAvgFG[Group == "Total Occupied HU"],
         wLeakAgeDaysAvgRRFG = wLeakAgeDaysAvgFG/wLeakAgeDaysAvgFG[Group == "Total Occupied HU"])

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
saveRDS(ppLeakDensityJoinedU, file = "Data/ppLeakDensityJoinedU_Tract.Rds")

# load data
ppLeakDensityJoinedU <- readRDS("Data/ppLeakDensityJoinedU_Tract.Rds")


# Facet wrap by utility for unrepaired leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerSqKmBG, wLeaksPerSqKmCG, wLeaksPerSqKmEV, 
                 wLeaksPerSqKmFG, wLeaksPerSqKmLU, wLeaksPerSqKmNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races",
                       "MA_MINORITY17", "MA_INCOME17")) %>% 
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
                            km^2, ")", " by Census Tract", sep = "")),
       title = "Piority Populations and Unrepaired Gas Leaks by Utility for 2019 across Massachusetts")

ggsave("Images/LeaksPPbyUtility_tract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksRRBG, wLeaksRRCG, wLeaksRREV, 
                 wLeaksRRFG, wLeaksRRLU, wLeaksRRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityRR_tract.png")


# Facet wrap by utility for unrepaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerHUBG, wLeaksPerHUCG, wLeaksPerHUEV, 
                 wLeaksPerHUFG, wLeaksPerHULU, wLeaksPerHUNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wLeaksPerHUBG" = "Berkshire Gas",
                          "wLeaksPerHUCG" = "Columbia Gas",
                          "wLeaksPerHUEV" = "Eversource Energy",
                          "wLeaksPerHUFG" = "Unitil/Fitchburg Gas",
                          "wLeaksPerHULU" = "Liberty Utilities",
                          "wLeaksPerHUNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Piority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")

ggsave("Images/LeaksPPbyUtility_HU_tract.png")

# faceted dot graph of RR for unrepaired leaks per occupied housing unit around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerHURRBG, wLeaksPerHURRCG, wLeaksPerHURREV, 
                 wLeaksPerHURRFG, wLeaksPerHURRLU, wLeaksPerHURRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtility_HU_RR_tract.png")


# Facet wrap by utility for repaired leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerSqKmREPBG, wLeaksPerSqKmREPCG, wLeaksPerSqKmREPEV, 
                 wLeaksPerSqKmREPFG, wLeaksPerSqKmREPLU, wLeaksPerSqKmREPNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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
                            km^2, ")", " by Census Tract", sep = "")),
       title = "Piority Populations and Repaired Gas Leaks by Utility for 2019 across Massachusetts")

ggsave("Images/LeaksPPbyUtilityREP_tract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksRRrepairBG, wLeaksRRrepairCG, wLeaksRRrepairEV, 
                 wLeaksRRrepairFG, wLeaksRRrepairLU, wLeaksRRrepairNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityREP_RR_tract.png")


# Facet wrap by utility for repaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wREPLeaksPerHUBG, wREPLeaksPerHUCG, wREPLeaksPerHUEV, 
                 wREPLeaksPerHUFG, wREPLeaksPerHULU, wREPLeaksPerHUNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wREPLeaksPerHUBG" = "Berkshire Gas",
                          "wREPLeaksPerHUCG" = "Columbia Gas",
                          "wREPLeaksPerHUEV" = "Eversource Energy",
                          "wREPLeaksPerHUFG" = "Unitil/Fitchburg Gas",
                          "wREPLeaksPerHULU" = "Liberty Utilities",
                          "wREPLeaksPerHUNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean of repaired leaks per occupied housing unit by Census Tract",
       title = "Piority Populations and Repaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")

ggsave("Images/LeaksPPbyUtilityREP_HU_tract.png")

# faceted dot graph of RR for repaired leaks per occupied housing unit around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wREPLeaksPerHURRBG, wREPLeaksPerHURRCG, wREPLeaksPerHURREV, 
                 wREPLeaksPerHURRFG, wREPLeaksPerHURRLU, wREPLeaksPerHURRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityREP_HU_RR_tract.png")


# Facet wrap by utility for all (repaired + unrepaired) leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksPerSqKmALLBG, wLeaksPerSqKmALLCG, wLeaksPerSqKmALLEV, 
                 wLeaksPerSqKmALLFG, wLeaksPerSqKmALLLU, wLeaksPerSqKmALLNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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
                            km^2, ")", " by Census Tract", sep = "")),
       title = "Piority Populations and All Repaired and Unrepaired Gas Leaks by Utility for 2019 across Massachusetts")

ggsave("Images/LeaksPPbyUtilityALL_tract.png")

# faceted dot graph of RR for total leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeaksRRtotalBG, wLeaksRRtotalCG, wLeaksRRtotalEV, 
                 wLeaksRRtotalFG, wLeaksRRtotalLU, wLeaksRRtotalNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityAll_RR_tract.png")


# Facet wrap by utility for all (repaired + unrepaired) leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wALLLeaksPerHUBG, wALLLeaksPerHUCG, wALLLeaksPerHUEV, 
                 wALLLeaksPerHUFG, wALLLeaksPerHULU, wALLLeaksPerHUNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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
       y = "Population-weighted mean leaks per occupied housing unity by Census Tract",
       title = "Piority Populations and All Repaired and Unrepaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")

ggsave("Images/LeaksPPbyUtilityALL_HU_tract.png")

# faceted dot graph of RR for total leaks per occupied housing unit around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wALLLeaksPerHURRBG, wALLLeaksPerHURRCG, wALLLeaksPerHURREV, 
                 wALLLeaksPerHURRFG, wALLLeaksPerHURRLU, wALLLeaksPerHURRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityAll_HU_RR_tract.png")


# Facet wrap by utility for percentage of leaks repaired
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wPctRepaired19BG, wPctRepaired19CG, wPctRepaired19EV, 
                 wPctRepaired19FG, wPctRepaired19LU, wPctRepaired19NG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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
       y = "Population-weighted mean percentage of leaks repaired in 2019 by Census Tract",
       title = "Piority Populations Percent Repaired Gas Leaks in 2019 by Utility across Massachusetts")

ggsave("Images/LeaksPPbyUtilityPctFix_tract.png")


# Facet wrap by utility for average age of unrepaired leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeakAgeDaysAvgBG, wLeakAgeDaysAvgCG, wLeakAgeDaysAvgEV, 
                 wLeakAgeDaysAvgFG, wLeakAgeDaysAvgLU, wLeakAgeDaysAvgNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wLeakAgeDaysAvgBG" = "Berkshire Gas",
                          "wLeakAgeDaysAvgCG" = "Columbia Gas",
                          "wLeakAgeDaysAvgEV" = "Eversource Energy",
                          "wLeakAgeDaysAvgFG" = "Unitil/Fitchburg Gas",
                          "wLeakAgeDaysAvgLU" = "Liberty Utilities",
                          "wLeakAgeDaysAvgNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Piority Populations and Average Age of Unrepaired Leaks in 2019 by Utility across Massachusetts")

ggsave("Images/LeaksPPbyUtilityAge_tract.png")

# faceted dot graph of RR for age of unrepaired leaks around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wLeakAgeDaysAvgRRBG, wLeakAgeDaysAvgRRCG, wLeakAgeDaysAvgRREV, 
                 wLeakAgeDaysAvgRRFG, wLeakAgeDaysAvgRRLU, wLeakAgeDaysAvgRRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityAge_RR_tract.png")


# Facet wrap by utility for average time to repair leaks
ppLeakDensityJoinedU %>% 
  pivot_longer(c(wDaysToRepairAvgBG, wDaysToRepairAvgCG, wDaysToRepairAvgEV, 
                 wDaysToRepairAvgFG, wDaysToRepairAvgLU, wDaysToRepairAvgNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme_minimal(base_size = 6) +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time(days) by Census Tract",
       title = "Piority Populations and Average Leak Repair Time in 2019 by Utility across Massachusetts")

ggsave("Images/LeaksPPbyUtilityTime_tract.png")

# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(c(wDaysToRepairAvgRRBG, wDaysToRepairAvgRRCG, wDaysToRepairAvgRREV, 
                 wDaysToRepairAvgRRFG, wDaysToRepairAvgRRLU, wDaysToRepairAvgRRNG), 
               names_to = "Utility", values_to = "leakDensity") %>% 
  filter(!Group %in% c("Native American", "Other race", 
                       "Native Pacific Islander", "Two or more races")) %>% 
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

ggsave("Images/LeaksPPbyUtilityTime_RR_tract.png")



### Break Out Analyses by Utility by Class (NOTE THAT FITCHBURG DID NOT REPORT LEAK CLASSES)
# create tables and figures for each metric
# National Grid by Leak Class
# Unrepaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmNG, wLeaksRRNG, wLeaksPerSqKmNGC1, wLeaksRRNGC1, wLeaksPerSqKmNGC2, wLeaksRRNGC2, wLeaksPerSqKmNGC3, wLeaksRRNGC3) %>% 
  arrange(desc(wLeaksPerSqKmNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "National Grid population-weighted mean leak density (leaks/sqkm) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Unrepaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmNG:wLeaksPerSqKmNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmNG" = "All Leaks",
                            "wLeaksPerSqKmNGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmNGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_NGtract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRNG:wLeaksRRNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>%
  mutate(leakClass = recode(leakClass, "wLeaksRRNG" = "All Leaks",
                            "wLeaksRRNGC1" = "Class 1 Leaks",
                            "wLeaksRRNGC2" = "Class 2 Leaks",
                            "wLeaksRRNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassRR_NGtract.png")

# Repaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmREPNG, wLeaksRRrepairNG, wLeaksPerSqKmREPNGC1, wLeaksRRrepairNGC1, wLeaksPerSqKmREPNGC2, wLeaksRRrepairNGC2, wLeaksPerSqKmREPNGC3, wLeaksRRrepairNGC3) %>% 
  arrange(desc(wLeaksPerSqKmREPNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "National Grid population-weighted mean leak density (leaks/sqkm) of repaired leaks in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Repaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmREPNG:wLeaksPerSqKmREPNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREPNG" = "All Leaks",
                            "wLeaksPerSqKmREPNGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmREPNGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmREPNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Repaired Gas Leaks in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_NGtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRrepairNG:wLeaksRRrepairNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRrepairNG" = "All Leaks",
                            "wLeaksRRrepairNGC1" = "Class 1 Leaks",
                            "wLeaksRRrepairNGC2" = "Class 2 Leaks",
                            "wLeaksRRrepairNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREPRR_NGtract.png")

# Total leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmALLNG, wLeaksRRtotalNG, wLeaksPerSqKmALLNGC1, wLeaksRRtotalNGC1, wLeaksPerSqKmALLNGC2, wLeaksRRtotalNGC2, wLeaksPerSqKmALLNGC3, wLeaksRRtotalNGC3) %>% 
  arrange(desc(wLeaksPerSqKmALLNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "National Grid population-weighted mean leak density (leaks/sqkm) of all leaks (unrepaired + repaired) in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmALLNG:wLeaksPerSqKmALLNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALLNG" = "All Leaks",
                            "wLeaksPerSqKmALLNGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmALLNGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmALLNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_NGtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRtotalNG:wLeaksRRtotalNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRtotalNG" = "All Leaks",
                            "wLeaksRRtotalNGC1" = "Class 1 Leaks",
                            "wLeaksRRtotalNGC2" = "Class 2 Leaks",
                            "wLeaksRRtotalNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAllRR_NGtract.png")

# Unrepaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerHUNG, wLeaksPerHURRNG, wLeaksPerHUNGC1, wLeaksPerHURRNGC1, wLeaksPerHUNGC2, wLeaksPerHURRNGC2, wLeaksPerHUNGC3, wLeaksPerHURRNGC3) %>% 
  arrange(desc(wLeaksPerHUNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "National Grid population-weighted mean unrepaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHUNG:wLeaksPerHUNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHUNG" = "All Leaks",
                            "wLeaksPerHUNGC1" = "Class 1 Leaks",
                            "wLeaksPerHUNGC2" = "Class 2 Leaks",
                            "wLeaksPerHUNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_NGtract.png")

# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHURRNG:wLeaksPerHURRNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHURRNG" = "All Leaks",
                            "wLeaksPerHURRNGC1" = "Class 1 Leaks",
                            "wLeaksPerHURRNGC2" = "Class 2 Leaks",
                            "wLeaksPerHURRNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_RR_NGtract.png")

# Repaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wREPLeaksPerHUNG, wREPLeaksPerHURRNG, wREPLeaksPerHUNGC1, wREPLeaksPerHURRNGC1, wREPLeaksPerHUNGC2, wREPLeaksPerHURRNGC2, wREPLeaksPerHUNGC3, wREPLeaksPerHURRNGC3) %>% 
  arrange(desc(wREPLeaksPerHUNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "National Grid population-weighted mean repaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHUNG:wREPLeaksPerHUNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHUNG" = "All Leaks",
                            "wREPLeaksPerHUNGC1" = "Class 1 Leaks",
                            "wREPLeaksPerHUNGC2" = "Class 2 Leaks",
                            "wREPLeaksPerHUNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean repaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_NGtract.png")

# faceted dot graph of RR for repaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHURRNG:wREPLeaksPerHURRNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHURRNG" = "All Leaks",
                            "wREPLeaksPerHURRNGC1" = "Class 1 Leaks",
                            "wREPLeaksPerHURRNGC2" = "Class 2 Leaks",
                            "wREPLeaksPerHURRNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Leaks Per Occupied Housing Unit in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_RR_NGtract.png")

# Total leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wALLLeaksPerHUNG, wALLLeaksPerHURRNG, wALLLeaksPerHUNGC1, wALLLeaksPerHURRNGC1, wALLLeaksPerHUNGC2, wALLLeaksPerHURRNGC2, wALLLeaksPerHUNGC3, wALLLeaksPerHURRNGC3) %>% 
  arrange(desc(wALLLeaksPerHUNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "National Grid population-weighted mean total leaks (repaired + unrepaired) per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHUNG:wALLLeaksPerHUNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHUNG" = "All Leaks",
                            "wALLLeaksPerHUNGC1" = "Class 1 Leaks",
                            "wALLLeaksPerHUNGC2" = "Class 2 Leaks",
                            "wALLLeaksPerHUNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean total leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_NGtract.png")

# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHURRNG:wALLLeaksPerHURRNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURRNG" = "All Leaks",
                            "wALLLeaksPerHURRNGC1" = "Class 1 Leaks",
                            "wALLLeaksPerHURRNGC2" = "Class 2 Leaks",
                            "wALLLeaksPerHURRNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_RR_NGtract.png")

# Average leak repair times
ppLeakDensityJoinedU %>% 
  select(Group, wDaysToRepairAvgNG, wDaysToRepairAvgRRNG, wDaysToRepairAvgNGC1, wDaysToRepairAvgRRNGC1, wDaysToRepairAvgNGC2, wDaysToRepairAvgRRNGC2, wDaysToRepairAvgNGC3, wDaysToRepairAvgRRNGC3) %>% 
  arrange(desc(wDaysToRepairAvgNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "National Grid population-weighted mean leak repair times (days) in 2019.", align = "r", digits = 1, 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of average leak repair time by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgNG:wDaysToRepairAvgNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgNG" = "All Leaks",
                            "wDaysToRepairAvgNGC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgNGC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time (days) by Census Tract",
       title = "Priority Populations and Mean Leak Repair Time in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_NGtract.png")

# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgRRNG:wDaysToRepairAvgRRNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgRRNG" = "All Leaks",
                            "wDaysToRepairAvgRRNGC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgRRNGC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgRRNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Mean Leak Repair Time in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_RR_NGtract.png")


# Average age of unrepaired leaks
ppLeakDensityJoinedU %>% 
  select(Group, wLeakAgeDaysAvgNG, wLeakAgeDaysAvgRRNG, wLeakAgeDaysAvgNGC1, wLeakAgeDaysAvgRRNGC1, wLeakAgeDaysAvgNGC2, wLeakAgeDaysAvgRRNGC2, wLeakAgeDaysAvgNGC3, wLeakAgeDaysAvgRRNGC3) %>% 
  arrange(desc(wLeakAgeDaysAvgNG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "National Grid population-weighted mean age (days) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak age by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgNG:wLeakAgeDaysAvgNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgNG" = "All Leaks",
                            "wLeakAgeDaysAvgNGC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgNGC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgNGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Priority Populations and Mean Age of Unrepaired Leaks in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_NGtract.png")

# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgRRNG:wLeakAgeDaysAvgRRNGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgRRNG" = "All Leaks",
                            "wLeakAgeDaysAvgRRNGC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgRRNGC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgRRNGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Mean Age of Unrepaired Leaks in 2019 for National Grid", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_RR_NGtract.png")




# Eversource Energy by Leak Class
# Unrepaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmEV, wLeaksRREV, wLeaksPerSqKmEVC1, wLeaksRREVC1, wLeaksPerSqKmEVC2, wLeaksRREVC2, wLeaksPerSqKmEVC3, wLeaksRREVC3) %>% 
  arrange(desc(wLeaksPerSqKmEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Eversource Energy population-weighted mean leak density (leaks/sqkm) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,2,2,2,2,2,2), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Unrepaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmEV:wLeaksPerSqKmEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmEV" = "All Leaks",
                            "wLeaksPerSqKmEVC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmEVC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_EVtract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRREV:wLeaksRREVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRREV" = "All Leaks",
                            "wLeaksRREVC1" = "Class 1 Leaks",
                            "wLeaksRREVC2" = "Class 2 Leaks",
                            "wLeaksRREVC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassRR_EVtract.png")

# Repaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmREPEV, wLeaksRRrepairEV, wLeaksPerSqKmREPEVC1, wLeaksRRrepairEVC1, wLeaksPerSqKmREPEVC2, wLeaksRRrepairEVC2, wLeaksPerSqKmREPEVC3, wLeaksRRrepairEVC3) %>% 
  arrange(desc(wLeaksPerSqKmREPEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Eversource Energy population-weighted mean leak density (leaks/sqkm) of repaired leaks in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Repaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmREPEV:wLeaksPerSqKmREPEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREPEV" = "All Leaks",
                            "wLeaksPerSqKmREPEVC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmREPEVC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmREPEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Repaired Gas Leaks in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_EVtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRrepairEV:wLeaksRRrepairEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>%
  mutate(leakClass = recode(leakClass, "wLeaksRRrepairEV" = "All Leaks",
                            "wLeaksRRrepairEVC1" = "Class 1 Leaks",
                            "wLeaksRRrepairEVC2" = "Class 2 Leaks",
                            "wLeaksRRrepairEVC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREPRR_EVtract.png")

# Total leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmALLEV, wLeaksRRtotalEV, wLeaksPerSqKmALLEVC1, wLeaksRRtotalEVC1, wLeaksPerSqKmALLEVC2, wLeaksRRtotalEVC2, wLeaksPerSqKmALLEVC3, wLeaksRRtotalEVC3) %>% 
  arrange(desc(wLeaksPerSqKmALLEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Eversource Energy population-weighted mean leak density (leaks/sqkm) of all leaks (unrepaired + repaired) in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmALLEV:wLeaksPerSqKmALLEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALLEV" = "All Leaks",
                            "wLeaksPerSqKmALLEVC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmALLEVC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmALLEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_EVtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRtotalEV:wLeaksRRtotalEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRtotalEV" = "All Leaks",
                            "wLeaksRRtotalEVC1" = "Class 1 Leaks",
                            "wLeaksRRtotalEVC2" = "Class 2 Leaks",
                            "wLeaksRRtotalEVC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAllRR_EVtract.png")

# Unrepaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerHUEV, wLeaksPerHURREV, wLeaksPerHUEVC1, wLeaksPerHURREVC1, wLeaksPerHUEVC2, wLeaksPerHURREVC2, wLeaksPerHUEVC3, wLeaksPerHURREVC3) %>% 
  arrange(desc(wLeaksPerHUEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Eversource Energy population-weighted mean unrepaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHUEV:wLeaksPerHUEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHUEV" = "All Leaks",
                            "wLeaksPerHUEVC1" = "Class 1 Leaks",
                            "wLeaksPerHUEVC2" = "Class 2 Leaks",
                            "wLeaksPerHUEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  drop_na(leakDensity) %>%
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_EVtract.png")

# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHURREV:wLeaksPerHURREVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHURREV" = "All Leaks",
                            "wLeaksPerHURREVC1" = "Class 1 Leaks",
                            "wLeaksPerHURREVC2" = "Class 2 Leaks",
                            "wLeaksPerHURREVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_RR_EVtract.png")

# Repaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wREPLeaksPerHUEV, wREPLeaksPerHURREV, wREPLeaksPerHUEVC1, wREPLeaksPerHURREVC1, wREPLeaksPerHUEVC2, wREPLeaksPerHURREVC2, wREPLeaksPerHUEVC3, wREPLeaksPerHURREVC3) %>% 
  arrange(desc(wREPLeaksPerHUEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Eversource Energy population-weighted mean repaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHUEV:wREPLeaksPerHUEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHUEV" = "All Leaks",
                            "wREPLeaksPerHUEVC1" = "Class 1 Leaks",
                            "wREPLeaksPerHUEVC2" = "Class 2 Leaks",
                            "wREPLeaksPerHUEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean repaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_EVtract.png")

# faceted dot graph of RR for repaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHURREV:wREPLeaksPerHURREVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHURREV" = "All Leaks",
                            "wREPLeaksPerHURREVC1" = "Class 1 Leaks",
                            "wREPLeaksPerHURREVC2" = "Class 2 Leaks",
                            "wREPLeaksPerHURREVC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Leaks Per Occupied Housing Unit in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_RR_EVtract.png")

# Total leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wALLLeaksPerHUEV, wALLLeaksPerHURREV, wALLLeaksPerHUEVC1, wALLLeaksPerHURREVC1, wALLLeaksPerHUEVC2, wALLLeaksPerHURREVC2, wALLLeaksPerHUEVC3, wALLLeaksPerHURREVC3) %>% 
  arrange(desc(wALLLeaksPerHUEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Eversource Energy population-weighted mean total leaks (repaired + unrepaired) per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHUEV:wALLLeaksPerHUEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHUEV" = "All Leaks",
                            "wALLLeaksPerHUEVC1" = "Class 1 Leaks",
                            "wALLLeaksPerHUEVC2" = "Class 2 Leaks",
                            "wALLLeaksPerHUEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean total leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_EVtract.png")

# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHURREV:wALLLeaksPerHURREVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURREV" = "All Leaks",
                            "wALLLeaksPerHURREVC1" = "Class 1 Leaks",
                            "wALLLeaksPerHURREVC2" = "Class 2 Leaks",
                            "wALLLeaksPerHURREVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_RR_EVtract.png")

# Average leak repair times
ppLeakDensityJoinedU %>% 
  select(Group, wDaysToRepairAvgEV, wDaysToRepairAvgRREV, wDaysToRepairAvgEVC1, wDaysToRepairAvgRREVC1, wDaysToRepairAvgEVC2, wDaysToRepairAvgRREVC2, wDaysToRepairAvgEVC3, wDaysToRepairAvgRREVC3) %>% 
  arrange(desc(wDaysToRepairAvgEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Eversource Energy population-weighted mean leak repair times (days) in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of average leak repair time by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgEV:wDaysToRepairAvgEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgEV" = "All Leaks",
                            "wDaysToRepairAvgEVC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgEVC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time (days) by Census Tract",
       title = "Priority Populations and Mean Leak Repair Time in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_EVtract.png")

# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgRREV:wDaysToRepairAvgRREVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgRREV" = "All Leaks",
                            "wDaysToRepairAvgRREVC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgRREVC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgRREVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Mean Leak Repair Time in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_RR_EVtract.png")


# Average age of unrepaired leaks
ppLeakDensityJoinedU %>% 
  select(Group, wLeakAgeDaysAvgEV, wLeakAgeDaysAvgRREV, wLeakAgeDaysAvgEVC1, wLeakAgeDaysAvgRREVC1, wLeakAgeDaysAvgEVC2, wLeakAgeDaysAvgRREVC2, wLeakAgeDaysAvgEVC3, wLeakAgeDaysAvgRREVC3) %>% 
  arrange(desc(wLeakAgeDaysAvgEV)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Eversource Energy population-weighted mean age (days) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak age by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgEV:wLeakAgeDaysAvgEVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgEV" = "All Leaks",
                            "wLeakAgeDaysAvgEVC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgEVC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgEVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  # drop_na(leakDensity) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_EVtract.png")

# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgRREV:wLeakAgeDaysAvgRREVC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgRREV" = "All Leaks",
                            "wLeakAgeDaysAvgRREVC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgRREVC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgRREVC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>% 
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
       title = "Relative Risk of Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Eversource Energy", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_RR_EVtract.png")






# Columbia Gas by Leak Class
# Unrepaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmCG, wLeaksRRCG, wLeaksPerSqKmCGC1, wLeaksRRCGC1, wLeaksPerSqKmCGC2, wLeaksRRCGC2, wLeaksPerSqKmCGC3, wLeaksRRCGC3) %>% 
  arrange(desc(wLeaksPerSqKmCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Columbia Gas population-weighted mean leak density (leaks/sqkm) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,3,2,2,2,2,2), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Unrepaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmCG:wLeaksPerSqKmCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmCG" = "All Leaks",
                            "wLeaksPerSqKmCGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmCGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_CGtract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRCG:wLeaksRRCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRCG" = "All Leaks",
                            "wLeaksRRCGC1" = "Class 1 Leaks",
                            "wLeaksRRCGC2" = "Class 2 Leaks",
                            "wLeaksRRCGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassRR_CGtract.png")

# Repaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmREPCG, wLeaksRRrepairCG, wLeaksPerSqKmREPCGC1, wLeaksRRrepairCGC1, wLeaksPerSqKmREPCGC2, wLeaksRRrepairCGC2, wLeaksPerSqKmREPCGC3, wLeaksRRrepairCGC3) %>% 
  arrange(desc(wLeaksPerSqKmREPCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Columbia Gas population-weighted mean leak density (leaks/sqkm) of repaired leaks in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Repaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmREPCG:wLeaksPerSqKmREPCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREPCG" = "All Leaks",
                            "wLeaksPerSqKmREPCGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmREPCGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmREPCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Repaired Gas Leaks in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_CGtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRrepairCG:wLeaksRRrepairCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRrepairCG" = "All Leaks",
                            "wLeaksRRrepairCGC1" = "Class 1 Leaks",
                            "wLeaksRRrepairCGC2" = "Class 2 Leaks",
                            "wLeaksRRrepairCGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREPRR_CGtract.png")

# Total leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmALLCG, wLeaksRRtotalCG, wLeaksPerSqKmALLCGC1, wLeaksRRtotalCGC1, wLeaksPerSqKmALLCGC2, wLeaksRRtotalCGC2, wLeaksPerSqKmALLCGC3, wLeaksRRtotalCGC3) %>% 
  arrange(desc(wLeaksPerSqKmALLCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Columbia Gas population-weighted mean leak density (leaks/sqkm) of all leaks (unrepaired + repaired) in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmALLCG:wLeaksPerSqKmALLCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALLCG" = "All Leaks",
                            "wLeaksPerSqKmALLCGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmALLCGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmALLCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_CGtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRtotalCG:wLeaksRRtotalCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRtotalCG" = "All Leaks",
                            "wLeaksRRtotalCGC1" = "Class 1 Leaks",
                            "wLeaksRRtotalCGC2" = "Class 2 Leaks",
                            "wLeaksRRtotalCGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAllRR_CGtract.png")

# Unrepaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerHUCG, wLeaksPerHURRCG, wLeaksPerHUCGC1, wLeaksPerHURRCGC1, wLeaksPerHUCGC2, wLeaksPerHURRCGC2, wLeaksPerHUCGC3, wLeaksPerHURRCGC3) %>% 
  arrange(desc(wLeaksPerHUCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Columbia Gas population-weighted mean unrepaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHUCG:wLeaksPerHUCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHUCG" = "All Leaks",
                            "wLeaksPerHUCGC1" = "Class 1 Leaks",
                            "wLeaksPerHUCGC2" = "Class 2 Leaks",
                            "wLeaksPerHUCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  drop_na(leakDensity) %>%
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_CGtract.png")

# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHURRCG:wLeaksPerHURRCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHURRCG" = "All Leaks",
                            "wLeaksPerHURRCGC1" = "Class 1 Leaks",
                            "wLeaksPerHURRCGC2" = "Class 2 Leaks",
                            "wLeaksPerHURRCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_RR_CGtract.png")

# Repaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wREPLeaksPerHUCG, wREPLeaksPerHURRCG, wREPLeaksPerHUCGC1, wREPLeaksPerHURRCGC1, wREPLeaksPerHUCGC2, wREPLeaksPerHURRCGC2, wREPLeaksPerHUCGC3, wREPLeaksPerHURRCGC3) %>% 
  arrange(desc(wREPLeaksPerHUCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Columbia Gas population-weighted mean repaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHUCG:wREPLeaksPerHUCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHUCG" = "All Leaks",
                            "wREPLeaksPerHUCGC1" = "Class 1 Leaks",
                            "wREPLeaksPerHUCGC2" = "Class 2 Leaks",
                            "wREPLeaksPerHUCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean repaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_CGtract.png")

# faceted dot graph of RR for repaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHURRCG:wREPLeaksPerHURRCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHURRCG" = "All Leaks",
                            "wREPLeaksPerHURRCGC1" = "Class 1 Leaks",
                            "wREPLeaksPerHURRCGC2" = "Class 2 Leaks",
                            "wREPLeaksPerHURRCGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Leaks Per Occupied Housing Unit in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_RR_CGtract.png")

# Total leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wALLLeaksPerHUCG, wALLLeaksPerHURRCG, wALLLeaksPerHUCGC1, wALLLeaksPerHURRCGC1, wALLLeaksPerHUCGC2, wALLLeaksPerHURRCGC2, wALLLeaksPerHUCGC3, wALLLeaksPerHURRCGC3) %>% 
  arrange(desc(wALLLeaksPerHUCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Columbia Gas population-weighted mean total leaks (repaired + unrepaired) per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHUCG:wALLLeaksPerHUCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHUCG" = "All Leaks",
                            "wALLLeaksPerHUCGC1" = "Class 1 Leaks",
                            "wALLLeaksPerHUCGC2" = "Class 2 Leaks",
                            "wALLLeaksPerHUCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean total leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_CGtract.png")

# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHURRCG:wALLLeaksPerHURRCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURRCG" = "All Leaks",
                            "wALLLeaksPerHURRCGC1" = "Class 1 Leaks",
                            "wALLLeaksPerHURRCGC2" = "Class 2 Leaks",
                            "wALLLeaksPerHURRCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_RR_CGtract.png")

# Average leak repair times
ppLeakDensityJoinedU %>% 
  select(Group, wDaysToRepairAvgCG, wDaysToRepairAvgRRCG, wDaysToRepairAvgCGC1, wDaysToRepairAvgRRCGC1, wDaysToRepairAvgCGC2, wDaysToRepairAvgRRCGC2, wDaysToRepairAvgCGC3, wDaysToRepairAvgRRCGC3) %>% 
  arrange(desc(wDaysToRepairAvgCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Columbia Gas population-weighted mean leak repair times (days) in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of average leak repair time by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgCG:wDaysToRepairAvgCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgCG" = "All Leaks",
                            "wDaysToRepairAvgCGC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgCGC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time (days) by Census Tract",
       title = "Priority Populations and Mean Leak Repair Time in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_CGtract.png")

# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgRRCG:wDaysToRepairAvgRRCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgRRCG" = "All Leaks",
                            "wDaysToRepairAvgRRCGC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgRRCGC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgRRCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Mean Leak Repair Time in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_RR_CGtract.png")


# Average age of unrepaired leaks
ppLeakDensityJoinedU %>% 
  select(Group, wLeakAgeDaysAvgCG, wLeakAgeDaysAvgRRCG, wLeakAgeDaysAvgCGC1, wLeakAgeDaysAvgRRCGC1, wLeakAgeDaysAvgCGC2, wLeakAgeDaysAvgRRCGC2, wLeakAgeDaysAvgCGC3, wLeakAgeDaysAvgRRCGC3) %>% 
  arrange(desc(wLeakAgeDaysAvgCG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Columbia Gas population-weighted mean age (days) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak age by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgCG:wLeakAgeDaysAvgCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgCG" = "All Leaks",
                            "wLeakAgeDaysAvgCGC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgCGC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  # drop_na(leakDensity) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_CGtract.png")

# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgRRCG:wLeakAgeDaysAvgRRCGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgRRCG" = "All Leaks",
                            "wLeakAgeDaysAvgRRCGC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgRRCGC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgRRCGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>% 
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
       title = "Relative Risk of Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Columbia Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_RR_CGtract.png")




# Liberty Utilities by Leak Class
# Unrepaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmLU, wLeaksRRLU, wLeaksPerSqKmLUC1, wLeaksRRLUC1, wLeaksPerSqKmLUC2, wLeaksRRLUC2, wLeaksPerSqKmLUC3, wLeaksRRLUC3) %>% 
  arrange(desc(wLeaksPerSqKmLU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Liberty Utilities population-weighted mean leak density (leaks/sqkm) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,3,2,2,2,2,2), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Unrepaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmLU:wLeaksPerSqKmLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmLU" = "All Leaks",
                            "wLeaksPerSqKmLUC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmLUC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_LUtract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRLU:wLeaksRRLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRLU" = "All Leaks",
                            "wLeaksRRLUC1" = "Class 1 Leaks",
                            "wLeaksRRLUC2" = "Class 2 Leaks",
                            "wLeaksRRLUC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassRR_LUtract.png")

# Repaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmREPLU, wLeaksRRrepairLU, wLeaksPerSqKmREPLUC1, wLeaksRRrepairLUC1, wLeaksPerSqKmREPLUC2, wLeaksRRrepairLUC2, wLeaksPerSqKmREPLUC3, wLeaksRRrepairLUC3) %>% 
  arrange(desc(wLeaksPerSqKmREPLU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Liberty Utilities population-weighted mean leak density (leaks/sqkm) of repaired leaks in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Repaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmREPLU:wLeaksPerSqKmREPLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREPLU" = "All Leaks",
                            "wLeaksPerSqKmREPLUC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmREPLUC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmREPLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Repaired Gas Leaks in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_LUtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRrepairLU:wLeaksRRrepairLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRrepairLU" = "All Leaks",
                            "wLeaksRRrepairLUC1" = "Class 1 Leaks",
                            "wLeaksRRrepairLUC2" = "Class 2 Leaks",
                            "wLeaksRRrepairLUC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREPRR_LUtract.png")

# Total leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmALLLU, wLeaksRRtotalLU, wLeaksPerSqKmALLLUC1, wLeaksRRtotalLUC1, wLeaksPerSqKmALLLUC2, wLeaksRRtotalLUC2, wLeaksPerSqKmALLLUC3, wLeaksRRtotalLUC3) %>% 
  arrange(desc(wLeaksPerSqKmALLLU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Liberty Utilities population-weighted mean leak density (leaks/sqkm) of all leaks (unrepaired + repaired) in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmALLLU:wLeaksPerSqKmALLLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALLLU" = "All Leaks",
                            "wLeaksPerSqKmALLLUC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmALLLUC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmALLLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_LUtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRtotalLU:wLeaksRRtotalLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRtotalLU" = "All Leaks",
                            "wLeaksRRtotalLUC1" = "Class 1 Leaks",
                            "wLeaksRRtotalLUC2" = "Class 2 Leaks",
                            "wLeaksRRtotalLUC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAllRR_LUtract.png")

# Unrepaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerHULU, wLeaksPerHURRLU, wLeaksPerHULUC1, wLeaksPerHURRLUC1, wLeaksPerHULUC2, wLeaksPerHURRLUC2, wLeaksPerHULUC3, wLeaksPerHURRLUC3) %>% 
  arrange(desc(wLeaksPerHULU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Liberty Utilities population-weighted mean unrepaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHULU:wLeaksPerHULUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHULU" = "All Leaks",
                            "wLeaksPerHULUC1" = "Class 1 Leaks",
                            "wLeaksPerHULUC2" = "Class 2 Leaks",
                            "wLeaksPerHULUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  drop_na(leakDensity) %>%
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_LUtract.png")

# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHURRLU:wLeaksPerHURRLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHURRLU" = "All Leaks",
                            "wLeaksPerHURRLUC1" = "Class 1 Leaks",
                            "wLeaksPerHURRLUC2" = "Class 2 Leaks",
                            "wLeaksPerHURRLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_RR_LUtract.png")

# Repaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wREPLeaksPerHULU, wREPLeaksPerHURRLU, wREPLeaksPerHULUC1, wREPLeaksPerHURRLUC1, wREPLeaksPerHULUC2, wREPLeaksPerHURRLUC2, wREPLeaksPerHULUC3, wREPLeaksPerHURRLUC3) %>% 
  arrange(desc(wREPLeaksPerHULU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Liberty Utilities population-weighted mean repaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHULU:wREPLeaksPerHULUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHULU" = "All Leaks",
                            "wREPLeaksPerHULUC1" = "Class 1 Leaks",
                            "wREPLeaksPerHULUC2" = "Class 2 Leaks",
                            "wREPLeaksPerHULUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean repaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_LUtract.png")

# faceted dot graph of RR for repaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHURRLU:wREPLeaksPerHURRLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHURRLU" = "All Leaks",
                            "wREPLeaksPerHURRLUC1" = "Class 1 Leaks",
                            "wREPLeaksPerHURRLUC2" = "Class 2 Leaks",
                            "wREPLeaksPerHURRLUC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Leaks Per Occupied Housing Unit in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_RR_LUtract.png")

# Total leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wALLLeaksPerHULU, wALLLeaksPerHURRLU, wALLLeaksPerHULUC1, wALLLeaksPerHURRLUC1, wALLLeaksPerHULUC2, wALLLeaksPerHURRLUC2, wALLLeaksPerHULUC3, wALLLeaksPerHURRLUC3) %>% 
  arrange(desc(wALLLeaksPerHULU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Liberty Utilities population-weighted mean total leaks (repaired + unrepaired) per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHULU:wALLLeaksPerHULUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHULU" = "All Leaks",
                            "wALLLeaksPerHULUC1" = "Class 1 Leaks",
                            "wALLLeaksPerHULUC2" = "Class 2 Leaks",
                            "wALLLeaksPerHULUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean total leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_LUtract.png")

# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHURRLU:wALLLeaksPerHURRLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURRLU" = "All Leaks",
                            "wALLLeaksPerHURRLUC1" = "Class 1 Leaks",
                            "wALLLeaksPerHURRLUC2" = "Class 2 Leaks",
                            "wALLLeaksPerHURRLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_RR_LUtract.png")

# Average leak repair times
ppLeakDensityJoinedU %>% 
  select(Group, wDaysToRepairAvgLU, wDaysToRepairAvgRRLU, wDaysToRepairAvgLUC1, wDaysToRepairAvgRRLUC1, wDaysToRepairAvgLUC2, wDaysToRepairAvgRRLUC2, wDaysToRepairAvgLUC3, wDaysToRepairAvgRRLUC3) %>% 
  arrange(desc(wDaysToRepairAvgLU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Liberty Utilities population-weighted mean leak repair times (days) in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of average leak repair time by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgLU:wDaysToRepairAvgLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgLU" = "All Leaks",
                            "wDaysToRepairAvgLUC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgLUC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time (days) by Census Tract",
       title = "Priority Populations and Mean Leak Repair Time in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_LUtract.png")

# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgRRLU:wDaysToRepairAvgRRLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgRRLU" = "All Leaks",
                            "wDaysToRepairAvgRRLUC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgRRLUC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgRRLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Mean Leak Repair Time in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_RR_LUtract.png")


# Average age of unrepaired leaks
ppLeakDensityJoinedU %>% 
  select(Group, wLeakAgeDaysAvgLU, wLeakAgeDaysAvgRRLU, wLeakAgeDaysAvgLUC1, wLeakAgeDaysAvgRRLUC1, wLeakAgeDaysAvgLUC2, wLeakAgeDaysAvgRRLUC2, wLeakAgeDaysAvgLUC3, wLeakAgeDaysAvgRRLUC3) %>% 
  arrange(desc(wLeakAgeDaysAvgLU)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Liberty Utilities population-weighted mean age (days) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak age by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgLU:wLeakAgeDaysAvgLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgLU" = "All Leaks",
                            "wLeakAgeDaysAvgLUC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgLUC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  # drop_na(leakDensity) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_LUtract.png")

# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgRRLU:wLeakAgeDaysAvgRRLUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgRRLU" = "All Leaks",
                            "wLeakAgeDaysAvgRRLUC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgRRLUC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgRRLUC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>% 
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
       title = "Relative Risk of Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Liberty Utilities", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_RR_LUtract.png")





# Berkshire Gas by Leak Class
# Unrepaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmBG, wLeaksRRBG, wLeaksPerSqKmBGC1, wLeaksRRBGC1, wLeaksPerSqKmBGC2, wLeaksRRBGC2, wLeaksPerSqKmBGC3, wLeaksRRBGC3) %>% 
  arrange(desc(wLeaksPerSqKmBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Berkshire Gas population-weighted mean leak density (leaks/sqkm) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,3,2,2,2,2,2), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Unrepaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmBG:wLeaksPerSqKmBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(Group != "MA Limited English HH") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmBG" = "All Leaks",
                            "wLeaksPerSqKmBGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmBGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_BGtract.png")

# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRBG:wLeaksRRBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRBG" = "All Leaks",
                            "wLeaksRRBGC1" = "Class 1 Leaks",
                            "wLeaksRRBGC2" = "Class 2 Leaks",
                            "wLeaksRRBGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassRR_BGtract.png")

# Repaired leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmREPBG, wLeaksRRrepairBG, wLeaksPerSqKmREPBGC1, wLeaksRRrepairBGC1, wLeaksPerSqKmREPBGC2, wLeaksRRrepairBGC2, wLeaksPerSqKmREPBGC3, wLeaksRRrepairBGC3) %>% 
  arrange(desc(wLeaksPerSqKmREPBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Berkshire Gas population-weighted mean leak density (leaks/sqkm) of repaired leaks in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Repaired Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmREPBG:wLeaksPerSqKmREPBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREPBG" = "All Leaks",
                            "wLeaksPerSqKmREPBGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmREPBGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmREPBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Repaired Gas Leaks in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_BGtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRrepairBG:wLeaksRRrepairBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRrepairBG" = "All Leaks",
                            "wLeaksRRrepairBGC1" = "Class 1 Leaks",
                            "wLeaksRRrepairBGC2" = "Class 2 Leaks",
                            "wLeaksRRrepairBGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Gas Leaks in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREPRR_BGtract.png")

# Total leaks per square kilometer
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerSqKmALLBG, wLeaksRRtotalBG, wLeaksPerSqKmALLBGC1, wLeaksRRtotalBGC1, wLeaksPerSqKmALLBGC2, wLeaksRRtotalBGC2, wLeaksPerSqKmALLBGC3, wLeaksRRtotalBGC3) %>% 
  arrange(desc(wLeaksPerSqKmALLBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Berkshire Gas population-weighted mean leak density (leaks/sqkm) of all leaks (unrepaired + repaired) in 2019.", align = "r", digits = c(0,1,1,2,1,2,1,2,1), 
        col.names = c("Group","Per SqKm","RR","Per SqKm","RR",
                      "Per SqKm","RR","Per SqKm","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leak density by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmALLBG:wLeaksPerSqKmALLBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALLBG" = "All Leaks",
                            "wLeaksPerSqKmALLBGC1" = "Class 1 Leaks",
                            "wLeaksPerSqKmALLBGC2" = "Class 2 Leaks",
                            "wLeaksPerSqKmALLBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", " by Census Tract",sep = "")),
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_BGtract.png")

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksRRtotalBG:wLeaksRRtotalBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksRRtotalBG" = "All Leaks",
                            "wLeaksRRtotalBGC1" = "Class 1 Leaks",
                            "wLeaksRRtotalBGC2" = "Class 2 Leaks",
                            "wLeaksRRtotalBGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAllRR_BGtract.png")

# Unrepaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wLeaksPerHUBG, wLeaksPerHURRBG, wLeaksPerHUBGC1, wLeaksPerHURRBGC1, wLeaksPerHUBGC2, wLeaksPerHURRBGC2, wLeaksPerHUBGC3, wLeaksPerHURRBGC3) %>% 
  arrange(desc(wLeaksPerHUBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Berkshire Gas population-weighted mean unrepaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHUBG:wLeaksPerHUBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHUBG" = "All Leaks",
                            "wLeaksPerHUBGC1" = "Class 1 Leaks",
                            "wLeaksPerHUBGC2" = "Class 2 Leaks",
                            "wLeaksPerHUBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  drop_na(leakDensity) %>%
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_BGtract.png")

# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerHURRBG:wLeaksPerHURRBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerHURRBG" = "All Leaks",
                            "wLeaksPerHURRBGC1" = "Class 1 Leaks",
                            "wLeaksPerHURRBGC2" = "Class 2 Leaks",
                            "wLeaksPerHURRBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClass_HU_RR_BGtract.png")

# Repaired leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wREPLeaksPerHUBG, wREPLeaksPerHURRBG, wREPLeaksPerHUBGC1, wREPLeaksPerHURRBGC1, wREPLeaksPerHUBGC2, wREPLeaksPerHURRBGC2, wREPLeaksPerHUBGC3, wREPLeaksPerHURRBGC3) %>% 
  arrange(desc(wREPLeaksPerHUBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ',', scientific = FALSE), 
        caption = "Berkshire Gas population-weighted mean repaired leaks per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of repaired leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHUBG:wREPLeaksPerHUBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHUBG" = "All Leaks",
                            "wREPLeaksPerHUBGC1" = "Class 1 Leaks",
                            "wREPLeaksPerHUBGC2" = "Class 2 Leaks",
                            "wREPLeaksPerHUBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean repaired leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Repaired Gas Leaks Per Occupied Housing Unit in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_BGtract.png")

# faceted dot graph of RR for repaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wREPLeaksPerHURRBG:wREPLeaksPerHURRBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wREPLeaksPerHURRBG" = "All Leaks",
                            "wREPLeaksPerHURRBGC1" = "Class 1 Leaks",
                            "wREPLeaksPerHURRBGC2" = "Class 2 Leaks",
                            "wREPLeaksPerHURRBGC3" = "Class 3 Leaks"),
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
       title = "Relative Risk of Priority Populations and Repaired Leaks Per Occupied Housing Unit in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassREP_HU_RR_BGtract.png")

# Total leaks per occupied housing unit
ppLeakDensityJoinedU %>% 
  select(Group, wALLLeaksPerHUBG, wALLLeaksPerHURRBG, wALLLeaksPerHUBGC1, wALLLeaksPerHURRBGC1, wALLLeaksPerHUBGC2, wALLLeaksPerHURRBGC2, wALLLeaksPerHUBGC3, wALLLeaksPerHURRBGC3) %>% 
  arrange(desc(wALLLeaksPerHUBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Berkshire Gas population-weighted mean total leaks (repaired + unrepaired) per occupied housing unit in 2019.", align = "r", digits = c(0,3,2,5,2,4,2,4,2), 
        col.names = c("Group","Per OHU","RR","Per OHU","RR",
                      "Per OHU","RR","Per OHU","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHUBG:wALLLeaksPerHUBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHUBG" = "All Leaks",
                            "wALLLeaksPerHUBGC1" = "Class 1 Leaks",
                            "wALLLeaksPerHUBGC2" = "Class 2 Leaks",
                            "wALLLeaksPerHUBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean total leaks per occupied housing unit by Census Tract",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_BGtract.png")

# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wALLLeaksPerHURRBG:wALLLeaksPerHURRBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURRBG" = "All Leaks",
                            "wALLLeaksPerHURRBGC1" = "Class 1 Leaks",
                            "wALLLeaksPerHURRBGC2" = "Class 2 Leaks",
                            "wALLLeaksPerHURRBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAll_HU_RR_BGtract.png")

# Average leak repair times
ppLeakDensityJoinedU %>% 
  select(Group, wDaysToRepairAvgBG, wDaysToRepairAvgRRBG, wDaysToRepairAvgBGC1, wDaysToRepairAvgRRBGC1, wDaysToRepairAvgBGC2, wDaysToRepairAvgRRBGC2, wDaysToRepairAvgBGC3, wDaysToRepairAvgRRBGC3) %>% 
  arrange(desc(wDaysToRepairAvgBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Berkshire Gas population-weighted mean leak repair times (days) in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of average leak repair time by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgBG:wDaysToRepairAvgBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgBG" = "All Leaks",
                            "wDaysToRepairAvgBGC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgBGC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean leak repair time (days) by Census Tract",
       title = "Priority Populations and Mean Leak Repair Time in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_BGtract.png")

# faceted dot graph of RR for leak repair time around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wDaysToRepairAvgRRBG:wDaysToRepairAvgRRBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wDaysToRepairAvgRRBG" = "All Leaks",
                            "wDaysToRepairAvgRRBGC1" = "Class 1 Leaks",
                            "wDaysToRepairAvgRRBGC2" = "Class 2 Leaks",
                            "wDaysToRepairAvgRRBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>%
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
       title = "Relative Risk of Priority Populations and Mean Leak Repair Time in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassTime_RR_BGtract.png")


# Average age of unrepaired leaks
ppLeakDensityJoinedU %>% 
  select(Group, wLeakAgeDaysAvgBG, wLeakAgeDaysAvgRRBG, wLeakAgeDaysAvgBGC1, wLeakAgeDaysAvgRRBGC1, wLeakAgeDaysAvgBGC2, wLeakAgeDaysAvgRRBGC2, wLeakAgeDaysAvgBGC3, wLeakAgeDaysAvgRRBGC3) %>% 
  arrange(desc(wLeakAgeDaysAvgBG)) %>% 
  kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Berkshire Gas population-weighted mean age (days) of unrepaired leaks in 2019.", align = "r", digits = c(0,1,2,1,2,1,2,1,2), 
        col.names = c("Group","Days","RR","Days","RR",
                      "Days","RR","Days","RR")) %>% 
  add_header_above(., c(" ", "All Leaks" = 2, "Class 1 Leaks" = 2, 
                        "Class 2 Leaks" = 2, "Class 3 Leaks" = 2)) %>% 
  kable_styling(latex_options = c("repeat_header")) %>% 
  add_footnote(., "RR = Relative Risk or ratio of group leak density to leak density of total population, total households, or total occupied housing units", notation = "none")

# faceted bar graph of unrepaired leak age by leak class
ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgBG:wLeakAgeDaysAvgBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgBG" = "All Leaks",
                            "wLeakAgeDaysAvgBGC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgBGC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  # drop_na(leakDensity) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
       title = "Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_BGtract.png")

# faceted dot graph of RR for leak age around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensityJoinedU %>% 
  pivot_longer(wLeakAgeDaysAvgRRBG:wLeakAgeDaysAvgRRBGC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvgRRBG" = "All Leaks",
                            "wLeakAgeDaysAvgRRBGC1" = "Class 1 Leaks",
                            "wLeakAgeDaysAvgRRBGC2" = "Class 2 Leaks",
                            "wLeakAgeDaysAvgRRBGC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass),
         Status = case_when(
           leakDensity == 1 ~ "Pop Mean",
           leakDensity > 1 ~ "Greater Risk",
           leakDensity < 1 ~ "Lower Risk"
         )) %>% 
  # drop_na(leakDensity) %>% 
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
       title = "Relative Risk of Priority Populations and Mean Age of Unrepaired Leaks in 2019 for Berkshire Gas", caption = "Based on 5-year ACS 2015-19 estimates")

ggsave("Images/LeaksPPbyClassAge_RR_BGtract.png")



### Test for significant differences using Wilcoxon test and Dunn's pairwise comparison tests
# create a small df
df <- ma_tracts %>% 
  as.data.frame() %>% 
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,disabledOver18E,
         renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE, 
         AllLeaks2019_sqkm, AllLeaks2019C1_sqkm, AllLeaks2019C2_sqkm, 
         AllLeaks2019C3_sqkm, starts_with("leaks_sqkm"), starts_with("REPleaks"), 
         starts_with("leaks_hu"), starts_with("ALLleaks_hu"),
         starts_with("LeakAgeDaysAvg"), starts_with("DaysToRepairAvg"))

# pivot df to long format so that so new df = #groups x #rows
dfp <- df %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- df %>% 
  select(eng_limitE, eng_hhE, c(AllLeaks2019_sqkm:DaysToRepairAvgC3)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- df %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(AllLeaks2019_sqkm:DaysToRepairAvgC3)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# # get summary stats. should all be identical
# dfp %>% 
#   group_by(group) %>% 
#   rstatix::get_summary_stats(leaks_sqkm, type = "common")

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkm, type = "common") %>% 
  arrange(desc(median))

dfpW_HH %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkm, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkm, type = "common") %>% 
  arrange(desc(median))

# use kruskal-wallis test like ANOVA
dfpW_OHU %>% 
  rstatix::kruskal_test(leaks_sqkm ~ group)

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkm <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkm ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkm <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkm ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkm <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkm ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkm %>% 
    rbind(pwdt_HH_leaks_sqkm, pwdt_OHU_leaks_sqkm) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_sqkm.csv"))

# C1 unrepaired leaks per sqkm
pwdt_leaks_sqkmC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmC1 %>% 
    rbind(pwdt_HH_leaks_sqkmC1, pwdt_OHU_leaks_sqkmC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_sqkmC1.csv"))

# C2 unrepaired leaks per sqkm
pwdt_leaks_sqkmC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmC2 %>% 
    rbind(pwdt_HH_leaks_sqkmC2, pwdt_OHU_leaks_sqkmC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_sqkmC2.csv"))

# C3 unrepaired leaks per sqkm
pwdt_leaks_sqkmC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmC3 %>% 
    rbind(pwdt_HH_leaks_sqkmC3, pwdt_OHU_leaks_sqkmC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_sqkmC3.csv"))


# repaired leaks per sqkm
pwdt_REPleaks_sqkm <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkm ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkm <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkm ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkm <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkm ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkm %>% 
    rbind(pwdt_HH_REPleaks_sqkm, pwdt_OHU_REPleaks_sqkm) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_sqkm.csv"))

# C1 repaired leaks per sqkm
pwdt_REPleaks_sqkmC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmC1 %>% 
    rbind(pwdt_HH_REPleaks_sqkmC1, pwdt_OHU_REPleaks_sqkmC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_sqkmC1.csv"))

# C2 repaired leaks per sqkm
pwdt_REPleaks_sqkmC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmC2 %>% 
    rbind(pwdt_HH_REPleaks_sqkmC2, pwdt_OHU_REPleaks_sqkmC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_sqkmC2.csv"))

# C3 repaired leaks per sqkm
pwdt_REPleaks_sqkmC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmC3 %>% 
    rbind(pwdt_HH_REPleaks_sqkmC3, pwdt_OHU_REPleaks_sqkmC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_sqkmC3.csv"))


# unrepaired leaks per OHU
pwdt_leaks_hu <- dfpW %>% 
  rstatix::dunn_test(leaks_hu ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_hu <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_hu ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_hu <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_hu ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_hu %>% 
    rbind(pwdt_HH_leaks_hu, pwdt_OHU_leaks_hu) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_hu.csv"))

# C1 unrepaired leaks per OHU
pwdt_leaks_huC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_huC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huC1 %>% 
    rbind(pwdt_HH_leaks_huC1, pwdt_OHU_leaks_huC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_huC1.csv"))

# C2 unrepaired leaks per OHU
pwdt_leaks_huC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_huC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huC2 %>% 
    rbind(pwdt_HH_leaks_huC2, pwdt_OHU_leaks_huC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_huC2.csv"))

# C3 unrepaired leaks per OHU
pwdt_leaks_huC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_huC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huC3 %>% 
    rbind(pwdt_HH_leaks_huC3, pwdt_OHU_leaks_huC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_leaks_huC3.csv"))


# repaired leaks per OHU
pwdt_REPleaks_hu <- dfpW %>% 
  rstatix::dunn_test(REPleaks_hu ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_hu <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_hu ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_hu <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_hu ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_hu %>% 
    rbind(pwdt_HH_REPleaks_hu, pwdt_OHU_REPleaks_hu) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_hu.csv"))

# C1 repaired leaks per OHU
pwdt_REPleaks_huC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huC1 %>% 
    rbind(pwdt_HH_REPleaks_huC1, pwdt_OHU_REPleaks_huC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_huC1.csv"))

# C2 repaired leaks per OHU
pwdt_REPleaks_huC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huC2 %>% 
    rbind(pwdt_HH_REPleaks_huC2, pwdt_OHU_REPleaks_huC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_huC2.csv"))

# C3 repaired leaks per OHU
pwdt_REPleaks_huC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huC3 %>% 
    rbind(pwdt_HH_REPleaks_huC3, pwdt_OHU_REPleaks_huC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_REPleaks_huC3.csv"))


# days to repair avg
pwdt_DaysToRepairAvg <- dfpW %>% 
  drop_na(DaysToRepairAvg) %>% 
  rstatix::dunn_test(DaysToRepairAvg ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvg <- dfpW_HH %>% 
  drop_na(DaysToRepairAvg) %>%
  rstatix::dunn_test(DaysToRepairAvg ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvg <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvg) %>%
  rstatix::dunn_test(DaysToRepairAvg ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvg %>% 
    rbind(pwdt_HH_DaysToRepairAvg, pwdt_OHU_DaysToRepairAvg) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_DaysToRepairAvg.csv"))

# C1 days to repair avg
pwdt_DaysToRepairAvgC1 <- dfpW %>% 
  drop_na(DaysToRepairAvgC1) %>% 
  rstatix::dunn_test(DaysToRepairAvgC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgC1 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgC1) %>%
  rstatix::dunn_test(DaysToRepairAvgC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgC1 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgC1) %>%
  rstatix::dunn_test(DaysToRepairAvgC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgC1 %>% 
    rbind(pwdt_HH_DaysToRepairAvgC1, pwdt_OHU_DaysToRepairAvgC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_DaysToRepairAvgC1.csv"))

# C2 days to repair avg
pwdt_DaysToRepairAvgC2 <- dfpW %>% 
  drop_na(DaysToRepairAvgC2) %>% 
  rstatix::dunn_test(DaysToRepairAvgC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgC2 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgC2) %>%
  rstatix::dunn_test(DaysToRepairAvgC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgC2 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgC2) %>%
  rstatix::dunn_test(DaysToRepairAvgC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgC2 %>% 
    rbind(pwdt_HH_DaysToRepairAvgC2, pwdt_OHU_DaysToRepairAvgC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_DaysToRepairAvgC2.csv"))

# C3 days to repair avg
pwdt_DaysToRepairAvgC3 <- dfpW %>% 
  drop_na(DaysToRepairAvgC3) %>% 
  rstatix::dunn_test(DaysToRepairAvgC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgC3 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgC3) %>%
  rstatix::dunn_test(DaysToRepairAvgC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgC3 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgC3) %>%
  rstatix::dunn_test(DaysToRepairAvgC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgC3 %>% 
    rbind(pwdt_HH_DaysToRepairAvgC3, pwdt_OHU_DaysToRepairAvgC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_DaysToRepairAvgC3.csv"))


# leak age days avg
pwdt_LeakAgeDaysAvg <- dfpW %>% 
  drop_na(LeakAgeDaysAvg) %>% 
  rstatix::dunn_test(LeakAgeDaysAvg ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvg <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvg) %>%
  rstatix::dunn_test(LeakAgeDaysAvg ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvg <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvg) %>%
  rstatix::dunn_test(LeakAgeDaysAvg ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvg %>% 
    rbind(pwdt_HH_LeakAgeDaysAvg, pwdt_OHU_LeakAgeDaysAvg) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_LeakAgeDaysAvg.csv"))

# C1 leak age days avg
pwdt_LeakAgeDaysAvgC1 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgC1) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgC1 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgC1 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgC1 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgC1, pwdt_OHU_LeakAgeDaysAvgC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_LeakAgeDaysAvgC1.csv"))

# C2 leak age days avg
pwdt_LeakAgeDaysAvgC2 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgC2) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgC2 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgC2 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgC2 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgC2, pwdt_OHU_LeakAgeDaysAvgC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_LeakAgeDaysAvgC2.csv"))

# C3 leak age days avg
pwdt_LeakAgeDaysAvgC3 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgC3) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgC3 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgC3 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgC3 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgC3, pwdt_OHU_LeakAgeDaysAvgC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/pwdt_LeakAgeDaysAvgC3.csv"))






### Test for significant differences for each utility using Wilcoxon test and Dunn's pairwise comparison tests
# Create df for NG to analyze
ppLeakDensityNG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^National") | 
           GAS == "Colonial Gas") %>% # limit to BGs in NG/Colonial svc area
  mutate(leaks_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                           `National Grid - Colonial Gas_19unrepaired`)/area_sqkm,
         leaks_sqkmNGC1 = (`National Grid - Boston Gas_19unrepairedC1` +
                             `National Grid - Colonial Gas_19unrepairedC1`)/area_sqkm,
         leaks_sqkmNGC2 = (`National Grid - Boston Gas_19unrepairedC2` +
                             `National Grid - Colonial Gas_19unrepairedC2`)/area_sqkm,
         leaks_sqkmNGC3 = (`National Grid - Boston Gas_19unrepairedC3` +
                             `National Grid - Colonial Gas_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmNG = (`National Grid - Boston Gas_19repaired` +
                              `National Grid - Colonial Gas_19repaired`)/area_sqkm,
         REPleaks_sqkmNGC1 = (`National Grid - Boston Gas_19repairedC1` +
                                `National Grid - Colonial Gas_19repairedC1`)/area_sqkm,
         REPleaks_sqkmNGC2 = (`National Grid - Boston Gas_19repairedC2` +
                                `National Grid - Colonial Gas_19repairedC2`)/area_sqkm,
         REPleaks_sqkmNGC3 = (`National Grid - Boston Gas_19repairedC3` +
                                `National Grid - Colonial Gas_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                                  `National Grid - Colonial Gas_19unrepaired` + 
                                  `National Grid - Boston Gas_19repaired` +
                                  `National Grid - Colonial Gas_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmNGC1 = (`National Grid - Boston Gas_19unrepairedC1` +
                                    `National Grid - Colonial Gas_19unrepairedC1` + 
                                    `National Grid - Boston Gas_19repairedC1` +
                                    `National Grid - Colonial Gas_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmNGC2 = (`National Grid - Boston Gas_19unrepairedC2` +
                                    `National Grid - Colonial Gas_19unrepairedC2` + 
                                    `National Grid - Boston Gas_19repairedC2` +
                                    `National Grid - Colonial Gas_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmNGC3 = (`National Grid - Boston Gas_19unrepairedC3` +
                                    `National Grid - Colonial Gas_19unrepairedC3` + 
                                    `National Grid - Boston Gas_19repairedC3` +
                                    `National Grid - Colonial Gas_19repairedC3`)/area_sqkm,
         leaks_huNG = if_else(total_occ_unitsE == 0, 0, 
                              (`National Grid - Boston Gas_19unrepaired` +
                                 `National Grid - Colonial Gas_19unrepaired`)/total_occ_unitsE),
         leaks_huNGC1 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid - Boston Gas_19unrepairedC1` +
                                   `National Grid - Colonial Gas_19unrepairedC1`)/total_occ_unitsE),
         leaks_huNGC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid - Boston Gas_19unrepairedC2` +
                                   `National Grid - Colonial Gas_19unrepairedC2`)/total_occ_unitsE),
         leaks_huNGC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`National Grid - Boston Gas_19unrepairedC3` +
                                   `National Grid - Colonial Gas_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huNG = if_else(total_occ_unitsE == 0, 0,
                                 (`National Grid - Boston Gas_19repaired` +
                                    `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
         REPleaks_huNGC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid - Boston Gas_19repairedC1` +
                                      `National Grid - Colonial Gas_19repairedC1`)/total_occ_unitsE),
         REPleaks_huNGC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid - Boston Gas_19repairedC2` +
                                      `National Grid - Colonial Gas_19repairedC2`)/total_occ_unitsE),
         REPleaks_huNGC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`National Grid - Boston Gas_19repairedC3` +
                                      `National Grid - Colonial Gas_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huNG = if_else(total_occ_unitsE == 0, 0,
                                     (`National Grid - Boston Gas_19unrepaired` + `National Grid - Colonial Gas_19unrepaired` + 
                                        `National Grid - Boston Gas_19repaired` +
                                        `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huNGC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid - Boston Gas_19unrepairedC1` + `National Grid - Colonial Gas_19unrepairedC1` + 
                                          `National Grid - Boston Gas_19repairedC1` +
                                          `National Grid - Colonial Gas_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huNGC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid - Boston Gas_19unrepairedC2` + `National Grid - Colonial Gas_19unrepairedC2` + 
                                          `National Grid - Boston Gas_19repairedC2` +
                                          `National Grid - Colonial Gas_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huNGC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`National Grid - Boston Gas_19unrepairedC3` + `National Grid - Colonial Gas_19unrepairedC3` + 
                                          `National Grid - Boston Gas_19repairedC3` +
                                          `National Grid - Colonial Gas_19repairedC3`)/total_occ_unitsE),
         PctRepaired19NG = (`National Grid - Boston Gas_19repaired` +
                              `National Grid - Colonial Gas_19repaired`)/ (`National Grid - Boston Gas_19unrepaired` +
                                                                             `National Grid - Colonial Gas_19unrepaired` + 
                                                                             `National Grid - Boston Gas_19repaired` +
                                                                             `National Grid - Colonial Gas_19repaired`)*100) %>% 
  rowwise() %>% 
  mutate(DaysToRepairAvgNG = sum(`National Grid - Boston Gas_19repairedDaysAvg`,                                 `National Grid - Colonial Gas_19repairedDaysAvg`, na.rm = TRUE)/2,
         DaysToRepairAvgNGC1 = sum(`National Grid - Boston Gas_19repairedDaysAvgC1`,                                 `National Grid - Colonial Gas_19repairedDaysAvgC1`, na.rm = TRUE)/2,
         DaysToRepairAvgNGC2 = sum(`National Grid - Boston Gas_19repairedDaysAvgC2`,                                 `National Grid - Colonial Gas_19repairedDaysAvgC2`, na.rm = TRUE)/2,
         DaysToRepairAvgNGC3 = sum(`National Grid - Boston Gas_19repairedDaysAvgC3`,                                 `National Grid - Colonial Gas_19repairedDaysAvgC3`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNG = sum(`National Grid - Boston Gas_19unrepairedDaysAvg`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvg`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNGC1 = sum(`National Grid - Boston Gas_19unrepairedDaysAvgC1`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvgC1`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNGC2 = sum(`National Grid - Boston Gas_19unrepairedDaysAvgC2`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvgC2`, na.rm = TRUE)/2,
         LeakAgeDaysAvgNGC3 = sum(`National Grid - Boston Gas_19unrepairedDaysAvgC3`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvgC3`, na.rm = TRUE)/2) %>% 
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,disabledOver18E,
         renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE,
         (starts_with("leaks_") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("AllLeaks") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))),
         (starts_with("REPleaks_") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))),
         (starts_with("leaks_hu") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("NG") | ends_with("NGC1") | ends_with("NGC2") | ends_with("NGC3")))) 


# pivot df to long format so that so new df = #groups x #rows
dfp <- ppLeakDensityNG %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- ppLeakDensityNG %>% 
  select(eng_limitE, eng_hhE, c(leaks_sqkmNG:DaysToRepairAvgNGC3)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- ppLeakDensityNG %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(leaks_sqkmNG:DaysToRepairAvgNGC3)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE),
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmNG, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmNG, type = "common") %>% 
  arrange(desc(median))

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkmNG <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmNG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmNG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmNG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmNG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmNG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmNG %>% 
    rbind(pwdt_HH_leaks_sqkmNG, pwdt_OHU_leaks_sqkmNG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_sqkmNG.csv"))

# C1 unrepaired leaks per sqkm
pwdt_leaks_sqkmNGC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmNGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmNGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmNGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmNGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmNGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmNGC1 %>% 
    rbind(pwdt_HH_leaks_sqkmNGC1, pwdt_OHU_leaks_sqkmNGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_sqkmNGC1.csv"))

# C2 unrepaired leaks per sqkm
pwdt_leaks_sqkmNGC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmNGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmNGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmNGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmNGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmNGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmNGC2 %>% 
    rbind(pwdt_HH_leaks_sqkmNGC2, pwdt_OHU_leaks_sqkmNGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_sqkmNGC2.csv"))

# C3 unrepaired leaks per sqkm
pwdt_leaks_sqkmNGC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmNGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmNGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmNGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmNGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmNGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmNGC3 %>% 
    rbind(pwdt_HH_leaks_sqkmNGC3, pwdt_OHU_leaks_sqkmNGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_sqkmNGC3.csv"))

# repaired leaks per sqkm
pwdt_REPleaks_sqkmNG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmNG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmNG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmNG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmNG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmNG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmNG %>% 
    rbind(pwdt_HH_REPleaks_sqkmNG, pwdt_OHU_REPleaks_sqkmNG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_sqkmNG.csv"))

# C1 repaired leaks per sqkm
pwdt_REPleaks_sqkmNGC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmNGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmNGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmNGC1 %>% 
    rbind(pwdt_HH_REPleaks_sqkmNGC1, pwdt_OHU_REPleaks_sqkmNGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_sqkmNGC1.csv"))

# C2 repaired leaks per sqkm
pwdt_REPleaks_sqkmNGC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmNGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmNGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmNGC2 %>% 
    rbind(pwdt_HH_REPleaks_sqkmNGC2, pwdt_OHU_REPleaks_sqkmNGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_sqkmNGC2.csv"))

# C3 repaired leaks per sqkm
pwdt_REPleaks_sqkmNGC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmNGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmNGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmNGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmNGC3 %>% 
    rbind(pwdt_HH_REPleaks_sqkmNGC3, pwdt_OHU_REPleaks_sqkmNGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_sqkmNGC3.csv"))

# unrepaired leaks per OHU
pwdt_leaks_huNG <- dfpW %>% 
  rstatix::dunn_test(leaks_huNG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huNG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huNG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huNG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huNG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huNG %>% 
    rbind(pwdt_HH_leaks_huNG, pwdt_OHU_leaks_huNG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_huNG.csv"))

# C1 unrepaired leaks per OHU
pwdt_leaks_huNGC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_huNGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huNGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huNGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huNGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huNGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huNGC1 %>% 
    rbind(pwdt_HH_leaks_huNGC1, pwdt_OHU_leaks_huNGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_huNGC1.csv"))

# C2 unrepaired leaks per OHU
pwdt_leaks_huNGC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_huNGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huNGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huNGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huNGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huNGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huNGC2 %>% 
    rbind(pwdt_HH_leaks_huNGC2, pwdt_OHU_leaks_huNGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_huNGC2.csv"))

# C3 unrepaired leaks per OHU
pwdt_leaks_huNGC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_huNGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huNGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huNGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huNGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huNGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huNGC3 %>% 
    rbind(pwdt_HH_leaks_huNGC3, pwdt_OHU_leaks_huNGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_leaks_huNGC3.csv"))

# repaired leaks per OHU
pwdt_REPleaks_huNG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huNG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huNG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huNG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huNG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huNG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huNG %>% 
    rbind(pwdt_HH_REPleaks_huNG, pwdt_OHU_REPleaks_huNG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_huNG.csv"))

# C1 repaired leaks per OHU
pwdt_REPleaks_huNGC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huNGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huNGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huNGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huNGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huNGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huNGC1 %>% 
    rbind(pwdt_HH_REPleaks_huNGC1, pwdt_OHU_REPleaks_huNGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_huNGC1.csv"))

# C2 repaired leaks per OHU
pwdt_REPleaks_huNGC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huNGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huNGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huNGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huNGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huNGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huNGC2 %>% 
    rbind(pwdt_HH_REPleaks_huNGC2, pwdt_OHU_REPleaks_huNGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_huNGC2.csv"))

# C3 repaired leaks per OHU
pwdt_REPleaks_huNGC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huNGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huNGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huNGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huNGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huNGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huNGC3 %>% 
    rbind(pwdt_HH_REPleaks_huNGC3, pwdt_OHU_REPleaks_huNGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_REPleaks_huNGC3.csv"))

# days to repair avg
pwdt_DaysToRepairAvgNG <- dfpW %>% 
  drop_na(DaysToRepairAvgNG) %>% 
  rstatix::dunn_test(DaysToRepairAvgNG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgNG <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgNG) %>%
  rstatix::dunn_test(DaysToRepairAvgNG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgNG <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgNG) %>%
  rstatix::dunn_test(DaysToRepairAvgNG ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgNG %>% 
    rbind(pwdt_HH_DaysToRepairAvgNG, pwdt_OHU_DaysToRepairAvgNG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_DaysToRepairAvgNG.csv"))

# C1 days to repair avg
pwdt_DaysToRepairAvgNGC1 <- dfpW %>% 
  drop_na(DaysToRepairAvgNGC1) %>% 
  rstatix::dunn_test(DaysToRepairAvgNGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgNGC1 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgNGC1) %>%
  rstatix::dunn_test(DaysToRepairAvgNGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgNGC1 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgNGC1) %>%
  rstatix::dunn_test(DaysToRepairAvgNGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgNGC1 %>% 
    rbind(pwdt_HH_DaysToRepairAvgNGC1, pwdt_OHU_DaysToRepairAvgNGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_DaysToRepairAvgNGC1.csv"))

# C2 days to repair avg
pwdt_DaysToRepairAvgNGC2 <- dfpW %>% 
  drop_na(DaysToRepairAvgNGC2) %>% 
  rstatix::dunn_test(DaysToRepairAvgNGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgNGC2 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgNGC2) %>%
  rstatix::dunn_test(DaysToRepairAvgNGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgNGC2 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgNGC2) %>%
  rstatix::dunn_test(DaysToRepairAvgNGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgNGC2 %>% 
    rbind(pwdt_HH_DaysToRepairAvgNGC2, pwdt_OHU_DaysToRepairAvgNGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_DaysToRepairAvgNGC2.csv"))

# C3 days to repair avg
pwdt_DaysToRepairAvgNGC3 <- dfpW %>% 
  drop_na(DaysToRepairAvgNGC3) %>% 
  rstatix::dunn_test(DaysToRepairAvgNGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgNGC3 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgNGC3) %>%
  rstatix::dunn_test(DaysToRepairAvgNGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgNGC3 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgNGC3) %>%
  rstatix::dunn_test(DaysToRepairAvgNGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgNGC3 %>% 
    rbind(pwdt_HH_DaysToRepairAvgNGC3, pwdt_OHU_DaysToRepairAvgNGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_DaysToRepairAvgNGC3.csv"))

# leak age days avg
pwdt_LeakAgeDaysAvgNG <- dfpW %>% 
  drop_na(LeakAgeDaysAvgNG) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgNG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgNG <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgNG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgNG <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgNG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNG ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgNG %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgNG, pwdt_OHU_LeakAgeDaysAvgNG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_LeakAgeDaysAvgNG.csv"))

# C1 leak age days avg
pwdt_LeakAgeDaysAvgNGC1 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgNGC1) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgNGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgNGC1 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgNGC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgNGC1 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgNGC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgNGC1 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgNGC1, pwdt_OHU_LeakAgeDaysAvgNGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_LeakAgeDaysAvgNGC1.csv"))

# C2 leak age days avg
pwdt_LeakAgeDaysAvgNGC2 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgNGC2) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgNGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgNGC2 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgNGC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgNGC2 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgNGC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgNGC2 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgNGC2, pwdt_OHU_LeakAgeDaysAvgNGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_LeakAgeDaysAvgNGC2.csv"))

# C3 leak age days avg
pwdt_LeakAgeDaysAvgNGC3 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgNGC3) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgNGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgNGC3 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgNGC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgNGC3 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgNGC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgNGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgNGC3 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgNGC3, pwdt_OHU_LeakAgeDaysAvgNGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/NG/pwdt_LeakAgeDaysAvgNGC3.csv"))



### Eversource
ppLeakDensityEV <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Eversource") | 
           GAS == "Energy$") %>% # limit to BGs in EV svc area
  mutate(leaks_sqkmEV = (`Eversource Energy_19unrepaired`)/area_sqkm,
         # leaks_sqkmEVC1 = (`Eversource Energy_19unrepairedC1`)/area_sqkm,
         leaks_sqkmEVC1 = 0,
         leaks_sqkmEVC2 = (`Eversource Energy_19unrepairedC2`)/area_sqkm,
         leaks_sqkmEVC3 = (`Eversource Energy_19unrepairedC3`)/area_sqkm,
         REPleaks_sqkmEV = (`Eversource Energy_19repaired`)/area_sqkm,
         REPleaks_sqkmEVC1 = (`Eversource Energy_19repairedC1`)/area_sqkm,
         REPleaks_sqkmEVC2 = (`Eversource Energy_19repairedC2`)/area_sqkm,
         REPleaks_sqkmEVC3 = (`Eversource Energy_19repairedC3`)/area_sqkm,
         AllLeaks2019_sqkmEV = (`Eversource Energy_19unrepaired` +
                                  `Eversource Energy_19repaired`)/area_sqkm,
         # AllLeaks2019_sqkmEVC1 = (`Eversource Energy_19unrepairedC1` + 
         #                            `Eversource Energy_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmEVC1 = (`Eversource Energy_19repairedC1`)/area_sqkm,
         AllLeaks2019_sqkmEVC2 = (`Eversource Energy_19unrepairedC2` + 
                                    `Eversource Energy_19repairedC2`)/area_sqkm,
         AllLeaks2019_sqkmEVC3 = (`Eversource Energy_19unrepairedC3` + 
                                    `Eversource Energy_19repairedC3`)/area_sqkm,
         leaks_huEV = if_else(total_occ_unitsE == 0, 0, 
                              (`Eversource Energy_19unrepaired`)/total_occ_unitsE),
         # leaks_huEVC1 = if_else(total_occ_unitsE == 0, 0, 
         #                        (`Eversource Energy_19unrepairedC1`)/total_occ_unitsE),
         leaks_huEVC1 = 0,
         leaks_huEVC2 = if_else(total_occ_unitsE == 0, 0, 
                                (`Eversource Energy_19unrepairedC2`)/total_occ_unitsE),
         leaks_huEVC3 = if_else(total_occ_unitsE == 0, 0, 
                                (`Eversource Energy_19unrepairedC3`)/total_occ_unitsE),
         REPleaks_huEV = if_else(total_occ_unitsE == 0, 0,
                                 (`Eversource Energy_19repaired`)/total_occ_unitsE),
         REPleaks_huEVC1 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource Energy_19repairedC1`)/total_occ_unitsE),
         REPleaks_huEVC2 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource Energy_19repairedC2`)/total_occ_unitsE),
         REPleaks_huEVC3 = if_else(total_occ_unitsE == 0, 0,
                                   (`Eversource Energy_19repairedC3`)/total_occ_unitsE),
         AllLeaks2019_huEV = if_else(total_occ_unitsE == 0, 0,
                                     (`Eversource Energy_19unrepaired` + 
                                        `Eversource Energy_19repaired`)/total_occ_unitsE),
         # AllLeaks2019_huEVC1 = if_else(total_occ_unitsE == 0, 0,
         #                               (`Eversource Energy_19unrepairedC1` +
         #                                  `Eversource Energy_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huEVC1 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource Energy_19repairedC1`)/total_occ_unitsE),
         AllLeaks2019_huEVC2 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource Energy_19unrepairedC2` +
                                          `Eversource Energy_19repairedC2`)/total_occ_unitsE),
         AllLeaks2019_huEVC3 = if_else(total_occ_unitsE == 0, 0,
                                       (`Eversource Energy_19unrepairedC3` +
                                          `Eversource Energy_19repairedC3`)/total_occ_unitsE),
         PctRepaired19EV = (`Eversource Energy_19repaired`)/ (`Eversource Energy_19unrepaired` + `Eversource Energy_19repaired`)*100,
         DaysToRepairAvgEV = `Eversource Energy_19repairedDaysAvg`,
         DaysToRepairAvgEVC1 = `Eversource Energy_19repairedDaysAvgC1`,
         DaysToRepairAvgEVC2 = `Eversource Energy_19repairedDaysAvgC2`,
         DaysToRepairAvgEVC3 = `Eversource Energy_19repairedDaysAvgC3`,
         LeakAgeDaysAvgEV = `Eversource Energy_19unrepairedDaysAvg`,
         # LeakAgeDaysAvgEVC1 = `Eversource Energy_19unrepairedDaysAvgC1`,
         LeakAgeDaysAvgEVC1 = NA,
         LeakAgeDaysAvgEVC2 = `Eversource Energy_19unrepairedDaysAvgC2`,
         LeakAgeDaysAvgEVC3 = `Eversource Energy_19unrepairedDaysAvgC3`) %>% 
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,
         disabledOver18E,renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE, (starts_with("leaks_") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("AllLeaks") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))),
         (starts_with("REPleaks_") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("PctRepaired19") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))),
         (starts_with("leaks_hu") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("EV") | ends_with("EVC1") | ends_with("EVC2") | ends_with("EVC3")))) 

# pivot df to long format so that so new df = #groups x #rows
dfp <- ppLeakDensityEV %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- ppLeakDensityEV %>% 
  select(eng_limitE, eng_hhE, c(leaks_sqkmEV:DaysToRepairAvgEVC3)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- ppLeakDensityEV %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(leaks_sqkmEV:DaysToRepairAvgEVC3)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE),
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmEV, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmEV, type = "common") %>% 
  arrange(desc(median))

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkmEV <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmEV ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmEV <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmEV ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmEV <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmEV ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmEV %>% 
    rbind(pwdt_HH_leaks_sqkmEV, pwdt_OHU_leaks_sqkmEV) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_leaks_sqkmEV.csv"))

# # C1 unrepaired leaks per sqkm
# pwdt_leaks_sqkmEVC1 <- dfpW %>% 
#   rstatix::dunn_test(leaks_sqkmEVC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_sqkmEVC1 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_sqkmEVC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_sqkmEVC1 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_sqkmEVC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_sqkmEVC1 %>% 
#     rbind(pwdt_HH_leaks_sqkmEVC1, pwdt_OHU_leaks_sqkmEVC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/EV/pwdt_leaks_sqkmEVC1.csv"))

# C2 unrepaired leaks per sqkm
pwdt_leaks_sqkmEVC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmEVC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmEVC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmEVC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmEVC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmEVC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmEVC2 %>% 
    rbind(pwdt_HH_leaks_sqkmEVC2, pwdt_OHU_leaks_sqkmEVC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_leaks_sqkmEVC2.csv"))

# C3 unrepaired leaks per sqkm
pwdt_leaks_sqkmEVC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmEVC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmEVC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmEVC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmEVC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmEVC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmEVC3 %>% 
    rbind(pwdt_HH_leaks_sqkmEVC3, pwdt_OHU_leaks_sqkmEVC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_leaks_sqkmEVC3.csv"))

# repaired leaks per sqkm
pwdt_REPleaks_sqkmEV <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmEV ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmEV <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmEV ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmEV <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmEV ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmEV %>% 
    rbind(pwdt_HH_REPleaks_sqkmEV, pwdt_OHU_REPleaks_sqkmEV) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_sqkmEV.csv"))

# C1 repaired leaks per sqkm
pwdt_REPleaks_sqkmEVC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmEVC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmEVC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmEVC1 %>% 
    rbind(pwdt_HH_REPleaks_sqkmEVC1, pwdt_OHU_REPleaks_sqkmEVC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_sqkmEVC1.csv"))

# C2 repaired leaks per sqkm
pwdt_REPleaks_sqkmEVC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmEVC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmEVC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmEVC2 %>% 
    rbind(pwdt_HH_REPleaks_sqkmEVC2, pwdt_OHU_REPleaks_sqkmEVC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_sqkmEVC2.csv"))

# C3 repaired leaks per sqkm
pwdt_REPleaks_sqkmEVC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmEVC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmEVC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmEVC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmEVC3 %>% 
    rbind(pwdt_HH_REPleaks_sqkmEVC3, pwdt_OHU_REPleaks_sqkmEVC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_sqkmEVC3.csv"))

# unrepaired leaks per OHU
pwdt_leaks_huEV <- dfpW %>% 
  rstatix::dunn_test(leaks_huEV ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huEV <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huEV ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huEV <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huEV ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huEV %>% 
    rbind(pwdt_HH_leaks_huEV, pwdt_OHU_leaks_huEV) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_leaks_huEV.csv"))

# # C1 unrepaired leaks per OHU
# pwdt_leaks_huEVC1 <- dfpW %>% 
#   rstatix::dunn_test(leaks_huEVC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_huEVC1 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_huEVC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_huEVC1 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_huEVC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_huEVC1 %>% 
#     rbind(pwdt_HH_leaks_huEVC1, pwdt_OHU_leaks_huEVC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/EV/pwdt_leaks_huEVC1.csv"))

# C2 unrepaired leaks per OHU
pwdt_leaks_huEVC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_huEVC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huEVC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huEVC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huEVC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huEVC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huEVC2 %>% 
    rbind(pwdt_HH_leaks_huEVC2, pwdt_OHU_leaks_huEVC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_leaks_huEVC2.csv"))

# C3 unrepaired leaks per OHU
pwdt_leaks_huEVC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_huEVC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huEVC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huEVC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huEVC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huEVC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huEVC3 %>% 
    rbind(pwdt_HH_leaks_huEVC3, pwdt_OHU_leaks_huEVC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_leaks_huEVC3.csv"))

# repaired leaks per OHU
pwdt_REPleaks_huEV <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huEV ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huEV <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huEV ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huEV <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huEV ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huEV %>% 
    rbind(pwdt_HH_REPleaks_huEV, pwdt_OHU_REPleaks_huEV) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_huEV.csv"))

# C1 repaired leaks per OHU
pwdt_REPleaks_huEVC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huEVC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huEVC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huEVC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huEVC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huEVC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huEVC1 %>% 
    rbind(pwdt_HH_REPleaks_huEVC1, pwdt_OHU_REPleaks_huEVC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_huEVC1.csv"))

# C2 repaired leaks per OHU
pwdt_REPleaks_huEVC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huEVC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huEVC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huEVC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huEVC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huEVC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huEVC2 %>% 
    rbind(pwdt_HH_REPleaks_huEVC2, pwdt_OHU_REPleaks_huEVC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_huEVC2.csv"))

# C3 repaired leaks per OHU
pwdt_REPleaks_huEVC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huEVC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huEVC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huEVC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huEVC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huEVC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huEVC3 %>% 
    rbind(pwdt_HH_REPleaks_huEVC3, pwdt_OHU_REPleaks_huEVC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_REPleaks_huEVC3.csv"))

# days to repair avg
pwdt_DaysToRepairAvgEV <- dfpW %>% 
  drop_na(DaysToRepairAvgEV) %>% 
  rstatix::dunn_test(DaysToRepairAvgEV ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgEV <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgEV) %>%
  rstatix::dunn_test(DaysToRepairAvgEV ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgEV <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgEV) %>%
  rstatix::dunn_test(DaysToRepairAvgEV ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgEV %>% 
    rbind(pwdt_HH_DaysToRepairAvgEV, pwdt_OHU_DaysToRepairAvgEV) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_DaysToRepairAvgEV.csv"))

# C1 days to repair avg
pwdt_DaysToRepairAvgEVC1 <- dfpW %>% 
  drop_na(DaysToRepairAvgEVC1) %>% 
  rstatix::dunn_test(DaysToRepairAvgEVC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgEVC1 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgEVC1) %>%
  rstatix::dunn_test(DaysToRepairAvgEVC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgEVC1 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgEVC1) %>%
  rstatix::dunn_test(DaysToRepairAvgEVC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgEVC1 %>% 
    rbind(pwdt_HH_DaysToRepairAvgEVC1, pwdt_OHU_DaysToRepairAvgEVC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_DaysToRepairAvgEVC1.csv"))

# C2 days to repair avg
pwdt_DaysToRepairAvgEVC2 <- dfpW %>% 
  drop_na(DaysToRepairAvgEVC2) %>% 
  rstatix::dunn_test(DaysToRepairAvgEVC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgEVC2 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgEVC2) %>%
  rstatix::dunn_test(DaysToRepairAvgEVC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgEVC2 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgEVC2) %>%
  rstatix::dunn_test(DaysToRepairAvgEVC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgEVC2 %>% 
    rbind(pwdt_HH_DaysToRepairAvgEVC2, pwdt_OHU_DaysToRepairAvgEVC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_DaysToRepairAvgEVC2.csv"))

# C3 days to repair avg
pwdt_DaysToRepairAvgEVC3 <- dfpW %>% 
  drop_na(DaysToRepairAvgEVC3) %>% 
  rstatix::dunn_test(DaysToRepairAvgEVC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgEVC3 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgEVC3) %>%
  rstatix::dunn_test(DaysToRepairAvgEVC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgEVC3 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgEVC3) %>%
  rstatix::dunn_test(DaysToRepairAvgEVC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgEVC3 %>% 
    rbind(pwdt_HH_DaysToRepairAvgEVC3, pwdt_OHU_DaysToRepairAvgEVC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_DaysToRepairAvgEVC3.csv"))

# leak age days avg
pwdt_LeakAgeDaysAvgEV <- dfpW %>% 
  drop_na(LeakAgeDaysAvgEV) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgEV ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgEV <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgEV) %>%
  rstatix::dunn_test(LeakAgeDaysAvgEV ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgEV <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgEV) %>%
  rstatix::dunn_test(LeakAgeDaysAvgEV ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgEV %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgEV, pwdt_OHU_LeakAgeDaysAvgEV) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_LeakAgeDaysAvgEV.csv"))

# # C1 leak age days avg
# pwdt_LeakAgeDaysAvgEVC1 <- dfpW %>% 
#   drop_na(LeakAgeDaysAvgEVC1) %>% 
#   rstatix::dunn_test(LeakAgeDaysAvgEVC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_LeakAgeDaysAvgEVC1 <- dfpW_HH %>% 
#   drop_na(LeakAgeDaysAvgEVC1) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgEVC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_LeakAgeDaysAvgEVC1 <- dfpW_OHU %>% 
#   drop_na(LeakAgeDaysAvgEVC1) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgEVC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_LeakAgeDaysAvgEVC1 %>% 
#     rbind(pwdt_HH_LeakAgeDaysAvgEVC1, pwdt_OHU_LeakAgeDaysAvgEVC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/EV/pwdt_LeakAgeDaysAvgEVC1.csv"))

# C2 leak age days avg
pwdt_LeakAgeDaysAvgEVC2 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgEVC2) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgEVC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgEVC2 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgEVC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgEVC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgEVC2 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgEVC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgEVC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgEVC2 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgEVC2, pwdt_OHU_LeakAgeDaysAvgEVC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_LeakAgeDaysAvgEVC2.csv"))

# C3 leak age days avg
pwdt_LeakAgeDaysAvgEVC3 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgEVC3) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgEVC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgEVC3 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgEVC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgEVC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgEVC3 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgEVC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgEVC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgEVC3 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgEVC3, pwdt_OHU_LeakAgeDaysAvgEVC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/EV/pwdt_LeakAgeDaysAvgEVC3.csv"))






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
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,
         disabledOver18E,
         renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE, (starts_with("leaks_") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("AllLeaks") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))),
         (starts_with("REPleaks_") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))),
         (starts_with("leaks_hu") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("CG") | ends_with("CGC1") | ends_with("CGC2") | ends_with("CGC3"))))

# pivot df to long format so that so new df = #groups x #rows
dfp <- ppLeakDensityCG %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- ppLeakDensityCG %>% 
  select(eng_limitE, eng_hhE, c(leaks_sqkmCG:DaysToRepairAvgCGC3)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- ppLeakDensityCG %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(leaks_sqkmCG:DaysToRepairAvgCGC3)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE),
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmCG, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmCG, type = "common") %>% 
  arrange(desc(median))

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkmCG <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmCG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmCG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmCG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmCG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmCG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmCG %>% 
    rbind(pwdt_HH_leaks_sqkmCG, pwdt_OHU_leaks_sqkmCG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_sqkmCG.csv"))

# C1 unrepaired leaks per sqkm
pwdt_leaks_sqkmCGC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmCGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmCGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmCGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmCGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmCGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmCGC1 %>% 
    rbind(pwdt_HH_leaks_sqkmCGC1, pwdt_OHU_leaks_sqkmCGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_sqkmCGC1.csv"))

# C2 unrepaired leaks per sqkm
pwdt_leaks_sqkmCGC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmCGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmCGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmCGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmCGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmCGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmCGC2 %>% 
    rbind(pwdt_HH_leaks_sqkmCGC2, pwdt_OHU_leaks_sqkmCGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_sqkmCGC2.csv"))

# C3 unrepaired leaks per sqkm
pwdt_leaks_sqkmCGC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmCGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmCGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmCGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmCGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmCGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmCGC3 %>% 
    rbind(pwdt_HH_leaks_sqkmCGC3, pwdt_OHU_leaks_sqkmCGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_sqkmCGC3.csv"))

# repaired leaks per sqkm
pwdt_REPleaks_sqkmCG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmCG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmCG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmCG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmCG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmCG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmCG %>% 
    rbind(pwdt_HH_REPleaks_sqkmCG, pwdt_OHU_REPleaks_sqkmCG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_sqkmCG.csv"))

# C1 repaired leaks per sqkm
pwdt_REPleaks_sqkmCGC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmCGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmCGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmCGC1 %>% 
    rbind(pwdt_HH_REPleaks_sqkmCGC1, pwdt_OHU_REPleaks_sqkmCGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_sqkmCGC1.csv"))

# C2 repaired leaks per sqkm
pwdt_REPleaks_sqkmCGC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmCGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmCGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmCGC2 %>% 
    rbind(pwdt_HH_REPleaks_sqkmCGC2, pwdt_OHU_REPleaks_sqkmCGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_sqkmCGC2.csv"))

# C3 repaired leaks per sqkm
pwdt_REPleaks_sqkmCGC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmCGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmCGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmCGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmCGC3 %>% 
    rbind(pwdt_HH_REPleaks_sqkmCGC3, pwdt_OHU_REPleaks_sqkmCGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_sqkmCGC3.csv"))

# unrepaired leaks per OHU
pwdt_leaks_huCG <- dfpW %>% 
  rstatix::dunn_test(leaks_huCG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huCG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huCG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huCG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huCG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huCG %>% 
    rbind(pwdt_HH_leaks_huCG, pwdt_OHU_leaks_huCG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_huCG.csv"))

# C1 unrepaired leaks per OHU
pwdt_leaks_huCGC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_huCGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huCGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huCGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huCGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huCGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huCGC1 %>% 
    rbind(pwdt_HH_leaks_huCGC1, pwdt_OHU_leaks_huCGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_huCGC1.csv"))

# C2 unrepaired leaks per OHU
pwdt_leaks_huCGC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_huCGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huCGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huCGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huCGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huCGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huCGC2 %>% 
    rbind(pwdt_HH_leaks_huCGC2, pwdt_OHU_leaks_huCGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_huCGC2.csv"))

# C3 unrepaired leaks per OHU
pwdt_leaks_huCGC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_huCGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huCGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huCGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huCGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huCGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huCGC3 %>% 
    rbind(pwdt_HH_leaks_huCGC3, pwdt_OHU_leaks_huCGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_leaks_huCGC3.csv"))

# repaired leaks per OHU
pwdt_REPleaks_huCG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huCG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huCG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huCG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huCG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huCG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huCG %>% 
    rbind(pwdt_HH_REPleaks_huCG, pwdt_OHU_REPleaks_huCG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_huCG.csv"))

# C1 repaired leaks per OHU
pwdt_REPleaks_huCGC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huCGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huCGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huCGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huCGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huCGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huCGC1 %>% 
    rbind(pwdt_HH_REPleaks_huCGC1, pwdt_OHU_REPleaks_huCGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_huCGC1.csv"))

# C2 repaired leaks per OHU
pwdt_REPleaks_huCGC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huCGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huCGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huCGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huCGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huCGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huCGC2 %>% 
    rbind(pwdt_HH_REPleaks_huCGC2, pwdt_OHU_REPleaks_huCGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_huCGC2.csv"))

# C3 repaired leaks per OHU
pwdt_REPleaks_huCGC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huCGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huCGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huCGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huCGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huCGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huCGC3 %>% 
    rbind(pwdt_HH_REPleaks_huCGC3, pwdt_OHU_REPleaks_huCGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_REPleaks_huCGC3.csv"))

# days to repair avg
pwdt_DaysToRepairAvgCG <- dfpW %>% 
  drop_na(DaysToRepairAvgCG) %>% 
  rstatix::dunn_test(DaysToRepairAvgCG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgCG <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgCG) %>%
  rstatix::dunn_test(DaysToRepairAvgCG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgCG <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgCG) %>%
  rstatix::dunn_test(DaysToRepairAvgCG ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgCG %>% 
    rbind(pwdt_HH_DaysToRepairAvgCG, pwdt_OHU_DaysToRepairAvgCG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_DaysToRepairAvgCG.csv"))

# C1 days to repair avg
pwdt_DaysToRepairAvgCGC1 <- dfpW %>% 
  drop_na(DaysToRepairAvgCGC1) %>% 
  rstatix::dunn_test(DaysToRepairAvgCGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgCGC1 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgCGC1) %>%
  rstatix::dunn_test(DaysToRepairAvgCGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgCGC1 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgCGC1) %>%
  rstatix::dunn_test(DaysToRepairAvgCGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgCGC1 %>% 
    rbind(pwdt_HH_DaysToRepairAvgCGC1, pwdt_OHU_DaysToRepairAvgCGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_DaysToRepairAvgCGC1.csv"))

# C2 days to repair avg
pwdt_DaysToRepairAvgCGC2 <- dfpW %>% 
  drop_na(DaysToRepairAvgCGC2) %>% 
  rstatix::dunn_test(DaysToRepairAvgCGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgCGC2 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgCGC2) %>%
  rstatix::dunn_test(DaysToRepairAvgCGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgCGC2 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgCGC2) %>%
  rstatix::dunn_test(DaysToRepairAvgCGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgCGC2 %>% 
    rbind(pwdt_HH_DaysToRepairAvgCGC2, pwdt_OHU_DaysToRepairAvgCGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_DaysToRepairAvgCGC2.csv"))

# C3 days to repair avg
pwdt_DaysToRepairAvgCGC3 <- dfpW %>% 
  drop_na(DaysToRepairAvgCGC3) %>% 
  rstatix::dunn_test(DaysToRepairAvgCGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgCGC3 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgCGC3) %>%
  rstatix::dunn_test(DaysToRepairAvgCGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgCGC3 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgCGC3) %>%
  rstatix::dunn_test(DaysToRepairAvgCGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgCGC3 %>% 
    rbind(pwdt_HH_DaysToRepairAvgCGC3, pwdt_OHU_DaysToRepairAvgCGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_DaysToRepairAvgCGC3.csv"))

# leak age days avg
pwdt_LeakAgeDaysAvgCG <- dfpW %>% 
  drop_na(LeakAgeDaysAvgCG) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgCG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgCG <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgCG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgCG <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgCG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCG ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgCG %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgCG, pwdt_OHU_LeakAgeDaysAvgCG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_LeakAgeDaysAvgCG.csv"))

# C1 leak age days avg
pwdt_LeakAgeDaysAvgCGC1 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgCGC1) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgCGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgCGC1 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgCGC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgCGC1 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgCGC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgCGC1 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgCGC1, pwdt_OHU_LeakAgeDaysAvgCGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_LeakAgeDaysAvgCGC1.csv"))

# C2 leak age days avg
pwdt_LeakAgeDaysAvgCGC2 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgCGC2) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgCGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgCGC2 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgCGC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgCGC2 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgCGC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgCGC2 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgCGC2, pwdt_OHU_LeakAgeDaysAvgCGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_LeakAgeDaysAvgCGC2.csv"))

# C3 leak age days avg
pwdt_LeakAgeDaysAvgCGC3 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgCGC3) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgCGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgCGC3 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgCGC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgCGC3 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgCGC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgCGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgCGC3 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgCGC3, pwdt_OHU_LeakAgeDaysAvgCGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/CG/pwdt_LeakAgeDaysAvgCGC3.csv"))




### Fitchburg Gas aka Unitil. NO CLASS FOR LEAKS!.
ppLeakDensityFG <- ma_tracts %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Unitil") | 
           GAS == "Unitil$") %>% # limit to BGs in FG svc area
  mutate(leaks_sqkmFG = (`Fitchburg Gas_19unrepaired`)/area_sqkm,
         REPleaks_sqkmFG = (`Fitchburg Gas_19repaired`)/area_sqkm,
         AllLeaks2019_sqkmFG = (`Fitchburg Gas_19unrepaired` +
                                  `Fitchburg Gas_19repaired`)/area_sqkm,
         leaks_huFG = if_else(total_occ_unitsE == 0, 0, 
                              (`Fitchburg Gas_19unrepaired`)/total_occ_unitsE),
         REPleaks_huFG = if_else(total_occ_unitsE == 0, 0,
                                 (`Fitchburg Gas_19repaired`)/total_occ_unitsE),
         AllLeaks2019_huFG = if_else(total_occ_unitsE == 0, 0,
                                     (`Fitchburg Gas_19unrepaired` + 
                                        `Fitchburg Gas_19repaired`)/total_occ_unitsE),
         PctRepaired19FG = (`Fitchburg Gas_19repaired`)/ (`Fitchburg Gas_19unrepaired` + `Fitchburg Gas_19repaired`)*100,
         DaysToRepairAvgFG = `Fitchburg Gas_19repairedDaysAvg`,
         LeakAgeDaysAvgFG = `Fitchburg Gas_19unrepairedDaysAvg`) %>% 
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,
         disabledOver18E,
         renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE, (starts_with("leaks_") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("AllLeaks") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))),
         (starts_with("REPleaks_") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))),
         (starts_with("leaks_hu") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("FG") | ends_with("FGC1") | ends_with("FGC2") | ends_with("FGC3"))))


# pivot df to long format so that so new df = #groups x #rows
dfp <- ppLeakDensityFG %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- ppLeakDensityFG %>% 
  select(eng_limitE, eng_hhE, c(leaks_sqkmFG:DaysToRepairAvgFG)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- ppLeakDensityFG %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(leaks_sqkmFG:DaysToRepairAvgFG)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE),
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmFG, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmFG, type = "common") %>% 
  arrange(desc(median))

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkmFG <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmFG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmFG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmFG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmFG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmFG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmFG %>% 
    rbind(pwdt_HH_leaks_sqkmFG, pwdt_OHU_leaks_sqkmFG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/FG/pwdt_leaks_sqkmFG.csv"))

# # C1 unrepaired leaks per sqkm
# pwdt_leaks_sqkmFGC1 <- dfpW %>% 
#   rstatix::dunn_test(leaks_sqkmFGC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_sqkmFGC1 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_sqkmFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_sqkmFGC1 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_sqkmFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_sqkmFGC1 %>% 
#     rbind(pwdt_HH_leaks_sqkmFGC1, pwdt_OHU_leaks_sqkmFGC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_leaks_sqkmFGC1.csv"))
# 
# # C2 unrepaired leaks per sqkm
# pwdt_leaks_sqkmFGC2 <- dfpW %>% 
#   rstatix::dunn_test(leaks_sqkmFGC2 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_sqkmFGC2 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_sqkmFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_sqkmFGC2 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_sqkmFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_sqkmFGC2 %>% 
#     rbind(pwdt_HH_leaks_sqkmFGC2, pwdt_OHU_leaks_sqkmFGC2) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_leaks_sqkmFGC2.csv"))
# 
# # C3 unrepaired leaks per sqkm
# pwdt_leaks_sqkmFGC3 <- dfpW %>% 
#   rstatix::dunn_test(leaks_sqkmFGC3 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_sqkmFGC3 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_sqkmFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_sqkmFGC3 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_sqkmFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_sqkmFGC3 %>% 
#     rbind(pwdt_HH_leaks_sqkmFGC3, pwdt_OHU_leaks_sqkmFGC3) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_leaks_sqkmFGC3.csv"))

# repaired leaks per sqkm
pwdt_REPleaks_sqkmFG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmFG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmFG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmFG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmFG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmFG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmFG %>% 
    rbind(pwdt_HH_REPleaks_sqkmFG, pwdt_OHU_REPleaks_sqkmFG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/FG/pwdt_REPleaks_sqkmFG.csv"))

# # C1 repaired leaks per sqkm
# pwdt_REPleaks_sqkmFGC1 <- dfpW %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_REPleaks_sqkmFGC1 <- dfpW_HH %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_REPleaks_sqkmFGC1 <- dfpW_OHU %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_REPleaks_sqkmFGC1 %>% 
#     rbind(pwdt_HH_REPleaks_sqkmFGC1, pwdt_OHU_REPleaks_sqkmFGC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_REPleaks_sqkmFGC1.csv"))
# 
# # C2 repaired leaks per sqkm
# pwdt_REPleaks_sqkmFGC2 <- dfpW %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC2 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_REPleaks_sqkmFGC2 <- dfpW_HH %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_REPleaks_sqkmFGC2 <- dfpW_OHU %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_REPleaks_sqkmFGC2 %>% 
#     rbind(pwdt_HH_REPleaks_sqkmFGC2, pwdt_OHU_REPleaks_sqkmFGC2) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_REPleaks_sqkmFGC2.csv"))
# 
# # C3 repaired leaks per sqkm
# pwdt_REPleaks_sqkmFGC3 <- dfpW %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC3 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_REPleaks_sqkmFGC3 <- dfpW_HH %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_REPleaks_sqkmFGC3 <- dfpW_OHU %>% 
#   rstatix::dunn_test(REPleaks_sqkmFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_REPleaks_sqkmFGC3 %>% 
#     rbind(pwdt_HH_REPleaks_sqkmFGC3, pwdt_OHU_REPleaks_sqkmFGC3) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_REPleaks_sqkmFGC3.csv"))

# unrepaired leaks per OHU
pwdt_leaks_huFG <- dfpW %>% 
  rstatix::dunn_test(leaks_huFG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huFG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huFG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huFG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huFG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huFG %>% 
    rbind(pwdt_HH_leaks_huFG, pwdt_OHU_leaks_huFG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/FG/pwdt_leaks_huFG.csv"))

# # C1 unrepaired leaks per OHU
# pwdt_leaks_huFGC1 <- dfpW %>% 
#   rstatix::dunn_test(leaks_huFGC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_huFGC1 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_huFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_huFGC1 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_huFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_huFGC1 %>% 
#     rbind(pwdt_HH_leaks_huFGC1, pwdt_OHU_leaks_huFGC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_leaks_huFGC1.csv"))
# 
# # C2 unrepaired leaks per OHU
# pwdt_leaks_huFGC2 <- dfpW %>% 
#   rstatix::dunn_test(leaks_huFGC2 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_huFGC2 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_huFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_huFGC2 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_huFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_huFGC2 %>% 
#     rbind(pwdt_HH_leaks_huFGC2, pwdt_OHU_leaks_huFGC2) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_leaks_huFGC2.csv"))
# 
# # C3 unrepaired leaks per OHU
# pwdt_leaks_huFGC3 <- dfpW %>% 
#   rstatix::dunn_test(leaks_huFGC3 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_leaks_huFGC3 <- dfpW_HH %>% 
#   rstatix::dunn_test(leaks_huFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_leaks_huFGC3 <- dfpW_OHU %>% 
#   rstatix::dunn_test(leaks_huFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_leaks_huFGC3 %>% 
#     rbind(pwdt_HH_leaks_huFGC3, pwdt_OHU_leaks_huFGC3) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_leaks_huFGC3.csv"))

# repaired leaks per OHU
pwdt_REPleaks_huFG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huFG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huFG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huFG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huFG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huFG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huFG %>% 
    rbind(pwdt_HH_REPleaks_huFG, pwdt_OHU_REPleaks_huFG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/FG/pwdt_REPleaks_huFG.csv"))

# # C1 repaired leaks per OHU
# pwdt_REPleaks_huFGC1 <- dfpW %>% 
#   rstatix::dunn_test(REPleaks_huFGC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_REPleaks_huFGC1 <- dfpW_HH %>% 
#   rstatix::dunn_test(REPleaks_huFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_REPleaks_huFGC1 <- dfpW_OHU %>% 
#   rstatix::dunn_test(REPleaks_huFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_REPleaks_huFGC1 %>% 
#     rbind(pwdt_HH_REPleaks_huFGC1, pwdt_OHU_REPleaks_huFGC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_REPleaks_huFGC1.csv"))
# 
# # C2 repaired leaks per OHU
# pwdt_REPleaks_huFGC2 <- dfpW %>% 
#   rstatix::dunn_test(REPleaks_huFGC2 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_REPleaks_huFGC2 <- dfpW_HH %>% 
#   rstatix::dunn_test(REPleaks_huFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_REPleaks_huFGC2 <- dfpW_OHU %>% 
#   rstatix::dunn_test(REPleaks_huFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_REPleaks_huFGC2 %>% 
#     rbind(pwdt_HH_REPleaks_huFGC2, pwdt_OHU_REPleaks_huFGC2) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_REPleaks_huFGC2.csv"))
# 
# # C3 repaired leaks per OHU
# pwdt_REPleaks_huFGC3 <- dfpW %>% 
#   rstatix::dunn_test(REPleaks_huFGC3 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_REPleaks_huFGC3 <- dfpW_HH %>% 
#   rstatix::dunn_test(REPleaks_huFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_REPleaks_huFGC3 <- dfpW_OHU %>% 
#   rstatix::dunn_test(REPleaks_huFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_REPleaks_huFGC3 %>% 
#     rbind(pwdt_HH_REPleaks_huFGC3, pwdt_OHU_REPleaks_huFGC3) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_REPleaks_huFGC3.csv"))

# days to repair avg
pwdt_DaysToRepairAvgFG <- dfpW %>% 
  drop_na(DaysToRepairAvgFG) %>% 
  rstatix::dunn_test(DaysToRepairAvgFG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgFG <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgFG) %>%
  rstatix::dunn_test(DaysToRepairAvgFG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgFG <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgFG) %>%
  rstatix::dunn_test(DaysToRepairAvgFG ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgFG %>% 
    rbind(pwdt_HH_DaysToRepairAvgFG, pwdt_OHU_DaysToRepairAvgFG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/FG/pwdt_DaysToRepairAvgFG.csv"))

# # C1 days to repair avg
# pwdt_DaysToRepairAvgFGC1 <- dfpW %>% 
#   drop_na(DaysToRepairAvgFGC1) %>% 
#   rstatix::dunn_test(DaysToRepairAvgFGC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_DaysToRepairAvgFGC1 <- dfpW_HH %>% 
#   drop_na(DaysToRepairAvgFGC1) %>%
#   rstatix::dunn_test(DaysToRepairAvgFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_DaysToRepairAvgFGC1 <- dfpW_OHU %>% 
#   drop_na(DaysToRepairAvgFGC1) %>%
#   rstatix::dunn_test(DaysToRepairAvgFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_DaysToRepairAvgFGC1 %>% 
#     rbind(pwdt_HH_DaysToRepairAvgFGC1, pwdt_OHU_DaysToRepairAvgFGC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_DaysToRepairAvgFGC1.csv"))
# 
# # C2 days to repair avg
# pwdt_DaysToRepairAvgFGC2 <- dfpW %>% 
#   drop_na(DaysToRepairAvgFGC2) %>% 
#   rstatix::dunn_test(DaysToRepairAvgFGC2 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_DaysToRepairAvgFGC2 <- dfpW_HH %>% 
#   drop_na(DaysToRepairAvgFGC2) %>%
#   rstatix::dunn_test(DaysToRepairAvgFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_DaysToRepairAvgFGC2 <- dfpW_OHU %>% 
#   drop_na(DaysToRepairAvgFGC2) %>%
#   rstatix::dunn_test(DaysToRepairAvgFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_DaysToRepairAvgFGC2 %>% 
#     rbind(pwdt_HH_DaysToRepairAvgFGC2, pwdt_OHU_DaysToRepairAvgFGC2) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_DaysToRepairAvgFGC2.csv"))
# 
# # C3 days to repair avg
# pwdt_DaysToRepairAvgFGC3 <- dfpW %>% 
#   drop_na(DaysToRepairAvgFGC3) %>% 
#   rstatix::dunn_test(DaysToRepairAvgFGC3 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_DaysToRepairAvgFGC3 <- dfpW_HH %>% 
#   drop_na(DaysToRepairAvgFGC3) %>%
#   rstatix::dunn_test(DaysToRepairAvgFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_DaysToRepairAvgFGC3 <- dfpW_OHU %>% 
#   drop_na(DaysToRepairAvgFGC3) %>%
#   rstatix::dunn_test(DaysToRepairAvgFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_DaysToRepairAvgFGC3 %>% 
#     rbind(pwdt_HH_DaysToRepairAvgFGC3, pwdt_OHU_DaysToRepairAvgFGC3) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_DaysToRepairAvgFGC3.csv"))

# leak age days avg
pwdt_LeakAgeDaysAvgFG <- dfpW %>% 
  drop_na(LeakAgeDaysAvgFG) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgFG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgFG <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgFG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgFG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgFG <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgFG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgFG ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgFG %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgFG, pwdt_OHU_LeakAgeDaysAvgFG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/FG/pwdt_LeakAgeDaysAvgFG.csv"))

# # C1 leak age days avg
# pwdt_LeakAgeDaysAvgFGC1 <- dfpW %>% 
#   drop_na(LeakAgeDaysAvgFGC1) %>% 
#   rstatix::dunn_test(LeakAgeDaysAvgFGC1 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_LeakAgeDaysAvgFGC1 <- dfpW_HH %>% 
#   drop_na(LeakAgeDaysAvgFGC1) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_LeakAgeDaysAvgFGC1 <- dfpW_OHU %>% 
#   drop_na(LeakAgeDaysAvgFGC1) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgFGC1 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_LeakAgeDaysAvgFGC1 %>% 
#     rbind(pwdt_HH_LeakAgeDaysAvgFGC1, pwdt_OHU_LeakAgeDaysAvgFGC1) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_LeakAgeDaysAvgFGC1.csv"))
# 
# # C2 leak age days avg
# pwdt_LeakAgeDaysAvgFGC2 <- dfpW %>% 
#   drop_na(LeakAgeDaysAvgFGC2) %>% 
#   rstatix::dunn_test(LeakAgeDaysAvgFGC2 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_LeakAgeDaysAvgFGC2 <- dfpW_HH %>% 
#   drop_na(LeakAgeDaysAvgFGC2) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_LeakAgeDaysAvgFGC2 <- dfpW_OHU %>% 
#   drop_na(LeakAgeDaysAvgFGC2) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgFGC2 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_LeakAgeDaysAvgFGC2 %>% 
#     rbind(pwdt_HH_LeakAgeDaysAvgFGC2, pwdt_OHU_LeakAgeDaysAvgFGC2) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_LeakAgeDaysAvgFGC2.csv"))
# 
# # C3 leak age days avg
# pwdt_LeakAgeDaysAvgFGC3 <- dfpW %>% 
#   drop_na(LeakAgeDaysAvgFGC3) %>% 
#   rstatix::dunn_test(LeakAgeDaysAvgFGC3 ~ group, p.adjust.method = "bonferroni") 
# 
# pwdt_HH_LeakAgeDaysAvgFGC3 <- dfpW_HH %>% 
#   drop_na(LeakAgeDaysAvgFGC3) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# pwdt_OHU_LeakAgeDaysAvgFGC3 <- dfpW_OHU %>% 
#   drop_na(LeakAgeDaysAvgFGC3) %>%
#   rstatix::dunn_test(LeakAgeDaysAvgFGC3 ~ group, p.adjust.method = "bonferroni")
# 
# (pwdt_LeakAgeDaysAvgFGC3 %>% 
#     rbind(pwdt_HH_LeakAgeDaysAvgFGC3, pwdt_OHU_LeakAgeDaysAvgFGC3) %>% 
#     filter(group2 == "totalpopE" | 
#              group2 == "total_occ_unitsE" |
#              (group2 == "eng_limitE" & group1 == "eng_hhE") | 
#              (group2 == "under5E" & group1 == "totalpopE")) %>% 
#     arrange(statistic) %>% 
#     write_csv("Tables/Tract/FG/pwdt_LeakAgeDaysAvgFGC3.csv"))




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
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,
         disabledOver18E,
         renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE, (starts_with("leaks_") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("AllLeaks") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))),
         (starts_with("REPleaks_") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("PctRepaired19") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))),
         (starts_with("leaks_hu") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("LU") | ends_with("LUC1") | ends_with("LUC2") | ends_with("LUC3"))))

# pivot df to long format so that so new df = #groups x #rows
dfp <- ppLeakDensityLU %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- ppLeakDensityLU %>% 
  select(eng_limitE, eng_hhE, c(leaks_sqkmLU:DaysToRepairAvgLUC3)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- ppLeakDensityLU %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(leaks_sqkmLU:DaysToRepairAvgLUC3)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE),
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmLU, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmLU, type = "common") %>% 
  arrange(desc(median))

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkmLU <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmLU ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmLU <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmLU ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmLU <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmLU ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmLU %>% 
    rbind(pwdt_HH_leaks_sqkmLU, pwdt_OHU_leaks_sqkmLU) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_sqkmLU.csv"))

# C1 unrepaired leaks per sqkm
pwdt_leaks_sqkmLUC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmLUC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmLUC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmLUC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmLUC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmLUC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmLUC1 %>% 
    rbind(pwdt_HH_leaks_sqkmLUC1, pwdt_OHU_leaks_sqkmLUC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_sqkmLUC1.csv"))

# C2 unrepaired leaks per sqkm
pwdt_leaks_sqkmLUC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmLUC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmLUC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmLUC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmLUC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmLUC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmLUC2 %>% 
    rbind(pwdt_HH_leaks_sqkmLUC2, pwdt_OHU_leaks_sqkmLUC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_sqkmLUC2.csv"))

# C3 unrepaired leaks per sqkm
pwdt_leaks_sqkmLUC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmLUC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmLUC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmLUC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmLUC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmLUC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmLUC3 %>% 
    rbind(pwdt_HH_leaks_sqkmLUC3, pwdt_OHU_leaks_sqkmLUC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_sqkmLUC3.csv"))

# repaired leaks per sqkm
pwdt_REPleaks_sqkmLU <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmLU ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmLU <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmLU ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmLU <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmLU ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmLU %>% 
    rbind(pwdt_HH_REPleaks_sqkmLU, pwdt_OHU_REPleaks_sqkmLU) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_sqkmLU.csv"))

# C1 repaired leaks per sqkm
pwdt_REPleaks_sqkmLUC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmLUC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmLUC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmLUC1 %>% 
    rbind(pwdt_HH_REPleaks_sqkmLUC1, pwdt_OHU_REPleaks_sqkmLUC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_sqkmLUC1.csv"))

# C2 repaired leaks per sqkm
pwdt_REPleaks_sqkmLUC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmLUC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmLUC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmLUC2 %>% 
    rbind(pwdt_HH_REPleaks_sqkmLUC2, pwdt_OHU_REPleaks_sqkmLUC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_sqkmLUC2.csv"))

# C3 repaired leaks per sqkm
pwdt_REPleaks_sqkmLUC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmLUC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmLUC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmLUC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmLUC3 %>% 
    rbind(pwdt_HH_REPleaks_sqkmLUC3, pwdt_OHU_REPleaks_sqkmLUC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_sqkmLUC3.csv"))

# unrepaired leaks per OHU
pwdt_leaks_huLU <- dfpW %>% 
  rstatix::dunn_test(leaks_huLU ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huLU <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huLU ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huLU <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huLU ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huLU %>% 
    rbind(pwdt_HH_leaks_huLU, pwdt_OHU_leaks_huLU) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_huLU.csv"))

# C1 unrepaired leaks per OHU
pwdt_leaks_huLUC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_huLUC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huLUC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huLUC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huLUC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huLUC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huLUC1 %>% 
    rbind(pwdt_HH_leaks_huLUC1, pwdt_OHU_leaks_huLUC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_huLUC1.csv"))

# C2 unrepaired leaks per OHU
pwdt_leaks_huLUC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_huLUC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huLUC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huLUC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huLUC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huLUC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huLUC2 %>% 
    rbind(pwdt_HH_leaks_huLUC2, pwdt_OHU_leaks_huLUC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_huLUC2.csv"))

# C3 unrepaired leaks per OHU
pwdt_leaks_huLUC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_huLUC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huLUC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huLUC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huLUC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huLUC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huLUC3 %>% 
    rbind(pwdt_HH_leaks_huLUC3, pwdt_OHU_leaks_huLUC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_leaks_huLUC3.csv"))

# repaired leaks per OHU
pwdt_REPleaks_huLU <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huLU ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huLU <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huLU ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huLU <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huLU ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huLU %>% 
    rbind(pwdt_HH_REPleaks_huLU, pwdt_OHU_REPleaks_huLU) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_huLU.csv"))

# C1 repaired leaks per OHU
pwdt_REPleaks_huLUC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huLUC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huLUC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huLUC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huLUC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huLUC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huLUC1 %>% 
    rbind(pwdt_HH_REPleaks_huLUC1, pwdt_OHU_REPleaks_huLUC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_huLUC1.csv"))

# C2 repaired leaks per OHU
pwdt_REPleaks_huLUC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huLUC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huLUC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huLUC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huLUC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huLUC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huLUC2 %>% 
    rbind(pwdt_HH_REPleaks_huLUC2, pwdt_OHU_REPleaks_huLUC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_huLUC2.csv"))

# C3 repaired leaks per OHU
pwdt_REPleaks_huLUC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huLUC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huLUC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huLUC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huLUC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huLUC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huLUC3 %>% 
    rbind(pwdt_HH_REPleaks_huLUC3, pwdt_OHU_REPleaks_huLUC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_REPleaks_huLUC3.csv"))

# days to repair avg
pwdt_DaysToRepairAvgLU <- dfpW %>% 
  drop_na(DaysToRepairAvgLU) %>% 
  rstatix::dunn_test(DaysToRepairAvgLU ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgLU <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgLU) %>%
  rstatix::dunn_test(DaysToRepairAvgLU ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgLU <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgLU) %>%
  rstatix::dunn_test(DaysToRepairAvgLU ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgLU %>% 
    rbind(pwdt_HH_DaysToRepairAvgLU, pwdt_OHU_DaysToRepairAvgLU) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_DaysToRepairAvgLU.csv"))

# C1 days to repair avg
pwdt_DaysToRepairAvgLUC1 <- dfpW %>% 
  drop_na(DaysToRepairAvgLUC1) %>% 
  rstatix::dunn_test(DaysToRepairAvgLUC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgLUC1 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgLUC1) %>%
  rstatix::dunn_test(DaysToRepairAvgLUC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgLUC1 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgLUC1) %>%
  rstatix::dunn_test(DaysToRepairAvgLUC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgLUC1 %>% 
    rbind(pwdt_HH_DaysToRepairAvgLUC1, pwdt_OHU_DaysToRepairAvgLUC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_DaysToRepairAvgLUC1.csv"))

# C2 days to repair avg
pwdt_DaysToRepairAvgLUC2 <- dfpW %>% 
  drop_na(DaysToRepairAvgLUC2) %>% 
  rstatix::dunn_test(DaysToRepairAvgLUC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgLUC2 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgLUC2) %>%
  rstatix::dunn_test(DaysToRepairAvgLUC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgLUC2 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgLUC2) %>%
  rstatix::dunn_test(DaysToRepairAvgLUC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgLUC2 %>% 
    rbind(pwdt_HH_DaysToRepairAvgLUC2, pwdt_OHU_DaysToRepairAvgLUC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_DaysToRepairAvgLUC2.csv"))

# C3 days to repair avg
pwdt_DaysToRepairAvgLUC3 <- dfpW %>% 
  drop_na(DaysToRepairAvgLUC3) %>% 
  rstatix::dunn_test(DaysToRepairAvgLUC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgLUC3 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgLUC3) %>%
  rstatix::dunn_test(DaysToRepairAvgLUC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgLUC3 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgLUC3) %>%
  rstatix::dunn_test(DaysToRepairAvgLUC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgLUC3 %>% 
    rbind(pwdt_HH_DaysToRepairAvgLUC3, pwdt_OHU_DaysToRepairAvgLUC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_DaysToRepairAvgLUC3.csv"))

# leak age days avg
pwdt_LeakAgeDaysAvgLU <- dfpW %>% 
  drop_na(LeakAgeDaysAvgLU) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgLU ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgLU <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgLU) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLU ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgLU <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgLU) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLU ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgLU %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgLU, pwdt_OHU_LeakAgeDaysAvgLU) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_LeakAgeDaysAvgLU.csv"))

# C1 leak age days avg
pwdt_LeakAgeDaysAvgLUC1 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgLUC1) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgLUC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgLUC1 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgLUC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLUC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgLUC1 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgLUC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLUC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgLUC1 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgLUC1, pwdt_OHU_LeakAgeDaysAvgLUC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_LeakAgeDaysAvgLUC1.csv"))

# C2 leak age days avg
pwdt_LeakAgeDaysAvgLUC2 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgLUC2) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgLUC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgLUC2 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgLUC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLUC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgLUC2 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgLUC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLUC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgLUC2 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgLUC2, pwdt_OHU_LeakAgeDaysAvgLUC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_LeakAgeDaysAvgLUC2.csv"))

# C3 leak age days avg
pwdt_LeakAgeDaysAvgLUC3 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgLUC3) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgLUC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgLUC3 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgLUC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLUC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgLUC3 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgLUC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgLUC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgLUC3 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgLUC3, pwdt_OHU_LeakAgeDaysAvgLUC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/LU/pwdt_LeakAgeDaysAvgLUC3.csv"))




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
  select(totalpopE, nhwhitepop_E, nhblackpop_E, hisppop_E, nhasianpop_E, 
         minority_E, eng_limitE, num2povE, lthsE, under5E, over64E,
         disabledOver18E,
         renter_occ_unitsE, house_burdened_E, total_occ_unitsE, eng_hhE, (starts_with("leaks_") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("AllLeaks") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("LeakAgeDaysAvg") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))),
         (starts_with("REPleaks_") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("DaystoRepairAvg") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("PctRepaired19") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))),
         (starts_with("leaks_hu") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))), 
         (starts_with("REPleaks_hu") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))),
         (starts_with("ALLleaks_hu") & (ends_with("BG") | ends_with("BGC1") | ends_with("BGC2") | ends_with("BGC3"))))

# pivot df to long format so that so new df = #groups x #rows
dfp <- ppLeakDensityBG %>% 
  select(-eng_limitE, -c(renter_occ_unitsE:eng_hhE)) %>% 
  pivot_longer(., cols = totalpopE:disabledOver18E, 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for households
dfpHH <- ppLeakDensityBG %>% 
  select(eng_limitE, eng_hhE, c(leaks_sqkmBG:DaysToRepairAvgBGC3)) %>% 
  pivot_longer(., cols = c(eng_limitE, eng_hhE), 
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# repeat for OHU
dfpOHU <- ppLeakDensityBG %>% 
  select(renter_occ_unitsE, house_burdened_E, total_occ_unitsE,
         c(leaks_sqkmBG:DaysToRepairAvgBGC3)) %>% 
  pivot_longer(., cols = c(renter_occ_unitsE, house_burdened_E, total_occ_unitsE),
               names_to = "group", values_to = "pop") %>% 
  mutate(group = as.factor(group))

# make sure it looks right
table(dfp$group)
table(dfpHH$group)
table(dfpOHU$group)

# try weighting by replicating observations by weights following suggestion at https://r.789695.n4.nabble.com/OT-a-weighted-rank-based-non-paired-test-statistic-td883773.html
dfpW <- dfp[rep(1:nrow(dfp), dfp$pop),]

dfpW_HH <- dfpHH[rep(1:nrow(dfpHH), dfpHH$pop),]

dfpW_OHU <- dfpOHU[rep(1:nrow(dfpOHU), dfpOHU$pop),]

# look at summary stats. should be equivalent to weighted means. 
dfpW %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmBG, type = "common") %>% 
  arrange(desc(median))

dfpW_OHU %>% 
  group_by(group) %>% 
  rstatix::get_summary_stats(leaks_sqkmBG, type = "common") %>% 
  arrange(desc(median))

# Pairwise comparisons using Dunn's test with `rstatix` package. Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings used by the Kruskal-Wallis test. It also does ties adjustments. See https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
# unreparied leaks per sqkm
pwdt_leaks_sqkmBG <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmBG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmBG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmBG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmBG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmBG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmBG %>% 
    rbind(pwdt_HH_leaks_sqkmBG, pwdt_OHU_leaks_sqkmBG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_sqkmBG.csv"))

# C1 unrepaired leaks per sqkm
pwdt_leaks_sqkmBGC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmBGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmBGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmBGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmBGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmBGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmBGC1 %>% 
    rbind(pwdt_HH_leaks_sqkmBGC1, pwdt_OHU_leaks_sqkmBGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_sqkmBGC1.csv"))

# C2 unrepaired leaks per sqkm
pwdt_leaks_sqkmBGC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmBGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmBGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmBGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmBGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmBGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmBGC2 %>% 
    rbind(pwdt_HH_leaks_sqkmBGC2, pwdt_OHU_leaks_sqkmBGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_sqkmBGC2.csv"))

# C3 unrepaired leaks per sqkm
pwdt_leaks_sqkmBGC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_sqkmBGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_sqkmBGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_sqkmBGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_sqkmBGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_sqkmBGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_sqkmBGC3 %>% 
    rbind(pwdt_HH_leaks_sqkmBGC3, pwdt_OHU_leaks_sqkmBGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_sqkmBGC3.csv"))

# repaired leaks per sqkm
pwdt_REPleaks_sqkmBG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmBG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmBG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmBG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmBG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmBG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmBG %>% 
    rbind(pwdt_HH_REPleaks_sqkmBG, pwdt_OHU_REPleaks_sqkmBG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_sqkmBG.csv"))

# C1 repaired leaks per sqkm
pwdt_REPleaks_sqkmBGC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmBGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmBGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmBGC1 %>% 
    rbind(pwdt_HH_REPleaks_sqkmBGC1, pwdt_OHU_REPleaks_sqkmBGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_sqkmBGC1.csv"))

# C2 repaired leaks per sqkm
pwdt_REPleaks_sqkmBGC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmBGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmBGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmBGC2 %>% 
    rbind(pwdt_HH_REPleaks_sqkmBGC2, pwdt_OHU_REPleaks_sqkmBGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_sqkmBGC2.csv"))

# C3 repaired leaks per sqkm
pwdt_REPleaks_sqkmBGC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_sqkmBGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_sqkmBGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_sqkmBGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_sqkmBGC3 %>% 
    rbind(pwdt_HH_REPleaks_sqkmBGC3, pwdt_OHU_REPleaks_sqkmBGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_sqkmBGC3.csv"))

# unrepaired leaks per OHU
pwdt_leaks_huBG <- dfpW %>% 
  rstatix::dunn_test(leaks_huBG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huBG <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huBG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huBG <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huBG ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huBG %>% 
    rbind(pwdt_HH_leaks_huBG, pwdt_OHU_leaks_huBG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>%  
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_huBG.csv"))

# C1 unrepaired leaks per OHU
pwdt_leaks_huBGC1 <- dfpW %>% 
  rstatix::dunn_test(leaks_huBGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huBGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huBGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huBGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huBGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huBGC1 %>% 
    rbind(pwdt_HH_leaks_huBGC1, pwdt_OHU_leaks_huBGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_huBGC1.csv"))

# C2 unrepaired leaks per OHU
pwdt_leaks_huBGC2 <- dfpW %>% 
  rstatix::dunn_test(leaks_huBGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huBGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huBGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huBGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huBGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huBGC2 %>% 
    rbind(pwdt_HH_leaks_huBGC2, pwdt_OHU_leaks_huBGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_huBGC2.csv"))

# C3 unrepaired leaks per OHU
pwdt_leaks_huBGC3 <- dfpW %>% 
  rstatix::dunn_test(leaks_huBGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_leaks_huBGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(leaks_huBGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_leaks_huBGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(leaks_huBGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_leaks_huBGC3 %>% 
    rbind(pwdt_HH_leaks_huBGC3, pwdt_OHU_leaks_huBGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_leaks_huBGC3.csv"))

# repaired leaks per OHU
pwdt_REPleaks_huBG <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huBG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huBG <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huBG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huBG <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huBG ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huBG %>% 
    rbind(pwdt_HH_REPleaks_huBG, pwdt_OHU_REPleaks_huBG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_huBG.csv"))

# C1 repaired leaks per OHU
pwdt_REPleaks_huBGC1 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huBGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huBGC1 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huBGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huBGC1 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huBGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huBGC1 %>% 
    rbind(pwdt_HH_REPleaks_huBGC1, pwdt_OHU_REPleaks_huBGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_huBGC1.csv"))

# C2 repaired leaks per OHU
pwdt_REPleaks_huBGC2 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huBGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huBGC2 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huBGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huBGC2 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huBGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huBGC2 %>% 
    rbind(pwdt_HH_REPleaks_huBGC2, pwdt_OHU_REPleaks_huBGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_huBGC2.csv"))

# C3 repaired leaks per OHU
pwdt_REPleaks_huBGC3 <- dfpW %>% 
  rstatix::dunn_test(REPleaks_huBGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_REPleaks_huBGC3 <- dfpW_HH %>% 
  rstatix::dunn_test(REPleaks_huBGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_REPleaks_huBGC3 <- dfpW_OHU %>% 
  rstatix::dunn_test(REPleaks_huBGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_REPleaks_huBGC3 %>% 
    rbind(pwdt_HH_REPleaks_huBGC3, pwdt_OHU_REPleaks_huBGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_REPleaks_huBGC3.csv"))

# days to repair avg
pwdt_DaysToRepairAvgBG <- dfpW %>% 
  drop_na(DaysToRepairAvgBG) %>% 
  rstatix::dunn_test(DaysToRepairAvgBG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgBG <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgBG) %>%
  rstatix::dunn_test(DaysToRepairAvgBG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgBG <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgBG) %>%
  rstatix::dunn_test(DaysToRepairAvgBG ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgBG %>% 
    rbind(pwdt_HH_DaysToRepairAvgBG, pwdt_OHU_DaysToRepairAvgBG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_DaysToRepairAvgBG.csv"))

# C1 days to repair avg
pwdt_DaysToRepairAvgBGC1 <- dfpW %>% 
  drop_na(DaysToRepairAvgBGC1) %>% 
  rstatix::dunn_test(DaysToRepairAvgBGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgBGC1 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgBGC1) %>%
  rstatix::dunn_test(DaysToRepairAvgBGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgBGC1 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgBGC1) %>%
  rstatix::dunn_test(DaysToRepairAvgBGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgBGC1 %>% 
    rbind(pwdt_HH_DaysToRepairAvgBGC1, pwdt_OHU_DaysToRepairAvgBGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_DaysToRepairAvgBGC1.csv"))

# C2 days to repair avg
pwdt_DaysToRepairAvgBGC2 <- dfpW %>% 
  drop_na(DaysToRepairAvgBGC2) %>% 
  rstatix::dunn_test(DaysToRepairAvgBGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgBGC2 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgBGC2) %>%
  rstatix::dunn_test(DaysToRepairAvgBGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgBGC2 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgBGC2) %>%
  rstatix::dunn_test(DaysToRepairAvgBGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgBGC2 %>% 
    rbind(pwdt_HH_DaysToRepairAvgBGC2, pwdt_OHU_DaysToRepairAvgBGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_DaysToRepairAvgBGC2.csv"))

# C3 days to repair avg
pwdt_DaysToRepairAvgBGC3 <- dfpW %>% 
  drop_na(DaysToRepairAvgBGC3) %>% 
  rstatix::dunn_test(DaysToRepairAvgBGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_DaysToRepairAvgBGC3 <- dfpW_HH %>% 
  drop_na(DaysToRepairAvgBGC3) %>%
  rstatix::dunn_test(DaysToRepairAvgBGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_DaysToRepairAvgBGC3 <- dfpW_OHU %>% 
  drop_na(DaysToRepairAvgBGC3) %>%
  rstatix::dunn_test(DaysToRepairAvgBGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_DaysToRepairAvgBGC3 %>% 
    rbind(pwdt_HH_DaysToRepairAvgBGC3, pwdt_OHU_DaysToRepairAvgBGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_DaysToRepairAvgBGC3.csv"))

# leak age days avg
pwdt_LeakAgeDaysAvgBG <- dfpW %>% 
  drop_na(LeakAgeDaysAvgBG) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgBG ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgBG <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgBG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBG ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgBG <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgBG) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBG ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgBG %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgBG, pwdt_OHU_LeakAgeDaysAvgBG) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_LeakAgeDaysAvgBG.csv"))

# C1 leak age days avg
pwdt_LeakAgeDaysAvgBGC1 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgBGC1) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgBGC1 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgBGC1 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgBGC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBGC1 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgBGC1 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgBGC1) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBGC1 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgBGC1 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgBGC1, pwdt_OHU_LeakAgeDaysAvgBGC1) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_LeakAgeDaysAvgBGC1.csv"))

# C2 leak age days avg
pwdt_LeakAgeDaysAvgBGC2 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgBGC2) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgBGC2 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgBGC2 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgBGC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBGC2 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgBGC2 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgBGC2) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBGC2 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgBGC2 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgBGC2, pwdt_OHU_LeakAgeDaysAvgBGC2) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_LeakAgeDaysAvgBGC2.csv"))

# C3 leak age days avg
pwdt_LeakAgeDaysAvgBGC3 <- dfpW %>% 
  drop_na(LeakAgeDaysAvgBGC3) %>% 
  rstatix::dunn_test(LeakAgeDaysAvgBGC3 ~ group, p.adjust.method = "bonferroni") 

pwdt_HH_LeakAgeDaysAvgBGC3 <- dfpW_HH %>% 
  drop_na(LeakAgeDaysAvgBGC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBGC3 ~ group, p.adjust.method = "bonferroni")

pwdt_OHU_LeakAgeDaysAvgBGC3 <- dfpW_OHU %>% 
  drop_na(LeakAgeDaysAvgBGC3) %>%
  rstatix::dunn_test(LeakAgeDaysAvgBGC3 ~ group, p.adjust.method = "bonferroni")

(pwdt_LeakAgeDaysAvgBGC3 %>% 
    rbind(pwdt_HH_LeakAgeDaysAvgBGC3, pwdt_OHU_LeakAgeDaysAvgBGC3) %>% 
    filter(group2 == "totalpopE" | 
             group2 == "total_occ_unitsE" |
             (group2 == "eng_limitE" & group1 == "eng_hhE") | 
             (group2 == "under5E" & group1 == "totalpopE")) %>% 
    arrange(statistic) %>% 
    write_csv("Tables/Tract/BG/pwdt_LeakAgeDaysAvgBGC3.csv"))



######## Look at unrepaired stats per tract
# define function to find statistical mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# list of stats to compute across
summary_stats <- list(Min = ~min(., na.rm = T),
                      Med = ~median(., na.rm = T),
                      Avg = ~mean(., na.rm = T),
                      Mod = ~Mode(.),
                      Max = ~max(., na.rm = T))

# count per BG
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("unrepaired2019total")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "unrepaired2019total" = "All",
                        "unrepaired2019totalC1" = "1",
                        "unrepaired2019totalC2" = "2",
                        "unrepaired2019totalC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats,
                   .names = "{.fn}"))

# leak density per tract
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("leaks_sqkm")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "leaks_sqkm" = "All",
                        "leaks_sqkmC1" = "1",
                        "leaks_sqkmC2" = "2",
                        "leaks_sqkmC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))

# leaks per OHU
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("leaks_hu")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "leaks_hu" = "All",
                        "leaks_huC1" = "1",
                        "leaks_huC2" = "2",
                        "leaks_huC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))


ma_tracts %>% 
  as.data.frame() %>% 
  pivot_longer(cols = c(unrepaired2019totalC1, unrepaired2019totalC2, 
                        unrepaired2019totalC3, unrepaired2019total),
               names_to = "Class", values_to = "count") %>% 
  mutate(Class = recode(Class, "unrepaired2019totalC1" = "1",
                        "unrepaired2019totalC2" = "2",
                        "unrepaired2019totalC3" = "3",
                        "unrepaired2019total" = "All")) %>% 
  # filter(count < 10) %>% 
  ggplot(aes(x = Class, y = count)) + geom_boxplot() +
  theme_minimal() +
  labs(title = "Unrepaired Leaks Per Tract in 2019", x = "leak class")

ggsave("Data/boxplotTractunrepaired.png")

# what pct of tracts with unrepaired leaks? Almost all (92%) of tracts in gas service territories had at least 1 unrepaired gas leak in 2019
ma_tracts %>% 
  filter(unrepaired2019total > 0) %>% 
  nrow()/nrow(ma_tracts)

# Look at repaired stats per tract
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("repaired2019total")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "repaired2019total" = "Total",
                        "repaired2019totalC1" = "1",
                        "repaired2019totalC2" = "2",
                        "repaired2019totalC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   list(min = ~ min(., na.rm = TRUE),
                        med = ~ median(., na.rm = TRUE),
                        avg = ~ round(mean(., na.rm = TRUE),2),
                        mod = ~ Mode(.),
                        max = ~ max(., na.rm = TRUE)),
                   .names = "{.fn}"))

# leak density per tract
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("REPleaks_sqkm")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "REPleaks_sqkm" = "Total",
                        "REPleaks_sqkmC1" = "1",
                        "REPleaks_sqkmC2" = "2",
                        "REPleaks_sqkmC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))

# leaks per OHU
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("REPleaks_hu")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "REPleaks_hu" = "Total",
                        "REPleaks_huC1" = "1",
                        "REPleaks_huC2" = "2",
                        "REPleaks_huC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))

ma_tracts %>% 
  as.data.frame() %>% 
  pivot_longer(cols = starts_with("repaired2019total"),
               names_to = "Class", values_to = "count") %>% 
  mutate(Class = recode(Class, "repaired2019totalC1" = "1",
                        "repaired2019totalC2" = "2",
                        "repaired2019totalC3" = "3",
                        "repaired2019total" = "All")) %>% 
  # filter(count < 10) %>% 
  ggplot(aes(x = Class, y = count)) + geom_boxplot() +
  theme_minimal() +
  labs(title = "Repaired Leaks Per Tract in 2019", x = "leak class")

ggsave("Data/boxplotTractrepaired.png")

# what pct of tracts with repaired leaks? Almost all (93%) of tracts in gas service territories had at least 1 repaired gas leak in 2019
ma_tracts %>% 
  filter(repaired2019total > 0) %>% 
  nrow()/nrow(ma_tracts)

# look at all leaks per tract
ma_tracts %>% 
  as.data.frame() %>% 
  select(AllLeaks2019, AllLeaks2019C1, AllLeaks2019C2, AllLeaks2019C3) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "AllLeaks2019" = "Total",
                        "AllLeaks2019C1" = "1",
                        "AllLeaks2019C2" = "2",
                        "AllLeaks2019C3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))

# leak density per tract
ma_tracts %>% 
  as.data.frame() %>% 
  select("AllLeaks2019_sqkm", "AllLeaks2019C1_sqkm", 
         "AllLeaks2019C2_sqkm", "AllLeaks2019C3_sqkm") %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "AllLeaks2019_sqkm" = "Total",
                        "AllLeaks2019C1_sqkm" = "1",
                        "AllLeaks2019C2_sqkm" = "2",
                        "AllLeaks2019C3_sqkm" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))

# leaks per OHU
ma_tracts %>% 
  as.data.frame() %>% 
  select(starts_with("ALLleaks_hu")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Leaks") %>% 
  mutate(Class = recode(Class, "ALLleaks_hu" = "Total",
                        "ALLleaks_huC1" = "1",
                        "ALLleaks_huC2" = "2",
                        "ALLleaks_huC3" = "3")) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = everything(), 
                   summary_stats, 
                   .names = "{.fn}"))

ma_tracts %>% 
  as.data.frame() %>% 
  pivot_longer(cols = c(AllLeaks2019, AllLeaks2019C1, 
                        AllLeaks2019C2, AllLeaks2019C3),
               names_to = "Class", values_to = "count") %>% 
  mutate(Class = recode(Class, "AllLeaks2019" = "Total",
                        "AllLeaks2019C1" = "1",
                        "AllLeaks2019C2" = "2",
                        "AllLeaks2019C3" = "3")) %>% 
  # filter(count < 10) %>% 
  ggplot(aes(x = Class, y = count)) + geom_boxplot() +
  theme_minimal() +
  labs(title = "Total Leaks Per Tract in 2019", x = "leak class")

ggsave("Data/boxplotTractallleaks.png")

# what pct of tracts with any reported leaks? Almost all (96.5%) of tracts in gas service territories had at least 1 reported gas leak in 2019
ma_tracts %>% 
  filter(AllLeaks2019 > 0) %>% 
  nrow()/nrow(ma_tracts)

# create a qunatile plot to see distribution of counts for all leaks
ma_tracts %>% 
  as.data.frame() %>% 
  ggplot(aes(sample = AllLeaks2019)) + 
  stat_qq(distribution = qunif) + 
  theme_minimal() +
  labs(title = "Percentile plot of total leaks per Tract",
       x = "percentile", y = "leak count")

colors_quant <- c("All" = "#8da0cb", "Unrepaired" = "#fc8d62", 
                  "Repaired" = "#66c2a5")
ma_tracts %>% 
  as.data.frame() %>% 
  mutate(All = percent_rank(AllLeaks2019),
         Unrepaired = percent_rank(unrepaired2019total),
         Repaired = percent_rank(repaired2019total)) %>% 
  ggplot() + 
  geom_line(aes(x = All, y = AllLeaks2019, 
                color = "All"), size = 1.25) +
  geom_line(aes(x = Unrepaired, y = unrepaired2019total, 
                color = "Unrepaired"), size = 1.25) +
  geom_line(aes(x = Repaired, y = repaired2019total, 
                color = "Repaired"), size = 1.25) +
  theme_minimal() +
  labs(title = "Percentile plot of leaks per Tract",
       x = "percentile", y = "leak count",
       color = "") +
  scale_color_manual(values = colors_quant)

ma_tracts %>% 
  as.data.frame() %>% 
  mutate(All = percent_rank(AllLeaks2019_sqkm),
         Unrepaired = percent_rank(leaks_sqkm),
         Repaired = percent_rank(REPleaks_sqkm)) %>% 
  ggplot() + 
  geom_line(aes(x = All, y = AllLeaks2019_sqkm, 
                color = "All"), size = 1.25) +
  geom_line(aes(x = Unrepaired, y = leaks_sqkm, 
                color = "Unrepaired"), size = 1.25) +
  geom_line(aes(x = Repaired, y = REPleaks_sqkm, 
                color = "Repaired"), size = 1.25) +
  theme_minimal() +
  labs(title = "Percentile plot of leak density per Tract",
       x = "percentile", y = "leak density",
       color = "") +
  scale_color_manual(values = colors_quant)





# maps of leaks
# create a hexagonal grid and create index column
gridCnt <- st_make_grid(x = ma_tracts, cellsize = 1000, square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(index = row_number())

# spatially join to grid ids to leaks, sum aggregate numbers of leaks per grid index, and then join sums back to hexagons for mapping and analysis
gridCnt <- unrepaired2019final %>% 
  st_join(., gridCnt) %>% 
  st_drop_geometry() %>% 
  group_by(index) %>% 
  summarize(unrepaired = n()) %>% 
  left_join(gridCnt, ., by = "index") %>% 
  replace_na(list(unrepaired = 0))

gridCnt <- repaired2019final %>% 
  st_join(., gridCnt) %>% 
  st_drop_geometry() %>% 
  group_by(index) %>% 
  summarize(repaired = n()) %>% 
  left_join(gridCnt, ., by = "index") %>% 
  replace_na(list(repaired = 0))

# create column with total leak points and clip to MA
gridCnt1 <- gridCnt %>% 
  mutate(total = unrepaired + repaired) %>% 
  crop_shape(., ma_tracts, polygon = TRUE) %>% 
  st_make_valid()

# map out counts of leaks per hexagons > 0 
m_gridUnrepaired <- gridCnt1 %>% 
  filter(unrepaired > 0) %>%
  tm_shape(.) + tm_fill(col = "unrepaired", palette = "YlOrRd",
                        style = "fisher",
                        # breaks = c(1,5,10,20,40,80), 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)
tmap_save(m_gridUnrepaired, filename = "Images/m_gridUnrepaired.png", dpi = 600)

m_gridRepaired <- gridCnt1 %>% 
  filter(repaired > 0) %>%
  tm_shape(.) + tm_fill(col = "repaired", palette = "YlOrRd",
                        style = "fisher",
                        # breaks = c(1,5,10,20,40,80), 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)
tmap_save(m_gridRepaired, filename = "Images/m_gridRepaired.png", dpi = 600)

m_gridTotal <- gridCnt1 %>% 
  filter(total > 0) %>%
  tm_shape(.) + 
  tm_fill(col = "total", palette = "YlOrRd",
          legend.format = list(digits = 0), 
          style = "fisher",
          # breaks = c(1,5,10,20,40,80),
          legend.hist = TRUE)
tmap_save(m_gridTotal, filename = "Images/m_gridTotal.png", dpi = 600)



# map out leaks  by tract
m1 <- ma_tracts %>% 
  filter(AllLeaks2019 > 0) %>% 
  tm_shape(.) + tm_fill(col = "AllLeaks2019", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

m2 <- ma_tracts %>% 
  filter(unrepaired2019total > 0) %>% 
  tm_shape(.) + tm_fill(col = "unrepaired2019total", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

# mArrange <- tmap_arrange(m1, m2)
# tmap_save(mArrange, "Images/arranged.png")

# map out leak densities by tract
ma_tracts %>% 
  filter(AllLeaks2019_sqkm > 0) %>% 
  tm_shape(.) + tm_fill(col = "AllLeaks2019_sqkm", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

ma_tracts %>% 
  filter(leaks_sqkm > 0) %>% 
  tm_shape(.) + tm_fill(col = "leaks_sqkm", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

# leask per OHU
ma_tracts %>% 
  filter(ALLleaks_hu > 0) %>% 
  tm_shape(.) + tm_fill(col = "ALLleaks_hu", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

ma_tracts %>% 
  filter(leaks_hu > 0) %>% 
  tm_shape(.) + tm_fill(col = "leaks_hu", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)


### explore geostatistical visualizations
# first, identify the bandwidth at which spatial autocorrelation plateaus using an empirical semivariogram
library(gstat)
# convert it to spdf for use in gstat
ma_tracts_sp <- as_Spatial(ma_tracts)

# generate empirical semivariogram
v <- variogram(AllLeaks2019_sqkm ~ 1, ma_tracts_sp, cutoff = 20000, width = 1000)
v

# plot it
plot(v, plot.numbers = F)
# first inflection in curve happens around 3000m
v %>% 
  ggplot(aes(x = dist, y = gamma)) + geom_point()


### Look for statistically significant clusters of leaks 

# Calculate distance based neighbors
nb <- dnearneigh(coordinates(ma_tracts_sp),0,15000)

# find max distance to nearest neighbor
# create vector of nearest neighbor distances
nn <- ma_tracts %>% 
  filter(!st_is_empty(.)) %>% 
  select(leaks_sqkm) %>% 
  st_centroid(., of_largest_polygon = TRUE) %>% 
  spatstat::as.ppp(.) %>% 
  spatstat::nndist(.)
# identify maximum distance
max(nn)

nb <- ma_tracts %>% 
  filter(!st_is_empty(.)) %>% 
  select(leaks_sqkm) %>% 
  as_Spatial(.) %>% 
  coordinates(.) %>% 
  dnearneigh(., 0, 11000) 

# compute neighbor weights
listw <- nb2listw(nb, style = "B")

# compute Getis-Ord Gi statistic
local_g <- localG(ma_tracts$leaks_sqkm, listw)
local_g <- cbind(ma_tracts, as.matrix(local_g))
names(local_g)[397] <- "gstat"

# map out hotspots
tm_shape(local_g, unit = "mi",) + 
  tm_fill("gstat",
          palette = "-RdBu", 
          style = "pretty",
          title = expression(paste("Getis-Ord ", G[i]^"*")),
          showNA = FALSE,
          midpoint = NA,
          labels = c("Significant Clusters","of Low Values", "","","","", "Significant Clusters","of High Values"),
          legend.show = FALSE,
          alpha = 0.5)

# map out leak density and and leaks per OHU to see if these patterns are different
# Calculate Queen's case neighbors
neighborsQC <- poly2nb(ma_tracts, queen = TRUE)

# Compute neighbor weights
# spdep::set.ZeroPolicyOption(TRUE)
listw <- nb2listw(neighborsQC, style = "W", zero.policy = TRUE)

# compute Getis-Ord Gi statistic
local_g <- localG(ma_tracts$leaks_sqkm, listw)
local_g <- cbind(ma_tracts, as.matrix(local_g))
names(local_g)[403] <- "gstat"

# map out hotspots
tm_shape(local_g, unit = "mi",) + 
  tm_fill("gstat",
          palette = "-RdBu", 
          style = "pretty",
          title = expression(paste("Getis-Ord ", G[i]^"*")),
          showNA = FALSE,
          midpoint = NA,
          labels = c("Significant Clusters","of Low Values", "","","","", "Significant Clusters","of High Values"),
          legend.show = FALSE,
          alpha = 0.5)








































# # create a bar plot of total unrepaired leak density with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wLeaksPerSqKm), y = wLeaksPerSqKm)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab(expression(paste("Population-weighted mean leak density (leaks/", 
#                         km^2, ")", " by Census Tract",sep = ""))) +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wLeaksPerSqKmUC, ymax = wLeaksPerSqKmLC)) +
#   ggtitle("Priority Populations and Unrepaired Gas Leaks in 2019\nacross Massachusetts")
# 
# ggsave("Images/LeaksPP_tract_moe.png")
# 
# # create a bar plot of total unrepaired leaks per occupied housing unit with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wLeaksPerHU), y = wLeaksPerHU)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab("Population-weighted mean leaks per occupied housing unit\nby Census Tract") +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wLeaksPerHUUC, ymax = wLeaksPerHULC)) +
#   ggtitle("Priority Populations and Unrepaired Gas Leaks\nPer Housing Unit in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPP_HU_tract_moe.png")
# 
# # create a bar plot of total leaks (unrepaired + repaired) with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wLeaksPerSqKmALL), y = wLeaksPerSqKmALL)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab(expression(paste("Population-weighted mean leak density (leaks/", 
#                         km^2, ")", " by Census Tract",sep = ""))) +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wLeaksPerSqKmALLUC, ymax = wLeaksPerSqKmALLLC)) +
#   ggtitle("Priority Populations and All Repaired and Unrepaired Gas Leaks\nin 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPall_tract_moe.png")
# 
# # create a bar plot of total leaks (unrepaired + repaired) per occupied housing unit with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wALLLeaksPerHU), y = wALLLeaksPerHU)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab("Population-weighted mean leaks per occupied housing unit\nby Census Tract") +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wALLLeaksPerHUUC, ymax = wALLLeaksPerHULC)) +
#   ggtitle("Priority Populations and All Repaired and Unrepaired Gas\nLeaks Per Housing Unit in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPall_HU_tract_moe.png")
# 
# # create a bar plot of repaired leaks with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wLeaksPerSqKmREP), y = wLeaksPerSqKmREP)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab(expression(paste("Population-weighted mean leak density (leaks/", 
#                         km^2, ")", " by Census Tract",sep = ""))) +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wLeaksPerSqKmREPUC, ymax = wLeaksPerSqKmREPLC)) +
#   ggtitle("Priority Populations and Repaired Gas Leaks\nin 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPrepaired_tract_moe.png")
# 
# # create a bar plot of total repaired leaks per occupied housing unit with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>%  
#   ggplot(aes(x = reorder(Group,wREPLeaksPerHU), y = wREPLeaksPerHU)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab("Population-weighted mean leaks per occupied housing unit\nby Census Tract") +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wREPLeaksPerHUUC, ymax = wREPLeaksPerHULC)) +
#   ggtitle("Priority Populations and Repaired Gas Leaks\nPer Housing Unit in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPrepaired_HU_tract_moe.png")
# 
# # create a bar plot of avg unrepaired leak age with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wLeakAgeDaysAvg), y = wLeakAgeDaysAvg)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   scale_y_continuous(labels = function(x) format(x, big.mark = ",",
#                                                  scientific = FALSE)) +
#   ylab("Population-weighted mean age (days) of unrepaired leaks\nby Census Tract") +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wLeakAgeDaysAvgUC, ymax = wLeakAgeDaysAvgLC)) +
#   ggtitle("Priority Populations and Average Age of Unrepaired\nLeaks in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPage_tract_moe.png")
# 
# 
# # create a bar plot of avg repair times with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wDaysToRepairAvg), y = wDaysToRepairAvg)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab("Population-weighted mean leak repair time (days) by Census Tract") +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wDaysToRepairAvgUC, ymax = wDaysToRepairAvgLC)) +
#   ggtitle("Priority Populations and Average Leak Repair Times\nin 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPtimeRep_tract_moe.png")
# 
# # create a bar plot of percentage of leaks repaired with error bars
# ppLeakDensityJoined %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   ggplot(aes(x = reorder(Group,wPctRepaired19), y = wPctRepaired19)) + 
#   geom_col() + coord_flip() + xlab("") + 
#   ylab("Population-weighted Percentage of Leaks Repaired in 2019\nby Census Tract") +
#   theme_minimal() +
#   geom_errorbar(aes(ymin = wPctRepaired19UC, ymax = wPctRepaired19LC)) +
#   ggtitle("Priority Populations and Percentage of Leaks Repaired\nin 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPpctFixed_tract_moe.png")
# 
# 
# # create a facet wrap bar graph by leak grade and ordered by unrepaired leaks
# ppLeakDensity %>% 
#   pivot_longer(wLeaksPerSqKm:wLeaksPerSqKmC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wLeaksPerSqKm" = "All Leaks",
#                             "wLeaksPerSqKmC1" = "Class 1 Leaks",
#                             "wLeaksPerSqKmC2" = "Class 2 Leaks",
#                             "wLeaksPerSqKmC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = expression(paste("Population-weighted mean leak density (leaks/", 
#                             km^2, ")", " by Census Tract",sep = "")),
#        title = "Priority Populations and Unrepaired Gas Leaks in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClass_tract.png")
# 
# # create a facet wrap bar graph by leak grade and ordered by unrepaired leaks per occupied housing unit
# ppLeakDensity %>% 
#   pivot_longer(wLeaksPerHU:wLeaksPerHUC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wLeaksPerHU" = "All Leaks",
#                             "wLeaksPerHUC1" = "Class 1 Leaks",
#                             "wLeaksPerHUC2" = "Class 2 Leaks",
#                             "wLeaksPerHUC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean leaks per occupied housing unit by Census Tract",
#        title = "Priority Populations and Unrepaired Gas Leaks Per Housing Unit in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClass_HU_tract.png")
# 
# 
# # create a facet wrap bar graph by leak grade and ordered by all repaired and unrepaired leaks
# ppLeakDensity %>% 
#   pivot_longer(wLeaksPerSqKmALL:wLeaksPerSqKmALLC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALL" = "All Leaks",
#                             "wLeaksPerSqKmALLC1" = "Class 1 Leaks",
#                             "wLeaksPerSqKmALLC2" = "Class 2 Leaks",
#                             "wLeaksPerSqKmALLC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = expression(paste("Population-weighted mean leak density (leaks/", 
#                             km^2, ")", " by Census Tract",sep = "")),
#        title = "Priority Populations and All Repaired and Unrepaired Gas Leaks in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassAll_tract.png")
# 
# # create a facet wrap bar graph by leak grade and ordered by all repaired and unrepaired leaks per occupied housing unit
# ppLeakDensity %>% 
#   pivot_longer(wALLLeaksPerHU:wALLLeaksPerHUC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wALLLeaksPerHU" = "All Leaks",
#                             "wALLLeaksPerHUC1" = "Class 1 Leaks",
#                             "wALLLeaksPerHUC2" = "Class 2 Leaks",
#                             "wALLLeaksPerHUC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean leaks per occupied housing unit by Census Tract",
#        title = "Priority Populations and All Repaired and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassAll_HU_tract.png")
# 
# 
# # create a facet wrap bar graph by leak grade and ordered by all repaired leaks
# ppLeakDensity %>% 
#   pivot_longer(wLeaksPerSqKmREP:wLeaksPerSqKmREPC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wLeaksPerSqKmREP" = "All Leaks",
#                             "wLeaksPerSqKmREPC1" = "Class 1 Leaks",
#                             "wLeaksPerSqKmREPC2" = "Class 2 Leaks",
#                             "wLeaksPerSqKmREPC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = expression(paste("Population-weighted mean leak density (leaks/", 
#                             km^2, ")", " by Census Tract",sep = "")),
#        title = "Priority Populations and All Repaired Gas Leaks in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassREP_tract.png")
# 
# # create a facet wrap bar graph by leak grade and ordered by all repaired leaks per occupied housing unit
# ppLeakDensity %>% 
#   pivot_longer(wREPLeaksPerHU:wREPLeaksPerHUC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wREPLeaksPerHU" = "All Leaks",
#                             "wREPLeaksPerHUC1" = "Class 1 Leaks",
#                             "wREPLeaksPerHUC2" = "Class 2 Leaks",
#                             "wREPLeaksPerHUC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean leaks per occupied housing unit by Census Tract",
#        title = "Priority Populations and All Repaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassREP_HU_tract.png")
# 
# 
# # create a facet wrap bar graph by leak grade and ordered by avg unrepaired leak age
# ppLeakDensity %>% 
#   pivot_longer(wLeakAgeDaysAvg:wLeakAgeDaysAvgC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wLeakAgeDaysAvg" = "All Leaks",
#                             "wLeakAgeDaysAvgC1" = "Class 1 Leaks",
#                             "wLeakAgeDaysAvgC2" = "Class 2 Leaks",
#                             "wLeakAgeDaysAvgC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   scale_y_continuous(labels = function(x) format(x, big.mark = ",",
#                                                  scientific = FALSE)) +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
#        title = "Priority Populations and Average Age of Unrepaired Leaks in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassAge_tract.png")
# 
# 
# # create a facet wrap bar graph by leak grade and ordered by avg leak repair time
# ppLeakDensity %>% 
#   pivot_longer(wDaysToRepairAvg:wDaysToRepairAvgC3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wDaysToRepairAvg" = "All Leaks",
#                             "wDaysToRepairAvgC1" = "Class 1 Leaks",
#                             "wDaysToRepairAvgC2" = "Class 2 Leaks",
#                             "wDaysToRepairAvgC3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean leak repair time (days) by Census Tract",
#        title = "Priority Populations and Average Leak Repair Time in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassTime_tract.png")
# 
# 
# # create a facet wrap bar graph by leak grade and ordered by avg pct of leaks fixed
# ppLeakDensity %>% 
#   pivot_longer(wPctRepaired19:wPctRepaired19C3, 
#                names_to = "leakClass", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   mutate(leakClass = recode(leakClass, "wPctRepaired19" = "All Leaks",
#                             "wPctRepaired19C1" = "Class 1 Leaks",
#                             "wPctRepaired19C2" = "Class 2 Leaks",
#                             "wPctRepaired19C3" = "Class 3 Leaks"),
#          Group = reorder_within(Group, leakDensity, leakClass)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ leakClass, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean percentage of leaks repaired in 2019 by Census Tract",
#        title = "Priority Populations and Percent Repaired Gas Leaks in 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyClassPctFix_tract.png")
# 
# 
# 
# 
# # Compare leak density distribution by utility
# # National Grid; includes both NG-Boston and NG-Colonial Gas
# ppLeakDensityNG <- ma_tracts %>% 
#   as.data.frame() %>% 
#   filter(str_detect(GAS, "^National") | 
#            GAS == "Colonial Gas") %>% # limit to BGs in NG/Colonial svc area
#   mutate(leaks_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
#                            `National Grid - Colonial Gas_19unrepaired`)/area_sqkm,
#          REPleaks_sqkmNG = (`National Grid - Boston Gas_19repaired` +
#                               `National Grid - Colonial Gas_19repaired`)/area_sqkm,
#          AllLeaks2019_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
#                                   `National Grid - Colonial Gas_19unrepaired` + 
#                                   `National Grid - Boston Gas_19repaired` +
#                                   `National Grid - Colonial Gas_19repaired`)/area_sqkm,
#          leaks_huNG = if_else(total_occ_unitsE == 0, 0, 
#                               (`National Grid - Boston Gas_19unrepaired` +
#                                  `National Grid - Colonial Gas_19unrepaired`)/total_occ_unitsE),
#          REPleaks_huNG = if_else(total_occ_unitsE == 0, 0,
#                                  (`National Grid - Boston Gas_19repaired` +
#                                     `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
#          AllLeaks2019_huNG = if_else(total_occ_unitsE == 0, 0,
#                                      (`National Grid - Boston Gas_19unrepaired` + `National Grid - Colonial Gas_19unrepaired` + 
#                                         `National Grid - Boston Gas_19repaired` +
#                                         `National Grid - Colonial Gas_19repaired`)/total_occ_unitsE),
#          PctRepaired19NG = (`National Grid - Boston Gas_19repaired` +
#                               `National Grid - Colonial Gas_19repaired`)/ (`National Grid - Boston Gas_19unrepaired` +
#                                                                              `National Grid - Colonial Gas_19unrepaired` + 
#                                                                              `National Grid - Boston Gas_19repaired` +
#                                                                              `National Grid - Colonial Gas_19repaired`)*100) %>% 
#   rowwise() %>% 
#   mutate(DaysToRepairAvgNG = sum(`National Grid - Boston Gas_19repairedDaysAvg`,                                 `National Grid - Colonial Gas_19repairedDaysAvg`, na.rm = TRUE)/2,
#          LeakAgeDaysAvgNG = sum(`National Grid - Boston Gas_19unrepairedDaysAvg`,                                 `National Grid - Colonial Gas_19unrepairedDaysAvg`, na.rm = TRUE)/2) %>% 
#   select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
#          eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
#          (starts_with("leaks_") & ends_with("NG")), 
#          (starts_with("AllLeaks") & ends_with("NG")), 
#          (starts_with("LeakAgeDaysAvg") & ends_with("NG")),
#          (starts_with("REPleaks_") & ends_with("NG")), 
#          (starts_with("DaystoRepairAvg") & ends_with("NG")), 
#          (starts_with("PctRepaired19") & ends_with("NG")),
#          (starts_with("leaks_hu") & ends_with("NG")), 
#          (starts_with("REPleaks_hu") & ends_with("NG")),
#          (starts_with("ALLleaks_hu") & ends_with("NG"))) %>% 
#   pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
#                values_to = "Pop", values_drop_na = TRUE) %>% 
#   group_by(Group) %>% 
#   summarize(wLeaksPerSqKmNG = weighted.mean(x = leaks_sqkmNG, w = Pop, 
#                                             na.rm = TRUE),
#             wLeaksPerSqKmREPNG = weighted.mean(x = REPleaks_sqkmNG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeaksPerSqKmALLNG = weighted.mean(x = AllLeaks2019_sqkmNG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeakAgeDaysAvgNG = weighted.mean(x = LeakAgeDaysAvgNG,
#                                               w = Pop, na.rm = TRUE),
#             wLeaksPerHUNG = weighted.mean(x = leaks_huNG, w = Pop, 
#                                           na.rm = T),
#             wREPLeaksPerHUNG = weighted.mean(x = REPleaks_huNG, 
#                                              w = Pop, na.rm = T),
#             wALLLeaksPerHUNG = weighted.mean(x = AllLeaks2019_huNG, 
#                                              w = Pop, na.rm = T),
#             wPctRepaired19NG = weighted.mean(x = PctRepaired19NG, 
#                                              w = Pop, na.rm = T),
#             wDaysToRepairAvgNG = weighted.mean(x = DaysToRepairAvgNG, 
#                                                w = Pop, na.rm = T)) %>% 
#   mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
#                         "minority_E" = "People of Color",
#                         "nh2morepop_E" = "Two or more races",
#                         "nhamerindpop_E" = "Native American",
#                         "nhasianpop_E" = "Asian",
#                         "nhblackpop_E" = "Black",
#                         "nhnativpop_E" = "Native Pacific Islander",
#                         "nhotherpop_E" = "Other race",
#                         "nhwhitepop_E" = "White",
#                         "totalpop_E" = "Total Population",
#                         "eng_hhE" = "Total Households",
#                         "under5E" = "Under 5",
#                         "over64E" = "Over 64",
#                         "eng_limitE" = "Limited English HH",
#                         "num2povE" = "Low Income",
#                         "lthsE" = "No HS Diploma",
#                         "total_occ_unitsE" = "Total Occupied HU",
#                         "renter_occ_unitsE" = "Renter Occupied HU",
#                         "disabledOver18E" = "Disabled Adults",
#                         "house_burdened_E" = "Housing Burdened"))
# 
# # Eversource Energy
# ppLeakDensityEV <- ma_tracts %>% 
#   as.data.frame() %>% 
#   filter(str_detect(GAS, "^Eversource") | 
#            str_detect(GAS, "Energy$")) %>% # limit to BGs in Eversource svc area
#   mutate(leaks_sqkmEV = `Eversource Energy_19unrepaired`/area_sqkm,
#          REPleaks_sqkmEV = `Eversource Energy_19repaired`/area_sqkm,
#          AllLeaks2019_sqkmEV = (`Eversource Energy_19unrepaired` + 
#                                   `Eversource Energy_19repaired`)/area_sqkm,
#          leaks_huEV = if_else(total_occ_unitsE == 0, 0, 
#                               `Eversource Energy_19unrepaired`/
#                                 total_occ_unitsE),
#          REPleaks_huEV = if_else(total_occ_unitsE == 0, 0,
#                                  `Eversource Energy_19repaired`/
#                                    total_occ_unitsE),
#          AllLeaks2019_huEV = if_else(total_occ_unitsE == 0, 0,
#                                      (`Eversource Energy_19unrepaired` + 
#                                         `Eversource Energy_19repaired`)
#                                      /total_occ_unitsE),
#          PctRepaired19EV = `Eversource Energy_19repaired`/ 
#            (`Eversource Energy_19unrepaired` + 
#               `Eversource Energy_19repaired`)*100,
#          DaysToRepairAvgEV = `Eversource Energy_19repairedDaysAvg`,
#          LeakAgeDaysAvgEV = `Eversource Energy_19unrepairedDaysAvg`) %>% 
#   select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
#          eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
#          (starts_with("leaks_") & ends_with("EV")), 
#          (starts_with("AllLeaks") & ends_with("EV")), 
#          (starts_with("LeakAgeDaysAvg") & ends_with("EV")),
#          (starts_with("REPleaks_") & ends_with("EV")), 
#          (starts_with("DaystoRepairAvg") & ends_with("EV")), 
#          (starts_with("PctRepaired19") & ends_with("EV")),
#          (starts_with("leaks_hu") & ends_with("EV")), 
#          (starts_with("REPleaks_hu") & ends_with("EV")),
#          (starts_with("ALLleaks_hu") & ends_with("EV"))) %>% 
#   pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
#                values_to = "Pop", values_drop_na = TRUE) %>% 
#   group_by(Group) %>% 
#   summarize(wLeaksPerSqKmEV = weighted.mean(x = leaks_sqkmEV, w = Pop, 
#                                             na.rm = TRUE),
#             wLeaksPerSqKmREPEV = weighted.mean(x = REPleaks_sqkmEV, 
#                                                w = Pop, na.rm = TRUE),
#             wLeaksPerSqKmALLEV = weighted.mean(x = AllLeaks2019_sqkmEV, 
#                                                w = Pop, na.rm = TRUE),
#             wLeakAgeDaysAvgEV = weighted.mean(x = LeakAgeDaysAvgEV,
#                                               w = Pop, na.rm = TRUE),
#             wLeaksPerHUEV = weighted.mean(x = leaks_huEV, w = Pop, 
#                                           na.rm = T),
#             wREPLeaksPerHUEV = weighted.mean(x = REPleaks_huEV, 
#                                              w = Pop, na.rm = T),
#             wALLLeaksPerHUEV = weighted.mean(x = AllLeaks2019_huEV, 
#                                              w = Pop, na.rm = T),
#             wPctRepaired19EV = weighted.mean(x = PctRepaired19EV, 
#                                              w = Pop, na.rm = T),
#             wDaysToRepairAvgEV = weighted.mean(x = DaysToRepairAvgEV, 
#                                                w = Pop, na.rm = T)) %>% 
#   mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
#                         "minority_E" = "People of Color",
#                         "nh2morepop_E" = "Two or more races",
#                         "nhamerindpop_E" = "Native American",
#                         "nhasianpop_E" = "Asian",
#                         "nhblackpop_E" = "Black",
#                         "nhnativpop_E" = "Native Pacific Islander",
#                         "nhotherpop_E" = "Other race",
#                         "nhwhitepop_E" = "White",
#                         "totalpop_E" = "Total Population",
#                         "eng_hhE" = "Total Households",
#                         "under5E" = "Under 5",
#                         "over64E" = "Over 64",
#                         "eng_limitE" = "Limited English HH",
#                         "num2povE" = "Low Income",
#                         "lthsE" = "No HS Diploma",
#                         "total_occ_unitsE" = "Total Occupied HU",
#                         "renter_occ_unitsE" = "Renter Occupied HU",
#                         "disabledOver18E" = "Disabled Adults",
#                         "house_burdened_E" = "Housing Burdened"))
# 
# # Columbia Gas
# ppLeakDensityCG <- ma_tracts %>% 
#   as.data.frame() %>% 
#   filter(str_detect(GAS, "^Columbia") | 
#            str_detect(GAS, "Columbia Gas$")) %>% # limit to BGs in CG svc area
#   mutate(leaks_sqkmCG = `Columbia Gas_19unrepaired`/area_sqkm,
#          REPleaks_sqkmCG = `Columbia Gas_19repaired`/area_sqkm,
#          AllLeaks2019_sqkmCG = (`Columbia Gas_19unrepaired` + 
#                                   `Columbia Gas_19repaired`)/area_sqkm,
#          leaks_huCG = if_else(total_occ_unitsE == 0, 0, 
#                               `Columbia Gas_19unrepaired`/
#                                 total_occ_unitsE),
#          REPleaks_huCG = if_else(total_occ_unitsE == 0, 0,
#                                  `Columbia Gas_19repaired`/
#                                    total_occ_unitsE),
#          AllLeaks2019_huCG = if_else(total_occ_unitsE == 0, 0,
#                                      (`Columbia Gas_19unrepaired` + 
#                                         `Columbia Gas_19repaired`)
#                                      /total_occ_unitsE),
#          PctRepaired19CG = `Columbia Gas_19repaired`/ 
#            (`Columbia Gas_19unrepaired` + 
#               `Columbia Gas_19repaired`)*100,
#          DaysToRepairAvgCG = `Columbia Gas_19repairedDaysAvg`,
#          LeakAgeDaysAvgCG = `Columbia Gas_19unrepairedDaysAvg`) %>%
#   select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
#          eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
#          (starts_with("leaks_") & ends_with("CG")), 
#          (starts_with("AllLeaks") & ends_with("CG")), 
#          (starts_with("LeakAgeDaysAvg") & ends_with("CG")),
#          (starts_with("REPleaks_") & ends_with("CG")), 
#          (starts_with("DaystoRepairAvg") & ends_with("CG")), 
#          (starts_with("PctRepaired19") & ends_with("CG")),
#          (starts_with("leaks_hu") & ends_with("CG")), 
#          (starts_with("REPleaks_hu") & ends_with("CG")),
#          (starts_with("ALLleaks_hu") & ends_with("CG"))) %>% 
#   pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
#                values_to = "Pop", values_drop_na = TRUE) %>% 
#   group_by(Group) %>% 
#   summarize(wLeaksPerSqKmCG = weighted.mean(x = leaks_sqkmCG, w = Pop, 
#                                             na.rm = TRUE),
#             wLeaksPerSqKmREPCG = weighted.mean(x = REPleaks_sqkmCG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeaksPerSqKmALLCG = weighted.mean(x = AllLeaks2019_sqkmCG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeakAgeDaysAvgCG = weighted.mean(x = LeakAgeDaysAvgCG,
#                                               w = Pop, na.rm = TRUE),
#             wLeaksPerHUCG = weighted.mean(x = leaks_huCG, w = Pop, 
#                                           na.rm = T),
#             wREPLeaksPerHUCG = weighted.mean(x = REPleaks_huCG, 
#                                              w = Pop, na.rm = T),
#             wALLLeaksPerHUCG = weighted.mean(x = AllLeaks2019_huCG, 
#                                              w = Pop, na.rm = T),
#             wPctRepaired19CG = weighted.mean(x = PctRepaired19CG, 
#                                              w = Pop, na.rm = T),
#             wDaysToRepairAvgCG = weighted.mean(x = DaysToRepairAvgCG, 
#                                                w = Pop, na.rm = T)) %>% 
#   mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
#                         "minority_E" = "People of Color",
#                         "nh2morepop_E" = "Two or more races",
#                         "nhamerindpop_E" = "Native American",
#                         "nhasianpop_E" = "Asian",
#                         "nhblackpop_E" = "Black",
#                         "nhnativpop_E" = "Native Pacific Islander",
#                         "nhotherpop_E" = "Other race",
#                         "nhwhitepop_E" = "White",
#                         "totalpop_E" = "Total Population",
#                         "eng_hhE" = "Total Households",
#                         "under5E" = "Under 5",
#                         "over64E" = "Over 64",
#                         "eng_limitE" = "Limited English HH",
#                         "num2povE" = "Low Income",
#                         "lthsE" = "No HS Diploma",
#                         "total_occ_unitsE" = "Total Occupied HU",
#                         "renter_occ_unitsE" = "Renter Occupied HU",
#                         "disabledOver18E" = "Disabled Adults",
#                         "house_burdened_E" = "Housing Burdened"))
# 
# # Fitchburg Gas aka Unitil
# ppLeakDensityFG <- ma_tracts %>% 
#   as.data.frame() %>% 
#   filter(str_detect(GAS, "^Unitil") | 
#            str_detect(GAS, "Unitil$")) %>% # limit to BGs in FG svc area
#   mutate(leaks_sqkmFG = `Fitchburg Gas_19unrepaired`/area_sqkm,
#          REPleaks_sqkmFG = `Fitchburg Gas_19repaired`/area_sqkm,
#          AllLeaks2019_sqkmFG = (`Fitchburg Gas_19unrepaired` + 
#                                   `Fitchburg Gas_19repaired`)/area_sqkm,
#          leaks_huFG = if_else(total_occ_unitsE == 0, 0, 
#                               `Fitchburg Gas_19unrepaired`/
#                                 total_occ_unitsE),
#          REPleaks_huFG = if_else(total_occ_unitsE == 0, 0,
#                                  `Fitchburg Gas_19repaired`/
#                                    total_occ_unitsE),
#          AllLeaks2019_huFG = if_else(total_occ_unitsE == 0, 0,
#                                      (`Fitchburg Gas_19unrepaired` + 
#                                         `Fitchburg Gas_19repaired`)
#                                      /total_occ_unitsE),
#          PctRepaired19FG = `Fitchburg Gas_19repaired`/ 
#            (`Fitchburg Gas_19unrepaired` + 
#               `Fitchburg Gas_19repaired`)*100,
#          DaysToRepairAvgFG = `Fitchburg Gas_19repairedDaysAvg`,
#          LeakAgeDaysAvgFG = `Fitchburg Gas_19unrepairedDaysAvg`) %>%
#   select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
#          eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
#          (starts_with("leaks_") & ends_with("FG")), 
#          (starts_with("AllLeaks") & ends_with("FG")), 
#          (starts_with("LeakAgeDaysAvg") & ends_with("FG")),
#          (starts_with("REPleaks_") & ends_with("FG")), 
#          (starts_with("DaystoRepairAvg") & ends_with("FG")), 
#          (starts_with("PctRepaired19") & ends_with("FG")),
#          (starts_with("leaks_hu") & ends_with("FG")), 
#          (starts_with("REPleaks_hu") & ends_with("FG")),
#          (starts_with("ALLleaks_hu") & ends_with("FG"))) %>% 
#   pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
#                values_to = "Pop", values_drop_na = TRUE) %>% 
#   group_by(Group) %>% 
#   summarize(wLeaksPerSqKmFG = weighted.mean(x = leaks_sqkmFG, w = Pop, 
#                                             na.rm = TRUE),
#             wLeaksPerSqKmREPFG = weighted.mean(x = REPleaks_sqkmFG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeaksPerSqKmALLFG = weighted.mean(x = AllLeaks2019_sqkmFG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeakAgeDaysAvgFG = weighted.mean(x = LeakAgeDaysAvgFG,
#                                               w = Pop, na.rm = TRUE),
#             wLeaksPerHUFG = weighted.mean(x = leaks_huFG, w = Pop, 
#                                           na.rm = T),
#             wREPLeaksPerHUFG = weighted.mean(x = REPleaks_huFG, 
#                                              w = Pop, na.rm = T),
#             wALLLeaksPerHUFG = weighted.mean(x = AllLeaks2019_huFG, 
#                                              w = Pop, na.rm = T),
#             wPctRepaired19FG = weighted.mean(x = PctRepaired19FG, 
#                                              w = Pop, na.rm = T),
#             wDaysToRepairAvgFG = weighted.mean(x = DaysToRepairAvgFG, 
#                                                w = Pop, na.rm = T)) %>% 
#   mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
#                         "minority_E" = "People of Color",
#                         "nh2morepop_E" = "Two or more races",
#                         "nhamerindpop_E" = "Native American",
#                         "nhasianpop_E" = "Asian",
#                         "nhblackpop_E" = "Black",
#                         "nhnativpop_E" = "Native Pacific Islander",
#                         "nhotherpop_E" = "Other race",
#                         "nhwhitepop_E" = "White",
#                         "totalpop_E" = "Total Population",
#                         "eng_hhE" = "Total Households",
#                         "under5E" = "Under 5",
#                         "over64E" = "Over 64",
#                         "eng_limitE" = "Limited English HH",
#                         "num2povE" = "Low Income",
#                         "lthsE" = "No HS Diploma",
#                         "total_occ_unitsE" = "Total Occupied HU",
#                         "renter_occ_unitsE" = "Renter Occupied HU",
#                         "disabledOver18E" = "Disabled Adults",
#                         "house_burdened_E" = "Housing Burdened"))
# 
# # Liberty Utilities
# ppLeakDensityLU <- ma_tracts %>% 
#   as.data.frame() %>% 
#   filter(GAS == "Liberty Utilities") %>% # limit to BGs in LU svc area
#   mutate(leaks_sqkmLU = `Liberty Utilities_19unrepaired`/area_sqkm,
#          REPleaks_sqkmLU = `Liberty Utilities_19repaired`/area_sqkm,
#          AllLeaks2019_sqkmLU = (`Liberty Utilities_19unrepaired` + 
#                                   `Liberty Utilities_19repaired`)/area_sqkm,
#          leaks_huLU = if_else(total_occ_unitsE == 0, 0, 
#                               `Liberty Utilities_19unrepaired`/
#                                 total_occ_unitsE),
#          REPleaks_huLU = if_else(total_occ_unitsE == 0, 0,
#                                  `Liberty Utilities_19repaired`/
#                                    total_occ_unitsE),
#          AllLeaks2019_huLU = if_else(total_occ_unitsE == 0, 0,
#                                      (`Liberty Utilities_19unrepaired` + 
#                                         `Liberty Utilities_19repaired`)
#                                      /total_occ_unitsE),
#          PctRepaired19LU = `Liberty Utilities_19repaired`/ 
#            (`Liberty Utilities_19unrepaired` + 
#               `Liberty Utilities_19repaired`)*100,
#          DaysToRepairAvgLU = `Liberty Utilities_19repairedDaysAvg`,
#          LeakAgeDaysAvgLU = `Liberty Utilities_19unrepairedDaysAvg`) %>%
#   select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
#          eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
#          (starts_with("leaks_") & ends_with("LU")), 
#          (starts_with("AllLeaks") & ends_with("LU")), 
#          (starts_with("LeakAgeDaysAvg") & ends_with("LU")),
#          (starts_with("REPleaks_") & ends_with("LU")), 
#          (starts_with("DaystoRepairAvg") & ends_with("LU")), 
#          (starts_with("PctRepaired19") & ends_with("LU")),
#          (starts_with("leaks_hu") & ends_with("LU")), 
#          (starts_with("REPleaks_hu") & ends_with("LU")),
#          (starts_with("ALLleaks_hu") & ends_with("LU"))) %>% 
#   pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
#                values_to = "Pop", values_drop_na = TRUE) %>% 
#   group_by(Group) %>% 
#   summarize(wLeaksPerSqKmLU = weighted.mean(x = leaks_sqkmLU, w = Pop, 
#                                             na.rm = TRUE),
#             wLeaksPerSqKmREPLU = weighted.mean(x = REPleaks_sqkmLU, 
#                                                w = Pop, na.rm = TRUE),
#             wLeaksPerSqKmALLLU = weighted.mean(x = AllLeaks2019_sqkmLU, 
#                                                w = Pop, na.rm = TRUE),
#             wLeakAgeDaysAvgLU = weighted.mean(x = LeakAgeDaysAvgLU,
#                                               w = Pop, na.rm = TRUE),
#             wLeaksPerHULU = weighted.mean(x = leaks_huLU, w = Pop, 
#                                           na.rm = T),
#             wREPLeaksPerHULU = weighted.mean(x = REPleaks_huLU, 
#                                              w = Pop, na.rm = T),
#             wALLLeaksPerHULU = weighted.mean(x = AllLeaks2019_huLU, 
#                                              w = Pop, na.rm = T),
#             wPctRepaired19LU = weighted.mean(x = PctRepaired19LU, 
#                                              w = Pop, na.rm = T),
#             wDaysToRepairAvgLU = weighted.mean(x = DaysToRepairAvgLU, 
#                                                w = Pop, na.rm = T)) %>% 
#   mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
#                         "minority_E" = "People of Color",
#                         "nh2morepop_E" = "Two or more races",
#                         "nhamerindpop_E" = "Native American",
#                         "nhasianpop_E" = "Asian",
#                         "nhblackpop_E" = "Black",
#                         "nhnativpop_E" = "Native Pacific Islander",
#                         "nhotherpop_E" = "Other race",
#                         "nhwhitepop_E" = "White",
#                         "totalpop_E" = "Total Population",
#                         "eng_hhE" = "Total Households",
#                         "under5E" = "Under 5",
#                         "over64E" = "Over 64",
#                         "eng_limitE" = "Limited English HH",
#                         "num2povE" = "Low Income",
#                         "lthsE" = "No HS Diploma",
#                         "total_occ_unitsE" = "Total Occupied HU",
#                         "renter_occ_unitsE" = "Renter Occupied HU",
#                         "disabledOver18E" = "Disabled Adults",
#                         "house_burdened_E" = "Housing Burdened"))
# 
# # The Berkshire Gas Company 
# ppLeakDensityBG <- ma_tracts %>% 
#   as.data.frame() %>% 
#   filter(GAS == "The Berkshire Gas Company") %>% # limit to BGs in BG svc area
#   mutate(leaks_sqkmBG = `Berkshire Gas_19unrepaired`/area_sqkm,
#          REPleaks_sqkmBG = `Berkshire Gas_19repaired`/area_sqkm,
#          AllLeaks2019_sqkmBG = (`Berkshire Gas_19unrepaired` + 
#                                   `Berkshire Gas_19repaired`)/area_sqkm,
#          leaks_huBG = if_else(total_occ_unitsE == 0, 0, 
#                               `Berkshire Gas_19unrepaired`/
#                                 total_occ_unitsE),
#          REPleaks_huBG = if_else(total_occ_unitsE == 0, 0,
#                                  `Berkshire Gas_19repaired`/
#                                    total_occ_unitsE),
#          AllLeaks2019_huBG = if_else(total_occ_unitsE == 0, 0,
#                                      (`Berkshire Gas_19unrepaired` + 
#                                         `Berkshire Gas_19repaired`)
#                                      /total_occ_unitsE),
#          PctRepaired19BG = `Berkshire Gas_19repaired`/ 
#            (`Berkshire Gas_19unrepaired` + 
#               `Berkshire Gas_19repaired`)*100,
#          DaysToRepairAvgBG = `Berkshire Gas_19repairedDaysAvg`,
#          LeakAgeDaysAvgBG = `Berkshire Gas_19unrepairedDaysAvg`) %>%
#   select(ends_with("_E"), disabledOver18E, eng_hhE, under5E, over64E, 
#          eng_limitE, num2povE, lthsE, ends_with("unitsE"), 
#          (starts_with("leaks_") & ends_with("BG")), 
#          (starts_with("AllLeaks") & ends_with("BG")),
#          (starts_with("LeakAgeDaysAvg") & ends_with("BG")),
#          (starts_with("REPleaks_") & ends_with("BG")), 
#          (starts_with("DaystoRepairAvg") & ends_with("BG")), 
#          (starts_with("PctRepaired19") & ends_with("BG")),
#          (starts_with("leaks_hu") & ends_with("BG")), 
#          (starts_with("REPleaks_hu") & ends_with("BG")),
#          (starts_with("ALLleaks_hu") & ends_with("BG"))) %>% 
#   pivot_longer(., cols = totalpop_E:renter_occ_unitsE, names_to = "Group", 
#                values_to = "Pop", values_drop_na = TRUE) %>% 
#   group_by(Group) %>% 
#   summarize(wLeaksPerSqKmBG = weighted.mean(x = leaks_sqkmBG, w = Pop, 
#                                             na.rm = TRUE),
#             wLeaksPerSqKmREPBG = weighted.mean(x = REPleaks_sqkmBG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeaksPerSqKmALLBG = weighted.mean(x = AllLeaks2019_sqkmBG, 
#                                                w = Pop, na.rm = TRUE),
#             wLeakAgeDaysAvgBG = weighted.mean(x = LeakAgeDaysAvgBG,
#                                               w = Pop, na.rm = TRUE),
#             wLeaksPerHUBG = weighted.mean(x = leaks_huBG, w = Pop, 
#                                           na.rm = T),
#             wREPLeaksPerHUBG = weighted.mean(x = REPleaks_huBG, 
#                                              w = Pop, na.rm = T),
#             wALLLeaksPerHUBG = weighted.mean(x = AllLeaks2019_huBG, 
#                                              w = Pop, na.rm = T),
#             wPctRepaired19BG = weighted.mean(x = PctRepaired19BG, 
#                                              w = Pop, na.rm = T),
#             wDaysToRepairAvgBG = weighted.mean(x = DaysToRepairAvgBG, 
#                                                w = Pop, na.rm = T)) %>% 
#   mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
#                         "minority_E" = "People of Color",
#                         "nh2morepop_E" = "Two or more races",
#                         "nhamerindpop_E" = "Native American",
#                         "nhasianpop_E" = "Asian",
#                         "nhblackpop_E" = "Black",
#                         "nhnativpop_E" = "Native Pacific Islander",
#                         "nhotherpop_E" = "Other race",
#                         "nhwhitepop_E" = "White",
#                         "totalpop_E" = "Total Population",
#                         "eng_hhE" = "Total Households",
#                         "under5E" = "Under 5",
#                         "over64E" = "Over 64",
#                         "eng_limitE" = "Limited English HH",
#                         "num2povE" = "Low Income",
#                         "lthsE" = "No HS Diploma",
#                         "total_occ_unitsE" = "Total Occupied HU",
#                         "renter_occ_unitsE" = "Renter Occupied HU",
#                         "disabledOver18E" = "Disabled Adults",
#                         "house_burdened_E" = "Housing Burdened"))
# 
# 
# # Join the utility df together
# ppLeakDensityJoinedU <- list(ppLeakDensityBG,
#                              ppLeakDensityCG,
#                              ppLeakDensityEV,
#                              ppLeakDensityFG,
#                              ppLeakDensityLU,
#                              ppLeakDensityNG) %>% 
#   reduce(left_join, by = "Group")
# 
# 
# # Facet wrap by utility for unrepaired leaks
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wLeaksPerSqKmBG, wLeaksPerSqKmCG, wLeaksPerSqKmEV, 
#                  wLeaksPerSqKmFG, wLeaksPerSqKmLU, wLeaksPerSqKmNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wLeaksPerSqKmBG" = "Berkshire Gas",
#                           "wLeaksPerSqKmCG" = "Columbia Gas",
#                           "wLeaksPerSqKmEV" = "Eversource Energy",
#                           "wLeaksPerSqKmFG" = "Unitil/Fitchburg Gas",
#                           "wLeaksPerSqKmLU" = "Liberty Utilities",
#                           "wLeaksPerSqKmNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = expression(paste("Population-weighted mean leak density (leaks/", 
#                             km^2, ")", " by Census Tract", sep = "")),
#        title = "Piority Populations and Unrepaired Gas Leaks by Utility for 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyUtility_tract.png")
# 
# 
# # Facet wrap by utility for unrepaired leaks per occupied housing unit
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wLeaksPerHUBG, wLeaksPerHUCG, wLeaksPerHUEV, 
#                  wLeaksPerHUFG, wLeaksPerHULU, wLeaksPerHUNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wLeaksPerHUBG" = "Berkshire Gas",
#                           "wLeaksPerHUCG" = "Columbia Gas",
#                           "wLeaksPerHUEV" = "Eversource Energy",
#                           "wLeaksPerHUFG" = "Unitil/Fitchburg Gas",
#                           "wLeaksPerHULU" = "Liberty Utilities",
#                           "wLeaksPerHUNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Tract",
#        title = "Piority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")
# 
# ggsave("Images/LeaksPPbyUtility_HU_tract.png")
# 
# 
# # Facet wrap by utility for repaired leaks
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wLeaksPerSqKmREPBG, wLeaksPerSqKmREPCG, wLeaksPerSqKmREPEV, 
#                  wLeaksPerSqKmREPFG, wLeaksPerSqKmREPLU, wLeaksPerSqKmREPNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wLeaksPerSqKmREPBG" = "Berkshire Gas",
#                           "wLeaksPerSqKmREPCG" = "Columbia Gas",
#                           "wLeaksPerSqKmREPEV" = "Eversource Energy",
#                           "wLeaksPerSqKmREPFG" = "Unitil/Fitchburg Gas",
#                           "wLeaksPerSqKmREPLU" = "Liberty Utilities",
#                           "wLeaksPerSqKmREPNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = expression(paste("Population-weighted mean leak density (leaks/", 
#                             km^2, ")", " by Census Tract", sep = "")),
#        title = "Piority Populations and Repaired Gas Leaks by Utility for 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyUtilityREP_tract.png")
# 
# 
# # Facet wrap by utility for repaired leaks per occupied housing unit
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wREPLeaksPerHUBG, wREPLeaksPerHUCG, wREPLeaksPerHUEV, 
#                  wREPLeaksPerHUFG, wREPLeaksPerHULU, wREPLeaksPerHUNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wREPLeaksPerHUBG" = "Berkshire Gas",
#                           "wREPLeaksPerHUCG" = "Columbia Gas",
#                           "wREPLeaksPerHUEV" = "Eversource Energy",
#                           "wREPLeaksPerHUFG" = "Unitil/Fitchburg Gas",
#                           "wREPLeaksPerHULU" = "Liberty Utilities",
#                           "wREPLeaksPerHUNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean of repaired leaks per occupied housing unit by Census Tract",
#        title = "Piority Populations and Repaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")
# 
# ggsave("Images/LeaksPPbyUtilityREP_HU_tract.png")
# 
# 
# # Facet wrap by utility for all (repaired + unrepaired) leaks
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wLeaksPerSqKmALLBG, wLeaksPerSqKmALLCG, wLeaksPerSqKmALLEV, 
#                  wLeaksPerSqKmALLFG, wLeaksPerSqKmALLLU, wLeaksPerSqKmALLNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wLeaksPerSqKmALLBG" = "Berkshire Gas",
#                           "wLeaksPerSqKmALLCG" = "Columbia Gas",
#                           "wLeaksPerSqKmALLEV" = "Eversource Energy",
#                           "wLeaksPerSqKmALLFG" = "Unitil/Fitchburg Gas",
#                           "wLeaksPerSqKmALLLU" = "Liberty Utilities",
#                           "wLeaksPerSqKmALLNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = expression(paste("Population-weighted mean leak density (leaks/", 
#                             km^2, ")", " by Census Tract", sep = "")),
#        title = "Piority Populations and All Repaired and Unrepaired Gas Leaks by Utility for 2019 across Massachusetts")
# 
# ggsave("Images/LeaksPPbyUtilityALL_tract.png")
# 
# # Facet wrap by utility for all (repaired + unrepaired) leaks per occupied housing unit
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wALLLeaksPerHUBG, wALLLeaksPerHUCG, wALLLeaksPerHUEV, 
#                  wALLLeaksPerHUFG, wALLLeaksPerHULU, wALLLeaksPerHUNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wALLLeaksPerHUBG" = "Berkshire Gas",
#                           "wALLLeaksPerHUCG" = "Columbia Gas",
#                           "wALLLeaksPerHUEV" = "Eversource Energy",
#                           "wALLLeaksPerHUFG" = "Unitil/Fitchburg Gas",
#                           "wALLLeaksPerHULU" = "Liberty Utilities",
#                           "wALLLeaksPerHUNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean leaks per occupied housing unity by Census Tract",
#        title = "Piority Populations and All Repaired and Unrepaired Gas Leaks Per Occupied Housing Unit by Utility for 2019")
# 
# ggsave("Images/LeaksPPbyUtilityALL_HU_tract.png")
# 
# # Facet wrap by utility for percentage of leaks repaired
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wPctRepaired19BG, wPctRepaired19CG, wPctRepaired19EV, 
#                  wPctRepaired19FG, wPctRepaired19LU, wPctRepaired19NG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wPctRepaired19BG" = "Berkshire Gas",
#                           "wPctRepaired19CG" = "Columbia Gas",
#                           "wPctRepaired19EV" = "Eversource Energy",
#                           "wPctRepaired19FG" = "Unitil/Fitchburg Gas",
#                           "wPctRepaired19LU" = "Liberty Utilities",
#                           "wPctRepaired19NG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean percentage of leaks repaired in 2019 by Census Tract",
#        title = "Piority Populations Percent Repaired Gas Leaks in 2019 by Utility across Massachusetts")
# 
# ggsave("Images/LeaksPPbyUtilityPctFix_tract.png")
# 
# 
# # Facet wrap by utility for average age of unrepaired leaks
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wLeakAgeDaysAvgBG, wLeakAgeDaysAvgCG, wLeakAgeDaysAvgEV, 
#                  wLeakAgeDaysAvgFG, wLeakAgeDaysAvgLU, wLeakAgeDaysAvgNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wLeakAgeDaysAvgBG" = "Berkshire Gas",
#                           "wLeakAgeDaysAvgCG" = "Columbia Gas",
#                           "wLeakAgeDaysAvgEV" = "Eversource Energy",
#                           "wLeakAgeDaysAvgFG" = "Unitil/Fitchburg Gas",
#                           "wLeakAgeDaysAvgLU" = "Liberty Utilities",
#                           "wLeakAgeDaysAvgNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   scale_y_continuous(labels = function(x) format(x, big.mark = ",",
#                                                  scientific = FALSE)) +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean age (days) of unrepaired leaks by Census Tract",
#        title = "Piority Populations and Average Age of Unrepaired Leaks in 2019 by Utility across Massachusetts")
# 
# ggsave("Images/LeaksPPbyUtilityAge_tract.png")
# 
# 
# # Facet wrap by utility for average time to repair leaks
# ppLeakDensityJoinedU %>% 
#   pivot_longer(c(wDaysToRepairAvgBG, wDaysToRepairAvgCG, wDaysToRepairAvgEV, 
#                  wDaysToRepairAvgFG, wDaysToRepairAvgLU, wDaysToRepairAvgNG), 
#                names_to = "Utility", values_to = "leakDensity") %>% 
#   filter(!Group %in% c("Native American", "Other race", 
#                        "Native Pacific Islander", "Two or more races")) %>% 
#   drop_na(leakDensity) %>% 
#   mutate(Utility = recode(Utility, "wDaysToRepairAvgBG" = "Berkshire Gas",
#                           "wDaysToRepairAvgCG" = "Columbia Gas",
#                           "wDaysToRepairAvgEV" = "Eversource Energy",
#                           "wDaysToRepairAvgFG" = "Unitil/Fitchburg Gas",
#                           "wDaysToRepairAvgLU" = "Liberty Utilities",
#                           "wDaysToRepairAvgNG" = "National Grid"),
#          Group = reorder_within(Group, leakDensity, Utility)) %>% 
#   ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
#   geom_col(show.legend = FALSE, na.rm = TRUE) +
#   coord_flip() + 
#   scale_x_reordered() +
#   scale_y_continuous(labels = function(x) format(x, big.mark = ",",
#                                                  scientific = FALSE)) +
#   theme_minimal(base_size = 6) +
#   facet_wrap(~ Utility, scales = "free") +
#   labs(x = NULL, 
#        y = "Population-weighted mean percentage of leak repair time(days) by Census Tract",
#        title = "Piority Populations and Average Leak Repair Time in 2019 by Utility across Massachusetts")
# 
# ggsave("Images/LeaksPPbyUtilityTime_tract.png")
# 
# 
# # conduct a weighted t-test to determine if differences are statistically significant
# # library(weights)
# weights::wtd.t.test(x = ma_tracts$leaks_sqkm, y = ma_tracts$leaks_sqkm, 
#                     weight = ma_tracts$nhwhitepop_E, 
#                     weighty = ma_tracts$nhblackpop_E, 
#                     bootse = TRUE, bootp = TRUE)
# 
# weights::wtd.t.test(x = ma_tracts$PctRepaired19, 
#                     y = ma_tracts$PctRepaired19, 
#                     weight = ma_tracts$nhwhitepop_E, 
#                     weighty = ma_tracts$eng_limitE, 
#                     bootse = TRUE, bootp = TRUE)


# scatterplot matrices


# correlation matrix between leak frequency and demographic group