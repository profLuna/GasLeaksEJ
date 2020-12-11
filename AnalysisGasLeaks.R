# Analysis of relationship between gas leaks and demographics

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(ggplot2)
library(foreign)

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


# # spatially aggregate leaks by block groups
# blkgrp_unrepaired <- ma_blkgrps18 %>% 
#   st_join(unrepaired2019final) %>% 
#   group_by(GEOID) %>% 
#   summarize(unrepaired2019 = n()) %>% 
#   as.data.frame() %>% 
#   select(-geometry) %>% 
#   left_join(ma_blkgrps18, ., by = "GEOID")

# # assign block group GEOID to each unrepaired leak that falls within it
# unrepaired2019_toblkgrp <- ma_blkgrps18 %>% 
#   select(GEOID) %>% 
#   st_join(unrepaired2019final, .)

# assign block group data to each unrepaired leak it intersects. 
# join block group info to each leak that falls within it
unrepaired2019_with_blkgrp <- ma_blkgrps18 %>% 
  select(GEOID) %>%
  st_join(unrepaired2019final, ., largest = TRUE)

# break them out by leak class
unrepaired2019_with_blkgrpC1 <- unrepaired2019_with_blkgrp %>% 
  filter(Class == "1")

unrepaired2019_with_blkgrpC2 <- unrepaired2019_with_blkgrp %>% 
  filter(Class == "2")

unrepaired2019_with_blkgrpC3 <- unrepaired2019_with_blkgrp %>% 
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
  

# ma_blkgrps18 <- unrepaired2019_by_utility %>% 
#   as.data.frame() %>% # to prevent dplyr trying to re-add grouping variable
#   select(-Utility) %>% # get rid of redundant grouping variable
#   left_join(ma_blkgrps18, ., by = "GEOID")
# 
# ma_blkgrps18 <- unrepaired2019_by_utilityC1 %>% 
#   as.data.frame() %>% # to prevent dplyr trying to re-add grouping variable
#   select(-Utility) %>% # get rid of redundant grouping variable
#   left_join(ma_blkgrps18, ., by = "GEOID")
# 
# ma_blkgrps18 <- unrepaired2019_by_utilityC2 %>% 
#   as.data.frame() %>% # to prevent dplyr trying to re-add grouping variable
#   select(-Utility) %>% # get rid of redundant grouping variable
#   left_join(ma_blkgrps18, ., by = "GEOID")
# 
# ma_blkgrps18 <- unrepaired2019_by_utilityC3 %>% 
#   as.data.frame() %>% # to prevent dplyr trying to re-add grouping variable
#   select(-Utility) %>% # get rid of redundant grouping variable
#   left_join(ma_blkgrps18, ., by = "GEOID")

# join leak counts by utility to the block group demographics
ma_blkgrps18 <- list(ma_blkgrps18, unrepaired2019_by_utility2, 
                          unrepaired2019_by_utility2C1,
                          unrepaired2019_by_utility2C2,
                          unrepaired2019_by_utility2C3) %>% 
  reduce(left_join, by = "GEOID") %>% 
  mutate(
    across(unrepaired2019total:`National Grid - Colonial Gas_19unrepairedC3`, 
           ~replace_na(., 0)))

# Look at distributions of leak counts by block group
ma_blkgrps18 %>% 
  ggplot(aes(x = unrepaired2019total)) + geom_histogram()

summary(ma_blkgrps18$unrepaired2019total)

ma_blkgrps18 %>% 
  ggplot(aes(x = unrepaired2019totalC1)) + geom_histogram()

summary(ma_blkgrps18$unrepaired2019totalC1)

ma_blkgrps18 %>% 
  ggplot(aes(x = unrepaired2019totalC2)) + geom_histogram()

summary(ma_blkgrps18$unrepaired2019totalC2)

ma_blkgrps18 %>% 
  ggplot(aes(x = unrepaired2019totalC3)) + geom_histogram()

summary(ma_blkgrps18$unrepaired2019totalC3)

# # create summary of total unrepaired leaks by block group
# # all leaks
# blkgrp_total_unrepaired <- unrepaired2019_toblkgrp %>% 
#   as.data.frame() %>% 
#   group_by(GEOID) %>% 
#   summarize(unrepaired2019total = n())

# # class 1 leaks
# blkgrp_class1_unrepaired <- unrepaired2019_toblkgrp %>% 
#   as.data.frame() %>% 
#   filter(Class == 1) %>% 
#   group_by(GEOID) %>% 
#   summarize(unrepaired2019class1 = n())
# 
# # class 2 leaks
# blkgrp_class2_unrepaired <- unrepaired2019_toblkgrp %>% 
#   as.data.frame() %>% 
#   filter(Class == 2) %>% 
#   group_by(GEOID) %>% 
#   summarize(unrepaired2019class2 = n())
# 
# # class 3 leaks
# blkgrp_class3_unrepaired <- unrepaired2019_toblkgrp %>% 
#   as.data.frame() %>% 
#   filter(Class == 3) %>% 
#   group_by(GEOID) %>% 
#   summarize(unrepaired2019class3 = n())
# 
# # join unrepaired leak counts to block group layer
# ma_blkgrps18 <- ma_blkgrps18 %>% 
#   left_join(., blkgrp_total_unrepaired, by = "GEOID") %>% 
#   left_join(., blkgrp_class1_unrepaired, by = "GEOID") %>%
#   left_join(., blkgrp_class2_unrepaired, by = "GEOID") %>%
#   left_join(., blkgrp_class3_unrepaired, by = "GEOID") %>%
#   mutate(area_sqkm = as.numeric(st_area(.)/10^6),
#          leaks_sqkm = if_else(unrepaired2019total > 0, 
#                               unrepaired2019total/area_sqkm, 0)) %>% 
#   replace_na(list(unrepaired2019total = 0, unrepaired2019class1= 0, 
#                   unrepaired2019class2 = 0, unrepaired2019class3 = 0, 
#                   leaks_sqkm = 0))


# comparison of population-weighted leak frequency and density by demographic group, restricting to areas served by gas utilities for which we have leak data
raceLeakDensity <- ma_blkgrps18 %>% 
  # st_filter(., ng_service_areas) %>%
  as.data.frame() %>% 
  select(ends_with("_E"), leaks_sqkm) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKm = weighted.mean(x = leaks_sqkm, w = Pop)) %>% 
  mutate(Group = recode(Group, "hisppop_E" = "Hispanic", 
                "minority_E" = "People of Color",
                "nh2morepop_E" = "Two or more races",
                "nhamerindpop_E" = "Native American",
                "nhasianpop_E" = "Asian",
                "nhblackpop_E" = "Black",
                "nhnativpop_E" = "Native Pacific Islander",
                "nhotherpop_E" = "Other race",
                "nhwhitepop_E" = "White",
                "totalpop_E" = "Total Population"))

raceLeakDensityUC <- ma_blkgrps18 %>% 
  st_filter(., ng_service_areas) %>%
  as.data.frame() %>% 
  select(ends_with("_UC") & starts_with("nh"), hisppop_UC, minority_UC, 
         totalpop_UC, leaks_sqkm) %>% 
  pivot_longer(., cols = ends_with("_UC"), names_to = "Group", 
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
                        "totalpop_UC" = "Total Population"))

raceLeakDensityLC <- ma_blkgrps18 %>% 
  st_filter(., ng_service_areas) %>%
  as.data.frame() %>% 
  select(ends_with("_LC") & starts_with("nh"), hisppop_LC, minority_LC, 
         totalpop_LC, leaks_sqkm) %>% 
  pivot_longer(., cols = ends_with("_LC"), names_to = "Group", 
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
                        "totalpop_LC" = "Total Population")) %>% 
  replace_na(., list(wLeaksPerSqKmLC = 0))

# bring df together
raceLeakDensityJoined <- raceLeakDensity %>% 
  left_join(., raceLeakDensityLC, by = "Group") %>% 
  left_join(., raceLeakDensityUC, by = "Group")

# create a bar plot with error bars
raceLeakDensityJoined %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKm), y = wLeaksPerSqKm)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted leak density (leaks/SqKm)") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wLeaksPerSqKmUC, ymax = wLeaksPerSqKmLC))


# Facet wrap by utility
ma_blkgrps18 %>% 
  st_join(., ng_service_areas, largest = TRUE) %>% 
  as.data.frame() %>% 
  select(ends_with("_E"), leaks_sqkm, GAS) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group, GAS) %>% 
  summarize(wLeaksPerSqKm = weighted.mean(x = leaks_sqkm, w = Pop)) %>% 
  # filter(GAS %in% c("Columbia Gas", "Eversource Energy", "Liberty Utilities", 
  #                   "National Grid", "National Grid, Eversource Energy", 
  #                   "The Berkshire Gas Company", "Unitil")) %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKm), y = wLeaksPerSqKm)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted leak density (leaks/SqKm)") +
  facet_wrap(~ GAS, drop = TRUE)

# inspect differences by utility
test <- ng_service_areas %>% 
  select(-TOWN) %>% 
  mutate(GAS = as.character(GAS)) %>% 
  st_join(ma_blkgrps18, ., largest = TRUE) %>% 
  as.data.frame()

# conduct a weighted t-test to determine if differences are statistically significant
library(weights)
wtd.t.test(x = ma_blkgrps18$leaks_sqkm, y = ma_blkgrps18$leaks_sqkm, 
           weight = ma_blkgrps18$nhwhitepop_E, 
           weighty = ma_blkgrps18$nhblackpop_E, 
           bootse = TRUE, bootp = TRUE)



# correlation matrix between leak frequency and demographic group