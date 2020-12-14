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


# comparison of population-weighted leak frequency and density by demographic group, restricting to areas served by gas utilities for which we have leak data
raceLeakDensity <- ma_blkgrps18 %>% 
  # st_filter(., ng_service_areas) %>%
  as.data.frame() %>% 
  select(ends_with("_E"), starts_with("leaks_")) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKm = weighted.mean(x = leaks_sqkm, w = Pop),
            wLeaksPerSqKmC1 = weighted.mean(x = leaks_sqkmC1, w = Pop),
            wLeaksPerSqKmC2 = weighted.mean(x = leaks_sqkmC2, w = Pop),
            wLeaksPerSqKmC3 = weighted.mean(x = leaks_sqkmC3, w = Pop)) %>% 
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
  # st_filter(., ng_service_areas) %>%
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
  # st_filter(., ng_service_areas) %>%
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

# create a bar plot of total leaks with error bars
raceLeakDensityJoined %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKm), y = wLeaksPerSqKm)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted leak density (leaks/SqKm)") +
  theme_minimal() +
  geom_errorbar(aes(ymin = wLeaksPerSqKmUC, ymax = wLeaksPerSqKmLC))

# create a bar plot of Class 3 leaks
raceLeakDensityJoined %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKmC3), y = wLeaksPerSqKmC3)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted leak density (Class 3 leaks/SqKm)") +
  theme_minimal() 

# create a bar plot of Class 2 leaks
raceLeakDensityJoined %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKmC2), y = wLeaksPerSqKmC2)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted leak density (Class 2 leaks/SqKm)") +
  theme_minimal() 

# create a bar plot of Class 1 leaks
raceLeakDensityJoined %>% 
  ggplot(aes(x = reorder(Group,wLeaksPerSqKmC1), y = wLeaksPerSqKmC1)) + 
  geom_col() + coord_flip() + xlab("") + 
  ylab("Population-weighted leak density (Class 1 leaks/SqKm)") +
  theme_minimal() 


# create a facet wrap bar graph by leak grade and ordered by leaks
raceLeakDensityJoined %>% 
  select(-(ends_with("LC") | ends_with("UC"))) %>% 
  pivot_longer(wLeaksPerSqKm:wLeaksPerSqKmC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKm" = "All Leaks",
                        "wLeaksPerSqKmC1" = "Class 1 Leaks",
                        "wLeaksPerSqKmC2" = "Class 2 Leaks",
                        "wLeaksPerSqKmC3" = "Class 3 Leaks"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal() +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", sep = "")),
       title = "Race and Gas Leaks across Massachusetts")

# Compare leak density distribution by utility
# National Grid; includes both NG-Boston and NG-Colonial Gas
raceLeakDensityNG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^National") | 
           GAS == "Colonial Gas") %>% # limit to BGs in NG/Colonial svc area
  mutate(leaks_sqkmNG = (`National Grid - Boston Gas_19unrepaired` +
                           `National Grid - Colonial Gas_19unrepaired`)/area_sqkm) %>% 
  select(ends_with("_E"), (starts_with("National") & ends_with("ed")), 
         leaks_sqkmNG) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
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
                        "totalpop_E" = "Total Population"))

# Eversource Energy
raceLeakDensityEV <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Eversource") | 
           str_detect(GAS, "Energy$")) %>% # limit to BGs in Eversource svc area
  mutate(leaks_sqkmEV = (`Eversource Energy_19unrepaired`)/area_sqkm) %>% 
  select(ends_with("_E"), leaks_sqkmEV) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
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
                        "totalpop_E" = "Total Population"))

# Columbia Gas
raceLeakDensityCG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Columbia") | 
           str_detect(GAS, "Columbia Gas$")) %>% # limit to BGs in CG svc area
  mutate(leaks_sqkmCG = (`Columbia Gas_19unrepaired`)/area_sqkm) %>% 
  select(ends_with("_E"), leaks_sqkmCG) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
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
                        "totalpop_E" = "Total Population"))

# Fitchburg Gas aka Unitil
raceLeakDensityFG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(str_detect(GAS, "^Unitil") | 
           str_detect(GAS, "Unitil$")) %>% # limit to BGs in FG svc area
  mutate(leaks_sqkmFG = (`Fitchburg Gas_19unrepaired`)/area_sqkm) %>% 
  select(ends_with("_E"), leaks_sqkmFG) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmFG = weighted.mean(x = leaks_sqkmFG, w = Pop)) %>% 
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

# Liberty Utilities
raceLeakDensityLU <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(GAS == "Liberty Utilities") %>% # limit to BGs in LU svc area
  mutate(leaks_sqkmLU = (`Liberty Utilities_19unrepaired`)/area_sqkm) %>% 
  select(ends_with("_E"), leaks_sqkmLU) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmLU = weighted.mean(x = leaks_sqkmLU, w = Pop)) %>% 
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

# The Berkshire Gas Company
raceLeakDensityBG <- ma_blkgrps18 %>% 
  as.data.frame() %>% 
  filter(GAS == "The Berkshire Gas Company") %>% # limit to BGs in BG svc area
  mutate(leaks_sqkmBG = (`Berkshire Gas_19unrepaired`)/area_sqkm) %>% 
  select(ends_with("_E"), leaks_sqkmBG) %>% 
  pivot_longer(., cols = ends_with("_E"), names_to = "Group", 
               values_to = "Pop", values_drop_na = TRUE) %>% 
  group_by(Group) %>% 
  summarize(wLeaksPerSqKmBG = weighted.mean(x = leaks_sqkmBG, w = Pop)) %>% 
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

# Join the utility df together
raceLeakDensityJoinedU <- list(raceLeakDensityBG,
                               raceLeakDensityCG,
                               raceLeakDensityEV,
                               raceLeakDensityFG,
                               raceLeakDensityLU,
                               raceLeakDensityNG) %>% 
  reduce(left_join, by = "Group")


# Facet wrap by utility
raceLeakDensityJoinedU %>% 
  pivot_longer(wLeaksPerSqKmBG:wLeaksPerSqKmNG, 
               names_to = "Utility", values_to = "leakDensity") %>% 
  drop_na(leakDensity) %>% 
  mutate(Utility = recode(Utility, "wLeaksPerSqKmBG" = "Berkshire Gas",
                            "wLeaksPerSqKmCG" = "Columbia Gas",
                            "wLeaksPerSqKmEV" = "Eversource Energy",
                            "wLeaksPerSqKmUN" = "Unitil/Fitchburg Gas",
                          "wLeaksPerSqKmLU" = "Liberty Utilities",
                          "wLeaksPerSqKmNG" = "National Grid"),
         Group = reorder_within(Group, leakDensity, Utility)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = Utility)) + 
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal() +
  facet_wrap(~ Utility, scales = "free") +
  labs(x = NULL, 
       y = expression(paste("Population-weighted mean leak density (leaks/", 
                            km^2, ")", sep = "")),
       title = "Race and Gas Leaks by Utility across Massachusetts")


# conduct a weighted t-test to determine if differences are statistically significant
library(weights)
wtd.t.test(x = ma_blkgrps18$leaks_sqkm, y = ma_blkgrps18$leaks_sqkm, 
           weight = ma_blkgrps18$nhwhitepop_E, 
           weighty = ma_blkgrps18$nhblackpop_E, 
           bootse = TRUE, bootp = TRUE)



# correlation matrix between leak frequency and demographic group