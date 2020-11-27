# Analysis of relationship between gas leaks and demographics

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(ggplot2)

# Load demographic and gas leaks data
load("Data/Demographics.rds")
load("Data/HEET2019Leaks.rds")

# spatially aggregate leaks by block groups
blkgrp_unrepaired <- ma_blkgrps18 %>% 
  st_join(unrepaired2019final) %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019 = n()) %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  left_join(ma_blkgrps18, ., by = "GEOID")

# assign block group GEOID to each unrepaired leak that falls within it
unrepaired2019_toblkgrp <- ma_blkgrps18 %>% 
  select(GEOID) %>% 
  st_join(unrepaired2019final, .)

# create summary of unrepaired leaks by block group
# all leaks
blkgrp_total_unrepaired <- unrepaired2019_toblkgrp %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019total = n())

# class 1 leaks
blkgrp_class1_unrepaired <- unrepaired2019_toblkgrp %>% 
  as.data.frame() %>% 
  filter(Class == 1) %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019class1 = n())

# class 2 leaks
blkgrp_class2_unrepaired <- unrepaired2019_toblkgrp %>% 
  as.data.frame() %>% 
  filter(Class == 2) %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019class2 = n())

# class 3 leaks
blkgrp_class3_unrepaired <- unrepaired2019_toblkgrp %>% 
  as.data.frame() %>% 
  filter(Class == 3) %>% 
  group_by(GEOID) %>% 
  summarize(unrepaired2019class3 = n())

# join unrepaired leak counts to block group layer
ma_blkgrps18 <- ma_blkgrps18 %>% 
  left_join(., blkgrp_total_unrepaired, by = "GEOID") %>% 
  left_join(., blkgrp_class1_unrepaired, by = "GEOID") %>%
  left_join(., blkgrp_class2_unrepaired, by = "GEOID") %>%
  left_join(., blkgrp_class3_unrepaired, by = "GEOID") %>%
  mutate(area_sqkm = as.numeric(st_area(.)/10^6),
         leaks_sqkm = if_else(unrepaired2019total > 0, 
                              unrepaired2019total/area_sqkm, 0)) %>% 
  replace_na(list(unrepaired2019total = 0, unrepaired2019class1= 0, 
                  unrepaired2019class2 = 0, unrepaired2019class3 = 0, 
                  leaks_sqkm = 0))


# comparison of leak frequency and density by demographic group


# correlation matrix between leak frequency and demographic group