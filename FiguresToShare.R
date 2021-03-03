# Figures for sharing with EOS magazine

library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)
library(ggplot2)
library(foreign) # for reading in dbf
library(tidytext) # for reordering within ggplot2 facets
library(kableExtra)
library(sp)
library(spdep)
library(tigris)

# Load demographic and gas leaks data
load("Data/Demographics.rds")
load("Data/HEET2019Leaks.rds")
ma_blkgrps <- readRDS("Data/ma_blkgrpsMEAN.Rds")
ppLeakDensityJoined <- readRDS("Data/ppLeakDensityAVG.Rds")
ppLeakDensity_df <- readRDS("Data/ppLeakDensity_df_blkgrps.Rds")
ppLeakDensityJoinedU <- readRDS("Data/ppLeakDensityJoinedU_BG.Rds")

# graphs of disparity in leak exposure
# faceted bar graph of unrepaired leak density by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerSqKm:wLeaksPerSqKmC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>% 
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKm" = "All Leaks",
                            "wLeaksPerSqKmC1" = "Class 1 Leaks (high hazard)",
                            "wLeaksPerSqKmC2" = "Class 2 Leaks (med hazard)",
                            "wLeaksPerSqKmC3" = "Class 3 Leaks (low hazard)"),
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
       title = "Priority Populations and Unrepaired Gas Leaks in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClass_blkgrp.png", dpi = 600)


# faceted dot graph of RR for unrepaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksRR:wLeaksRRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>% 
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksRR" = "All Leaks",
                            "wLeaksRRC1" = "Class 1 Leaks (high hazard)",
                            "wLeaksRRC2" = "Class 2 Leaks (med hazard)",
                            "wLeaksRRC3" = "Class 3 Leaks (low hazard)"),
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
                            km^2, ")", " to total population-weighted mean by Census Block Group",sep = "")),
       title = "Relative Risk of Priority Populations and Unrepaired Gas Leaks in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClassRR_blkgrp.png", dpi = 600)


# faceted bar graph of total leak density by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerSqKmALL:wLeaksPerSqKmALLC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>%
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerSqKmALL" = "All Leaks",
                            "wLeaksPerSqKmALLC1" = "Class 1 Leaks (high hazard)",
                            "wLeaksPerSqKmALLC2" = "Class 2 Leaks (med hazard)",
                            "wLeaksPerSqKmALLC3" = "Class 3 Leaks (low hazard)"),
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
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClassAll_blkgrp.png", dpi = 600)

# faceted dot graph of RR for repaired leak density around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksRRtotal:wLeaksRRtotalC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>%
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksRRtotal" = "All Leaks",
                            "wLeaksRRtotalC1" = "Class 1 Leaks (high hazard)",
                            "wLeaksRRtotalC2" = "Class 2 Leaks (med hazard)",
                            "wLeaksRRtotalC3" = "Class 3 Leaks (low hazard)"),
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
                            km^2, ")", " to total population-weighted mean by Census Block Group",sep = "")),
       title = "Relative Risk of Priority Populations and Total Gas Leaks (unrepaired + repaired) in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClassAllRR_blkgrp.png", dpi = 600)


# faceted bar graph of unrepaired leaks per occupied housing unit by leak class
ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerHU:wLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>%
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerHU" = "All Leaks",
                            "wLeaksPerHUC1" = "Class 1 Leaks (high hazard)",
                            "wLeaksPerHUC2" = "Class 2 Leaks (med hazard)",
                            "wLeaksPerHUC3" = "Class 3 Leaks (low hazard)"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean unrepaired leaks per occupied housing unit by Census Block Group",
       title = "Priority Populations and Unrepaired Gas Leaks Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClass_HU_blkgrp.png", dpi = 600)

# faceted dot graph of RR for unrepaired per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wLeaksPerHURR:wLeaksPerHURRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>%
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wLeaksPerHURR" = "All Leaks",
                            "wLeaksPerHURRC1" = "Class 1 Leaks (high hazard)",
                            "wLeaksPerHURRC2" = "Class 2 Leaks (med hazard)",
                            "wLeaksPerHURRC3" = "Class 3 Leaks (low hazard)"),
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
       y = "Ratio of group population-weighted mean leaks per occupied housing unit to total population-weighted mean by Census Block Group",
       title = "Relative Risk of Priority Populations and Unrepaired Leaks Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClass_HU_RR_blkgrp.png", dpi = 600)


# faceted bar graph of total leaks per occupied housing unit by leak class
ppLeakDensity_df %>% 
  pivot_longer(wALLLeaksPerHU:wALLLeaksPerHUC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>%
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHU" = "All Leaks",
                            "wALLLeaksPerHUC1" = "Class 1 Leaks (high hazard)",
                            "wALLLeaksPerHUC2" = "Class 2 Leaks (med hazard)",
                            "wALLLeaksPerHUC3" = "Class 3 Leaks (low hazard)"),
         Group = reorder_within(Group, leakDensity, leakClass)) %>% 
  ggplot(aes(x = Group, y = leakDensity, fill = leakClass)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  scale_x_reordered() +
  theme_minimal(base_size = 6) +
  facet_wrap(~ leakClass, scales = "free") +
  labs(x = NULL, 
       y = "Population-weighted mean total leaks per occupied housing unit by Census Block Group",
       title = "Priority Populations and Total Gas Leaks (unrepaired + repaired) Per Occupied Housing Unit in 2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClassAll_HU_blkgrp.png", dpi = 600)

# faceted dot graph of RR for total per HU around zero line
cols <- c("#F7A35C", "#7CB5EC", "gray30")

ppLeakDensity_df %>% 
  pivot_longer(wALLLeaksPerHURR:wALLLeaksPerHURRC3, 
               names_to = "leakClass", values_to = "leakDensity") %>% 
  filter(!Group %in% c("MA_ENGLISH","MA_MINORITY21","MA_INCOME21")) %>%
  # mutate(Group = recode(Group, "MA_ENGLISH" = "MA Limited English HH",
  #                       "MA_MINORITY21" = "MA Minority",
  #                       "MA_INCOME21" = "MA Low Income")) %>%
  mutate(leakClass = recode(leakClass, "wALLLeaksPerHURR" = "All Leaks",
                            "wALLLeaksPerHURRC1" = "Class 1 Leaks (high hazard)",
                            "wALLLeaksPerHURRC2" = "Class 2 Leaks (med hazard)",
                            "wALLLeaksPerHURRC3" = "Class 3 Leaks (low hazard)"),
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
       y = "Ratio of group population-weighted mean leaks per occupied housing unit to total population-weighted mean by Census Block Group",
       title = "Relative Risk of Priority Populations and Total Leaks (unrepaired + repaired) Per Occupied Housing Unit in\n2019 across Massachusetts", caption = "Gas leak data from Massachusetts Dept of Public Utilities and geocoded by HEET. Population data from 5-year ACS 2015-19 estimates.")

ggsave("Images/SHARE/LeaksPPbyClassAll_HU_RR_blkgrp.png", dpi = 600)


# maps of leaks
# create a hexagonal grid and create index column
gridCnt <- st_make_grid(x = ma_blkgrps, cellsize = 1000, square = FALSE) %>% 
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
  crop_shape(., ma_blkgrps, polygon = TRUE) %>% 
  st_make_valid()

# context for maps
# grab neighboring state boundaries for context
ne_states_sf_cb <- states(cb = TRUE) %>% 
  filter(STUSPS %in% c("CT","RI","NY","NH","VT","ME"))

# separate MA for cropping
ma_state_sf <- states(cb = TRUE) %>% 
  filter(STUSPS == "MA")

# grab municipal boundaries
ma_towns_sf <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  st_transform(., crs = 26986)

# create point layer of towns for context
ma_towns_sf_pts <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME %in% c("Boston",
                     "Lawrence",
                     "Lowell",
                     "Brockton",
                     "New Bedford",
                     "Worcester",
                     "Springfield",
                     "Pittsfield",
                     "Fitchburg",
                     "Stockbridge",
                     "Fall River",
                     "West Yarmouth",
                     "Lynn",
                     "Randolph",
                     "Webster",
                     "Attleboro",
                     "Medford",
                     "Sturbridge",
                     "Quincy",
                     "Weymouth")) %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# create a separate point for Eastham so that it can be repositioned
newton <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME == "Newton") %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
ma_highways <- primary_roads() %>% 
  filter(FULLNAME %in% c("I- 84","I- 90","I- 91","I- 95","I- 190","I- 195","I- 290","I- 395","I- 495","US Hwy 6","US Hwy 202","Mohawk Trl","George W Stanton Hwy","State Rte 2","Mass State Hwy","Concord Tpke","State Rte 25")) %>% 
  tmaptools::crop_shape(., ma_state_sf, polygon = TRUE) %>% 
  st_transform(., crs = 26986)

ma_highways2nd <- primary_secondary_roads("MA") %>% 
  filter(FULLNAME %in% c("US Hwy 6","Mohawk Trl","State Rte 2","Cambridge Tpke")) %>% 
  st_transform(., crs = 26986)

# Extract highway segments for labeling
I90roadSegment <- ma_highways %>% 
  filter(LINEARID == "1103745154991")

I90roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "110340769311")

I91roadSegment <- ma_highways %>% 
  filter(LINEARID == "1104748241453")

I95roadSegment <- ma_highways %>% 
  filter(LINEARID == "1105569136116")

I95roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "1103737956638")

I195roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "1101922014382")

I395roadSegment <- ma_highways %>% 
  filter(LINEARID == "1104259933162")

I495roadSegment <- ma_highways %>% 
  filter(LINEARID == "1103745404033")

I495roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "1105589457557")

I495roadSegment3 <- ma_highways %>% 
  filter(LINEARID == "1101922014436")

StRt2Segment <- ma_highways2nd %>% 
  filter(LINEARID == "1106087431756")

USHwy6Segment <- ma_highways %>% 
  filter(LINEARID == "1109096413415")

# Create custom icons of highway shields
I90 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/I-90.svg/200px-I-90.svg.png")
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")
I395 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/I-395.svg/200px-I-395.svg.png")
I91 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/I-91.svg/200px-I-91.svg.png")
I495 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/I-495.svg/200px-I-495.svg.png")
Hwy2 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/5/54/MA_Route_2.svg/240px-MA_Route_2.svg.png")
Hwy6 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/US_6.svg/200px-US_6.svg.png")

# map out counts of leaks per hexagons > 0 
m_gridUnrepaired <- gridCnt1 %>% 
  filter(unrepaired > 0) %>%
  tm_shape(., unit = "mi", bbox = ma_state_sf) + 
  tm_fill(col = "unrepaired", palette = "YlOrRd",
                        style = "fisher",
                        # breaks = c(1,5,10,20,40,80), 
                        legend.format = list(digits = 0),
          title = "# leaks") +
  # tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_shape(ne_states_sf_cb) + 
  tm_polygons(col = "gray90", border.col = "white") +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_state_sf) + tm_borders(lwd = 0.5, alpha = 0.8) +
  # tm_shape(ma_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  # tm_shape(ma_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  # tm_shape(I95roadSegment) +
  # tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  # tm_shape(I95roadSegment2) +
  # tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  # tm_shape(I395roadSegment) +
  # tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  # tm_shape(I91roadSegment) +
  # tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  # tm_shape(I495roadSegment) +
  # tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  # tm_shape(I495roadSegment2) +
  # tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  # tm_shape(I495roadSegment3) +
  # tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  # tm_shape(I90roadSegment) +
  # tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  # tm_shape(I90roadSegment2) +
  # tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Massachusetts\nUnrepaired Gas\nLeaks 2019",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))
tmap_save(m_gridUnrepaired, filename = "Images/SHARE/m_gridUnrepaired.png", 
          dpi = 600)

m_gridTotal <- gridCnt1 %>% 
  filter(total > 0) %>%
  tm_shape(.) + 
  tm_fill(col = "total", palette = "YlOrRd",
          legend.format = list(digits = 0), 
          style = "fisher",
          # breaks = c(1,5,10,20,40,80),
          legend.hist = TRUE)
tmap_save(m_gridTotal, filename = "Images/m_gridTotal.png", dpi = 600)

# map out leak densities by blockgroup
ma_blkgrps %>% 
  filter(AllLeaks2019_sqkm > 0) %>% 
  tm_shape(.) + tm_fill(col = "AllLeaks2019_sqkm", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

ma_blkgrps %>% 
  filter(leaks_sqkm > 0) %>% 
  tm_shape(.) + tm_fill(col = "leaks_sqkm", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

# leask per OHU
ma_blkgrps %>% 
  filter(ALLleaks_hu > 0) %>% 
  tm_shape(.) + tm_fill(col = "ALLleaks_hu", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

ma_blkgrps %>% 
  filter(leaks_hu > 0) %>% 
  tm_shape(.) + tm_fill(col = "leaks_hu", style = "fisher", 
                        legend.format = list(digits = 0), 
                        legend.hist = TRUE)

# create a smoothed raster of leak density
library(SpatialKDE)
grid <- create_grid_hexagonal(ma_blkgrps, cell_size = 1000)

Unrepaired_kde_grid3k <- kde(unrepaired2019final, band_width = 3000, grid = grid)
m_UnrepairedKDE3k <- Unrepaired_kde_grid3k %>% 
  crop_shape(., ma_blkgrps, polygon = TRUE) %>% 
  st_make_valid() %>% 
  tm_shape(.) + tm_fill(col = "kde_value", style = "fisher")
m_UnrepairedKDE3k
# save the map for comparison
tmap_save(m_UnrepairedKDE3k, filename = "Images/m_UnrepairedKDE3k.png", dpi = 600)

# all leak points
total2019 <- rbind(st_as_sf(st_geometry(unrepaired2019final)), 
                   st_as_sf(st_geometry(repaired2019final)))

total_kde_grid3k <- kde(total2019, band_width = 3000, grid = grid)
m_TotalKDE3k <- total_kde_grid3k %>% 
  # filter(kde_value > 0) %>% 
  crop_shape(., ma_blkgrps, polygon = TRUE) %>% 
  st_make_valid() %>% 
  tm_shape(.) + tm_fill(col = "kde_value", style = "fisher")
m_TotalKDE3k
# save the map for comparison
tmap_save(m_TotalKDE3k, filename = "Images/m_TotalKDE3k.png", dpi = 600)