# create a shp to share with GIS Director of DEP Kashif Rashid
temp <- readRDS("Data/ma_blkgrps2019.Rds")

ma_blkgrps_EJ <- ma_blkgrps %>% 
  select(GEOID, TOWN, totalpopE, totalpopM, MA_INCOME21, MA_MINORITY21, MA_ENGLISH, unrepaired2019total, LeakAgeDaysAvg) %>% 
  separate(TOWN, c("Town","County","State"), sep = ", ")



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

ma_tracts <- readRDS("Data/ma_tractsMEAN.Rds")
ma_cosub <- readRDS("Data/ma_cosubMEAN.Rds")

# table of leak stats by class and repair status
unrepaired_Sumdf <- unrepaired2019final %>% as.data.frame() %>% 
  group_by(Class) %>% summarize(Unrepaired = n())

repaired_Sumdf <- repaired2019final %>% as.data.frame() %>% 
  group_by(Class) %>% summarize(Repaired = n())

total_Sumdf <- left_join(unrepaired_Sumdf, repaired_Sumdf, by = "Class") %>% 
  drop_na() %>%
  mutate(Total = Unrepaired + Repaired) %>% 
  bind_rows(summarize(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total"))) %>% 
  mutate(PctUnrepaired = Unrepaired/Total*100,
         PctRepaired = Repaired/Total*100,
         PctTotal = Total/Total[Class == "Total"]*100) %>%
  select(Class, Unrepaired, PctUnrepaired, Repaired, PctRepaired, 
         Total, PctTotal)

total_Sumdf %>% 
  mutate(PctUnrepaired = paste0(round(PctUnrepaired,1),"%"),
         PctRepaired = paste0(round(PctRepaired,1),"%"),
         PctTotal = paste0(round(PctTotal,1),"%")) %>% 
  kable(., longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Gas leaks by class and repair status, 2019", align = "r", 
        # digits = c(0,0,1,0,1,0,1), 
        col.names = c("Class","Count","Percent","Count","Percent",
                      "Total","Pct Total")) %>% 
  add_header_above(., c(" ", "Unrepaired Leaks" = 2, "Repaired Leaks" = 2, 
                        " " = 2)) %>% 
  row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))


# Map of leak counts by hexagons and utility territories
# Load natural gas utility service areas dbf from MassGIS and join to MassGIS towns layer
ng_dbf <- read.dbf("Data/pubutil/TOWNS_POLY_UTILITIES.dbf")

ng_service_areas <- st_read(dsn = "Data/townssurvey_shp",
                            layer = "TOWNSSURVEY_POLYM") %>% 
  select(-TOWN) %>% 
  left_join(., ng_dbf, by = "TOWN_ID") %>% 
  select(TOWN, GAS, GAS_LABEL) %>%  
  st_transform(., crs = st_crs(ma_blkgrps)) %>% 
  st_make_valid()

# isolate No gas areas and municipal
ng_nogas_muni <- ng_service_areas %>% 
  filter(GAS_LABEL %in% c("No Natural Gas Service","Municipal")) %>% 
  group_by(GAS_LABEL) %>%
  summarize() %>% 
  mutate(GAS_LABEL = as.character(GAS_LABEL))

# consolidate into basic gas service districts
ng_service_areas2 <- ng_service_areas %>% 
  filter(!GAS_LABEL %in% c("No Natural Gas Service","Municipal")) %>% 
  group_by(GAS_LABEL) %>% 
  summarize(TownCnt = n()) %>% 
  mutate(ID = case_when(
    GAS_LABEL == "National Grid" ~ "NG",
    GAS_LABEL == "Blackstone Gas Company" ~ "BGC",
    GAS_LABEL == "Columbia Gas" ~ "CG",
    GAS_LABEL == "Eversource Energy" ~ "EV",
    GAS_LABEL == "Columbia Gas, Eversource Energy" ~ "CG,EV",
    GAS_LABEL == "Unitil" ~ "UN",
    GAS_LABEL == "National Grid, Unitil" ~ "NG,UN",
    GAS_LABEL == "Eversource Energy, National Grid" ~ "EV,NG",
    GAS_LABEL == "The Berkshire Gas Company" ~ "BG",
    GAS_LABEL == "Columbia Gas, Blackstone Gas Company" ~ "CG,BGC",
    GAS_LABEL == "Liberty Utilities" ~ "LU",
    GAS_LABEL == "Colonial Gas" ~ "NG",
    GAS_LABEL == "Columbia Gas, National Grid" ~ "CG,NG"
  ))

# join ID to original municipalities within ng_service_area to use centroids as labels; restrict to central municipalities within territories
ng_service_labels <- ng_service_areas2 %>% 
  as.data.frame() %>% 
  select(ID, GAS_LABEL) %>% 
  inner_join(ng_service_areas, ., by = "GAS_LABEL") %>% 
  filter(TOWN %in% c("LANESBOROUGH","DEERFIELD","NORTHAMPTON","LUDLOW",
                     "NORTH BROOKFIELD","OXFORD",
                     "WESTMINSTER","CARLISLE","ANDOVER","WESTBOROUGH",
                     "BLACKSTONE","MILTON",
                     "NORWOOD","WEST BRIDGEWATER","PLAINVILLE","PLYMOUTH",
                     "WESTPORT","ACUSHNET","WAREHAM","BARNSTABLE",
                     "SWANSEA","MARSHFIELD","COHASSET",
                     "HAMILTON","WEST NEWBURY","DOVER","DUNSTABLE")) %>% 
  st_centroid(., of_largest_polygon = TRUE)

ng_service_labels2 <- ng_service_areas2 %>% 
  as.data.frame() %>% 
  select(ID, GAS_LABEL) %>% 
  inner_join(ng_service_areas, ., by = "GAS_LABEL") %>% 
  filter(TOWN %in% c("LEICESTER","LUNENBURG","MENDON","BELLINGHAM","HANSON",
                     "WAYLAND","NATICK","BOSTON","SOMERVILLE")) %>% 
  st_centroid(., of_largest_polygon = TRUE)

ng_service_labels3 <- ng_service_areas2 %>% 
  as.data.frame() %>% 
  select(ID, GAS_LABEL) %>% 
  inner_join(ng_service_areas, ., by = "GAS_LABEL") %>% 
  filter(TOWN == "BOSTON") %>% 
  st_centroid(., of_largest_polygon = TRUE)


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
gridCnt <- gridCnt %>% 
  mutate(total = unrepaired + repaired) %>% 
  crop_shape(., ma_blkgrps, polygon = TRUE) %>% 
  st_make_valid()

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
                     "Worcester",
                     "Springfield",
                     "Pittsfield", 
                     "Stockbridge",
                     "Fall River",
                     "West Yarmouth",
                     "Lynn",
                     "Randolph",
                     "Webster",
                     "Attleboro",
                     "Medford",
                     "Amherst",
                     "Quincy",
                     "Weymouth Town",
                     "Nantucket")) %>% 
  mutate(NAME = recode(NAME, "Weymouth Town" = "Weymouth")) %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# create a separate point for Newton so that it can be repositioned
newton <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME %in% c("Fitchburg","Newton","Edgartown","New Bedford")) %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# create layer of unrepaired hexagons > 0
gridCnt1 <- gridCnt %>% 
  filter(unrepaired > 0)

m_gridUnrepaired <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") +
  tm_shape(gridCnt1, unit = "km", bbox = ma_state_sf) + 
  tm_fill(col = "unrepaired", palette = "YlOrRd",
          style = "fisher",
          # breaks = c(1,5,10,20,40,80), 
          legend.format = list(digits = 0),
          title = "Number\nof leaks") +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Unrepaired Gas Leaks and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m_gridUnrepaired, filename = "Images/SHARE2/m_gridUnrepairedUtil.png", 
          dpi = 600)



# create layer of total leaks by hexagons > 0
gridCntT <- gridCnt %>% 
  filter(total > 0)

m_gridTotal <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") +
  tm_shape(gridCnt1, unit = "km", bbox = ma_state_sf) + 
  tm_fill(col = "total", palette = "YlOrRd",
          style = "fisher",
          # breaks = c(1,5,10,20,40,80), 
          legend.format = list(digits = 0),
          title = "Number\nof leaks") +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Gas Leaks and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m_gridTotal, filename = "Images/SHARE2/m_gridTotalUtil.png", 
          dpi = 600)


# create map of leak densities by BG
blkgrpsLeaksAll <- ma_blkgrps %>% 
  filter(AllLeaks2019_sqkm > 0)

m1 <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") + 
  tm_shape(blkgrpsLeaksAll) + tm_fill(col = "AllLeaks2019_sqkm", style = "fisher",
                                      palette = "YlOrRd", 
                                      title = "Leaks per SqKm",
                                      legend.format = list(digits = 2)) +
  tm_shape(ma_blkgrps) + tm_borders(lwd = 0.01, alpha = 0.8) +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Gas Leak Density by Census Block Group and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m1, filename = "Images/SHARE2/m_blkgrpDensityUtil_lines.png", 
          dpi = 600)


# create map of leaks per OHU by BG
blkgrpsLeaksOHU <- ma_blkgrps %>% 
  filter(ALLleaks_hu > 0)

m1b <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") + 
  tm_shape(blkgrpsLeaksOHU) + tm_fill(col = "ALLleaks_hu", style = "fisher",
                                      palette = "YlOrRd", 
                                      title = "Leaks per\nOccupied\nHousing\nUnit",
                                      legend.format = list(digits = 2)) +
  tm_shape(ma_blkgrps) + tm_borders(lwd = 0.01, alpha = 0.8) +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Gas Leaks per Occupied Housing Unit by Census Block Group and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m1b, filename = "Images/SHARE2/m_blkgrpOHUUtil_lines.png", 
          dpi = 600)


# create map of leak densities by Tract
tractsLeaksAll <- ma_tracts %>% 
  filter(AllLeaks2019_sqkm > 0)

m2 <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") + 
  tm_shape(tractsLeaksAll) + tm_fill(col = "AllLeaks2019_sqkm", style = "fisher",
                                      palette = "YlOrRd", 
                                      title = "Leaks per SqKm",
                                      legend.format = list(digits = 2)) +
  tm_shape(ma_tracts) + tm_borders(lwd = 0.01, alpha = 0.8) +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Gas Leak Density by Census Tract and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m2, filename = "Images/SHARE2/m_tractDensityUtil_lines.png", 
          dpi = 600)


# create map of leak densities by CoSub
cosubLeaksAll <- ma_cosub %>% 
  filter(AllLeaks2019_sqkm > 0)

m3 <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") + 
  tm_shape(cosubLeaksAll) + tm_fill(col = "AllLeaks2019_sqkm", style = "fisher",
                                     palette = "YlOrRd", 
                                     title = "Leaks per SqKm",
                                     legend.format = list(digits = 2)) +
  tm_shape(ma_cosub) + tm_borders(lwd = 0.01, alpha = 0.8) +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Gas Leak Density by Municipality and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m3, filename = "Images/SHARE2/m_cosubDensityUtil_lines.png", dpi = 600)


# table of leak count stats by block group
# list of stats to compute across
summary_stats <- list(Min = ~min(., na.rm = T),
                      Med = ~median(., na.rm = T),
                      Avg = ~mean(., na.rm = T),
                      Max = ~max(., na.rm = T))
# table of stats by block group
unrep_blkgrp_cnt <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(starts_with("unrepaired")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>% 
  group_by(Class) %>% 
  summarize(across(.cols = Count, summary_stats, 
                             .names = "{.fn}U")) %>% 
  mutate(Class = recode(Class, "unrepaired2019total" = "All",
                        "unrepaired2019totalC1" = "1",
                        "unrepaired2019totalC2" = "2",
                        "unrepaired2019totalC3" = "3")) %>% 
  arrange(Class)

rep_blkgrp_cnt <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(starts_with("repaired")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>% 
  group_by(Class) %>% 
  summarize(across(.cols = Count, summary_stats, 
                   .names = "{.fn}R")) %>% 
  mutate(Class = recode(Class, "repaired2019total" = "All",
                        "repaired2019totalC1" = "1",
                        "repaired2019totalC2" = "2",
                        "repaired2019totalC3" = "3")) %>% 
  arrange(Class)

all_blkgrp_cnt <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(AllLeaks2019:AllLeaks2019C3) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>% 
  group_by(Class) %>% 
  summarize(across(.cols = Count, summary_stats, 
                   .names = "{.fn}T")) %>% 
  mutate(Class = recode(Class, "AllLeaks2019" = "All",
                        "AllLeaks2019C1" = "1",
                        "AllLeaks2019C2" = "2",
                        "AllLeaks2019C3" = "3")) %>% 
  arrange(Class)

# join together
list(unrep_blkgrp_cnt,rep_blkgrp_cnt,all_blkgrp_cnt) %>% 
  reduce(., left_join, by = "Class") %>% 
  kable(., longtable = T, booktabs = T,
        # format.args = list(big.mark = ','), 
        caption = "Gas leak counts by class per Census Block Group, 2019", 
        align = "r", 
        digits = c(0,0,0,1,0,0,0,1,0,0,0,1),
        col.names = c("Class","Min","Med","Avg","Max",
                      "Min","Med","Avg","Max",
                      "Min","Med","Avg","Max")) %>% 
  add_header_above(., c(" ", "Unrepaired Leaks" = 4, "Repaired Leaks" = 4, 
                        "Total Leaks" = 4)) %>% 
  # row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))



# table of leak density stats by block group
# list of stats to compute across
summary_stats <- list(Min = ~min(., na.rm = T),
                      Med = ~median(., na.rm = T),
                      Avg = ~mean(., na.rm = T),
                      Max = ~max(., na.rm = T))
# table of stats by block group
unrep_blkgrp_dns <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(starts_with("leaks_sqkm")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>% 
  group_by(Class) %>% 
  summarize(across(.cols = Count, summary_stats, 
                   .names = "{.fn}U")) %>% 
  mutate(Class = recode(Class, "leaks_sqkm" = "All",
                        "leaks_sqkmC1" = "1",
                        "leaks_sqkmC2" = "2",
                        "leaks_sqkmC3" = "3")) %>% 
  arrange(Class)

rep_blkgrp_dns <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(starts_with("REPleaks_sqkm")) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>% 
  group_by(Class) %>% 
  summarize(across(.cols = Count, summary_stats, 
                   .names = "{.fn}R")) %>% 
  mutate(Class = recode(Class, "REPleaks_sqkm" = "All",
                        "REPleaks_sqkmC1" = "1",
                        "REPleaks_sqkmC2" = "2",
                        "REPleaks_sqkmC3" = "3")) %>% 
  arrange(Class)

all_blkgrp_dns <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(AllLeaks2019_sqkm:AllLeaks2019C3_sqkm) %>% 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>% 
  group_by(Class) %>% 
  summarize(across(.cols = Count, summary_stats, 
                   .names = "{.fn}T")) %>% 
  mutate(Class = recode(Class, "AllLeaks2019_sqkm" = "All",
                        "AllLeaks2019C1_sqkm" = "1",
                        "AllLeaks2019C2_sqkm" = "2",
                        "AllLeaks2019C3_sqkm" = "3")) %>% 
  arrange(Class)

# join together
list(unrep_blkgrp_dns,rep_blkgrp_dns,all_blkgrp_dns) %>% 
  reduce(., left_join, by = "Class") %>% 
  kable(., longtable = T, booktabs = T,
        # format.args = list(big.mark = ','), 
        caption = "Gas leak density (per sqkm) by class per Census Block Group, 2019", 
        align = "r", 
        digits = 2,
        col.names = c("Class","Min","Med","Avg","Max",
                      "Min","Med","Avg","Max",
                      "Min","Med","Avg","Max")) %>% 
  add_header_above(., c(" ", "Unrepaired Leaks" = 4, "Repaired Leaks" = 4, 
                        "Total Leaks" = 4)) %>% 
  # row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))


# Dunn's Test tables
read_csv("Tables/BG/pwdt_leaks_hu.csv") %>% 
  select(-.y.) %>% 
  mutate(group1 = recode(group1, "nhasianpop_E" = "Asian",
                         "nhblackpop_E" = "Black",
                         "minority_E" = "People of Color",
                         "over64E" = "Over 64",
                         "MA_MINORITY21"= "MA Minority",
                         "totalpopE" = "Total Pop",
                         "nhwhitepop_E" = "White",
                         "eng_limitE" = "Limited English HH",
                         "lthsE" = "No HS Dip",
                         "renter_occ_unitsE" = "Renter Occupied HU",
                         "hisppop_E" = "Hispanic",
                         "num2povE" = "Low Income",
                         "MA_ENGLISH" = "MA Limited English HH",
                         "MA_INCOME21" = "MA Low Income"),
         group2 = recode(group2, "under5E" = "Under 5",
                         "totalpopE" = "Total Pop",
                         "total_occ_unitsE" = "Total Occupied HU")) %>% 
  kable(., longtable = T, booktabs = T,
        format.args = list(big.mark = ','),
        caption = "Dunn's Test for Unrepaired Leaks per Occupied Housing Unit by Census Block Group", 
        align = "r", 
        digits = c(0,0,0,0,3,3,7,0)
        ) %>% 
  # add_header_above(., c(" ", "Unrepaired Leaks" = 4, "Repaired Leaks" = 4, 
  #                       "Total Leaks" = 4)) %>% 
  # row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))


# table of time to repair
# list of stats to compute across
summary_stats <- list(Min = ~min(., na.rm = T),
                      Med = ~median(., na.rm = T),
                      Avg = ~mean(., na.rm = T),
                      Max = ~max(., na.rm = T))

# time to repair by class
repaired_Timedf <- repaired2019final %>% 
  as.data.frame() %>% 
  select(Class,DaysToRepair) %>% 
  drop_na(Class) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = DaysToRepair, summary_stats, 
                   .names = "{.fn}")) %>% 
  arrange(Class)

# add row for all repaired
repaired_Timedf <- repaired2019final %>% 
  as.data.frame() %>% 
  select(DaysToRepair) %>% 
  summarize(across(.cols = DaysToRepair, summary_stats, 
                   .names = "{.fn}")) %>% 
  mutate(Class = "All", .before = Min) %>% 
  rbind(repaired_Timedf, .)

repaired_Timedf %>% kable(., longtable = T, booktabs = T,
                          format.args = list(big.mark = ','),
                          caption = "Leak repair time (days), 2019", 
                          align = "r", 
                          digits = 1) %>% 
  # add_header_above(., c(" ", "Unrepaired Leaks" = 4, "Repaired Leaks" = 4, 
  #                       "Total Leaks" = 4)) %>% 
  # row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))

# create a overlapping histogram of repair times
# timeHist <- repaired2019final %>% 
#   as.data.frame() %>% 
#   select(Class,DaysToRepair) %>% 
#   drop_na(Class) %>% 
#   ggpubr::gghistogram(., x = "DaysToRepair",
#             add = "median", rug = TRUE,
#             color = "Class", fill = "Class",
#             palette = c("#fc8d62","#66c2a5","#8da0cb"),
#             title = "Leak Repair Times by Leak Class in 2019",
#             xlab = "Days to Repair",
#             ylab = "Number of Repaired Leaks",
#             ggtheme = theme_minimal())
# 
# mu1 <- repaired2019final %>% 
#   as.data.frame() %>% 
#   drop_na(Class) %>% 
#   group_by(Class) %>% 
#   summarize(grp.med = median(DaysToRepair, na.rm = T))

repaired2019final %>% 
  as.data.frame() %>% 
  select(Class,DaysToRepair) %>% 
  drop_na(Class) %>% 
  ggplot(aes(x = DaysToRepair, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  geom_vline(data = mu1, aes(xintercept = grp.med, color = Class), 
             linetype = "dashed") +
  scale_color_manual(values = c("#fc8d62","#66c2a5","#8da0cb"),
                     guide = guide_legend(override.aes = list(fill = c("#fc8d62","#66c2a5","#8da0cb")))) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5","#8da0cb"),
                    guide = FALSE) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "Leak Repair Times by Leak Class in 2019",
       x = "Days to Repair",
       y = "Number of Repaired Leaks", fill = "",
       caption = "Column heights indicate number of leaks for a given age range. Colors correspond to leak class.\nDashed lines indicate median values by leak class. Rug plot at base shows individual values.") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) + geom_rug()

ggsave("Images/SHARE2/histRepairTime.png")

# ordered boxplot of time to repair by utility
repaired2019final %>% 
  as.data.frame() %>% 
  mutate(Utility = recode(Utility, "National Grid - Boston Gas" = "National Grid",
                          "National Grid - Colonial Gas" = "National Grid")) %>% 
  ggplot(aes(x = reorder(Utility, DaysToRepair, fun = median), 
             y = DaysToRepair, fill = Utility)) +
  # geom_violin(width=1.4) +
  # geom_boxplot(width=0.1, color="grey", alpha=0.2) + 
  geom_boxplot() + 
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position="none") +
  ggtitle("Leak Repair Times (days) by Utility in 2019") +
  xlab("") + ylab("Days to repair through Dec 31, 2019") + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ","))
# facet_wrap("Class", scales = "free")

ggsave(filename = "Images/SHARE2/boxplotLeaksByUtilityTime.png")

# create a map of average repair times by block group
blkgrpsLeaksTime <- ma_blkgrps %>% 
  filter(repaired2019total > 0)

m4 <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") + 
  tm_shape(blkgrpsLeaksTime) + tm_fill(col = "DaysToRepairAvg", style = "fisher",
                                      palette = "YlOrRd", 
                                      title = "Avg Days\nto Repair",
                                      legend.format = list(digits = 0),
                                      colorNA = NULL) +
  tm_shape(ma_blkgrps) + tm_borders(lwd = 0.01, alpha = 0.8) +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Mean Leak Repair Times by Census Block Group and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m4, filename = "Images/SHARE2/m_blkgrpTimeUtil_lines.png", 
          dpi = 600)


# table of leak age
# list of stats to compute across
summary_stats <- list(Min = ~min(., na.rm = T),
                      Med = ~median(., na.rm = T),
                      Avg = ~mean(., na.rm = T),
                      Max = ~max(., na.rm = T))

# unrepaired age by class
unrepaired_Timedf <- unrepaired2019final %>% 
  as.data.frame() %>% 
  select(Class,LeakAgeDays) %>% 
  drop_na(Class) %>% 
  group_by(Class) %>% 
  summarize(across(.cols = LeakAgeDays, summary_stats, 
                                       .names = "{.fn}")) %>% 
  arrange(Class)

# add row for all unrepaired
unrepaired_Timedf <- unrepaired2019final %>% 
  as.data.frame() %>% 
  select(LeakAgeDays) %>% 
  summarize(across(.cols = LeakAgeDays, summary_stats, 
                   .names = "{.fn}")) %>% 
  mutate(Class = "All", .before = Min) %>% 
  rbind(unrepaired_Timedf, .)

unrepaired_Timedf %>% kable(., longtable = T, booktabs = T,
                          format.args = list(big.mark = ','),
                          caption = "Unrepaired gas leak age (days), 2019", 
                          align = "r", 
                          digits = 1) %>% 
  # add_header_above(., c(" ", "Unrepaired Leaks" = 4, "Repaired Leaks" = 4, 
  #                       "Total Leaks" = 4)) %>% 
  # row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))

# create a overlapping histogram of leak age
# unrepaired2019final %>%
#   as.data.frame() %>%
#   select(Class,LeakAgeDays) %>%
#   mutate(Class = factor(Class, levels = c("3","2","1"), ordered = T)) %>%
#   drop_na(Class) %>%
#   ggpubr::gghistogram(., x = "LeakAgeDays",
#                       add = "median", rug = TRUE,
#                       color = "Class", fill = "Class",
#                       palette = c("#8da0cb","#66c2a5","#fc8d62"),
#                       title = "Unrepaired Leak Age by Leak Class in 2019",
#                       xlab = "Age (days)",
#                       ylab = "Number of Unrepaired Leaks",
#                       ggtheme = theme_minimal())

# create group medians for histogram
mu <- unrepaired2019final %>% 
  as.data.frame() %>% 
  drop_na(Class) %>% 
  group_by(Class) %>% 
  summarize(grp.med = median(LeakAgeDays, na.rm = T))

unrepaired2019final %>% 
  as.data.frame() %>% 
  select(Class,LeakAgeDays) %>% 
  mutate(Class = factor(Class, levels = c("3","2","1"), ordered = T)) %>% 
  drop_na(Class) %>% 
  ggplot(aes(x = LeakAgeDays, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  geom_vline(data = mu, aes(xintercept = grp.med, color = Class), 
             linetype = "dashed") +
  scale_color_manual(values = c("#8da0cb","#66c2a5","#fc8d62"),
                     guide = guide_legend(reverse = TRUE,
                                          override.aes = list(fill = c("#fc8d62","#66c2a5","#8da0cb")))) +
  scale_fill_manual(values = c("#8da0cb","#66c2a5","#fc8d62"),
                    guide = FALSE) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "Unrepaired Leak Age by Leak Class in 2019",
       x = "Age (days)",
       y = "Number of Unrepaired Leaks", fill = "",
       caption = "Column heights indicate number of leaks for a given age range. Colors correspond to leak class.\nDashed lines indicate median values by leak class. Rug plot at base shows individual values.") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) + geom_rug()

ggsave("Images/SHARE2/histAge.png")




# create a map of average leak age by block group
blkgrpsLeaksAge <- ma_blkgrps %>% 
  filter(unrepaired2019total > 0)

m5 <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") + 
  tm_shape(blkgrpsLeaksAge) + tm_fill(col = "LeakAgeDaysAvg", style = "fisher",
                                      palette = "YlOrRd", 
                                      title = "Mean Leak\nAge (days)",
                                      legend.format = list(digits = 0),
                                      colorNA = NULL) +
  tm_shape(ma_blkgrps) + tm_borders(lwd = 0.01, alpha = 0.8) +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(newton) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Mean Age of Unrepaired Leaks by Census Block Group and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m5, filename = "Images/SHARE2/m_blkgrpAgeUtil_lines.png", 
          dpi = 600)





















total_Sumdf <- left_join(unrepaired_Sumdf, repaired_Sumdf, by = "Class") %>% 
  drop_na() %>%
  mutate(Total = Unrepaired + Repaired) %>% 
  bind_rows(summarize(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total"))) %>% 
  mutate(PctUnrepaired = Unrepaired/Total*100,
         PctRepaired = Repaired/Total*100,
         PctTotal = Total/Total[Class == "Total"]*100) %>%
  select(Class, Unrepaired, PctUnrepaired, Repaired, PctRepaired, 
         Total, PctTotal)

total_Sumdf %>% 
  mutate(PctUnrepaired = paste0(round(PctUnrepaired,1),"%"),
         PctRepaired = paste0(round(PctRepaired,1),"%"),
         PctTotal = paste0(round(PctTotal,1),"%")) %>% 
  kable(., longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        caption = "Gas leaks by class and repair status, 2019", align = "r", 
        # digits = c(0,0,1,0,1,0,1), 
        col.names = c("Class","Count","Percent","Count","Percent",
                      "Total","Pct Total")) %>% 
  add_header_above(., c(" ", "Unrepaired Leaks" = 2, "Repaired Leaks" = 2, 
                        " " = 2)) %>% 
  row_spec(4, bold = T) %>% 
  kable_styling(latex_options = c("repeat_header"))












# create a csv to share with Dom
ma_blkgrps_EJ <- ma_blkgrps %>% 
  as.data.frame() %>% 
  filter(MA_INCOME21 == "I" | MA_MINORITY21 == "M" | MA_ENGLISH == "E") %>%
  select(GEOID, TOWN, MA_INCOME21, MA_MINORITY21, MA_ENGLISH) %>% 
  separate(TOWN, c("Town","County","State"), sep = ", ")

write_csv(ma_blkgrps_EJ, file = "Images/SHARE2/ma_blkgrps_EJ.csv")



# create a csv to share with Cecilia in AG's office
ma_blkgrps_EJ <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(GEOID, area_sqkm, TOWN, GAS_LABEL, totalpopE, totalpopM, MA_INCOME21, MA_MINORITY21, MA_ENGLISH, unrepaired2019total, LeakAgeDaysAvg) %>% 
  separate(TOWN, c("Town","County","State"), sep = ", ")

write_csv(ma_blkgrps_EJ, file = "Images/SHARE2/ma_blkgrps_EJ.csv")
  

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
# create an EJ layer
EJlayer <- ma_blkgrps %>% 
  filter(MA_MINORITY21 == "M" | MA_ENGLISH == "E" | MA_INCOME21 == "I") %>% 
  st_union(.) %>% 
  st_as_sf(.)

# Load natural gas utility service areas dbf from MassGIS and join to MassGIS towns layer
ng_dbf <- read.dbf("Data/pubutil/TOWNS_POLY_UTILITIES.dbf")

ng_service_areas <- st_read(dsn = "Data/townssurvey_shp",
                            layer = "TOWNSSURVEY_POLYM") %>% 
  select(-TOWN) %>% 
  left_join(., ng_dbf, by = "TOWN_ID") %>% 
  select(TOWN, GAS, GAS_LABEL) %>%  
  st_transform(., crs = st_crs(ma_blkgrps)) %>% 
  st_make_valid()

# isolate No gas areas and municipal
ng_nogas_muni <- ng_service_areas %>% 
  filter(GAS_LABEL %in% c("No Natural Gas Service","Municipal")) %>% 
  group_by(GAS_LABEL) %>%
  summarize() %>% 
  mutate(GAS_LABEL = as.character(GAS_LABEL))

# consolidate into basic gas service districts
ng_service_areas2 <- ng_service_areas %>% 
  filter(!GAS_LABEL %in% c("No Natural Gas Service","Municipal")) %>% 
  group_by(GAS_LABEL) %>% 
  summarize(TownCnt = n()) %>% 
  mutate(ID = case_when(
    GAS_LABEL == "National Grid" ~ "NG",
    GAS_LABEL == "Blackstone Gas Company" ~ "BGC",
    GAS_LABEL == "Columbia Gas" ~ "CG",
    GAS_LABEL == "Eversource Energy" ~ "EV",
    GAS_LABEL == "Columbia Gas, Eversource Energy" ~ "CG,EV",
    GAS_LABEL == "Unitil" ~ "UN",
    GAS_LABEL == "National Grid, Unitil" ~ "NG,UN",
    GAS_LABEL == "Eversource Energy, National Grid" ~ "EV,NG",
    GAS_LABEL == "The Berkshire Gas Company" ~ "BG",
    GAS_LABEL == "Columbia Gas, Blackstone Gas Company" ~ "CG,BGC",
    GAS_LABEL == "Liberty Utilities" ~ "LU",
    GAS_LABEL == "Colonial Gas" ~ "NG",
    GAS_LABEL == "Columbia Gas, National Grid" ~ "CG,NG"
  ))

# join ID to original municipalities within ng_service_area to use centroids as labels; restrict to central municipalities within territories
ng_service_labels <- ng_service_areas2 %>% 
  as.data.frame() %>% 
  select(ID, GAS_LABEL) %>% 
  inner_join(ng_service_areas, ., by = "GAS_LABEL") %>% 
  filter(TOWN %in% c("LANESBOROUGH","DEERFIELD","NORTHAMPTON","LUDLOW",
                     "NORTH BROOKFIELD","OXFORD",
                     "WESTMINSTER","CARLISLE","ANDOVER","WESTBOROUGH",
                     "BLACKSTONE","MILTON",
                     "NORWOOD","WEST BRIDGEWATER","PLAINVILLE","PLYMOUTH",
                     "WESTPORT","ACUSHNET","WAREHAM","BARNSTABLE",
                     "SWANSEA","MARSHFIELD","COHASSET",
                     "HAMILTON","WEST NEWBURY","DOVER","DUNSTABLE")) %>% 
  st_centroid(., of_largest_polygon = TRUE)

ng_service_labels2 <- ng_service_areas2 %>% 
  as.data.frame() %>% 
  select(ID, GAS_LABEL) %>% 
  inner_join(ng_service_areas, ., by = "GAS_LABEL") %>% 
  filter(TOWN %in% c("LEICESTER","LUNENBURG","MENDON","BELLINGHAM","HANSON",
                     "WAYLAND","NATICK","BOSTON","SOMERVILLE")) %>% 
  st_centroid(., of_largest_polygon = TRUE)

ng_service_labels3 <- ng_service_areas2 %>% 
  as.data.frame() %>% 
  select(ID, GAS_LABEL) %>% 
  inner_join(ng_service_areas, ., by = "GAS_LABEL") %>% 
  filter(TOWN == "BOSTON") %>% 
  st_centroid(., of_largest_polygon = TRUE)


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
gridCnt <- gridCnt %>% 
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
                     "Worcester",
                     "Springfield",
                     "Pittsfield", 
                     "Stockbridge",
                     "Fall River",
                     "West Yarmouth",
                     "Lynn",
                     "Randolph",
                     "Webster",
                     "Attleboro",
                     "Medford",
                     "Amherst",
                     "Quincy",
                     "Weymouth Town",
                     "Nantucket")) %>% 
  mutate(NAME = recode(NAME, "Weymouth Town" = "Weymouth")) %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# create a separate point for Newton so that it can be repositioned
newton <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME %in% c("Fitchburg","Newton","Edgartown","New Bedford")) %>% 
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
# # use OSM basemap
# maBM <- gridCnt %>% 
#   st_transform(., crs = 4326) %>% 
#   st_bbox(.) %>% 
#   read_osm(., type = "esri")
# # tm_shape(maBM) + tm_rgb() +
  

# create layer of hexagonx > 0
gridCnt1 <- gridCnt %>% 
  filter(unrepaired > 0)

m_gridUnrepaired <- tm_shape(ng_nogas_muni, bbox = ma_state_sf) + 
  tm_fill(col = "GAS_LABEL", palette = c("honeydew2","gray88"), title = "") +
  tm_shape(gridCnt1, unit = "km", bbox = ma_state_sf) + 
  tm_fill(col = "unrepaired", palette = "YlOrRd",
                        style = "fisher",
                        # breaks = c(1,5,10,20,40,80), 
                        legend.format = list(digits = 0),
          title = "Number\nof leaks") +
  # tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  # tm_shape(ne_states_sf_cb) + 
  # tm_polygons(col = "gray90", border.col = "white") +
  # tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ng_service_areas2) + tm_borders(lwd = 0.8, col = "grey68", alpha = 0.7) +
  # tm_shape(ma_state_sf) + tm_borders(lwd = 0.5, alpha = 0.8) +
  tm_shape(ng_service_labels) + 
  tm_text("ID", size = 0.6, col = "gray") +
  tm_shape(ng_service_labels2) + 
  tm_text("ID", size = 0.4, col = "gray") +
  tm_shape(ng_service_labels3) + 
  tm_text("ID", size = 0.6, col = "gray", xmod = 2.7, ymod = 0.7) +
  # tm_shape(EJlayer) + tm_borders(col = "blue", alpha = 0.7) +
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
  tm_scale_bar(breaks = c(0,25,50), position = c("center","BOTTOM")) +
  tm_layout(legend.position = c("left","bottom"),
            title = "Massachusetts Unrepaired Gas Leaks and Utility Territories, 2019",
            inner.margins = c(0.02,0.02,0.09,0.02)) +
  tm_credits("BG = Berkshire Gas\nBGC = Blackstone Gas Co.\nCG = Columbia Gas\nEV = Eversource Energy\nLU = Liberty Utilities\nNG = National Grid\nUN = Unitil/Fitchburg Gas", position = c(.3,.08), col = "gray47", size = 0.7)

tmap_save(m_gridUnrepaired, filename = "Images/SHARE2/m_gridUnrepairedUtil.png", 
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