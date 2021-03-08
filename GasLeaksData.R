# Import geocoded gas leaks data from HEET - kmls downloaded Nov 14, 2020

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)
library(lubridate)

# create vector of text and regex to remove descriptors and : from Description column but leave <br> to separate columns
removeTxt <- c("description: <br>", 
               "(?<=<br>).*?: ")

# create vector of characters and regex to remove extraneous characters from file names to identify utilities
removeFile <- c("^.*/", 
                " - 2019 REPAIRED.kml",
                " - 2019 UNREPAIRED.kml", 
                "[0-9]", "_")

# # Bring in kml file and separate Description column into separate columns
# BerkshireGas2019 <- st_read("KML/leaks2019/Berkshire Gas - 2019.kml") %>% 
#   mutate(Description = str_remove_all(Description, 
#                                       paste(removeTxt, collapse = "|"))) %>%
#   separate(Description, c("ReptAddress", "Date", "Class", "LeakNo", 
#                           "RepairDate", "RptLoc", "RptTown", "MapAddress"), 
#            sep = "<br>", extra = "merge")
# 
# BerkshireGas2019Unrepaired <- st_read("KML/leaks2019/Berkshire Gas - 2019 UNREPAIRED.kml") %>% 
#   mutate(Description = str_remove_all(Description, 
#                                       paste(removeTxt, collapse = "|"))) %>%
#   separate(Description, c("ReptAddress", "Date", "Class", "LeakNo", 
#                           "RptLoc", "RptTown", "MapAddress"), 
#            sep = "<br>", extra = "merge")

# # NOTE THAT Eversource Energy, Fitchburg Gas, and Liberty Utilities DO NOT HAVE A FIELD FOR LEAK NUMBER, SO NEED TO BE TREATED SEPARATELY
# 
# # Read in all unrepaired kml and merge to one layer; clean up columns
# unrepaired2019 <- list.files(path = "KML/leaks2019", pattern = "UNREPAIRED.kml",
#                              full.names = TRUE) %>% 
#   lapply(., function(x){
#     ret <- st_read(x)
#     ret$Utility <- str_remove_all(x, paste(removeFile, collapse = "|"))
#     ret
#   }) %>% 
#   do.call(rbind, .) %>% 
#   mutate(Description = str_remove_all(Description, 
#                                       paste(removeTxt, collapse = "|"))) %>%
#   separate(Description, c("ReptAddress", "Date", "Class", "LeakNo", 
#                           "RptLoc", "RptTown", "MapAddress"), 
#            sep = "<br>", extra = "merge")
# 
# 
# # Pull in leaks for utilities with leak number field
# unrepaired2019a <- list.files(path = "KML/leaks2019", pattern = "UNREPAIRED.kml",
#                              full.names = TRUE) %>% 
#   lapply(., function(x){
#     ret <- st_read(x)
#     ret$Utility <- str_remove_all(x, paste(removeFile, collapse = "|"))
#     ret
#   }) %>% 
#   do.call(rbind, .) %>% 
#   mutate(Description = str_remove_all(Description, 
#                                       paste(removeTxt, collapse = "|"))) %>%
#   filter(!Utility %in% c("Eversource Energy", 
#                         "Fitchburg Gas", "Liberty Utilities")) %>% 
#   separate(Description, c("ReptAddress", "Date", "Class", "LeakNo", 
#                           "RptLoc", "RptTown", "MapAddress"), 
#            sep = "<br>", extra = "merge")
# 
# # Pull in leaks for utilities without leak number field
# unrepaired2019b <- list.files(path = "KML/leaks2019", pattern = "UNREPAIRED.kml",
#                               full.names = TRUE) %>% 
#   lapply(., function(x){
#     ret <- st_read(x)
#     ret$Utility <- str_remove_all(x, paste(removeFile, collapse = "|"))
#     ret
#   }) %>% 
#   do.call(rbind, .) %>% 
#   mutate(Description = str_remove_all(Description, 
#                                       paste(removeTxt, collapse = "|"))) %>%
#   filter(Utility %in% c("Eversource Energy", 
#                          "Fitchburg Gas", "Liberty Utilities")) %>% 
#   separate(Description, c("ReptAddress", "Date", "Class",  
#                           "RptLoc", "RptTown", "MapAddress"), 
#            sep = "<br>", extra = "merge")
# 
# # bring leaks together with a common set of variables
# unrepaired2019 <- unrepaired2019b %>% 
#   add_column(LeakNo = NA, .after = "Class") %>% 
#   rbind(unrepaired2019a) %>% 
#   arrange(Utility)
# 
# 
# # Need to reconcile possible fields in Description column
# unrepaired2019 <- list.files(path = "KML/leaks2019", pattern = "UNREPAIRED.kml",
#                              full.names = TRUE) %>% 
#   lapply(., function(x){
#     ret <- st_read(x)
#     ret$Utility <- str_remove_all(x, paste(removeFile, collapse = "|"))
#     ret
#   }) %>% 
#   do.call(rbind, .)
# 
# # pull out one row from each utility
# utilSamples <- unrepaired2019 %>% 
#   as.data.frame() %>% 
#   group_by(Utility) %>% 
#   slice(1)
# # inspect Description field of each utility to discern differences and similarities
# as.character(utilSamples[2,2])
# 
# # this works to replace descriptors with <br>
# str_replace_all(utilSamples[1,2], "<br>.*?: ", "<br>")
# # this works to remove descriptors, although it also takes out <br>
# str_remove_all(utilSamples[1,2], "<br>.*?: ")
# # this works to remove descriptors and :, but leave <br>
# str_remove_all(utilSamples[1,2], "(?<=<br>).*?: ")


# Need to reconcile differences in Description column between different utilities before merging them
# Bring in all KML files for all unrepaired leaks
unrepaired2019 <- list.files(path = "Data/leaks2019", pattern = "UNREPAIRED.kml",
                             full.names = TRUE) %>% 
  lapply(., function(x){
    ret <- st_read(x)
    ret$Utility <- str_remove_all(x, paste(removeFile, collapse = "|"))
    ret
  }) %>% 
  do.call(rbind, .) %>% 
  mutate(Description = str_remove_all(Description, 
                                      paste(removeTxt, collapse = "|"))) %>% 
  st_transform(., crs = 2805)

# Isolate individual utilities in order to correctly parse out Description column with separate. Note that not all fields are shared between utilities (e.g., leak id, leak class) AND some fields for National Grid are in different order from all other utilities. 
# National Grid
natlGrid2019unrepaired <- unrepaired2019 %>%
  filter(Utility %in% c("National Grid - Boston Gas", 
                        "National Grid - Colonial Gas")) %>% 
  separate(Description, c("ReptAddress", "Class", "LeakNo", 
                          "RptLoc", "RptDate", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge") 

# Columbia Gas
columbia2019unrepaired <- unrepaired2019 %>%
  filter(Utility == "Columbia Gas") %>% 
  separate(Description, c("ReptAddress", "RptDate", "Class", "LeakNo", 
                          "RptLoc", "RptTown", "Status", "MapAddress"), 
           sep = "<br>", extra = "merge")

# Berkshire Gas
berkshire2019unrepaired <- unrepaired2019 %>%
  filter(Utility == "Berkshire Gas") %>% 
  separate(Description, c("ReptAddress", "RptDate", "Class", "LeakNo", 
                          "RptLoc", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge")

# Fitchburg Gas
fitchburg2019unrepaired <- unrepaired2019 %>%
  filter(Utility == "Fitchburg Gas") %>% 
  separate(Description, c("ReptAddress", "RptDate", "LeakNo", 
                          "RptLoc", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(Class = NA)

# Eversource Energy and Liberty Utilities
everLiberty2019unrepaired <- unrepaired2019 %>%
  filter(Utility %in% c("Eversource Energy", "Liberty Utilities")) %>% 
  separate(Description, c("ReptAddress", "RptDate", "Class", 
                          "RptLoc", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(LeakNo = NA)

# bring them together in a merged file with common fields
unrepaired2019final <- mget(ls(pattern = "unrepaired$")) %>% 
  lapply(., function(x){
    x %>% 
      select(Name, RptDate, Class, LeakNo, Utility)
  }) %>% 
  do.call(rbind, .) %>% 
  rename(Address = Name) %>% 
  mutate(RptDate = mdy(RptDate),
         EndDate = as.Date("2019-12-31"),
         LeakAgeDays = abs(interval(EndDate,RptDate)/days(1)),
         Class = recode(Class, "Grade 2" = "2",
                        "Grade 3" = "3",
                        "2A" = "2"))

# write it out to shapefile
unrepaired2019final %>% 
  st_zm(., drop = TRUE) %>% 
  st_write(., delete_layer = TRUE, "Data/HEETunrepaired2019.shp")

#### Bring in repaired leaks for 2019 ####
# Bring in all KML files for all repaired leaks
repaired2019 <- list.files(path = "Data/leaks2019", pattern = " REPAIRED.kml",
                           full.names = TRUE) %>% 
  lapply(., function(x){
    ret <- st_read(x)
    ret$Utility <- str_remove_all(x, paste(removeFile, collapse = "|"))
    ret
  }) %>% 
  do.call(rbind, .) %>% 
  mutate(Description = str_remove_all(Description, 
                                      paste(removeTxt, collapse = "|"))) %>% 
  st_transform(., crs = 2805)

# # pull out one row from each utility to inspect formatting
# utilSamples <- repaired2019 %>%
#   as.data.frame() %>%
#   group_by(Utility) %>%
#   slice(1)
# # inspect Description field of each utility to discern differences and similarities
# as.character(utilSamples[3,2])

# Isolate individual utilities in order to correctly parse out Description column with separate. Note that not all fields are shared between utilities (e.g., leak id, leak class) AND some fields for National Grid are in different order from all other utilities. 
# National Grid
natlGrid2019repaired <- repaired2019 %>%
  filter(Utility %in% c("National Grid - Boston Gas", 
                        "National Grid - Colonial Gas")) %>% 
  separate(Description, c("ReptAddress", "Class", "LeakNo", "RepairDate",
                          "RptLoc", "RptDate", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(Class = recode(Class, "2A" = "2"),
         RptDate = parse_date_time(RptDate, orders = "mdy"), 
         RepairDate = parse_date_time(RepairDate, orders = "mdy"),
         DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)))

# Columbia Gas
columbia2019repaired <- repaired2019 %>%
  filter(Utility == "Columbia Gas") %>% 
  separate(Description, c("ReptAddress", "RptDate", "Class", "LeakNo", 
                          "RepairDate", "RptLoc", "RptTown", "Status",
                          "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(RptDate = parse_date_time(RptDate, orders = "mdy"), 
         RepairDate = parse_date_time(RepairDate, orders = "mdy"),
         DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)))

# Berkshire Gas
berkshire2019repaired <- repaired2019 %>%
  filter(Utility == "Berkshire Gas") %>% 
  separate(Description, c("ReptAddress", "RptDate", "Class", "LeakNo", 
                          "RepairDate", "RptLoc", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(RptDate = parse_date_time(RptDate, orders = "mdy"), 
         RepairDate = parse_date_time(RepairDate, orders = "mdy"),
         DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)))

# Fitchburg Gas
fitchburg2019repaired <- repaired2019 %>%
  filter(Utility == "Fitchburg Gas") %>% 
  separate(Description, c("ReptAddress", "RptDate", "LeakNo", "RepairDate",
                          "RptLoc", "RptTown", "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(Class = NA,
         RptDate = parse_date_time(RptDate, orders = "mdy"), 
         RepairDate = parse_date_time(RepairDate, orders = "mdy"),
         DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)))

# # Check Eversource Energy for parsing problems because some dates aren't being converted. 
# temp <- st_read("KML/leaks2019/Eversource Energy - 2019 REPAIRED.kml")
# temp %>% as.data.frame() %>% select(Description) %>% head()
# temp2 <- temp %>% 
#   mutate(Description = str_remove_all(Description, 
#                                       paste(removeTxt, collapse = "|"))) %>% 
#   separate(Description, c("ReptAddress", "RepairDate", "RptDate", 
#                           "Class", "RptIntSt", "RptStreet", "RptStNo", 
#                           "RptTown", "MapAddress"), sep = "<br>") %>% 
#   mutate(LeakNo = NA, 
#          RptDate = dmy(RptDate), 
#          RepairDate = dmy(RepairDate))
# # identify the dates that aren't being converted
# temp[is.na(temp2$RptDate),"RptDate"]
# # need to allow for different date formats

# Eversource Energy
eversource2019repaired <- repaired2019 %>%
  filter(Utility == "Eversource Energy") %>% 
  separate(Description, c("ReptAddress", "RepairDate", "RptDate", "Class", 
                          "RptIntSt", "RptStreet", "RptStNo", "RptTown", 
                          "MapAddress"), 
           sep = "<br>") %>% 
  mutate(RptDate = recode(RptDate, "01-May-29" = "01-May-19")) %>% # fix improperly coded year from original data
  mutate(LeakNo = NA,
         RptDate = parse_date_time(RptDate, orders = c("dmy","mdy")), 
         RepairDate = parse_date_time(RepairDate, orders = c("dmy","mdy")),
         DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)))
  # filter(year(RptDate) <= 2019 & year(RepairDate) <= 2019) # remove transcription error in original data



# Liberty Utilities
liberty2019repaired <- repaired2019 %>%
  filter(Utility == "Liberty Utilities") %>% 
  separate(Description, c("ReptAddress", "RptDate", "Class", 
                          "RptLoc", "RptTown", "RepairDate", "MapAddress"), 
           sep = "<br>", extra = "merge") %>% 
  mutate(LeakNo = NA, Class = str_extract(Class, "[0-9]"),
         RptDate = parse_date_time(RptDate, orders = "mdy"), 
         RepairDate = parse_date_time(RepairDate, orders = "mdy"),
         DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)))

# bring them together in a merged file with common fields. note that there are inconsistencies in date format (only for Eversource) AND some some report and repair dates are reversed (hence the abs() for getting interval)
repaired2019final <- mget(ls(pattern = "9repaired$")) %>% 
  lapply(., function(x){
    x %>% 
      select(Name, RptDate, Class, LeakNo, RepairDate, Utility, DaysToRepair)
  }) %>% 
  do.call(rbind, .) %>% 
  rename(Address = Name)

# compare by utility
repaired2019final %>% 
  group_by(Class, Utility) %>% 
  summarize(n(), min(DaysToRepair), mean(DaysToRepair), max(DaysToRepair))

# write it out to shapefile
repaired2019final %>% 
  st_zm(., drop = TRUE) %>% 
  st_write(., delete_layer = TRUE, "Data/HEETrepaired2019.shp")

# save everything
save(unrepaired2019final, repaired2019final, file = "Data/HEET2019Leaks.rds")

# map it out
tmap_mode("view")
tm_shape(unrepaired2019final) + tm_dots()
tm_shape(unrepaired2019final) + tm_dots(col = "Utility")

tm_shape(repaired2019final) + tm_dots()
tm_shape(repaired2019final) + tm_dots(col = "DaysToRepair", style = "quantile")