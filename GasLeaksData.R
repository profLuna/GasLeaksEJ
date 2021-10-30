# Processing natural gas leak data supplied by Dominic Nicholas at HEET

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
# library(tigris)
library(lubridate)
library(readxl)

# Process 2020 gas leak data
# Look at available sheets
excel_sheets("Data/DATA - 2021 - cleaned - 4.16.2021.xlsx")

# NOTE PROBLEM WITH FITCBURG AND EGMA-REPAIRED SHEETS. `DATE REPORTED` AND `DATE REPAIRED` COLUMNS WOULD NOT IMPORT CORRECTLY INTO R BECAUSE SOME DATES WERE NOT FORMATTED AS DATES AND TWO ENTRIES FOR FITCHBURG WERE NOT DATES - HAD TO DELETE.

# Desired variable names
# [1] "Address"      "RptDate"      "Class"        "LeakNo"       "RepairDate"  
# [6] "Utility"      "DaysToRepair"

# look at names of fields to determine what needs to be harmonized
lapply(repaired2020, names)
# Address = formatted_address
# RptDateDate = Reported/Classification Date/Classification Date/DATE REPORTED/Date Discovered/Reported Date/Reported Date
# Class = Leak Class/Grade/Grade [1, 2, 3]/LEAK GRADE/Grade/Ending Leak Grade/Ending Leak Grade
# LeakNo = Leak Number/Leak ID/Leak ID (if applicable)/Leak #/
# RepairDate = Repair/Repair Dates (during 2019)/Repair Dates (during quarter)/DATE REPAIRED/Resolved Date/Repaired Date
# Utility
# DaysToRepair
# lon = lon
# lat = lat

# create list of various versions of variable names
rptdateV <- c("Reported","Classification Date","DATE REPORTED", "Date Reported",
              "Date Discovered","Reported Date")

classV <- c("Leak Class", "Grade", "Grade [1, 2, 3]", "LEAK GRADE", "Ending Leak Grade", "Leak Grade")

# leaknoV <- c("Leak Number", "Leak ID", "Leak ID (if applicable)", "Leak #")

repairV <- c("Repair", "Repair Dates (during 2019)", "Repair Dates (during quarter)", "DATE REPAIRED", "Resolved Date", "Repaired Date", "Date Repaired", "Repair Date")

# Extract repaired sheets and harmonize variables
repaired2020 <- excel_sheets("Data/DATA - 2021 - cleaned - 4.16.2021.xlsx") %>% 
  .[str_detect(., "- re")|str_detect(., "- Colonial")] %>% 
  lapply(., function(x) {
    ret <- read_excel(path = "Data/DATA - 2021 - cleaned - 4.16.2021FIXED.xlsx", 
                      sheet = x)
    ret$Utility = str_extract_all(x, "^.*?(?= -)")
    ret}) %>% 
  lapply(., function(x){
    select(x, formatted_address,
           any_of(rptdateV), 
           any_of(classV),
           # any_of(leaknoV),
           any_of(repairV),
           Utility,
           lon,
           lat) %>%   
      transmute(Address = formatted_address,
                RptDate = .[[2]],
                Class = as.character(.[[3]]),
                RepairDate = .[[4]],
                Utility = as.character(Utility),
                DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)),
                lon = as.numeric(lon),
                lat = as.numeric(lat))
  }) %>% 
  do.call(rbind, .)

# Standardize leak grades; note weird hyphen that will not be recognized by mutate or recode. See https://stackoverflow.com/questions/44353306/r-regex-not-matching-all-hyphens
repaired2020 <- repaired2020 %>% 
  mutate(Class = gsub("\\p{Pd}","", Class, perl = T), #get rid of weird hyphen
         Class = recode(Class, "Grade 1" = "1",
                        "Grade 2" = "2",
                        "Grade 3" = "3",
                        "21" = "2"))

# Process unrepaired leaks
# desired variable names
# [1] "Address"     "RptDate"     "Class"       "LeakNo"      "Utility" 
# [7] "EndDate"     "LeakAgeDays"

# look at names of fields to determine what needs to be harmonized
lapply(unrepaired2019, names)
# Address = formatted_address
# RptDateDate = Reported/Classification Date/DATE REPORTED/Date Discovered/Reported Date/Date Reported
# Class = Leak Class/Grade/Grade [1, 2, 3]/LEAK GRADE/Ending Leak Grade
# Utility
# lon = lon
# lat = lat

unrepaired2020 <- excel_sheets("Data/DATA - 2021 - cleaned - 4.16.2021FIXED.xlsx") %>% 
  .[str_detect(., "- un")|str_detect(., "Sheet16")] %>% 
  lapply(., function(x) {
    ret <- read_excel(path = "Data/DATA - 2021 - cleaned - 4.16.2021FIXED.xlsx", sheet = x)
    ret$Utility = str_extract_all(x, "^.*?(?= -)")
    ret}) %>% 
  lapply(., function(x){
    select(x, formatted_address,
           any_of(rptdateV), 
           any_of(classV),
           Utility,
           lon,
           lat) %>% 
      transmute(Address = formatted_address,
                RptDate = .[[2]],
                Class = .[[3]],
                Utility = as.character(Utility),
                EndDate = as.Date("2020-12-31"),
                LeakAgeDays = abs(interval(EndDate,RptDate)/days(1)),
                lon = as.numeric(lon),
                lat = as.numeric(lat))
  }) %>% 
  do.call(rbind, .)

# Standardize leak grades and assign utility name
unrepaired2020 <- unrepaired2020 %>% 
  mutate(Class = recode(Class, "Grade 1" = "1",
                        "Grade 2" = "2",
                        "Grade 3" = "3"),
         Utility = recode(Utility, "character(0)" = "National Grid"))

# convert to sf
repaired2020 <- repaired2020 %>% 
  st_as_sf(., coords = c("lon","lat"), crs = 4326)

unrepaired2020 <- unrepaired2020 %>% 
  st_as_sf(., coords = c("lon","lat"), crs = 4326)

# save data
# write it out to shapefile
repaired2020 %>% 
  st_write(., delete_layer = TRUE, "Data/HEETrepaired2020.shp")

unrepaired2020 %>% 
  st_write(., delete_layer = TRUE, "Data/HEETunrepaired2020.shp")

# save everything
save(unrepaired2020, repaired2020, file = "Data/HEET2020Leaks.rds")




# Process 2019 gas leak data
# view sheets in workbook to identify sheet names
excel_sheets("Data/DATA - cleaned.xlsx")

# NOTE PROBLEM WITH EVERSOURCE-REPAIRED SHEET. `DATE REPORTED` COLUMN WOULD NOT IMPORT CORRECTLY INTO R BECAUSE 48 DATES WERE FORMATTED ODDLY IN EXCEL. TO FIX, IN EXCEL, CREATE NEW COLUMN, USE DATEVALUE FORMULA TO EXTRACT DATE VALUES ONLY FOR MESSED UP DATES, SORT TO IDENTIFY THEM, SET FORMAT OF DATE VALUES TO SAME AS ORIGINAL CUSTOM FORMAT, PASTE VALUES BACK INTO ORIGINAL COLUMN.
# NOTE THAT REPAIRED LEAKS FOR COLUMBIA INCLUDE THOSE THAT WERE 'REPAIRED BY REPLACEMENT' WHICH IS NOT THE SAME AS THE OTHER UTILITIES. THOSE REPAIRED BY REPLACEMENT HAVE BEEN DELETED IN THE EXCEL SHEET.

# Desired variable names
# [1] "Address"      "RptDate"      "Class"        "LeakNo"       "RepairDate"  
# [6] "Utility"      "DaysToRepair"

# look at names of fields to determine what needs to be harmonized
lapply(RepairedList, names)
# Address = formatted_address
# RptDateDate = Reported/Classification Date/Classification Date/DATE REPORTED/Date Discovered/Reported Date/Reported Date
# Class = Leak Class/Grade/Grade [1, 2, 3]/LEAK GRADE/Grade/Ending Leak Grade/Ending Leak Grade
# LeakNo = Leak Number/Leak ID/Leak ID (if applicable)/Leak #/
# RepairDate = Repair/Repair Dates (during 2019)/Repair Dates (during quarter)/DATE REPAIRED/Resolved Date/Repaired Date
# Utility
# DaysToRepair
# lon = lon
# lat = lat

# create list of various versions of variable names
rptdateV <- c("Reported","Classification Date","DATE REPORTED", "Date Reported",
              "Date Discovered","Reported Date")

classV <- c("Leak Class", "Grade", "Grade [1, 2, 3]", "LEAK GRADE", "Ending Leak Grade", "Leak Grade")

# leaknoV <- c("Leak Number", "Leak ID", "Leak ID (if applicable)", "Leak #")

repairV <- c("Repair", "Repair Dates (during 2019)", "Repair Dates (during quarter)", "DATE REPAIRED", "Resolved Date", "Repaired Date", "Date Repaired", "Repair Date")


# Extract repaired sheets and harmonize variables
repaired2019 <- excel_sheets("Data/DATA - cleaned.xlsx") %>% 
  .[str_detect(., "- re")|str_detect(., "- Colonial")] %>% 
  lapply(., function(x) {
    ret <- read_excel(path = "Data/DATA - cleanedFIXED.xlsx", sheet = x)
    ret$Utility = str_extract_all(x, "^.*?(?= -)")
    ret}) %>% 
  lapply(., function(x){
    select(x, formatted_address,
           any_of(rptdateV), 
           any_of(classV),
           # any_of(leaknoV),
           any_of(repairV),
           Utility,
           lon,
           lat) %>% 
      transmute(Address = formatted_address,
                RptDate = .[[2]],
                Class = .[[3]],
                RepairDate = .[[4]],
                Utility = as.character(Utility),
                DaysToRepair = abs(interval(RptDate, RepairDate)/days(1)),
                lon = as.numeric(lon),
                lat = as.numeric(lat))
  }) %>% 
  do.call(rbind, .)

# Standardize leak grades and filter out anything after 2019
repaired2019 <- repaired2019 %>% 
  mutate(Class = recode(Class, "Grade 1" = "1",
                        "Grade 2" = "2",
                        "Grade 3" = "3",
                        "2A" = "2")) %>% 
  filter(RepairDate < "2020-01-01" & RptDate < "2020-01-01")
         
# RptDate = if_else(year(RptDate) > 2019, 
#                            `year<-`(RptDate, 2019), RptDate))

# Process unrepaired leaks
# desired variable names
# [1] "Address"     "RptDate"     "Class"       "LeakNo"      "Utility" 
# [7] "EndDate"     "LeakAgeDays"

# look at names of fields to determine what needs to be harmonized
lapply(unrepaired2019, names)
# Address = formatted_address
# RptDateDate = Reported/Classification Date/DATE REPORTED/Date Discovered/Reported Date/Date Reported
# Class = Leak Class/Grade/Grade [1, 2, 3]/LEAK GRADE/Ending Leak Grade
# Utility
# lon = lon
# lat = lat

unrepaired2019 <- excel_sheets("Data/DATA - cleaned.xlsx") %>% 
  .[str_detect(., "- un")|str_detect(., "Sheet30")] %>% 
  lapply(., function(x) {
    ret <- read_excel(path = "Data/DATA - cleaned.xlsx", sheet = x)
    ret$Utility = str_extract_all(x, "^.*?(?= -)")
    ret}) %>% 
  lapply(., function(x){
    select(x, formatted_address,
           any_of(rptdateV), 
           any_of(classV),
           Utility,
           lon,
           lat) %>% 
      transmute(Address = formatted_address,
                RptDate = .[[2]],
                Class = .[[3]],
                Utility = as.character(Utility),
                EndDate = as.Date("2019-12-31"),
                LeakAgeDays = abs(interval(EndDate,RptDate)/days(1)),
                lon = as.numeric(lon),
                lat = as.numeric(lat))
  }) %>% 
  do.call(rbind, .)

# Standardize leak grades, assign utility name, filter out after 2019
unrepaired2019 <- unrepaired2019 %>% 
  mutate(Class = recode(Class, "Grade 1" = "1",
                        "Grade 2" = "2",
                        "Grade 3" = "3",
                        "2A" = "2"),
         Utility = recode(Utility, 
                          "character(0)" = "National Grid")) %>% 
  filter(RptDate < "2020-01-01")
  

# convert to sf
repaired2019 <- repaired2019 %>% 
  st_as_sf(., coords = c("lon","lat"), crs = 4326)

unrepaired2019 <- unrepaired2019 %>% 
  st_as_sf(., coords = c("lon","lat"), crs = 4326)

# save data
# write it out to shapefile
repaired2019 %>% 
  st_write(., delete_layer = TRUE, "Data/HEETrepaired2019v2.shp")

unrepaired2019 %>% 
  st_write(., delete_layer = TRUE, "Data/HEETunrepaired2019v2.shp")

# save everything
save(unrepaired2019, repaired2019, file = "Data/HEET2019Leaksv2.rds")







#### STEPS BELOW ARE FOR PROCESSING KML FILES FROM HEET
# Import geocoded gas leaks data from HEET - kmls downloaded Nov 14, 2020
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