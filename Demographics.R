# Environmental Justice analysis of gas leaks across Massachusetts

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")
# (wdir <- getwd())
# setwd(wdir)

# set ACS parameters
year = 2019
survey = "acs5"
moe = 95

# Load list of variables to identify tables of interest
v <- load_variables(year, survey, cache = TRUE)

# Download ACS 5-year estimates of demographic data and convert to projected local CRS EPSG:2805: NAD83(HARN) / Massachusetts See https://spatialreference.org/ref/epsg/2805/
# Block groups
ma_blkgrps <- get_acs(geography = "block group", 
                        year = year, moe_level = moe, survey = survey,
                        variables = c(totalpop = "B03002_001", 
                                      medhhinc = "B19013_001"),
                        state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$')) %>% 
  st_transform(., crs = 2805)


# County subdivisions (i.e., cities and towns)
ma_cosub <- get_acs(geography = "county subdivision", 
                      year = year, moe_level = moe, survey = survey,
                      variables = c(totalpop = "B03002_001", 
                                    medhhinc = "B19013_001"),
                      state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$')) %>% 
  st_transform(., crs = 2805)

# Isolate MA statewide median household income for EJ threshold for income criterion in 2021 EJ policy
MA_MED_HHINC <- get_acs(geography = "state", 
                        year = year, moe_level = moe, survey = survey,
                        variables = c(totalpop = "B03002_001",
                                      medhhinc = "B19013_001"),
                        state = "MA", output = "wide") %>% 
  dplyr::select(medhhincE) %>% 
  pull()

# add variables to identify EJ criteria thresholds for to allow for definition of minority threshold for 2021 EJ policy
ma_cosub <- ma_cosub %>% 
  mutate(MACOSUB_INC_BELOW150 = if_else(medhhincE <= 1.5*MA_MED_HHINC,"Y","N"),
         MACOSUB_INC_BELOW150_UC = if_else(medhhincE_UC <= 1.5*MA_MED_HHINC,"Y","N"),
         MACOSUB_INC_BELOW150_LC = if_else(medhhincE_LC <= 1.5*MA_MED_HHINC,"Y","N"))

###### DEMOGRAPHIC DATA FRAMES BLOCK GROUP LEVEL ##############

## HOUSEHOLDS BY INCOME
# this is for 2017 MA EJ Policy
# download table of counts of household income categories, sum up households in categories below 65% of MA statewide median
B19001 <- get_acs(geography = "block group", 
                  year = year, moe_level = moe, survey = survey,
                  table = "B19001", state = "MA")
# Isolate estimate of total households
medhhinc_total <- B19001 %>%
  filter(variable == "B19001_001") %>%
  transmute(GEOID = GEOID,
            householdsE = estimate,
            householdsM = moe)
# Isolate household counts less than 65% of MA statewide median of $81,215, which as of ACS 5-year 2019 is $52,789.75. Closest range is 45 - 49,9 and below.
# create vector of patterns for medhhinc levels below 50k
med_strings <- rep(c(2:10)) %>%
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
medhhinclt50 <- B19001 %>%
  filter(str_detect(variable,paste(med_strings,collapse = "|"))) %>%
  group_by(GEOID) %>%
  summarize(medhhinclt50E = sum(estimate),
            medhhinclt50M = moe_sum(moe, estimate)) %>%
  mutate(medhhinclt50_UC = medhhinclt50E + medhhinclt50M,
         medhhinclt50_LC = ifelse(
           medhhinclt50E < medhhinclt50M, 0, medhhinclt50E - medhhinclt50M))
# Join total households and compute derived proportions
medhhinclt50_pct <- medhhinclt50 %>%
  left_join(., medhhinc_total, by = "GEOID") %>%
  mutate(r2medhhincE = ifelse(householdsE <= 0, 0, medhhinclt50E/householdsE),
    r2medhhincM = moe_prop(medhhinclt50E,householdsE,medhhinclt50M,householdsM),
    pct_medhhinclt50E = r2medhhincE*100,
    pct_medhhinclt50M = r2medhhincM*100,
    pct_medhhinclt50E_UC = pct_medhhinclt50E + pct_medhhinclt50M,
    pct_medhhinclt50E_LC = ifelse(
      pct_medhhinclt50E < pct_medhhinclt50M, 0,
      pct_medhhinclt50E - pct_medhhinclt50M)) %>%
  select(-starts_with("r2m"))

# add variables to identify EJ criteria thresholds of income for 2017 EJ policy
medhhinclt50_pct <- medhhinclt50_pct %>%
  mutate(MA_INCOME17 = if_else(pct_medhhinclt50E >= 25, "I", NULL),
         MA_INCOME17_UC = if_else(pct_medhhinclt50E_UC >= 25, "I", NULL),
         MA_INCOME17_LC = if_else(pct_medhhinclt50E_LC >= 25, "I", NULL))
# clean up
# rm(B19001,med_strings,medhhinc_total,medhhinclt50)

# add variables to identify EJ criteria thresholds for income for 2021 EJ policy
ma_blkgrps <- ma_blkgrps %>%
  mutate(medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = medhhincE - medhhincM,
         MA_INCOME21 = if_else(medhhincE <= .65*MA_MED_HHINC, "I", NULL),
         MA_INCOME21_UC = if_else(medhhincE_UC <= .65*MA_MED_HHINC, "I", NULL),
         MA_INCOME21_LC = if_else(medhhincE_LC <= .65*MA_MED_HHINC, "I", NULL))

# join town data with MA town income threshold to block groups in order to allow for definition of minority threshold based on town income for 2021 MA EJ POLICY
ma_blkgrps <- ma_cosub %>% 
  transmute(TOWN = NAME, MACOSUB_INC_BELOW150 = MACOSUB_INC_BELOW150, 
            MACOSUB_INC_BELOW150_UC = MACOSUB_INC_BELOW150_UC,
            MACOSUB_INC_BELOW150_LC = MACOSUB_INC_BELOW150_LC) %>% 
  st_join(ma_blkgrps, ., largest = TRUE)


### RATIO OF INCOME TO POVERTY LEVEL
# Download ratio of income to poverty level in the past 12 months to calculate the number or percent of a block group’s population in households where the household income is less than or equal to twice the federal “poverty level.” More precisely, percent low-income is calculated as a percentage of those for whom the poverty ratio was known, as reported by the Census Bureau, which may be less than the full population in some block groups. More information on the federally-defined poverty threshold is available at http://www.census.gov/hhes/www/poverty/methods/definitions.html. Note also that poverty status is not determined for people living in institutional group quarters (i.e. prisons, college dormitories, military barracks, nursing homes), so these populations are not included in the poverty estimates (https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html).
# First, download table of ratio of income to poverty level
C17002 <- get_acs(geography = "block group", table = "C17002", 
                  year = year, moe_level = moe, survey = survey,
                  state = "MA")
# Isolate universe pop for whom poverty status is known
povknown <- C17002 %>% 
  filter(variable == "C17002_001") %>% 
  transmute(GEOID = GEOID,
            povknownE = estimate,
            povknownM = moe,
            povknownE_UC = povknownE + povknownM,
            povknownE_LC = ifelse(
              povknownE < povknownM, 0, povknownE - povknownM))
# Isolate population less than 2x poverty level and compute derived sum esimate along with MOE and UC and LC
num2pov <- C17002 %>% 
  filter(!variable %in% c("C17002_001", "C17002_008")) %>% 
  group_by(GEOID) %>% 
  summarize(num2povE = sum(estimate),
            num2povM = moe_sum(moe, estimate)) %>% 
  mutate(num2povE_UC = num2povE + num2povM,
         num2povE_LC = ifelse(
           num2povE < num2povM, 0, num2povE - num2povM))
# Join tables and compute derived ratios and MOEs and then pcts with UC and LC
poverty_pct <- povknown %>% 
  left_join(., num2pov, by = "GEOID") %>% 
  mutate(r2povE = ifelse(
    povknownE == 0, 0, num2povE/povknownE),
    r2povM = moe_ratio(num2povE,povknownE,num2povM,povknownM),
    pct2povE = r2povE * 100,
    pct2povM = r2povM * 100,
    pct2povE_UC = pct2povE + pct2povM,
    pct2povE_LC = ifelse(
      pct2povE < pct2povM, 0, pct2povE - pct2povM)) %>% 
  select(-starts_with("r2p"))
# clean up
# rm(C17002,num2pov,povknown)


### RACE AND ETHNICITY
# Download B03002 HISPANIC OR LATINO ORIGIN BY RACE in two sets. 
# Start with total pop and all races in wide format and compute upper and lower confidence values. 
B03002_totrace <- get_acs(geography = "block group", 
                          year = year, moe_level = moe, survey = survey,
                          variables = c(
    totalpop = "B03002_001",
    nhwhitepop = "B03002_003",
    nhblackpop = "B03002_004",
    nhamerindpop = "B03002_005",
    nhasianpop = "B03002_006",
    nhnativpop = "B03002_007",
    nhotherpop = "B03002_008",
    nh2morepop = "B03002_009",
    hisppop = "B03002_012"),
    state = "MA") %>% 
  mutate(UC = estimate + moe,
         LC = if_else(estimate < moe, 0, estimate - moe)) %>% 
  rename(E = estimate, M = moe) %>% 
  pivot_wider(names_from = "variable", 
              names_glue = "{variable}_{.value}",
              values_from = c(E, M, UC, LC))

# Next acquire estimates for Hispanic and nonWhite groups in long format to create aggregated minority variable
B03002_minority <- get_acs(geography = "block group", 
                           year = year, moe_level = moe, survey = survey,
                           variables = c(
    nhblackpop = "B03002_004",
    nhamerindpop = "B03002_005",
    nhasianpop = "B03002_006",
    nhnativhpop = "B03002_007",
    nhotherpop = "B03002_008",
    nh2morepop = "B03002_009",
    hisppop = "B03002_012"),
    state = "MA") %>% 
  group_by(GEOID) %>% 
  summarize(minority_E = sum(estimate),
            minority_M = moe_sum(moe,estimate)) %>% 
  mutate(minority_UC = minority_E + minority_M,
         minority_LC = ifelse(
           minority_E < minority_M, 0, minority_E - minority_M))

# Join with all race pops and compute proportions
race_pct <- B03002_totrace %>% 
  left_join(., B03002_minority, by = "GEOID") %>% 
  mutate(across(ends_with("_E"),
                ~ if_else(totalpop_E == 0, 0, .x/totalpop_E), 
                .names = "{col}_p"))

# Compute MOEs for derived proportions
# First, extract unique names for variables to be computed
unique_names <- race_pct %>% 
  select(-ends_with("_p")) %>% 
  names() %>% 
  str_extract(.,"^.+(?=_)") %>% 
  unique() %>% 
  .[!is.na(.)]

# Next, subset data frame to estimates and moe variables only
estimatesDF <- race_pct %>% 
  select(-c(GEOID,NAME), -ends_with(c("UC","LC","_p")))

# Use purrr::map_dfc to match unique names to variables and pass along to moe_prop function and cbind back to race_pct df and then convert proportion to percentages
race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                          select(matches(.x), totalpop_E, totalpop_M) %>%
                          mutate(!!paste0(.x, "_M_p") := 
                                   moe_prop(num = .[[1]], 
                                            denom = totalpop_E, 
                                            moe_num = .[[2]], 
                                            moe_denom = totalpop_M))) %>% 
  select(ends_with("_p")) %>% 
  cbind(race_pct, .) %>% 
  mutate(across(ends_with("_p"), ~(.x * 100)))

# Calculate upper and lower confidence values for percentages
# First, extract unique names for variables to be computed
unique_names <- race_pct %>% 
  select(-ends_with("_p")) %>% 
  names() %>% 
  str_extract(.,"^.+(?=_)") %>% 
  unique() %>% 
  .[!is.na(.)]

# Next, subset data frame to estimates and moe variables only
estimatesDF <- race_pct %>% 
  select(ends_with("_p"))

# Match unique names to variables and pass along to calculate upper and lower estimates and cbind back to race_pct
race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                  select(matches(.x)) %>% 
                  mutate(!!paste0(.x, "_pctUC") := 
                           .[[1]] + .[[2]])) %>% 
  select(ends_with("_pctUC")) %>% 
  cbind(race_pct, .)

race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                  select(matches(.x)) %>% 
                  mutate(!!paste0(.x, "_pctLC") := 
                           if_else(.[[1]] < .[[2]], 0, .[[1]] - .[[2]]))) %>% 
  select(ends_with("_pctLC")) %>% 
  cbind(race_pct, .)

# clean up
# rm(list = ls(pattern = paste(c("B03002","estimatesDF","unique"), 
                             # collapse = "|")))

# add variables to identify EJ minority criteria thresholds for 2017 and 2021 EJ policies. join town-level income variables for minority designation for 2021 policy. 
race_pct <- ma_blkgrps %>% 
  as.data.frame() %>% 
  select(GEOID, MACOSUB_INC_BELOW150, MACOSUB_INC_BELOW150_LC, 
         MACOSUB_INC_BELOW150_UC) %>% 
  left_join(race_pct, ., by = "GEOID") %>% 
  mutate(MA_MINORITY17 = if_else(minority_E_p >= 25, "M", NULL),
         MA_MINORITY17_UC = if_else(minority_pctUC >= 25, "M", NULL),
         MA_MINORITY17_LC = if_else(minority_pctLC >= 25, "M", NULL),
         MA_MINORITY21 = if_else(minority_E_p >= 40 | (minority_E_p >= 25 & MACOSUB_INC_BELOW150 == "Y"), "M", NULL),
         MA_MINORITY21_UC = if_else(minority_pctUC >= 40 | (minority_pctUC >= 25 & MACOSUB_INC_BELOW150_UC == "Y"), "M", NULL),
         MA_MINORITY21_LC = if_else(minority_pctLC >= 40 | (minority_pctLC >= 25 & MACOSUB_INC_BELOW150_LC == "Y"), "M", NULL)) %>% 
  select(-starts_with("MACOSUB"), -NAME)



### ENGLISH LANGUAGE ISOLATION
# Download C16002. Household Language by Household Limited English Speaking Status. Note that this table is a collapsed version of table B16002. EPA and MA use the latter, but there is no significant difference since we are not interested in disaggregating categories.
eng_limited <- get_acs(geography = "block group", 
                       year = year, moe_level = moe, survey = survey,
                       variables = c("C16002_001",
                                     "C16002_004", 
                                     "C16002_007",
                                     "C16002_010",
                                     "C16002_013"), 
                       state = "MA")

# Isolate limited English speaking households and compute derived estimates and MOEs
eng_limited_est <- eng_limited %>% 
  filter(variable != "C16002_001") %>% 
  group_by(GEOID) %>% 
  summarize(eng_limitE = sum(estimate),
            eng_limitM = moe_sum(moe,estimate))
# Join with total households and calculate derived proportions and MOEs, along with upper and lower confidence interval values from MOE. Rename columns and remove proportion variables. 
eng_limited_pct <- eng_limited %>% 
  filter(variable == "C16002_001") %>% 
  group_by(GEOID) %>% 
  left_join(., eng_limited_est, by = "GEOID") %>% 
  transmute(eng_hhE = estimate,
            eng_hhM = moe,
            eng_hh_UC = estimate + moe,
            eng_hh_LC = ifelse(estimate < moe, 0, estimate - moe),
            eng_limitE = eng_limitE,
            eng_limitM = eng_limitM,
            eng_limitE_UC = eng_limitE + eng_limitM,
            eng_limitE_LC = if_else(eng_limitE < eng_limitM, 0,
                                    eng_limitE - eng_limitM),
            eng_li_pE = ifelse(estimate==0,0,eng_limitE/estimate),
            eng_li_pM = moe_prop(eng_limitE,estimate,eng_limitM,moe),
            eng_limit_pctE = eng_li_pE*100,
            eng_limit_pctM = eng_li_pM*100,
            eng_limit_pctE_UC = eng_limit_pctE + eng_limit_pctM,
            eng_limit_pctE_LC = ifelse(
              eng_limit_pctE < eng_limit_pctM, 0, 
              eng_limit_pctE - eng_limit_pctM)) %>% 
  select(-eng_li_pE,-eng_li_pM)
# add variables to identify EJ criteria thresholds
eng_limited_pct <- eng_limited_pct %>% 
  mutate(MA_ENGLISH = if_else(eng_limit_pctE >= 25, "E", NULL),
         MA_ENGLISH_UC = if_else(eng_limit_pctE_UC >= 25, "E", NULL),
         MA_ENGLISH_LC = if_else(eng_limit_pctE_LC >= 25, "E", NULL))
# clean up
# rm(eng_limited,eng_limited_est)


### EDUCATIONAL ATTAINMENT FOR THOSE AGE 25+
# Less than high school education: The number or percent of people age 25 or older in a block group whose education is short of a high school diploma.
# Download Table B15002 SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
B15002 <- get_acs(geography = "block group", table = "B15002", 
                  year = year, moe_level = moe, survey = survey,
                  state = "MA")

# Isolate universe of population 25+
age25up <- B15002 %>% 
  filter(variable == "B15002_001") %>% 
  transmute(GEOID = GEOID,
            age25upE = estimate,
            age25upM = moe,
            age25upE_UC = age25upE + age25upM,
            age25upE_LC = ifelse(
              age25upE < age25upM, 0, age25upE - age25upM))
# Isolate populations with less than HS diploma
# create vector of patterns for male and female variables less than HS
lths_strings <- rep(c(3:10,20:27)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
lths_num <- B15002 %>% 
  filter(str_detect(variable,paste(lths_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(lthsE = sum(estimate),
            lthsM = moe_sum(moe,estimate)) %>% 
  mutate(lthsE_UC = lthsE + lthsM,
         lthsE_LC = ifelse(
           lthsE < lthsM, 0, lthsE - lthsM))
# Isolate populations with college degree or higher
# create vector of patterns for male and female variables with college+
col_strings <- rep(c(15:18,32:35)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
col_num <- B15002 %>% 
  filter(str_detect(variable,paste(col_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(collegeE = sum(estimate),
            collegeM = moe_sum(moe,estimate)) %>% 
  mutate(collegeE_UC = collegeE + collegeM,
         collegeE_LC = ifelse(
           collegeE < collegeM, 0, collegeE - collegeM))
# Join tables and compute derived proportion and MOE
lths_pct <- age25up %>% 
  left_join(.,lths_num, by = "GEOID") %>% 
  mutate(r_lthsE = ifelse(
    age25upE == 0, 0, lthsE/age25upE),
    r_lthsM = moe_ratio(lthsE,age25upE,lthsM,age25upM),
    pct_lthsE = r_lthsE * 100,
    pct_lthsM = r_lthsM * 100,
    pct_lthsE_UC = pct_lthsE + pct_lthsM,
    pct_lthsE_LC = ifelse(
      pct_lthsE < pct_lthsM, 0, pct_lthsE - pct_lthsM)) %>% 
  left_join(.,col_num, by = "GEOID") %>% 
  mutate(r_collegeE = ifelse(
    age25upE == 0, 0, collegeE/age25upE),
    r_collegeM = moe_ratio(collegeE,age25upE,collegeM,age25upM),
    pct_collegeE = r_collegeE * 100,
    pct_collegeM = r_collegeM * 100,
    pct_collegeE_UC = pct_collegeE + pct_collegeM,
    pct_collegeE_LC = ifelse(
      pct_collegeE < pct_collegeM, 0, pct_collegeE - pct_collegeM)) %>% 
  select(-starts_with("r_"))
# clean up
# rm(age25up,B15002,lths_num,lths_strings,col_num,col_strings)


### AGE UNDER 5 AND OVER 64
# Individuals under age 5: The number or percent of people in a block group under the age of 5.
# Individuals over age 64: The number or percent of people in a block group over the age of 64.
# Download Table B01001 TOTAL POPULATION COUNTS AND AGES
B01001 <- get_acs(geography = "block group", table = "B01001",
                  year = year, moe_level = moe, survey = survey,
                  state = "MA")

# Isolate universe of population for all sex and ages
allAges <- B01001 %>% 
  filter(variable == "B01001_001") %>% 
  transmute(GEOID = GEOID,
            allAgesE = estimate,
            allAgesM = moe,
            allAgesE_UC = allAgesE + allAgesM,
            allAgesE_LC = ifelse(
              allAgesE < allAgesM, 0, allAgesE - allAgesM))
# Isolate under 5 pop, compute derived sum estimates and MOEs
under5 <- B01001 %>% 
  filter(variable %in% c("B01001_003", "B01001_027")) %>% 
  group_by(GEOID) %>% 
  summarize(under5E = sum(estimate),
            under5M = moe_sum(moe,estimate)) %>% 
  mutate(under5E_UC = under5E + under5M,
         under5E_LC = ifelse(
           under5E < under5M, 0, under5E - under5M))

# Isolate over 64 pop
# create vector of patterns for male and female variables 65+
ovr64_strings <- rep(c(20:25,44:49)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over64 <- B01001 %>% 
  filter(str_detect(variable,paste(ovr64_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(over64E = sum(estimate),
            over64M = moe_sum(moe,estimate)) %>% 
  mutate(over64E_UC = over64E + over64M,
         over64E_LC = ifelse(
           over64E < over64M, 0, over64E - over64M))
# Join the tables and compute derived proportions with MOEs
age5_64_pct <- allAges %>% 
  left_join(., under5, by = "GEOID") %>% 
  mutate(r_under5E = ifelse(
    allAgesE == 0, 0, under5E/allAgesE),
    r_under5M = moe_ratio(under5E,allAgesE,under5M,allAgesM),
    pct_under5E = r_under5E * 100,
    pct_under5M = r_under5M * 100,
    pct_under5E_UC = pct_under5E + pct_under5M,
    pct_under5E_LC = ifelse(
      pct_under5E < pct_under5M, 0, pct_under5E - pct_under5M)) %>% 
  left_join(., over64, by = "GEOID") %>% 
  mutate(r_over64E = ifelse(
    allAgesE == 0, 0, over64E/allAgesE),
    r_over64M = moe_ratio(over64E,allAgesE,over64M,allAgesM),
    pct_over64E = r_over64E * 100,
    pct_over64M = r_over64M * 100,
    pct_over64E_UC = pct_over64E + pct_over64M,
    pct_over64E_LC = ifelse(
      pct_over64E < pct_over64M, 0, pct_over64E - pct_over64M)) %>% 
  select(-starts_with("r_"))
# clean up
# rm(allAges,B01001,over64,under5,ovr64_strings)


### MEDIAN AGE OF HOUSING 
# Based on median year structure built. 
housing_age <- get_acs(geography = "block group", 
                       year = year, moe_level = moe, survey = survey,
                     variables = c(housing_yr_built = "B25035_001"), 
                     state = "MA", output = "wide") %>% 
  mutate(housing_age_est = if_else(housing_yr_builtE > 1700, 2020 - housing_yr_builtE, NULL),
         housing_age_est_UC = housing_age_est + housing_yr_builtM,
         housing_age_est_LC = housing_age_est - housing_yr_builtM) %>% 
  select(-NAME)


### TENURE
# Based on number and percent of renters in occupied housing units
renters <- get_acs(geography = "block group", 
                   year = year, moe_level = moe, survey = survey,
                   variables = c(total_occ_units = "B25003_001",
                                 renter_occ_units = "B25003_003"),
                   state = "MA", output = "wide") %>% 
  mutate(total_occ_units_UC = total_occ_unitsE + total_occ_unitsM,
         total_occ_units_LC = if_else(total_occ_unitsE < total_occ_unitsM, 0,
                                      total_occ_unitsE - total_occ_unitsM),
         renter_occ_units_UC = renter_occ_unitsE + renter_occ_unitsM,
         renter_occ_units_LC = if_else(renter_occ_unitsE < renter_occ_unitsM, 0,
                                       renter_occ_unitsE - renter_occ_unitsM),
         renter_occ_units_p = if_else(total_occ_unitsE <= 0, 0, 
                                        renter_occ_unitsE/total_occ_unitsE),
         renter_occ_units_p_moe = moe_prop(num = renter_occ_unitsE,
                                           denom = total_occ_unitsE,
                                           moe_num = renter_occ_unitsM,
                                           moe_denom = total_occ_unitsM),
         renter_occ_units_pct = renter_occ_units_p * 100,
         renter_occ_units_pct_UC = 
           (renter_occ_units_p + renter_occ_units_p_moe) * 100,
         renter_occ_units_pct_LC = if_else(
           renter_occ_units_p < renter_occ_units_p_moe, 0, 
           (renter_occ_units_p - renter_occ_units_p_moe) * 100)) %>% 
  select(-NAME)


######### JOIN DATA FRAMES TO POLYGONS #############

# join demographic df to block groups
ma_blkgrps <- ma_blkgrps %>% 
  # select(-starts_with("total")) %>% 
  left_join(., race_pct, by = "GEOID") %>% 
  left_join(., medhhinclt50_pct, by = "GEOID") %>%
  left_join(., age5_64_pct, by = "GEOID") %>% 
  left_join(., eng_limited_pct, by = "GEOID") %>% 
  left_join(., poverty_pct, by = "GEOID") %>% 
  left_join(., lths_pct, by = "GEOID") %>% 
  left_join(., renters, by = "GEOID") %>%
  left_join(., housing_age, by = "GEOID")


# save output
save(ma_blkgrps, file = "/Data/Demographics.rds")
# clear environment
# rm(list = ls())



### DEMOGRAPHICS AT TRACT LEVEL

# Tracts
ma_tracts <- get_acs(geography = "tract", 
                       year = year, moe_level = moe, survey = survey,
                       variables = c(totalpop = "B03002_001", 
                                     medhhinc = "B19013_001"),
                       state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$')) %>% 
  st_transform(., crs = 2805)


### RATIO OF INCOME TO POVERTY LEVEL
# Download ratio of income to poverty level in the past 12 months to calculate the number or percent of a tract’s population in households where the household income is less than or equal to twice the federal “poverty level.” More precisely, percent low-income is calculated as a percentage of those for whom the poverty ratio was known, as reported by the Census Bureau, which may be less than the full population in some tracts. More information on the federally-defined poverty threshold is available at http://www.census.gov/hhes/www/poverty/methods/definitions.html. Note also that poverty status is not determined for people living in institutional group quarters (i.e. prisons, college dormitories, military barracks, nursing homes), so these populations are not included in the poverty estimates (https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html).
# First, download table of ratio of income to poverty level
C17002 <- get_acs(geography = "tract", table = "C17002", 
                  year = year, moe_level = moe, survey = survey,
                  state = "MA")
# Isolate universe pop for whom poverty status is known
povknown <- C17002 %>% 
  filter(variable == "C17002_001") %>% 
  transmute(GEOID = GEOID,
            povknownE = estimate,
            povknownM = moe,
            povknownE_UC = povknownE + povknownM,
            povknownE_LC = ifelse(
              povknownE < povknownM, 0, povknownE - povknownM))
# Isolate population less than 2x poverty level and compute derived sum esimate along with MOE and UC and LC
num2pov <- C17002 %>% 
  filter(!variable %in% c("C17002_001", "C17002_008")) %>% 
  group_by(GEOID) %>% 
  summarize(num2povE = sum(estimate),
            num2povM = moe_sum(moe, estimate)) %>% 
  mutate(num2povE_UC = num2povE + num2povM,
         num2povE_LC = ifelse(
           num2povE < num2povM, 0, num2povE - num2povM))
# Join tables and compute derived ratios and MOEs and then pcts with UC and LC
poverty_pct <- povknown %>% 
  left_join(., num2pov, by = "GEOID") %>% 
  mutate(r2povE = ifelse(
    povknownE == 0, 0, num2povE/povknownE),
    r2povM = moe_ratio(num2povE,povknownE,num2povM,povknownM),
    pct2povE = r2povE * 100,
    pct2povM = r2povM * 100,
    pct2povE_UC = pct2povE + pct2povM,
    pct2povE_LC = ifelse(
      pct2povE < pct2povM, 0, pct2povE - pct2povM)) %>% 
  select(-starts_with("r2p"))
# clean up
# rm(C17002,num2pov,povknown)


### RACE AND ETHNICITY
# Download B03002 HISPANIC OR LATINO ORIGIN BY RACE in two sets. 
# Start with total pop and all races in wide format and compute upper and lower confidence values. 
B03002_totrace <- get_acs(geography = "tract", 
                          year = year, moe_level = moe, survey = survey,
                          variables = c(
  totalpop = "B03002_001",
  nhwhitepop = "B03002_003",
  nhblackpop = "B03002_004",
  nhamerindpop = "B03002_005",
  nhasianpop = "B03002_006",
  nhnativpop = "B03002_007",
  nhotherpop = "B03002_008",
  nh2morepop = "B03002_009",
  hisppop = "B03002_012"),
  state = "MA") %>% 
  mutate(UC = estimate + moe,
         LC = if_else(estimate < moe, 0, estimate - moe)) %>% 
  rename(E = estimate, M = moe) %>% 
  pivot_wider(names_from = "variable", 
              names_glue = "{variable}_{.value}",
              values_from = c(E, M, UC, LC))

# Next acquire estimates for Hispanic and nonWhite groups in long format to create aggregated minority variable
B03002_minority <- get_acs(geography = "tract", 
                           year = year, moe_level = moe, survey = survey,
                           variables = c(
  nhblackpop = "B03002_004",
  nhamerindpop = "B03002_005",
  nhasianpop = "B03002_006",
  nhnativhpop = "B03002_007",
  nhotherpop = "B03002_008",
  nh2morepop = "B03002_009",
  hisppop = "B03002_012"),
  state = "MA") %>% 
  group_by(GEOID) %>% 
  summarize(minority_E = sum(estimate),
            minority_M = moe_sum(moe,estimate)) %>% 
  mutate(minority_UC = minority_E + minority_M,
         minority_LC = ifelse(
           minority_E < minority_M, 0, minority_E - minority_M))

# Join with all race pops and compute proportions
race_pct <- B03002_totrace %>% 
  left_join(., B03002_minority, by = "GEOID") %>% 
  mutate(across(ends_with("_E"),
                ~ if_else(totalpop_E == 0, 0, .x/totalpop_E), 
                .names = "{col}_p"))

# Compute MOEs for derived proportions
# First, extract unique names for variables to be computed
unique_names <- race_pct %>% 
  select(-ends_with("_p")) %>% 
  names() %>% 
  str_extract(.,"^.+(?=_)") %>% 
  unique() %>% 
  .[!is.na(.)]

# Next, subset data frame to estimates and moe variables only
estimatesDF <- race_pct %>% 
  select(-c(GEOID,NAME), -ends_with(c("UC","LC","_p")))

# Use purrr::map_dfc to match unique names to variables and pass along to moe_prop function and cbind back to race_pct df and then convert proportion to percentages
race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                      select(matches(.x), totalpop_E, totalpop_M) %>%
                      mutate(!!paste0(.x, "_M_p") := 
                               moe_prop(num = .[[1]], 
                                        denom = totalpop_E, 
                                        moe_num = .[[2]], 
                                        moe_denom = totalpop_M))) %>% 
  select(ends_with("_p")) %>% 
  cbind(race_pct, .) %>% 
  mutate(across(ends_with("_p"), ~(.x * 100)))

# Calculate upper and lower confidence values for percentages
# First, extract unique names for variables to be computed
unique_names <- race_pct %>% 
  select(-ends_with("_p")) %>% 
  names() %>% 
  str_extract(.,"^.+(?=_)") %>% 
  unique() %>% 
  .[!is.na(.)]

# Next, subset data frame to estimates and moe variables only
estimatesDF <- race_pct %>% 
  select(ends_with("_p"))

# Match unique names to variables and pass along to calculate upper and lower estimates and cbind back to race_pct
race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                      select(matches(.x)) %>% 
                      mutate(!!paste0(.x, "_pctUC") := 
                               .[[1]] + .[[2]])) %>% 
  select(ends_with("_pctUC")) %>% 
  cbind(race_pct, .)

race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                      select(matches(.x)) %>% 
                      mutate(!!paste0(.x, "_pctLC") := 
                               if_else(.[[1]] < .[[2]], 0, .[[1]] - .[[2]]))) %>% 
  select(ends_with("_pctLC")) %>% 
  cbind(race_pct, .) %>% 
  select(-NAME)

# clean up
# rm(list = ls(pattern = paste(c("B03002","estimatesDF","unique"), 
#                              collapse = "|")))


### ENGLISH LANGUAGE ISOLATION
# Download C16002. Household Language by Household Limited English Speaking Status. Note that this table is a collapsed version of table B16002. EPA and MA use the latter, but there is no significant difference since we are not interested in disaggregating categories.
eng_limited <- get_acs(geography = "tract", 
                       year = year, moe_level = moe, survey = survey,
                       variables = c("C16002_001",
                                     "C16002_004", 
                                     "C16002_007",
                                     "C16002_010",
                                     "C16002_013"), 
                       state = "MA")

# Isolate limited English speaking households and compute derived estimates and MOEs
eng_limited_est <- eng_limited %>% 
  filter(variable != "C16002_001") %>% 
  group_by(GEOID) %>% 
  summarize(eng_limitE = sum(estimate),
            eng_limitM = moe_sum(moe,estimate))
# Join with total households and calculate derived proportions and MOEs, along with upper and lower confidence interval values from MOE. Rename columns and remove proportion variables. 
eng_limited_pct <- eng_limited %>% 
  filter(variable == "C16002_001") %>% 
  group_by(GEOID) %>% 
  left_join(., eng_limited_est, by = "GEOID") %>% 
  transmute(eng_hhE = estimate,
            eng_hhM = moe,
            eng_hh_UC = estimate + moe,
            eng_hh_LC = ifelse(estimate < moe, 0, estimate - moe),
            eng_limitE = eng_limitE,
            eng_limitM = eng_limitM,
            eng_limitE_UC = eng_limitE + eng_limitM,
            eng_limitE_LC = if_else(eng_limitE < eng_limitM, 0,
                                    eng_limitE - eng_limitM),
            eng_li_pE = ifelse(estimate==0,0,eng_limitE/estimate),
            eng_li_pM = moe_prop(eng_limitE,estimate,eng_limitM,moe),
            eng_limit_pctE = eng_li_pE*100,
            eng_limit_pctM = eng_li_pM*100,
            eng_limit_pctE_UC = eng_limit_pctE + eng_limit_pctM,
            eng_limit_pctE_LC = ifelse(
              eng_limit_pctE < eng_limit_pctM, 0, 
              eng_limit_pctE - eng_limit_pctM)) %>% 
  select(-eng_li_pE,-eng_li_pM)

# clean up
# rm(eng_limited,eng_limited_est)


### EDUCATIONAL ATTAINMENT FOR THOSE AGE 25+
# Less than high school education: The number or percent of people age 25 or older in a tract whose education is short of a high school diploma.
# Download Table B15002 SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
B15002 <- get_acs(geography = "tract", table = "B15002", 
                  year = year, moe_level = moe, survey = survey, 
                  state = "MA")

# Isolate universe of population 25+
age25up <- B15002 %>% 
  filter(variable == "B15002_001") %>% 
  transmute(GEOID = GEOID,
            age25upE = estimate,
            age25upM = moe,
            age25upE_UC = age25upE + age25upM,
            age25upE_LC = ifelse(
              age25upE < age25upM, 0, age25upE - age25upM))
# Isolate populations with less than HS diploma
# create vector of patterns for male and female variables less than HS
lths_strings <- rep(c(3:10,20:27)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
lths_num <- B15002 %>% 
  filter(str_detect(variable,paste(lths_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(lthsE = sum(estimate),
            lthsM = moe_sum(moe,estimate)) %>% 
  mutate(lthsE_UC = lthsE + lthsM,
         lthsE_LC = ifelse(
           lthsE < lthsM, 0, lthsE - lthsM))
# Isolate populations with college degree or higher
# create vector of patterns for male and female variables with college+
col_strings <- rep(c(15:18,32:35)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
col_num <- B15002 %>% 
  filter(str_detect(variable,paste(col_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(collegeE = sum(estimate),
            collegeM = moe_sum(moe,estimate)) %>% 
  mutate(collegeE_UC = collegeE + collegeM,
         collegeE_LC = ifelse(
           collegeE < collegeM, 0, collegeE - collegeM))
# Join tables and compute derived proportion and MOE
lths_pct <- age25up %>% 
  left_join(.,lths_num, by = "GEOID") %>% 
  mutate(r_lthsE = ifelse(
    age25upE == 0, 0, lthsE/age25upE),
    r_lthsM = moe_ratio(lthsE,age25upE,lthsM,age25upM),
    pct_lthsE = r_lthsE * 100,
    pct_lthsM = r_lthsM * 100,
    pct_lthsE_UC = pct_lthsE + pct_lthsM,
    pct_lthsE_LC = ifelse(
      pct_lthsE < pct_lthsM, 0, pct_lthsE - pct_lthsM)) %>% 
  left_join(.,col_num, by = "GEOID") %>% 
  mutate(r_collegeE = ifelse(
    age25upE == 0, 0, collegeE/age25upE),
    r_collegeM = moe_ratio(collegeE,age25upE,collegeM,age25upM),
    pct_collegeE = r_collegeE * 100,
    pct_collegeM = r_collegeM * 100,
    pct_collegeE_UC = pct_collegeE + pct_collegeM,
    pct_collegeE_LC = ifelse(
      pct_collegeE < pct_collegeM, 0, pct_collegeE - pct_collegeM)) %>% 
  select(-starts_with("r_"))
# clean up
# rm(age25up,B15002,lths_num,lths_strings,col_num,col_strings)


### AGE UNDER 5 AND OVER 64
# Individuals under age 5: The number or percent of people in a tract under the age of 5.
# Individuals over age 64: The number or percent of people in a tract over the age of 64.
# Download Table B01001 TOTAL POPULATION COUNTS AND AGES
B01001 <- get_acs(geography = "tract", table = "B01001", 
                  year = year, moe_level = moe, survey = survey, 
                  state = "MA")

# Isolate universe of population for all sex and ages
allAges <- B01001 %>% 
  filter(variable == "B01001_001") %>% 
  transmute(GEOID = GEOID,
            allAgesE = estimate,
            allAgesM = moe,
            allAgesE_UC = allAgesE + allAgesM,
            allAgesE_LC = ifelse(
              allAgesE < allAgesM, 0, allAgesE - allAgesM))
# Isolate under 5 pop, compute derived sum estimates and MOEs
under5 <- B01001 %>% 
  filter(variable %in% c("B01001_003", "B01001_027")) %>% 
  group_by(GEOID) %>% 
  summarize(under5E = sum(estimate),
            under5M = moe_sum(moe,estimate)) %>% 
  mutate(under5E_UC = under5E + under5M,
         under5E_LC = ifelse(
           under5E < under5M, 0, under5E - under5M))

# Isolate over 64 pop
# create vector of patterns for male and female variables 65+
ovr64_strings <- rep(c(20:25,44:49)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over64 <- B01001 %>% 
  filter(str_detect(variable,paste(ovr64_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(over64E = sum(estimate),
            over64M = moe_sum(moe,estimate)) %>% 
  mutate(over64E_UC = over64E + over64M,
         over64E_LC = ifelse(
           over64E < over64M, 0, over64E - over64M))
# Join the tables and compute derived proportions with MOEs
age5_64_pct <- allAges %>% 
  left_join(., under5, by = "GEOID") %>% 
  mutate(r_under5E = ifelse(
    allAgesE == 0, 0, under5E/allAgesE),
    r_under5M = moe_ratio(under5E,allAgesE,under5M,allAgesM),
    pct_under5E = r_under5E * 100,
    pct_under5M = r_under5M * 100,
    pct_under5E_UC = pct_under5E + pct_under5M,
    pct_under5E_LC = ifelse(
      pct_under5E < pct_under5M, 0, pct_under5E - pct_under5M)) %>% 
  left_join(., over64, by = "GEOID") %>% 
  mutate(r_over64E = ifelse(
    allAgesE == 0, 0, over64E/allAgesE),
    r_over64M = moe_ratio(over64E,allAgesE,over64M,allAgesM),
    pct_over64E = r_over64E * 100,
    pct_over64M = r_over64M * 100,
    pct_over64E_UC = pct_over64E + pct_over64M,
    pct_over64E_LC = ifelse(
      pct_over64E < pct_over64M, 0, pct_over64E - pct_over64M)) %>% 
  select(-starts_with("r_"))
# clean up
# rm(allAges,B01001,over64,under5,ovr64_strings)


### MEDIAN AGE OF HOUSING 
# Based on median year structure built. 
housing_age <- get_acs(geography = "tract", 
                       year = year, moe_level = moe, survey = survey,
                       variables = c(housing_yr_built = "B25035_001"), 
                       state = "MA", output = "wide") %>% 
  mutate(housing_age_est = if_else(housing_yr_builtE > 1700, 2020 - housing_yr_builtE, NULL),
         housing_age_est_UC = housing_age_est + housing_yr_builtM,
         housing_age_est_LC = housing_age_est - housing_yr_builtM) %>% 
  select(-NAME)


### TENURE
# Based on number and percent of renters in occupied housing units
renters <- get_acs(geography = "tract", 
                   year = year, moe_level = moe, survey = survey,
                   variables = c(total_occ_units = "B25003_001",
                                 renter_occ_units = "B25003_003"),
                   state = "MA", output = "wide") %>% 
  mutate(total_occ_units_UC = total_occ_unitsE + total_occ_unitsM,
         total_occ_units_LC = if_else(total_occ_unitsE < total_occ_unitsM, 0,
                                      total_occ_unitsE - total_occ_unitsM),
         renter_occ_units_UC = renter_occ_unitsE + renter_occ_unitsM,
         renter_occ_units_LC = if_else(renter_occ_unitsE < renter_occ_unitsM, 0, 
                                       renter_occ_unitsE - renter_occ_unitsM),
         renter_occ_units_p = if_else(total_occ_unitsE <= 0, 0, 
                                      renter_occ_unitsE/total_occ_unitsE),
         renter_occ_units_p_moe = moe_prop(num = renter_occ_unitsE,
                                           denom = total_occ_unitsE,
                                           moe_num = renter_occ_unitsM,
                                           moe_denom = total_occ_unitsM),
         renter_occ_units_pct = renter_occ_units_p * 100,
         renter_occ_units_pct_UC = 
           (renter_occ_units_p + renter_occ_units_p_moe) * 100,
         renter_occ_units_pct_LC = if_else(
           renter_occ_units_p < renter_occ_units_p_moe, 0, 
           (renter_occ_units_p - renter_occ_units_p_moe) * 100)) %>% 
  select(-NAME)



### HOUSING COST BURDENED
# NOTE THAT THIS DATA IS ONLY AVAILABLE AT TRACT LEVEL, NOT BLKGRP
# Based on tenure by housing cost as a percentage of income in the last 12 months for those paying 30% or more of income
# First compute derived sum and moe for house cost burdened as a group
house_burdened <- get_acs(geography = "tract", 
                          year = year, moe_level = moe, survey = survey,
                          variables = c("B25106_006", 
                                        "B25106_010",
                                        "B25106_014",
                                        "B25106_018",
                                        "B25106_022",
                                        "B25106_028",
                                        "B25106_032",
                                        "B25106_036",
                                        "B25106_040",
                                        "B25106_044"), 
                          state = "MA") %>% 
  group_by(GEOID) %>% 
  summarize(house_burdened_E = sum(estimate),
            house_burdened_M = moe_sum(moe,estimate)) %>% 
  mutate(house_burdened_UC = house_burdened_E + house_burdened_M,
         house_burdened_LC = ifelse(
           house_burdened_E < house_burdened_M, 0, 
           house_burdened_E - house_burdened_M))

# Next, acquire universe estimate to calculate percentages
house_burdened <- get_acs(geography = "tract", 
                          year = year, moe_level = moe, survey = survey,
                          variables = c(occ_housing = "B25106_001"), 
                          state = "MA", output = "wide") %>% 
  left_join(house_burdened, ., by = "GEOID") %>% 
  mutate(house_burdened_p = if_else(house_burdened_E < occ_housingE, 0,
                                    house_burdened_E/occ_housingE),
         house_burdened_p_moe = moe_prop(num = house_burdened_E,
                                         denom = occ_housingE,
                                         moe_num = house_burdened_M,
                                         moe_denom = occ_housingM),
         house_burdened_pct = house_burdened_p * 100,
         house_burdened_pct_UC = 
           (house_burdened_p + house_burdened_p_moe) * 100,
         house_burdened_pct_LC = if_else(
           house_burdened_p < house_burdened_p_moe, 0, 
           (house_burdened_p - house_burdened_p_moe) * 100)) %>% 
  select(-NAME)


### DISABILITY
# NOTE THAT THIS DATA IS ONLY AVAILABLE AT TRACT LEVEL, NOT BLKGRP
# Number and percent of individuals 18+ with a disability
# Download Table B18101 SEX BY AGE BY DISABILITY STATUS
B18101 <- get_acs(geography = "tract", table = "B18101", 
                  year = year, moe_level = moe, survey = survey, 
                  state = "MA")

# Isolate disabled and non-disabled population 18+
# create vector of patterns for male and female variables 18+
ovr18_strings <- rep(c(9:20,28:39)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over18 <- B18101 %>% 
  filter(str_detect(variable,paste(ovr18_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(Over18E = sum(estimate),
            Over18M = moe_sum(moe,estimate)) %>% 
  mutate(Over18E_UC = Over18E + Over18M,
         Over18E_LC = ifelse(
           Over18E < Over18M, 0, 
           Over18E - Over18M))
# compute derived sum and moe for those over 18 with a disability only
# create vector of patterns for male and female variables 18+ with disability
disabledOvr18_strings <- sort(c(seq(from = 10, to = 19, by = 3), 
                                seq(from = 29, to = 38, by = 3))) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
disabledOver18 <- B18101 %>%
  filter(str_detect(variable,paste(disabledOvr18_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(disabledOver18E = sum(estimate),
            disabledOver18M = moe_sum(moe,estimate)) %>% 
  mutate(disabledOver18E_UC = disabledOver18E + disabledOver18M,
         disabledOver18E_LC = ifelse(
           disabledOver18E < disabledOver18M, 0, 
           disabledOver18E - disabledOver18M))
# Join the tables and compute derived proportions with MOEs
disabilityOver18_pct <- over18 %>% 
  left_join(., disabledOver18, by = "GEOID") %>% 
  mutate(r_disabilityOver18E = ifelse(
    Over18E == 0, 0, disabledOver18E/Over18E),
    r_disabilityOver18M = moe_ratio(
      disabledOver18E,Over18E,disabledOver18M,Over18M),
    pct_disabilityOver18E = r_disabilityOver18E * 100,
    pct_disabilityOver18M = r_disabilityOver18M * 100,
    pct_disabilityOver18E_UC = pct_disabilityOver18E + pct_disabilityOver18M,
    pct_disabilityOver18E_LC = ifelse(
      pct_disabilityOver18E < pct_disabilityOver18M, 0, 
      pct_disabilityOver18E - pct_disabilityOver18M))%>% 
  select(-starts_with("r_"))
# clean up
# rm(B18101,disabledOver18,disabledOvr18_strings,over18,ovr18_strings)


######### JOIN DATA FRAMES TO POLYGONS #############

# join demographic df to block groups
ma_tracts <- ma_tracts %>% 
  left_join(., race_pct, by = "GEOID") %>% 
  left_join(., age5_64_pct, by = "GEOID") %>% 
  left_join(., eng_limited_pct, by = "GEOID") %>% 
  left_join(., poverty_pct, by = "GEOID") %>% 
  left_join(., lths_pct, by = "GEOID") %>% 
  left_join(., renters, by = "GEOID") %>%
  left_join(., housing_age, by = "GEOID") %>% 
  left_join(., house_burdened, by = "GEOID") %>% 
  left_join(., disabilityOver18_pct, by = "GEOID")

# save output
# load("/Data/Demographics.rds")
save(ma_blkgrps, ma_tracts, file = "/Data/Demographics.rds")
# clear environment
# rm(list = ls())





### DEMOGRAPHICS AT COUNTY SUBDIVISION LEVEL

# County subdivisions (i.e., cities and towns)
ma_cosub <- get_acs(geography = "county subdivision", 
                      year = year, moe_level = moe, survey = survey,
                      variables = c(totalpop = "B03002_001", 
                                    medhhinc = "B19013_001"),
                      state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$')) %>% 
  st_transform(., crs = 2805)


### RATIO OF INCOME TO POVERTY LEVEL
# Download ratio of income to poverty level in the past 12 months to calculate the number or percent of a county subdivision’s population in households where the household income is less than or equal to twice the federal “poverty level.” More precisely, percent low-income is calculated as a percentage of those for whom the poverty ratio was known, as reported by the Census Bureau, which may be less than the full population in some tracts. More information on the federally-defined poverty threshold is available at http://www.census.gov/hhes/www/poverty/methods/definitions.html. Note also that poverty status is not determined for people living in institutional group quarters (i.e. prisons, college dormitories, military barracks, nursing homes), so these populations are not included in the poverty estimates (https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html).
# First, download table of ratio of income to poverty level
C17002 <- get_acs(geography = "county subdivision", table = "C17002", 
                  year = year, moe_level = moe, survey = survey, 
                  state = "MA")
# Isolate universe pop for whom poverty status is known
povknown <- C17002 %>% 
  filter(variable == "C17002_001") %>% 
  transmute(GEOID = GEOID,
            povknownE = estimate,
            povknownM = moe,
            povknownE_UC = povknownE + povknownM,
            povknownE_LC = ifelse(
              povknownE < povknownM, 0, povknownE - povknownM))
# Isolate population less than 2x poverty level and compute derived sum esimate along with MOE and UC and LC
num2pov <- C17002 %>% 
  filter(!variable %in% c("C17002_001", "C17002_008")) %>% 
  group_by(GEOID) %>% 
  summarize(num2povE = sum(estimate),
            num2povM = moe_sum(moe, estimate)) %>% 
  mutate(num2povE_UC = num2povE + num2povM,
         num2povE_LC = ifelse(
           num2povE < num2povM, 0, num2povE - num2povM))
# Join tables and compute derived ratios and MOEs and then pcts with UC and LC
poverty_pct <- povknown %>% 
  left_join(., num2pov, by = "GEOID") %>% 
  mutate(r2povE = ifelse(
    povknownE == 0, 0, num2povE/povknownE),
    r2povM = moe_ratio(num2povE,povknownE,num2povM,povknownM),
    pct2povE = r2povE * 100,
    pct2povM = r2povM * 100,
    pct2povE_UC = pct2povE + pct2povM,
    pct2povE_LC = ifelse(
      pct2povE < pct2povM, 0, pct2povE - pct2povM)) %>% 
  select(-starts_with("r2p"))
# clean up
# rm(C17002,num2pov,povknown)


### RACE AND ETHNICITY
# Download B03002 HISPANIC OR LATINO ORIGIN BY RACE in two sets. 
# Start with total pop and all races in wide format and compute upper and lower confidence values. 
B03002_totrace <- get_acs(geography = "county subdivision", 
                          year = year, moe_level = moe, survey = survey, 
                          variables = c(
  totalpop = "B03002_001",
  nhwhitepop = "B03002_003",
  nhblackpop = "B03002_004",
  nhamerindpop = "B03002_005",
  nhasianpop = "B03002_006",
  nhnativpop = "B03002_007",
  nhotherpop = "B03002_008",
  nh2morepop = "B03002_009",
  hisppop = "B03002_012"),
  state = "MA") %>% 
  mutate(UC = estimate + moe,
         LC = if_else(estimate < moe, 0, estimate - moe)) %>% 
  rename(E = estimate, M = moe) %>% 
  pivot_wider(names_from = "variable", 
              names_glue = "{variable}_{.value}",
              values_from = c(E, M, UC, LC))

# Next acquire estimates for Hispanic and nonWhite groups in long format to create aggregated minority variable
B03002_minority <- get_acs(geography = "county subdivision", 
                           year = year, moe_level = moe, survey = survey, 
                           variables = c(
  nhblackpop = "B03002_004",
  nhamerindpop = "B03002_005",
  nhasianpop = "B03002_006",
  nhnativhpop = "B03002_007",
  nhotherpop = "B03002_008",
  nh2morepop = "B03002_009",
  hisppop = "B03002_012"),
  state = "MA") %>% 
  group_by(GEOID) %>% 
  summarize(minority_E = sum(estimate),
            minority_M = moe_sum(moe,estimate)) %>% 
  mutate(minority_UC = minority_E + minority_M,
         minority_LC = ifelse(
           minority_E < minority_M, 0, minority_E - minority_M))

# Join with all race pops and compute proportions
race_pct <- B03002_totrace %>% 
  left_join(., B03002_minority, by = "GEOID") %>% 
  mutate(across(ends_with("_E"),
                ~ if_else(totalpop_E == 0, 0, .x/totalpop_E), 
                .names = "{col}_p")) 

# Compute MOEs for derived proportions
# First, excounty subdivision unique names for variables to be computed
unique_names <- race_pct %>% 
  select(-ends_with("_p")) %>% 
  names() %>% 
  str_extract(.,"^.+(?=_)") %>% 
  unique() %>% 
  .[!is.na(.)]

# Next, subset data frame to estimates and moe variables only
estimatesDF <- race_pct %>% 
  select(-c(GEOID,NAME), -ends_with(c("UC","LC","_p")))

# Use purrr::map_dfc to match unique names to variables and pass along to moe_prop function and cbind back to race_pct df and then convert proportion to percentages
race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                      select(matches(.x), totalpop_E, totalpop_M) %>%
                      mutate(!!paste0(.x, "_M_p") := 
                               moe_prop(num = .[[1]], 
                                        denom = totalpop_E, 
                                        moe_num = .[[2]], 
                                        moe_denom = totalpop_M))) %>% 
  select(ends_with("_p")) %>% 
  cbind(race_pct, .) %>% 
  mutate(across(ends_with("_p"), ~(.x * 100)))

# Calculate upper and lower confidence values for percentages
# First, excounty subdivision unique names for variables to be computed
unique_names <- race_pct %>% 
  select(-ends_with("_p")) %>% 
  names() %>% 
  str_extract(.,"^.+(?=_)") %>% 
  unique() %>% 
  .[!is.na(.)]

# Next, subset data frame to estimates and moe variables only
estimatesDF <- race_pct %>% 
  select(ends_with("_p"))

# Match unique names to variables and pass along to calculate upper and lower estimates and cbind back to race_pct
race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                      select(matches(.x)) %>% 
                      mutate(!!paste0(.x, "_pctUC") := 
                               .[[1]] + .[[2]])) %>% 
  select(ends_with("_pctUC")) %>% 
  cbind(race_pct, .)

race_pct <- map_dfc(unique_names, ~estimatesDF %>% 
                      select(matches(.x)) %>% 
                      mutate(!!paste0(.x, "_pctLC") := 
                               if_else(.[[1]] < .[[2]], 0, .[[1]] - .[[2]]))) %>% 
  select(ends_with("_pctLC")) %>% 
  cbind(race_pct, .) %>% 
  select(-NAME)

# clean up
# rm(list = ls(pattern = paste(c("B03002","estimatesDF","unique"), 
#                              collapse = "|")))


### ENGLISH LANGUAGE ISOLATION
# Download C16002. Household Language by Household Limited English Speaking Status. Note that this table is a collapsed version of table B16002. EPA and MA use the latter, but there is no significant difference since we are not interested in disaggregating categories.
eng_limited <- get_acs(geography = "county subdivision", 
                       year = year, moe_level = moe, survey = survey,
                       variables = c("C16002_001",
                                     "C16002_004", 
                                     "C16002_007",
                                     "C16002_010",
                                     "C16002_013"), 
                       state = "MA")

# Isolate limited English speaking households and compute derived estimates and MOEs
eng_limited_est <- eng_limited %>% 
  filter(variable != "C16002_001") %>% 
  group_by(GEOID) %>% 
  summarize(eng_limitE = sum(estimate),
            eng_limitM = moe_sum(moe,estimate))
# Join with total households and calculate derived proportions and MOEs, along with upper and lower confidence interval values from MOE. Rename columns and remove proportion variables. 
eng_limited_pct <- eng_limited %>% 
  filter(variable == "C16002_001") %>% 
  group_by(GEOID) %>% 
  left_join(., eng_limited_est, by = "GEOID") %>% 
  transmute(eng_hhE = estimate,
            eng_hhM = moe,
            eng_hh_UC = estimate + moe,
            eng_hh_LC = ifelse(estimate < moe, 0, estimate - moe),
            eng_limitE = eng_limitE,
            eng_limitM = eng_limitM,
            eng_limitE_UC = eng_limitE + eng_limitM,
            eng_limitE_LC = if_else(eng_limitE < eng_limitM, 0,
                                    eng_limitE - eng_limitM),
            eng_li_pE = ifelse(estimate==0,0,eng_limitE/estimate),
            eng_li_pM = moe_prop(eng_limitE,estimate,eng_limitM,moe),
            eng_limit_pctE = eng_li_pE*100,
            eng_limit_pctM = eng_li_pM*100,
            eng_limit_pctE_UC = eng_limit_pctE + eng_limit_pctM,
            eng_limit_pctE_LC = ifelse(
              eng_limit_pctE < eng_limit_pctM, 0, 
              eng_limit_pctE - eng_limit_pctM)) %>% 
  select(-eng_li_pE,-eng_li_pM)

# clean up
# rm(eng_limited,eng_limited_est)


### EDUCATIONAL ATTAINMENT FOR THOSE AGE 25+
# Less than high school education: The number or percent of people age 25 or older in a county subdivision whose education is short of a high school diploma.
# Download Table B15002 SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
B15002 <- get_acs(geography = "county subdivision", table = "B15002", 
                  year = year, moe_level = moe, survey = survey, 
                  state = "MA")

# Isolate universe of population 25+
age25up <- B15002 %>% 
  filter(variable == "B15002_001") %>% 
  transmute(GEOID = GEOID,
            age25upE = estimate,
            age25upM = moe,
            age25upE_UC = age25upE + age25upM,
            age25upE_LC = ifelse(
              age25upE < age25upM, 0, age25upE - age25upM))
# Isolate populations with less than HS diploma
# create vector of patterns for male and female variables less than HS
lths_strings <- rep(c(3:10,20:27)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
lths_num <- B15002 %>% 
  filter(str_detect(variable,paste(lths_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(lthsE = sum(estimate),
            lthsM = moe_sum(moe,estimate)) %>% 
  mutate(lthsE_UC = lthsE + lthsM,
         lthsE_LC = ifelse(
           lthsE < lthsM, 0, lthsE - lthsM))
# Isolate populations with college degree or higher
# create vector of patterns for male and female variables with college+
col_strings <- rep(c(15:18,32:35)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
col_num <- B15002 %>% 
  filter(str_detect(variable,paste(col_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(collegeE = sum(estimate),
            collegeM = moe_sum(moe,estimate)) %>% 
  mutate(collegeE_UC = collegeE + collegeM,
         collegeE_LC = ifelse(
           collegeE < collegeM, 0, collegeE - collegeM))
# Join tables and compute derived proportion and MOE
lths_pct <- age25up %>% 
  left_join(.,lths_num, by = "GEOID") %>% 
  mutate(r_lthsE = ifelse(
    age25upE == 0, 0, lthsE/age25upE),
    r_lthsM = moe_ratio(lthsE,age25upE,lthsM,age25upM),
    pct_lthsE = r_lthsE * 100,
    pct_lthsM = r_lthsM * 100,
    pct_lthsE_UC = pct_lthsE + pct_lthsM,
    pct_lthsE_LC = ifelse(
      pct_lthsE < pct_lthsM, 0, pct_lthsE - pct_lthsM)) %>% 
  left_join(.,col_num, by = "GEOID") %>% 
  mutate(r_collegeE = ifelse(
    age25upE == 0, 0, collegeE/age25upE),
    r_collegeM = moe_ratio(collegeE,age25upE,collegeM,age25upM),
    pct_collegeE = r_collegeE * 100,
    pct_collegeM = r_collegeM * 100,
    pct_collegeE_UC = pct_collegeE + pct_collegeM,
    pct_collegeE_LC = ifelse(
      pct_collegeE < pct_collegeM, 0, pct_collegeE - pct_collegeM)) %>% 
  select(-starts_with("r_"))
# clean up
# rm(age25up,B15002,lths_num,lths_strings,col_num,col_strings)


### AGE UNDER 5 AND OVER 64
# Individuals under age 5: The number or percent of people in a county subdivision under the age of 5.
# Individuals over age 64: The number or percent of people in a county subdivision over the age of 64.
# Download Table B01001 TOTAL POPULATION COUNTS AND AGES
B01001 <- get_acs(geography = "county subdivision", table = "B01001", 
                  year = year, moe_level = moe, survey = survey,
                  state = "MA")

# Isolate universe of population for all sex and ages
allAges <- B01001 %>% 
  filter(variable == "B01001_001") %>% 
  transmute(GEOID = GEOID,
            allAgesE = estimate,
            allAgesM = moe,
            allAgesE_UC = allAgesE + allAgesM,
            allAgesE_LC = ifelse(
              allAgesE < allAgesM, 0, allAgesE - allAgesM))
# Isolate under 5 pop, compute derived sum estimates and MOEs
under5 <- B01001 %>% 
  filter(variable %in% c("B01001_003", "B01001_027")) %>% 
  group_by(GEOID) %>% 
  summarize(under5E = sum(estimate),
            under5M = moe_sum(moe,estimate)) %>% 
  mutate(under5E_UC = under5E + under5M,
         under5E_LC = ifelse(
           under5E < under5M, 0, under5E - under5M))

# Isolate over 64 pop
# create vector of patterns for male and female variables 65+
ovr64_strings <- rep(c(20:25,44:49)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over64 <- B01001 %>% 
  filter(str_detect(variable,paste(ovr64_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(over64E = sum(estimate),
            over64M = moe_sum(moe,estimate)) %>% 
  mutate(over64E_UC = over64E + over64M,
         over64E_LC = ifelse(
           over64E < over64M, 0, over64E - over64M))
# Join the tables and compute derived proportions with MOEs
age5_64_pct <- allAges %>% 
  left_join(., under5, by = "GEOID") %>% 
  mutate(r_under5E = ifelse(
    allAgesE == 0, 0, under5E/allAgesE),
    r_under5M = moe_ratio(under5E,allAgesE,under5M,allAgesM),
    pct_under5E = r_under5E * 100,
    pct_under5M = r_under5M * 100,
    pct_under5E_UC = pct_under5E + pct_under5M,
    pct_under5E_LC = ifelse(
      pct_under5E < pct_under5M, 0, pct_under5E - pct_under5M)) %>% 
  left_join(., over64, by = "GEOID") %>% 
  mutate(r_over64E = ifelse(
    allAgesE == 0, 0, over64E/allAgesE),
    r_over64M = moe_ratio(over64E,allAgesE,over64M,allAgesM),
    pct_over64E = r_over64E * 100,
    pct_over64M = r_over64M * 100,
    pct_over64E_UC = pct_over64E + pct_over64M,
    pct_over64E_LC = ifelse(
      pct_over64E < pct_over64M, 0, pct_over64E - pct_over64M)) %>% 
  select(-starts_with("r_"))
# clean up
# rm(allAges,B01001,over64,under5,ovr64_strings)


### MEDIAN AGE OF HOUSING 
# Based on median year structure built. 
housing_age <- get_acs(geography = "county subdivision", 
                       year = year, moe_level = moe, survey = survey,
                       variables = c(housing_yr_built = "B25035_001"), 
                       state = "MA", output = "wide") %>% 
  mutate(housing_age_est = if_else(housing_yr_builtE > 1700, 2020 - housing_yr_builtE, NULL),
         housing_age_est_UC = housing_age_est + housing_yr_builtM,
         housing_age_est_LC = housing_age_est - housing_yr_builtM) %>% 
  select(-NAME)


### TENURE
# Based on number and percent of renters in occupied housing units
renters <- get_acs(geography = "county subdivision", 
                   year = year, moe_level = moe, survey = survey,
                   variables = c(total_occ_units = "B25003_001",
                                 renter_occ_units = "B25003_003"),
                   state = "MA", output = "wide") %>% 
  mutate(total_occ_units_UC = total_occ_unitsE + total_occ_unitsM,
         total_occ_units_LC = if_else(total_occ_unitsE < total_occ_unitsM, 0,
                                      total_occ_unitsE - total_occ_unitsM),
         renter_occ_units_UC = renter_occ_unitsE + renter_occ_unitsM,
         renter_occ_units_LC = if_else(renter_occ_unitsE < renter_occ_unitsM, 0, 
                                       renter_occ_unitsE - renter_occ_unitsM),
         renter_occ_units_p = if_else(total_occ_unitsE <= 0, 0, 
                                      renter_occ_unitsE/total_occ_unitsE),
         renter_occ_units_p_moe = moe_prop(num = renter_occ_unitsE,
                                           denom = total_occ_unitsE,
                                           moe_num = renter_occ_unitsM,
                                           moe_denom = total_occ_unitsM),
         renter_occ_units_pct = renter_occ_units_p * 100,
         renter_occ_units_pct_UC = 
           (renter_occ_units_p + renter_occ_units_p_moe) * 100,
         renter_occ_units_pct_LC = if_else(
           renter_occ_units_p < renter_occ_units_p_moe, 0, 
           (renter_occ_units_p - renter_occ_units_p_moe) * 100)) %>% 
  select(-NAME)



### HOUSING COST BURDENED
# NOTE THAT THIS DATA IS ONLY AVAILABLE AT TRACT LEVEL, NOT BLKGRP
# Based on tenure by housing cost as a percentage of income in the last 12 months for those paying 30% or more of income
# First compute derived sum and moe for house cost burdened as a group
house_burdened <- get_acs(geography = "county subdivision", 
                          year = year, moe_level = moe, survey = survey,
                          variables = c("B25106_006", 
                                        "B25106_010",
                                        "B25106_014",
                                        "B25106_018",
                                        "B25106_022",
                                        "B25106_028",
                                        "B25106_032",
                                        "B25106_036",
                                        "B25106_040",
                                        "B25106_044"), 
                          state = "MA") %>% 
  group_by(GEOID) %>% 
  summarize(house_burdened_E = sum(estimate),
            house_burdened_M = moe_sum(moe,estimate)) %>% 
  mutate(house_burdened_UC = house_burdened_E + house_burdened_M,
         house_burdened_LC = ifelse(
           house_burdened_E < house_burdened_M, 0, 
           house_burdened_E - house_burdened_M))

# Next, acquire universe estimate to calculate percentages
house_burdened <- get_acs(geography = "county subdivision", 
                          year = year, moe_level = moe, survey = survey,
                          variables = c(occ_housing = "B25106_001"), 
                          state = "MA", output = "wide") %>% 
  left_join(house_burdened, ., by = "GEOID") %>% 
  mutate(house_burdened_p = if_else(house_burdened_E < occ_housingE, 0,
                                    house_burdened_E/occ_housingE),
         house_burdened_p_moe = moe_prop(num = house_burdened_E,
                                         denom = occ_housingE,
                                         moe_num = house_burdened_M,
                                         moe_denom = occ_housingM),
         house_burdened_pct = house_burdened_p * 100,
         house_burdened_pct_UC = 
           (house_burdened_p + house_burdened_p_moe) * 100,
         house_burdened_pct_LC = if_else(
           house_burdened_p < house_burdened_p_moe, 0, 
           (house_burdened_p - house_burdened_p_moe) * 100)) %>% 
  select(-NAME)


### DISABILITY
# NOTE THAT THIS DATA IS ONLY AVAILABLE AT TRACT LEVEL, NOT BLKGRP
# Number and percent of individuals 18+ with a disability
# Download Table B18101 SEX BY AGE BY DISABILITY STATUS
B18101 <- get_acs(geography = "county subdivision", table = "B18101", 
                  year = year, moe_level = moe, survey = survey,
                  state = "MA")

# Isolate disabled and non-disabled population 18+
# create vector of patterns for male and female variables 18+
ovr18_strings <- rep(c(9:20,28:39)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over18 <- B18101 %>% 
  filter(str_detect(variable,paste(ovr18_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(Over18E = sum(estimate),
            Over18M = moe_sum(moe,estimate)) %>% 
  mutate(Over18E_UC = Over18E + Over18M,
         Over18E_LC = ifelse(
           Over18E < Over18M, 0, 
           Over18E - Over18M))
# compute derived sum and moe for those over 18 with a disability only
# create vector of patterns for male and female variables 18+ with disability
disabledOvr18_strings <- sort(c(seq(from = 10, to = 19, by = 3), 
                                seq(from = 29, to = 38, by = 3))) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
disabledOver18 <- B18101 %>%
  filter(str_detect(variable,paste(disabledOvr18_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(disabledOver18E = sum(estimate),
            disabledOver18M = moe_sum(moe,estimate)) %>% 
  mutate(disabledOver18E_UC = disabledOver18E + disabledOver18M,
         disabledOver18E_LC = ifelse(
           disabledOver18E < disabledOver18M, 0, 
           disabledOver18E - disabledOver18M))
# Join the tables and compute derived proportions with MOEs
disabilityOver18_pct <- over18 %>% 
  left_join(., disabledOver18, by = "GEOID") %>% 
  mutate(r_disabilityOver18E = ifelse(
    Over18E == 0, 0, disabledOver18E/Over18E),
    r_disabilityOver18M = moe_ratio(
      disabledOver18E,Over18E,disabledOver18M,Over18M),
    pct_disabilityOver18E = r_disabilityOver18E * 100,
    pct_disabilityOver18M = r_disabilityOver18M * 100,
    pct_disabilityOver18E_UC = pct_disabilityOver18E + pct_disabilityOver18M,
    pct_disabilityOver18E_LC = ifelse(
      pct_disabilityOver18E < pct_disabilityOver18M, 0, 
      pct_disabilityOver18E - pct_disabilityOver18M))%>% 
  select(-starts_with("r_"))
# clean up
# rm(B18101,disabledOver18,disabledOvr18_strings,over18,ovr18_strings)


######### JOIN DATA FRAMES TO POLYGONS #############

# join demographic df to block groups
ma_cosub <- ma_cosub %>% 
  left_join(., race_pct, by = "GEOID") %>% 
  left_join(., age5_64_pct, by = "GEOID") %>% 
  left_join(., eng_limited_pct, by = "GEOID") %>% 
  left_join(., poverty_pct, by = "GEOID") %>% 
  left_join(., lths_pct, by = "GEOID") %>% 
  left_join(., renters, by = "GEOID") %>%
  left_join(., housing_age, by = "GEOID") %>% 
  left_join(., house_burdened, by = "GEOID") %>% 
  left_join(., disabilityOver18_pct, by = "GEOID")

# save output
# load("/Data/Demographics.rds")
save(ma_blkgrps, ma_tracts, ma_cosub, file = "C:/Users/Marcos/Documents/Research/GasLeaksEJ/Data/Demographics.rds")

