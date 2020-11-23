# Environmental Justice analysis of gas leaks across Massachusetts

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# Load list of variables to identify tables of interest
v18 <- load_variables(2018, "acs5", cache = TRUE)

# Download ACS 5-year estimates of demographic data
# Block groups
ma_blkgrps18 <- get_acs(geography = "block group", 
                        variables = c(totalpop = "B03002_001", 
                                      medhhinc = "B19013_001",
                                      households = "B19001_001"),
                        state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$'))

# Tracts
ma_tracts18 <- get_acs(geography = "tract", 
                        variables = c(totalpop = "B03002_001", 
                                      medhhinc = "B19013_001",
                                      households = "B19001_001"),
                        state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$'))

# County subdivisions (i.e., cities and towns)
ma_cosub18 <- get_acs(geography = "county subdivision", 
                      variables = c(totalpop = "B03002_001", 
                                    medhhinc = "B19013_001",
                                    households = "B19001_001"),
                      state = "MA", output = "wide", geometry = TRUE) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$'))

# Isolate MA statewide median household income for EJ threshold
MA_MED_HHINC <- get_acs(geography = "state", 
                        variables = c(totalpop = "B03002_001",
                                      medhhinc = "B19013_001"),
                        state = "MA", output = "wide") %>% 
  dplyr::select(medhhincE) %>% 
  pull()

# add variables to identify EJ criteria thresholds for 2021 EJ policy
ma_cosub18 <- ma_cosub18 %>% 
  mutate(MA_INC_BELOW150 = if_else(medhhincE <= 1.5*MA_MED_HHINC,"Y","N"),
         MA_INC_BELOW150_UC = if_else(medhhincE_UC <= 1.5*MA_MED_HHINC,"Y","N"),
         MA_INC_BELOW150_LC = if_else(medhhincE_LC <= 1.5*MA_MED_HHINC,"Y","N"))

###### DEMOGRAPHIC DATA FRAMES BLOCK GROUP LEVEL ##############

## HOUSEHOLDS BY INCOME
# this is for 2017 MA EJ Policy
# download table of counts of household income categories, sum up households in categories below 65% of MA statewide median
B19001 <- get_acs(geography = "block group", table = "B19001", state = "MA")
# Isolate estimate of total households
medhhinc_total <- B19001 %>%
  filter(variable == "B19001_001") %>%
  transmute(GEOID = GEOID,
            householdsE = estimate,
            householdsM = moe)
# Isolate household counts less than 65% of MA statewide median of $77,378, which is $50,295.70. Closest range is 45 - 49,9.
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
# add variables to identify EJ criteria thresholds
medhhinclt50_pct <- medhhinclt50_pct %>%
  mutate(MA_INCOME = if_else(pct_medhhinclt50E >= 25, "I", NULL),
         MA_INCOME_UC = if_else(pct_medhhinclt50E_UC >= 25, "I", NULL),
         MA_INCOME_LC = if_else(pct_medhhinclt50E_LC >= 25, "I", NULL))
# clean up
rm(B19001,med_strings,medhhinc_total,medhhinclt50)

# join town data with MA income threshold to block groups in order to allow for definition of minority threshold for 2021 MA EJ POLICY
ma_blkgrps18 <- ma_cosub18 %>% 
  transmute(TOWN = NAME, MA_INC_BELOW150 = MA_INC_BELOW150, 
            MA_INC_BELOW150_UC = MA_INC_BELOW150_UC,
            MA_INC_BELOW150_LC = MA_INC_BELOW150_LC) %>% 
  st_join(ma_blkgrps18, ., largest = TRUE)
# add variables to identify EJ criteria thresholds
ma_blkgrps18 <- ma_blkgrps18 %>%
  mutate(medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = medhhincE - medhhincM,
         MA_INCOME21 = if_else(medhhincE <= .65*MA_MED_HHINC, "I", NULL),
         MA_INCOME21_UC = if_else(medhhincE_UC <= .65*MA_MED_HHINC, "I", NULL),
         MA_INCOME21_LC = if_else(medhhincE_LC <= .65*MA_MED_HHINC, "I", NULL))

### RATIO OF INCOME TO POVERTY LEVEL
# Download ratio of income to poverty level in the past 12 months to calculate the number or percent of a block group’s population in households where the household income is less than or equal to twice the federal “poverty level.” More precisely, percent low-income is calculated as a percentage of those for whom the poverty ratio was known, as reported by the Census Bureau, which may be less than the full population in some block groups. More information on the federally-defined poverty threshold is available at http://www.census.gov/hhes/www/poverty/methods/definitions.html. Note also that poverty status is not determined for people living in institutional group quarters (i.e. prisons, college dormitories, military barracks, nursing homes), so these populations are not included in the poverty estimates (https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html).
# First, download table of ratio of income to poverty level
C17002 <- get_acs(geography = "block group", table = "C17002", state = "MA")
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
rm(C17002,num2pov,povknown)

### RACE AND ETHNICITY
# Download B03002 HISPANIC OR LATINO ORIGIN BY RACE in two sets. 
# Start with total pop and all races in wide format and compute upper and lower confidence values. 
B03002_totrace <- get_acs(geography = "block group", variables = c(
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
B03002_minority <- get_acs(geography = "block group", variables = c(
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

# Join with all race pops and compute derived proportions
race_pct <- B03002_totrace %>% 
  left_join(., B03002_minority, by = "GEOID") %>% 
  mutate(across(ends_with("_E"),
                ~ if_else(totalpop_E == 0, 0, .x/totalpop_E), 
                .names = "{col}_p")) %>% 
  mutate(across(ends_with("_M"),
                ~ moe_prop(num = minorityE, 
                           denom = totalpop_E, 
                           moe_num = .x, 
                           moe_denom = totalpop_M), 
                .names = "{col}_p"))

  mutate(minority_pE = ifelse(
    totalpopE == 0, 0, minority_E/totalpop_E),
    minority_pM = moe_prop(num = minorityE, 
                           denom = totalpopE, 
                           moe_num = minorityM, 
                           moe_denom = totalpopM),
    minority_pctE = minority_pE*100,
    minority_pctM = minority_pM*100,
    minority_pctE_UC = minority_pctE + minority_pctM,
    minority_pctE_LC = ifelse(
      minority_pctE < minority_pctM, 0, minority_pctE - minority_pctM)) %>% 
  select(-minority_pE,-minority_pM)
# clean up
rm(list = ls(pattern = "B03002"))

# add variables to identify EJ criteria thresholds
minority_pct <- minority_pct %>% 
  mutate(minority_pctile = percent_rank(minority_pctE),
         minority_pctile_UC = percent_rank(minority_pctE_UC),
         minority_pctile_LC = percent_rank(minority_pctE_LC),
         # MA_MINORITY = if_else(minority_pctE >= 25, "M", NULL),
         # MA_MINORITY_UC = if_else(minority_pctE_UC >= 25, "M", NULL),
         # MA_MINORITY_LC = if_else(minority_pctE_LC >= 25, "M", NULL),
         RI_MINORITY = if_else(minority_pctile >= 0.85, "M", NULL),
         RI_MINORITY_UC = if_else(minority_pctile_UC >= 0.85, "M", NULL),
         RI_MINORITY_LC = if_else(minority_pctile_LC >= 0.85, "M", NULL))
# join non-white group estimates
# first download nonwhite estimates in wide format for easier joining
B03002_nonwhite_wide <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", variables = c(
    nhblackpop = "B03002_004",
    nhamerindpop = "B03002_005",
    nhasianpop = "B03002_006",
    nhnativhpop = "B03002_007",
    nhotherpop = "B03002_008",
    nh2morepop = "B03002_009",
    hisppop = "B03002_012"),
    state = x, output = "wide")})
# join to minority_pct
minority_pct <- B03002_nonwhite_wide %>% 
  select(-NAME) %>% 
  left_join(minority_pct, ., by = "GEOID")
# clean up
rm(list = ls(pattern = "B03002"))