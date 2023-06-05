## Set working directory
setwd("/Users/lamhine/Library/CloudStorage/Box-Box/San Joaquin Valley PH/Disparities Report/BRFSS/Request#1422-Lam-Hine/2017-2020 Datasets with SJVPHC County Recode")

## Load packages
library(tidyverse)
library(foreign)
library(haven)
library(survey)
library(naniar)

## Load BRFSS and County FIPS data
brfss18 <- read_sas("brfss18sjvphc.sas7bdat")
fips_ca <- read.csv("/Users/lamhine/Library/CloudStorage/Box-Box/San Joaquin Valley PH/ca_regions_counties_fips.csv") %>% 
  select(c("COUNTY_NAME", "COUNTY_FIPS")) %>% 
  mutate(COUNTY_FIPS = as.numeric(COUNTY_FIPS))

# Join in county names by FIPS
brfss18 <- left_join(
  brfss18,
  fips_ca,
  by = c("_IMPCTY" = "COUNTY_FIPS")
  )

# Check to make sure COUNTY1 and COUNTY_NAME line up properly
check <- data.frame(rbind(table(brfss18$COUNTY_NAME, brfss18$COUNTY1, exclude = NULL)))
rm(check) # remove when done

# Create new variable called SJVPHC1, where:
# if COUNTY1 is missing and COUNTY_NAME (imputed) matches SJVPHC region, use COUNTY_NAME,
# otherwise NA_real_ (i.e. where imputed COUNTY_NAME does not match SJVPHC region)
brfss18 <- brfss18 %>% 
  mutate(
    SJVPHC1 = NA_character_,
    SJVPHC1 = case_when(
      SJVPHC %in% c("VALLEY ONLY", "VALLEY ADJACENT") ~ "SJVPHC",
      TRUE ~ SJVPHC
    ),
    SJVPHC2 = NA_character_,
    SJVPHC2 = case_when(
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Alameda" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Alpine" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Amador" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Butte" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Calaveras" & SJVPHC == "VALLEY ADJACENT" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Colusa" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Contra Costa" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Del Norte" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "El Dorado" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Fresno" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Glenn" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Humboldt" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Imperial" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Inyo" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Kern" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Kings" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Lake" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Lassen" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Los Angeles" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Madera" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Marin" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Mariposa" & SJVPHC == "VALLEY ADJACENT" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Mendocino" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Merced" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Modoc" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Mono" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Monterey" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Napa" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Nevada" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Orange" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Placer" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Plumas" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Riverside" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Sacramento" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Benito" & SJVPHC == "VALLEY ADJACENT" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Bernardino" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Diego" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Francisco" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Joaquin" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Luis Obispo" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "San Mateo" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Santa Barbara" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Santa Clara" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Santa Cruz" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Shasta" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Sierra" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Siskiyou" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Solano" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Sonoma" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Stanislaus" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Sutter" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Tehama" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Trinity" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Tulare" & SJVPHC == "VALLEY ONLY" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Tuolumne" & SJVPHC == "VALLEY ADJACENT" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Ventura" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Yolo" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      COUNTY1 %in% c(777,888,999) & COUNTY_NAME == "Yuba" & SJVPHC == "NON-SJVPHC" ~ COUNTY_NAME,
      !COUNTY1 %in% c(777,888,999) ~ COUNTY_NAME,
      TRUE ~ NA_character_
      )
    )


### RECODE HEALTH OUTCOME VARIABLES 

# inspect codes for each of the outcome variables for recoding
table(brfss18$`OTHCANC`, exclude = NULL)
table(brfss18$`KIDNEY`, exclude = NULL)
table(brfss18$`COPDEVER`, exclude = NULL)
table(brfss18$`_CASTHM1`, exclude = NULL)
table(brfss18$`_MICHD`, exclude = NULL)
table(brfss18$`STROKE2`, exclude = NULL)
table(brfss18$`DIABCOR3`, exclude = NULL)
table(brfss18$`_ALTETH3`, exclude = NULL)
table(brfss18$`_RFBING5`, exclude = NULL)
table(brfss18$`MENTHLTH`, exclude = NULL)
table(brfss18$`PHYSHLTH`, exclude = NULL)
table(brfss18$`_HCVU651`, exclude = NULL)
table(brfss18$`_SMOKER3`, exclude = NULL)
table(brfss18$`EXERANY1`, exclude = NULL)
table(brfss18$`_RFBMI5`, exclude = NULL)
table(brfss18$`SLEEPHR2`, exclude = NULL)
table(brfss18$`_DENVST3`, exclude = NULL)

# recode all variables to 1 = disease, 0 = no disease
brfss18 <- brfss18 %>%
  mutate(
    # Cancer diagnosis - all but skin (OTHCANC: EVER TOLD HAD ANY CANCER NOT INCLUDING SKIN CANCER)
    OTHCANC = recode(OTHCANC,     
                     `1` = 1,
                     `2` = 0,
                     .default = NA_real_),
    
    # ***Chronic kidney disease diagnoses (KIDNEY?? "EVER HAD KIDNEY OR BLADDER TROUBLE")***
    KIDNEY = recode(KIDNEY,       
                     `1` = 1,
                     `2` = 0,
                     .default = NA_real_),
    
    # COPD diagnosis (COPDEVER: "EVER TOLD THAT YOU HAVE COPD, EMPHYSEMA, OR CHRONIC BRONCHITIS")
    COPDEVER = recode(COPDEVER,       
                    `1` = 1,
                    `2` = 0,
                    .default = NA_real_),
    
    # Asthma diagnosis (_CASTHM1: "CURRENT ASTHMA")
    `_CASTHM1` = recode(`_CASTHM1`,
                     `1` = 0,
                     `2` = 1,
                     .default = NA_real_),
    
    # Heart disease diagnosis (_MICHD: "REPORTED HAVING MYOCARDIAL INFARCTION OR CORONARY HEART DISEASE")
    `_MICHD` = recode(`_MICHD`,
                      `1` = 1,
                      `2` = 0,
                      .default = NA_real_),
    
    # ***High blood pressure diagnosis (??)***
    
    # Stroke diagnosis (STROKE2: "EVER TOLD YOU HAD A STROKE")
    STROKE2 = recode(STROKE2,
                     `1` = 1,
                     `2` = 0,
                     .default = NA_real_),
    
    # Diabetes diagnosis (DIABCOR3: "EVER HAD DIABETES OR PRE-DIABETES")
    DIABCOR3 = case_when(
      DIABCOR3 == 1 ~ 1,
      DIABCOR3 %in% c(2,3,4) ~ 0,
      TRUE ~ NA_real_),
    
    # Total tooth loss (_ALTETH3: "ADULTS AGED 65+ ALL-NATURAL TEETH EXTRACTED")
    `_ALTETH3` = recode(`_ALTETH3`,
                     `1` = 0,
                     `2` = 1,
                     .default = NA_real_),
    
    # Excessive drinking (_RFBING5: "BINGE DRINKING CALCULATED VAR")
    `_RFBING5` = recode(`_RFBING5`,
                     `1` = 0,
                     `2` = 1,
                     .default = NA_real_),
    
    # Frequent mental distress (MENTHLTH: "DAYS POOR MENTAL HEALTH IN PAST MONTH")
    MENTHLTH = case_when(
      MENTHLTH %in% c(77,88,99) ~ NA_real_,
      MENTHLTH >= 14 ~ 1,
      MENTHLTH < 14  ~ 0,
      TRUE ~ NA_real_),
    
    # Frequent physical distress (PHYSHLTH: "DAYS OF POOR PHYSICAL HEALTH IN LAST MONTH")
    PHYSHLTH = case_when(
      PHYSHLTH %in% c(77,88,99) ~ NA_real_,
      PHYSHLTH >= 14 ~ 1,
      PHYSHLTH < 14  ~ 0,
      TRUE ~ NA_real_),
    
    # Uninsured adults (_HCVU651: "RESPONDENTS 18-64 WITH HEALTH CARE COVERAGE")
    `_HCVU651` = recode(`_HCVU651`,
                     `1` = 0,
                     `2` = 1,
                     .default = NA_real_),
    
    # Smokers (_SMOKER3: "COMPUTED SMOKING STATUS")
    `_SMOKER3` = case_when(
      `_SMOKER3` %in% c(1, 2) ~ 1,
      `_SMOKER3` %in% c(3, 4) ~ 0,
      TRUE ~ NA_real_),
    
    # No leisure time physical activity (EXERANY1: "ANY PHYSICAL ACTIVITY OTHER THAN JOB")
    EXERANY1 = recode(EXERANY1,
                        `1` = 0,
                        `2` = 1,
                        .default = NA_real_),    
    
    # Obesity (_RFBMI5: "OVERWEIGHT OR OBESE CALCULATED VARIABLE")
    `_RFBMI5` = recode(`_RFBMI5`,
                      `1` = 1,
                      `2` = 0,
                      .default = NA_real_),    
    
    # Insufficient sleep (SLEEPHR2 < 7: "ON AVERAGE: # HOURS SLEEP IN 24-HOUR PERIOD")
    SLEEPHR2 = case_when(
      SLEEPHR2 %in% c(77,88,99) ~ NA_real_,
      SLEEPHR2 < 7 ~ 1,
      SLEEPHR2 >= 7  ~ 0,
      TRUE ~ NA_real_),
    
    # Dental visits (_DENVST3: "ADULTS WHO HAVE VISITED A DENTIST, DENTAL HYGIENIST OR DENTAL CLINIC W/IN THE PAST YEAR")
    `_DENVST3` = recode(`_DENVST3`,
                       `1` = 1,
                       `2` = 0,
                       .default = NA_real_)   
    )

## Use imputed age to create categories matching US standard pop distribution #9
brfss18 <- brfss18 %>%
  rename(IMPAGE = `_IMPAGE`) 

brfss18 <- brfss18 %>% 
  mutate(
    age_g_09 = NA_real_,
    age_g_09 = case_when(
      IMPAGE <= 24 ~ 1,
      25 <= IMPAGE & IMPAGE <= 34 ~ 2,
      35 <= IMPAGE & IMPAGE <= 44 ~ 3,
      45 <= IMPAGE & IMPAGE <= 64 ~ 4,
      IMPAGE & IMPAGE >= 65 ~ 5
    )
  )


## Create weighted survey design object
brfss18_dsn <- svydesign(id = ~ 1,
                         strata = ~ `_STSTR`, 
                         weights = ~ `_LLCPWT`,  # using CDC BRFSS weight
                         data = brfss18)


## Create age-adjusted survey design object

age_dist_2000 <- read.csv("/Users/lamhine/Library/CloudStorage/Box-Box/San Joaquin Valley PH/Disparities Report/BRFSS/age_dist_2000.csv")
popage_09 <- age_dist_2000 %>% 
  filter(group == "dist_09",
         age_cat_r != "[18, Inf)") %>% 
  select(pop_k) %>% 
  unlist()

brfss18_aa <- svystandardize(brfss18_dsn,
                             by = ~ age_g_09,
                             population = popage_09
                             )

## GET PREVALENCES ##


cancer <- cbind(
  rev(data.frame(rbind(t(svyby(~OTHCANC, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~OTHCANC, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
  )[,-c(2,3,6)]

kidney <- cbind(
  rev(data.frame(rbind(t(svyby(~KIDNEY, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~KIDNEY, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

copd <- cbind(
  rev(data.frame(rbind(t(svyby(~COPDEVER, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~COPDEVER, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

asth <- cbind(
  rev(data.frame(rbind(t(svyby(~`_CASTHM1`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_CASTHM1`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

mi <- cbind(
  rev(data.frame(rbind(t(svyby(~`_MICHD`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_MICHD`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

stroke <- cbind(
  rev(data.frame(rbind(t(svyby(~STROKE2, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~STROKE2, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

diab <- cbind(
  rev(data.frame(rbind(t(svyby(~DIABCOR3, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~DIABCOR3, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

teeth <- cbind(
  rev(data.frame(rbind(t(svyby(~`_ALTETH3`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_ALTETH3`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

binge <- cbind(
  rev(data.frame(rbind(t(svyby(~`_RFBING5`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_RFBING5`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

mntdis <- cbind(
  rev(data.frame(rbind(t(svyby(~MENTHLTH, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~MENTHLTH, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

phydis <- cbind(
  rev(data.frame(rbind(t(svyby(~PHYSHLTH, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~PHYSHLTH, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

unins <- cbind(
  rev(data.frame(rbind(t(svyby(~`_HCVU651`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_HCVU651`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

smoke <- cbind(
  rev(data.frame(rbind(t(svyby(~`_SMOKER3`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_SMOKER3`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

exer <- cbind(
  rev(data.frame(rbind(t(svyby(~EXERANY1, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~EXERANY1, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

bmi <- cbind(
  rev(data.frame(rbind(t(svyby(~`_RFBMI5`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_RFBMI5`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

sleep <- cbind(
  rev(data.frame(rbind(t(svyby(~SLEEPHR2, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~SLEEPHR2, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

dent <- cbind(
  rev(data.frame(rbind(t(svyby(~`_DENVST3`, ~SJVPHC1, brfss18_aa, svymean, na.rm = T)))))[2:3,],
  rev(data.frame(t(data.frame(svyby(~`_DENVST3`, ~SJVPHC, brfss18_aa, svymean, na.rm = T)))[2:3,]))
)[,-c(2,3,6)]

# rbind all health outcomes together in one dataframe
tab1 <- rbind(cancer, kidney, copd, asth, mi, stroke, diab, teeth, binge, mntdis,
              phydis, unins, smoke, exer, bmi, sleep, dent)

write.csv(tab1, "brfss18.csv")
