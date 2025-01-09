###-----------------------###
###   Data Manipulation   ###
###-----------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))


# Packages ----

library(haven)
library(tidyverse)


# Load data ----

## Demographic data ----

df_hrs_original <- read_stata("../raw/trk2020tr_r.dta")
dim(df_hrs_original) # 43559

## Fertility data ----

df_kid_original <- read_stata("../raw/randhrs1992_2020v1.dta") # 12 minutes to run
dim(df_kid_original) # 42406

## Genetic data ----

df_pgs_original <- read_stata("../raw/pgenscore4e_r.dta")
dim(df_pgs_original) # 12090


# Merge data ----

df_hrs_original <- df_hrs_original %>%
  mutate(hhidpn = 1000 * as.numeric(HHID) + as.numeric(PN))

df_pgs_original <- df_pgs_original %>%
  mutate(hhidpn = 1000 * as.numeric(hhid) + as.numeric(pn))

df_original <- list(df_pgs_original, df_hrs_original, df_kid_original) %>%
  reduce(left_join, by = "hhidpn") 

dim(df_original) # 12090


# Clean data ----

df <- df_original %>%
 
  ## Rename variables ----

  rename(birth_year = BIRTHYR,            
         death_year = KNOWNDECEASEDYR,
         alive_year = LASTALIVEYR,
         sex = GENDER,
         edu = SCHLYRS,
         degree = DEGREE,
         kids = raevbrn,
         pgs_l = E4_LONG_CHARGE15,       
         pgs_k = E4_NEBC_SOCGEN16,
         pgs_kf = E4_NEBF_SOCGEN16,      
         pgs_km = E4_NEBM_SOCGEN16,      
         pc1 = PC1_5A,
         pc2 = PC1_5B,
         pc3 = PC1_5C,
         pc4 = PC1_5D,
         pc5 = PC1_5E,
         pc6 = PC6_10A,
         pc7 = PC6_10B,
         pc8 = PC6_10C,
         pc9 = PC6_10D,
         pc10 = PC6_10E
  ) %>%
  
  ## Remove NA ----

  filter(!is.na(kids)) %>%
  filter(!is.na(edu) & edu!=99) %>%
  
  ## Recode and add variables ----

  mutate(
    
    # Recode sex
    sex = as.factor(ifelse(sex == 1, "Male", "Female")),
    
    # Recode birth year (years after 1905)
    birth_year_new = birth_year - min(birth_year),
    
    # Dead/alive status and age in 2020 or at death
    death_year_new = ifelse(death_year > 2020, NA, death_year),
    
    delta = ifelse(is.na(death_year_new), 0, 1),
    
    age = ifelse(delta == 0, 2020 - birth_year, death_year_new - birth_year),
    
    # Recode education
    educat = as.factor(ifelse(degree == 0, "No degree",
                       ifelse(degree %in% c(1, 2, 9), "High school diploma",
                       ifelse(degree %in% c(3, 4), "College degree",
                       "Advanced degree")))),
    
    # Find most recent marital status
    mar = case_when(!is.na(RMARST) & RMARST != 5 ~ RMARST,
                    !is.na(QMARST) & QMARST != 5 ~ QMARST,
                    !is.na(PMARST) & PMARST != 5 ~ PMARST,
                    !is.na(OMARST) & OMARST != 5 ~ OMARST,
                    !is.na(NMARST) & NMARST != 5 ~ NMARST,
                    !is.na(MMARST) & MMARST != 5 ~ MMARST,
                    !is.na(LMARST) & LMARST != 5 ~ LMARST,
                    !is.na(KMARST) & KMARST != 5 ~ KMARST,
                    !is.na(JMARST) & JMARST != 5 ~ JMARST,
                    TRUE ~ NA_integer_),
    
    # Recode marital status 
    marcat = as.factor(ifelse(mar == 1, "Married", 
                      ifelse(mar == 2, "Separated/Divorced", 
                      ifelse(mar == 3, "Widowed",
                      "Never married")))),
                      
    # Recode the number of kids
    kidscat = as.factor(ifelse(kids > 3, "More than 3 children",
                        ifelse(kids == 3, "3 children",
                        ifelse(kids == 2, "2 children", 
                        ifelse(kids == 1, "1 child",
                        "No children"))))),
    
    # Sign of PGS
    pgs_l_sign = ifelse(pgs_l > 0, "Positive", "Negative"),
    
    pgs_k_sign = ifelse(pgs_k > 0, "Positive", "Negative"),
    
    pgs_sign = ifelse(pgs_l > 0 & pgs_k > 0, "High PRS for longevity and fertility",
                      ifelse(pgs_l > 0 & pgs_k < 0, "High PRS for longevity and low PRS for fertility",
                             ifelse(pgs_l < 0 & pgs_k > 0, "Low PRS for longevity and high PRS for fertility",
                                    "Low PRS for longevity and fertility")))
    ) %>%

  
  ## Select variables ----
  
  select(hhidpn, sex, birth_year, birth_year_new, delta, age, edu, educat, mar, 
         marcat, kids, kidscat, pgs_l, pgs_k, pgs_kf, pgs_km, pgs_l_sign, 
         pgs_k_sign, pgs_sign, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10)


head(df)


# Save data ----

## Combined ----

save(df, file = "../data/df.RData")
dim(df) # 12050

## Females ----

df_f <- df %>%
  filter(sex == "Female")

save(df_f, file = "../data/df_f.RData")
dim(df_f) # 6875

## Males ----

df_m <- df %>%
  filter(sex == "Male")

save(df_m, file = "../data/df_m.RData")
dim(df_m) # 5175

