##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                R script for Flexibility of SES project （Chuan-Peng's part).                               ###
###                           [SES Score Calculation]                                                          ###
###               Email = hcp4715@gmail.com       cyq_2016@outlook.com                                         ###
###                                                                                                            ###
##################################################################################################################
##################################################################################################################

##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###  Purpose:                                                                                                  ###
###  Calculate the SES scores according to each method of the original fMRI studies                            ###
###                                                                                                            ###
###  Code authors: Hu Chuan-Peng, PhD, School of Psychology, Nanjing Normal University                         ###  
###                                    210024 Nanjing, China                                                   ###
###                Yuqing Cai, Tsinghua University, 100086, China                                              ###
###                                                                                                            ###
###  Input data                                                                                                ###
###      CFPS: "CFPS2010.RData"                                                                                ###
###      PSID: "GID_map_psid.sav" (family roster data)                                                         ###
###            "PSID_selected_data.sav"(selected variables from the website)                                   ###
###                                                                                                            ###
###  Output results: SES scores according to  methods of calculating the SES for each paper                    ###                                              ###
###                                                                                                            ###
### Table of output SES scores for each paper                                                                  ###
### ------------------------------------------------------------------------------------------------------     ###
### original paper    subject	  SES_cfps        	          SES_psid       	             type of SES           ###
### ------------------------------------------------------------------------------------------------------     ###
### ----------------------------------------------------------------------------------------------------       ###
###                                                                                                            ###
##################################################################################################################
##################################################################################################################
###                                                                                                           ###
###                       ============= Notes about data ==================                                   ###
###   For each family, if there are more than one children, only one children is selected                     ###
###   for the analysis.                                                                                       ###
###   To be consistent across the index reproduced, for all children and adolescents' SES,                    ###
###   We used data from participant with age ranged 10-22 yrs-old.                                            ###
###                                                                                                           ###
#################################################################################################################
#################################################################################################################
###     # --------------------------------------------------------------------------------------------#       ###
###     # ------------------------------Table of Contents --------------------------------------------#       ###
###     # --------------------------------------------------------------------------------------------#       ###
###     # ---------- 1. Prepare the CFPS data for later analysis -------------------------------------#       ###
###     # ---------- 2. Prepare the PSID data for later analysis -------------------------------------#       ###
###     # ---------- 3. Reproduce SES indexes in papers ----------------------------------------------#       ###
###     # ---------- 4. Extract columns of pid and SES from all the dataframes of SES ----------------#       ###
#################################################################################################################
#################################################################################################################

######################## Start of the script #####################
rm(list = ls())                     # clean the memory to avoid unnecessary errors
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set the feedback language to English
options(scipen = 999)               # force R to output in decimal instead of scientific notion
options(digits = 5)                 # limit the number of reporting

# check package manager:
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman") }   

# load the packages needed
pacman::p_load("tidyverse", "psych", "foreign", "pracma")

# load self-defined function
source("income_conversion_functions.R")

# ---------------------------------------------------------------------------------------
# ---------- 3.  Reproduce SES indexes in papers--------------------------------------------
# ---------------------------------------------------------------------------------------

###################################################################################################################################
####################################### reproduced by Chuan-Peng   ################################################################
###################################################################################################################################

# load preprocessed data for reproducing.
lapply(c("df.CFPS_child.RData", "df.CFPS_adult.RData", "df.CFPS_elderly.RData",
         "df.PSID_child.RData", "df.PSID_adult.RData", "df.PSID_elderly.RData"), 
       load, environment())

####### Park, etal, 2018 SCAN (CFPS & PSID) ###########
#  Subject: children
#  SES = Annual income and maternal education, separately

## CFPS ##
park2018_CFPS_incom <- df.CFPS_child %>%
  dplyr::select(faminc)
  
    # set 7 levels for mother's education
park2018_CFPS_edu_m <- df.CFPS_child %>%
  dplyr::select(edu_m)

# check SES score
# table(park2018_CFPS_incom$faminc)

## PSID ##
park2018_PSID_incom <- df.PSID_child %>%
  dplyr::select(fincome)

park2018_PSID_edu_m <- df.PSID_child %>%
  dplyr::select(eduy_m)


# check SES score
# table(betan_PSID$SES_betan_psid)

######## Park, et al., 2021 (CFPS & PSID)######
# Subject：children
# Socioeconomic status (SES) was defined as the average of Z-scored income and 
# Z-scored years of parental education (parental education was averaged across parents if available for both parents).
# SES = (Z_income + Z_parental_edu)/2
#      Z_parental_edu = Normalized(mean(parents))

# income 11 bracket, instead of using the original,
# Less than $5,000, $5,000 through $11,999, $12,000 through $15,999, 
# $16,000 through $24,999, $25,000 through $34,999, $35,000 through $49,999, 
# $50,000 through $74,999, $75,000 through $99,999, $100,000 through $149,999, 
# $150,000 through $199,999, $200,000 and greater, and Unsure. 
# Annual family income was estimated as the median value of the selected income bracket.
# But how the first and last bin should be processed?

# citaion of Operario et al., 2004 seems not really relevant

# first, get the convert index for CFPS
convert_index_cfps_function(paper_year = 2021, paper_country = "USA")  
convert_idx = index_ppp_first_cfps # index_cpi_first_cfps

# educ, highest education level was asked from participants, but the article used 
# years of education, did not report how the educational level and years of education were transformed.
# here we used years of education directly.

## CFPS ##
park2021_CFPS <- df.CFPS_child %>%
  dplyr::mutate(income_bin = cut(faminc,
                                 breaks=c(0,     4999,  11999, 15999,  24999, 34999,
                                          49999, 74999, 99999, 149999, 199999, Inf)*convert_idx, 
                                 labels= c("<5000", "5000~11999", "12000~15999", "16000~24999", "25000~34999",
                                           "35000~49999", "50000~74999","75000~99999","100000~149999",
                                           "150000~199999",">=200000")),
                # income_bin = factor(income_bin, 
                #                      levels = c("<5000", "5000~11999", "12000~15999", "16000~24999", "25000~34999",
                #                                 "35000~49999", "50000~74999","75000~99999","100000~149999",
                #                                 "150000~199999",">=200000"))
                ) %>%
  dplyr::mutate(income_med = dplyr::recode(income_bin,
                                           "<5000" = 4999*convert_idx*0.9, 
                                           "5000~11999"  = median(c(5000*convert_idx, 12000*convert_idx)),
                                           "12000~15999" = median(c(12000*convert_idx, 16000*convert_idx)), 
                                           "16000~24999" = median(c(16000*convert_idx, 25000*convert_idx)), 
                                           "25000~34999" = median(c(25000*convert_idx, 35000*convert_idx)),
                                           "35000~49999" = median(c(35000*convert_idx, 50000*convert_idx)), 
                                           "50000~74999" = median(c(50000*convert_idx, 75000*convert_idx)),
                                           "75000~99999" = median(c(75000*convert_idx, 100000*convert_idx)),
                                           "100000~149999" = median(c(100000*convert_idx, 150000*convert_idx)),
                                           "150000~199999" = median(c(150000*convert_idx, 200000*convert_idx)),
                                           ">=200000" = 200000*convert_idx*1.125,
                                           )) %>%
  # dplyr::mutate(income_bin = ifelse(faminc<= 4999*convert_idx, 4999*convert_idx*0.9, ## here use the approximate of original conversion (<5000-->4500)
  #                                   ifelse(faminc <= 12000*convert_idx, median(c(5000*convert_idx, 12000*convert_idx)),
  #                                          ifelse(faminc <= 16000*convert_idx, median(c(12000*convert_idx, 16000*convert_idx)),
  #                                                 ifelse(faminc <= 25000*convert_idx, median(c(16000*convert_idx, 25,000*convert_idx)),
  #                                                        ifelse(faminc <=35000*convert_idx, median(c(25000*convert_idx, 35000*convert_idx)),
  #                                                               ifelse(faminc <= 50000*convert_idx, median(c(35000*convert_idx,50000*convert_idx)),
  #                                                                      ifelse(faminc <= 75000*convert_idx, median(c(50000*convert_idx, 75000*convert_idx)),
  #                                                                             ifelse(faminc <= 100000*convert_idx, median(c(75000*convert_idx, 100000*convert_idx)),
  #                                                                                    ifelse(faminc <= 150000*convert_idx, median(c(100000*convert_idx,150000*convert_idx)), 
  #                                                                                           ifelse(faminc <= 200000*convert_idx, median(c(150000*convert_idx,200000*convert_idx)),
  #                                                                                                  200000*convert_idx*1.125))))))))))) %>%
  dplyr::select(income_med, eduy_f, eduy_m) %>%
  dplyr::mutate(par_eduy = ifelse(is.na(eduy_f) & is.na(eduy_m), NA,
                                  ifelse(is.na(eduy_f) & !is.na(eduy_m), eduy_m,
                                         ifelse(!is.na(eduy_f) & is.na(eduy_m), eduy_f,
                                                rowMeans(select(., starts_with("eduy")))))
                                  )
                ) %>%
  dplyr::mutate(z_income = scale(income_med) %>% as.vector,
                z_edu = scale(par_eduy) %>% as.vector,
                SES = (z_income + z_edu)/2)

# check SES score
# table(park2021_CFPS$SES)

## PSID ##
convert_index_psid_function(paper_year = 2021, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid

park2021_PSID <- df.PSID_child %>%
  dplyr::mutate(income_bin = cut(fincome,
                                 breaks=c(0,     4999,  11999, 15999,  24999, 34999,
                                          49999, 74999, 99999, 149999, 199999, Inf)*convert_idx, 
                                 labels= c("<5000", "5000~11999", "12000~15999", "16000~24999", "25000~34999",
                                           "35000~49999", "50000~74999","75000~99999","100000~149999",
                                           "150000~199999",">=200000"))
  ) %>%
  dplyr::mutate(income_med = dplyr::recode(income_bin,
                                           "<5000" = 4999*convert_idx*0.9, 
                                           "5000~11999"  = median(c(5000*convert_idx, 12000*convert_idx)),
                                           "12000~15999" = median(c(12000*convert_idx, 16000*convert_idx)), 
                                           "16000~24999" = median(c(16000*convert_idx, 25000*convert_idx)), 
                                           "25000~34999" = median(c(25000*convert_idx, 35000*convert_idx)),
                                           "35000~49999" = median(c(35000*convert_idx, 50000*convert_idx)), 
                                           "50000~74999" = median(c(50000*convert_idx, 75000*convert_idx)),
                                           "75000~99999" = median(c(75000*convert_idx, 100000*convert_idx)),
                                           "100000~149999" = median(c(100000*convert_idx, 150000*convert_idx)),
                                           "150000~199999" = median(c(150000*convert_idx, 200000*convert_idx)),
                                           ">=200000" = 200000*convert_idx*1.125,
  )
  ) %>%
  dplyr::select(income_med, eduy_f, eduy_m) %>%
  dplyr::mutate(par_eduy = ifelse(is.na(eduy_f) & is.na(eduy_m), NA,
                                  ifelse(is.na(eduy_f) & !is.na(eduy_m), eduy_m,
                                         ifelse(!is.na(eduy_f) & is.na(eduy_m), eduy_f,
                                                rowMeans(select(., starts_with("eduy")))))
  )
  ) %>%
  dplyr::mutate(z_income = scale(income_med) %>% as.vector,
                z_edu = scale(par_eduy) %>% as.vector,
                SES = (z_income + z_edu)/2)

######## Perdue, et al., 2019 (CFPS & PSID)######
# Subject：Infants/toddlers
# Two measures related to low SES: poverty and maternal education;
# World Bank's criteria for poverty may not be able to reproduced.

# Poverty: Household poverty was assessed at enrollment shortly after the child was born
# using items from home observations and sociodemographic questionnaires to create 
# a cumulative score. 
# This score included information about income‐to‐needs relative
# to the World Bank's definition of extreme poverty as living on less $1.9 per day,
# presence or absence of ‘housing risks’ related to poor construction materials, 
# crowding, and poor sanitation and an index of family assets such as ownership of 
# furniture and electrical appliances. 
# Among the 6‐month‐olds, 88% of the families met the World Bank's criteria for poverty
# and an average family had six assets at home and was exposed to three housing risks 
# (e.g. living with dirt floor and sharing toilet facilities). 
# Among the 36‐months‐olds,93% of the families met the World Bank's criteria for poverty,
# an average family had five assets at home and was exposed to four housing risks.

# Maternal education
# The number of years of self‐reported maternal education was used as a continuous 
# variable. Mothers reported between 0 and 10 years of formal education. 
# The mean length of education was 4.78 (SD = 3.46) years in the 6‐month‐old cohort
# and 4.21 (SD = 3.65) in the 36‐month‐old cohort

## CFPS ##
Perdue2019_CFPS_edu <- df.CFPS_child %>%
  dplyr::select(eduy_m)

## PSID ##
Perdue2019_PSID_edu <- df.PSID_child %>%
  dplyr::select(eduy_m)

## Pfefferbaum, 2016 ## 
# Subject: Adolescents
# SES determined as the highest education achieved by either parent (Akshoomoff et al. 2014).
# in years

## CFPS ##
Pfefferbaum2016_CFPS <- df.CFPS_child %>%
  dplyr::select(eduy_f, eduy_m) %>%
  dplyr::mutate(par_eduy = ifelse(is.na(eduy_f) & is.na(eduy_m), NA,
                                  ifelse(is.na(eduy_f) & !is.na(eduy_m), eduy_m,
                                         ifelse(!is.na(eduy_f) & is.na(eduy_m), eduy_f,
                                                ifelse(eduy_f > eduy_m, eduy_f, eduy_m)))))

## PSID ##
Pfefferbaum2016_PSID <- df.PSID_child %>%
  dplyr::select(eduy_f, eduy_m) %>%
  dplyr::mutate(par_eduy = ifelse(is.na(eduy_f) & is.na(eduy_m), NA,
                                  ifelse(is.na(eduy_f) & !is.na(eduy_m), eduy_m,
                                         ifelse(!is.na(eduy_f) & is.na(eduy_m), eduy_f,
                                                ifelse(eduy_f > eduy_m, eduy_f, eduy_m)))))

## Phillips, et al., 2011 ##
# subject: adults

# Need to revise the data extraction
# publication year: 2013 -> 2011
# 
# rechecked Table 1, this paper seems to included both participants' own SES and Childhood SES (parental edu)

# Participants' Objective SES: education (years) and family income (1 - 15)
# a composite measure of objective SES was computed by averaging the standardized (z-score) values
# of the two index variables for each individual. 
# This measure was then re-standardized to yield of a distribution with mean of 0.0 and SD of 1.0.

# Annual (pre-tax) family income, within bracketed ranges of: 
# 1 = < $10,000; 2 = $10,000-14,999; 3 = $15,000-24,999; 
# 4 = $25,000-34,999; 5 = $35,000-49,999; 6 = $50,000-64,999; 
# 7 = $65,000-79,999; 8 = $80,000-94,999; 9 = $95,000-109,999; 
# 10 = $110,000-124,999; 11 = $125,000-139,999; 12 = $140,000-154,999; 
# 13 = $155,000-169,999; 14 = $170,000-185,000; 15 = > $185,000.

# Unclear procedure: how the family income is standardized? Did the author used the median income of each bracket?
# Solution: I assume that they used 1 ~ 15 directly (from table 1)

# Participants object SES was quantified in two ways
# continuous: average the z-score and re-standardize
# binary: lowest income and lowest edu vs the rest


# Childhood SES:
# Participants reported both their mothers’ and fathers’ level of education completed by the time the participant was age 18 
# (range: 0 = no H.S. diploma, 1 = GED, 2 = H.S. diploma, 3 = Technical training, 4 = Some college, 
# no degree, 5 = AS, 6 = BS, 7 = MS, 8 = MD/PhD)

# Unclear procedure: did they standardize parental education?
# solution: I assume that they used 0 ~ 8 directly

# Subjective SES
# Participants' own Subjective SES, MacArthur Scale of Subjective Social Status
# Father's and Mother's subjective SES before 18, rated by participants

# From Table 5, and Results section, it seems to me that the authors used family income & composite SES,
# Also they used Father's and Mother's subjective SES and father's and mother's education level in the regression.
# in this sense, the composite and parental SES actually were used separately.
# Only reproduce participants' Objective SES

# first, get the convert index for CFPS
convert_index_cfps_function(paper_year = 2011, paper_country = "USA")  
convert_idx = index_ppp_first_cfps

Phillips2011_CFPS <- df.CFPS_adult %>%
  dplyr::select(income, eduy) %>%
  dplyr::mutate(income_bin = cut(income,
                                 breaks=c(-1,      9999,   14999, 24999, 34999, 49999,
                                          64999,  79999,  94999, 109999, 124999, 13999,
                                          154999, 169999, 185000, Inf)*convert_idx, 
                                 labels= c("1", "2", "3", "4", "5", "6",
                                           "7", "8", "9", "10", "11", "12",
                                           "13","14", "15")
                                 ),
                income_bin = as.numeric(as.character(income_bin))
                ) %>%
  dplyr::mutate(z_income = scale(income_bin) %>% as.vector,
                z_edu = scale(eduy) %>% as.vector,
                SES = (z_income + z_edu)/2,
                SES = scale(SES) %>% as.vector)


# PSID
convert_index_psid_function(paper_year = 2011, paper_country = "USA")  
convert_idx = index_ppp_first_psid # index_cpi_first_psid

Phillips2011_PSID <- df.PSID_adult %>%
  dplyr::select(income, eduy) %>%
  dplyr::mutate(income_bin = cut(income,
                                 breaks=c(-1,      9999,   14999, 24999, 34999, 49999,
                                          64999,  79999,  94999, 109999, 124999, 13999,
                                          154999, 169999, 185000, Inf)*convert_idx, 
                                 labels= c("1", "2", "3", "4", "5", "6",
                                           "7", "8", "9", "10", "11", "12",
                                           "13","14", "15")
                                 ),
                income_bin = as.numeric(as.character(income_bin))
                ) %>%
  dplyr::mutate(z_income = scale(income_bin) %>% as.vector,
                z_edu = scale(eduy) %>% as.vector,
                SES = (z_income + z_edu)/2,
                SES = scale(SES) %>% as.vector)


## Piccolo, et al., 2016 ##
# Subjects: children, adolescents
# Noble's group, PING
# income and education separately,
# might be same as Nobel et al. 2015, need to re-check
# also Piccolo, et al., 2018, 2019 

# Parents reported the total yearly family income and their level of educational attainment. 
# Both parental education and family income data were collected in bins, which were recoded 
# as the means of the bins for analysis (Noble, et al., 2015). 
# Family income was natural log-transformed, as it was positively skewed.

# information from S1 Table

# Annual family income bin | Recoded value
# Less than $5,000 | 4,500
# $5,000 - $9,999 | 7,500
# $10,000 - $19,999 | 15,000
# $20,000 - $29,999 | 25,000
# $30,000 - $39,999 | 35,000
# $40,000 - $49,999 | 45,000
# $50,000 - $99,999 | 75,000
# $100,000 - $149,999 | 125,000
# $150,000 - $199,999 | 175,000
# $200,000 - $249,999 | 225,000
# $250,000 - $299,999 | 275,000
# $300,000 and above | 325,000

# first, get the convert index for CFPS
# Ping was launched in 2009, we suppose the data collected in 2010
convert_index_cfps_function(paper_year = 2010, paper_country = "USA")  
convert_idx = index_ppp_first_cfps

Piccolo2016_CFPS_income <- df.CFPS_child %>%
  dplyr::select(faminc) %>%
  dplyr::mutate(income_bin = cut(faminc,
                                 breaks=c(-1,      4999,   9999, 19999, 29999, 39999,
                                          49999,  99999,  149999, 199999, 249999, 299999,
                                          Inf)*convert_idx, 
                                 labels= c("4500",  "7500",   "15000",  "25000",  "35000",  "45000",
                                           "75000", "125000", "175000", "225000", "275000", "325000"
                                           )
  ),
  income_bin = as.numeric(as.character(income_bin)),
  income_bin = log(income_bin * convert_idx) 
  )

# information from S1 Table
# Parental education bin | Recoded value
# Less than seven years of school | 6
# Seven to nine years of school | 8
# Ten to eleven years of school | 10.5
# High school graduate | 12
# Some college (1-3 years, AA, business schools) | 14
# Four-year college graduate (BA, BS, BM) | 16
# Professional degree (MA, MS, ME, MD, PhD, LLD, JD, etc.) | 18

# To exactly reproduce this approach, we need to combine year of education and level of education
# That says, we recode the edu based on both years of education and degree, and then average betwwen parents.

# in CFPS, edu_f/edu_m:
# 1=illiterate 2=primary 3=junior high 4=senior high 5=3-year college 6=4-year college 7=master 8=doctoral

Piccolo2016_CFPS_edu <- df.CFPS_child %>%
  dplyr::select(edu_f,eduy_f, edu_m, eduy_m) %>%
  dplyr::mutate(edu_f = as.character(edu_f),
                edu_f_degree = dplyr::recode(edu_f,
                                             "4" = "high_school",
                                             "5" = "some_college",
                                             "6" = "college_grad",
                                             "7" = "master",
                                             "8" = "doctoral",
                                             .default = as.character(edu_f)),
                edu_m = as.character(edu_m),
                edu_m_degree = dplyr::recode(edu_m,
                                             "4" = "high_school",
                                             "5" = "some_college",
                                             "6" = "college_grad",
                                             "7" = "master",
                                             "8" = "doctoral",
                                             .default = as.character(edu_m))) %>%
  dplyr::mutate(eduy_f_new1 = ifelse(eduy_f <= 6.5, 6,
                                    ifelse(eduy_f >= 6.5 & eduy_f <= 9.5, 8,
                                           ifelse(eduy_f >= 9.5 & eduy_f <= 11.5, 10.5, NA))),
                eduy_f_new2 = dplyr::recode(edu_f_degree,
                                            "high_school" = "12",
                                            "some_college" = "14",
                                            "college_grad" = "16",
                                            "master" = "18",
                                            "doctoral" = "18",
                                            .default = NA_character_),
                eduy_f_new2 = as.numeric(eduy_f_new2),
                eduy_m_new1 = ifelse(eduy_m <= 6.5, 6,
                                     ifelse(eduy_m >= 6.5 & eduy_m <= 9.5, 8,
                                            ifelse(eduy_m >= 9.5 & eduy_m <= 11.5, 10.5, NA))),
                eduy_m_new2 = dplyr::recode(edu_m_degree,
                                            "high_school" = "12",
                                            "some_college" = "14",
                                            "college_grad" = "16",
                                            "master" = "18",
                                            "doctoral" = "18",
                                            .default = NA_character_),
                eduy_m_new2 = as.numeric(eduy_m_new2)) %>%
  tidyr::unite("eduy_f_new", eduy_f_new1:eduy_f_new2, na.rm = T) %>%
  tidyr::unite("eduy_m_new", eduy_m_new1:eduy_m_new2, na.rm = T) %>%
  dplyr::mutate(eduy_f_new = as.numeric(eduy_f_new),
                eduy_m_new = as.numeric(eduy_m_new)) %>%
  dplyr::mutate(par_edu = ifelse(is.na(eduy_f_new) & is.na(eduy_m_new), NA,
                                 ifelse(is.na(eduy_f_new) & !is.na(eduy_m_new), eduy_m_new,
                                        ifelse(!is.na(eduy_f_new) & is.na(eduy_m_new), eduy_f_new,
                                               (eduy_f_new + eduy_m_new)/2))))

# PSID income
convert_index_psid_function(paper_year = 2010, paper_country = "USA")  
convert_idx = index_ppp_first_psid # index_cpi_first_psid

Piccolo2016_PSID <- df.PSID_child %>%
  dplyr::select(income, eduy) %>%
  dplyr::mutate(income_bin = cut(income,
                                 breaks= c(-1,      4999,   9999, 19999, 29999, 39999,
                                          49999,  99999,  149999, 199999, 249999, 299999,
                                          Inf)*convert_idx, 
                                 labels= c("4500", "7500", "15000", "25000", "35000", "45000",
                                           "75000", "125000", "175000", "225000", "275000", "325000"
                                 )
  ),
  income_bin = as.numeric(as.character(income_bin)),
  income_bin = log(income_bin * convert_idx) 
  )

# PSID edu, used year only.
Piccolo2016_PSID_edu <- df.PSID_child %>%
  dplyr::select(edu_f,eduy_f, edu_m, eduy_m) %>%
  dplyr::mutate(eduy_f_new = ifelse(eduy_f <= 6.5, 6,
                                     ifelse(eduy_f >= 6.5 & eduy_f <= 9.5, 8,
                                            ifelse(eduy_f >= 9.5 & eduy_f <= 11.5, 10.5, 
                                                   ifelse(eduy_f ==12, 12, 
                                                          ifelse(eduy_f >= 12.5 & eduy_f <= 14.5, 14,
                                                                 ifelse(eduy_f >= 14.5 & eduy_f <= 16.5, 16, 
                                                                        ifelse(eduy_f >=16.5 & eduy_f <= 80, 18, NA))))))),
                eduy_m_new =ifelse(eduy_m <= 6.5, 6,
                                    ifelse(eduy_m >= 6.5 & eduy_m <= 9.5, 8,
                                           ifelse(eduy_m >= 9.5 & eduy_m <= 11.5, 10.5, 
                                                  ifelse(eduy_m ==12, 12, 
                                                         ifelse(eduy_m >= 12.5 & eduy_m <= 14.5, 14,
                                                                ifelse(eduy_m >= 14.5 & eduy_m <= 16.5, 16, 
                                                                       ifelse(eduy_m >=16.5 & eduy_m <= 80, 18, NA))))))),
                ) %>%
  dplyr::mutate(par_edu = ifelse(is.na(eduy_f_new) & is.na(eduy_m_new), NA,
                                 ifelse(is.na(eduy_f_new) & !is.na(eduy_m_new), eduy_m_new,
                                        ifelse(!is.na(eduy_f_new) & is.na(eduy_m_new), eduy_f_new,
                                               (eduy_f_new + eduy_m_new)/2))))


## Powers et al., 2016 
# Subject: child
# SES: One feature of SES, parent education, was used as a covariate in this analysis to control for SES.
#      income was not included

# The reported level of education for each parent was assigned a value from 1 to 7 
# 1 = Less than High School, 
# 2 = Some High School, 
# 3 = Completed High School, 
# 4 = Associate’s Degree or some college, 
# 5 = Completed college, 
# 6 = Master’s or some graduate school, 
# 7 = Doctorate or equivalent). 
# The values for each parent were then averaged, or an individual value was taken for single parents, 
# to create a measure of parent education with a maximum score of 7.

## CFPS ##
Powers2016_CFPS <- df.CFPS_child %>%
  dplyr::select(edu_f, edu_m) %>%
  dplyr::mutate(edu_f_new = ifelse(edu_f > 1, edu_f - 1, edu_f),
                edu_m_new = ifelse(edu_m > 1, edu_m - 1, edu_m)) %>%
  dplyr::mutate(SES = ifelse(is.na(edu_f_new) & is.na(edu_m_new), NA,
                                 ifelse(is.na(edu_f_new) & !is.na(edu_m_new), edu_m_new,
                                        ifelse(!is.na(edu_f_new) & is.na(edu_m_new), edu_f_new,
                                               (edu_f_new + edu_m_new)/2))))

# check SES score
table(Powers2016_CFPS$SES)

## PSID, can not be reproduced because the PSID data can not distinguish between master and doctoral.
# Powers2016_PSID <- df.PSID_child %>%
#   dplyr::select(eduy_f, eduy_m) %>%
#   dplyr::mutate(edu_m_recode = cut(eduy_m, 
#                                    breaks = c(-0.5, 8.5, 11.5, 12.5, 14.5, 16.5, 22.5), 
#                                    labels = c("1", "2", "3", "4", "5", "6")), # recode education
#                 ) %>%
#   dplyr::mutate(SES = ifelse(is.na(edu_f_new) & is.na(edu_m_new), NA,
#                              ifelse(is.na(edu_f_new) & !is.na(edu_m_new), edu_m_new,
#                                     ifelse(!is.na(edu_f_new) & is.na(edu_m_new), edu_f_new,
#                                            (edu_f_new + edu_m_new)/2))))

## Qiu, 2017 ##

# Subject: neonates
# SES was measured using monthly household income (Table 1) and was collected at 26 weeks gestation via questionnaire.
# Note: this was included in the preliminary analysis. 
# We used family income directly instead of monthly household income.

## CFPS ##
Qiu2017_CFPS <- df.CFPS_child%>%
  dplyr::mutate(SES_qiu_cfps = faminc)

# check SES score
summary(Qiu2017_CFPS$SES_qiu_cfps)

## PSID ##
Qiu2017_PSID <- df.PSID_child %>%
  dplyr::select(pid, fid, fincome) %>%
  dplyr::mutate(SES_qiu_psid = fincome) 


# SES = mother's education and occupation
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: only reproduced using CFPS data because of the occupation data

############# Romeo, 2018a (CFPS)##################
# Romeo published three papers in 2018 as the first author
# Journal: Cerebra Cortex, psych sci, J Neurosci
# coded as Romeo 2018a, 2018b, 2018c

# Romeo 2018a, Barratt Simplified Measure of Social Status
# Romeo 2018b, parental edu and household income separately
# Romeo 2018c, used composite SES as independent variable, but not mentioned how the SES composite was obtained.

# Subject: adolescent

# Barratt Simplified Measure of Social Status” 
# SES = mother's education and occupation
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: only reproduced using CFPS data because of the occupation data

Romeo2018a_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.5, 6.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%  # recode education
  dplyr::mutate(occu_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, 
                                       "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8)) %>%  # recode occupation
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(occu_m_recode = as.numeric(as.character(occu_m_recode)),  # convert variables into numeric ones
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_romeo1_cfps = occu_m_recode*5 + edu_m_recode*3)   # calculate composite SES score

# check SES score
table(Romeo2018a_CFPS$SES_romeo1_cfps)

############### Romeo, 2018b (CFPS & PSID)############
# Subject: adolescent
# SES = (Z_{parents' education} + Z_{family income})/2
# education: recode into 5 levels (0-4) and then calculate z-score of the parents' education 
#           (replace parents' education with each other if one of them is NA)

## CFPS ##
Romeo2018b_CFPS <- df.CFPS_child %>%
  dplyr::select(faminc, edu_f, edu_m) %>%
  # recode income 
  dplyr::mutate(income_zscore = (faminc - mean(faminc, na.rm = TRUE))/sd(faminc, na.rm = TRUE),
                edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5), # calculate composite parents' education score
                                   labels = c("1", "2", "3", "4","5")),
                edu_f_recode = as.numeric(as.character(edu_f_recode))-1,  # recode education 0-4
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5), 
                                   labels = c("1", "2", "3", "4","5")),  # calculate composite parents' education score
                edu_m_recode = as.numeric(as.character(edu_m_recode))-1,  # recode education 0-4
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode),  # calculate composite parents' education score
                edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),
                edu_parents = (edu_f_recode + edu_m_recode)/2,   # calculate composite parents' education score
                edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE),  # calculate mean of edu and income (ses)
                SES_romeo2_cfps = (edu_zscore + income_zscore)/2)  # calculate mean of edu and income (ses)

# check SES score
summary(Romeo2018b_CFPS$SES_romeo2_cfps)

## PSID ##
Romeo2018a_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.00001, 8.5, 12.5,  14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5",  NA))) %>%  # set NA
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.00001, 8.5, 12.5,  14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5",  NA))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))-1,
                edu_f_recode = as.numeric(as.character(edu_f_recode))-1)%>% #recode as 0-4
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),  # replace father/mother education if one is missing
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode)) %>%
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2) %>%  # calculate z-score of parents' education
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%  # calculate z-score of parents' education
  dplyr::mutate(income_zscore = (fincome - mean(fincome, na.rm = TRUE))/sd(fincome, na.rm = TRUE)) %>%
  dplyr::mutate(SES_romeo2_psid = (edu_zscore + income_zscore)/2)  # calculate mean of edu and income (ses)

# check SES score
summary(romeo2_PSID$SES_romeo2_psid)

## Rosen, 2018 (Not possible)

# subjects: youths
# SES: income to needs; 
# Parents reported annual income in 10 bins, ??
# the median of the income bins (except for the lowest and highest bins, which were assigned $5,000 and $200,000 respectively) 
# divided by total household income by the 2014 U.S. census-defined poverty line for a family of that size,


# Note

# not reproducible because how 10 bins are divided was not reported.

## Rosen, 2021 (Not possible)
# not possible because of unreported detail.
# https://osf.io/s78aq/ need permission 

# subjects: youths
# SES: income to needs; 
# Parents reported annual income in 10 bins, ??
# the median of the income bins (except for the lowest and highest bins, which were assigned $5,000 and $200,000 respectively) 
# divided by total household income by the 2014 U.S. census-defined poverty line for a family of that size,


# Note

# not reproducible because how 10 bins are divided was not reported.

## Salzwedel, 2016
# subjects: infants

# SES: Maternal education
# Rank scores: 
# Some High School=3, 
# Graduated from High School=4, 
# Trade School or Business College=5, 
# Some College=6, 
# Graduated with 4-year College Degree=7, 
# and Post-graduate work at university = 8.

## CFPS
# for CFPS, less than high school is normal, which means they are not even "some high school"
# 5 and 6 are a bit arbitary.
Salzwedel2016_CFPS <- df.CFPS_child %>%
  dplyr::select(eduy_m) %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.00001, 9.5, 11.5,  12.5, 13.5, 15.5, 16.5, 22.5), 
                                   labels = c(NA_character_, "3", "4", "5", "6",  "7", "8"))) 
  

## CFPS
Salzwedel2016_PSID <- df.PSID_child %>%
  dplyr::select(eduy_m) %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.00001, 8.5, 11.5,  12.5, 13.5, 15.5, 16.5, 20, 100), 
                                   labels = c(NA_character_, "3", "4", "5", "6",  "7", "8", NA_character_))) 

table(Salzwedel2016_PSID$edu_m_recode)