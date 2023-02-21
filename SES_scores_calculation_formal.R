##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                The formal reproduction R script for Flexibility of SES project                             ###
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
###  Code authors: HU Chuan-Peng, PhD, Leibniz Institute for Resilience Research,                              ###  
###                                    55131 Mainz, Germany;                                                   ###
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

# set directory to the folder of analytic data
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
if (!require(foreign)) {install.packages("foreign",repos = "http://cran.us.r-project.org"); require(foreign)}

# import data CFPS
load("CFPS2010.RData")

# ---------------------------------------------------------------------------------------
# ---------- 1.  Prepare the CFPS data for later analysis--------------------------------------------
# ---------------------------------------------------------------------------------------
## combine children, adult, family and community data in a data frame
## Note: only children between 10 and 22 year-old were included.
df.CFPS <- df.children %>%
  dplyr::full_join(., df.individual) %>%    # merge children and individual data
  dplyr::arrange(fid, pid) %>%              # arrange the sequence of data according to fid and pid
  dplyr::mutate(age = ifelse(is.na(qa1age), wa1age, qa1age)) %>%  # combine age in child datafrome and adult dataframe
  dplyr::mutate(role_c = ifelse(age <= 22 & age >= 10, "child", NA))%>% # select 10-22 years old as child
  dplyr::left_join(., df.family, by = "fid") %>%   # merge with family data
  dplyr::group_by(fid) %>%                         # copy cid to all rows that share the same family id
  dplyr::mutate(cid = ifelse(sum(!is.na(cid)) == 0,               
                             NA, cid[!is.na(cid)])) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., df.community, by = "cid") %>%    # merge with community data 
  dplyr::left_join(., df.county, by = "countyid") %>%
  # add a column to indicate father or NA        
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c =="child")$pid_f, 
                                'father', NA),               
                # add a column to indicate mother  or NA
                role_m = ifelse(pid %in% subset(., role_c =="child")$pid_m, 
                                'mother', NA)) %>%      
  dplyr::select(pid, fid, cid, pid_f, pid_m, role_f, role_m, role_c, everything()) %>%  # get some columns as the first few columns
  tidyr::unite("role", role_f:role_c, na.rm = TRUE, remove= TRUE) %>% # combine the columns indicating role of the individuals into one role
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         # pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         # pid_father: same as pid_mother
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    # pid_chilld: if the person is child, it is his/her pid; if not, NA
  dplyr::na_if(., -8) %>% # set all the '-8' as NA (in CFPS -8 means not applicable)
  dplyr::na_if(., -1) %>% # set all the '-1' as NA (in CFPS -1 means "I don't know")
  dplyr::na_if(., -2) %>% # set all the '-2' as NA (in CFPS -2 means "I don't want to answer"
  dplyr::na_if(., -7) %>% # set all the '-7' as NA (in CFPS -7 means "did not answer clearly. cannot categorize")
  dplyr::na_if(., -9)     # set all the '-9' as NA (in CFPS -9 means "missing value"

# check the data
summary(df.CFPS)
load("CFPS2010.RData")
# select children data (and parents SES) for the further analysis
df.CFPS_child <- df.CFPS %>%
  dplyr::filter(role == "child") %>% # Select data of children
  # select one child for every family
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1) %>%  # chose the first child of the family
  dplyr::ungroup() %>%
  # rename variables of children to distinguish from parents
  dplyr::rename(egp_c = qg307egp, 
                eduy_c = cfps2010eduy_best,
                edu_c = cfps2010edu_best,
                sss_c = qm402) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", c('pid_mother', "qg307egp", "cfps2010eduy_best", "qm402")], by = 'pid_mother') %>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(egp_m= qg307egp,# occupation coding
                eduy_m = cfps2010eduy_best,
                sss_m = qm402) %>% 
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", c('pid_father',"qg307egp","cfps2010eduy_best", "qm402")], by = 'pid_father') %>%
  # rename variables of parents to distinguish from children (father)
  dplyr::rename(egp_f = qg307egp,
                eduy_f = cfps2010eduy_best,
                sss_f = qm402)

# check data
summary(df.CFPS_child)

# save the data of children (CFPS) as rdata for further processing
#save(df.CFPS_child, file = "df.CFPS_child.RData")

# select adult (age 23-60) data for further analysis
df.CFPS_adult <- df.CFPS %>%
  dplyr::filter(age >22 & age <= 60)  

# check data
summary(df.CFPS_adult)

# save the data of adults (CFPS) as rdata for further processing
save(df.CFPS_adult, file = "df.CFPS_adult.RData")

# select elderly (age > 60)
df.CFPS_elderly <- df.CFPS %>%
  dplyr::filter(age > 60)

# check data
summary(df.CFPS_elderly)

# save the data of elderly (cfps) as rdata for further processing
#save(df.CFPS_elderly, file = "df.CFPS_elderly.Rdata")
# ---------------------------------------------------------------------------------------
# ---------- 2.  Prepare the PSID data for later analysis--------------------------------------------
# ---------------------------------------------------------------------------------------

## Load the data about family structure.
df.map_psid <- foreign::read.spss("GID_map_psid.sav", to.data.frame = TRUE) %>% # 'GID_map_psid': family roster data of PSID
  # Generate a unique variable for each person (pid), father (pid_f), mother (pid_mother)
  # Because personal identity variable ER30001 & ER30002 are not unique for each person
  dplyr::mutate(pid = (ER30001 * 1000) + ER30002,
                pid_f = ER30001_P_F *1000 + ER30002_P_F,
                pid_m = ER30001_P_M *1000 + ER30002_P_M) %>% 
  # select only three variables that will be used in the further data analysis to determine family structure
  dplyr::select(pid, pid_f, pid_m)

# load the PSID data
df.PSID <- foreign::read.spss("PSID_selected_data.sav", to.data.frame = TRUE) %>% # select data from PSID website
  dplyr::mutate(pid = (ER30001 * 1000) + ER30002) %>% # generate a new column for unique pid for each individual.
  # rename frequently used variables
  dplyr::rename(fid = ER34501,
                fincome = ER71426,
                edu = ER34548,
                relation = ER34503,
                sex = ER32000,
                age = ER34504,
                sequence=ER34502,
                depression = ER70680,
                life_satisfaction = ER66025,
                sss_rp = ER70879,
                sss_sp = ER70741) %>%
  # calculate personal income according the calculation method of family income
  dplyr::mutate(income = ER71330+ER71391+ER71420+ER71422) %>%
  # calculate family size
  dplyr::group_by(fid) %>%
  dplyr::mutate(familysize = length(fid)) %>%   
  dplyr::ungroup() %>%
  # add family structure to the data (children,father and mother)
  dplyr::left_join(., df.map_psid) %>%
  # set role of the person
  dplyr::mutate(role_c = ifelse(age >=10 & age <=22, "child", NA)) %>% # children: age from 10-22
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c == "child")$pid_f, "father", NA), # father: children's father
                role_m = ifelse(pid %in% subset(., role_c == "child")$pid_m, "mother", NA)) %>% # mother: children's mother
  # combine columns indicating roles into one column
  tidyr::unite("role", role_c:role_m, na.rm = TRUE, remove = TRUE) %>%
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         # pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         # pid_father: same as pid_mother
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    # pid child: if the person is child, it is her pid; if not, NA
  dplyr::filter(sequence <= 20)  # select only those still live in the family in 2017 (sequence <= 20)

## select children data (and parents SES) for the further analysis
df.PSID_child <- df.PSID %>%
  dplyr::filter(role == "child") %>%  # select only children
  # select variables related to children that are used for further analysis
  dplyr::select(pid, fid, age, sex, relation, pid_father, pid_mother, familysize, fincome, sequence, role, depression, life_satisfaction, sss_rp, sss_sp) %>%
  dplyr::left_join(., df.PSID[df.PSID$role == "mother", c("pid_mother", "edu")], by = "pid_mother")%>%  # combine mother's variable with children
  dplyr::rename(edu_m = edu) %>%  # rename mother's variable
  dplyr::left_join(., df.PSID[df.PSID$role == "father", c("pid_father", "edu")], by = "pid_father")%>%  # combine father's variable with children
  dplyr::rename(edu_f = edu) %>%  # rename father's variable
  # select one child for every family 
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1)%>% 
  dplyr::ungroup()

# check data
summary(df.PSID_child)

# save the data as RData for the further analysis
#save(df.PSID_child, file = "df.PSID_child.RData")

# select adults (23-60) data for further analysis
df.PSID_adult <- df.PSID %>%
  dplyr::filter(age >22 & age <= 60)

# check data
summary(df.PSID_adult)

#save the data as RData for further analysis
#save(df.PSID_adult, file = "df.PSID_adult.RData")

# select elderly (age> 60) data for further analysis
df.PSID_elderly <- df.PSID %>%
  dplyr:: filter(age >60)

# check data
summary(df.PSID_elderly)

# save the data as Rdata for further analysis
#save(df.PSID_elderly, file = "df.PSID_elderly.Rdata")
# ---------------------------------------------------------------------------------------
# ---------- 3.  Reproduce SES indexes in papers--------------------------------------------
# ---------------------------------------------------------------------------------------
#####################Avinun, 2019 (CFPS) ##########################
# subjects: young adults (younger than 22 years old --> children)
# SES: subjective SES (ladder on a scale of 0-10)
#   CFPS: children's subjective SES
#   PSID: NA
## CFPS ##
avinun_CFPS <-df.CFPS_child %>%
  dplyr::mutate(SES_avinun_cfps = sss_c)
##################### Banerjee, 2020 (CFPS & PSID)  ##########################
# SUBJECT: adults
# SES: poverty (poverty or not according to household income)
# - poverty: 0 = not poverty, 1 = poverty
## CFPS ##
banerjee_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(poverty_line = (faminc/familysize)/1274)%>%
  dplyr::mutate(SES_banerjee_cfps = ifelse(poverty_line<=1, 1, 0))
## PSID ##
banerjee_PSID <- df.PSID_adult %>%
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_banerjee_psid = ifelse(fincome<= poverty_line, 1, 0))
table(banerjee_CFPS$SES_banerjee_cfps)
#################### Beatty Moody, 2019 (CFPS & psid)#####################
# subject: Adults
# SES: High SES vs Low SES
# - Poverty: 125% federal line relative to family size (considering the data was collected in 2004, we use 125% 2017 federal line)
# - Education: 0 = higher than 12 years and 1 = lower than 12 years
# - High SES: not poverty and education = 0
# - low SES: either poverty or education = 1 or both
# - [same as waldstein, 2012]
## CFPS ##
beaty_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = ifelse(cfps2010eduy_best < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(poverty_line = 1274*familysize) %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*poverty_line, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_beaty_cfps = ifelse(SES_low == 0, 0, 1))
table(beaty_CFPS$SES_beaty_cfps)
## PSID ##
beaty_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = ifelse(edu < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*poverty_line, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_beaty_psid = ifelse(SES_low == 0, 0, 1))
table(beaty_PSID$SES_beaty_psid)

#################### Betacourt, 2016b (CFPS & PSID)#######################
# Subject: infants
# SES: Low SES vs High SES
# - ITN: <= poverty line or not
# - Education: maternal, <= high school or not
# - high SES: ITN > poverty line and maternal education > high school (SES =1)
## CFPS ##
betan2_CFPS <- df.CFPS_child %>%
  dplyr::mutate(itn = (faminc/familysize)/1274) %>%
  # set 2 levels for mother's education
  dplyr::mutate(edu_m_recode_low = ifelse(meduc <= 4, 1, 0))%>%
  dplyr::mutate(SES_betan2_cfps = ifelse(itn >1 & edu_m_recode_low ==0,1,0))   # calculate composite SES score
## PSID ##
betan2_PSID <- df.PSID_child %>%
  dplyr::mutate(itn = (12060 +  (familysize-1)*4180)/fincome) %>%
  # set 2 levels for mother's education
  dplyr::mutate(edu_m_recode_low = ifelse(edu_m <= 12, 1, 0))%>%
  dplyr::mutate(SES_betan2_cfps = ifelse(itn >1 & edu_m_recode_low ==0,1,0))   # calculate composite SES score
table(betan2_CFPS$SES_betan2_cfps)
table(betan2_PSID$SES_betan2_cfps)
#################### Beydoun, 2020 #################################
# subject: adult
# SES: above poverty or below poverty 
# - 125% above or below federal poverty line in 2004 (considering the data was also collected in 2004, we use 2017 federal line)
# - poverty = 1, non-poverty = 0
## CFPS ##
beydoun_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(poverty_line = (faminc/familysize)/1274)%>%
  dplyr::mutate(SES_beydoun_cfps = ifelse(poverty_line<=1.25, 1, 0))
## PSID ##
beydoun_PSID <- df.PSID_adult %>%
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_beydoun_psid = ifelse(fincome<= poverty_line*1.25, 1, 0))
table(beydoun_CFPS$SES_beydoun_cfps)
table(beydoun_PSID$SES_beydoun_psid)
###################Cascio,2017#####################################
# subject: adolescents
# SES: parental education
# - 1 = less than high school, 2 = high school, 3 = trade school, 4 = associate, 5 = bachelor, 6 = graduate, 7 = no response
# - Low SES = average of parents is less than or equal to 4 (0); high SES = average of parents is graduate (1)
# - Note: cannot find equivalent of trade school land associate degree with educ, so used eduy
cascio_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.5, 9.5, 12.5, 13.5, 14.5, 16.5,99),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f,
                                   breaks = c(-0.5, 9.5, 12.5, 13.5, 14.5, 16.5,99),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode))) %>%
  
  dplyr::mutate(edu_mean = (edu_m_recode + edu_f_recode)/2) %>%
  dplyr::mutate(SES_cascio_cfps = ifelse(edu_mean<=4, 0, 
                                         ifelse(edu_mean == 6, 1, NA)))
cascio_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.5, 8.5, 12.5, 13.5, 14.5, 16.5,99),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_f_recode = cut(edu_f,
                                   breaks = c(-0.5, 8.5, 12.5, 13.5, 14.5, 16.5,99),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode))) %>%
  
  dplyr::mutate(edu_mean = (edu_m_recode + edu_f_recode)/2) %>%
  dplyr::mutate(SES_cascio_cfps = ifelse(edu_mean<=4, 0, 
                                         ifelse(edu_mean == 6, 1, NA)))
################################Conant, 2017####################
# Subjects: children
# SES: maternal education
# - high school degree = 12; some college, no degree = 13, Associate’s degree = 14; Bachelor’s degree = 16; some graduate school, no degree = 17; Master’s degree = 18; and professional degree (Ph.D. or M.D.) = 20. 
# No mother had less than a high school education-> NA
# Note: - cannot find some college and associate's degree equivalent in educ so use eduy
#       - master's degree is three years in China, so used 18-19 eduy as master's (18)
#       - PSID does not have those levels: Master’s degree = 18; and professional degree (Ph.D. or M.D.) = 20
## CFPS##
conant_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.5, 9.5, 12.5, 13.5, 14.5, 16.5,17.5,19.5,99),
                                   labels = c("0", "12", "13", "14", "16", "17", "18", "20"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_conant_cfps = na_if(edu_m_recode, 0))
summary(conant_CFPS$SES_conant_cfps)
## PSID ##
conant_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.5, 8.5, 12.5, 13.5, 14.5, 16.5,99),
                                   labels = c("0", "12", "13", "14", "16", "17"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode)))%>%
  dplyr::mutate(SES_conant_psid = na_if(edu_m_recode, 0))
summary(conant_PSID$SES_conant_psid)

###############################Degeilh, 2020#######################
# subjects: children
# SES: parental education and ITN
# - parental education
# - ITN: collected in bins:1 = less than CAD 20,000 (n = 1); 2 = CAD 20,000 to CAD 39,000 (n = 3); 3 = CAD 40,000 to CAD 59,000 (n = 2); 4 = CAD 60,000 to CAD 79,000 (n = 5); 5 = CAD 80,000 to CAD 99,000 (n = 8); 6 = CAD 100,000 and up (n = 8).
#        ITN calculated by median of the bin
#   CFPS: CAD(2020) --> CAD(2010): CAD2020* (100/117.6); CAD(2010) --> RMB(2010): CAD2010* (3.329/1.222)
#   PSID: CAD(2020) --> CAD(2017): CAD2020* (112/117.6); CAD(2010) --> US dollar(2010): CAD2010* (1/1.222)
#   ? bins not covering all incomes e.g. 20000-39000 next bin is 40000-59000
## CFPS ##
convert_idx = (100/117.6)*(3.329/1.222)
degeilh_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc_bin = ifelse(faminc < 20000*convert_idx, 1,
                                    ifelse(faminc > 20000*convert_idx & faminc < 39000*convert_idx, 2,
                                           ifelse(faminc > 40000*convert_idx & faminc < 59000*convert_idx, 3,
                                                  ifelse(faminc > 60000*convert_idx & faminc < 79000*convert_idx, 4,
                                                         ifelse(faminc > 80000*convert_idx & faminc < 99000*convert_idx, 5,
                                                                ifelse(faminc > 100000*convert_idx, 6, NA))))))) %>%
  dplyr::mutate(faminc_bin_median = recode(faminc_bin,'1'='10000', '2' = '29500', '3' = '49500', '4' = '69500', '5' = '89500', '6' = '109500')) %>%
  dplyr::mutate(faminc_bin_median = as.numeric(as.character(faminc_bin_median))*convert_idx) %>%
  dplyr::mutate(SES_degeilh_cfps_itn = (faminc_bin_median/familysize)/1274) %>%
  dplyr::mutate(SES_degeilh_cfps_edu = (eduy_m+eduy_f)/2)
summary(degeilh_CFPS$SES_degeilh_cfps_itn)
summary(degeilh_CFPS$SES_degeilh_cfps_edu)
## PSID ##
convert_idx = (112/117.6)*(1/1.222)
degeilh_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_bin = ifelse(fincome < 20000*convert_idx, 1,
                                    ifelse(fincome > 20000*convert_idx & fincome < 39000*convert_idx, 2,
                                           ifelse(fincome > 40000*convert_idx & fincome < 59000*convert_idx, 3,
                                                  ifelse(fincome > 60000*convert_idx & fincome < 79000*convert_idx, 4,
                                                         ifelse(fincome > 80000*convert_idx & fincome < 99000*convert_idx, 5,
                                                                ifelse(fincome > 100000*convert_idx, 6, NA))))))) %>%
  dplyr::mutate(fincome_bin_median = recode(fincome_bin,'1'='10000', '2' = '29500', '3' = '49500', '4' = '69500', '5' = '89500', '6' = '109500')) %>%
  dplyr::mutate(fincome_bin_median = as.numeric(as.character(fincome_bin_median))*convert_idx) %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_degeilh_psid_itn = fincome_bin_median/poverty) %>%
  dplyr::mutate(SES_degeilh_psid_edu = (edu_m+edu_f)/2)
summary(degeilh_PSID$SES_degeilh_psid_itn)
summary(degeilh_PSID$SES_degeilh_psid_edu)

###########################Dejoseph, 2021#########################

# Subject: children
# SES: itn & highest of parental education
# - itn: in bins <$5000, $5,000 - $11,999, $12,000 - $15,999, $16,000 - $24,999, $25,000 - $34,999, $35,000 - $49,999, $50,000 - $74,999, $75,000 - $99,999, $100000 - $199999, >=$200000
#        use average of each binned income to calculate itn
#   CFPS: US dollar (2021) --> US dollar (2010): US(2021) *(100/124.3), US dollor (2010) --> RMB (2010): 3.329
# - parental education: highest of father and mother's education year (or education year of one parent if only one available)
## CFPS ##
convert_idx = (100/124.3)*3.329
dejoseph_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc_bin = ifelse(faminc < 5000*convert_idx, 1,
                                    ifelse(faminc < 11999*convert_idx, 2,
                                           ifelse(faminc < 15999*convert_idx, 3,
                                                  ifelse(faminc < 24999*convert_idx, 4,
                                                         ifelse(faminc < 34999*convert_idx, 5,
                                                                ifelse(faminc < 49999*convert_idx, 6,
                                                                       ifelse(faminc < 74999*convert_idx, 7,
                                                                              ifelse(faminc < 99999*convert_idx, 8,
                                                                                    ifelse(faminc < 199999*convert_idx, 9,
                                                                                          ifelse(faminc > 200000*convert_idx, 10, NA))))))))))) %>%
  dplyr::mutate(faminc_bin_median = recode(faminc_bin,'1'='2500', '2' = '8500', '3' = '14000', '4' = '20500', '5' = '30000', '6' = '42500', '7' = '62500', '8' = '87500', '9' = '150000','10' = '250000')) %>%
  dplyr::mutate(faminc_bin_median = as.numeric(as.character(faminc_bin_median))*convert_idx) %>%
  dplyr::mutate(SES_dejoseph_cfps_itn = (faminc_bin_median/familysize)/1274) %>%
  dplyr::mutate(SES_dejoseph_cfps_edu = ifelse(is.na(eduy_f), eduy_m,
                                              ifelse(is.na(eduy_m), eduy_f, pmax(eduy_f, eduy_m))))
summary(dejoseph_CFPS$SES_dejoseph_cfps_edu)
summary(dejoseph_CFPS$SES_dejoseph_cfps_itn)

## PSID ##
convert_idx = (112.4/124.3)
dejoseph_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_bin = ifelse(fincome < 5000*convert_idx, 1,
                                    ifelse(fincome < 11999*convert_idx, 2,
                                           ifelse(fincome < 15999*convert_idx, 3,
                                                  ifelse(fincome < 24999*convert_idx, 4,
                                                         ifelse(fincome < 34999*convert_idx, 5,
                                                                ifelse(fincome < 49999*convert_idx, 6,
                                                                       ifelse(fincome < 74999*convert_idx, 7,
                                                                              ifelse(fincome < 99999*convert_idx, 8,
                                                                                     ifelse(fincome < 199999*convert_idx, 9,
                                                                                            ifelse(fincome > 200000*convert_idx, 10, NA))))))))))) %>%
  dplyr::mutate(fincome_bin_median = recode(fincome_bin,'1'='2500', '2' = '8500', '3' = '14000', '4' = '20500', '5' = '30000', '6' = '42500', '7' = '62500', '8' = '87500', '9' = '150000','10' = '250000')) %>%
  dplyr::mutate(fincome_bin_median = as.numeric(as.character(fincome_bin_median))*convert_idx) %>%
  dplyr::mutate(SES_dejoseph_psid_itn = (fincome_bin_median/familysize)/1274) %>%
  dplyr::mutate(SES_dejoseph_psid_edu = ifelse(is.na(edu_f), edu_m,
                                               ifelse(is.na(edu_m), edu_f, pmax(edu_f, edu_m))))
summary(dejoseph_PSID$SES_dejoseph_psid_edu)
summary(dejoseph_PSID$SES_dejoseph_psid_itn)
############################Demirlira, 2016#########################
# subject: children
# SES: Hollingshead occupation *7 + education *4
# - highest of mother and father if both or use the only one 
# - education in 7 levels and occupation in 9 levels
# Only with CFPS 
# (similar to McDermott, 2019)
## CFPS ##
demirlira_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.5, 7.5, 9.5,12.5,13.5,14.5,16.5,22.5), 
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education  (mother)
  dplyr::mutate(occup_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                        "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation  (mother)
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.5, 7.5, 9.5,12.5,13.5,14.5,16.5,22.5), 
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education  (father)
  dplyr::mutate(occup_f_recode = recode(egp_f, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, 
                                        "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation (father)
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(occup_f_recode = as.numeric(as.character(occup_f_recode)),  # convert variables into numeric ones
                occup_m_recode = as.numeric(as.character(occup_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(occup_recode = ifelse(is.na(occup_f_recode),occup_m_recode,
                                      ifelse(is.na(occup_m_recode),occup_f_recode, pmax(occup_f_recode,occup_m_recode)))) %>%
  dplyr::mutate(edu_recode = ifelse(is.na(edu_f_recode),edu_m_recode,
                                    ifelse(is.na(edu_m_recode),edu_f_recode, pmax(edu_f_recode, edu_m_recode)))) %>%
  dplyr::mutate(SES_demirlira_cfps = occup_recode*7 + edu_recode*4) # calculate composite SES score
############################Dougherty, 2020#########################
# subject: senior (mean age under 60 --> adult)
# SES: education + income
#  - education: 1 = ??? 12; 2 = 13-16, 3 = ??? 17; 
#  - income: 1 = ??? $49,999; 2 = $50,000-$99,999; 3 = ??? $100,000) 
#  - eduction + income (2-6)
# convert income: US dollar (2020) --> US dollar (2010): US(2020) *(100/118.7), US dollor (2010) --> RMB (2010): 3.329
## CFPS ##
convert_idx = (100/118.7) * 3.329
dougherty_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best, 
                                   breaks = c(-0.5, 12.5, 16.5,22.5), 
                                   labels = c("1", "2", "3"))) %>%        # recode education
  dplyr::mutate(fincome_bin = ifelse(faminc < 49999*convert_idx, 1,
                                     ifelse(faminc < 99999*convert_idx, 2,3)))%>%
  dplyr::mutate(edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_dougherty_cfps = fincome_bin+ edu_recode)
table(dougherty_CFPS$SES_dougherty_cfps)

## PSID ##
convert_idx = (112.4/118.7) 
dougherty_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = cut(edu, 
                                 breaks = c(-0.5, 12.5, 16.5,22.5), 
                                 labels = c("1", "2", "3"))) %>%        # recode education
  dplyr::mutate(fincome_bin = ifelse(fincome < 49999*convert_idx, 1,
                                     ifelse(fincome < 99999*convert_idx, 2,3)))%>%
  dplyr::mutate(edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_dougherty_psid = fincome_bin+ edu_recode)
table(dougherty_PSID$SES_dougherty_psid)

##############################Dufford, 2017############################
# subject: children
# SES: INR and maternal education
# - INR: family income adjusted for poverty line of certain family size
# - maternal education: in years
## CFPS ##
dufford_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_dufford_edu_cfps = eduy_m) %>%
  dplyr::mutate(SES_dufford_inr_cfps = (faminc/familysize)/1274)
summary(dufford_CFPS$SES_dufford_edu_cfps)
summary(dufford_CFPS$SES_dufford_inr_cfps)
## PSID ##
dufford_PSID <- df.PSID_child %>%
  dplyr::mutate(SES_dufford_edu_psid = edu_m) %>%
  dplyr::mutate(SES_dufford_edu_psid = na_if(SES_dufford_edu_psid, 99)) %>%
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_dufford_inr_psid = fincome/poverty_line)
table(dufford_PSID$SES_dufford_edu_psid)
summary(dufford_PSID$SES_dufford_inr_psid)
########################################Elbejjani, 2017#########################
# subject: adults
# SES: parental education for childhood SEP; own education for early adulthood SEP and occupation (isco) for midlife SEP
# - summed parental education: Father's and mother's education in four levels (0 no school; 1 some primary school; 2 2,some secondary school; 3,some university studies) 
#     - 0 & 1 --> low; >=2 --> high
# - own education:  own educational attainment (0,primary school diploma or lower-->low; 1,technical/intermediate school degree; 2,second- ary school degree; 3,some university studies --> high)
# - skill level 1 & 2 -> low ; skill level 3 & 4 --> high
# Aggregate three SEP: number of period categorized as low
# Note: psid cannot be reproduced
elbejjani_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best, 
                                 breaks = c(-0.5, 6.5, 9.5,12.5,22.5), 
                                 labels = c("0","1", "2", "3"))) %>%        # recode education
  dplyr::mutate(feduc_recode = cut(feduc, 
                            breaks = c(-0.5,0.5, 6.5, 12.5,22.5), 
                            labels = c("0","1", "2", "3"))) %>%
  dplyr::mutate(meduc_recode = cut(meduc, 
                                   breaks = c(-0.5,0.5, 6.5, 12.5,22.5), 
                                   labels = c("0","1", "2", "3"))) %>%
  dplyr::mutate(edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  
  dplyr::mutate(ses_childhood_low = ifelse(meduc_recode + feduc_recode <2,1,0)) %>%
  dplyr::mutate(ses_earlyadulthood_low = ifelse(edu_recode <1, 1,0)) %>%
  dplyr::mutate(skill_level = round(qg307isco/1000)) %>%
  dplyr::mutate(ses_midadulthood_low = ifelse(skill_level>3,1, 0)) %>%
  dplyr::mutate(SES_elbejjani_cfps = ses_midadulthood_low + ses_earlyadulthood_low + ses_childhood_low)
table(elbejjani_CFPS$SES_elbejjani_cfps)
########################Ellwood-Lowe, 2020################
# subjects: children
# SES: poverty:  part of a family of 4 with a total income of less than $25,000, or a family of 5 or more with a total income of less than $35,000. 
# - 0 = not poverty; 1 = poverty
## CFPS ##
convert_idx = (100/118.7) * 3.329
Ellwood_CFPS <- df.CFPS_child %>%
  dplyr::filter(familysize ==4 | familysize ==5) %>%
  dplyr::mutate(SES_ellwood_cfps = ifelse(familysize ==4 & faminc < 25000*convert_idx,1,
                                 ifelse(familysize ==5 & faminc <35000*convert_idx,1,0)))
table(Ellwood_CFPS$SES_ellwood_cfps)
## PSID ##
convert_idx = (112.4/118.7) 
Ellwood_PSID <- df.PSID_child %>%
  dplyr::filter(familysize ==4 | familysize ==5) %>%
  dplyr::mutate(SES_ellwood_psid = ifelse(familysize ==4 & fincome < 25000*convert_idx,1,
                                          ifelse(familysize ==5 & fincome <35000*convert_idx,1,0)))
table(Ellwood_PSID$SES_ellwood_psid)
