##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                The 1st R script for Flexibility of SES project                                             ###
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
### Betancourt, 2916	children	SES_betan_CFPS              SES_betan_PSID     	           composite           ###
### Moog, 2008	      children	SES_moog_CFPS               SES_mpog_PSID (1-5)	           composite           ###
### Jednoróg, 2012	  children	SES_jed_CFPS                NA	                           composite           ###
### McDermott, 2019	  children	SES_mcder_CFPS              NA	                           composite           ###
### Romeo, 2018a	    children	SES_romeo1_CFPS             NA	             	             composite           ###
### Romeo, 2018b	    children	SES_romeo2_CFPS             SES_romeo2_PSID                composite           ###
### Qiu, 2017	        children	SES_qiu_CFPS 	              SES_qiu_PSID                   income              ###
### Kim, 2019	        children	SES_kim_CFPS 	              SES_kim_PSID                   poverty(income)     ###
### Hanson, 2013	    children	SES_hanson_CFPS             SES_hanson_PSID 	             poverty(income)     ###
### Leonard, 2019	    children	SES_leo_CFPS                SES_leo_PSID 	                 education           ###
### Ozernov-Palchik	  children	SES_ozer_CFPS 	            SES_ozer_PSID                  education           ###
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
###     # -------------# Betancourt, L, 2016 (CFPS & PSID)--------------------------------------------#       ###
###     # -------------# Moog, et al., 2008 (CFPS & PSID)---------------------------------------------#       ###
###     # -------------# Jednoróg,  2012 (CFPS) ------------------------------------------------------#       ###
###     # -------------# McDermott, 2019 (CFPS)-------------------------------------------------------#       ###
###     # -------------# Romeo, 2018a (CFPS)----------------------------------------------------------#       ###
###     # -------------# Romeo, 2018b (CFPS & PSID)---------------------------------------------------#       ###
###     # -------------# Qiu, 2017 (CFPS & PSID)------------------------------------------------------#       ###
###     # -------------# Kim, 2019 (CFPS & PSID)------------------------------------------------------#       ###
###     # -------------# Hanson, 2013 (CFPS & PSID)---------------------------------------------------#       ###
###     # -------------# Leonard, 2019 (CFPS & PSID)--------------------------------------------------#       ###
###     # -------------# Ozernov-Palchik, O (CFPS & PSID)---------------------------------------------#       ###
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
                edu_c = cfps2010edu_best) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", c('pid_mother', "qg307egp", "cfps2010eduy_best")], by = 'pid_mother') %>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(egp_m= qg307egp,
                eduy_m = cfps2010eduy_best) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", c('pid_father',"qg307egp","cfps2010eduy_best")], by = 'pid_father') %>%
  # rename variables of parents to distinguish from children (father)
  dplyr::rename(egp_f = qg307egp,
                eduy_f = cfps2010eduy_best)

# check data
summary(df.CFPS_child)

# save the data of children (CFPS) as rdata for further processing
save(df.CFPS_child, file = "df.CFPS_child.RData")

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
                life_satisfaction = ER66025) %>%
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
  dplyr::select(pid, fid, age, sex, pid_father, pid_mother, familysize, fincome, sequence, role, depression, life_satisfaction) %>%
  dplyr::left_join(., df.PSID[df.PSID$role == "mother", c("pid_mother", "edu")], by = "pid_mother")%>%  # combine mother's variable with children
  dplyr::rename(edu_m = edu) %>%  # rename mother's variable
  dplyr::left_join(., df.PSID[df.PSID$role == "father", c("pid_father", "edu")], by = "pid_father")%>%  # combine father's variable with children
  dplyr::rename(edu_f = edu) %>%  # rename father's variable
  # select one child for every family 
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1)%>% # hcp: same here? meaning of this line?
  dplyr::ungroup()

# check data
summary(df.PSID_child)

# save the data as RData for the further analysis
save(df.PSID_child, file = "df.PSID_child.RData")

# ---------------------------------------------------------------------------------------
# ---------- 3.  Reproduce SES indexes in papers--------------------------------------------
# ---------------------------------------------------------------------------------------

####### Betancourt, L, 2016 (CFPS & PSID)###########
#  Subject: neonates
#  SES = (ITN + mother's edu)/2
#  ITN: income-to-need ratio;5 levels of family income according to poverty line 
#       4 cut-point: itn1 = poverty line, itn4 = 400% above poverty line, rest two set between itn1 and itn4)
#      * in China, use the rural poverty line as the poverty line for all population
#      * in America, poverty line depends on family size: poverty line in 2017 = 12060 +  (familysize-1)*4180
#  Mother's education: in categories, convert education year into 7 categories. see 'Education & Occupation recode.xlsx'
#
# Note: in CFPS and PSID, cannot distinguish "Technical/Vocational (3);some college (4)" levels. Only 6 levels
## CFPS ##
betan_CFPS <- df.CFPS_child %>%
  dplyr::mutate(itn = base::cut(faminc/familysize,
                                       breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), 
                                       labels = c("1", "2", "3", "4", "5"))) %>%
  # set 7 levels for mother's education
  dplyr::mutate(edu_m_recode = base::cut(eduy_m, 
                                         breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 22.5), 
                                         labels = c("1", "2", "3", "4", "5", "6"))) %>% 
  dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  dplyr::mutate(SES_betan_cfps = (itn + edu_m_recode)/2)    # calculate composite SES score

# check SES score
table(betan_CFPS$SES_betan_cfps)

## PSID ##
betan_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5", "6", NA)),  # set 7 levels for mother's education
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  # set the poverty line for every family: poverty line 12060 for one people and increase 4180 for an extra person
  dplyr::mutate(itn1 = 12060 +  (familysize-1)*4180) %>% 
  # get the interval of ITN 
  dplyr::mutate(itn = ifelse(fincome < itn1, 1, 
                                 ifelse(itn1 <= fincome & fincome < itn1*2, 2,
                                         ifelse(itn1*2 <= fincome & fincome < itn1*3, 3,
                                                ifelse(itn1*3 <= fincome & fincome < itn1*4, 4, 
                                                       ifelse(itn1*4 <= fincome, 5, NA)))))) %>%  # No NA
  dplyr::mutate(SES_betan_psid = (itn + edu_m_recode)/2)   # calculate composite SES

# check SES score
table(betan_PSID$SES_betan_psid)

######## Moog, et al., 2008 (CFPS & PSID)######
# Subject：neonates
# SES = (mother's highest education + income)/2
# mother's highest edu: 5 levels, see 'Education & Occupation recode.xlsx'
# income: recode income into 5 levels 
#        original (year 2018) categories: <= $15,000, >=100,000 (no specific bins, set as 15,000, 45,000, 75,000, 100,000)
#        CFPS: convert to equivalence in US 2010 using CPI and then convert to equivalence in China 2010 using PPP
#             CPI US 2010/2008 = 218.056/215.303 = 1.013; PPP China CNY/US dollar (2010) = 3.329
#        PSID: convert to equivalence in US 2010
## CFPS ##
moog_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_cat = dplyr::recode_factor(meduc, "1" = 1,"2" = 1,"3" = 1,
                                               "4" = 2,"5" = 3,"6" = 4,"7" = 5,"8" = 5))  %>% # recode education
  dplyr::mutate(income_cat = base::cut(faminc, #recode income, 
                               breaks= c(-0.01, 15000*1.013*3.329, 45000*1.013*3.329, 
                                         75000*1.013*3.329, 100000*1.013*3.329, 300000), 
                               labels = c("1", "2", "3", "4", "5"))) %>% 
  dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),# convert income and education into numeric variables
                edu_cat = as.numeric(as.character(edu_cat)))%>% 
  dplyr::mutate(SES_moog_cfps = (edu_cat+income_cat)/2)  # calculate composite SES

# check SES score
table(moog_CFPS$SES_moog_cfps)

##PSID##
moog_PSID <- df.PSID_child %>%
  dplyr::mutate(income_cat=cut(fincome, 
                               breaks= c(-0.01, 15000*1.139, 45000*1.139, 75000*1.139, 100000*1.139, 300000), 
                               labels = c("1", "2","3", "4","5"))) %>% #recode income:CPI US 2017/2008 = 245.120/215.303 = 1.139;
  dplyr::mutate(edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.00001, 8.5, 12.5, 14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4",  "5", NA))) %>% #recode education: cut into 5 groups
  dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),  # convert variables into numeric ones
                edu_m_recode = as.numeric(as.character(edu_m_recode)))%>%
  dplyr::mutate(SES_moog_psid = (income_cat + edu_m_recode)/2)  # calculate composite SES

# check SES score
table(moog_PSID$SES_moog_psid)

########## Jednoróg,  2012 (CFPS only) ##########
# Subject: child 
# SES = mother's education * 4 + mother's occupation *7
# mother's education: see 'Education & Occupation recode.xlsx'
# mother's occupation: see 'Education & Occupation recode.xlsx'
# Note: we reversed the score from the original so that the correlation between it and other SES scores are positive
# Note: only reproduced using CFPS data because occupation data is not available from PSID
## CFPS ##
jed_CFPS <- df.CFPS_child %>%
  dplyr::mutate(occup_ses = recode(egp_m, "1"= 7, "2"=6, "3"=5,  "7"= 5, "4"=4, "5"= 4, 
                                   "6"= 4, "8"= 3, "9"=2, "10"=1, "11"=1,
                                   "-8"=-8,"80000"= 8, .default = -8)) %>%  # recode occupation
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01,3.5,6.5,9.5,12.5,14.5,16.5, 22.5), 
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%  # recode education into 7 levels
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(occup_ses = as.numeric(as.character(occup_ses)),  # calculate composite SES score
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_jed_cfps = 4*edu_m_recode + 7*occup_ses)   # calculate composite SES score

# check SES score
table(jed_CFPS$SES_jed_cfps)

############ McDermott, 2019 (CFPS only)##########
# Subject: child/young adult
# SES: (SES_father + SES_mother)/2 (replace each other if one of them is NA)
# SES_father = father's occupation*5 + father's education*3; 
# SES_mother = mother's occupation*5 + mother's education*3
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: can only be reproduced using CFPS data because of the occupation

## CFPS ##
mcder_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education  (mother)
  dplyr::mutate(occup_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                        "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation  (mother)
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education  (father)
  dplyr::mutate(occup_f_recode = recode(egp_f, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, 
                                        "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation (father)
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(occup_f_recode = as.numeric(as.character(occup_f_recode)),  # convert variables into numeric ones
                occup_m_recode = as.numeric(as.character(occup_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_f = occup_f_recode*5 + edu_f_recode*3,      # calculate SES of father and mother
                SES_m = occup_m_recode*5 + edu_m_recode*3)%>%
  dplyr::mutate(SES_f = ifelse(is.na(SES_f), SES_m, SES_f),
                SES_m = ifelse(is.na(SES_m), SES_f, SES_m))%>%  # replace father and mother's ses with each other if one is NA
  dplyr::mutate(SES_mcder_cfps = (SES_m + SES_f)/2)             # calculate composite SES score

# check SES score
table(mcder_CFPS$SES_mcder_cfps)

############# Romeo, 2018a (CFPS)##################
# Subject: adolescent
# SES = mother's education and occupation
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: only reproduced using CFPS data because of the occupation data

romeo1_CFPS <- df.CFPS_child %>%
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
table(romeo1_CFPS$SES_romeo1_cfps)

############### Romeo, 2018b (CFPS & PSID)############
# Subject: adolescent
# SES = (Z_{parents' education} + Z_{family income})/2
# education: recode into 5 levels (0-4) and then calculate z-score of the parents' education 
#           (replace parents' education with each other if one of them is NA)

## CFPS ##
romeo2_CFPS <- df.CFPS_child %>%
  # recode income 
  dplyr::mutate(income_zscore = (faminc - mean(faminc, na.rm = TRUE))/sd(faminc, na.rm = TRUE)) %>%
  dplyr::mutate(edu_f_recode = cut(feduc, 
                                   breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5), 
                                   labels = c("1", "2", "3", "4","5"))) %>%  # calculate composite parents' education score
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode))-1) %>% # recode education 0-4
  dplyr::mutate(edu_m_recode = cut(meduc, 
                                   breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5), 
                                   labels = c("1", "2", "3", "4","5"))) %>%  # calculate composite parents' education score
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))-1) %>% # recode education 0-4
  dplyr::mutate(edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode),  # calculate composite parents' education score
                edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode))%>% 
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2)%>%  # calculate composite parents' education score
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%  # calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_cfps = (edu_zscore + income_zscore)/2)  # calculate mean of edu and income (ses)
# check SES score
summary(romeo2_CFPS$SES_romeo2_cfps)

## PSID ##
romeo2_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.00001, 8.5, 12.5,  14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5",  NA))) %>%  # set NA
  dplyr::mutate(edu_f_recode = cut(edu_f, 
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

########### Qiu, 2017 (CFPS & PSID)############
# SES = household income 

## CFPS ##
qiu_CFPS <- df.CFPS_child%>%
  dplyr::mutate(SES_qiu_cfps = faminc)

# check SES score
summary(qiu_CFPS$SES_qiu_cfps)

## PSID ##
qiu_PSID <- df.PSID_child %>%
  dplyr::select(pid, fid, fincome) %>%
  dplyr::mutate(SES_qiu_psid = fincome) 

# check SES score
summary(qiu_PSID$SES_qiu_psid)

############## Kim, 2019 (CFPS & PSID)###############
# SES = log10[(family income)/(poverty line)]
# Note: for PSID, family poverty line varies according to family size

## CFPS ##
kim_CFPS <- df.CFPS_child %>%
  dplyr::mutate(INR = (faminc/familysize)/1274)  %>%    # calculate INR 
  dplyr::mutate(INR = log10(INR)) %>%                   # calculate INR 
  dplyr::mutate(SES_kim_cfps = ifelse(!is.finite(INR), NA, INR)) %>% # set infinite value as NA
  dplyr::select(SES_kim_cfps,pid)

# check SES score
summary(kim_CFPS$SES_kim_cfps)

## PSID ##
kim_PSID <- df.PSID_child %>%  
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%     # calculate poverty line for every family
  dplyr::mutate(SES_kim_psid = fincome/poverty) %>%    # log transformation
  dplyr::mutate(SES_kim_psid = log10(SES_kim_psid)) %>%    # log transformation
  dplyr::mutate(SES_kim_psid = ifelse(!is.finite(SES_kim_psid), NA, SES_kim_psid))  # calculate poverty line for every family
#check SES score
summary(kim_PSID$SES_kim_psid)

########################## Hanson, 2013 (CFPS & PSID)################
# SES = family poverty line (cut into 3 levels)

## CFPS ##
hanson_CFPS <- df.CFPS_child %>%
  dplyr::mutate(FPL = (faminc/familysize)/1274) %>%   # calculate FPL
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>%   #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps = as.numeric(as.character(SES_hanson_cfps)))  #convert SES score into numeric
# check SES score
table(hanson_CFPS$SES_hanson_cfps)
  
## PSID ##
hanson_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty) %>%   # calculate FPL according to poverty line
  dplyr::mutate(SES_hanson_psid = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>%   # 200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_psid = as.numeric(as.character(SES_hanson_psid)))   # convert SES score into numeric

# check SES score
table(hanson_PSID$SES_hanson_psid)

#################### Leonard, 2019 (CFPS & PSID) ######################
# SES = maternal education: dichotomous, divided by college

## CFPS ##
leo_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_leo_cfps = cut(meduc, breaks = c(0, 4.5, 8.5), labels = c("1", "2"))) %>%  # recode education
  dplyr::mutate(SES_leo_cfps = as.numeric(as.character(SES_leo_cfps))-1)   # convert it to numeric one

# check SES score
table(leo_CFPS$SES_leo_cfps)
  
## PSID ##
leo_PSID <- df.PSID_child %>%
  dplyr::mutate(SES_leo_psid = cut(edu_m, breaks = c(-0.01, 12.5, 17.5, 100), labels = c("1", "2", NA))) %>%  # recode education
  dplyr::mutate(SES_leo_psid = as.numeric(as.character(SES_leo_psid))-1) %>%  # convert it to numeric one
  dplyr::mutate(SES_leo_psid = na_if(SES_leo_psid, 3))    # set NA

# check SES score
table(leo_PSID$SES_leo_psid)
  
################### Ozernov-Palchik, 2O19 (CFPS & PSID)###########
# SES: father's education (7 levels)*3 + mother's education (7 levels)*3, 
##     cut it as two levels (median as cutting point)
# Note: same as romeo(a)--Barratt Simplified Measure of Social Status (BSMSS) 3-21(1-7 multiply 3)

## CFPS ##
ozer_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, breaks = c(-0.5, 6.5, 9.5,11.5,12.5,14.5,16.5,22.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%  # composite education score of parents
  dplyr::mutate(edu_f_recode = cut(eduy_m, breaks = c(-0.5, 6.5, 9.5,11.5,12.5,14.5,16.5,22.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),  # composite education score of parents
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode))%>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))*3,  # composite education score of parents
                edu_f_recode = as.numeric(as.character(edu_f_recode))*3) %>%
  dplyr::mutate(edu_parents = edu_m_recode + edu_f_recode) %>%  # composite education score of parents
  dplyr::mutate(SES_ozer_cfps = cut(edu_parents, breaks = c(0, median(edu_parents, na.rm = TRUE), 100), labels = c('1', '2')))%>%  #recode education
  dplyr::mutate(SES_ozer_cfps = as.numeric(as.character(SES_ozer_cfps))-1)   # recode education
#check SES score
table(ozer_CFPS$SES_ozer_cfps)

## PSID ##
ozer_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%  #recode education (father)
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%  #recode education (mother)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),  # replace father with mother and verse versa
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(edu_f_recode = na_if(edu_f_recode, 8),  #set NA
                edu_m_recode = na_if(edu_m_recode, 8)) %>%
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),  # replace father with mother and verse versa
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode)) %>%
  dplyr::mutate(edu_parents = edu_m_recode*3 + edu_f_recode*3) %>%  # composite education score of parents
  dplyr::mutate(SES_ozer_psid = cut(edu_parents, breaks = c(0, median(edu_parents, na.rm = TRUE)-0.01, 100), labels = c('1', '2')))%>%  #convert SES score into numeric
  dplyr::mutate(SES_ozer_psid = as.numeric(as.character(SES_ozer_psid))-1)  #convert SES score into numeric

# check SES score
table(ozer_PSID$SES_ozer_psid)

# ---------------------------------------------------------------------------------------
# ---------- 4.  extract columns of pid and SES from all the dataframes of SES ----------
# ---------------------------------------------------------------------------------------

# save SES score reproduced from CFPS
# extract CFPS data from each data frames  
names_dataframe_cfps <- list(betan_CFPS, moog_CFPS, jed_CFPS,
                             mcder_CFPS, romeo1_CFPS, romeo2_CFPS,
                             qiu_CFPS, kim_CFPS, hanson_CFPS, leo_CFPS, ozer_CFPS) 

# extract number of papers
N_SES_CFPS <- length(names_dataframe_cfps)
# create a list to store all the SES scores 
SES_dataframes_cfps <- replicate(N_SES_CFPS, data.frame())

# put each SES score and pid information in each element of the list
for (i in 1:N_SES_CFPS) {
  SES_dataframes_cfps[[i]] <- names_dataframe_cfps[[i]][c(1, ncol(names_dataframe_cfps[[i]]))]
  print(SES_dataframes_cfps)
}
dataframes_cfps <-  Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), SES_dataframes_cfps) 
dataframes_cfps
# save the list
save(dataframes_cfps, file = 'SES_CFPS.RData')

# save SES score reproduced from PSID
# extract data 
names_dataframe_psid <- list(betan_PSID, moog_PSID,
                             romeo2_PSID, qiu_PSID, kim_PSID, 
                             hanson_PSID, leo_PSID, ozer_PSID)

# extract number of papers
N_SES_PSID <- length(names_dataframe_psid)

# create a list to store all the SES scores 
SES_dataframes_psid <- replicate(N_SES_PSID, data.frame())

# put each SES score and pid information in each element of the list
for (i in 1:N_SES_PSID) {
  SES_dataframes_psid[[i]] <- names_dataframe_psid[[i]][c(1, ncol(names_dataframe_psid[[i]]))]
  print(SES_dataframes_psid)
}
dataframes_psid<- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), SES_dataframes_psid) 

# save the list
save(dataframes_psid, file = "SES_PSID.RData")