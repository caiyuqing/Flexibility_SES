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
save(df.CFPS_child, file = "df.CFPS_child.RData")

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
save(df.CFPS_elderly, file = "df.CFPS_elderly.Rdata")
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
tmp<-as.data.frame(colnames(df.PSID))
write.csv(tmp, file = "colnames.csv")
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
save(df.PSID_child, file = "df.PSID_child.RData")

# select adults (23-60) data for further analysis
df.PSID_adult <- df.PSID %>%
  dplyr::filter(age >22 & age <= 60) 

# check data
summary(df.PSID_adult)

#save the data as RData for further analysis
save(df.PSID_adult, file = "df.PSID_adult.RData")

# select elderly (age> 60) data for further analysis
df.PSID_elderly <- df.PSID %>%
  dplyr:: filter(age >60)

# check data
summary(df.PSID_elderly)

# save the data as Rdata for further analysis
save(df.PSID_elderly, file = "df.PSID_elderly.Rdata")
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

#################### Knickmeyer,2017 (CFPS & PSID)########################
# Subject: neonates
# SES: Maternal education (years), paternal education (years)
#      Income (<200% FPL, 200%-400% FPL, >400% FPL) 
# Note: two separate SESs

## CFPS ##
knick_CFPS <- df.CFPS_child %>%
  dplyr::mutate(FPL = (faminc/familysize)/1274) %>%   # calculate FPL
  dplyr::mutate(SES_knick_cfps_inc = cut(FPL, 
                                         breaks = c(0, 2, 4, Inf), 
                                         labels = c("1", "2", "3")))%>%   #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_knick_cfps_edu = (eduy_f +eduy_m)/2) #education of parents: average of father and mother's education

summary(knick_CFPS$SES_knick_cfps_edu)
summary(knick_CFPS$SES_knick_cfps_inc)

## PSID ##
knick_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty) %>%   # calculate FPL according to poverty line
  dplyr::mutate(SES_knick_psid_inc = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>%   # 200%, 400% of poverty line as cut point
  dplyr::mutate(edu_f = na_if(edu_f, 99),
                edu_m = na_if(edu_m, 99)) %>%
  dplyr::mutate(SES_knick_psid_edu = (edu_f + edu_m)/2) #education of parents: average of father and mother's education
summary(knick_PSID$SES_knick_psid_inc)
summary(knick_PSID$SES_knick_psid_edu)

####################Brain Development Cooperative Group, 2012#######################
## Subject: children
## SES: income  and education
## Note: two separate SESs


################### Lawson, 2013 (CFPS & PSID)###################
## SES: cut into bins and use the midpoint of each bin then adjust for family size (based on adjustments used by the US department of Housing and Urban Development (HUD), see https://www.huduser.gov/portal/datasets/il/fmr99/sect82.html
##      bins: $5, 000; $10, 000; $15,000;$25,000;$35, 000; $50, 000; $75, 000; $100, 000; $150, 000;
##      education (in 6 levels, see education & occupation.xlsx): education of parents: sum of father and mother (2-12) and square-root transformed
# Note: two separate SESs

## CFPS ## 
lawson_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(feduc, 
                                   breaks = c(-0.01, 3.5, 4.5,5.5,6.5,7.5, 8.5), 
                                   labels = c("1", "2", "3", "4", "5", "6")),
                edu_m_recode = cut(meduc, 
                                   breaks = c(-0.01, 3.5, 4.5,5.5,6.5,7.5, 8.5), 
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_lawson_cfps_edu = sqrt(edu_f_recode+edu_m_recode)) %>%
  # convert income into bins and use the median of the bin as the converted income (CPI US 2010/2013 = 0.93; PPP China CNY/US dollar (2010) = 3.329;)
  dplyr::mutate(income_bin = ifelse(faminc<= 4999*0.93*3.329, 4999*0.93*3.329*0.9, ## here use the approximate of original conversion (<5000-->4500)
                                    ifelse(faminc <= 10000*0.93*3.329, median(c(5000*0.93*3.329, 10000*0.93*3.329)),
                                           ifelse(faminc <= 15000*0.93*3.329, median(c(10000*0.93*3.329, 15000*0.93*3.329)),
                                                  ifelse(faminc <= 25000*0.93*3.329, median(c(15000*0.93*3.329, 25,000*0.93*3.329)),
                                                         ifelse(faminc <=35000*0.93*3.329, median(c(25000*0.93*3.329, 35000*0.93*3.329)),
                                                                ifelse(faminc <= 50000*0.93*3.329, median(c(35000*0.93*3.329,50000*0.93*3.329)),
                                                                       ifelse(faminc <= 75000*0.93*3.329, median(c(50000*0.93*3.329, 75000*0.93*3.329)),
                                                                              ifelse(faminc <= 100000*0.93*3.329, median(c(75000*0.93*3.329, 100000*0.93*3.329)),
                                                                                     ifelse(faminc <= 150000*0.93*3.329, median(c(100000*0.93*3.329,150000*0.93*3.329)), 
                                                                                            150000*0.93*3.329*1.125)))))))))) %>%## here use the approximate of original conversion (>100000-->112500)
  dplyr::mutate(income_bin_adjusted = ifelse(familysize ==1, income_bin*0.7,
                                             ifelse(familysize ==2, income_bin*0.8,
                                                    ifelse(familysize ==3, income_bin*0.9,
                                                           ifelse(familysize ==4, income_bin, income_bin*(1+0.08*(familysize-4))))))) %>%# adjust for family size
  dplyr::rename(SES_lawson_cfps_inc = income_bin_adjusted)

summary(lawson_CFPS$SES_lawson_cfps_edu)
summary(lawson_CFPS$SES_lawson_cfps_inc)

## PSID ##
lawson_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 8.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 8.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5", NA))) %>% # 6 = NA; did not distinguish "some graduate" and "graduate" in PSID (only 5 levels)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_lawson_psid_edu = sqrt(edu_f_recode+edu_m_recode)) %>%
  dplyr::mutate(income_bin = ifelse(fincome <=5000*1.05, 4500*1.05,
                                    ifelse(fincome <=10000*1.05, median(c(5000*1.05, 10000*1.05)),
                                           ifelse(fincome<= 15000*1.05, median(c(10000*1.05, 15000*1.05)),
                                                  ifelse(fincome<= 25000*1.05,median(c(15000*1.05, 25000*1.05)),
                                                         ifelse(fincome <=35000*1.05,median(c(25000*1.05, 35000*1.05)),
                                                                ifelse(fincome <=50000*1.05, median(c(35000*1.05, 50000*1.05)),
                                                                       ifelse(fincome <= 75000*1.05, median(c(50000*1.05, 75000*1.05)),
                                                                              ifelse(fincome <= 100000*1.05, median(c(75000*1.05, 100000*1.05)),
                                                                                     ifelse(fincome <= 150000*1.05, median(c(100000*1.05, 150000*1.05)), # CPI 2017/2013= 245.120/232.957 = 1.05
                                                                                            150000*1.125*1.05)))))))))) %>%
  dplyr::mutate(income_bin_adjusted = ifelse(familysize ==1, income_bin*0.7,
                                             ifelse(familysize ==2, income_bin*0.8,
                                                    ifelse(familysize ==3, income_bin*0.9,
                                                           ifelse(familysize ==4, income_bin, income_bin*(1+0.08*(familysize-4))))))) %>%# adjust for family size
  dplyr::rename(SES_lawson_psid_inc = income_bin_adjusted)
summary(lawson_PSID$SES_lawson_psid_edu)
summary(lawson_PSID$SES_lawson_psid_inc)

#########################hanson, 2011(CFPS & PSID)##############
## Subjects: children/young adults
## SES: income: family income was categorized into 9 groups and each group was log-transformed mid-point was used
##       education: (in 7 levels, see education & occupation.xlsx) and transformed into years (?not certain whether 
#        to composite parents education (currently use composite))
#    # CPI US dollar 2010/2011 =218.056/224.939 =0.96 , PPP CNY/US dollar in 2010 = 3.329
## Note: two separate SESs
##       Although education was in years in original paper, it was transformed from categorical variable of education, 
#        so we also use categorical education index (fedu, medu) in CFPS 

## CFPS ##
hanson2_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(feduc, 
                                   breaks = c(-0.01, 2.5, 3.5, 4.5, 5.5, 6.5,7.5, 8.5), 
                                   labels = c("5", "11", "12", "14", "16", "17", "19")),
                edu_m_recode = cut(meduc, 
                                   breaks = c(-0.01, 2.5, 3.5, 4.5, 5.5, 6.5,7.5, 8.5), 
                                   labels = c("5", "11", "12", "14", "16", "17", "19"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_hanson2_cfps_edu = (edu_m_recode + edu_f_recode)/2) %>%
  dplyr::mutate(fincome_recode = cut (faminc, 
                                      breaks = c(-0.01, 5000*0.96*3.329, 10000*0.96*3.329, 15000*0.96*3.329, 35000*0.96*3.329, 
                                                 50000*0.96*3.329, 75000*0.96*3.329, 100000*0.96*3.329, 5000000*0.96*3.329),
                                      labels = c("4500","7500", "12500", "25000", "42500", "62500", "87500", "112500"))) %>% 
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  # convert the midpoint also to the equivalence of the CNY and log transform it
  dplyr::mutate(SES_hanson2_cfps_inc = log10(fincome_recode*0.96*3.329)) 
summary(hanson2_CFPS$SES_hanson2_cfps_edu)
summary(hanson2_CFPS$SES_hanson2_cfps_inc)


## PSID ##
# # CPI 2017/2011 US dollar = 245.120/224.939 = 1.090
hanson2_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01,6.5, 11.5, 12.5,14.5,16.5, 17.5, 100), 
                                   labels = c("5", "11", "12", "14", "16", "17", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01,6.5, 11.5, 12.5,14.5,16.5, 17.5, 100), 
                                   labels = c("5", "11", "12", "14", "16", "17", NA))) %>% #  did not distinguish "some graduate" and "graduate" in PSID (only 6 levels)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_hanson2_psid_edu = (edu_m_recode + edu_f_recode)/2) %>%
  dplyr::mutate(fincome_recode = cut (fincome, 
                                      breaks = c(-0.01, 5000*1.090, 10000*1.090, 15000*1.090, 35000*1.090, 50000*1.090, 
                                                 75000*1.090, 100000*1.090, 5000000*1.090), 
                                      labels = c("4500", "12500", "20000", "30000", "42500", "62500", "87500", "112500"))) %>% 
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_hanson2_psid_inc = log10(fincome_recode*1.090)) #convert to mid point also using cpi and use log transformation
summary(hanson2_PSID$SES_hanson2_psid_edu)
summary(hanson2_PSID$SES_hanson2_psid_inc)


########################## Noble, 2015 (Ursache, 2016; Brito, 2017, 2018; Piccolo, 2016, 2019; Merz, 2018) (CFPS & PSID)#############
## Subject: children/young adults
## SES: family income: recode in the means of bins (in 12 levels) and log transformed (same as previous ones)
##      education: (in 7 levels, see education & occupation.xlsx)
## Note: all papers mentioned above use the criteria of Noble, 2015
##       two separate SES
## CFPS ## 
noble_CFPS <- df.CFPS_child %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 5000*0.92*3.329, 9999*0.92*3.329, 19999*0.92*3.329, 29999*0.92*3.329, 
                                                39999*0.92*3.329, 49999*0.92*3.329, 99999*0.92*3.329,149999*0.92*3.329, 
                                                199999*0.92*3.329, 249999*0.92*3.329, 299999*0.92*3.329, 5000000*0.92*3.329), 
                                     labels = c("4500", "7500", "15000", "25000", "35000", "45000", "75000",
                                                "125000","175000", "225000", "275000", "325000"))) %>% #Incomes were  divided into 12 levels, CPI US dollar 2010/2015 = 218.056/237.017 = 0.92; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_noble_cfps_inc = log10(fincome_recode*0.92*3.329)) %>% #log transformation and also convert to the equivalence of CNY
  dplyr::mutate(edu_f_recode = cut(eduy_f, breaks = c(-0.01,  6.5, 9.5, 11.5, 12.5,15.5, 16.5, 22.5), labels = c("6","8", "10.5", "12", "14", "16", "18")),
                edu_m_recode = cut(eduy_m, breaks = c(-0.01,  6.5, 9.5, 11.5, 12.5,15.5, 16.5, 22.5), labels = c("6","8", "10.5", "12", "14", "16", "18"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_noble_cfps_edu = (edu_f_recode + edu_m_recode)/2)
summary(noble_CFPS$SES_noble_cfps_inc)
summary(noble_CFPS$SES_noble_cfps_edu)

## PSID ## 
noble_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_recode = cut(fincome, breaks = c(-0.01, 5000*1.034, 9999*1.034, 19999*1.034, 
                                                         29999*1.034, 39999*1.034, 49999*1.034, 99999*1.034,
                                                         149999*1.034, 199999*1.034, 249999*1.034, 299999*1.034, 5000000*1.034), 
                                     labels = c("4500", "7500", "15000", "25000", "35000", "45000", 
                                                "75000", "125000","175000", "225000", "275000", "325000"))) %>% #Incomes were  divided into 12 levels: CPI 2017/2015 = 245.120/237.017 =1.034
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_noble_psid_inc = log10(fincome_recode*1.034)) %>% #log transformation and also transform use cpi
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("6","8", "10.5", "12", "14", "16", "18", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("6","8", "10.5", "12", "14", "16", "18", NA))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_noble_psid_edu = (edu_f_recode + edu_m_recode)/2)
summary(noble_PSID$SES_noble_psid_inc)
summary(noble_PSID$SES_noble_psid_edu)

################## Ellwood-Lowe, 2018 (CFPS & PSID)#########################
## Subjects: adolescents
## SES: income cut into bins  and converted into midpoint of 6 bins  
##      education: average of parents (7 levels, see education & occupation.xlsx) and converted into years (do not know the exact conversion rule)
## Note: two separate SESs
## CFPS ## 
ell_CFPS <- df.CFPS_child %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 10000*3.329*0.87, 25000*3.329*0.87, 50000*3.329*0.87, 
                                                75000*3.329*0.87, 100000*3.329*0.87, 5000000*3.329*0.87), 
                                     labels = c("5000", "12500", "37500", "62500", "87500", "112500"))) %>% #Incomes were  divided into 12 levels: CPI: 2010/2018 US dollar = 218.056/251.107 = 0.87; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_ell_cfps_inc = fincome_recode*3.329*0.87) %>% # convert the mid point to equivalence of CNY
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 9.5, 12.5,13.5, 14.5, 16.5,19.5, 22.5), 
                                   labels = c("9","12", "13", "14", "16", "18", "21")),
                edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01, 9.5, 12.5,13.5, 14.5, 16.5,19.5, 22.5), 
                                   labels = c("9","12", "13", "14", "16", "18", "21"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_ell_cfps_edu = (edu_f_recode + edu_m_recode)/2) 
summary(ell_CFPS$SES_ell_cfps_inc)
summary(ell_CFPS$SES_ell_cfps_edu)

## PSID ##
ell_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_recode = cut(fincome, 
                                     breaks = c(-0.01, 10000*0.97, 25000*0.97, 50000*0.97, 75000*0.97, 100000*0.97, 5000000*0.97), 
                                     labels = c("5000", "12500", "37500", "62500", "87500", "112500"))) %>% #Incomes were  divided into 12 levels: CPI: 2017/2018 US dollar = 245.120/251.107 = 0.97
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_ell_psid_inc = fincome_recode*0.97) %>% # convert the mid point to equivalence of CNY
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 8.5,12.5, 13.5, 14.5,16.5, 17.5, 100), 
                                   labels = c("8","12", "13", "14", "16", "19.5", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 8.5,12.5, 13.5, 14.5,16.5, 17.5, 100), 
                                   labels = c("8","12", "13", "14", "16", "19.5", NA))) %>% #similar to the previous ones, no distinguish between master and doctorate (only 6 levels, last level set to 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_ell_psid_edu = (edu_f_recode + edu_m_recode)/2) 
summary(ell_PSID$SES_ell_psid_inc)
summary(ell_PSID$SES_ell_psid_edu)

#################### Luby, 2013(CFPS & PSID)#####################
## Subject: children
## SES：ITN (adjusted for family size)
##      Education: 5 levels, see education & occupation.xlsx)
## Note: also measured parents' education level, but not used as SES
## CFPS ##
luby_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_luby_cfps_inc = (faminc/familysize)/1274) %>%   # calculate itn
  dplyr::mutate(edu_f_recode = cut(feduc, 
                                   breaks = c(-0.01, 3.5,4.5,5.5, 6.5,8.5), 
                                   labels = c("1","2", "3", "4", "5")),
                edu_m_recode = cut(meduc, 
                                   breaks = c(-0.01, 3.5,4.5,5.5, 6.5,8.5), 
                                   labels = c("1","2", "3", "4", "5"))) %>% #similar to the previous ones, no distinguish between master and doctorate (only 6 levels, last level set to 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_luby_cfps_edu = (edu_f_recode + edu_m_recode)/2)
summary(luby_CFPS$SES_luby_cfps_inc)
summary(luby_CFPS$SES_luby_cfps_edu)

## PSID ##
luby_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(SES_luby_psid_inc = fincome/poverty) %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 8.5,12.5,14.5, 16.5,17.5, 100), 
                                   labels = c("1","2", "3", "4", "5", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 8.5,12.5,14.5, 16.5,17.5, 100), 
                                   labels = c("1","2", "3", "4", "5", NA))) %>% #similar to the previous ones, no distinguish between master and doctorate (only 6 levels, last level set to 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_luby_psid_edu = (edu_f_recode + edu_m_recode)/2)
summary(luby_PSID$SES_luby_psid_inc)
summary(luby_PSID$SES_luby_psid_edu)

#################### Noble, 2012(CFPS & PSID)###############
## Subjects: adolescents/young adults
## SES: ITN (adjusted for family size)
##      education: average number of years of parents
## CFPS ##
noble2_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_noble2_cfps_inc = (faminc/familysize)/1274) %>%   # calculate itn
  dplyr::mutate(SES_noble2_cfps_edu = (eduy_m + eduy_f)/2)
summary(noble2_CFPS$SES_noble2_cfps_edu)
summary(noble2_CFPS$SES_noble2_cfps_inc)

## PSID ##
noble2_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(SES_noble2_psid_inc = fincome/poverty) %>%  
  dplyr::mutate(SES_noble2_psid_edu = (edu_m + edu_f)/2)
summary(noble2_PSID$SES_noble2_psid_edu)
summary(noble2_PSID$SES_noble2_psid_inc)

############### Takeuchi, 2019(CFPS & PSID)###############
## Subjects: young adults
## SES: education: eight categories and then converted to year (first divide education year into categories and then convert to years)
##      income: in bins 1-7 1, <2 million yen; 2, 2–4 million yen; 3, 4–6 million yen; 4, 6–8 million yen; 5, 8–10 million yen; 6, 10–12 million yen; 7,  ≥12 million yen. 
## Note: two separate SESs

## CFPS ##
# CPI: Japan yen 2010/2019 = 100/105.482 = 0.94; PPP CNY/Japan yen 2010 = 3.329/111.667 = 0.03
takeu_CFPS <- df.CFPS_child %>%
  dplyr::mutate(income_cat = cut(faminc, 
                                 breaks = c(-0.01, 2000000*0.94*0.03, 4000000*0.94*0.03, 6000000*0.94*0.03, 8000000*0.94*0.03,
                                            10000000*0.94*0.03, 12000000*0.94*0.03, 5000000), 
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>% 
  dplyr::mutate(SES_takeu_cfps_inc = as.numeric(as.character(income_cat))) %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 6.5,9.5,11.5,12.5, 14.5, 16.5, 19.5,22.5), 
                                   labels = c("6","9", "11", "12", "14", "16", "18", "21")),
                edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01, 6.5,9.5,11.5,12.5, 14.5, 16.5, 19.5,22.5), 
                                   labels = c("6","9", "11", "12", "14", "16", "18", "21"))) %>% 
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_takeu_cfps_edu = (edu_f_recode + edu_m_recode)/2)
summary(takeu_CFPS$SES_takeu_cfps_inc)
summary(takeu_CFPS$SES_takeu_cfps_edu)

## PSID ##
takeu_PSID <- df.PSID_child %>%
  dplyr::mutate(income_cat = cut(fincome, breaks = c(-0.01, 2000000*0.99*0.0095, 4000000*0.99*0.0095, 6000000*0.99*0.0095, 8000000*0.99*0.0095, 10000000*0.99*0.0095, 12000000*0.99*0.0095, 5000000), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>% #CPI: Japan yen 2017/2019 = 103.963/105.482 = 0.99; PPP US dollar/Japan yen 2010 = 1/105.379 = 0.0095
  dplyr::mutate(SES_takeu_psid_inc = as.numeric(as.character(income_cat))) %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 6.5,8.5,11.5,12.5, 14.5, 16.5, 17.5,99), 
                                   labels = c("6","9", "11", "12", "14", "16", "19.5", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 6.5,8.5,11.5,12.5, 14.5, 16.5, 17.5,99), 
                                   labels = c("6","9", "11", "12", "14", "16", "19.5", NA))) %>% # not distinguish "master" and "doctorate", only 7 levels (highest level: 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_takeu_psid_edu = (edu_f_recode + edu_m_recode)/2)
summary(takeu_PSID$SES_takeu_psid_edu)
summary(takeu_PSID$SES_takeu_psid_inc)

########################Kong, 2015 (CFPS & PSID) ##########################
## Subjects: Young adults (parent's SES)
## SES: Parents' education (separate): categorical education convert to years (0-18)
##      Family SSS: ladder scale (use other SSS variables in CFPS and PSID as alternatives)
##                  CFPS: composite mother and father's subjective social status as family SSS
##                  PSID: composite reference person and spouse subjective subjective social status
## CFPS ##
kong_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(feduc,
                                   breaks = c(-0.01, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5),
                                   labels = c("0", "6", "9", "12", "15", "16", "17", "18")),
                edu_m_recode = cut(meduc,
                                   breaks = c(-0.01, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5),
                                   labels = c("0", "6", "9", "12", "15", "16", "17", "18"))) %>% # recode categorical education of parents into educational years (0-18)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(sss_family = (sss_f + sss_m)/2) %>%
  dplyr::rename(SES_kong_cfps_fedu = edu_f_recode,
                SES_kong_cfps_medu = edu_m_recode, 
                SES_kong_cfps_sss = sss_family)
summary(kong_CFPS$SES_kong_cfps_fedu)
summary(kong_CFPS$SES_kong_cfps_medu)
summary(kong_CFPS$SES_kong_cfps_sss)

## PSID ##
kong_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01,0.5, 6.5, 8.5, 12.5, 16.5, 17.5, 99),
                                   labels = c("0", "6","8", "12", "16", "18", NA)),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01,0.5, 6.5, 8.5, 12.5, 16.5, 17.5, 99),
                                   labels = c("0", "6","8", "12", "16", "18", NA))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(sss_f = na_if(sss_f, 9),
                sss_m = na_if(sss_m, 9)) %>% # set NA for sss (sss = 9 means NA, don't know, refuse to answer)
  dplyr::mutate(sss_family = (sss_f + sss_m)/2) %>%
  dplyr::rename(SES_kong_psid_fedu = edu_f_recode,
                SES_kong_psid_medu = edu_m_recode, 
                SES_kong_psid_sss = sss_family)
summary(kong_PSID$SES_kong_psid_fedu)
summary(kong_PSID$SES_kong_psid_medu)
summary(kong_PSID$SES_kong_psid_sss)

########################Hair, 2015 (CFPS & PSID) ##########################
## Subject: children/young adults
## SES: income (poverty): both in binary (200%FPL) and also in categories (100%, 150%. 200%FPL)
##      maternal education (as covariate): six levels (see education & occupation.xslx)

## CFPS ##
hair_CFPS <- df.CFPS_child %>%
  dplyr::mutate(FPL = (faminc/familysize)/1274) %>%
  dplyr::mutate(income_cat = cut(FPL,
                                 breaks = c(-0.01, 1,1.5,2,10000),
                                 labels = c("1", "2", "3", "4")),
                income_bin = ifelse(FPL > 2, 1, 0)) %>% # below 100% = 1, 100%-150% = 2, 150%-200%= 3, above 200% = 4
  dplyr::mutate(edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.01, 9.5, 12.5, 14.5, 16.5, 18.5, 22.5),
                                   labels = c("1","2", "3", "4", "5","6"))) %>%
  dplyr::mutate(SES_hair_cfps_income_cat = as.numeric(as.character(income_cat)),
                SES_hair_cfps_income_bin = as.numeric(as.character(income_bin)),
                SES_hair_cfps_medu = as.numeric(as.character(edu_m_recode)))
summary(hair_CFPS$SES_hair_cfps_income_cat)
summary(hair_CFPS$SES_hair_cfps_income_bin)
summary(hair_CFPS$SES_hair_cfps_medu)

## PSID ##
hair_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty) %>%
  dplyr::mutate(income_cat = cut(FPL,
                                 breaks = c(-0.01, 1,1.5,2,10000),
                                 labels = c("1", "2", "3", "4")),
                income_bin = ifelse(FPL > 2, 1, 0)) %>% # below 100% = 1, 100%-150% = 2, 150%-200%= 3, above 200% = 4
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.01, 8.5, 12.5, 14.5, 16.5, 17.5, 99),
                                   labels = c("1", "2", "3", "4", "5", NA))) %>% #No distinction between some graduate and graduage (set both level as 5)
  dplyr::mutate(SES_hair_psid_income_cat = as.numeric(as.character(income_cat)),
                SES_hair_psid_income_bin = as.numeric(as.character(income_bin)),
                SES_hair_psid_medu = as.numeric(as.character(edu_m_recode)))
summary(hair_PSID$SES_hair_psid_income_cat)
summary(hair_PSID$SES_hair_psid_income_bin)
summary(hair_PSID$SES_hair_psid_medu)

################## Yu,2018 (only CFPS) ########
# Subject: children & young adult
# SES: SSS (MAS): self sss
#      ITN: income data were collected in 9 categories: < $5000; $5000 ~ 11, 999; $ 12, 000 ~ 15, 999 ; $ 16, 000 ~ 24, 999; $ 25,000 ~ 34, 999; $ 35, 000 ~ 49, 999; 
#                                                       $ 50, 000 ~ 74, 999; $ 75, 000 ~ 99, 999; $ > =100, 000 
#      Education father and mother: 7 categories
#      composite: weighted factor composite PCA score for the four variables

# laod related packages
library(FactoMineR)
library(factoextra)
yu_CFPS <- df.CFPS_child %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 5000*0.868*3.329, 11999* 0.868*3.329, 15999* 0.868*3.329, 24999* 0.868*3.329, 
                                                34999* 0.868*3.329, 49999* 0.868*3.329, 74999* 0.868*3.329,99999* 0.868*3.329, 5000000*0.868*3.329), 
                                     labels = c("4500", "8500", "14000", "20500", "30000", "42500", 
                                                "62500","875000", "125000"))) %>% #Incomes were  divided into 9 levels, CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(ITN = fincome_recode/(1274*familysize)) %>%
  dplyr::mutate(edu_m_recode = cut(meduc,
                                   breaks = c(-0.01, 1.5, 3.5, 4.5, 5.5,6.5,7.5,8.5),
                                   labels = c("1", "2", "3", "4", "5", "6", "7")),
                edu_f_recode = cut(feduc,
                                   breaks = c(-0.01, 1.5, 3.5, 4.5, 5.5,6.5,7.5,8.5),
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)))%>%
  dplyr::select(ITN, edu_m_recode, edu_f_recode, sss_c) %>%
  tidyr::drop_na(.) 
# calculate PCA
yu_pca <- PCA(yu_CFPS, scale.unit = TRUE, graph = TRUE)
get_eigenvalue(yu_pca)
fviz_eig(yu_pca)#scree plot
var <- get_pca_var(yu_pca)
# calculate composite SES
yu_CFPS <- yu_CFPS %>%
  dplyr::mutate(SES_yu_cfps = var$coord[1]*ITN+ var$coord[2]*edu_m_recode +var$coord[3]*edu_f_recode + var$coord[4]*sss_c)
summary(yu_CFPS$SES_yu_cfps)

# calculate SES as first component (?PCA does not appear to be single component)
yu_cfps <- yu_cfps %>%
  dplyr::mutate(SES_yu_cfps = 0.71118313*ITN + 0.78339583*edu_f_recode + 0.81539907*edu_m_recode + (-0.04682041)*sss)

## PSID ##
## only for children who are also rp/sp in relation? --> need to check
yu_PSID <- df.PSID_child%>%
  dplyr::filter(relation == 10 | relation ==20) ###

############### Yang, 2016 ##############
# Subject: young adults
# SES: family annual income < RMB 5,000; RMB 5,000–15000; RMB 15,001–30,000; RMB 30,001–50,000; RMB 50,001–100, 000; > RMB 100,000. (in six categories and recoded as 1-6)
#      education: collected in 6 categories and recoded as years (according to chinese and us educational systems respectively)
# composite: average of z-scores of two variables
## CFPS ##
yang_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 4999/1.1705, 15000/1.1705, 30000/1.1705, 50000/1.1705, 100000/1.1705, 5000000),
                                     labels = c("1", "2", "3", "4", "5", "6"))) %>% # CPI 2016 = 627.5, CPI 2010 = 536.1, CPI 2016/CPI 2010 = 1.1705
  dplyr::mutate(edu_recode = cut(cfps2010edu_best,
                                 breaks = c(-0.01, 2.5, 3.5, 4.5, 6.5, 8.5),
                                 labels = c("6", "9", "12", "16", "19"))) %>%
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(fincome_zscore = (fincome_recode - mean(fincome_recode, na.rm = TRUE))/sd(fincome_recode, na.rm = TRUE)) %>%  # calculate z-score of family income and education
  dplyr::mutate(edu_zscore = (edu_recode - mean(edu_recode, na.rm = TRUE))/sd(edu_recode, na.rm = TRUE)) %>%
  dplyr::mutate(SES_yang_cfps = (fincome_zscore + edu_zscore)/2) 
summary(yang_CFPS$SES_yang_cfps)

## PSID ##
yang_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome, 
                                     breaks = c(-0.01, 4999/1.1705/3.329, 15000/1.1705/3.329, 30000/1.1705/3.329, 50000/1.1705/3.329, 100000/1.1705/3.329, 500000000),
                                     labels = c("1", "2", "3", "4", "5", "6"))) %>% # CPI 2016 = 627.5, CPI 2010 = 536.1, CPI 2016/CPI 2010 = 1.1705, PPP 2010 China/US = 3.329
  dplyr::mutate(edu_recode = cut(edu,
                                 breaks = c(-0.01, 6.5, 8.5, 12.5, 16.5, 18.5, 99),
                                 labels = c("6", "8", "12", "16", "18", NA))) %>%
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(fincome_zscore = (fincome_recode - mean(fincome_recode, na.rm = TRUE))/sd(fincome_recode, na.rm = TRUE)) %>%  # calculate z-score of family income and education
  dplyr::mutate(edu_zscore = (edu_recode - mean(edu_recode, na.rm = TRUE))/sd(edu_recode, na.rm = TRUE)) %>%
  dplyr::mutate(SES_yang_psid = (fincome_zscore + edu_zscore)/2) 
summary(yang_PSID$SES_yang_psid)

#################Johnson, 2013 (only CFPS) ##################
# Subject: middle aged
# SES: composite: Hollingshead two-factor: own occupation & highest formal edu (composite = 7*occu + 4* edu)
#      recode of occupation and education seee education & occupation.xlsx
johnson_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(occup_recode = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                      "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation 
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best, 
                                breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(SES_johnson_cfps = 7*occup_recode +4*edu_recode)
summary(johnson_CFPS$SES_johnson_cfps)

#################Johnson, 2013b (only CFPS) #######################
# Subject: elderly
# SES: same as Johnson, 2013
johnson2_CFPS <- df.CFPS_elderly %>%
  dplyr::mutate(occup_recode = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                      "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation 
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best, 
                                 breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(SES_johnson2_cfps = 7*occup_recode +4*edu_recode)
summary(johnson2_CFPS$SES_johnson2_cfps)

###############Waldstein, 2017################
# Subject: middle aged
# SES: dichotomous low SES:  below median education (<12 years) and/or income below 125% of the federal poverty line 
## CFPS ##
waldstein_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = ifelse(cfps2010eduy_best < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(income < 1.25*1274, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_waldstein_cfps = ifelse(SES_low == 0, 0, 1))
table(waldstein_CFPS$SES_waldstein_cfps)
## PSID ##
waldstein_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = ifelse(edu < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(income < 1.25*12060, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_waldstein_psid = ifelse(SES_low == 0, 0, 1))
table(waldstein_PSID$SES_waldstein_psid)

##############Shaked, 2018####################
# Subjects: middle aged
# SES: dichotomous low SES:  below median education and/or income below 125% of the federal poverty line 
## CFPS ##
shake_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = ifelse(cfps2010eduy_best < median(cfps2010eduy_best), 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(income < 1.25*1274, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_shake_cfps = ifelse(SES_low == 0, 0, 1))
table(shake_CFPS$SES_shake_cfps)
## PSID ##
shake_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = ifelse(edu < median(edu), 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(income < 1.25*12060, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_shake_psid = ifelse(SES_low == 0, 0, 1))
table(shake_PSID$SES_shake_psid)

###########Noble, 2012b############
# Subject: young to old adults (above 17; here use age>22 in accordance with other studies)
# SES: education
#     1. dichotomous <14/>=14yrs
#     2. categorical: high school or less (11); Some college; colege and up (16)
## CFPS ##
noble3_CFPS <- df.CFPS %>%
  dplyr::filter(age > 22) %>% # extract adults and elderly
  dplyr::mutate(SES_noble3_edu1_cfps = ifelse(cfps2010eduy_best <14, 0, 1),
                SES_noble3_edu2_cfps = cut(cfps2010eduy_best,
                                           breaks = c(-0.01, 11.5, 15.5, 22.5),
                                           labels = c("high school", "some college", "colege")))
table(noble3_CFPS$SES_noble3_edu1_cfps)
table(noble3_CFPS$SES_noble3_edu2_cfps)
## PSID ##
noble3_PSID <- df.PSID %>%
  dplyr::filter(age > 22) %>% # extract adults and elderly
  dplyr::mutate(SES_noble3_edu1_psid = ifelse(edu <14, 0, 1),
                SES_noble3_edu2_psid = cut(edu,
                                           breaks = c(-0.01, 11.5, 15.5, 17.5, 99),
                                           labels = c("high school", "some college", "colege", NA)))
table(noble3_PSID$SES_noble3_edu1_psid)
table(noble3_PSID$SES_noble3_edu2_psid)

############Cavanagh, 2013#################
# Subject: middle-aged
# SES: contains early life SES and current SES (only current SES can be calculated)
#     --> Current SES: deprivations (summed score of dichotomous score), including income (<25,000), social class (original cut point: 2/3), house owner (both CFPS and PSID only have data about whether the house belong to the family, not the individual)
#        SES: sum of the three deprivation (least deprivation = 0, most deprivation = 3)
# social class cut point: subjective social class in both CFPS and PSID was in 1-5. Use 3
## CFPS ##
cavanagh_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(income_dich = ifelse(income < 25000*0.936*3.329, 0, 1), # convert income into dichotomous variable (cut point $25,000 in 2013): CPI US dollar 2010/2013 = 218.056/232.957 = 0.936;PPP: CNY/US dollar in 2010 = 3.329
                social_dich = ifelse(qm402 <= 3, 0, 1), # convert social class into dichotomous variable
                house_dich = ifelse(fd1 <=2 | fd1 == 6, 1, 
                                ifelse(fd1 == 77, NA, 0))) %>% # convert housing into dichotomous variable: ownership of the house: in CFPS in 7 categories: 1 = "Property right solely owned by the family" 2 = "Joint property right with work unit(danwei)" 3 = "Rented"  4 = "Provided by the government for free" 5 = "Provided by work unit(danwei) for free"6 = "Provided by parents/children" 7 = "Borrowed from friends or relatives" 77 = "Other [Please specify]” —>  count 1,2,6 as owned by family and set 77 as NA
  dplyr::mutate(SES_cavanagh_cfps = income_dich + social_dich + house_dich)
table(cavanagh_CFPS$SES_cavanagh_cfps)

## PSID ## 
cavanagh_PSID <- df.PSID_adult %>%
  dplyr::mutate(income_dich = ifelse(income < 25000*1.05, 0, 1)) %>% # convert income: CPI US 2017/2013 = 245.120/232.957
  dplyr::mutate(sss_rp = ifelse(relation ==10 , sss_rp, NA), # set sss_rp only for reference person
                sss_sp = ifelse(relation ==20 | relation == 22, sss_sp, NA)) %>% # set sss_sp only for spouse/paterner
  dplyr::mutate(sss = ifelse(!is.na(sss_rp), sss_rp, sss_sp)) %>% # combine sss_sp and sss_rp into sss
  dplyr::mutate(social_dich = ifelse(sss <= 3, 0, 1)) %>% # convert social class
  dplyr::mutate(house_dich = ifelse(ER66031 ==0, 0, 1)) %>% # convert housing: ER66031 = 0, rent or neither rent or own; ER66031 = 1-9999997, actual value of the owned house; 9999998, DK; 9999999, refuse to answer (but still the owner of the house)
  dplyr::mutate(SES_cavanagh_psid = income_dich + social_dich + house_dich)
table(cavanagh_PSID$SES_cavanagh_psid)  

###################Holz, 2015####################
# Subjects: young adults
# SES: early life poverty using income (60% of the median of the adjusted family income)
#      CFPS: median income in China in 2010: CNY 16839 per capita (source: http://www.gov.cn/gzdt/2012-01/20/content_2050056.htm)
#      PSID: median family income in US in 2017: $61,372 (source: https://www.census.gov/library/publications/2018/demo/p60-263.html)
## CFPS ##
holz_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_holz_cfps = ifelse(faminc/familysize <= 0.6*16839, 1, 0)) # 0 = not exposed, 1= exposed
table(holz_CFPS$SES_holz_cfps)

## PSID ##
holz_PSID <- df.PSID_child %>%
  dplyr::mutate(SES_holz_psid = ifelse(fincome <= 0.6*61372, 1, 0))
table(holz_PSID$SES_holz_psid)

#################Piras, 2011############
# Subjects: middle-aged
# SES: Education (in years)
## CFPS ##
piras_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES_piras_cfps = cut(cfps2010edu_best, 
                                     breaks = c(-0.01, 2.5, 3.5, 4.5, 6.5, 8.5),
                                     labels = c("6", "9", "12", "16", "18"))) %>%
  dplyr::mutate(SES_piras_cfps = as.numeric(as.character(SES_piras_cfps)))
summary(piras_CFPS$SES_piras_cfps)

## PSID ##
piras_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES_piras_psid = cut(edu, 
                                     breaks = c(-0.01, 6.5, 8.5, 12.5, 16.5, 18.5, 99),
                                     labels = c("6", "8", "12", "16", "18", NA))) %>%
  dplyr::mutate(SES_piras_psid = as.numeric(as.character(SES_piras_psid)))
summary(piras_PSID$SES_piras_psid)

#############Zhu, 2018################
# Subjects: young adults
# SES: own education, household income & employment status
#      education: <11 = 11; 12; 13; 14; 15; 16; 17+ = 17
#      income: 1 = <10 000USD; 2 = 10 000 ~ 19 999; 3 = 20 000 ~ 29 999; 4 = 30 000 ~ 39 999; 5 = 40 000 49 999; 6 = 50 000 ~ 74 999; 7 = 75 000 ~ 99 999; 8 = >=100 000 USD
#      employment: not working = 0; part-time = 1; full time =2 (CFPS only distinguish employment/unemployment)
## CFPS ##
zhu_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 10000*0.868*3.329, 19999*0.868*3.329, 29999*0.868*3.329, 39999*0.868*3.329, 49999*0.868*3.329, 74999*0.868*3.329, 99999*0.868*3.329, 10000000),
                                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>% # CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best,
                                 breaks = c(-0.01, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 22.5),
                                 labels = c("11", "12", "13", "14", "15", "16", "17"))) %>%
  dplyr::mutate(employment_recode = ifelse(qg3 == 1, 2, 0)) %>%
  dplyr::mutate(SES_zhu_income_cfps = as.numeric(as.character(fincome_recode)),
                SES_zhu_edu_cfps = as.numeric(as.character(edu_recode)),
                SES_zhu_employment_cfps = employment_recode)
table(zhu_CFPS$SES_zhu_income_cfps)
table(zhu_CFPS$SES_zhu_edu_cfps)
table(zhu_CFPS$SES_zhu_employment_cfps)

## PSID ##
zhu_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 10000*0.976, 19999*0.976, 29999*0.976, 39999*0.976, 49999*0.976, 74999*0.976, 99999*0.976, 100000000000),
                                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>% # CPI US dollar 2017/2018 = 245.120/251.107 = 0.976 
  dplyr::mutate(edu_recode = cut(edu,
                                 breaks = c(-0.01, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 22.5),
                                 labels = c("11", "12", "13", "14", "15", "16", "17"))) %>%
  dplyr::mutate(employment_recode = ifelse(ER34516 == 1, 2, 
                                           ifelse(ER34516 == 2, 1,
                                                  ifelse(ER34516==9 | ER34516 == 0, NA, 0)))) %>% # 1= working; 2= Only temporarily laid off; 3-8 = not employed; 9 = DK/NA; 0 = not applicable 
  dplyr::mutate(SES_zhu_income_psid = as.numeric(as.character(fincome_recode)),
                SES_zhu_edu_psid = as.numeric(as.character(edu_recode)),
                SES_zhu_employment_psid = employment_recode)
table(zhu_PSID$SES_zhu_income_psid)
table(zhu_PSID$SES_zhu_edu_psid)
table(zhu_PSID$SES_zhu_employment_psid)

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
