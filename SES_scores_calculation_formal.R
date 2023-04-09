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
if (!require(pracma)) {install.packages("pracma",repos = "http://cran.us.r-project.org"); require(pracma)}


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
  # dplyr::left_join(., df.county, by = "countyid") %>%
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
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -8)))%>% #%>% # set all the '-8' as NA (in CFPS -8 means not applicable)
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -1))) %>% # set all the '-1' as NA (in CFPS -1 means "I don't know")
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -2))) %>% # set all the '-2' as NA (in CFPS -2 means "I don't want to answer"
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -7))) %>% # set all the '-7' as NA (in CFPS -7 means "did not answer clearly. cannot categorize")
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -9)))   # set all the '-9' as NA (in CFPS -9 means "missing value"

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
                isco_c = qg307isco,
                eduy_c = cfps2010eduy_best,
                edu_c = cfps2010edu_best,
                income_c = income,
                sss_c = qm402,
                employment_c = qg3) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", c('pid_mother', "qg307egp","qg307isco",'cfps2010edu_best', "cfps2010eduy_best", "qm402","income", "qg3")], by = 'pid_mother') %>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(egp_m= qg307egp,# occupation coding
                isco_m = qg307isco,
                edu_m = cfps2010edu_best,
                eduy_m = cfps2010eduy_best,
                income_m = income,
                sss_m = qm402,
                employment_m = qg3) %>% 
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", c('pid_father',"qg307egp","qg307isco",'cfps2010edu_best',"cfps2010eduy_best", "qm402", "income","qg3")], by = 'pid_father') %>%
  # rename variables of parents to distinguish from children (father)
  dplyr::rename(egp_f = qg307egp,
                isco_f = qg307isco,
                edu_f = cfps2010edu_best,
                eduy_f = cfps2010eduy_best,
                income_f = income,
                sss_f = qm402,
                employment_f = qg3) %>%
  dplyr::mutate(edu_f = ifelse(is.na(edu_f), feduc, edu_f),
                edu_m = ifelse(is.na(edu_m), meduc, edu_m)) %>%
  dplyr::select(pid, pid_m, pid_f,role, fid, cid, age, gender,
                edu_c,eduy_c, edu_f, eduy_f,edu_m, eduy_m,
                income_c, income_f, income_m, faminc,familysize,
                egp_c, egp_f, egp_m, isco_c, isco_f, isco_m,
                sss_c, sss_f, sss_m, employment_c, employment_f, employment_m,fd1) %>%
  dplyr::rename(housing = fd1)

# check data
names(df.CFPS_child)

# save the data of children (CFPS) as rdata for further processing
save(df.CFPS_child, file = "df.CFPS_child.RData")

# select adult (age 23-60) data for further analysis
df.CFPS_adult <- df.CFPS %>%
  dplyr::filter(age >22 & age <= 60)  

df.CFPS_adult_father <- df.CFPS %>%
  dplyr::filter(pid %in% df.CFPS_adult$pid_f) %>%
  dplyr::select(pid,qg307egp,qg307isco,cfps2010edu_best,cfps2010eduy_best,qm402,income,qg3) %>%
  dplyr::mutate(pid_f = ifelse(pid %in% df.CFPS_adult$pid_f, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(egp_f = qg307egp,
                isco_f = qg307isco,
                edu_f = cfps2010edu_best,
                eduy_f = cfps2010eduy_best,
                income_f = income,
                sss_f = qm402,
                employment_f = qg3)
df.CFPS_adult_mother <- df.CFPS %>%
  dplyr::filter(pid %in% df.CFPS_adult$pid_m) %>%
  dplyr::select(pid,qg307egp,qg307isco,cfps2010edu_best,cfps2010eduy_best,qm402,income,qg3) %>%
  dplyr::mutate(pid_m = ifelse(pid %in% df.CFPS_adult$pid_m, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(egp_m = qg307egp,
                isco_m = qg307isco,
                edu_m = cfps2010edu_best,
                eduy_m = cfps2010eduy_best,
                income_m = income,
                sss_m = qm402,
                employment_f = qg3)
df.CFPS_adult <- df.CFPS_adult %>%
  dplyr::select(pid:gender, age,qg307egp, qg307isco, cfps2010eduy_best,cfps2010edu_best, income, qm402, faminc,familysize,meduc, feduc,fd1,qg3)%>%
  dplyr::rename(egp = qg307egp,
                isco = qg307isco,
                eduy = cfps2010eduy_best,
                edu = cfps2010edu_best,
                sss = qm402,
                housing = fd1,
                employment = qg3)%>%
  dplyr::left_join(., df.CFPS_adult_father, by = 'pid_f') %>%
  dplyr::left_join(., df.CFPS_adult_mother, by = 'pid_m') %>%
  dplyr::mutate(edu_f = ifelse(is.na(edu_f), feduc, edu_f),
                edu_m = ifelse(is.na(edu_m), meduc, edu_m)) %>%
  dplyr::select(pid, pid_m, pid_f,role, fid, cid, age, gender,
                edu, eduy, edu_f, eduy_f, edu_m, eduy_m,
                income, income_f, income_m,
                egp, egp_f, egp_m, isco, isco_f, isco_m, 
                sss, sss_f, sss_m,
                faminc, familysize,housing, employment)
    
  
  
# check data
summary(df.CFPS_adult)

# save the data of adults (CFPS) as rdata for further processing
save(df.CFPS_adult, file = "df.CFPS_adult.RData")

# select elderly (age > 60)
df.CFPS_elderly <- df.CFPS %>%
  dplyr::filter(age > 60)
df.CFPS_elderly_father <- df.CFPS %>%
  dplyr::filter(pid %in% df.CFPS_elderly$pid_f) %>%
  dplyr::select(pid,qg307egp,qg307isco,cfps2010edu_best,cfps2010eduy_best,qm402,income,qg3) %>%
  dplyr::mutate(pid_f = ifelse(pid %in% df.CFPS_elderly$pid_f, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(egp_f = qg307egp,
                isco_f = qg307isco,
                edu_f = cfps2010edu_best,
                eduy_f = cfps2010eduy_best,
                income_f = income,
                sss_f = qm402,
                employment_f = qg3)
df.CFPS_elderly_mother <- df.CFPS %>%
  dplyr::filter(pid %in% df.CFPS_elderly$pid_m) %>%
  dplyr::select(pid,qg307egp,qg307isco,cfps2010edu_best,cfps2010eduy_best,qm402,income,qg3) %>%
  dplyr::mutate(pid_m = ifelse(pid %in% df.CFPS_elderly$pid_m, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(egp_m = qg307egp,
                isco_m = qg307isco,
                edu_m = cfps2010edu_best,
                eduy_m = cfps2010eduy_best,
                income_m = income,
                sss_m = qm402,
                employment_m = qg3)
df.CFPS_elderly <- df.CFPS_elderly %>%
  dplyr::select(pid:gender, age,qg307egp, qg307isco, cfps2010eduy_best,cfps2010edu_best, income, qm402, faminc,familysize,meduc, feduc,fd1,qg3)%>%
  dplyr::rename(egp = qg307egp,
                isco = qg307isco,
                eduy = cfps2010eduy_best,
                edu = cfps2010edu_best,
                sss = qm402,
                housing = fd1,
                employment = qg3)%>%
  dplyr::left_join(., df.CFPS_elderly_father, by = 'pid_f') %>%
  dplyr::left_join(., df.CFPS_elderly_mother, by = 'pid_m') %>%
  dplyr::mutate(edu_f = ifelse(is.na(edu_f), feduc, edu_f),
                edu_m = ifelse(is.na(edu_m), meduc, edu_m)) %>%
  dplyr::select(pid, pid_m, pid_f,role, fid, cid, age, gender,
                edu, eduy, edu_f, eduy_f, edu_m, eduy_m,
                income, income_f, income_m,
                egp, egp_f, egp_m, isco, isco_f, isco_m, 
                sss, sss_f, sss_m,
                faminc, familysize,housing, employment)
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
                occupation_code = ER66195,
                industry_code = ER66196,
                depression = ER70680,
                life_satisfaction = ER66025,
                sss_rp = ER70879,
                sss_sp = ER70741,
                eduf_rp =ER70845,
                edum_rp = ER70855,
                eduf_sp = ER70707,
                edum_sp = ER70717,
                housing = ER66031,
                employment = ER34516) %>%
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
  dplyr::mutate(edu_f = ifelse(relation ==10, eduf_rp,
                               ifelse(relation ==20, eduf_sp,NA))) %>%
  dplyr::mutate(edu_m = ifelse(relation ==10, edum_rp,
                               ifelse(relation ==20, edum_sp,NA))) %>%
  dplyr::mutate(sss = ifelse(relation ==10, sss_rp,
                            ifelse(relation ==20, sss_sp,NA))) %>%
  
  dplyr::filter(sequence <= 20)  # select only those still live in the family in 2017 (sequence <= 20)

## select children data (and parents SES) for the further analysis
df.PSID_child <- df.PSID %>%
  dplyr::filter(role == "child") %>%  # select only children
  # select variables related to children that are used for further analysis
  dplyr::select(pid, fid, age, sex, relation, pid_f, pid_m,pid_father, pid_mother, familysize, fincome, edu,income,occupation_code, industry_code, sequence, role, sss, edu_f, edu_m,eduf_rp, eduf_sp, edum_rp, edum_sp,employment, housing) %>%
  dplyr::rename(sss_c = sss) %>%
  dplyr::rename(eduy_c = edu,
                income_c = income,
                occupation_code_c = occupation_code,
                industry_code_c = industry_code,
                relation_c = relation,
                employment_c = employment) %>%
  dplyr::left_join(., df.PSID[df.PSID$role == "mother", c("pid_mother", "edu","income","occupation_code","industry_code","sss","employment","relation")], by = "pid_mother")%>%  # combine mother's variable with children
  dplyr::rename(eduy_m = edu,
                income_m = income,
                occupation_code_m = occupation_code,
                industry_code_m = industry_code,
                sss_m = sss,
                relation_m = relation,
                employment_m = employment) %>%  # rename mother's variable
  dplyr::left_join(., df.PSID[df.PSID$role == "father", c("pid_father", "edu","income","occupation_code","industry_code","sss","employment","relation")], by = "pid_father")%>%  # combine father's variable with children
  dplyr::rename(eduy_f = edu,
                income_f = income,
                occupation_code_f = occupation_code,
                industry_code_f = industry_code,
                sss_f = sss,
                relation_f = relation,
                employment_f = employment) %>%  # rename father's variable
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
df.PSID_adult_father <- df.PSID %>%
  dplyr::filter(pid %in% df.PSID_adult$pid_f) %>%
  dplyr::select(pid,relation, occupation_code,industry_code,edu,income,sss, employment) %>%
  dplyr::mutate(pid_f = ifelse(pid %in% df.PSID_adult$pid_f, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(relation_f = relation) %>%
  dplyr::rename(occupation_code_f = occupation_code,
                industry_code_f = industry_code,
                eduy_f = edu,
                income_f = income,
                sss_f = sss,
                employment_f = employment)
df.PSID_adult_mother <- df.PSID %>%
  dplyr::filter(pid %in% df.PSID_adult$pid_m) %>%
  dplyr::select(pid,relation, occupation_code,industry_code,edu,income,sss, employment) %>%
  dplyr::mutate(pid_m = ifelse(pid %in% df.PSID_adult$pid_m, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(relation_m = relation) %>%
  dplyr::rename(occupation_code_m = occupation_code,
                industry_code_m = industry_code,
                eduy_m = edu,
                income_m = income,
                sss_m = sss,
                employment_m = employment)
df.PSID_adult <- df.PSID_adult %>%
  dplyr::select(pid, pid_f, pid_m, fid, age, sex, relation, pid_father, pid_mother, familysize, fincome, edu,income, occupation_code, industry_code, sequence, role, sss, edu_f, edu_m, housing, employment) %>%
  dplyr::rename(eduy = edu)%>%
  dplyr::left_join(., df.PSID_adult_father, by = 'pid_f') %>%
  dplyr::left_join(., df.PSID_adult_mother, by = 'pid_m') %>%
  dplyr::select(pid, pid_m, pid_f,role, age, sex,
                eduy, edu_f, edu_m,eduy_f, eduy_m,
                income, income_f, income_m,
                occupation_code, occupation_code_f, occupation_code_m, industry_code,industry_code_f, industry_code_m, 
                sss, sss_f, sss_m,
                fincome, familysize,relation, relation_f, relation_m, housing, employment,employment_f, employment_m)
# check data
summary(df.PSID_adult)

#save the data as RData for further analysis
save(df.PSID_adult, file = "df.PSID_adult.RData")

# select elderly (age> 60) data for further analysis
df.PSID_elderly <- df.PSID %>%
  dplyr:: filter(age >60)   
df.PSID_elderly_father <- df.PSID %>%
  dplyr::filter(pid %in% df.PSID_elderly$pid_f) %>%
  dplyr::select(pid,relation, occupation_code,industry_code,edu,income,sss,employment) %>%
  dplyr::mutate(pid_f = ifelse(pid %in% df.PSID_elderly$pid_f, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(relation_f = relation) %>%
  dplyr::rename(occupation_code_f = occupation_code,
                industry_code_f = industry_code,
                eduy_f = edu,
                income_f = income,
                sss_f = sss,
                employment_f = employment)
df.PSID_elderly_mother <- df.PSID %>%
  dplyr::filter(pid %in% df.PSID_elderly$pid_m) %>%
  dplyr::select(pid,relation, occupation_code,industry_code,edu,income,sss,employment) %>%
  dplyr::mutate(pid_m = ifelse(pid %in% df.PSID_elderly$pid_m, pid, NA))%>%
  dplyr::select(-pid) %>%
  dplyr::rename(relation_m = relation) %>%
  dplyr::rename(occupation_code_m = occupation_code,
                industry_code_m = industry_code,
                eduy_m = edu,
                income_m = income,
                sss_m = sss,
                employment_m = employment)
df.PSID_elderly <- df.PSID_elderly %>%
  dplyr::select(pid, pid_f, pid_m, fid, age, sex, relation, pid_father, pid_mother, familysize, fincome, edu,income, occupation_code, industry_code, sequence, role, sss, edu_f, edu_m, housing,employment) %>%
  dplyr::rename(eduy = edu)%>%
  dplyr::left_join(., df.PSID_elderly_father, by = 'pid_f') %>%
  dplyr::left_join(., df.PSID_elderly_mother, by = 'pid_m') %>%
  dplyr::select(pid, pid_m, pid_f,role, age, sex,
                eduy, edu_f, edu_m, eduy_f, eduy_m,
                income, income_f, income_m,
                occupation_code, occupation_code_f, occupation_code_m, industry_code,industry_code_f, industry_code_m, 
                sss, sss_f, sss_m,
                fincome, familysize,relation, relation_f, relation_m, housing, employment, employment_f, employment_m)
# check data
summary(df.PSID_elderly)

# save the data as Rdata for further analysis
save(df.PSID_elderly, file = "df.PSID_elderly.Rdata")
# ---------------------------------------------------------------------------------------
# ---------- 3.  Reproduce SES indexes in papers--------------------------------------------
# ---------------------------------------------------------------------------------------
dir.create(tempdir())
###################################################################################################################################
#######################################Preregistration (modified to suit the modified datasets)#####################################
###################################################################################################################################

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
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
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
  dplyr::mutate(edu_cat = dplyr::recode_factor(edu_m, "1" = 1,"2" = 1,"3" = 1,
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
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
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
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5), 
                                   labels = c("1", "2", "3", "4","5"))) %>%  # calculate composite parents' education score
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode))-1) %>% # recode education 0-4
  dplyr::mutate(edu_m_recode = cut(edu_m, 
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

########### Qiu, 2017 (CFPS & PSID)############
# subjects: neonates
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
# subjects: children
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
# subjects: children
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
# subjects: adolescents
# SES = maternal education: dichotomous, divided by college

## CFPS ##
leo_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_leo_cfps = cut(edu_m, breaks = c(0, 4.5, 8.5), labels = c("1", "2"))) %>%  # recode education
  dplyr::mutate(SES_leo_cfps = as.numeric(as.character(SES_leo_cfps))-1)   # convert it to numeric one

# check SES score
summary(leo_CFPS$SES_leo_cfps)

## PSID ##
leo_PSID <- df.PSID_child %>%
  dplyr::mutate(SES_leo_psid = cut(eduy_m, breaks = c(-0.01, 12.5, 17.5, 100), labels = c("1", "2", NA))) %>%  # recode education
  dplyr::mutate(SES_leo_psid = as.numeric(as.character(SES_leo_psid))-1) %>%  # convert it to numeric one
  dplyr::mutate(SES_leo_psid = na_if(SES_leo_psid, 3))    # set NA

# check SES score
table(leo_PSID$SES_leo_psid)

################### Ozernov-Palchik, 2O19 (CFPS & PSID)###########
# subjects: children
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
  dplyr::mutate(edu_f_recode = cut(eduy_f, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%  #recode education (father)
  dplyr::mutate(edu_m_recode = cut(eduy_m, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%  #recode education (mother)
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
  dplyr::mutate(edu_f = na_if(eduy_f, 99),
                edu_m = na_if(eduy_m, 99)) %>%
  dplyr::mutate(SES_knick_psid_edu = (edu_f + edu_m)/2) #education of parents: average of father and mother's education
summary(knick_PSID$SES_knick_psid_inc)
summary(knick_PSID$SES_knick_psid_edu)

####################Brain Development Cooperative Group, 2012#######################
## Subject: children
## SES: income  and education
## Note: two separate SESs


################### Lawson, 2013 (CFPS & PSID)###################
# subjects: children
## SES: cut into bins and use the midpoint of each bin then adjust for family size (based on adjustments used by the US department of Housing and Urban Development (HUD), see https://www.huduser.gov/portal/datasets/il/fmr99/sect82.html
##      bins: $5, 000; $10, 000; $15,000;$25,000;$35, 000; $50, 000; $75, 000; $100, 000; $150, 000;
##      education (in 6 levels, see education & occupation.xlsx): education of parents: sum of father and mother (2-12) and square-root transformed
# Note: two separate SESs
convert_index_cfps_function(paper_year = 2013, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
## CFPS ## 
lawson_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 3.5, 4.5,5.5,6.5,7.5, 8.5), 
                                   labels = c("1", "2", "3", "4", "5", "6")),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 3.5, 4.5,5.5,6.5,7.5, 8.5), 
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_lawson_cfps_edu = sqrt(edu_f_recode+edu_m_recode)) %>%
  # convert income into bins and use the median of the bin as the converted income (CPI US 2010/2013 = 0.93; PPP China CNY/US dollar (2010) = 3.329;)
  dplyr::mutate(income_bin = ifelse(faminc<= 4999*convert_idx, 4999*convert_idx*0.9, ## here use the approximate of original conversion (<5000-->4500)
                                    ifelse(faminc <= 10000*convert_idx, median(c(5000*convert_idx, 10000*convert_idx)),
                                           ifelse(faminc <= 15000*convert_idx, median(c(10000*convert_idx, 15000*convert_idx)),
                                                  ifelse(faminc <= 25000*convert_idx, median(c(15000*convert_idx, 25,000*convert_idx)),
                                                         ifelse(faminc <=35000*convert_idx, median(c(25000*convert_idx, 35000*convert_idx)),
                                                                ifelse(faminc <= 50000*convert_idx, median(c(35000*convert_idx,50000*convert_idx)),
                                                                       ifelse(faminc <= 75000*convert_idx, median(c(50000*convert_idx, 75000*convert_idx)),
                                                                              ifelse(faminc <= 100000*convert_idx, median(c(75000*convert_idx, 100000*convert_idx)),
                                                                                     ifelse(faminc <= 150000*convert_idx, median(c(100000*convert_idx,150000*convert_idx)), 
                                                                                            150000*convert_idx*1.125)))))))))) %>%## here use the approximate of original conversion (>100000-->112500)
  dplyr::mutate(income_bin_adjusted = ifelse(familysize ==1, income_bin*0.7,
                                             ifelse(familysize ==2, income_bin*0.8,
                                                    ifelse(familysize ==3, income_bin*0.9,
                                                           ifelse(familysize ==4, income_bin, income_bin*(1+0.08*(familysize-4))))))) %>%# adjust for family size
  dplyr::rename(SES_lawson_cfps_inc = income_bin_adjusted)

summary(lawson_CFPS$SES_lawson_cfps_edu)
summary(lawson_CFPS$SES_lawson_cfps_inc)

## PSID ##
convert_index_psid_function(paper_year = 2013, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
lawson_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 8.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5", NA)),
                edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01, 8.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("1", "2", "3", "4", "5", NA))) %>% # 6 = NA; did not distinguish "some graduate" and "graduate" in PSID (only 5 levels)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_lawson_psid_edu = sqrt(edu_f_recode+edu_m_recode)) %>%
  dplyr::mutate(income_bin = ifelse(fincome <=5000*convert_idx, 4500*convert_idx,
                                    ifelse(fincome <=10000*convert_idx, median(c(5000*convert_idx, 10000*convert_idx)),
                                           ifelse(fincome<= 15000*convert_idx, median(c(10000*convert_idx, 15000*convert_idx)),
                                                  ifelse(fincome<= 25000*convert_idx,median(c(15000*convert_idx, 25000*convert_idx)),
                                                         ifelse(fincome <=35000*convert_idx,median(c(25000*convert_idx, 35000*convert_idx)),
                                                                ifelse(fincome <=50000*convert_idx, median(c(35000*convert_idx, 50000*convert_idx)),
                                                                       ifelse(fincome <= 75000*convert_idx, median(c(50000*convert_idx, 75000*convert_idx)),
                                                                              ifelse(fincome <= 100000*convert_idx, median(c(75000*convert_idx, 100000*convert_idx)),
                                                                                     ifelse(fincome <= 150000*convert_idx, median(c(100000*convert_idx, 150000*convert_idx)), # CPI 2017/2013= 245.120/232.957 = 1.05
                                                                                            150000*1.125*convert_idx)))))))))) %>%
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
convert_index_cfps_function(paper_year = 2011, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
hanson2_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 2.5, 3.5, 4.5, 5.5, 6.5,7.5, 8.5), 
                                   labels = c("5", "11", "12", "14", "16", "17", "19")),
                edu_m_recode = cut(edu_m, 
                                   breaks = c(-0.01, 2.5, 3.5, 4.5, 5.5, 6.5,7.5, 8.5), 
                                   labels = c("5", "11", "12", "14", "16", "17", "19"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_hanson2_cfps_edu = (edu_m_recode + edu_f_recode)/2) %>%
  dplyr::mutate(fincome_recode = cut (faminc, 
                                      breaks = c(-0.01, 5000*convert_idx, 10000*convert_idx, 15000*convert_idx, 35000*convert_idx, 
                                                 50000*convert_idx, 75000*convert_idx, 100000*convert_idx, 5000000*convert_idx),
                                      labels = c("4500","7500", "12500", "25000", "42500", "62500", "87500", "112500"))) %>% 
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  # convert the midpoint also to the equivalence of the CNY and log transform it
  dplyr::mutate(SES_hanson2_cfps_inc = log10(fincome_recode*convert_idx)) 
summary(hanson2_CFPS$SES_hanson2_cfps_edu)
summary(hanson2_CFPS$SES_hanson2_cfps_inc)


## PSID ##
# # CPI 2017/2011 US dollar = 245.120/224.939 = 1.090
convert_index_psid_function(paper_year = 2011, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
hanson2_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01,6.5, 11.5, 12.5,14.5,16.5, 17.5, 100), 
                                   labels = c("5", "11", "12", "14", "16", "17", NA)),
                edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01,6.5, 11.5, 12.5,14.5,16.5, 17.5, 100), 
                                   labels = c("5", "11", "12", "14", "16", "17", NA))) %>% #  did not distinguish "some graduate" and "graduate" in PSID (only 6 levels)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_hanson2_psid_edu = (edu_m_recode + edu_f_recode)/2) %>%
  dplyr::mutate(fincome_recode = cut (fincome, 
                                      breaks = c(-0.01, 5000*convert_idx, 10000*convert_idx, 15000*convert_idx, 35000*convert_idx, 50000*convert_idx, 
                                                 75000*convert_idx, 100000*convert_idx, 5000000*convert_idx), 
                                      labels = c("4500", "12500", "20000", "30000", "42500", "62500", "87500", "112500"))) %>% 
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_hanson2_psid_inc = log10(fincome_recode*convert_idx)) #convert to mid point also using cpi and use log transformation
summary(hanson2_PSID$SES_hanson2_psid_edu)
summary(hanson2_PSID$SES_hanson2_psid_inc)


########################## Noble, 2015 (Ursache, 2016; Brito, 2017, 2018; Piccolo, 2016, 2019; Merz, 2018) (CFPS & PSID)#############
## Subject: children/young adults (3-20y)
## SES: family income: recode in the means of bins (in 12 levels) and log transformed (same as previous ones)
##      education: (in 7 levels, see education & occupation.xlsx)
## Note: all papers mentioned above use the criteria of Noble, 2015
##       two separate SES
## CFPS ## 
convert_index_cfps_function(paper_year = 2015, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps

noble_CFPS <- df.CFPS_child %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 5000*convert_idx, 9999*convert_idx, 19999*convert_idx, 29999*convert_idx, 
                                                39999*convert_idx, 49999*convert_idx, 99999*convert_idx,149999*convert_idx, 
                                                199999*convert_idx, 249999*convert_idx, 299999*convert_idx, 5000000*convert_idx), 
                                     labels = c("4500", "7500", "15000", "25000", "35000", "45000", "75000",
                                                "125000","175000", "225000", "275000", "325000"))) %>% #Incomes were  divided into 12 levels, CPI US dollar 2010/2015 = 218.056/237.017 = 0.92; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_noble_cfps_inc = log10(fincome_recode*convert_idx)) %>% #log transformation and also convert to the equivalence of CNY
  dplyr::mutate(edu_f_recode = cut(eduy_f, breaks = c(-0.01,  6.5, 9.5, 11.5, 12.5,15.5, 16.5, 22.5), labels = c("6","8", "10.5", "12", "14", "16", "18")),
                edu_m_recode = cut(eduy_m, breaks = c(-0.01,  6.5, 9.5, 11.5, 12.5,15.5, 16.5, 22.5), labels = c("6","8", "10.5", "12", "14", "16", "18"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_noble_cfps_edu = (edu_f_recode + edu_m_recode)/2)
summary(noble_CFPS$SES_noble_cfps_inc)
summary(noble_CFPS$SES_noble_cfps_edu)

## PSID ## 
convert_index_psid_function(paper_year = 2015, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
noble_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_recode = cut(fincome, breaks = c(-0.01, 5000*convert_idx, 9999*convert_idx, 19999*convert_idx, 
                                                         29999*convert_idx, 39999*convert_idx, 49999*convert_idx, 99999*convert_idx,
                                                         149999*convert_idx, 199999*convert_idx, 249999*convert_idx, 299999*convert_idx, 5000000*convert_idx), 
                                     labels = c("4500", "7500", "15000", "25000", "35000", "45000", 
                                                "75000", "125000","175000", "225000", "275000", "325000"))) %>% #Incomes were  divided into 12 levels: CPI 2017/2015 = 245.120/237.017 =1.034
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_noble_psid_inc = log10(fincome_recode*convert_idx)) %>% #log transformation and also transform use cpi
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5,14.5, 16.5, 17.5, 100), 
                                   labels = c("6","8", "10.5", "12", "14", "16", "18", NA)),
                edu_m_recode = cut(eduy_m, 
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
convert_index_cfps_function(paper_year = 2018, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
ell_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc_recode = cut(faminc, 
                                     breaks = c(-0.01, 10000*convert_idx, 25000*convert_idx, 50000*convert_idx, 
                                                75000*convert_idx, 100000*convert_idx, 5000000*convert_idx), 
                                     labels = c("5000", "12500", "37500", "62500", "87500", "112500"))) %>% #Incomes were  divided into 12 levels: CPI: 2010/2018 US dollar = 218.056/251.107 = 0.87; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(faminc_recode = as.numeric(as.character(faminc_recode))) %>%
  dplyr::mutate(SES_ell_cfps_inc = faminc_recode*convert_idx) %>% # convert the mid point to equivalence of CNY
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
convert_index_psid_function(paper_year = 2018, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
ell_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_recode = cut(fincome, 
                                     breaks = c(-0.01, 10000*convert_idx, 25000*convert_idx, 50000*convert_idx, 75000*convert_idx, 100000*convert_idx, 5000000*convert_idx), 
                                     labels = c("5000", "12500", "37500", "62500", "87500", "112500"))) %>% #Incomes were  divided into 12 levels: CPI: 2017/2018 US dollar = 245.120/251.107 = 0.97
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(SES_ell_psid_inc = fincome_recode*convert_idx) %>% # convert the mid point to equivalence of CNY
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 8.5,12.5, 13.5, 14.5,16.5, 17.5, 100), 
                                   labels = c("8","12", "13", "14", "16", "19.5", NA)),
                edu_m_recode = cut(eduy_m, 
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
  dplyr::mutate(edu_f_recode = cut(edu_f, 
                                   breaks = c(-0.01, 3.5,4.5,5.5, 6.5,8.5), 
                                   labels = c("1","2", "3", "4", "5")),
                edu_m_recode = cut(edu_m, 
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
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 8.5,12.5,14.5, 16.5,17.5, 100), 
                                   labels = c("1","2", "3", "4", "5", NA)),
                edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01, 8.5,12.5,14.5, 16.5,17.5, 100), 
                                   labels = c("1","2", "3", "4", "5", NA))) %>% #similar to the previous ones, no distinguish between master and doctorate (only 6 levels, last level set to 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_luby_psid_edu = (edu_f_recode + edu_m_recode)/2)
summary(luby_PSID$SES_luby_psid_inc)
summary(luby_PSID$SES_luby_psid_edu)

#################### Noble, 2012(CFPS & PSID)###############
## Subjects: adolescents/young adults (5-17y)
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
  dplyr::mutate(SES_noble2_psid_edu = (eduy_m + eduy_f)/2)
summary(noble2_PSID$SES_noble2_psid_edu)
summary(noble2_PSID$SES_noble2_psid_inc)

############### Takeuchi, 2019(CFPS & PSID)###############
## Subjects: young adults (mean = 20.8)
## SES: education: eight categories and then converted to year (first divide education year into categories and then convert to years)
##      income: in bins 1-7 1, <2 million yen; 2, 2–4 million yen; 3, 4–6 million yen; 4, 6–8 million yen; 5, 8–10 million yen; 6, 10–12 million yen; 7,  ≥12 million yen. 
## Note: two separate SESs

## CFPS ##
# CPI: Japan yen 2010/2019 = 100/105.482 = 0.94; PPP CNY/Japan yen 2010 = 3.329/111.667 = 0.03
convert_index_cfps_function(paper_year = 2019, paper_country = "JPN")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
takeu_CFPS <- df.CFPS_child %>%
  dplyr::mutate(income_cat = cut(faminc, 
                                 breaks = c(-0.01, 2000000*convert_idx, 4000000*convert_idx, 6000000*convert_idx, 8000000*convert_idx,
                                            10000000*convert_idx, 12000000*convert_idx, 5000000), 
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
convert_index_psid_function(paper_year = 2019, paper_country = "JPN")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
takeu_PSID <- df.PSID_child %>%
  dplyr::mutate(income_cat = cut(fincome, breaks = c(-0.01, 2000000*convert_idx, 4000000*convert_idx, 6000000*convert_idx, 8000000*convert_idx, 10000000*convert_idx, 12000000*convert_idx, 5000000), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>% #CPI: Japan yen 2017/2019 = 103.963/105.482 = 0.99; PPP US dollar/Japan yen 2010 = 1/105.379 = 0.0095
  dplyr::mutate(SES_takeu_psid_inc = as.numeric(as.character(income_cat))) %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01, 6.5,8.5,11.5,12.5, 14.5, 16.5, 17.5,99), 
                                   labels = c("6","9", "11", "12", "14", "16", "19.5", NA)),
                edu_m_recode = cut(eduy_m, 
                                   breaks = c(-0.01, 6.5,8.5,11.5,12.5, 14.5, 16.5, 17.5,99), 
                                   labels = c("6","9", "11", "12", "14", "16", "19.5", NA))) %>% # not distinguish "master" and "doctorate", only 7 levels (highest level: 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_takeu_psid_edu = (edu_f_recode + edu_m_recode)/2)
summary(takeu_PSID$SES_takeu_psid_edu)
summary(takeu_PSID$SES_takeu_psid_inc)

########################Kong, 2015 (CFPS & PSID) ##########################
## Subjects: Young adults (parent's SES) (mean= 21.57)
## SES: Parents' education (separate): categorical education convert to years (0-18)
##      Family SSS: ladder scale (use other SSS variables in CFPS and PSID as alternatives)
##                  CFPS: composite mother and father's subjective social status as family SSS
##                  PSID: composite reference person and spouse subjective subjective social status
## CFPS ##
kong_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f,
                                   breaks = c(-0.01, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5),
                                   labels = c("0", "6", "9", "12", "15", "16", "17", "18")),
                edu_m_recode = cut(edu_m,
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
  dplyr::mutate(edu_f_recode = cut(eduy_f, 
                                   breaks = c(-0.01,0.5, 6.5, 8.5, 12.5, 16.5, 17.5, 99),
                                   labels = c("0", "6","8", "12", "16", "18", NA)),
                edu_m_recode = cut(eduy_m, 
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
## Subject: children/young adults (4-20y)
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
  dplyr::mutate(edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.01, 8.5, 12.5, 14.5, 16.5, 17.5, 99),
                                   labels = c("1", "2", "3", "4", "5", NA))) %>% #No distinction between some graduate and graduage (set both level as 5)
  dplyr::mutate(SES_hair_psid_income_cat = as.numeric(as.character(income_cat)),
                SES_hair_psid_income_bin = as.numeric(as.character(income_bin)),
                SES_hair_psid_medu = as.numeric(as.character(edu_m_recode)))
summary(hair_PSID$SES_hair_psid_income_cat)
summary(hair_PSID$SES_hair_psid_income_bin)
summary(hair_PSID$SES_hair_psid_medu)

################## Yu,2018 (only CFPS) ########
# Subject: children & young adult (10-25y)
# SES: SSS (MAS): self sss
#      ITN: income data were collected in 9 categories: < $5000; $5000 ~ 11, 999; $ 12, 000 ~ 15, 999 ; $ 16, 000 ~ 24, 999; $ 25,000 ~ 34, 999; $ 35, 000 ~ 49, 999; 
#                                                       $ 50, 000 ~ 74, 999; $ 75, 000 ~ 99, 999; $ > =100, 000 
#      Education father and mother: 7 categories
#      composite: weighted factor composite PCA score for the four variables

# laod related packages
library(FactoMineR)
library(factoextra)
convert_index_cfps_function(paper_year = 2018, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
yu_CFPS <- df.CFPS_child %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 5000*convert_idx, 11999* convert_idx, 15999* convert_idx, 24999* convert_idx, 
                                                34999* convert_idx, 49999* convert_idx, 74999* convert_idx,99999* convert_idx, 5000000*convert_idx), 
                                     labels = c("4500", "8500", "14000", "20500", "30000", "42500", 
                                                "62500","875000", "125000"))) %>% #Incomes were  divided into 9 levels, CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(fincome_recode = fincome_recode * convert_idx) %>%
  dplyr::mutate(ITN = fincome_recode/(1274*familysize)) %>%
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.01, 1.5, 3.5, 4.5, 5.5,6.5,7.5,8.5),
                                   labels = c("1", "2", "3", "4", "5", "6", "7")),
                edu_f_recode = cut(edu_f,
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

## PSID ##

convert_index_psid_function(paper_year = 2018, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
yu_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome_recode = cut(fincome, 
                                     breaks = c(-0.01, 5000*convert_idx, 11999* convert_idx, 15999* convert_idx, 24999* convert_idx, 
                                                34999* convert_idx, 49999* convert_idx, 74999* convert_idx,99999* convert_idx, 5000000*convert_idx), 
                                     labels = c("4500", "8500", "14000", "20500", "30000", "42500", 
                                                "62500","875000", "125000"))) %>% #Incomes were  divided into 9 levels, CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))) %>%
  dplyr::mutate(fincome_recode = fincome_recode * convert_idx) %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(ITN = fincome_recode/poverty) %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.01, 0.5, 8.5, 12.5, 14.5,16.5,17.5),
                                   labels = c("1", "2", "3", "4", "5", "6")),
                edu_f_recode = cut(eduy_f,
                                   breaks = c(-0.01, 0.5, 8.5, 12.5, 14.5,16.5,17.5),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)))%>%
  dplyr::select(ITN, edu_m_recode, edu_f_recode, sss_c) %>%
  tidyr::drop_na(.) 
# calculate PCA
yu_pca <- PCA(yu_PSID, scale.unit = TRUE, graph = TRUE)
get_eigenvalue(yu_pca)
fviz_eig(yu_pca)#scree plot
var <- get_pca_var(yu_pca)
# calculate composite SES
yu_PSID <- yu_PSID %>%
  dplyr::mutate(SES_yu_psid = var$coord[1]*ITN+ var$coord[2]*edu_m_recode +var$coord[3]*edu_f_recode + var$coord[4]*sss_c)
summary(yu_CFPS$SES_yu_cfps)

############### Yang, 2016 ##############
# Subject: young adults
# SES: family annual income < RMB 5,000; RMB 5,000–15000; RMB 15,001–30,000; RMB 30,001–50,000; RMB 50,001–100, 000; > RMB 100,000. (in six categories and recoded as 1-6)
#      education: collected in 6 categories and recoded as years (according to chinese and us educational systems respectively)
# composite: average of z-scores of two variables
## CFPS ##
convert_index_cfps_function(paper_year = 2016, paper_country = "CHN")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
yang_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc, 
                                     breaks = c(-0.01, 4999*convert_idx, 15000*convert_idx, 30000*convert_idx, 50000*convert_idx, 100000*convert_idx, 5000000),
                                     labels = c("1", "2", "3", "4", "5", "6"))) %>% # CPI 2016 = 627.5, CPI 2010 = 536.1, CPI 2016/CPI 2010 = 1.1705
  dplyr::mutate(edu_recode = cut(edu,
                                 breaks = c(-0.01, 2.5, 3.5, 4.5, 6.5, 8.5),
                                 labels = c("6", "9", "12", "16", "19"))) %>%
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(fincome_zscore = (fincome_recode - mean(fincome_recode, na.rm = TRUE))/sd(fincome_recode, na.rm = TRUE)) %>%  # calculate z-score of family income and education
  dplyr::mutate(edu_zscore = (edu_recode - mean(edu_recode, na.rm = TRUE))/sd(edu_recode, na.rm = TRUE)) %>%
  dplyr::mutate(SES_yang_cfps = (fincome_zscore + edu_zscore)/2) 
summary(yang_CFPS$SES_yang_cfps)

## PSID ##
convert_index_psid_function(paper_year = 2016, paper_country = "CHN")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
yang_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome, 
                                     breaks = c(-0.01, 4999*convert_idx, 15000*convert_idx, 30000*convert_idx, 50000*convert_idx, 100000*convert_idx, 500000000),
                                     labels = c("1", "2", "3", "4", "5", "6"))) %>% # CPI 2016 = 627.5, CPI 2010 = 536.1, CPI 2016/CPI 2010 = 1.1705, PPP 2010 China/US = 3.329
  dplyr::mutate(edu_recode = cut(eduy,
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
  dplyr::mutate(occup_recode = recode(egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                      "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation 
  dplyr::mutate(edu_recode = cut(eduy, 
                                 breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -8))) %>%  # set NA
  dplyr::mutate(SES_johnson_cfps = 7*occup_recode +4*edu_recode)
summary(johnson_CFPS$SES_johnson_cfps)

#################Johnson, 2013b (only CFPS) #######################
# Subject: elderly
# SES: same as Johnson, 2013
johnson2_CFPS <- df.CFPS_elderly %>%
  dplyr::mutate(occup_recode = recode(egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                      "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation 
  dplyr::mutate(edu_recode = cut(eduy, 
                                 breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -8))) %>%  # set NA
  dplyr::mutate(SES_johnson2_cfps = 7*occup_recode +4*edu_recode)
summary(johnson2_CFPS$SES_johnson2_cfps)

###############Waldstein, 2017################
# Subject: middle aged
# SES: dichotomous low SES:  below median education (<12 years) and/or income below 125% of the federal poverty line (relative to family size)
## CFPS ##
waldstein_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = ifelse(eduy < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(faminc < 1.25*1274, 0, 1)) %>%
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
  dplyr::mutate(edu_recode = ifelse(eduy < median(eduy), 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*poverty, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_shake_psid = ifelse(SES_low == 0, 0, 1))
table(shake_PSID$SES_shake_psid)

###########Noble, 2012b############
# Subject: young to old adults (above 17; use adult & ederly dataset)
# SES: education
#     1. dichotomous <14/>=14yrs
#     2. categorical: high school or less (11); Some college; colege and up (16)
## CFPS ##
nobel3_CFPS_data <- rbind(df.CFPS_adult, df.CFPS_elderly)
noble3_CFPS <- nobel3_CFPS_data %>%
  dplyr::mutate(SES_noble3_edu1_cfps = ifelse(eduy <14, 0, 1),
                SES_noble3_edu2_cfps = cut(eduy,
                                           breaks = c(-0.01, 11.5, 15.5, 22.5),
                                           labels = c("high school", "some college", "colege")))
table(noble3_CFPS$SES_noble3_edu1_cfps)
table(noble3_CFPS$SES_noble3_edu2_cfps)
## PSID ##
nobel3_PSID_data <- rbind(df.PSID_adult, df.PSID_elderly)

noble3_PSID <- nobel3_PSID_data %>%
  dplyr::mutate(SES_noble3_edu1_psid = ifelse(eduy <14, 0, 1),
                SES_noble3_edu2_psid = cut(eduy,
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
## CFPS ##.
convert_index_cfps_function(paper_year = 2013, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
cavanagh_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(income_dich = ifelse(income < 25000*convert_idx, 0, 1), # convert income into dichotomous variable (cut point $25,000 in 2013): CPI US dollar 2010/2013 = 218.056/232.957 = 0.936;PPP: CNY/US dollar in 2010 = 3.329
                social_dich = ifelse(sss <= 3, 0, 1), # convert social class into dichotomous variable
                house_dich = ifelse(housing <=2 | housing == 6, 1, 
                                    ifelse(housing == 77, NA, 0))) %>% # convert housing into dichotomous variable: ownership of the house: in CFPS in 7 categories: 1 = "Property right solely owned by the family" 2 = "Joint property right with work unit(danwei)" 3 = "Rented"  4 = "Provided by the government for free" 5 = "Provided by work unit(danwei) for free"6 = "Provided by parents/children" 7 = "Borrowed from friends or relatives" 77 = "Other [Please specify]” —>  count 1,2,6 as owned by family and set 77 as NA
  dplyr::mutate(SES_cavanagh_cfps = income_dich + social_dich + house_dich)
table(cavanagh_CFPS$SES_cavanagh_cfps)

## PSID ## 
convert_index_psid_function(paper_year = 2013, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
cavanagh_PSID <- df.PSID_adult %>%
  dplyr::mutate(income_dich = ifelse(income < 25000*convert_idx, 0, 1)) %>% # convert income: CPI US 2017/2013 = 245.120/232.957
  dplyr::mutate(social_dich = ifelse(sss <= 3, 0, 1)) %>% # convert social class
  dplyr::mutate(housing = ifelse(housing >=9999998, NA, housing)) %>%
  dplyr::mutate(house_dich = ifelse(housing ==0, 0, 1)) %>% # convert housing: ER66031 = 0, rent or neither rent or own; ER66031 = 1-9999997, actual value of the owned house; 9999998, DK; 9999999, refuse to answer 
  dplyr::mutate(SES_cavanagh_psid = income_dich + social_dich + house_dich)
summary(cavanagh_PSID$SES_cavanagh_psid)  

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
  dplyr::mutate(SES_piras_cfps = cut(edu, 
                                     breaks = c(-0.01, 2.5, 3.5, 4.5, 6.5, 8.5),
                                     labels = c("6", "9", "12", "16", "18"))) %>%
  dplyr::mutate(SES_piras_cfps = as.numeric(as.character(SES_piras_cfps)))
summary(piras_CFPS$SES_piras_cfps)

## PSID ##
piras_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES_piras_psid = cut(eduy, 
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
convert_index_cfps_function(paper_year = 2018, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
zhu_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 10000*convert_idx, 19999*convert_idx, 29999*convert_idx, 39999*convert_idx, 49999*convert_idx, 74999*convert_idx, 99999*convert_idx, 10000000),
                                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>% # CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(edu_recode = cut(eduy,
                                 breaks = c(-0.01, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 22.5),
                                 labels = c("11", "12", "13", "14", "15", "16", "17"))) %>%
  dplyr::mutate(employment_recode = ifelse(employment == 1, 2, 0)) %>%
  dplyr::mutate(SES_zhu_income_cfps = as.numeric(as.character(fincome_recode)),
                SES_zhu_edu_cfps = as.numeric(as.character(edu_recode)),
                SES_zhu_employment_cfps = employment_recode)
table(zhu_CFPS$SES_zhu_income_cfps)
table(zhu_CFPS$SES_zhu_edu_cfps)
table(zhu_CFPS$SES_zhu_employment_cfps)

## PSID ##
convert_index_psid_function(paper_year = 2018, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
zhu_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 10000*convert_idx, 19999*convert_idx, 29999*convert_idx, 39999*convert_idx, 49999*convert_idx, 74999*convert_idx, 99999*convert_idx, 100000000000),
                                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>% # CPI US dollar 2017/2018 = 245.120/251.107 = 0.976 
  dplyr::mutate(edu_recode = cut(eduy,
                                 breaks = c(-0.01, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 22.5),
                                 labels = c("11", "12", "13", "14", "15", "16", "17"))) %>%
  dplyr::mutate(employment_recode = ifelse(employment == 1, 2, 
                                           ifelse(employment == 2, 1,
                                                  ifelse(employment==9 | employment == 0, NA, 0)))) %>% # 1= working; 2= Only temporarily laid off; 3-8 = not employed; 9 = DK/NA; 0 = not applicable 
  dplyr::mutate(SES_zhu_income_psid = as.numeric(as.character(fincome_recode)),
                SES_zhu_edu_psid = as.numeric(as.character(edu_recode)),
                SES_zhu_employment_psid = employment_recode)
table(zhu_PSID$SES_zhu_income_psid)
table(zhu_PSID$SES_zhu_edu_psid)
table(zhu_PSID$SES_zhu_employment_psid)






###################################################################################################
#####################Formal reproduction (starting from 2023)######################################
###################################################################################################
#####################Avinun, 2019 (CFPS) ##########################
# subjects: young adults (younger than 22 years old --> children)
# SES: subjective SES (ladder on a scale of 0-10)

## CFPS ##
avinun_CFPS <-df.CFPS_child %>%
  dplyr::mutate(SES_avinun_cfps = sss_c)
## PSID $$
avinun_PSID <- df.PSID_child %>%
  dplyr::mutate(SES_avinun_psid = sss_c)
summary(avinun_CFPS$SES_avinun_cfps)

summary(avinun_PSID$SES_avinun_psid)
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
  dplyr::mutate(edu_recode = ifelse(eduy < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(poverty_line = 1274*familysize) %>%
  dplyr::mutate(income_recode = ifelse(faminc < 1.25*poverty_line, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES_beaty_cfps = ifelse(SES_low == 0, 0, 1))
table(beaty_CFPS$SES_beaty_cfps)
## PSID ##
beaty_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = ifelse(eduy < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(income_recode = ifelse(fincome < 1.25*poverty_line, 0, 1)) %>%
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
  dplyr::mutate(edu_m_recode_low = ifelse(edu_m <= 4, 1, 0))%>%
  dplyr::mutate(SES_betan2_cfps = ifelse(itn >1 & edu_m_recode_low ==0,1,0))   # calculate composite SES score
## PSID ##
betan2_PSID <- df.PSID_child %>%
  dplyr::mutate(itn = (12060 +  (familysize-1)*4180)/fincome) %>%
  # set 2 levels for mother's education
  dplyr::mutate(edu_m_recode_low = ifelse(eduy_m <= 12, 1, 0))%>%
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
## CFPS ##
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
## psid ##
cascio_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.5, 8.5, 12.5, 13.5, 14.5, 16.5,99),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f,
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
  dplyr::mutate(edu_m_recode = cut(eduy_m,
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
convert_index_cfps_function(paper_year = 2020, paper_country = "CAN")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
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
convert_index_psid_function(paper_year = 2020, paper_country = "CAN")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
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
convert_index_cfps_function(paper_year = 2021, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
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
convert_index_psid_function(paper_year = 2021, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
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
  dplyr::mutate(SES_dejoseph_psid_edu = ifelse(is.na(eduy_f), eduy_m,
                                               ifelse(is.na(eduy_m), eduy_f, pmax(eduy_f, eduy_m))))
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
convert_index_cfps_function(paper_year = 2020, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
dougherty_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = cut(eduy, 
                                   breaks = c(-0.5, 12.5, 16.5,22.5), 
                                   labels = c("1", "2", "3"))) %>%        # recode education
  dplyr::mutate(fincome_bin = ifelse(faminc < 49999*convert_idx, 1,
                                     ifelse(faminc < 99999*convert_idx, 2,3)))%>%
  dplyr::mutate(edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_dougherty_cfps = fincome_bin+ edu_recode)
table(dougherty_CFPS$SES_dougherty_cfps)

## PSID ##
convert_index_psid_function(paper_year = 2020, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
dougherty_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = cut(eduy, 
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
  dplyr::mutate(edu_recode = cut(eduy, 
                                 breaks = c(-0.5, 6.5, 9.5,12.5,22.5), 
                                 labels = c("0","1", "2", "3"))) %>%        # recode education
  dplyr::mutate(feduc_recode = cut(edu_f, 
                            breaks = c(-0.5,1.5, 2.5, 4.5,8.5), 
                            labels = c("0","1", "2", "3"))) %>%
  dplyr::mutate(meduc_recode = cut(edu_m, 
                                   breaks = c(-0.5,1.5, 2.5, 4.5,8.5), 
                                   labels = c("0","1", "2", "3"))) %>%
  dplyr::mutate(edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  
  dplyr::mutate(ses_childhood_low = ifelse(meduc_recode + feduc_recode <2,1,0)) %>%
  dplyr::mutate(ses_earlyadulthood_low = ifelse(edu_recode <1, 1,0)) %>%
  dplyr::mutate(skill_level = round(isco/1000)) %>%
  dplyr::mutate(ses_midadulthood_low = ifelse(skill_level>3,1, 0)) %>%
  dplyr::mutate(SES_elbejjani_cfps = ses_midadulthood_low + ses_earlyadulthood_low + ses_childhood_low)
table(elbejjani_CFPS$SES_elbejjani_cfps)
########################Ellwood-Lowe, 2020################
# subjects: children
# SES: poverty:  part of a family of 4 with a total income of less than $25,000, or a family of 5 or more with a total income of less than $35,000. 
# - 0 = not poverty; 1 = poverty
## CFPS ##
convert_index_cfps_function(paper_year = 2020, paper_country = "USA")  
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps

Ellwood_CFPS <- df.CFPS_child %>%
  dplyr::filter(familysize ==4 | familysize ==5) %>%
  dplyr::mutate(SES_ellwood_cfps = ifelse(familysize ==4 & faminc < 25000*convert_idx,1,
                                 ifelse(familysize ==5 & faminc <35000*convert_idx,1,0)))
table(Ellwood_CFPS$SES_ellwood_cfps)
## PSID ##
convert_index_psid_function(paper_year = 2020, paper_country = "USA")  
convert_idx = index_ppp_first_psid #index_cpi_first_psid
Ellwood_PSID <- df.PSID_child %>%
  dplyr::filter(familysize ==4 | familysize ==5) %>%
  dplyr::mutate(SES_ellwood_psid = ifelse(familysize ==4 & fincome < 25000*convert_idx,1,
                                          ifelse(familysize ==5 & fincome <35000*convert_idx,1,0)))
table(Ellwood_PSID$SES_ellwood_psid)
########################Gianaros, 2011########################
# subjects: adults
# SES: parental education
# - first coded as no high school diploma; high school diploma or some technical training; some college without a degree; associate’s degree; bachelor’s degree; master’s degree; or doctoral degree
# - and then divided into low (those from households in which neither parent attained a postsecondary or higher college degree (i.e., no high school diploma, high school diploma, or some technical training)) and high education group (those from households in which at least one parent attained a postsecondary or higher degree (i.e., at least an associate’s or higher degree)
## CFPS ##
gianaros_CFPS2011 <- df.CFPS_adult %>%
  dplyr::mutate(feduc_recode = cut(edu_f, 
                                   breaks = c(-0.5,4.5,8.5), 
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = cut(edu_m, 
                                   breaks = c(-0.5,4.5, 8.5), 
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  # recode the education level as SES (low = 0, high = 1, other = NA)
  dplyr::mutate(SES_gianaros_cfps = ifelse(meduc_recode==0 & feduc_recode ==0,0,
                                           ifelse(meduc_recode ==1|feduc_recode ==1,1, NA)))
table(gianaros_CFPS2011$SES_gianaros_cfps)
## PSID ##
gianaros_PSID2011 <- df.PSID_adult %>%
  dplyr::mutate(feduc_recode = cut(eduy_f, 
                                   breaks = c(-0.5,12.5,17.5), 
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = cut(eduy_m, 
                                   breaks = c(-0.5,12.5, 17.5), 
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  # recode the education level as SES (low = 0, high = 1, other = NA)
  dplyr::mutate(SES_gianaros_cfps = ifelse(meduc_recode==0 & feduc_recode ==0,0,
                                           ifelse(meduc_recode ==1|feduc_recode ==1,1, NA)))
table(gianaros_PSID2011$meduc_recode)
##########################Gullick, 2016##########################
#subjects: children
#SES: parental education
# - lower SES was defined as 10–14 years of education for both parents, and higher SES as 16–18 years 
## CFPS ##
gullick_CFPS <- df.CFPS_child %>%
  dplyr::mutate(feduc_recode = cut(eduy_f,
                                   breaks = c(-0.5, 9.5, 14.5, 15.5, 18.5, 22.5),
                                   labels = c(NA, "0", NA, "1", NA))) %>%
  dplyr::mutate(meduc_recode = cut(eduy_m,
                                   breaks = c(-0.5, 9.5, 14.5, 15.5, 18.5, 22.5),
                                   labels = c(NA, "0", NA, "1", NA))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  # recode the SES as higher education (1) and lower education (0)
  dplyr::mutate(SES_gullick_cfps = ifelse(meduc_recode + feduc_recode == 0, 0,
                                          ifelse(meduc_recode + feduc_recode ==2,1, NA)))
table(gullick_CFPS$SES_gullick_cfps)

## PSID ##
gullick_PSID <- df.PSID_child %>%
  dplyr::mutate(feduc_recode = cut(eduy_f,
                                   breaks = c(-0.5, 9.5, 14.5, 15.5, 18.5, 22.5),
                                   labels = c(NA, "0", NA, "1", NA))) %>%
  dplyr::mutate(meduc_recode = cut(eduy_m,
                                   breaks = c(-0.5, 9.5, 14.5, 15.5, 18.5, 22.5),
                                   labels = c(NA, "0", NA, "1", NA))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  # recode the SES as higher education (1) and lower education (0)
  dplyr::mutate(SES_gullick_psid = ifelse(meduc_recode + feduc_recode == 0, 0,
                                          ifelse(meduc_recode + feduc_recode ==2,1, NA)))
table(gullick_PSID$SES_gullick_psid)
########################Hackman, 2018########################
# Subjects: adults
# SES: childhood SEP and adulthood SEP
# - Childhood: parental education: highest level of education achieved by either parent
# - adulthood: educational attainment
# - coded as 1 = high school graduate or less, including high school equivalency, 2 = 1 to 3 years of college, 3 = 4- year college graduate, and 4 = graduate degree completion. 
# - composite:  low SEP was defined as below a 4-year college degree, whereas high SEP was defined as achievement of a college degree or higher. 
#               - recode into (categorical) (1) stable low SEP, with low parental education and educational attainment; (2) upward socioeconomic trajectory, indicated by low parental education and high educational achievement; (3) downward socioeconomic trajectory, with high parental education followed by low educational achievement; and (4) stable high SEP, with high parental education and high educational achievement.
## CFPS ##
hackman_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(feduc_recode = cut(edu_f, 
                                   breaks = c(-0.5,4.5,5.5, 6.5,8.5), 
                                   labels = c("1","2","3","4"))) %>%
  dplyr::mutate(meduc_recode = cut(edu_m, 
                                   breaks = c(-0.5,4.5,5.5, 6.5,8.5), 
                                   labels = c("1","2","3","4"))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  dplyr::mutate(childhood_SEP = ifelse(meduc_recode> feduc_recode, meduc_recode, feduc_recode))%>%
  dplyr::mutate(adulthood_SEP = cut(edu, 
                                    breaks = c(-0.5,4.5,5.5, 6.5,8.5), 
                                    labels = c("1","2","3","4"))) %>%
  dplyr::mutate(adulthood_SEP = as.numeric(as.character(adulthood_SEP))) %>%
  dplyr::mutate(childhood_SEP = ifelse(childhood_SEP <2.5, "low", "high")) %>%
  dplyr::mutate(adulthood_SEP = ifelse(adulthood_SEP <2.5, "low", "high")) %>%
  dplyr::mutate(SES_hackman_cfps = ifelse(childhood_SEP == "low" & adulthood_SEP =="low","stable low SEP",
                                          ifelse(childhood_SEP == "low" & adulthood_SEP =="high", "upward SES trajectory",
                                                 ifelse(childhood_SEP == "high" & adulthood_SEP =="low", "downward SES trajectory", 
                                                        ifelse(childhood_SEP == "high" & adulthood_SEP =="high", "stable high SEP", NA)))))
table(hackman_CFPS$SES_hackman_cfps)
## PSID ##
hackman_PSID <- df.PSID_adult %>%
  dplyr::mutate(feduc_recode = cut(eduy_f, 
                                   breaks = c(-0.5,12.5,15.5, 16.5,17.5), 
                                   labels = c("1","2","3","4"))) %>%
  dplyr::mutate(meduc_recode = cut(eduy_m, 
                                   breaks = c(-0.5,12.5,15.5, 16.5,17.5), 
                                   labels = c("1","2","3","4"))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  dplyr::mutate(childhood_SEP = ifelse(meduc_recode> feduc_recode, meduc_recode, feduc_recode))%>%
  dplyr::mutate(adulthood_SEP = cut(eduy, 
                                    breaks = c(-0.5,12.5,15.5, 16.5,17.5), 
                                    labels = c("1","2","3","4"))) %>%
  dplyr::mutate(adulthood_SEP = as.numeric(as.character(adulthood_SEP))) %>%
  dplyr::mutate(childhood_SEP = ifelse(childhood_SEP <2.5, "low", "high")) %>%
  dplyr::mutate(adulthood_SEP = ifelse(adulthood_SEP <2.5, "low", "high")) %>%
  dplyr::mutate(SES_hackman_psid = ifelse(childhood_SEP == "low" & adulthood_SEP =="low","stable low SEP",
                                          ifelse(childhood_SEP == "low" & adulthood_SEP =="high", "upward SES trajectory",
                                                 ifelse(childhood_SEP == "high" & adulthood_SEP =="low", "downward SES trajectory", 
                                                        ifelse(childhood_SEP == "high" & adulthood_SEP =="high", "stable high SEP", NA)))))
table(hackman_PSID$SES_hackman_psid)
########################Gianaros, 2013##########################
#subjects: adults
#SES: - individual level
#       1. education: year of schooling
#       2. household income: on a 15-point scale
#          less than $10 000/year, to more than $185 000/year (bins 15000)
#         - bins: <$10K; $10–14999K; $15–24999K; $25-34 999K; $35–49 999K; 50–64999K; 65–79999K; 80–94 999K; 95–109999K; 110–124999K; 25–139999K; 140–154999K; 155–169999K; 170–185K; and >185K
#         - took middle point of the bins (highest: 25% above $185 000) and adjust by familysize (square root of number of occupants)
#         - standardized by cube root
#     - community level: cannot be reproduced
## CFPS ##
convert_index_cfps_function(paper_year = 2013, paper_country = "USA")
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps

gianaros_CFPS2013 <- df.CFPS_adult %>%
  dplyr::mutate(SES_ginaros_edu_cfps = eduy) %>%
  dplyr::mutate(fincome_bin = cut(faminc,
                                 breaks = c(-0.5, 10000*convert_idx, 15000*convert_idx, 25000*convert_idx, 35000*convert_idx, 50000*convert_idx, 65000*convert_idx, 80000*convert_idx, 95000*convert_idx,110000*convert_idx, 125000*convert_idx, 140000*convert_idx, 155000*convert_idx, 170000*convert_idx, 185000*convert_idx, 100000000000*convert_idx),
                                 labels = c("1", "2", "3", "4", "5","6","7","8", "9", "10", "11", "12","13","14", "15"))) %>%
  dplyr::mutate(fincome_bin_median = recode(fincome_bin,'1'='5000', '2' = '12500', '3' = '20000', '4' = '30000', '5' = '42500', '6' = '57500', '7' = '72500', '8' = '87500', '9' = '102500','10' = '117500', '11' = '132500', '12' = '147500', '13' = '162500', '14' = '177500', '15' = '231250')) %>%
  dplyr::mutate(fincome_bin_median = as.numeric(as.character(fincome_bin_median))) %>%
  dplyr::mutate(fincome_bin_median = fincome_bin_median * convert_idx) %>%
  dplyr::mutate(fincome_adjusted = pracma::nthroot(fincome_bin_median/sqrt(familysize),3)) %>%
  dplyr::mutate(SES_ginaros_inc_cfps = fincome_adjusted)
table(gianaros_CFPS2013$SES_ginaros_edu_cfps)
table(gianaros_CFPS2013$SES_ginaros_inc_cfps)
## PSID ##
convert_index_psid_function(paper_year = 2013, paper_country = "USA")
convert_idx = index_ppp_first_psid #index_cpi_first_psid

gianaros_PSID2013 <- df.PSID_adult %>%
  dplyr::mutate(SES_ginaros_edu_psid = eduy) %>%
  dplyr::mutate(fincome_bin = cut(fincome,
                                  breaks = c(-0.5, 10000*convert_idx, 15000*convert_idx, 25000*convert_idx, 35000*convert_idx, 50000*convert_idx, 65000*convert_idx, 80000*convert_idx, 95000*convert_idx,110000*convert_idx, 125000*convert_idx, 140000*convert_idx, 155000*convert_idx, 170000*convert_idx, 185000*convert_idx, 100000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5","6","7","8", "9", "10", "11", "12","13","14", "15"))) %>%
  dplyr::mutate(fincome_bin_median = recode(fincome_bin,'1'='5000', '2' = '12500', '3' = '20000', '4' = '30000', '5' = '42500', '6' = '57500', '7' = '72500', '8' = '87500', '9' = '102500','10' = '117500', '11' = '132500', '12' = '147500', '13' = '162500', '14' = '177500', '15' = '231250')) %>%
  dplyr::mutate(fincome_bin_median = as.numeric(as.character(fincome_bin_median))) %>%
  dplyr::mutate(fincome_bin_median = fincome_bin_median * convert_idx) %>%
  dplyr::mutate(fincome_adjusted = pracma::nthroot(fincome_bin_median/sqrt(familysize),3)) %>%
  dplyr::mutate(SES_ginaros_inc_psid = fincome_adjusted)
table(gianaros_PSID2013$SES_ginaros_edu_psid)
table(gianaros_PSID2013$SES_ginaros_inc_psid)
####################################################################
####################################################################
############start to use the new data sets##########################
####################################################################
####################################################################
#####################Hanson, 2011###################################
# subjects: children/adolescents
# SES:
# 1. Household income:  
# - Incomes were then divided into 9 levels: $1–5000,$5001–10000, $10001–15000, $15001–25000, $25001–35000,$35001–50000, $50001–75000, $75001–100000,and $100001+ 
# - log-transformed, mid-point of each category
# 2. father and mother's education (separate)
# - less than a 6th grade education, less than high school, graduated high school, completed some college, graduated college, obtain some graduate education, or completed graduate school.
# - ( <6th grade=5 years, less than high school =11 years, high school 12 years, some college =14 years, college = 16 years, some grad = 17 years and graduate = 19 years

## CFPS ##
convert_index_cfps_function(paper_year = 2006, paper_country = "USA") # dataset published year: 2006
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
hanson_CFPS2011 <- df.CFPS_child %>%
  dplyr::mutate(fincome_bin = cut(faminc,
                                  breaks = c(0.5*convert_idx, 5000*convert_idx, 10000*convert_idx, 15000*convert_idx, 25000*convert_idx, 35000*convert_idx, 50000*convert_idx, 75000*convert_idx, 100000*convert_idx, 100000000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5","6","7","8", "9"))) %>%
  dplyr::mutate(fincome_bin_median = recode(fincome_bin,'1'='2500', '2' = '7500', '3' = '12500', '4' = '20000', '5' = '30000', '6' = '42500', '7' = '57500', '8' = '87500', '9' = '112500')) %>%
  dplyr::mutate(fincome_bin_median = as.numeric(as.character(fincome_bin_median))) %>%
  dplyr::mutate(fincome_bin_median = fincome_bin_median * convert_idx) %>%
  dplyr::mutate(SES_hanson_inc_cfps = log10(fincome_bin_median)) %>%
  dplyr::mutate(fedu_recode = cut(eduy_f, 
                                  breaks = c(-0.5, 5.5, 11.5, 12.5, 14.5, 16.5, 17.5,23),
                                  labels = c("5", "11","12", "14","16", "17", "19"))) %>%
  dplyr::mutate(SES_hanson_fedu_cfps = as.numeric(as.character(fedu_recode))) %>%
  dplyr::mutate(medu_recode = cut(eduy_m, 
                                  breaks = c(-0.5, 5.5, 11.5, 12.5, 14.5, 16.5, 17.5,23),
                                  labels = c("5", "11","12", "14","16", "17", "19"))) %>%
  dplyr::mutate(SES_hanson_medu_cfps = as.numeric(as.character(medu_recode))) 
summary(hanson_CFPS2011$SES_hanson_inc_cfps)
summary(hanson_CFPS2011$SES_hanson_fedu_cfps)
summary(hanson_CFPS2011$SES_hanson_medu_cfps)
## PSID ##
convert_index_psid_function(paper_year = 2006, paper_country = "USA") # dataset published year: 2006
convert_idx = index_ppp_first_psid #index_cpi_first_psid
hanson_PSID2011 <- df.PSID_child %>%
  dplyr::mutate(fincome_bin = cut(fincome,
                                  breaks = c(0.5*convert_idx, 5000*convert_idx, 10000*convert_idx, 15000*convert_idx, 25000*convert_idx, 35000*convert_idx, 50000*convert_idx, 75000*convert_idx, 100000*convert_idx, 100000000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5","6","7","8", "9"))) %>%
  dplyr::mutate(fincome_bin_median = recode(fincome_bin,'1'='2500', '2' = '7500', '3' = '12500', '4' = '20000', '5' = '30000', '6' = '42500', '7' = '57500', '8' = '87500', '9' = '112500')) %>%
  dplyr::mutate(fincome_bin_median = as.numeric(as.character(fincome_bin_median))) %>%
  dplyr::mutate(fincome_bin_median = fincome_bin_median * convert_idx) %>%
  dplyr::mutate(SES_hanson_inc_psid = log10(fincome_bin_median)) %>%
  dplyr::mutate(fedu_recode = cut(eduy_f, 
                                  breaks = c(-0.5, 5.5, 11.5, 12.5, 14.5, 16.5, 17.5,30),
                                  labels = c("5", "11","12", "14","16", "17", "19"))) %>%
  dplyr::mutate(SES_hanson_fedu_psid = as.numeric(as.character(fedu_recode))) %>%
  dplyr::mutate(medu_recode = cut(eduy_m, 
                                  breaks = c(-0.5, 5.5, 11.5, 12.5, 14.5, 16.5, 17.5,30),
                                  labels = c("5", "11","12", "14","16", "17", "19"))) %>%
  dplyr::mutate(SES_hanson_medu_psid = as.numeric(as.character(medu_recode))) 
summary(hanson_PSID2011$SES_hanson_inc_psid)
summary(hanson_PSID2011$SES_hanson_fedu_psid)
summary(hanson_PSID2011$SES_hanson_medu_psid)
####################################Hanson 2019##########################################
# subject: adolescents
# SES: income 
# - in 10 categories: 1=up to $5,000; 2=between $5,000 and $10,000; 3=between $11,000 and $15,000; 4=between $16,000 and $29,000; 5=between $30,000 and $40,000; 6=between $41,000 and $50,000; 7=between $51,000 and $60,000;8=between $61,000 and $70,000; 9=between $71,000 and $80,000; 10=beyond $81,000
# Note: Parenting across cultures wave 2 (not find specific year) -- Using paper publish year?
## CFPS ##
convert_index_cfps_function(paper_year = 2019, paper_country = "USA") # dataset published year: 2006
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
hanson_CFPS2019 <- df.CFPS_child %>%
  dplyr::mutate(fincome_bin = cut(faminc,
                                  breaks = c(0.5*convert_idx, 5000*convert_idx, 10000*convert_idx, 15000*convert_idx, 29000*convert_idx, 40000*convert_idx, 50000*convert_idx, 60000*convert_idx, 70000*convert_idx, 80000*convert_idx, 1000000000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5","6","7","8", "9", "10"))) %>%
  dplyr::mutate(SES_hanson_cpfs = as.numeric(as.character(fincome_bin))) 
table(hanson_CFPS2019$SES_hanson_cpfs)
## PSID ##
convert_index_psid_function(paper_year = 2019, paper_country = "USA") # dataset published year: 2006
convert_idx = index_ppp_first_psid #index_cpi_first_psid
hanson_PSID2019 <- df.PSID_child %>%
  dplyr::mutate(fincome_bin = cut(fincome,
                                  breaks = c(0.5*convert_idx, 5000*convert_idx, 10000*convert_idx, 15000*convert_idx, 29000*convert_idx, 40000*convert_idx, 50000*convert_idx, 60000*convert_idx, 70000*convert_idx, 80000*convert_idx, 1000000000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5","6","7","8", "9", "10"))) %>%
  dplyr::mutate(SES_hanson_psid = as.numeric(as.character(fincome_bin))) 
table(hanson_PSID2019$SES_hanson_psid)
###########################Hanson, 2012###################
# subjects: children
# SES: maternal education: in 8 categories (1-8):m 1 to 8, denoting level of education obtained with possible choices of grade school, high school or general education diploma, 2-year college, trade, or technical school, 4-year college, or graduate school
# Note: not enough years to cut for 8 categories (combine 4 and 5)
## CFPS ##
hanson_CFPS2012 <- df.CFPS_child %>%
  dplyr::mutate(medu_recode = cut(eduy_m, 
                                  breaks = c(-0.5, 9.5, 12.5, 13.5, 14.5, 15.5, 16.5,30),
                                  labels = c("1", "2","3", "4.5","6", "7", "8"))) %>%
  dplyr::mutate(SES_hanson_cfps = as.numeric(as.character(medu_recode))) 
table(hanson_CFPS2012$SES_hanson_cfps)
## PSID##
hanson_PSID2012 <- df.PSID_child %>%
  dplyr::mutate(medu_recode = cut(eduy_m, 
                                  breaks = c(-0.5, 8.5, 12.5, 13.5, 14.5, 15.5, 16.5,30), # US system middle school 8 years
                                  labels = c("1", "2","3", "4.5","6", "7", "8"))) %>%
  dplyr::mutate(SES_hanson_psid = as.numeric(as.character(medu_recode))) 
table(hanson_PSID2012$SES_hanson_psid)
##################################Lambert, 2017######################
#subject: adolescents
# SES: INR (Poverty or not poverty), correctly by familysize
# Note: similar as Banerjee, 2020 (but childlren)
## CFPS ##
lambert_CFPS <- df.CFPS_child %>%
  dplyr::mutate(poverty_line = (faminc/familysize)/1274)%>%
  dplyr::mutate(SES_lambert_cfps = ifelse(poverty_line<=1, 1, 0))
## PSID ##
lambert_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_lambert_psid = ifelse(fincome<= poverty_line, 1, 0))
table(lambert_CFPS$SES_lambert_cfps)
table(lambert_PSID$SES_lambert_psid)
###########################Hedge, 2019 #####################
# Subject: adults
# ses: household income (pounds) <18k, 18k-30999, 31k-51999, 52k -100k, >100k
# Note: biobank data, no specific year (use article year 2019)
## CFPS ##
convert_index_cfps_function(paper_year = 2019, paper_country = "GBR") # dataset published year: 2019
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
hedge_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_bin = cut(faminc,
                                  breaks = c(0.5*convert_idx, 18000*convert_idx, 30999*convert_idx, 51999*convert_idx, 100000*convert_idx, 1000000000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5"))) %>%
  dplyr::mutate(SES_hedge_cpfs = as.numeric(as.character(fincome_bin))) 
table(hedge_CFPS$SES_hedge_cpfs)
## PSID ##
convert_index_psid_function(paper_year = 2019, paper_country = "GBR") # dataset published year: 2006
convert_idx = index_ppp_first_psid #index_cpi_first_psid
hedge_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_bin = cut(fincome,
                                  breaks = c(0.5*convert_idx, 18000*convert_idx, 30999*convert_idx, 51999*convert_idx, 100000*convert_idx, 1000000000000000*convert_idx),
                                  labels = c("1", "2", "3", "4", "5"))) %>%
  dplyr::mutate(SES_hedge_psid = as.numeric(as.character(fincome_bin))) 
table(hedge_PSID$SES_hedge_psid)
######################################Hutton, 2021##########################
# subject: children
# SES: poverty or not (binary), use US poverty criteria (same as Banerjee, 2020, but subjects are children)
## CFPS ##
convert_index_cfps_function(paper_year = 2020, paper_country = "USA")  # year 2001-2003
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps

hutton_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc_cat = cut(faminc ,
                               breaks = c(-1, 25000*convert_idx , 75000*convert_idx ,150000*convert_idx ,250000*convert_idx),
                               labels = c(12500*convert_idx, 37500*convert_idx, 75000*convert_idx, 125000*convert_idx))) %>%
  dplyr::mutate(faminc_cat = as.numeric(as.character(faminc_cat))) %>%
  dplyr::mutate(poverty_line = (faminc_cat/familysize)/1274)%>%
  dplyr::mutate(SES_hutton_cfps = ifelse(poverty_line<=1, 1, 0))
## PSID ##
convert_index_psid_function(paper_year = 2020, paper_country = "USA")  # year 2001-2003
convert_idx = index_ppp_first_psid #index_cpi_first_cfps

hutton_PSID <- df.PSID_child %>%
  dplyr::mutate(faminc_cat = cut(fincome ,
                                 breaks = c(-1, 25000*convert_idx , 75000*convert_idx ,150000*convert_idx ,250000*convert_idx),
                                 labels = c(12500*convert_idx, 37500*convert_idx, 75000*convert_idx, 125000*convert_idx))) %>%
  dplyr::mutate(faminc_cat = as.numeric(as.character(faminc_cat))) %>%
  
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_hutton_psid = ifelse(faminc_cat<= poverty_line, 1, 0))
table(hutton_CFPS$SES_hutton_cfps)
table(hutton_PSID$SES_hutton_psid)
#########################Javanbakht, 2015/2016##################
# subjects: young adults
# SES:INR 
## CFPS ##
javanbakht_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES_javanbakht_cfps = (faminc/familysize)/1274)
## PSID ##
javanbakht_PSID <- df.PSID_adult %>%
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_javanbakht_psid = fincome/poverty_line)
summary(javanbakht_CFPS$SES_javanbakht_cfps)
summary(javanbakht_PSID$SES_javanbakht_psid)
#############################Jensen, 2021##############
# subject: infants
# SES: 
# - wealth: cannot be reproduced (materials are not exactly the same?)
# - maternal education: 1-10 years
## CFPS ##
jensen_CFPS <- df.CFPS_child%>%
  dplyr::mutate(SES_jensen_cfps = ifelse(eduy_m <=10, eduy_m, NA))
summary(jensen_CFPS$SES_jensen_cfps)
## PSID ##
jensen_PSID <- df.PSID_child%>%
  dplyr::mutate(SES_jensen_psid = ifelse(eduy_m <=10, eduy_m, NA))
summary(jensen_PSID$SES_jensen_psid)
#########################Johnson, 2021##################
# subjects: adolescents
# SES:INR 
## CFPS ##
johnson_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_johnson_cfps = (faminc/familysize)/1274)
## PSID ##
johnson_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty_line = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES_johnson_psid = fincome/poverty_line)
summary(johnson_CFPS$SES_johnson_cfps)
summary(johnson_PSID$SES_johnson_psid)
#################################Kelvin, 2012###################################
# subject: adults
# SES
# - Income: household income (US$ <70 or >70 per month)
# - education: primary school or less vs. more than primary schoo
# Note: ? data collected in Ecudor (not exist in ppp data) but currency was US dollor, Use USA as country for now
## CFPS ##
convert_index_cfps_function(paper_year = 2002, paper_country = "ECU")  # year 2001-2003
convert_idx = index_ppp_first_cfps #index_cpi_first_cfps
kelvin_CFPS <- df.CFPS_adult%>%
  dplyr::mutate(SES_inc_cfps = ifelse(faminc>70*12*convert_idx, 1, 0))%>%
  dplyr::mutate(SES_edu_cfps = ifelse(eduy >6, 1, 0))
table(kelvin_CFPS$SES_edu_cfps)
table(kelvin_CFPS$SES_inc_cfps)
## PSID ##
convert_index_psid_function(paper_year = 2002, paper_country = "USA")  # year 2001-2003
convert_idx = index_ppp_first_psid #index_cpi_first_psid
kelvin_PSID <-df.PSID_adult %>%
  dplyr::mutate(SES_inc_cfps = ifelse(faminc > 70*12*convert_idx, ))

dplyr::mutate(occup_ses = recode(egp_m, "1"= 7, "2"=6, "3"=5,  "7"= 5, "4"=4, "5"= 4, 
                                 "6"= 4, "8"= 3, "9"=2, "10"=1, "11"=1,
                                 "-8"=-8,"80000"= 8, .default = -8)) %>%  # recode occupation
  
