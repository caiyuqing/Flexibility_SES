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

# import data CFPS, including the following variable:
# df.children, df.community, df.family, df.individual
load("CFPS2010.RData")

# ---------- 1.  Prepare the CFPS data for later analysis--------------------------------------------
## Combine children, adult, family and community data in a data frame
## Note: only children between 10 and 22 year-old were included.
df.CFPS <- df.children %>%
  dplyr::full_join(., df.individual) %>%    # merge children and individual data
  dplyr::arrange(fid, pid) %>%              # arrange the sequence of data according to fid and pid
  dplyr::mutate(age = ifelse(is.na(qa1age), wa1age, qa1age)) %>%         # combine age in child datafrome and adult dataframe
  dplyr::mutate(role_c = ifelse(age <= 22 & age >= 10, "child", NA)) %>% # select 10-22 years old as child
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
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,           # pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         # pid_father: same as pid_mother
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    # pid_chilld: if the person is child, it is his/her pid; if not, NA
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -8))) %>% # set all the '-8' as NA (in CFPS -8 means not applicable)
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -1))) %>% # set all the '-1' as NA (in CFPS -1 means "I don't know")
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -2))) %>% # set all the '-2' as NA (in CFPS -2 means "I don't want to answer"
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -7))) %>% # set all the '-7' as NA (in CFPS -7 means "did not answer clearly. cannot categorize")
  dplyr::mutate(across(where(is.numeric), ~ na_if(.x, -9)))     # set all the '-9' as NA (in CFPS -9 means "missing value"

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
                isco_c = qg307isco,
                eduy_c = cfps2010eduy_best,
                edu_c = cfps2010edu_best,
                income_c = income,
                sss_c = qm402,
                employment_c = qg3) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", 
                              c('pid_mother', "qg307egp","qg307isco",'cfps2010edu_best', "cfps2010eduy_best", "qm402","income", "qg3")], 
                   by = 'pid_mother') %>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(egp_m= qg307egp,       # occupation coding
                isco_m = qg307isco,
                edu_m = cfps2010edu_best,
                eduy_m = cfps2010eduy_best,
                income_m = income,
                sss_m = qm402,
                employment_m = qg3) %>% 
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", 
                              c('pid_father',"qg307egp","qg307isco",'cfps2010edu_best',"cfps2010eduy_best", "qm402", "income","qg3")], 
                   by = 'pid_father') %>%
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
  dplyr::select(pid:gender, age,qg307egp, qg307isco, cfps2010eduy_best,cfps2010edu_best, 
                income, qm402, faminc,familysize,meduc, feduc,fd1,qg3)%>%
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

# ---------- 2.  Prepare the PSID data for later analysis--------------------------------------------
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