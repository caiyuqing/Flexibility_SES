################## data extraction for "what is SES" project ##############################
# 
# Author      Date(yy-mm-dd)   Change History
# =================================================
# Cai, Y-Q    20-03-15         The first version
# Hu, C-P     20-04-27         Validate the script
# 
# 
# ============= Notes about data ==================
#   CFPS: there are some families in which two or more children has different mother (i.e., pid_m)
#   PSID: there are one family with two female adults; there are families without child.
#   To be consistent across the index reproduced, for all children and adolescents' SES, 
#   We used data from participant with age ranged 10-22 yrs-old.
#
######################## Start of the script ###########################
### clean the memory to avoid unnecessary errors:
rm(list = ls())
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientifc notion
options(digits=5)       # limit the number of reporting

### set directory to the folder of analytic data
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
if (!require(foreign)) {install.packages("foreign",repos = "http://cran.us.r-project.org"); require(foreign)}

# import data CFPS
load("CFPS2010.RData")
df.children[df.children == -8] <- NA
df.community[df.community == -8] <- NA
df.family[df.family == -8] <- NA
df.individual[df.individual == -8] <- NA

############# prepare the CFPS data for later analysis##########
## combine children, adult, family and community data in a dataframe
df.CFPS <- df.children %>%
  dplyr::full_join(., df.individual) %>%    # merge children and individual data
  dplyr::arrange(fid, pid) %>%
  dplyr::mutate(age = ifelse(is.na(qa1age), wa1age, qa1age)) %>%  #combine age in child datafrome and adult dataframe
  dplyr::mutate(role_c = ifelse(age <= 22 & age >= 10, "child", NA))%>% #select 10-22 years old as child
  dplyr::left_join(., df.family, by = "fid") %>%        # merge with family data
  dplyr::group_by(fid) %>% 
  dplyr::mutate(cid = ifelse(sum(!is.na(cid)) == 0,               # copy cid to all rows that share the same family id
                             NA, cid[!is.na(cid)])) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., df.community, by = "cid") %>%                # merge with community data
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c =="child")$pid_f, 
                                'father', NA),                     # add a column to indicate father or NA
                role_m = ifelse(pid %in% subset(., role_c =="child")$pid_m, 
                                'mother', NA)) %>%      # add a column to indicate mother  or NA
  dplyr::ungroup() %>%
  dplyr::select(pid, fid, cid, pid_f, pid_m, role_f, role_m, role_c, everything()) %>%  # get some columns as the first few columns
  tidyr::unite("role", role_f:role_c, na.rm = TRUE, remove= TRUE) %>%
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         #pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                ifelse(role == 'mother', pid, NA)), #pid_m: if the person is a child, pid_m = pid_mother; if the person is not a child, pid_m = his/her mother's pid
                pid_father = ifelse(role == 'child', pid_f,         #pid_father, pid_f same
                               ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>% 
  dplyr::na_if(.,-8) # set NA

## select children data (and parents SES) for the further analysis
df.CFPS_child <- df.CFPS %>%
  dplyr::filter(role == "child") %>% #select children
  dplyr::group_by(fid) %>% # group by family
  arrange(pid) %>%
  dplyr::filter(row_number()==1)%>%
  dplyr::ungroup() %>%
  dplyr::rename(educ_c = educ,
                edu2010_t1_best_c = edu2010_t1_best,
                egp_c = qg307egp) %>%
  #select their parents, and parents SES variables (only select those will be used in the following analysis)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", c('pid_mother', 'educ', "edu2010_t1_best", "qg307egp")], by = 'pid_mother')%>%
  dplyr::rename(educ_m = educ,
                edu2010_t1_best_m = edu2010_t1_best,
                egp_m = qg307egp) %>%
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", c('pid_father', 'educ', "edu2010_t1_best", "qg307egp")], by = 'pid_father') %>%
  dplyr::rename(educ_f = educ,
               edu2010_t1_best_f = edu2010_t1_best,
               egp_f = qg307egp) 

############# prepare the PSID data for later analysis##########
# Load the data about family structure.
df.map_psid <- read.spss("GID_map_psid.sav", to.data.frame = TRUE) %>%
  dplyr::mutate(pid = (ER30001 * 1000) + ER30002,
                pid_f = ER30001_P_F *1000 + ER30002_P_F,
                pid_m = ER30001_P_M *1000 + ER30002_P_M) %>% 
  dplyr::select(pid, pid_f, pid_m)

# load the PSID data
df.PSID <- read.spss("PSID_selected_data.sav", to.data.frame = TRUE) %>%
  dplyr::mutate(pid = (ER30001 * 1000) + ER30002) %>% # generate a new column for unique pid for each individual.
  dplyr::rename(fid = ER34501,
                fincome = ER71426,
                edu = ER34548,
                relation = ER34503,
                sex = ER32000,
                age = ER34504,
                sequence=ER34502,
                depression = ER70680,
                life_satisfaction = ER66025) %>%
  dplyr::group_by(fid) %>%
  dplyr::mutate(familysize = length(fid)) %>%   # add family size to the data
  dplyr::ungroup() %>%
  dplyr::left_join(., df.map_psid) %>%
  dplyr::mutate(role_c = ifelse(age >=10 & age <=22, "child", NA)) %>%
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c == "child")$pid_f, "father", NA),
                role_m = ifelse(pid %in% subset(., role_c == "child")$pid_m, "mother", NA)) %>%
  tidyr::unite("role", role_c:role_m, na.rm = TRUE, remove = TRUE) %>%
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         #pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), #pid_m: if the person is a child, pid_m = pid_mother; if the person is not a child, pid_m = his/her mother's pid
                pid_father = ifelse(role == 'child', pid_f,         #pid_father, pid_f same
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%
  dplyr::filter(sequence <= 20)  # ?

df.PSID_child <- df.PSID %>%
  dplyr::filter(role == "child") %>%
  dplyr::select(pid, fid, age, sex, pid_father, pid_mother, familysize, fincome, sequence, role, depression, life_satisfaction) %>%
  dplyr::left_join(., df.PSID[df.PSID$role == "mother", c("pid_mother", "edu")], by = "pid_mother")%>%
  dplyr::rename(edu_m = edu) %>%
  dplyr::left_join(., df.PSID[df.PSID$role == "father", c("pid_father", "edu")], by = "pid_father")%>%
  dplyr::rename(edu_f = edu) %>%
  dplyr::group_by(fid) %>% # group by family
  arrange(pid) %>%
  dplyr::filter(row_number()==1)%>%
  dplyr::ungroup()

##################### Reproduce SES indexes in papers ###################################################################
####### Betancourt, L, 2016###########
#  SES = (ITN + mother's edu)/2
# ITN: income-to-need ratio, ITN = 
# maternal education:
#   ?technical/vocational as technical/vocational college; different system
#   (1)1=Illiterate 2=Adult primary school/Literacy class 3=Ordinary primary school 4=Adult junior high school 5=Vocational junior high school 6=Ordinary junior high school 
#   (2)7=Ordinary specialized high school/Vocational high school/Technical high school 8=Adult senior high school 9=Specialized adult high school 10=Ordinary senior high school 
#   (3)11=3-year adult college (4)12=Ordinary 3-year college (5)13=4-year adult college (6)14=Ordinary 4-year college (7)15=Master’s degree 16=Doctoral degree (No such level as "some graduate")

betan_CFPS <- df.CFPS_child %>%
  dplyr::mutate(itn = base::cut(finc_per, breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), labels = c("1", "2", "3", "4", "5"))) %>% #set 5 level of itn according to poverty line (4 cut-point: itn1 = poverty line, itn4 = 400% above poverty line, rest two set between itn1 and itn4)
  dplyr::mutate(edu_m_recode = dplyr::recode(educ_m, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, 
                                      "7" = 2, "8" = 2, "9" = 2, "10" = 2, 
                                      "11" = 3,"12" = 4,"13" = 5, "14" = 6, "15" = 7,"16" = 7)) %>%  
  dplyr::mutate(itn = as.numeric(as.character(itn)), # convert factor into numeric variable
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% # edu_m_recode was already numeric after recoding? 
  dplyr::mutate(SES_betan_cfps = (itn + edu_m_recode)/2)  # SES as composite of mean of itn and edu level
table(betan_CFPS$SES_betan_cfps)

betan_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.00001, 11.5, 12.5, 13.5, 14.5, 16.5, 98, 100), 
                                   labels = c("1", "2", "3", "4", "5", "6", NA)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% # factor to number is a bit tricky & may cause error.
  dplyr::mutate(itn1 = 12060 +  (familysize-1)*4180) %>% # poverty line 12060 for one people and increase 4180 for an extra person
  # get the interval of ITN using "ifelse"
  dplyr::mutate(itn = ifelse(fincome < itn1, 1, 
                                 ifelse(itn1 <= fincome & fincome < itn1*2, 2,
                                         ifelse(itn1*2 <= fincome & fincome < itn1*3, 3,
                                                ifelse(itn1*3 <= fincome & fincome < itn1*4, 4, 
                                                       ifelse(itn1*4 <= fincome, 5, 0)))))) %>%
  dplyr::mutate(SES_betan_psid = (itn + edu_m_recode)/2) 
table(betan_PSID$SES_betan_psid)

######## Moog, et al., 2008 ######
# SES = 
# Note: 
# sources of SES: mother's highest edu, income
# preproc. of mother's highest edu:
# preproc. of income: 
# final SES score: (mother's edu + income)/2

moog_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_cat = dplyr::recode_factor(edu2010_t1_best_m, "1" = 1,"2" = 1,"3" = 1,"4" = 2,"5" = 3,"6" = 3,"7" = 4,"8" = 5))  %>% #recode education
  dplyr::mutate(income_cat = base::cut(fincome, 
                               breaks= quantile(fincome, probs = seq(0, 1, 0.2), na.rm= TRUE), 
                               labels = c("1", "2", "3", "4", "5"))) %>% #recode income
  dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),
                edu_cat = as.numeric(as.character(edu_cat)))%>% #convert income and education into numeric variables
  dplyr::mutate(SES_moog_cfps = (edu_cat+income_cat)/2)
table(moog_CFPS$SES_moog_cfps)

moog_PSID <- df.PSID_child %>%
  dplyr::mutate(income_cat=cut(fincome, breaks= quantile(fincome, probs = seq(0, 1, 0.2), na.rm= TRUE), labels = c("1", "2","3", "4","5"))) %>%#recode income
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.00001, 11.5, 12.5, 16.5, 98, 100), labels = c("1", "2", "3", "4",  NA))) %>% #cut into 4 groups: <12, 12, 13-16, 17  (?do not distinguish master and phd)
  dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),
                edu_m_recode = as.numeric(as.character(edu_m_recode)))%>%
  dplyr::mutate(SES_moog_psid = (income_cat + edu_m_recode)/2)
table(moog_PSID$SES_moog_psid)

##########################################################################################
######## Yu,2018, not figured out yet ######
## Can not be reproduced yet because of .......
#
#
## PCA
## install.packages("FactoMineR")
## install.packages("factoextra")
#library(FactoMineR)
#library(factoextra)
#yu_pca_cfps <- yu_cfps %>%
#  dplyr::select(ITN, edu_f_recode, edu_m_recode, sss)
#yu_pca_cfps <- drop_na(yu_pca_cfps)
#yu.pr_cfps<-PCA(yu_pca_cfps, scale.unit = TRUE, graph = TRUE)
#get_eigenvalue(yu.pr_cfps)
#fviz_eig(yu.pr_cfps)#scree plot
#var <- get_pca_var(yu.pr_cfps) #sss shows opposite contribution (?)
#var$coord
#
## calculate SES as first component (?PCA does not appear to be single component)
#yu_cfps <- yu_cfps %>%
#  dplyr::mutate(SES_yu_cfps = 0.71118313*ITN + 0.78339583*edu_f_recode + 0.81539907*edu_m_recode + (-0.04682041)*sss)
#
## psid
## psid:only rp and sp have sss and parental education info
## extract sp and rp 
## reproduce Yu,2018 second young adult version first
## subject here is rp and sp
## import more data (education related)
#yu_yadult_psid_rp <- df.psid %>%
#  dplyr::select(ER34503,ER34501,ER30002,ER32000,ER34504) %>% #relation to RP; fid; pid; sex;age
#  dplyr::filter(ER34503 == 10) %>%
#  dplyr::filter(ER34504 <= 25 &ER34504 >= 18)
#yu_yadult_psid_sp <- df.psid %>%
#  dplyr::select(ER34503,ER34501,ER30002,ER32000,ER34504) %>% #relation to RP; fid; pid; sex;age
#  dplyr::filter(ER34503 == 20) %>%
#  dplyr::filter(ER34504 <= 25 &ER34504 >= 18)
#names(yu_yadult_psid_rp)  <- c("relation_rp", "fid", "pid", "sex", "age")
#names(yu_yadult_psid_sp)  <- c("relation_rp", "fid", "pid", "sex", "age")
#
## recode income
#yu_psid_income <- df.psid %>%
#  dplyr::select(ER34501,ER30002, ER71426)  %>% #fid, pid, total family income
#  dplyr::mutate(income_cat = cut(ER71426, 
#                                 breaks = quantile(df.psid$ER71426, probs = seq(0, 1, 1/9), na.rm= TRUE),
#                                 labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")))%>% #recode family income into 9 groups
#  dplyr::mutate(income_cat = as.numeric(income_cat)) #convert into numeric variable
#names(yu_psid_income) <- c("fid", "pid", "fincome", "income_cat")
#yu_psid_income <- merge(yu_psid_income, familysize_psid, by = "fid")
#group_median <- yu_psid_income %>% 
#  dplyr::group_by(income_cat) %>% #group income into 9 groups
#  dplyr::summarise(group_median = median(fincome))
#yu_psid_income <- merge(yu_psid_income, group_median, by= "income_cat") #merge income median with main data frame
#yu_psid_income <- yu_psid_income %>%
#  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  #calculate poverty line according to family size
#  dplyr::mutate(ITN = group_median/poverty) #calculate ITN: group_median divide poverty line
#table(yu_psid_income$ITN) #check ITN
#
## parents' education
#yu_psid_edu <- df.psid %>%
#  dplyr::select(ER34501,ER30002,TA171981,TA171983) %>% #fid, pid, mother_edu, father_edu
#  dplyr::mutate(edu_m_recode = cut(TA171981, breaks = c(-0.001, 0.5, 11.5, 12.5, 13.5, 16.5,20), labels = c("1", "2", "3", "4", "5", "6")),
#                edu_f_recode = cut(TA171983, breaks = c(-0.001, 0.5, 11.5, 12.5, 13.5, 16.5,20), labels = c("1", "2", "3", "4", "5", "6"))) %>% #cut education into 6 groups: Noe of below (1):0; < high school (2):1-10; High scool (3):11-12; associate degree (4):13-14; bachelor's degree (5):14-16; Master's degree (6)&PhD/MD (7):17
#  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode),
#                edu_f_recode = as.numeric(edu_f_recode))
#names(yu_psid_edu) <- c("fid", "pid", "mother_edu", "father_edu", "edu_m_recode", "edu_f_recode")
#
## sss
#yu_psid_sss_rp <- df.psid%>%
#  dplyr::select(ER34501,ER30002,ER70879) # fid,pid, sss_rp
#yu_psid_sss_sp <- df.psid%>%
#  dplyr::select(ER34501,ER30002,ER70741) # fid,pid, sss_sp
#names(yu_psid_sss_rp) <- c("fid", "pid", "sss")
#names(yu_psid_sss_sp) <- c("fid", "pid", "sss")
#
## merge
## merge rp with rp sss
#yu_yadult_psid_rp <-  merge(yu_yadult_psid_rp, yu_psid_sss_rp, by = c("fid", "pid"))
#yu_yadult_psid_sp <-  merge(yu_yadult_psid_sp, yu_psid_sss_sp, by = c("fid", "pid"))
##combine rp with sp
#yu_yadult_psid <- rbind(yu_yadult_psid_rp, yu_yadult_psid_sp)
#
## merge with education and income
#yu_yadult_psid <- merge(yu_yadult_psid,  yu_psid_income, by = c("fid", "pid"))
#yu_yadult_psid <- merge(yu_yadult_psid, yu_psid_edu, by = c("fid", "pid"))
#
## PCA
#yu_pca_psid <- yu_yadult_psid %>%
#  dplyr::select(ITN, edu_f_recode, edu_m_recode, sss)
#yu_pca_psid <- drop_na(yu_pca_psid)
#yu.pr_psid<-PCA(yu_pca_psid, scale.unit = TRUE, graph = TRUE)
#get_eigenvalue(yu.pr_psid)
#fviz_eig(yu.pr_pid)#scree plot
#var <- get_pca_var(yu.pr_psid)
#var$coord
## calculate SES as first component (?PCA does not appear to be single component)
#yu_yadult_psid <- yu_yadult_psid %>%
#  dplyr::mutate(SES_yu_psid = 0.5152558*ITN + 0.7833663*edu_f_recode + 0.81539907*edu_m_recode + (-0.04682041)*sss)
#
##################################### Jednoróg,  2012 ###############################################
# SES = Maternal_Edu * 4 + Occup *7
# Note: we reversed from the original so that ....
# Note: only reproduced using CFPS data because occupation data is not available from PSID

######### CFPS ##########
jed_CFPS <- df.CFPS_child %>%
  dplyr::mutate(occup_ses = recode(egp_m, "1"= 7, "2"=6, "3"=5,  "7"= 5, "4"=4, "5"= 4, "6"= 4, "8"= 3, "9"=2, "10"=1, "11"=1, "-8"=-8,"80000"= 8, .default = -8)) %>%
  dplyr::na_if(.,-8) %>% #set NA
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5,4.5,5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_ses = as.numeric(as.character(occup_ses)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_jed_cfps = 4*edu_m_recode + 7*occup_ses) 
table(jed_CFPS$SES_jed_cfps)


#################################### McDermott, 2019 ###############################################
# SES: (SES_father + SES_mother)/2
# SES_father = occup_f*5 + edu_f*3; 
# SES_mother = occup_m*5 + edu_m*3
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: can ONLY be reproduced using CFPS data because of the occupation

mcder_CFPS <- df.CFPS_child %>%
  # recode education and occupation
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%
  dplyr::mutate(edu_f_recode = cut(educ_f, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_f_recode = recode(egp_f, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%
  dplyr::na_if(., -8) %>%
  # composite SES
  dplyr::mutate(occup_f_recode = as.numeric(as.character(occup_f_recode)),
                occup_m_recode = as.numeric(as.character(occup_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_f = occup_f_recode*5 + edu_f_recode*3,
                SES_m = occup_m_recode*5 + edu_m_recode*3)%>%
  #replace father and mother's ses with each other if one is NA
  dplyr::mutate(SES_f = ifelse(is.na(SES_f), SES_m, SES_f),
                SES_m = ifelse(is.na(SES_m), SES_f, SES_m))%>%
  dplyr::mutate(SES_mcder_cfps = (SES_m + SES_f)/2) %>%
  dplyr::group_by(fid) %>%
  dplyr::arrange(pid) %>%
  dplyr::filter(row_number()==1)%>%
  dplyr::ungroup()
table(mcder_CFPS$SES_mcder_cfps)

#################################### Romeo, 2018a ###############################################
# SES = mother's education and occupation
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: only reproduced using CFPS data because of the occupation data

romeo1_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occu_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8)) %>%
  dplyr::na_if(., -8) %>%
  dplyr::mutate(occu_m_recode = as.numeric(as.character(occu_m_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_romeo1_cfps = occu_m_recode*5 + edu_m_recode*3)
table(romeo1_CFPS$SES_romeo1_cfps)

#################################### Romeo, 2018b ###############################################
# SES = (Z_{parents' education} + Z_{family income})/2
# Note: 

romeo2_CFPS <- df.CFPS_child %>%
  #recode income and education
  dplyr::mutate(income_zscore = (fincome - mean(fincome, na.rm = TRUE))/sd(fincome, na.rm = TRUE)) %>%
  dplyr::mutate(edu_f_recode = cut(educ_f, breaks = c(-0.5,6.5,11.5,13.5,14.5,16.4), labels = c("1", "2", "3", "4","5"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode))-1) %>% #recode education 0-4
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5,6.5,11.5,13.5,14.5,16.4), labels = c("1", "2", "3", "4","5"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))-1) %>%
  # replace father/mother education if one is missing
  dplyr::mutate(edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode),
                edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode))%>% 
  # composite SES
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2)%>%
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%
  #calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_cfps = (edu_zscore + income_zscore)/2)
summary(romeo2_CFPS$SES_romeo2_cfps)

romeo2_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.00001, 11.5, 12.5,  14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5",  "6"))) %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, breaks = c(-0.00001, 11.5, 12.5,  14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5",  "6"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))-1,
                edu_f_recode = as.numeric(as.character(edu_f_recode))-1)%>% #recode as 0-4
  dplyr::mutate(edu_m_recode = na_if(edu_m_recode, 5),
                edu_f_recode = na_if(edu_f_recode, 5)) %>%
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode)) %>%
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2) %>%
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%
  dplyr::mutate(income_zscore = (fincome - mean(fincome, na.rm = TRUE))/sd(fincome, na.rm = TRUE)) %>%
  #calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_psid = (edu_zscore + income_zscore)/2)
summary(romeo2_PSID$SES_romeo2_psid)

#################################### Qiu, 2017################################
# SES = household income 
# Note: 

qiu_CFPS <- df.CFPS_child%>%
  dplyr::rename(SES_qiu_cfps = fincome)
summary(qiu_CFPS$SES_qiu_cfps)

qiu_PSID <- df.PSID_child %>%
  dplyr::select(fid, pid, fincome, age) %>%
  dplyr::rename(SES_qiu_psid = fincome) 
summary(qiu_PSID$SES_qiu_psid)

#################################### Kim, 2019 ##################################
# SES = (family income)/(poverty line)
# Note: 

kim_CFPS <- df.CFPS_child %>%
  dplyr::mutate(INR = finc_per/1274)  %>%
  dplyr::mutate(INR = log10(INR)) %>%
  dplyr::mutate(SES_kim_cfps = ifelse(!is.finite(INR), NA, INR)) 
summary(kim_CFPS$SES_kim_cfps)

kim_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%    # calculate poverty line for every family
  dplyr::mutate(SES_kim_psid = fincome/poverty) %>%            # INR = SES
  dplyr::mutate(SES_kim_psid = log10(SES_kim_psid)) %>%        # log transformation
  dplyr::mutate(SES_kim_psid = ifelse(!is.finite(SES_kim_psid), NA, SES_kim_psid))
summary(kim_PSID$SES_kim_psid)

########################## Hanson, 2013 ################
# SES = income and poverty
# Note: 

hanson_CFPS <- df.CFPS_child %>%
  dplyr::mutate(FPL = finc_per/1274) %>% #calculate FPL according to poverty line
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>% #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps = as.numeric(as.character(SES_hanson_cfps)))
table(hanson_CFPS$SES_hanson_cfps)
  
hanson_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%#calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty) %>% #calculate FPL according to poverty line
  dplyr::mutate(SES_hanson_psid = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>% #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_psid = as.numeric(as.character(SES_hanson_psid))) 
table(hanson_PSID$SES_hanson_psid)

#################### Leonard, 2019 ######################
# SES = maternal education: dichotomous, divided by college
# Note: 
leo_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES_leo_cfps = cut(edu2010_t1_best_m, breaks = c(0, 4.5, 8.5), labels = c("1", "2"))) %>%
  dplyr::mutate(SES_leo_cfps = as.numeric(as.character(SES_leo_cfps))-1)
table(leo_CFPS$SES_leo_cfps)
  
leo_PSID <- df.PSID_child %>%
  dplyr::mutate(SES_leo_psid = cut(edu_m, breaks = c(-0.01, 12.5, 17.5, 100), labels = c("1", "2", "3"))) %>%
  dplyr::mutate(SES_leo_psid = as.numeric(as.character(SES_leo_psid))) %>%
  dplyr::mutate(SES_leo_psid = na_if(SES_leo_psid, 3))  
table(leo_PSID$SES_leo_psid)
  
#################################### Ozernov-Palchik, O################################
# SES = 
# Note: same as romeo(a)--Barratt Simplified Measure of Social Status (BSMSS) 3-21(1-7 multiply 3)
# Note: only reproduced using CFPS data
# Questions: also dichotomous?

ozer_CFPS <- df.CFPS_child %>%
  # recode education
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_f_recode = cut(educ_f, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  # replace father with mother and verse versa
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode))%>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))*3,
                edu_f_recode = as.numeric(as.character(edu_f_recode))*3) %>%
  # composite SES
  dplyr::mutate(edu_parents = edu_m_recode + edu_f_recode) %>%
  dplyr::mutate(SES_ozer_cfps = cut(edu_parents, breaks = c(0, median(edu_parents, na.rm = 10), 100), labels = c('1', '2')))%>%
  dplyr::mutate(SES_ozer_cfps = as.numeric(as.character(SES_ozer_cfps))-1) 
table(ozer_CFPS$SES_ozer_cfps)

ozer_PSID <- df.PSID_child %>%
  #recode education
  dplyr::mutate(edu_f_recode = cut(edu_f, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(edu_f_recode = na_if(edu_f_recode, 8),
                edu_m_recode = na_if(edu_m_recode, 8)) %>%
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode)) %>%
  dplyr::mutate(edu_parents = edu_m_recode + edu_f_recode) %>%
  dplyr::mutate(SES_ozer_psid = cut(edu_parents, breaks = c(0, median(edu_parents, na.rm = 10), 100), labels = c('1', '2')))%>%
  dplyr::mutate(SES_ozer_psid = as.numeric(as.character(SES_ozer_psid))) 
table(ozer_PSID$SES_ozer_psid)

# Save data
# save SES score reproduced from CFPS


# save SES score reproduced from PSID