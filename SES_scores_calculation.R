##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                 R script for Flexibility of SES project                                                    ###
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
###  Code authors: Chuan-Peng Hu, PhD, Neuroimaging Center (NIC), Johannes Gutenberg                           ###  
###                University Medical Center, 55131 Mainz, Germany;                                            ###
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
### original paper    subject	  SES_cfps (range)	          SES_psid (range)	             type of SES         ###
### ------------------------------------------------------------------------------------------------------     ###
### Betancourt, 2916	children	SES_betan_CFPS (1-6)        SES_betan_PSID (1-5.5)       	 composite           ###
### Moog, 2008	      children	SES_moog_CFPS (1-5)         SES_mpog_PSID (1-5)	           composite           ###
### Jednoróg, 2012	  children	SES_jed_CFPS (11-84)        NA	                           composite           ###
### McDermott, 2019	  children	SES_mcder_CFPS (3-66)       NA	                           composite           ###
### Romeo, 2018a	    children	SES_romeo1_CFPS (8-66)	    NA	             	             composite           ###
### Romeo, 2018b	    children	SES_romeo2_CFPS (-0.39-0.12)SES_romeo2_PSID (-0.39-0.12)   composite           ###
### Qiu, 2017	        children	SES_qiu_CFPS (10200-298600)	SES_qiu_PSID (-73950~229087)	 income              ###
### Kim, 2019	        children	SES_kim_CFPS (-3.88~2.51	  SES_kim_PSID (-3.65-2.01)	     poverty(income)     ###
### Hanson, 2013	    children	SES_hanson_CFPS (1，2，3)	  SES_hanson_PSID (1，2，3)	     poverty(income)     ###
### Leonard, 2019	    children	SES_leo_CFPS (1，2)	        SES_leo_PSID (1，2)		         education           ###
### Ozernov-Palchik	  children	SES_ozer_CFPS (1，2)	      SES_ozer_PSID (1，2)	         education           ###
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

# ---------------------------------------------------------------------------------------
# ---------- 1.  Prepare the CFPS data for later analysis--------------------------------------------
# ---------------------------------------------------------------------------------------
## combine children, adult, family and community data in a data frame
df.CFPS <- df.children %>%
  dplyr::full_join(., df.individual) %>%    # merge children and individual data
  dplyr::arrange(fid, pid) %>% #arrange the sequence of data according to fid and pid
  dplyr::mutate(age = ifelse(is.na(qa1age), wa1age, qa1age)) %>%  #combine age in child datafrome and adult dataframe
  dplyr::mutate(role_c = ifelse(age <= 22 & age >= 10, "child", NA))%>% #select 10-22 years old as child
  dplyr::left_join(., df.family, by = "fid") %>%  # merge with family data
  # copy cid to all rows that share the same family id
  dplyr::group_by(fid) %>% 
  dplyr::mutate(cid = ifelse(sum(!is.na(cid)) == 0,               
                             NA, cid[!is.na(cid)])) %>%
  dplyr::ungroup() %>%
  # merge with community data
  dplyr::left_join(., df.community, by = "cid") %>%         
  # add a column to indicate father or NA        
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c =="child")$pid_f, 
                                'father', NA),               
  # add a column to indicate mother  or NA
                role_m = ifelse(pid %in% subset(., role_c =="child")$pid_m, 
                                'mother', NA)) %>%      
  dplyr::select(pid, fid, cid, pid_f, pid_m, role_f, role_m, role_c, everything()) %>%  # get some columns as the first few columns
  tidyr::unite("role", role_f:role_c, na.rm = TRUE, remove= TRUE) %>% #combine the columns indicating role of the individuals into one role
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         #pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         #pid_father: same as pid_mother
                               ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    #pid_chilld: if the person is child, it is his/her pid; if not, NA
  dplyr::na_if(., -8) %>%#set all the -8 as NA (in CFPS -8 means not applicable)
  dplyr::na_if(., -1) %>%#set all the -1 as NA (in CFPS -1 means "I don't know")
  dplyr::na_if(., -2) %>%#set all the -2 as NA (in CFPS -2 means "I don't want to answer"
  dplyr::na_if(., -7) %>%#set all the -7 as NA (in CFPS -7 means "did not answer clearly. cannot categorize")
  dplyr::na_if(., -9) #set all the -9 as NA (in CFPS -9 means "missing value"
  
summary(df.CFPS)
## select children data (and parents SES) for the further analysis
df.CFPS_child <- df.CFPS %>%
  dplyr::filter(role == "child") %>% #select children
  # select one child for every family
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1)%>%
  dplyr::ungroup() %>%
  # rename variables of children to distinguish from parents
  dplyr::rename(educ_c = educ, #education level
                edu2010_t1_best_c = edu2010_t1_best, #education year
                egp_c = qg307egp) %>% #occupation coding
  #select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", c('pid_mother', 'educ', "edu2010_t1_best", "qg307egp")], by = 'pid_mother')%>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(educ_m = educ, #education level
                edu2010_t1_best_m = edu2010_t1_best,#education year
                egp_m = qg307egp) %>% #occupation coding
  #select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", c('pid_father', 'educ', "edu2010_t1_best", "qg307egp")], by = 'pid_father') %>%
  # rename variables of parents to distinguish from children (father)
   dplyr::rename(educ_f = educ,
               edu2010_t1_best_f = edu2010_t1_best,
               egp_f = qg307egp) 
#check data
summary(df.CFPS_child)
# save the data of children (CFPS) as rdata for further processing
save(df.CFPS_child, file = "df.CFPS_child.RData")


# ---------------------------------------------------------------------------------------
# ---------- 2.  Prepare the PSID data for later analysis--------------------------------------------
# ---------------------------------------------------------------------------------------

## Load the data about family structure.
df.map_psid <- read.spss("GID_map_psid.sav", to.data.frame = TRUE) %>% #'GID_map_psid': family roster data of PSID
  # Generate a unique variable for each person (pid), father (pid_f), mother (pid_mother)
  # Because personal identity variable ER30001 & ER30002 are not unique for each person
  dplyr::mutate(pid = (ER30001 * 1000) + ER30002,
                pid_f = ER30001_P_F *1000 + ER30002_P_F,
                pid_m = ER30001_P_M *1000 + ER30002_P_M) %>% 
  #select only three variables that will be used in the further data analysis to determine family structure
  dplyr::select(pid, pid_f, pid_m)

# load the PSID data
df.PSID <- read.spss("PSID_selected_data.sav", to.data.frame = TRUE) %>% #selecte data from PSID website
  dplyr::mutate(pid = (ER30001 * 1000) + ER30002) %>% # generate a new column for unique pid for each individual.
  #rename frequently used variables
  dplyr::rename(fid = ER34501,
                fincome = ER71426,
                edu = ER34548,
                relation = ER34503,
                sex = ER32000,
                age = ER34504,
                sequence=ER34502,
                depression = ER70680,
                life_satisfaction = ER66025) %>%
  #calculate family size
  dplyr::group_by(fid) %>%
  dplyr::mutate(familysize = length(fid)) %>%   
  dplyr::ungroup() %>%
  #add family structure to the data (children,father and mother)
  dplyr::left_join(., df.map_psid) %>%
  #set role of the person
  dplyr::mutate(role_c = ifelse(age >=10 & age <=22, "child", NA)) %>% #children: age from 10-22
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c == "child")$pid_f, "father", NA), #father: children's father
                role_m = ifelse(pid %in% subset(., role_c == "child")$pid_m, "mother", NA)) %>% #mother: children's mother
  #combine columns indicating roles into one column
  tidyr::unite("role", role_c:role_m, na.rm = TRUE, remove = TRUE) %>%
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         #pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         #pid_father: same as pid_mother
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    #pid child: if the person is child, it is her pid; if not, NA
  dplyr::filter(sequence <= 20)  # select only those still live in the family in 2017 (sequence <= 20)
## select children data (and parents SES) for the further analysis
df.PSID_child <- df.PSID %>%
  #select only children
  dplyr::filter(role == "child") %>%
  #select variables related to children that are used for further analysis
  dplyr::select(pid, fid, age, sex, pid_father, pid_mother, familysize, fincome, sequence, role, depression, life_satisfaction) %>%
  #combine mother's variable with children
  dplyr::left_join(., df.PSID[df.PSID$role == "mother", c("pid_mother", "edu")], by = "pid_mother")%>%
  #rename mother's variable
  dplyr::rename(edu_m = edu) %>%
  #combine father's variable with children
  dplyr::left_join(., df.PSID[df.PSID$role == "father", c("pid_father", "edu")], by = "pid_father")%>%
  #rename father's variable
  dplyr::rename(edu_f = edu) %>%
  #select one child for every family 
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1)%>%
  dplyr::ungroup()
#check data
summary(df.PSID_child)
#save the data as RData for the further analysis
save(df.PSID_child, file = "df.PSID_child.RData")
# ---------------------------------------------------------------------------------------
# ---------- 3.  Reproduce SES indexes in papers--------------------------------------------
# ---------------------------------------------------------------------------------------

####### Betancourt, L, 2016 (CFPS & PSID)###########
#  SES = (ITN + mother's edu)/2
#  ITN: income-to-need ratio; set 5 level of family income according to poverty line (4 cut-point: itn1 = poverty line, itn4 = 400% above poverty line, rest two set between itn1 and itn4)
#      * in America, poverty line is different for different family size
#  mother's education: see 'Education & Occupation recode.xlsx'

## CFPS ##
betan_CFPS <- df.CFPS_child %>%
  # set the ITN for every individual
  dplyr::mutate(itn = base::cut(finc_per, breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), labels = c("1", "2", "3", "4", "5"))) %>% 
  # set 7 levels for mtoher's education
  dplyr::mutate(edu_m_recode = dplyr::recode(educ_m, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, 
                                      "7" = 2, "8" = 2, "9" = 2, "10" = 2, 
                                      "11" = 3,"12" = 4,"13" = 5, "14" = 6, "15" = 7,"16" = 7)) %>% 
  # convert factors into numeric variable
  dplyr::mutate(itn = as.numeric(as.character(itn)), 
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  # calculate composite SES score
  dplyr::mutate(SES_betan_cfps = (itn + edu_m_recode)/2)  
#check SES score
table(betan_CFPS$SES_betan_cfps)

## PSID ##
betan_PSID <- df.PSID_child %>%
  # set 7 levels for mtoher's education
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.00001, 11.5, 12.5, 13.5, 14.5, 16.5, 98, 100), 
                                   labels = c("1", "2", "3", "4", "5", "6", NA)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  # set the poverty line for every family
  dplyr::mutate(itn1 = 12060 +  (familysize-1)*4180) %>% # poverty line 12060 for one people and increase 4180 for an extra person
  # get the interval of ITN 
  dplyr::mutate(itn = ifelse(fincome < itn1, 1, 
                                 ifelse(itn1 <= fincome & fincome < itn1*2, 2,
                                         ifelse(itn1*2 <= fincome & fincome < itn1*3, 3,
                                                ifelse(itn1*3 <= fincome & fincome < itn1*4, 4, 
                                                       ifelse(itn1*4 <= fincome, 5, 0)))))) %>%
  # calculate composite SES score
  dplyr::mutate(SES_betan_psid = (itn + edu_m_recode)/2) 
#check SES score
table(betan_PSID$SES_betan_psid)

######## Moog, et al., 2008 (CFPS & PSID)######
# SES = (mother's highest education + income)/2
# mother's highest edu: see 'Education & Occupation recode.xlsx'
# income: recode income into 5 levels
##CFPS##
moog_CFPS <- df.CFPS_child %>%
  #recode education
  dplyr::mutate(edu_cat = dplyr::recode_factor(edu2010_t1_best_m, "1" = 1,"2" = 1,"3" = 1,"4" = 2,"5" = 3,"6" = 3,"7" = 4,"8" = 5))  %>% #recode education
  #recode income
  dplyr::mutate(income_cat = base::cut(fincome, 
                               breaks= quantile(fincome, probs = seq(0, 1, 0.2), na.rm= TRUE), 
                               labels = c("1", "2", "3", "4", "5"))) %>% #recode income
  #convert variables into numeric ones
  dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),
                edu_cat = as.numeric(as.character(edu_cat)))%>% #convert income and education into numeric variables
  #calculate composite SES
  dplyr::mutate(SES_moog_cfps = (edu_cat+income_cat)/2)
#check SES score
table(moog_CFPS$SES_moog_cfps)

##PSID##
moog_PSID <- df.PSID_child %>%
  #recode income
  dplyr::mutate(income_cat=cut(fincome, breaks= quantile(fincome, probs = seq(0, 1, 0.2), na.rm= TRUE), labels = c("1", "2","3", "4","5"))) %>%#recode income
  #recode education
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.00001, 11.5, 12.5, 16.5, 98, 100), labels = c("1", "2", "3", "4",  NA))) %>% #cut into 4 groups: <12, 12, 13-16, 17  (?do not distinguish master and phd)
  #convert variables into numeric ones
  dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),
                edu_m_recode = as.numeric(as.character(edu_m_recode)))%>%
  #calculate composite SES
  dplyr::mutate(SES_moog_psid = (income_cat + edu_m_recode)/2)
#check SES score
table(moog_PSID$SES_moog_psid)

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
########## Jednoróg,  2012 (CFPS) ##########
# SES = mother's education * 4 + mother's occupation *7
# mother's education: see 'Education & Occupation recode.xlsx'
# mother's occupation: see 'Education & Occupation recode.xlsx'
# Note: we reversed the score from the original so that the correlation between it and other SES scores are positive
# Note: only reproduced using CFPS data because occupation data is not available from PSID
## CFPS ##
jed_CFPS <- df.CFPS_child %>%
  #recode occupation
  dplyr::mutate(occup_ses = recode(egp_m, "1"= 7, "2"=6, "3"=5,  "7"= 5, "4"=4, "5"= 4, "6"= 4, "8"= 3, "9"=2, "10"=1, "11"=1, "-8"=-8,"80000"= 8, .default = -8)) %>%
  #recode education
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5,4.5,5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  #set NA
  dplyr::na_if(., -8)%>%
  #convert variables into numeric ones
  dplyr::mutate(occup_ses = as.numeric(as.character(occup_ses)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  #calculate composite SES score
  dplyr::mutate(SES_jed_cfps = 4*edu_m_recode + 7*occup_ses) 
#check SES score
table(jed_CFPS$SES_jed_cfps)

############ McDermott, 2019 (CFPS)##########
# SES: (SES_father + SES_mother)/2
# SES_father = father's occupation*5 + father's education*3; 
# SES_mother = mother's occupation*5 + mother's education*3
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: can ONLY be reproduced using CFPS data because of the occupation

##CFPS##
mcder_CFPS <- df.CFPS_child %>%
  # recode education and occupation (mother)
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%
  # recode education and occupation (father)
  dplyr::mutate(edu_f_recode = cut(educ_f, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_f_recode = recode(egp_f, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%
  #set NA
  dplyr::na_if(., -8)%>%
  # convert variables into numeric ones
  dplyr::mutate(occup_f_recode = as.numeric(as.character(occup_f_recode)),
                occup_m_recode = as.numeric(as.character(occup_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  # calculate SES of father and mother
  dplyr::mutate(SES_f = occup_f_recode*5 + edu_f_recode*3,
                SES_m = occup_m_recode*5 + edu_m_recode*3)%>%
  #replace father and mother's ses with each other if one is NA
  dplyr::mutate(SES_f = ifelse(is.na(SES_f), SES_m, SES_f),
                SES_m = ifelse(is.na(SES_m), SES_f, SES_m))%>%
  # calculate composite SES score
  dplyr::mutate(SES_mcder_cfps = (SES_m + SES_f)/2) 
table(mcder_CFPS$SES_mcder_cfps)

############# Romeo, 2018a (CFPS)##################
# SES = mother's education and occupation
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: only reproduced using CFPS data because of the occupation data

romeo1_CFPS <- df.CFPS_child %>%
  #recode education
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  #recode occupation
  dplyr::mutate(occu_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8)) %>%
  #set NA
  dplyr::na_if(., -8) %>%
  # convert variables into numeric ones
  dplyr::mutate(occu_m_recode = as.numeric(as.character(occu_m_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  # calculate composite SES score
  dplyr::mutate(SES_romeo1_cfps = occu_m_recode*5 + edu_m_recode*3)
#check SES score
table(romeo1_CFPS$SES_romeo1_cfps)

############### Romeo, 2018b (CFPS & PSID)############
# SES = (Z_{parents' education} + Z_{family income})/2
# education: recode into 5 levels (0-4) and then calculate z-score of the parents' education

##CFPS
romeo2_CFPS <- df.CFPS_child %>%
  #recode income 
  dplyr::mutate(income_zscore = (fincome - mean(fincome, na.rm = TRUE))/sd(fincome, na.rm = TRUE)) %>%
  #recode parents education into 5 levels and convert into numeric ones
  dplyr::mutate(edu_f_recode = cut(educ_f, breaks = c(-0.5,6.5,11.5,13.5,14.5,16.4), labels = c("1", "2", "3", "4","5"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode))-1) %>% #recode education 0-4
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5,6.5,11.5,13.5,14.5,16.4), labels = c("1", "2", "3", "4","5"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))-1) %>%
  # replace father/mother education if one is missing
  dplyr::mutate(edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode),
                edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode))%>% 
  # calculate composite parents' education score
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2)%>%
  # calculate z-score of parents' education
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%
  #calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_cfps = (edu_zscore + income_zscore)/2)
# check SES score
summary(romeo2_CFPS$SES_romeo2_cfps)

##PSID##
romeo2_PSID <- df.PSID_child %>%
  #recode parents education into 5 levels and convert into numeric ones
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.00001, 11.5, 12.5,  14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5",  "6"))) %>%
  dplyr::mutate(edu_f_recode = cut(edu_f, breaks = c(-0.00001, 11.5, 12.5,  14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5",  "6"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))-1,
                edu_f_recode = as.numeric(as.character(edu_f_recode))-1)%>% #recode as 0-4
  #set NA
  dplyr::mutate(edu_m_recode = na_if(edu_m_recode, 5),
                edu_f_recode = na_if(edu_f_recode, 5)) %>%
  # replace father/mother education if one is missing
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode)) %>%
  # calculate composite parents' education score
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2) %>%
  # calculate z-score of parents' education
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%
  dplyr::mutate(income_zscore = (fincome - mean(fincome, na.rm = TRUE))/sd(fincome, na.rm = TRUE)) %>%
  #calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_psid = (edu_zscore + income_zscore)/2)
#check SES score
summary(romeo2_PSID$SES_romeo2_psid)

########### Qiu, 2017 (CFPS & PSID)############
# SES = household income 

##CFPS##
qiu_CFPS <- df.CFPS_child%>%
  dplyr::rename(SES_qiu_cfps = fincome)
#check SES score
summary(qiu_CFPS$SES_qiu_cfps)

##PSID##
qiu_PSID <- df.PSID_child %>%
  dplyr::select(fid, pid, fincome, age) %>%
  dplyr::rename(SES_qiu_psid = fincome) 
#check SES score
summary(qiu_PSID$SES_qiu_psid)

############## Kim, 2019 (CFPS & PSID)###############
# SES = log10[(family income)/(poverty line)]
# Note: for PSID, family poverty line varies according to family size

##CFPS##
kim_CFPS <- df.CFPS_child %>%
  #calculate INR 
  dplyr::mutate(INR = finc_per/1274)  %>%
  # log transformation
  dplyr::mutate(INR = log10(INR)) %>%
  #set infinite value as NA
  dplyr::mutate(SES_kim_cfps = ifelse(!is.finite(INR), NA, INR))
#check SES score
summary(kim_CFPS$SES_kim_cfps)

##PSID
kim_PSID <- df.PSID_child %>%  
  # calculate poverty line for every family
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%   
  # INR = SES
  dplyr::mutate(SES_kim_psid = fincome/poverty) %>%  
  # log transformation
  dplyr::mutate(SES_kim_psid = log10(SES_kim_psid)) %>%  
  #set infinite value as NA
  dplyr::mutate(SES_kim_psid = ifelse(!is.finite(SES_kim_psid), NA, SES_kim_psid))
#check SES score
summary(kim_PSID$SES_kim_psid)

########################## Hanson, 2013 (CFPS & PSID)################
# SES = family poverty line (cut into 3 levels)

##CFPS##
hanson_CFPS <- df.CFPS_child %>%
  #calculate FPL according to poverty line
  dplyr::mutate(FPL = finc_per/1274) %>% 
  #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>% 
  #convert SES score into numeric
  dplyr::mutate(SES_hanson_cfps = as.numeric(as.character(SES_hanson_cfps)))
#check SES score
table(hanson_CFPS$SES_hanson_cfps)
  
##PSID##
hanson_PSID <- df.PSID_child %>%
  #calculate poverty line for every family
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  #calculate FPL according to poverty line
  dplyr::mutate(FPL = fincome/poverty) %>% 
  #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_psid = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>% 
  #convert SES score into numeric
  dplyr::mutate(SES_hanson_psid = as.numeric(as.character(SES_hanson_psid))) 
#check SES score
table(hanson_PSID$SES_hanson_psid)

#################### Leonard, 2019 (CFPS & PSID) ######################
# SES = maternal education: dichotomous, divided by college

##CFPS##
leo_CFPS <- df.CFPS_child %>%
  #recode education
  dplyr::mutate(SES_leo_cfps = cut(edu2010_t1_best_m, breaks = c(0, 4.5, 8.5), labels = c("1", "2"))) %>%
  #convert it to numeric one
  dplyr::mutate(SES_leo_cfps = as.numeric(as.character(SES_leo_cfps))-1)
#check SES score
table(leo_CFPS$SES_leo_cfps)
  
##PSID##
leo_PSID <- df.PSID_child %>%
  #recode education
  dplyr::mutate(SES_leo_psid = cut(edu_m, breaks = c(-0.01, 12.5, 17.5, 100), labels = c("1", "2", "3"))) %>%
  #convert it to numeric one
  dplyr::mutate(SES_leo_psid = as.numeric(as.character(SES_leo_psid))) %>%
  #set NA
  dplyr::mutate(SES_leo_psid = na_if(SES_leo_psid, 3))  
#check SES score
table(leo_PSID$SES_leo_psid)
  
################### Ozernov-Palchik, O (CFPS & PSID)###########
# SES: father's education (7 levels)*3 + mother's education (7 levels)*3, then cut it as two levels (median as cutting point)
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
  #multiply education score
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode))*3,
                edu_f_recode = as.numeric(as.character(edu_f_recode))*3) %>%
  # composite education score of parents
  dplyr::mutate(edu_parents = edu_m_recode + edu_f_recode) %>%
  #cut composite education score of parents into two levels according to the edian
  dplyr::mutate(SES_ozer_cfps = cut(edu_parents, breaks = c(0, median(edu_parents, na.rm = 10), 100), labels = c('1', '2')))%>%
  #convert SES score into numeric
  dplyr::mutate(SES_ozer_cfps = as.numeric(as.character(SES_ozer_cfps))-1) 
#check SES score
table(ozer_CFPS$edu_parents)

##PSID
ozer_PSID <- df.PSID_child %>%
  #recode education
  dplyr::mutate(edu_f_recode = cut(edu_f, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%
  dplyr::mutate(edu_m_recode = cut(edu_m, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%
  #convert SES score into numeric
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  #set NA
  dplyr::mutate(edu_f_recode = na_if(edu_f_recode, 8),
                edu_m_recode = na_if(edu_m_recode, 8)) %>%
  # replace father with mother and verse versa
  dplyr::mutate(edu_m_recode = ifelse(is.na(edu_m_recode), edu_f_recode, edu_m_recode),
                edu_f_recode = ifelse(is.na(edu_f_recode), edu_m_recode, edu_f_recode)) %>%
  # composite education score of parents
  dplyr::mutate(edu_parents = edu_m_recode*3 + edu_f_recode*3) %>%
  #cut composite education score of parents into two levels according to the edian
  dplyr::mutate(SES_ozer_psid = cut(edu_parents, breaks = c(0, median(edu_parents, na.rm = 10), 100), labels = c('1', '2')))%>%
  #convert SES score into numeric
  dplyr::mutate(SES_ozer_psid = as.numeric(as.character(SES_ozer_psid))) 
#check SES score
table(ozer_PSID$SES_ozer_psid)



# ---------------------------------------------------------------------------------------
# ---------- 4.  extract columns of pid and SES from all the dataframes of SES--------------------------------------------
# ---------------------------------------------------------------------------------------

# save SES score reproduced from CFPS
# extract CFPS data from each dataframe  
names_dataframe_cfps <- list(betan_CFPS, moog_CFPS,
                             jed_CFPS, mcder_CFPS,
                             romeo1_CFPS, romeo2_CFPS, qiu_CFPS, kim_CFPS, 
                             hanson_CFPS, leo_CFPS, ozer_CFPS)
# extract names of paper
names_paper_cfps <- c("betan", "moog","jed", "mcder",
                      "romeo1", "romeo2", "qiu", "kim", 
                      "hanson", "leo", "ozer")
# extract number of papers
N_SES_CFPS <- length(names_paper_cfps)
#create a list to store all the SES scores 
dataframes_cfps <- replicate(N_SES_CFPS, data.frame())
# put each SES score and pid information in each element of the list
for (i in 1:N_SES_CFPS) {
  dataframes_cfps[[i]] <- names_dataframe_cfps[[i]][, c("pid", paste0("SES_", names_paper_cfps[i], "_cfps"))]
  print(dataframes_cfps)
}
dataframes_cfps
#save the list
save(dataframes_cfps, file = 'SES_CFPS.RData')

# save SES score reproduced from PSID
# extract data 
names_dataframe_psid <- list(betan_PSID, moog_PSID,
                             romeo2_PSID, qiu_PSID, kim_PSID, 
                             hanson_PSID, leo_PSID, ozer_PSID)
# extract names of paper
names_paper_psid <- c("betan", "moog", "romeo2", "qiu", "kim", 
                      "hanson", "leo", "ozer")
# extract number of papers
N_SES_PSID <- length(names_paper_psid)

#create a list to store all the SES scores 
dataframes_psid <- replicate(N_SES_PSID, data.frame())

# put each SES score and pid information in each element of the list
for (i in 1:N_SES_PSID) {
  dataframes_psid[[i]] <- names_dataframe_psid[[i]][, c("pid", paste0("SES_", names_paper_psid[i], "_psid"))]
  print(dataframes_psid)
}
dataframes_psid
#save the list
save(dataframes_psid, file = "SES_PSID.RData")
