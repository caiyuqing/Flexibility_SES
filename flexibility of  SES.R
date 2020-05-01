################## data extraction for "what is ses" project ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    20-03-15         The first version
# Hu, C-P     20-04-27         Validate the script
# 
# 
#
###### input######
# CFPS_adult_ses
###### output #####
#
#
#
#
######################## Start of the script ###########################
### clean the memory to avoid unnecessary errors:
rm(list = ls())
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientifc notion
options(digits=5)       # limit the number of reporting

### set directory to the folder of analytic data

# Get the directory of the current R script
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory where this script is 
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")
library("dplyr")
library("tidyverse")
library("foreign")

#import data CFPS
load("CFPS2010.RData")
#load("df.children.RData")
#load("df.individual.RData")
#load("df.community.RData")
#load("df.family.RData")  # load dataframes: df.children, individual , community, family

# PSID
df.psid <- read.spss("PSID_SES&mental health_selected data.sav", to.data.frame = TRUE, use.value.labels = TRUE)
df.psid_proposal <- read.spss("selected for proposal_v1.sav", to.data.frame = TRUE, use.value.labels = TRUE)

## extract familysize_psid
familysize_psid <- data.frame(table(df.psid$ER34501))
names(familysize_psid) <- c("fid", "familysize")  # hcp: using this way, `fid` is 1, 2, 3.....
familysize_psid$fid <- as.numeric(as.character(familysize_psid$fid))

#########################################################################################
#######Betancourt, L, 2016###########
#family income, maternal education
#CFPS
#select children info: fid, pid, pid of mother
betan_child_cfps <- df.children %>%
  dplyr::select(fid, pid, pid_m)
#drop NA
betan_child_cfps[betan_child_cfps == -8] <-NA
betan_child_cfps <- drop_na(betan_child_cfps)

#select family income from family dataframe
betan_family_cfps <- df.family %>%
  dplyr::select(fid, finc_per)
#select maternal education from adult dataframe
betan_mater_cfps <- df.individual %>%
  dplyr::select(pid, educ)
names(betan_mater_cfps) <- c("pid_m", "edu_m")
#extract children's family income and maternal education
betan_child_cfps <- merge(betan_child_cfps, betan_family_cfps, by = "fid", all.x = TRUE)
betan_child_cfps <- merge(betan_child_cfps, betan_mater_cfps, by = "pid_m", all.x = TRUE)
#recode family income: poverty line = 1274 in 2010 (??it is not income to need line)
#recode maternal education: < high school (1); high school/GED (2); Techical/Vocational (3); som college (4); two-year degree (5); four-year degree (6); some graduate school (7); MA, PhD, Prefessional (8).
betan_child_cfps <- betan_child_cfps %>%
  dplyr::mutate(itn = cut(finc_per, breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), labels = c("1", "2", "3", "4", "5"))) %>% #set 5 level of itn according to poverty line (4 cut-point: itn1 = poverty line, itn4 = 400% above poverty line, rest two set between itn1 and itn4)
  dplyr::mutate(edu_m_recode = recode(edu_m, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, 
                                             "7" = 2, "8" = 2, "9" = 2, "10" = 2, 
                                             "11" = 3,"12" = 4,"13" = 5, "14" = 6, "15" = 7,"16" = 7)) %>%  
                                  #??technical/vocational as technical/vocational college; different system
                                  #??CFPS: (1)1=Illiterate 2=Adult primary school/Literacy class 3=Ordinary primary school 4=Adult junior high school 5=Vocational junior high school 6=Ordinary junior high school 
                                  #      (2)7=Ordinary specialized high school/Vocational high school/Technical high school 8=Adult senior high school 9=Specialized adult high school 10=Ordinary senior high school 
                                  #      (3)11=3-year adult college (4)12=Ordinary 3-year college (5)13=4-year adult college (6)14=Ordinary 4-year college (7)15=Master’s degree 16=Doctoral degree (No such level as "some graduate")
  dplyr::mutate(itn = as.numeric(itn),
                edu_m_recode = as.numeric(edu_m_recode)) %>% #convert into numeric variable
  dplyr::mutate(SES_betan_cfps = (itn + edu_m_recode)/2)  #SES as composte of mean of itn and edu level
table(betan_child_cfps$SES_betan_cfps)

#identify mother/father/children in the data
##!!note that dplyr will not keep the right label for variable, change that later
psid_child <- df.psid_proposal %>%
  dplyr::select(ER34503,ER34501,ER30002,ER32000,ER34504) %>% #relation to RP; fid; pid; sex; age
  dplyr::filter(ER34503 ==30) # Relation to the reference person is children-par

psid_father <- df.psid_proposal %>% # extract rp and sp
  # select Relation to the reference person, 2017 interview #, release #, personal #.
  dplyr::select(ER34503,ER34501,ER30002,ER32000)  %>%
  # select relation to the reference person are: 10 - the reference person, or (|), 20-legal spouse of RP
  dplyr::filter(ER34503 == 10 | ER34503 == 20) %>% 
  dplyr::filter(ER32000 == 1) # male

psid_mother <- df.psid_proposal %>% #extract rp and sp
  dplyr::select(ER34503,ER34501,ER30002,ER32000)  %>%
  dplyr::filter(ER34503 == 10 | ER34503 == 20) %>%
  dplyr::filter(ER32000 == 2) #female

names(psid_child) <- c("relation_rp_c", "fid", "pid", "sex", "age")
names(psid_father) <- c("relation_rp_f", "fid", "pid_f", "sex_f")
names(psid_mother) <- c("relation_rp_m", "fid", "pid_m", "sex_m")

#another possible way to identify mother (?)
#psid_mother <- df.psid_proposal %>%
#  dplyr::select(ER66021,ER34503,ER34501,ER30002,ER32000) %>% #children in FU; relation to RP; fid; pid; sex
#  dplyr::filter(ER66021 != 0)  %>% #the family have children
#  dplyr::filter(ER34503 == 10 & 20) %>% #reference person & spouse & paterner
#  dplyr::filter(ER32000 == 2) #female
#  var_labs <- attr(df.psid, "variable.labels")
#  var_labs <- var_labs[names(psid_mother)]
#  attr(psid_mother, "variable.labels") <- var_labs
#recode maternal education: < high school (1); high school/GED (2); Techical/Vocational (3); som college (4); two-year degree (5); four-year degree (6); some graduate school (7); MA, PhD, Prefessional (8).
betan_psid_edu <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(edu_m_recode = cut(ER34548, breaks = c(-0.00001, 11.5, 12.5, 13.5, 14.5, 16.5, 98, 100), 
                                   labels = c("1", "2", "3", "4", "5", "6", NA))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode))
                                                    #1=<high school: <12; 2=high school: 12; 3=some college/technical: 13; 4=two-years: 14; 5=4-years: 15/16; 6=some graduate/MA/Phd: 17 above; NA: 99
betan_psid_edu$edu_m_recode[betan_psid_edu$edu_m_recode == 7]<- NA 
names(betan_psid_edu) <-c("edu_year",  "fid","pid_m", "edu_m_recode")
#recode income: poverty line = 12,060 + (familysize-1)*4180

##extract family income
betan_psid_income <- df.psid_proposal%>%
  dplyr::select(ER71426, ER34501,ER30002) # total family income, fid, pid
names(betan_psid_income) <- c("income", "fid", "pid_m")
##merge income and size
betan_psid_income <- merge(betan_psid_income, familysize_psid, by = "fid")
##recode family income according to poverty lineand family size
betan_psid_income <- betan_psid_income %>%
  dplyr::mutate(itn1 = 12060 +  (familysize-1)*4180) %>% # poverty line 12060 for one people and increase 4180 for an extra person
  dplyr::mutate(itn4 = itn1*4,
                itn3 = itn1*3,
                itn2 = itn1*2) %>% # 4 cut-points
  dplyr::mutate(income_itn1 = income- itn1,
                income_itn2 = income- itn2,
                income_itn3 = income- itn3,
                income_itn4 = income- itn4) %>% #compare family income with cut-points
  dplyr::mutate(income_itn1 = cut(income_itn1, breaks = c(-Inf, 0, Inf), labels = c("0", "1")),
                income_itn2 = cut(income_itn2, breaks = c(-Inf, 0, Inf), labels = c("0", "1")),
                income_itn3 = cut(income_itn3, breaks = c(-Inf, 0, Inf), labels = c("0", "1")),
                income_itn4 = cut(income_itn4, breaks = c(-Inf, 0, Inf), labels = c("0", "1"))) %>% #if income > cut-point, code as 1, else as 0
  dplyr::mutate(income_itn1 = as.numeric(income_itn1)-1,
                income_itn2 = as.numeric(income_itn2)-1,
                income_itn3 = as.numeric(income_itn3)-1,
                income_itn4 = as.numeric(income_itn4)-1)%>% #convert to numeric and minus 1 to keep 0 and 1 coding
  dplyr::mutate(itn = income_itn1 + income_itn2 + income_itn3 + income_itn4+1) # add 4 cut-point (e.g. four 0 means below poverty line: level 1)

#merge education and income
betan_psid <- merge(psid_mother, betan_psid_edu, by = c("fid", "pid_m"), all.x = TRUE)
betan_psid <- merge(betan_psid, betan_psid_income, by = c("fid", "pid_m"), all.x = TRUE)
#calculate SES 
betan_psid <- betan_psid %>%
  dplyr::mutate(itn = as.numeric(itn),
                edu_m_recode = as.numeric(edu_m_recode))%>%
  dplyr::mutate(SES_betan_psid = (itn + edu_m_recode)/2)
#select children
betan_child_psid <- merge(psid_child, betan_psid,  by = "fid", all.x = TRUE) 
table(betan_child_psid$SES_betan_psid)
#########################################################################################
#######Moog, 2008######
#cfps
#income and education
moog_child_cfps <- df.children %>%
  dplyr::select(pid, fid, pid_m)
moog_child_cfps[moog_child_cfps == -8]  <-NA
#recode income into 5 groups: quantiles
#recode education into five groups: <high school, high school, bachelor, master, docterate
#extract family income
moog_cfps_income <-  df.family  %>%
  dplyr::select(fid, fincome) #?do not consider family size
#set NA
moog_cfps_income$fincome[moog_cfps_income$fincome == -8]<-NA
#extract education
moog_cfps_education <- df.individual %>%
  dplyr::select(pid, edu2010_t1_best)
table(moog_cfps_education$edu2010_t1_best) #check education
#rename education
names(moog_cfps_education) <- c("pid_m", "edu2010_t1_best")
#merge income and education
moog_child_cfps <- merge(moog_child_cfps, moog_cfps_income, by ="fid", all.x = TRUE)
moog_child_cfps <- merge(moog_child_cfps, moog_cfps_education, by = "pid_m", all.x = TRUE)
#recode education, income and calculate composite SES
moog_child_cfps <-moog_child_cfps %>%
  dplyr::mutate(edu_cat = recode_factor(edu2010_t1_best, "1" = 1,"2" = 1,"3" = 1,"4" = 2,"5" = 3,"6" = 3,"7" = 4,"8" = 5))  %>% #recode education
  dplyr::mutate(income_cat=cut(fincome, breaks= quantile(moog_cfps_income$fincome, probs = seq(0, 1, 0.2), na.rm= TRUE), labels = c("1", "2", "3", "4", "5"))) %>% #recode income
  dplyr::mutate(income_cat = as.numeric(income_cat),
                edu_cat = as.numeric((edu_cat)))%>% #convert income and education into numeric variables
  dplyr::mutate(SES_moog_cfps = (edu_cat+income_cat)/2) #compoite SES: mean of education and income
table(moog_child_cfps$SES_moog_cfps)
#psid
psid_mother #from before
#education recode
moog_psid_edu <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(edu_m_recode = cut(ER34548, breaks = c(-0.00001, 11.5, 12.5, 16.5, 98, 100), labels = c("1", "2", "3", "4",  NA))) %>% #cut into 4 groups: <12, 12, 13-16, 17  (?do not distinguish master and phd)
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode))
#set NA
moog_psid_edu$edu_m_recode[moog_psid_edu$edu_m_recode == 7]<- NA 
#rename variables
names(moog_psid_edu) <-c("edu_year",  "fid","pid_m", "edu_m_recode")
#income recode
moog_psid_income <- df.psid_proposal  %>%
  dplyr::select(ER71426, ER34501,ER30002) #total family income, fid, pid
names(moog_psid_income) <- c("income", "fid", "pid_m")
moog_psid_income <- moog_psid_income%>%
  dplyr::mutate(income_cat=cut(income, breaks= quantile(moog_psid_income$income, probs = seq(0, 1, 0.2), na.rm= TRUE), labels = c("1", "2","3", "4","5")))#recode income
#merge income and eduation
moog_psid <- merge(psid_mother, moog_psid_edu, by = c("pid_m", "fid"), all.x = TRUE)
moog_psid <- merge(moog_psid, moog_psid_income, by  =  c("pid_m", "fid"), all.x = TRUE)
#calculate composite SES
moog_psid <- moog_psid %>%
  dplyr::mutate(income_cat = as.numeric(income_cat),
                edu_m_recode = as.numeric(edu_m_recode))%>%
  dplyr::mutate(SES_moog_psid = (income_cat + edu_m_recode)/2)
moog_child_psid <- merge(moog_psid, psid_child, by = "fid") #select children
table(moog_child_psid$SES_moog_psid)
#########################################################################################
####### Yu,2018, not figured out yet ######
# cfps
# reproduce the second sample (young adult)
# SSS, ITN, education 
df.cfps_roster <- read.csv("./data/2010familyroster.csv", header = TRUE, fileEncoding="UTF-8-BOM")
yu_yadult_cfps <- df.cfps_roster %>%
  dplyr::select(pid, pid_m, pid_f, fid, tb1b_a_p) %>%#pid, pid_m, pid_f, fid, age
  dplyr::filter(tb1b_a_p <= 25 &tb1b_a_p >= 18) 
yu_yadult_cfps
yu_yadult_cfps[yu_yadult_cfps == -8] <-NA
#recode family income into ITN (poverty line in 2010: 1274)
yu_cfps_income <- df.family %>%
  dplyr::select(fid, finc_per) %>%
  dplyr::mutate(income_cat = cut(finc_per, 
                                 breaks = quantile(finc_per, probs = seq(0, 1, 1/9), na.rm= TRUE),
                                 labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")))%>% #recode family income into 9 groups
  dplyr::mutate(income_cat = as.numeric(income_cat)) #convert into numeric variable
group_ITN <- yu_cfps_income %>% 
  dplyr::group_by(income_cat) %>% #group income into 9 groups
  dplyr::summarise(group_median = median(finc_per))%>% #calculate median of each group
  dplyr::mutate(ITN = group_median/1274) #calculate ITN: divide poverty line 1274
yu_cfps_income <- merge(yu_cfps_income, group_ITN, by= "income_cat") #merge ITN with main data frame
table(yu_cfps_income$ITN) #check ITN
#recode father education and mother education: 7 level
#father education
yu_cfps_fedu <- df.individual %>%
  dplyr::select(pid, edu2010_t1_best) %>% #1=illiterate 2=primary 3=junior high 4=senior high 5=3-year college 6=4-year college 7=master 8=doctoral
  dplyr::mutate(edu_f_recode = recode(edu2010_t1_best, "1" = 1,"2" = 2,"3" = 2,"4" = 3,"5" = 4,"6" = 5,"7" = 6,"8" = 7)) #combine 2, 3 into one level
names(yu_cfps_fedu) <- c("pid_f", "edu2010_t1_best_f", "edu_f_recode")
#mother education 
yu_cfps_medu <- df.individual %>%
  dplyr::select(pid, edu2010_t1_best) %>% #1=illiterate 2=primary 3=junior high 4=senior high 5=3-year college 6=4-year college 7=master 8=doctoral
  dplyr::mutate(edu_m_recode = recode(edu2010_t1_best, "1" = 1,"2" = 2,"3" = 2,"4" = 3,"5" = 4,"6" = 5,"7" = 6,"8" = 7)) #combine 2, 3 into one level
names(yu_cfps_medu) <- c("pid_m", "edu2010_t1_best_m", "edu_m_recode")
# SSS
yu_cfps_sss <- df.individual %>%
  dplyr::select(pid, qm402) #pid, subjective social status
names(yu_cfps_sss) <-  c("pid", "sss")
yu_cfps_sss[yu_cfps_sss < 0] <- NA

# merge income, father education, mother education and sss
yu_cfps <- merge(yu_yadult_cfps, yu_cfps_income, by = "fid")
yu_cfps  <- merge(yu_cfps, yu_cfps_fedu, by = "pid_f")  
yu_cfps <- merge(yu_cfps, yu_cfps_medu, by = "pid_m")
yu_cfps <- merge(yu_cfps, yu_cfps_sss, by = "pid")

# PCA
# install.packages("FactoMineR")
# install.packages("factoextra")
library(FactoMineR)
library(factoextra)
yu_pca_cfps <- yu_cfps %>%
  dplyr::select(ITN, edu_f_recode, edu_m_recode, sss)
yu_pca_cfps <- drop_na(yu_pca_cfps)
yu.pr_cfps<-PCA(yu_pca_cfps, scale.unit = TRUE, graph = TRUE)
get_eigenvalue(yu.pr_cfps)
fviz_eig(yu.pr_cfps)#scree plot
var <- get_pca_var(yu.pr_cfps) #sss shows opposite contribution (?)
var$coord

# calculate SES as first component (?PCA does not appear to be single component)
yu_cfps <- yu_cfps %>%
  dplyr::mutate(SES_yu_cfps = 0.71118313*ITN + 0.78339583*edu_f_recode + 0.81539907*edu_m_recode + (-0.04682041)*sss)

# psid
# psid:only rp and sp have sss and parental education info
# extract sp and rp 
# reproduce Yu,2018 second young adult version first
# subject here is rp and sp
# import more data (education related)
df.psid_supp_edu <- read.spss("supplement_data_education.sav", to.data.frame = TRUE, use.value.labels = TRUE)
yu_yadult_psid_rp <- df.psid_proposal %>%
  dplyr::select(ER34503,ER34501,ER30002,ER32000,ER34504) %>% #relation to RP; fid; pid; sex;age
  dplyr::filter(ER34503 == 10) %>%
  dplyr::filter(ER34504 <= 25 &ER34504 >= 18)
yu_yadult_psid_sp <- df.psid_proposal %>%
  dplyr::select(ER34503,ER34501,ER30002,ER32000,ER34504) %>% #relation to RP; fid; pid; sex;age
  dplyr::filter(ER34503 == 20) %>%
  dplyr::filter(ER34504 <= 25 &ER34504 >= 18)
names(yu_yadult_psid_rp)  <- c("relation_rp", "fid", "pid", "sex", "age")
names(yu_yadult_psid_sp)  <- c("relation_rp", "fid", "pid", "sex", "age")

# recode income
yu_psid_income <- df.psid_proposal %>%
  dplyr::select(ER34501,ER30002, ER71426)  %>% #fid, pid, total family income
  dplyr::mutate(income_cat = cut(ER71426, 
                                 breaks = quantile(df.psid_proposal$ER71426, probs = seq(0, 1, 1/9), na.rm= TRUE),
                                 labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")))%>% #recode family income into 9 groups
  dplyr::mutate(income_cat = as.numeric(income_cat)) #convert into numeric variable
names(yu_psid_income) <- c("fid", "pid", "fincome", "income_cat")
yu_psid_income <- merge(yu_psid_income, familysize_psid, by = "fid")
group_median <- yu_psid_income %>% 
  dplyr::group_by(income_cat) %>% #group income into 9 groups
  dplyr::summarise(group_median = median(fincome))
yu_psid_income <- merge(yu_psid_income, group_median, by= "income_cat") #merge income median with main data frame
yu_psid_income <- yu_psid_income %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  #calculate poverty line according to family size
  dplyr::mutate(ITN = group_median/poverty) #calculate ITN: group_median divide poverty line
table(yu_psid_income$ITN) #check ITN

# parents' education
yu_psid_edu <- df.psid_supp_edu %>%
  dplyr::select(ER34501,ER30002,TA171981,TA171983) %>% #fid, pid, mother_edu, father_edu
  dplyr::mutate(edu_m_recode = cut(TA171981, breaks = c(-0.001, 0.5, 11.5, 12.5, 13.5, 16.5,20), labels = c("1", "2", "3", "4", "5", "6")),
                edu_f_recode = cut(TA171983, breaks = c(-0.001, 0.5, 11.5, 12.5, 13.5, 16.5,20), labels = c("1", "2", "3", "4", "5", "6"))) %>% #cut education into 6 groups: Noe of below (1):0; < high school (2):1-10; High scool (3):11-12; associate degree (4):13-14; bachelor's degree (5):14-16; Master's degree (6)&PhD/MD (7):17
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode),
                edu_f_recode = as.numeric(edu_f_recode))
names(yu_psid_edu) <- c("fid", "pid", "mother_edu", "father_edu", "edu_m_recode", "edu_f_recode")

# sss
yu_psid_sss_rp <- df.psid_proposal%>%
  dplyr::select(ER34501,ER30002,ER70879) # fid,pid, sss_rp
yu_psid_sss_sp <- df.psid_proposal%>%
  dplyr::select(ER34501,ER30002,ER70741) # fid,pid, sss_sp
names(yu_psid_sss_rp) <- c("fid", "pid", "sss")
names(yu_psid_sss_sp) <- c("fid", "pid", "sss")

# merge
# merge rp with rp sss
yu_yadult_psid_rp <-  merge(yu_yadult_psid_rp, yu_psid_sss_rp, by = c("fid", "pid"))
yu_yadult_psid_sp <-  merge(yu_yadult_psid_sp, yu_psid_sss_sp, by = c("fid", "pid"))
#combine rp with sp
yu_yadult_psid <- rbind(yu_yadult_psid_rp, yu_yadult_psid_sp)

# merge with education and income
yu_yadult_psid <- merge(yu_yadult_psid,  yu_psid_income, by = c("fid", "pid"))
yu_yadult_psid <- merge(yu_yadult_psid, yu_psid_edu, by = c("fid", "pid"))

# PCA
yu_pca_psid <- yu_yadult_psid %>%
  dplyr::select(ITN, edu_f_recode, edu_m_recode, sss)
yu_pca_psid <- drop_na(yu_pca_psid)
yu.pr_psid<-PCA(yu_pca_psid, scale.unit = TRUE, graph = TRUE)
get_eigenvalue(yu.pr_psid)
fviz_eig(yu.pr_pid)#scree plot
var <- get_pca_var(yu.pr_psid)
var$coord
# calculate SES as first component (?PCA does not appear to be single component)
yu_yadult_psid <- yu_yadult_psid %>%
  dplyr::mutate(SES_yu_psid = 0.5152558*ITN + 0.7833663*edu_f_recode + 0.81539907*edu_m_recode + (-0.04682041)*sss)

##################################### Jednoróg,  2012###############################################
# Jednoróg,  2012 (!reversed from the orginal)
# cfps
jed_child_cfps <- df.children  %>%
  dplyr::select(pid, pid_m)
jed_child_cfps[jed_child_cfps == -8] <-NA
# recode education with eduy(0-22)
jed_cfps_edu <- df.individual  %>%
  dplyr::select(pid, educ) %>%
  dplyr::mutate(edu_m_recode = cut(educ, breaks = c(-0.5,4.5,5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode))
names(jed_cfps_edu) <- c("pid_m", "eduy", "edu_m_recode")
table(jed_cfps_edu$edu_m_recode)

# Recode occupation:
## CFPS
# 1= Higher controllers; 2=Lower controllers; 3=Routine nonmanual; 
# 4=Self-employed with employees; 5=Self-employed without employees; 7=Manual supervisor(?)
# 8=Skilled manual; 9=Semi-unskilld manual; 10=Agricultural laborers; 11= Self-employed agricultural workers; 
# Jednoróg:
# 1, High executitve, major profession, etc.(1); 2, business manager, etc(2); 3, administrative personnel, etc.(3,7); 
# 4, clerical and sales, technician, etc.(4,5,6); 5, skilled manual(8); 6, machine operator, semi-skilled(9); 
# 7, unskilled(10,11); 8,Never employed (-8,80000? -8=not applicable; ; 80000=no occupation); NA (-7,-2,-1,70000, 90000)
jed_cfps_occup <- df.individual %>%
  dplyr::select(pid, qg307egp) %>%
  dplyr::mutate(occup_ses = recode(qg307egp, "1"= 7, "2"=6, "3"=5,  "7"= 5, "4"=4, "5"= 4, "6"= 4, "8"= 3, "9"=2, "10"=1, "11"=1, "-8"=-8,"80000"= -8, .default = -8))
jed_cfps_occup$occup_ses[jed_cfps_occup$occup_ses <0] <- NA

# merge income and occupation
names(jed_cfps_occup) <- c("pid_m", "egp", "occup_ses")
table(jed_cfps_occup$occup_ses)

# merge income and occupation
jed_cfps <- merge(jed_child_cfps, jed_cfps_edu, by = "pid_m", all.x = TRUE)
jed_cfps <- merge(jed_cfps, jed_cfps_occup, by = "pid_m", all.x = TRUE)

# composite ses: Maternal Edu (weighted 4) + Occup (weight 7)
jed_cfps <- jed_cfps %>%
  dplyr::mutate(SES_jed_cfps = 4*edu_m_recode + 7*occup_ses) 
table(jed_cfps$SES_jed_cfps)
# psid?? no classification of occupation according to prestige

#################################### McDermott, 2019 ###############################################
# “higher SES” refers to a lower Hollingshead score.
# CFPS:
# children
mcder_child_cfps <- df.children %>%
  dplyr::select(pid, pid_m,pid_f)
mcder_child_cfps[mcder_child_cfps == -8] <- NA
# education(1-7) and occupation(1-9) of parents
# father
mcder_father_edu_occu <- df.individual %>%
  dplyr::select(pid, educ, qg307egp) %>%
  dplyr::mutate(edu_f_recode = cut(educ, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_f_ses = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, "-8"=0,"80000"= 0, .default = -8))
mcder_father_edu_occu[mcder_father_edu_occu == -8] <- NA
names(mcder_father_edu_occu) <- c("pid_f", "educ_f", "egp_f", "edu_f_recode", "occu_f_recode")

# mother
mcder_mother_edu_occu <- df.individual %>%
  dplyr::select(pid, educ, qg307egp) %>%
  dplyr::mutate(edu_m_recode = cut(educ, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_m_ses = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, "-8"=0,"80000"= 0, .default = -8))
mcder_mother_edu_occu[mcder_mother_edu_occu == -8] <- NA
names(mcder_mother_edu_occu) <- c("pid_m", "educ_m", "egp_m", "edu_m_recode", "occu_m_recode")

# combine with children
mcder_child_cfps <- merge(mcder_child_cfps, mcder_father_edu_occu, by = "pid_f", all.x = TRUE)
mcder_child_cfps <- merge(mcder_child_cfps, mcder_mother_edu_occu, by = "pid_m", all.x = TRUE)

# composite ses (occupation x 5, education x 3)
mcder_child_cfps <- mcder_child_cfps %>%
  dplyr::mutate(occu_f_recode = as.numeric(occu_f_recode),
                occu_m_recode = as.numeric(occu_m_recode),
                edu_f_recode = as.numeric(edu_f_recode),
                edu_m_recode = as.numeric(edu_m_recode)) %>%
  dplyr::mutate(SES_f = occu_f_recode*5 + edu_f_recode*3,
                SES_m = occu_m_recode*5 + edu_m_recode*3)
mcder_child_cfps$SES_f[is.na(mcder_child_cfps$SES_f)] <- mcder_child_cfps$SES_m[is.na(mcder_child_cfps$SES_f)]# replace NA father ses with mother ses
mcder_child_cfps$SES_m[is.na(mcder_child_cfps$SES_m)] <- mcder_child_cfps$SES_f[is.na(mcder_child_cfps$SES_m)]# replace NA mother ses with father ses

# calculate children's SES by a mean of parent's SES
mcder_child_cfps <- mcder_child_cfps %>%
  dplyr::mutate(SES_mcder_cfps = (SES_m + SES_f)/2)
summary(mcder_child_cfps)
table(mcder_child_cfps$SES_mcder_cfps)

# PSID: not applicable(no occupation)

#################################### Romeo, 2018a ###############################################
# “higher SES” refers to a lower Hollingshead score.
# CFPS:
# children
romeo1_child_cfps <- df.children %>%
  dplyr::select(pid, pid_m)
romeo1_child_cfps[romeo1_child_cfps == -8] <- NA

# education(1-7) and occupation(1-9) of mother
# mother
romeo1_mother_edu_occu <- df.individual %>%
  dplyr::select(pid, educ, qg307egp) %>%
  dplyr::mutate(edu_m_recode = cut(educ, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(occup_m_ses = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8))
romeo1_mother_edu_occu[romeo1_mother_edu_occu == -8] <- NA
names(romeo1_mother_edu_occu) <- c("pid_m", "educ_m", "egp_m", "edu_m_recode", "occu_m_recode")

# combine with children
romeo1_child_cfps <- merge(romeo1_child_cfps, romeo1_mother_edu_occu, by = "pid_m", all.x = TRUE)

# composite ses (occupation x 5, education x 3)
romeo1_child_cfps <- romeo1_child_cfps %>%
  dplyr::mutate(occu_m_recode = as.numeric(occu_m_recode),
                edu_m_recode = as.numeric(edu_m_recode)) %>%
  dplyr::mutate(SES_romeo1_cfps = occu_m_recode*5 + edu_m_recode*3)
table(romeo1_child_cfps$SES_romeo1_cfps)

#################################### Romeo, 2018b ###############################################
# CFPS
# children
romeo2_child_cfps <- df.children %>%
  dplyr::select(pid, pid_m, pid_f, fid)
romeo2_child_cfps[romeo2_child_cfps == -8] <- NA
#education parents
romeo2_father_edu_cfps <- df.individual %>%
  dplyr::select(pid, educ) %>%
  dplyr::mutate(edu_f_recode = cut(educ, breaks = c(-0.5,6.5,11.5,13.5,14.5,16.4), labels = c("1", "2", "3", "4","5"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(edu_f_recode)-1) #recode education 0-4
romeo2_mother_edu_cfps <- df.individual %>%
  dplyr::select(pid, educ) %>%
  dplyr::mutate(edu_m_recode = cut(educ, breaks = c(-0.5,6.5,11.5,13.5,14.5,16.4), labels = c("1", "2", "3", "4","5"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode)-1) #recode education 0-4
names(romeo2_father_edu_cfps) <- c("pid_f", "educ_f", "edu_f_recode")
names(romeo2_mother_edu_cfps) <- c("pid_m", "educ_m", "edu_m_recode")

# family
romeo2_income <- df.family %>%
  dplyr::select(fincome, fid) %>% #family income (did not consider family size)
  dplyr::mutate(income_zscore = (fincome - mean(fincome, na.rm = TRUE))/sd(fincome, na.rm = TRUE))

# combine education and income
romeo2_child_cfps <- merge(romeo2_child_cfps, romeo2_father_edu_cfps, by = "pid_f", all.x = TRUE)
romeo2_child_cfps <- merge(romeo2_child_cfps, romeo2_mother_edu_cfps, by = "pid_m", all.X = TRUE)
romeo2_child_cfps <- merge(romeo2_child_cfps, romeo2_income, by = "fid")

# replace father/mother education if one is missing
romeo2_child_cfps$edu_f_recode[is.na(romeo2_child_cfps$edu_f_recode)] <- romeo2_child_cfps$edu_m_recode[is.na(romeo2_child_cfps$edu_f_recode)]#replace NA father ses with mother ses
romeo2_child_cfps$edu_m_recode[is.na(romeo2_child_cfps$edu_m_recode)] <- romeo2_child_cfps$edu_f_recode[is.na(romeo2_child_cfps$edu_m_recode)]#replace NA mother ses with father ses

# combine parents education and z-score it
romeo2_child_cfps <- romeo2_child_cfps%>%
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2)%>%
  dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%
  #calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_cfps = (edu_zscore + income_zscore)/2)
table(romeo2_child_cfps$SES_romeo2_cfps)

# psid
# extract father and mother
# psid_father
# psid_mother
# education parents

# father
romeo2_father_edu_psid <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(edu_f_recode = cut(ER34548, breaks = c(-0.00001, 11.5, 12.5,  14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5",  "6"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(edu_f_recode)-1) #recode as 0-4
table(romeo2_father_edu_psid$edu_f_recode)
# 0 = less than high school; 1=high school; 2=some college/associate’s degree; 3=bachelor’s degree; 4=advanced degree.
romeo2_father_edu_psid$edu_f_recode[romeo2_father_edu_psid$edu_f_recode == 5]<- NA 
names(romeo2_father_edu_psid) <-c("edu_year_f",  "fid","pid_f", "edu_f_recode")

# mother 
romeo2_mother_edu_psid <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% # education, fid, pid
  dplyr::mutate(edu_m_recode = cut(ER34548, breaks = c(-0.00001, 11.5, 12.5,  14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5",  "6"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode)-1) #recode as 0-4
table(romeo2_mother_edu_psid$edu_m_recode)  # hcp: here the results is exactly the same as romeo2_father_edu_psid)
#0 = less than high school; 1=high school; 2=some college/associate’s degree; 3=bachelor’s degree; 4=advanced degree.

romeo2_mother_edu_psid$edu_m_recode[romeo2_mother_edu_psid$edu_m_recode == 5]<- NA 
names(romeo2_mother_edu_psid) <-c("edu_year_m",  "fid","pid_m", "edu_m_recode")

# family income
# extract family income
romeo2_psid_income <- df.psid_proposal%>%
  dplyr::select(ER71426, ER34501) #total family income, fid, pid
names(romeo2_psid_income) <- c("income", "fid")

## recode family income into zscore
romeo2_psid_income <- romeo2_psid_income %>%
  dplyr::mutate(income_zscore = (income - mean(income, na.rm = TRUE))/sd(income, na.rm = TRUE))

# merge education and income
romeo2_father_psid <- merge(psid_father, romeo2_father_edu_psid, by = c("fid", "pid_f"), all.x = TRUE)
romeo2_mother_psid <- merge(psid_mother, romeo2_mother_edu_psid, by = c("fid", "pid_m"), all.x = TRUE)
romeo2_psid <- merge(romeo2_father_psid, romeo2_mother_psid, by = "fid", all.x = TRUE, all.y = TRUE)
romeo2_psid <- merge(romeo2_psid, romeo2_psid_income, by = "fid", all.x = TRUE)

# select children
romeo2_child_psid <- left_join(psid_child, romeo2_psid, by = "fid") #select children

# delet repetitive data
romeo2_child_psid <- romeo2_child_psid %>%
  dplyr::group_by(pid,fid) %>%
  dplyr::filter(row_number() ==1)

# calculate education z-score
# replace father/mother education if one is missing
romeo2_child_psid$edu_f_recode[is.na(romeo2_child_psid$edu_f_recode)] <- romeo2_child_psid$edu_m_recode[is.na(romeo2_child_psid$edu_f_recode)]#replace NA father ses with mother ses
romeo2_child_psid$edu_m_recode[is.na(romeo2_child_psid$edu_m_recode)] <- romeo2_child_psid$edu_f_recode[is.na(romeo2_child_psid$edu_m_recode)]#replace NA mother ses with father ses
romeo2_child_psid$edu_zscore

# combine parents education and z-score it
romeo2_child_psid <- romeo2_child_psid %>%
  dplyr::mutate(edu_parents = (edu_f_recode + edu_m_recode)/2) 

romeo2_child_psid$edu_z <- scale(romeo2_child_psid$edu_parents)
# calculate mean and sd for z-score
mean(romeo2_child_psid$edu_parents, na.rm = TRUE) # 1.8778
sd(romeo2_child_psid$edu_parents, na.rm = TRUE)  # 1.1473

romeo2_child_psid$edu_zscore
romeo2_child_psid <- romeo2_child_psid %>%
  dplyr::mutate(edu_zscore = (edu_parents - 1.8778)/1.1473) %>%
  #calculate mean of edu and income (ses)
  dplyr::mutate(SES_romeo2_psid = (edu_zscore + income_zscore)/2)
romeo2_child_psid$SES_romeo2_psid

#################################### Qiu,  2017################################
# household income as SES
# cfps:
qiu_child_cfps <- df.children %>%
  dplyr::select(pid, fid)
qiu_child_cfps[qiu_child_cfps == -8] <- NA
qiu_income_cfps <- df.family  %>%
  dplyr::select(fid, fincome)
qiu_child_cfps <- merge(qiu_child_cfps, qiu_income_cfps, by = "fid")
names(qiu_child_cfps) <- c("fid", "pid", "SES_qiu_cfps")
summary(qiu_child_cfps$SES_qiu_cfps)

# psid:
qiu_income_psid <- df.psid_proposal%>%
  dplyr::select(ER34501,ER30002, ER71426) #fid, pid, total family income
names(qiu_income_psid) <- c("fid", "pid", "SES_qiu_psid")
qiu_child_psid <- merge(psid_child, qiu_income_psid, by = c("fid", "pid"), all.x = TRUE)
summary(qiu_child_psid$SES_qiu_psid)

#################################### Kim, 2019 ##################################
# INR poverty as SES
# cfps:
kim_child_cfps <- df.children %>%
  dplyr::select(pid, fid)
kim_child_cfps[kim_child_cfps== -8]<- NA
kim_income_cfps <- df.family  %>%
  dplyr::select(fid, finc_per) %>%
  dplyr::mutate(INR = finc_per/1274)  %>%
  dplyr::mutate(INR = log10(INR))
kim_child_cfps <- merge(kim_child_cfps, kim_income_cfps, by = "fid", all.x = TRUE)
names(kim_child_cfps) <- c("fid", "pid","income_per","SES_kim_cfps")
kim_child_cfps$SES_kim_cfps[!is.finite(kim_child_cfps$SES_kim_cfps)] <-NA #set inf as NA for further correlation analysis
summary(kim_child_cfps)

# psid:
# familysize_psid
## extract familysize_psid
familysize_psid <- df.psid %>%
  dplyr::select(ER34501) %>%
  dplyr::group_by(ER34501) %>%
  dplyr::tally() %>%
  #data.frame(table(.))
  dplyr::rename(fid = ER34501,
               familysize = n)  # hcp: using this way, `fid` is 1, 2, 3.....

kim_income_psid <- df.psid_proposal%>%
  dplyr::select(ER34501,ER30002, ER71426) %>%
  dplyr::rename(fid = ER34501,
                pid = ER30002,
                fincome = ER71426) %>% #fid, pid, total family income
  dplyr::left_join(., familysize_psid, by = "fid", all.x = TRUE) %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>% # calculate poverty line for every family
  dplyr::mutate(SES_kim_psid = fincome/poverty) %>%          # INR = SES
  dplyr::mutate(SES_kim_psid = log10(SES_kim_psid))         # log transformation
kim_income_psid$poverty
summary(kim_child_psid)
head(kim_income_psid)

kim_child_psid <- merge(psid_child, kim_income_psid, by = c("fid", "pid"), all.x = TRUE)
kim_child_psid$SES_kim_psid[!is.finite(kim_child_psid$SES_kim_psid)] <-NA #set inf as NA for further correlation analysis
summary(kim_child_psid$SES_kim_psid)
summary(kim_child_cfps$SES_kim_cfps)
########################## Hanson, 2013 ################
# CFPS
# select children
hanson_child_cfps <- df.children %>%
  dplyr::select(fid, pid)
hanson_child_cfps[hanson_child_cfps == -8] <-NA
# select income
hanson_income_cfps <- df.family %>%
  dplyr::select(finc_per, fid) %>%
  dplyr::mutate(FPL = finc_per/1274) %>% #calculate FPL according to poverty line
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>% #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps = as.numeric(SES_hanson_cfps))

hanson_child_cfps <- merge(hanson_child_cfps,  hanson_income_cfps,  by = "fid", all.x = TRUE)
table(hanson_child_cfps$SES_hanson_cfps)

# PSID
familysize_psid
hanson_income_psid <- df.psid_proposal%>%
  dplyr::select(ER34501, ER30002, ER71426) #fid,pid, total family income
names(hanson_income_psid) <- c("fid","pid", "fincome")
hanson_income_psid <- merge(hanson_income_psid, familysize_psid, by = "fid", all.x = TRUE)
hanson_income_psid  <- hanson_income_psid %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%#calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty) %>% #calculate FPL according to poverty line
  dplyr::mutate(SES_hanson_psid = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>% #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_psid = as.numeric(SES_hanson_psid)) 
hanson_child_psid <- merge(psid_child, hanson_income_psid, by= c("fid", "pid"), all.x = TRUE)

table(hanson_child_cfps$SES_hanson_cfps)
table(hanson_child_psid$SES_hanson_psid)
####################Leonard, 2019######################
#maternal education: dichchotomous, divided by college
#CFPS
#extract children
leo_child_cfps <- df.children %>%
  dplyr::select(pid, pid_m)
leo_child_cfps[leo_child_cfps ==-8] <- NA
# extract mother's education and recode
leo_education_cfps <- df.individual %>%
  dplyr::select(pid, edu2010_t1_best) %>%
  dplyr::mutate(SES_leo_cfps = cut(edu2010_t1_best, breaks = c(0, 4.5, 8.5), labels = c("1", "2"))) %>%
  dplyr::mutate(SES_leo_cfps = as.numeric(SES_leo_cfps)-1)
summary(leo_education_cfps)
#rename mother's education
names(leo_education_cfps) <- c("pid_m", "edu2010_t1_best", "SES_leo_cfps")
#merge mother's education and children
leo_child_cfps <- merge(leo_child_cfps, leo_education_cfps, by = "pid_m", all.x = TRUE)
summary(leo_child_cfps)
#leo_child_cfps$SES_leo_cfps

# psid
# extract mother and child from psid
head(psid_mother)
# psid_child
# education
leo_education_psid <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(SES_leo_psid = cut(ER34548, breaks = c(-0.01, 12.5, 17.5, 100), labels = c("1", "2", "3"))) %>%
  dplyr::mutate(SES_leo_psid = as.numeric(SES_leo_psid))
leo_education_psid$SES_leo_psid[leo_education_psid$SES_leo_psid ==3] <- NA
names(leo_education_psid) <- c("edu_m_raw","fid", "pid_m", "SES_leo_psid")

# extract mother from education data
leo_edu_mother_pid <- merge(leo_education_psid, psid_mother, by = c("fid", "pid_m"), all.y= TRUE)

# merge mother's education and child
leo_child_psid <- merge(psid_child, leo_edu_mother_pid, by = "fid", all.x = TRUE)
summary(leo_child_psid)

#################################### Ozernov-Palchik, O################################
# same as romeo(a)--Barratt Simplified Measure of Social Status (BSMSS) 3-21(1-7 multiply 3)
# CFPS
# extract children
ozer_child_cfps <- df.children %>%
  dplyr::select(pid, pid_m, pid_f)
ozer_child_cfps[ozer_child_cfps == -8] <- NA

# education
ozer_education_cfps <- df.individual %>%
  dplyr::select(pid, educ) %>%
  dplyr::mutate(edu_bsmss = cut(educ, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_bsmss = as.numeric(edu_bsmss)*3)
# mother's education & father's education
head(ozer_education_cfps)
ozer_edu_mother_cfps<- ozer_education_cfps
ozer_edu_father_cfps<- ozer_education_cfps
names(ozer_edu_mother_cfps) <- c("pid_m","educ_m", "edu_m_recode")
names(ozer_edu_father_cfps) <- c("pid_f","educ_f", "edu_f_recode")

# merge together with children
ozer_child_cfps <- merge(ozer_child_cfps, ozer_edu_father_cfps, by = "pid_f", all.x = TRUE)
ozer_child_cfps <- merge(ozer_child_cfps, ozer_edu_mother_cfps, by = "pid_m", all.x = TRUE)
summary(ozer_child_cfps)

# replace father with mother and mother with father if one of them is NA
ozer_child_cfps$edu_f_recode[is.na(ozer_child_cfps$edu_f_recode)] <- ozer_child_cfps$edu_m_recode[is.na(ozer_child_cfps$edu_f_recode)]#replace NA father ses with mother ses
ozer_child_cfps$edu_m_recode[is.na(ozer_child_cfps$edu_m_recode)] <- ozer_child_cfps$edu_f_recode[is.na(ozer_child_cfps$edu_m_recode)]#replace NA mother ses with father ses
#transform into ses (dichotomous)
ozer_child_cfps <- ozer_child_cfps %>%
  dplyr::mutate(edu_parents = edu_m_recode + edu_f_recode) 
summary(ozer_child_cfps$edu_parents) #obtain median of education = 12
ozer_child_cfps <- ozer_child_cfps %>%
  dplyr::mutate(SES_ozer_cfps = cut(edu_parents, breaks = c(0, 12, 100), labels = c('1', '2')))%>%
  dplyr::mutate(SES_ozer_cfps = as.numeric(SES_ozer_cfps)-1)
summary(ozer_child_cfps)

# psid
# extract participants
# psid_child
# psid_father
# psid_mother
# education
ozer_education_psid <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(edu_recode = cut(ER34548, breaks = c(-0.01, 6.5, 9.5,11.5,12.5,14.5,16.5, 17.4, 100), labels = c("1", "2", "3", "4", "5", "6", "7", "8")))%>%
  dplyr::mutate(edu_recode = as.numeric(edu_recode))
ozer_education_psid$edu_recode[ozer_education_psid$edu_recode == 8] <- NA 
head(ozer_education_psid)

# mother's education & father's education
ozer_edu_mother_psid <- ozer_education_psid
ozer_edu_father_psid <- ozer_education_psid
names(ozer_edu_mother_psid) <- c("edu_raw_m", "fid", "pid_m", "edu_m_recode")
names(ozer_edu_father_psid) <- c("edu_raw_f", "fid", "pid_f", "edu_f_recode")
ozer_edu_mother_psid <- merge(psid_mother, ozer_edu_mother_psid, by = c("fid", "pid_m"), all.x = TRUE)
ozer_edu_father_psid <- merge(psid_father, ozer_edu_father_psid, by = c("fid", "pid_f"), all.x = TRUE)

# merge father mother and children
ozer_child_psid <- merge(psid_child, ozer_edu_mother_psid, by = "fid", all.x = TRUE)
ozer_child_psid <- merge(ozer_child_psid, ozer_edu_father_psid, by = "fid", all.x = TRUE)

# replace father with mother and mother with father if one of them is NA
ozer_child_psid$edu_f_recode[is.na(ozer_child_psid$edu_f_recode)] <- ozer_child_psid$edu_m_recode[is.na(ozer_child_psid$edu_f_recode)]#replace NA father ses with mother ses
ozer_child_psid$edu_m_recode[is.na(ozer_child_psid$edu_m_recode)] <- ozer_child_psid$edu_f_recode[is.na(ozer_child_psid$edu_m_recode)]#replace NA mother ses with father ses
##transform into ses (dichotomous)
ozer_child_psid <- ozer_child_psid %>%
  dplyr::mutate(edu_parents = edu_m_recode + edu_f_recode) 
summary(ozer_child_psid$edu_parents) #obtain median of eduation = 10
ozer_child_psid <- ozer_child_psid %>%
  dplyr::mutate(SES_ozer_psid = cut(edu_parents, breaks = c(0, 10, 100), labels = c('1', '2')))%>%
  dplyr::mutate(SES_ozer_psid = as.numeric(SES_ozer_psid))
summary(ozer_child_psid)

