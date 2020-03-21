################## data extraction for "what is ses" project ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    20-03-15         The first version
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

#import data
df_cfps <-  read.csv("CFPS2010_ses_adults.csv")

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
betan_child_cfps <- merge(betan_child_cfps, betan_family_cfps, by = "fid")
betan_child_cfps <- merge(betan_child_cfps, betan_mater_cfps, by = "pid_m")
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
betan_child_cfps <-drop_na(betan_child_cfps) #check how many complete data left
#PSID
library(foreign)
df.psid <- read.spss("PSID_SES&mental health_selected data.sav", to.data.frame = TRUE, use.value.labels = TRUE)
df.psid_proposal <- read.spss("selected for proposal_v1.sav", to.data.frame = TRUE, use.value.labels = TRUE)
df.psid_proposal$pid <- 1:26445
#identify mother in the data
##!!note that dplyr will not keep the right label for variable, change that later
psid_mother <- df.psid_proposal %>%
  dplyr::select(ER66021,ER34503,ER34501,ER30002,ER32000) %>% #children in FU; relation to RP; fid; pid; sex
  dplyr::filter(ER66021 != 0)  %>% #the family have children
  dplyr::filter(ER34503 == 10 & 20) %>% #reference person & spouse & paterner
  dplyr::filter(ER32000 == 2) #female
var_labs <- attr(df.psid, "variable.labels")
var_labs <- var_labs[names(psid_mother)]
attr(psid_mother, "variable.labels") <- var_labs
names(psid_mother) <-  c("number_child", "relation_rp", "fid", "pid", "sex")
#recode maternal education: < high school (1); high school/GED (2); Techical/Vocational (3); som college (4); two-year degree (5); four-year degree (6); some graduate school (7); MA, PhD, Prefessional (8).
betan_psid_edu <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(edu_m_recode = cut(ER34548, breaks = c(-0.00001, 11.5, 12.5, 13.5, 14.5, 16.5, 98, 100), labels = c("1", "2", "3", "4", "5", "6", NA))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode))
                                                    #1=<high school: <12; 2=high school: 12; 3=some college/technical: 13; 4=two-years: 14; 5=4-years: 15/16; 6=some graduate/MA/Phd: 17 above; NA: 99
betan_psid_edu$edu_m_recode[betan_psid_edu$edu_m_recode == 7]<- NA 
names(betan_psid_edu) <-c("edu_year",  "fid","pid", "edu_m_recode")
#recode income: poverty line = 12,060 + (familysize-1)*4180
##extract familysize
familysize <- data.frame(table(df.psid$ER34501))
names(familysize) <- c("fid", "familysize")
##extract family income
betan_psid_income <- df.psid_proposal%>%
  dplyr::select(ER71426, ER34501,ER30002) #total family income, fid, pid
names(betan_psid_income) <- c("income", "fid", "pid")
##merge income and size
betan_psid_income <- merge(betan_psid_income, familysize, by = "fid")
##recode family income according to poverty lineand family size
betan_psid_income <- betan_psid_income %>%
  dplyr::mutate(itn1 = 12060 +  (familysize-1)*4180) %>% #poverty line 12060 for one people and increase 4180 for an extra person
  dplyr::mutate(itn4 = itn1*4,
                itn3 = itn1*3,
                itn2 = itn1*2) %>% #4 cut-points
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
  dplyr::mutate(itn = income_itn1 + income_itn2 + income_itn3 + income_itn4+1) #add 4 cut-point (e.g. four 0 means below poverty line: level 1)

#merge education and income
betan_psid <- merge(psid_mother, betan_psid_edu, by = c("fid", "pid"))
betan_psid <- merge(betan_psid, betan_psid_income, by = c("fid", "pid"))
betan_psid <- betan_psid %>%
  dplyr::mutate(itn = as.numeric(itn),
                edu_m_recode = as.numeric(edu_m_recode))%>%
  dplyr::mutate(SES_betan_psid = (itn + edu_m_recode)/2)

#########################################################################################
#######Moog, 2008######
#cfps
#income and education
moog_child_cfps <- df.children %>%
  dplyr::select(pid, fid, pid_m)
moog_child_cfps[moog_child_cfps == -8]  <-NA
#recode income into 5 groups: quantiles
#recode education into five groups: <high school, high school, bachelor, master, docterate
moog_cfps_income <-  df.family  %>%
  dplyr::select(fid, fincome) #?do not consider family size
moog_cfps_income$fincome[moog_cfps_income$fincome == -8]<-NA
moog_cfps_education <- df.individual %>%
  dplyr::select(pid, edu2010_t1_best)
table(moog_cfps_education$edu2010_t1_best) #check education
names(moog_cfps_education) <- c("pid_m", "edu2010_t1_best")
#merge income and education
moog_child_cfps <- merge(moog_child_cfps, moog_cfps_income, by ="fid")
moog_child_cfps <- merge(moog_child_cfps, moog_cfps_education, by = "pid_m")
#recode education, income and calculate composite SES
moog_child_cfps <-moog_child_cfps %>%
  dplyr::mutate(edu_cat = recode_factor(edu2010_t1_best, "1" = 1,"2" = 1,"3" = 1,"4" = 1,"5" = 2,"6" = 3,"7" = 4,"8" = 5))  %>% #recode education
  dplyr::mutate(income_cat=cut(fincome, breaks= quantile(moog_cfps_income$fincome, probs = seq(0, 1, 0.2), na.rm= TRUE), labels = c("1", "2", "3", "4", "5"))) %>% #recode income
  dplyr::mutate(income_cat = as.numeric(income_cat),
                edu_cat = as.numeric((edu_cat)))%>% #convert income and education into numeric variables
  dplyr::mutate(SES_moog_cfps = (edu_cat+income_cat)/2) #compoite SES: mean of education and income
#psid
psid_mother #from before
#education recode
moog_psid_edu <- df.psid %>%
  dplyr::select(ER34548,ER34501,ER30002) %>% #education, fid, pid
  dplyr::mutate(edu_m_recode = cut(ER34548, breaks = c(-0.00001, 11.5, 12.5, 16.5, 98, 100), labels = c("1", "2", "3", "4",  NA))) %>% #cut into 4 groups: <12, 12, 13-16, 17  (?do not distinguish master and phd)
  dplyr::mutate(edu_m_recode = as.numeric(edu_m_recode))
moog_psid_edu$edu_m_recode[moog_psid_edu$edu_m_recode == 7]<- NA 
names(moog_psid_edu) <-c("edu_year",  "fid","pid", "edu_m_recode")
#income recode
moog_psid_income <- df.psid_proposal  %>%
  dplyr::select(ER71426, ER34501,ER30002) #total family income, fid, pid
names(moog_psid_income) <- c("income", "fid", "pid")
moog_psid_income <- moog_psid_income%>%
  dplyr::mutate(income_cat=cut(income, breaks= quantile(moog_psid_income$income, probs = seq(0, 1, 0.2), na.rm= TRUE), labels = c("1", "2","3", "4","5")))#recode income
#merge income and eduation
moog_psid <- merge(psid_mother, moog_psid_edu, by = c("pid", "fid"))
moog_psid <- merge(moog_psid, moog_psid_income, by  =  c("pid", "fid"))
#calculate composite SES
moog_psid <- moog_psid %>%
  dplyr::mutate(income_cat = as.numeric(income_cat),
                edu_m_recode = as.numeric(edu_m_recode))%>%
  dplyr::mutate(SES_moog_psid = (income_cat + edu_m_recode)/2)

#########################################################################################
#######Yu,2018######
#cfps
##?children do not have subjective SES
##do second one (young adult)
#SSS, ITN, education 
df.cfps_roster <- read.csv("CFPSfamilyroster.csv", header = TRUE)
yu_yadult_cfps <- df.cfps_roster %>%
  dplyr::select(pid, pid_m, pid_f, fid, tb1b_a_p) %>%#pid, pid_m, pid_f, fid, age
  dplyr::filter(tb1b_a_p <= 25 &tb1b_a_p >= 18) 
yu_yadult_cfps
yu_yadult_cfps[yu_yadult_cfps == -8] <-NA
#recode family income into ITN (poverty line in 2010: 1274)
yu_cfps_income <- df.family %>%
  dplyr::select(fid, finc_per) %>%
  dplyr::mutate(income_cat = cut(finc_per, 
                                 breaks = quantile(yu_2018_cfps$finc_per, probs = seq(0, 1, 1/9), na.rm= TRUE),
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
#SSS
yu_cfps_sss <- df.individual %>%
  dplyr::select(pid, qm402) #pid, subjective social status
names(yu_cfps_sss) <-  c("pid", "sss")
#merge income, father education, mother education and sss
yu_cfps <- merge(yu_yadult_cfps, yu_cfps_income, by = "fid")
yu_cfps  <- merge(yu_cfps, yu_cfps_fedu, by = "pid_f")  
yu_cfps <- merge(yu_cfps, yu_cfps_medu, by = "pid_m")
yu_cfps <- merge(yu_cfps, yu_cfps_sss, by = "pid")
#PCA
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)
yu_pca <- yu_cfps %>%
  dplyr::select(ITN, edu_f_recode, edu_m_recode, sss)
yu_pca <- drop_na(yu_pca)
yu.pr<-PCA(yu_pca, scale.unit = TRUE, graph = TRUE)
get_eigenvalue(yu.pr)
fviz_eig(yu.pr)#scree plot
var <- get_pca_var(yu.pr) #sss shows opposite contribution (?)
#calculate SES as first component (?PCA does not appear to be single component)
yu_cfps <- yu_cfps %>%
  dplyr::mutate(SES_yu_cfps = 0.71118313*ITN + 0.78339583*edu_f_recode + 0.81539907*edu_f_recode + (-0.04682041)*sss)
